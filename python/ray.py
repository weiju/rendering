#!/usr/bin/env python3

from PIL import Image
import pygame
import numpy as np
from numpy import linalg as LA
import sys
import multiprocessing
import math
import time
import random
import argparse

import model

def current_millis():
    """returns the current time in milliseconds"""
    return int(math.floor(time.time() * 1000))

def __normalize(v):
    return v / LA.norm(v)

"""
The illumination routines could mostly be attached to the Scene class,
but for now, I want to keep them out and treat the scene as a pure data
holder.
"""
def diffuse_component(obj, ray, t, normal, light):
    intersect_point = ray.origin + t * ray.direction
    intersect_to_light = light.position - intersect_point
    intersect_to_light = intersect_to_light / LA.norm(intersect_to_light)

    ldot_normal = np.dot(normal, intersect_to_light)
    if ldot_normal < 0:
        ldot_normal = 0.0

    return obj.material.diffuse_coeff * obj.material.diffuse_color * light.color * ldot_normal

def phong_component(obj, ray, t, normal, light):
    specular_coeff = 0.95
    n = 10  # larger => smaller highlight spot, smaller n => larger spot

    intersect_point = ray.origin + t * ray.direction
    l = __normalize(light.position - intersect_point)
    v = __normalize(ray.origin - intersect_point)
    r = 2 * (np.dot(l, normal)) * (normal - l)
    color_comp = specular_coeff * light.color * (np.dot(r, v) ** n)
    return np.array([color_comp, color_comp, color_comp])


def illuminate(scene, ray, closest):
    obj, intersection = closest
    light = scene.lights[0]
    t, normal = intersection
    diffuse = diffuse_component(obj, ray, t, normal, light)
    specular = phong_component(obj, ray, t, normal, light)
    ambient = scene.ambient_coeff * obj.material.diffuse_coeff * scene.ambient_color * obj.material.diffuse_color
    total = diffuse + ambient + specular
    for i in range(3):
        if total[i] > 1.0:
            total[i] = 1.0
    return total


def find_closest(scene, ray):
    """
    Find the (object, intersection) pair closest to the camera.
    This is done by finding the object with the smallest t
    """
    closest = None
    for obj in scene.objects:
        intersection = obj.intersect(ray)
        if intersection is not None:
            if closest is None:
                closest = (obj, intersection)
            else:
                closest_obj, closest_intersect = closest
                if intersection[0] < closest_intersect[0]:
                    closest = (obj, intersection)
    return closest


def trace_ray(scene, ray):
    closest = find_closest(scene, ray)
    if closest is not None:
        obj, intersection = closest
        return illuminate(scene, ray, closest)
    else:
        return scene.bgcolor

class get_mp_pool:
    def __init__(self):
        self.pool = multiprocessing.Pool()

    def __enter__(self):
        return self.pool

    def __exit__(self, type, value, tb):
        self.pool.close()
        self.pool.join()

def trim_color_comp(value):
    """make sure the color components don't excced their maximum"""
    if value > 255:
        return 255
    elif value < 0:
        return 0
    else:
        return value

def render_line(line):
    y, sample_offsets = line
    for x in range(vp.width):
        samples = [(x + offset[0], y + offset[1]) for offset in sample_offsets]
        num_samples = len(samples)
        r_sum = 0.0
        g_sum = 0.0
        b_sum = 0.0
        for xs, ys in samples:
            ray = camera.make_ray(xs, ys)
            rs, gs, bs = trace_ray(scene, ray)
            r_sum += rs
            g_sum += gs
            b_sum += bs
        rgb = (trim_color_comp(int(r_sum / num_samples * 255)),
               trim_color_comp(int(g_sum / num_samples * 255)),
               trim_color_comp(int(b_sum / num_samples * 255)))
        try:
            im.putpixel((x, y), rgb)
            pxarray[x][y] = rgb
        except:
            print("rgb: ", rgb)
            raise

def jitter(jsize):
    """add random positive or negative jitter within the specified box"""
    return random.uniform(-jsize / 2, jsize / 2)


class StochasticSampler:
    def __init__(self, num_sections=3, pixel_width=1.0, pixel_height=1.0):
        self.pixel_width = pixel_width
        self.pixel_height = pixel_height
        self.num_sections = num_sections

    def make_sample_offsets(self):
        xsec_size = self.pixel_width / self.num_sections
        ysec_size = self.pixel_height / self.num_sections

        xdiv = [xsec_size * i + xsec_size / 2 for i in range(self.num_sections)]
        ydiv = [ysec_size * i + ysec_size / 2 for i in range(self.num_sections)]
        return [(x + jitter(xsec_size), y + jitter(ysec_size)) for y in ydiv for x in xdiv]


def render(sampler, multiprocessing=True):
    sample_offsets = sampler.make_sample_offsets()
    if multiprocessing:
        with get_mp_pool() as pool:
            pool.map(render_line, [(y, sample_offsets) for y in range(vp.height)])
    else:
        for y in range(vp.height):
            render_line((y, sample_offsets))
    pygame.display.flip()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="raytrace a scene")
    parser.add_argument('scenefile', help='scene file in JSON format')
    parser.add_argument('pngfile', help='output file in PNG format')
    args = parser.parse_args()

    pygame.init()
    scene = model.load_scene(args.scenefile)
    camera = scene.camera
    vp = scene.viewport
    window = pygame.display.set_mode((vp.width, vp.height))
    pygame.display.set_caption('Raytracing Demo (Python) 1.0')
    pxarray = pygame.PixelArray(window)
    im = Image.new("RGB", (vp.width, vp.height))
    start_time = current_millis()
    render(StochasticSampler(), multiprocessing=False)
    elapsed = current_millis() - start_time
    print("Rendering in %d ms." % elapsed)

    im.save(args.pngfile, "PNG")

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit(0)
