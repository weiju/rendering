#!/usr/bin/env python3

from PIL import Image
import pygame
import numpy as np
from numpy import linalg as LA
import sys
import multiprocessing
import math
import time

import model

def current_millis():
    """returns the current time in milliseconds"""
    return int(math.floor(time.time() * 1000))

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


def illuminate(scene, obj, ray, intersection, light):
    light_color = 0.7

    t, normal = intersection
    diffuse = diffuse_component(obj, ray, t, normal, light)
    ambient = scene.ambient_coeff * obj.material.diffuse_coeff * scene.ambient_color * obj.material.diffuse_color
    total = diffuse + ambient
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
        return illuminate(scene, obj, ray, intersection, scene.lights[0])
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

def render_line(y):
    for x in range(vp.width):
        ray = camera.make_ray(x, y)
        r, g, b = trace_ray(scene, ray)
        rgb = (int(r * 255), int(g * 255), int(b * 255))
        im.putpixel((x, y), rgb)
        pxarray[x][y] = rgb


def render(multiprocessing=True):
    if multiprocessing:
        with get_mp_pool() as pool:
            pool.map(render_line, range(vp.height))
    else:
        for y in range(vp.height):
            render_line(y)
    pygame.display.flip()


if __name__ == '__main__':
    pygame.init()
    scene = model.load_scene('scene.json')
    camera = scene.camera
    vp = scene.viewport
    window = pygame.display.set_mode((vp.width, vp.height))
    pygame.display.set_caption('Raytracing Demo (Python) 1.0')
    pxarray = pygame.PixelArray(window)
    im = Image.new("RGB", (vp.width, vp.height))
    start_time = current_millis()
    render(multiprocessing=False)
    elapsed = current_millis() - start_time
    print("Rendering in %d ms." % elapsed)

    im.save("example.png", "PNG")

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                sys.exit(0)
