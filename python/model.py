"""model.py - model classes and functions for the raytracer
"""

from collections import namedtuple
import math
import numpy as np
from numpy import linalg as LA
import json

EPS = 1.0e-7

Viewport = namedtuple('Viewport', ['width', 'height'])
Ray = namedtuple('Ray', ['origin', 'direction'])
Material = namedtuple('Material', ['diffuse_color', 'diffuse_coeff', 'specular_coeff', 'hardness'])


def compute_fov(sensor_width, sensor_height, focal_length):
    """
    Field of View (FOV): https://en.wikipedia.org/wiki/Field_of_view
    FOV = 2 * arctan(sensorsize / 2 * focal length)"""
    return 2 * np.arctan(np.array([sensor_width, sensor_height]) / (2 * focal_length))

class Camera:
    """A model camera
    see:
    https://steveharveynz.wordpress.com/2012/12/20/ray-tracer-part-two-creating-the-camera/
    http://www.scratchapixel.com/lessons/3d-basic-rendering/ray-tracing-generating-camera-rays/generating-camera-rays
    FOV (rectilinear) =  2 * arctan (frame size/(focal length * 2))
    """
    def __init__(self, vpwidth, vpheight, eye, lookat,
                 focal_length=0.50, up=np.array([0.0, 1.0, 0.0]),
                 pixel_width=1.0, pixel_height=1.0):
        self.eye = eye
        direction = lookat - eye
        fov = compute_fov(vpwidth, vpheight, focal_length)
        u = np.cross(direction, up)
        v = np.cross(u, direction)
        u = u / LA.norm(u)
        v = v / LA.norm(v)

        aspect_ratio = vpheight/vpwidth
        viewplane_halfwidth = math.tan(fov[0] / 2)
        viewplane_halfheight = aspect_ratio * viewplane_halfwidth

        self.viewplane_bottomleft = lookat - v * viewplane_halfheight - u * viewplane_halfwidth
        self.xinc = (u * 2 * viewplane_halfwidth) / vpwidth * pixel_width
        self.yinc = (v * 2 * viewplane_halfheight) / vpheight * pixel_height

    def make_ray(self, x, y):
        viewplane_point = self.viewplane_bottomleft + x * self.xinc + y * self.yinc
        ray_dir = viewplane_point - self.eye
        ray_dir = ray_dir / LA.norm(ray_dir)
        return Ray(self.eye, ray_dir)


class Sphere:
    """A true sphere"""

    def __init__(self, center, radius, material):
        self.center = center
        self.radius = radius
        self.material = material

    def __intersect_analytic(self, ray):
        """this is the analytic solution for intersecting a ray with a sphere
        See: http://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection
        for an explanation
        """
        l = ray.origin - self.center
        a = np.dot(ray.direction, ray.direction)
        b = 2 * np.dot(ray.direction, l)
        c = np.dot(l, l) - self.radius * self.radius

        """
        solve quadratic equation (discr = b^2 - 4ac)
        if discr is 0 => t0 = t1 = -b/2a
        if disc > 0 =>
        t0 = (-b + sqrt(discr)) / 2a
        t1 = (-b - sqrt(discr)) / 2a
        """
        discr = b * b - 4.0 * a * c

        if discr < 0.0:
            return None
        elif discr == 0.0:
            t = -0.5 * b / a
            return t, self.normal_at(ray, t)
        else:
            e = math.sqrt(discr)
            denom = 2 * a
            t0 = (-b - e) / denom
            t1 = (-b + e) / denom
            if (t0 > EPS):
                return t0, self.normal_at(ray, t0)
            if (t1 > EPS):
                return t1, self.normal_at(ray, t1)
        return None

    def __intersect_geometric(self, ray):
        l = self.center - ray.origin
        tca = np.dot(l, ray.direction)
        if tca < 0:
            return None

        d2 = np.dot(l, l) - tca * tca
        radius2 = self.radius * self.radius
        if d2 > radius2:
            return None
        thc = math.sqrt(radius2 - d2)
        t0 = tca - thc
        t1 = tca + thc

        if t0 > EPS:
            return t0, self.normal_at(ray, t0)
        elif t1 > EPS:
            return t1, self.normal_at(ray, t1)
        return None

    def intersect(self, ray):
        return self.__intersect_geometric(ray)

    def normal_at(self, ray, t):
        """compute the surface normal of this sphere at the ray intersection point defined
        by ray and t"""
        l = ray.origin - self.center
        result = (l + t * ray.direction) / self.radius
        return result / LA.norm(result)


class Light:
    def __init__(self, position, color):
        self.position = position
        self.color = color


class Scene:
    def __init__(self, viewport, camera, lights, bgcolor, amb_color, amb_coeff, objects):
        self.viewport = viewport
        self.camera = camera
        self.lights = lights
        self.bgcolor = bgcolor
        self.ambient_color = amb_color
        self.ambient_coeff = amb_coeff
        self.objects = objects


def load_scene(path):
    with open(path) as infile:
        scene = json.load(infile)
        vp = Viewport(scene['viewport']['width'], scene['viewport']['height'])
        camera = Camera(vp.width, vp.height,
                        eye=np.array([scene['camera']['eye']['x'],
                                      scene['camera']['eye']['y'],
                                      scene['camera']['eye']['z']]),
                        lookat=np.array([scene['camera']['lookat']['x'],
                                         scene['camera']['lookat']['y'],
                                         scene['camera']['lookat']['z']]))
        lights = [Light(np.array([l['position']['x'], l['position']['y'], l['position']['z']]),
                        l['color']) for l in scene['lights']]
        bgcolor = scene['background_color']
        objects = []
        for obj in scene['objects']:
            if 'sphere' in obj:
                center = obj['sphere']['center']
                mat = obj['sphere']['material']
                diff_col = mat['diffuse_color']
                material = Material(diffuse_color=np.array([diff_col['r'], diff_col['g'], diff_col['b']]),
                                    diffuse_coeff=mat['diffuse_coeff'],
                                    specular_coeff=mat['specular_coeff'],
                                    hardness=mat['hardness'])
                objects.append(Sphere(center=np.array([center['x'], center['y'], center['z']]),
                                      radius=obj['sphere']['radius'],
                                      material=material))
    return Scene(vp, camera, lights, [bgcolor['r'], bgcolor['g'], bgcolor['b']],
                 scene['ambient_light']['color'], scene['ambient_light']['coeff'], objects)
