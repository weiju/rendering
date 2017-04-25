#!/usr/bin/env python3
import sys
import unittest
import xmlrunner
import numpy as np
from numpy import linalg as LA

import model

class CameraTest(unittest.TestCase):  # pylint: disable-msg=R0904

    def test_compute_fov(self):
        fov_x, fov_y = model.compute_fov(320, 200, 0.5)
        self.assertAlmostEqual(fov_x, 3.1353426739347263)
        self.assertAlmostEqual(fov_y, 3.1315927369218768)

    def test_make_ray(self):
        camera = model.Camera(320, 200, np.array([0.0, 0.0, -400.0]), np.array([0.0, 0.0, 0.0]))
        ray = camera.make_ray(0.0, 0.0)
        self.assertTrue(np.allclose(ray.origin, np.array([0.0, 0.0, -400.0])))
        self.assertAlmostEqual(LA.norm(ray.direction), 1.0)
        self.assertTrue(np.allclose(ray.direction,
                                    np.array([0.581914373963, -0.363696483727, 0.727392967453])))

class SphereTest(unittest.TestCase):  # pylint: disable-msg=R0904

    def setUp(self):
        self.sphere = model.Sphere(np.array([0.0, 0.0, 300]), 150.0,
                                   model.Material(np.array([1.0, 0.0, 0.0]), 0.56, 0.8, 10.0))

    def test_initial(self):
        self.assertTrue(np.allclose(self.sphere.center, np.array([0.0, 0.0, 300.0])))
        self.assertAlmostEqual(self.sphere.radius, 150.0)

if __name__ == '__main__':
    suite = [unittest.TestLoader().loadTestsFromTestCase(CameraTest),
             unittest.TestLoader().loadTestsFromTestCase(SphereTest)]

    if len(sys.argv) > 1 and sys.argv[1] == 'xml':
      xmlrunner.XMLTestRunner(output='test-reports').run(unittest.TestSuite(suite))
    else:
      unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(suite))

