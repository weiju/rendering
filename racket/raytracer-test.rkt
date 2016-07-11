#lang racket
(require rackunit "raytracer.rkt")

(check-equal?
 (v3d-add (Vector3d 1 2 3) (Vector3d 2 3 4))
 (Vector3d 3 5 7) "sum of two vectors")

(check-equal?
 (v3d-sub (Vector3d 9 3 1) (Vector3d 4 6 1))
 (Vector3d 5 -3 0) "difference of two vectors")

(check-equal?
 (v3d-cross (Vector3d 1 2 3) (Vector3d 1 2 3))
 (Vector3d 0 0 0) "cross product with itself is 0 vector")

(check-equal?
 (v3d-cross (Vector3d 1 2 3) (Vector3d 2 3 4))
 (Vector3d -1 2 -1) "cross product of 2 vectors")