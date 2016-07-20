#lang racket
(require rackunit "raytracer.rkt")

;; Tests for the vector functions
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

(check-equal?
 (v3d-dot (Vector3d 8 9 10) (Vector3d 4 3 7))
 129 "dot product of two vectors")

(check-equal?
 (v3d-length (Vector3d 1 2 3))
 (sqrt 14) "calculate length of a vector")

(check-equal?
 (v3d-length (v3d-normalize (Vector3d 3 14 5)))
 1.0 "normalize a vector")

(check-equal?
 (v3d-smul (Vector3d 1 2 3) 3.0)
 (Vector3d 3.0 6.0 9.0) "multiply vector with a scalar")

(check-equal?
 (v3d-sdiv (Vector3d 3.0 6.0 9.0) 3.0)
 (Vector3d 1.0 2.0 3.0) "divide vector by a scalar")

;; check color functions
(check-equal? (trim-color-comp 1.3) 1.0)
(check-equal? (trim-color-comp -1.3) 0.0)
(check-equal? (convert-color-val 1.0) 255)
(check-equal? (convert-color-val 0.0) 0)
(check-equal? (convert-color-val -0.5) 0)
(check-equal? (convert-color-val 1.2) 255)
(check-equal? (convert-color-val 0.5) 128)
