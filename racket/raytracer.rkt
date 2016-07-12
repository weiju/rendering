#lang racket
(provide Vector3d v3d-cross v3d-add v3d-sub)
(require json)

;; data structures
(struct Vector3d (x y z) #:transparent)
(struct Viewport (width height) #:transparent)
(struct Ray (origin direction) #:transparent)
(struct Light (position color) #:transparent)
(struct Material (diffuse-color diffuse-coeff) #:transparent)
(struct Color (r g b a) #:transparent)
(struct AmbientLight (color coeff) #:transparent)
(struct Camera (eye bottom-left xinc yinc) #:transparent)
(struct Sphere (center radius material) #:transparent)
(struct Scene (viewport background-color ambient-light
                        camera lights objects) #:transparent)

;; factory functions
(define (make-color r g b [a 1.0]) (Color r g b a))
(define (make-color-hash ht) (make-color (hash-ref ht 'r) (hash-ref ht 'g)
                                         (hash-ref ht 'b)))
(define (make-vec3d ht)
  (Vector3d (hash-ref ht 'x) (hash-ref ht 'y) (hash-ref ht 'z)))
(define (make-light ht)
  (Light (make-vec3d (hash-ref ht 'position)) (hash-ref ht 'color)))
(define (make-material ht)
  (Material (make-color-hash (hash-ref ht 'diffuse_color))
            (hash-ref ht 'diffuse_coeff)))
(define (make-sphere ht)
  (Sphere (make-vec3d (hash-ref ht 'center)) (hash-ref ht 'radius)
                      (make-material (hash-ref ht 'material))))
(define (make-object ht)
  (cond [(hash-has-key? ht 'sphere) (make-sphere (hash-ref ht 'sphere))]))

(define (compute-fov sensor-width sensor-height focal-length)
  ;; Field of View (FOV): https://en.wikipedia.org/wiki/Field_of_view
  ;; FOV = 2 * arctan(sensorsize / 2 * focal length)
  (list (* 2 (atan (/ sensor-width (* 2 focal-length))))
        (* 2 (atan (/ sensor-height (* 2 focal-length))))))

(define (make-camera vpwidth vpheight eye lookat
                     [focal-length 0.5]
                     [up (Vector3d 0.0 1.0 0.0)]
                     [pixel-width 1.0]
                     [pixel-height 1.0])
  (let* ([dir (v3d-sub lookat eye)]
        [fov (compute-fov vpwidth vpheight focal-length)]
        [u0 (v3d-cross dir up)]
        [v0 (v3d-cross u0 dir)]
        [u (v3d-normalize u0)]
        [v (v3d-normalize v0)]
        [aspect-ratio (/ vpheight vpwidth)]
        [vp-halfwidth (tan (/ (car fov) 2))]
        [vp-halfheight (* aspect-ratio vp-halfwidth)])
    (Camera eye
            (v3d-sub (v3d-sub lookat (v3d-smul v vp-halfheight))
                     (v3d-smul u vp-halfwidth))
            (v3d-sdiv (v3d-smul u (* 2 vp-halfwidth)) (* vpwidth pixel-width))
            (v3d-sdiv (v3d-smul v (* 2 vp-halfheight)) (* vpheight pixel-height)))))

;; vector math
(define (v3d-sub v1 v2)
  (Vector3d (- (Vector3d-x v1) (Vector3d-x v2))
            (- (Vector3d-y v1) (Vector3d-y v2))
            (- (Vector3d-z v1) (Vector3d-z v2))))
(define (v3d-add v1 v2)
  (Vector3d (+ (Vector3d-x v1) (Vector3d-x v2))
            (+ (Vector3d-y v1) (Vector3d-y v2))
            (+ (Vector3d-z v1) (Vector3d-z v2))))
(define (v3d-smul v s)
  (Vector3d (* (Vector3d-x v) s)
            (* (Vector3d-y v) s)
            (* (Vector3d-z v) s)))
(define (v3d-sdiv v s)
  (Vector3d (/ (Vector3d-x v) s)
            (/ (Vector3d-y v) s)
            (/ (Vector3d-z v) s)))
(define (v3d-dot v1 v2)
  (+ (* (Vector3d-x v1) (Vector3d-x v2))
     (* (Vector3d-y v1) (Vector3d-y v2))
     (* (Vector3d-z v1) (Vector3d-z v2))))

(define (v3d-cross v1 v2)
  (Vector3d (- (* (Vector3d-y v1) (Vector3d-z v2)) (* (Vector3d-z v1) (Vector3d-y v2)))
            (- (* (Vector3d-z v1) (Vector3d-x v2)) (* (Vector3d-x v1) (Vector3d-z v2)))
            (- (* (Vector3d-x v1) (Vector3d-y v2)) (* (Vector3d-y v1) (Vector3d-x v2)))))

(define (v3d-length v)
  (sqrt (+ (* (Vector3d-x v) (Vector3d-x v))
           (* (Vector3d-y v) (Vector3d-y v))
           (* (Vector3d-z v) (Vector3d-z v)))))
(define (v3d-normalize v)
  (let ([l (v3d-length v)])
    (Vector3d (/ (Vector3d-x v) l) (/ (Vector3d-y v) l) (/ (Vector3d-z v) l))))

;; Loading a scene from a JSON file
(define (load-scene path)
  (let* ([in (open-input-file path)]
         [scene-json (read-json in)]
         [vp (hash-ref scene-json 'viewport)]
         [bgc (hash-ref scene-json 'background_color)]
         [amb (hash-ref scene-json 'ambient_light)]
         [cam (hash-ref scene-json 'camera)]
         [lgts (hash-ref scene-json 'lights)]
         [objs (hash-ref scene-json 'objects)])
    (close-input-port in)
    (Scene (Viewport (hash-ref vp 'width) (hash-ref vp 'height))
           (make-color (hash-ref bgc 'r) (hash-ref bgc 'g) (hash-ref bgc 'b))
           (AmbientLight (hash-ref amb 'color) (hash-ref amb 'coeff))
           (make-camera (hash-ref vp 'width) (hash-ref vp 'height)
                        (make-vec3d (hash-ref cam 'eye))
                        (make-vec3d (hash-ref cam 'lookat)))
           (map make-light lgts)
           (map make-object objs))))

(println "Loading scene...")
(load-scene "../scene.json")
(println "Done.")