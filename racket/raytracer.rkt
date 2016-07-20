#lang racket
(provide Vector3d v3d-cross v3d-add v3d-sub v3d-length v3d-normalize
         v3d-smul v3d-sdiv v3d-dot
         trim-color-comp convert-color-val)
(require racket/draw)
(require json)

(define EPS 1.0e-7)
;; data structures
(struct Vector3d (x y z) #:transparent)
(struct Viewport (width height) #:transparent)
(struct Ray (origin direction) #:transparent)
(struct Light (position color) #:transparent)
(struct Material (diffuse-color diffuse-coeff
                                specular-coeff specular-highlight) #:transparent)
(struct Color (r g b a) #:transparent)
(struct AmbientLight (color coeff) #:transparent)
(struct Camera (eye bottom-left xinc yinc) #:transparent)
(struct Sphere (center radius material) #:transparent)
(struct Scene (viewport background-color ambient-light
                        camera lights objects) #:transparent)

;; factory functions
(define (make-color2 r g b [a 1.0]) (Color r g b a))
(define (make-color-hash ht) (make-color2 (hash-ref ht 'r) (hash-ref ht 'g)
                                          (hash-ref ht 'b)))
(define (trim-color-comp c) (min 1 (max 0 c)))
(define (convert-color-val v) (exact-round (* (trim-color-comp v) 255)))
(define (convert-color c)
  (make-color (convert-color-val (Color-r c)) (convert-color-val (Color-g c))
              (convert-color-val (Color-b c))))

(define (make-vec3d ht)
  (Vector3d (hash-ref ht 'x) (hash-ref ht 'y) (hash-ref ht 'z)))
(define (make-light ht)
  (Light (make-vec3d (hash-ref ht 'position)) (hash-ref ht 'color)))
(define (make-material ht)
  (Material (make-color-hash (hash-ref ht 'diffuse_color))
            (hash-ref ht 'diffuse_coeff)
            (hash-ref ht 'specular_coeff)
            (hash-ref ht 'specular_highlight)))
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

;; intersection procedures

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
           (make-color-hash bgc)
           (AmbientLight (hash-ref amb 'color) (hash-ref amb 'coeff))
           (make-camera (hash-ref vp 'width) (hash-ref vp 'height)
                        (make-vec3d (hash-ref cam 'eye))
                        (make-vec3d (hash-ref cam 'lookat)))
           (map make-light lgts)
           (map make-object objs))))

;; generate camera ray from a given screen position
(define (make-ray camera x y)
  (let* ([eye (Camera-eye camera)]
         [vp-point (v3d-add (v3d-add (Camera-bottom-left camera)
                           (v3d-smul (Camera-xinc camera) x))
                           (v3d-smul (Camera-yinc camera) y))]
         [ray-dir (v3d-sub vp-point eye)])
  (Ray eye (v3d-normalize ray-dir))))

;; geometric solution of intersecting a ray with a sphere
(define (sphere-normal s ray t)
  (let ([l (v3d-sub (Ray-origin ray) (Sphere-center s))])
    (v3d-normalize (v3d-sdiv (v3d-add l (v3d-smul (Ray-direction ray) t))
                             (Sphere-radius s)))))

(define (intersect-sphere s ray)
  (let* ([l (v3d-sub (Sphere-center s) (Ray-origin ray))]
        [tca (v3d-dot l (Ray-direction ray))])
    (cond [(< tca 0) '()]
          [else
           (let ([d2 (- (v3d-dot l l) (* tca tca))]
                 [radius2 (* (Sphere-radius s) (Sphere-radius s))])
             (cond [(> d2 radius2) '()]
                   [else
                    (let* ([thc (sqrt (- radius2 d2))]
                           [t0 (- tca thc)]
                           [t1 (+ tca thc)])
                      (cond [(> t0 EPS) (list t0 (sphere-normal s ray t0) s)]
                            [(> t1 EPS) (list t1 (sphere-normal s ray t1) s)]
                            [else
                             '()]))]))])))

;; find closest object that intersects the ray coming from the viewer
(define (find-closest objs ray closest)
  (cond [(empty? objs) closest]
        [else
         (let ([intersection (intersect-sphere (car objs) ray)])
           (cond [(empty? closest) (find-closest (cdr objs) ray intersection)]
                 [(empty? intersection) (find-closest (cdr objs) ray closest)]
                 [else
                  (let ([tc (car closest)]
                        [ti (car intersection)])
                    (cond [(< ti tc)
                           (find-closest (cdr objs) ray intersection)]
                          [else (find-closest (cdr objs) ray closest)]))]))]))

;; determine the color of the object-ray intersection point
(define (trim-negative n) (max n 0))

(define (diffuse-component ray intersection light)
  (let* ([t (car intersection)]
         [normal (cadr intersection)]
         [mat (Sphere-material (caddr intersection))]
         [col (Material-diffuse-color mat)]
         [intersect-point (v3d-add (Ray-origin ray) (v3d-smul (Ray-direction ray) t))]
         [intersect-to-light (v3d-normalize (v3d-sub (Light-position light)
                                                     intersect-point))]
         [ldot-normal (trim-negative (v3d-dot normal intersect-to-light))]
         [ds (* (Material-diffuse-coeff mat) (Light-color light) ldot-normal)])
    (make-color2 (* (Color-r col) ds) (* (Color-g col) ds)
                 (* (Color-b col) ds))))

(define (phong-component ray intersection light)
  (let* ([t (car intersection)]
         [normal (cadr intersection)]
         [mat (Sphere-material (caddr intersection))]
         [intersect-point (v3d-add (Ray-origin ray) (v3d-smul (Ray-direction ray) t))]
         [l (v3d-normalize (v3d-sub (Light-position light) intersect-point))]
         [v (v3d-normalize (v3d-sub (Ray-origin ray) intersect-point))]
         [r (v3d-smul (v3d-sub normal l) (* 2 (v3d-dot l normal)))]
         [color-comp (* (Material-specular-coeff mat) (Light-color light)
                     (expt (v3d-dot r v) (Material-specular-highlight mat)))])
    (make-color2 color-comp color-comp color-comp)))

(define (ambient-component objmat scene)
  (let* ([amblight (Scene-ambient-light scene)]
         [amb-coeff (AmbientLight-coeff amblight)]
         [amb-color (AmbientLight-color amblight)]
         [as (* amb-coeff (Material-diffuse-coeff objmat) amb-color)]
         [dcol (Material-diffuse-color objmat)])
    (make-color2 (* (Color-r dcol) as) (* (Color-g dcol) as)
                 (* (Color-b dcol) as))))

(define (color-add c1 c2)
  (make-color2 (+ (Color-r c1) (Color-r c2))
               (+ (Color-g c1) (Color-g c2))
               (+ (Color-b c1) (Color-b c2))))

(define (color-average colors)
  (let ([ncols (length colors)]
        [sum (foldl color-add (make-color2 0 0 0 ) colors)])
    (make-color2 (/ (Color-r sum) ncols) (/ (Color-g sum) ncols)
                 (/ (Color-b sum) ncols))))

(define (illuminate scene ray intersection)
  (let* ([obj (caddr intersection)]
         [diffuse (diffuse-component ray intersection (car (Scene-lights scene)))]
         [ambient (ambient-component (Sphere-material obj) scene)]
         [specular (phong-component ray intersection (car (Scene-lights scene)))])
    (color-add (color-add diffuse ambient) specular)))

(define (trace-ray scene ray)
  (let* ([background (Scene-background-color scene)]
         [closest (find-closest (Scene-objects scene) ray '())])
    (cond [(empty? closest) background]
          [else
           (illuminate scene ray closest)])))

(define (render-line scene y sample-offsets dc)
  (let ([width (Viewport-width (Scene-viewport scene))]
        [camera (Scene-camera scene)])
    (for ([x (in-range width)])
      (let ([sample-colors (map (lambda (offs)
                                  (let* ([xi (+ x (car offs))]
                                         [yi (+ y (cadr offs))]
                                         [ray (make-ray camera xi yi)])
                                    (trace-ray scene ray)))
                                sample-offsets)])
        (send dc set-pixel x y (convert-color (color-average sample-colors)))))))

(define (render scene sample-offsets dc)
  (let ([height (Viewport-height (Scene-viewport scene))])
    (for ([y (in-range height)])
      (render-line scene y sample-offsets dc)))
    '())

(define (raytracer scene-file png-file [sample-divisions 3])
  (let* ([scene (load-scene scene-file)]
         [width (Viewport-width (Scene-viewport scene))]
         [height (Viewport-height (Scene-viewport scene))]
         [bm (make-bitmap width height)]
         [dc (send bm make-dc)])
    (render scene (make-sample-offsets sample-divisions) dc)
    (send bm save-file png-file 'png)))

(define (jitter jsize)
  (let ([signum (modulo (random 999) 2)]
        [absval (/ (* (random) jsize) 2)])
    (cond [(= signum 0) absval]
          [else (- absval)])))

(define (make-division sec-size i)
  (+ (* sec-size i) (/ sec-size 2)))

(define (make-divisions total num-sections)
  (let ([sec-size (/ total num-sections)])
    (map (lambda (i) (make-division sec-size i)) (for/list ([i num-sections]) i))))

(define (make-sample-offsets num-sections [pixel-width 1.0] [pixel-height 1.0])
  (let ([xdiv (make-divisions pixel-width num-sections)]
        [ydiv (make-divisions pixel-height num-sections)]
        [xsec-size (/ pixel-width num-sections)]
        [ysec-size (/ pixel-height num-sections)])
    (for*/list ([x xdiv]
           [y ydiv])
      (list (+ x (jitter xsec-size)) (+ y (jitter ysec-size))))))

;;(raytracer "../scene.json" "testout.png")
