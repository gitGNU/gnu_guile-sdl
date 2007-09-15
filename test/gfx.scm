;;; gfx.scm --- simple graphics primitives test

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl gfx) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL video
(SDL:init '(SDL_INIT_VIDEO))

;; initialize the video mode
(SDL:set-video-mode 640 480 16)

(set! *random-state* (seed->random-state (current-time)))

(define SCREEN (SDL:get-video-surface))

;; clear the screen
(SDL:fill-rect SCREEN #f #xffffff)
(SDL:flip)

;; draw beziers
(do ((bez 0 (1+ bez)))
    ((= 100 bez))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-vector n 0))
         (y-uv (make-vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (vector-set! x-uv i (random 640))
      (vector-set! y-uv i (random 480)))
    (SDL:draw-bezier SCREEN x-uv y-uv 5
                     (+ #x80 (ash (random #xffffff) 8))))
  (SDL:flip))

;; draw horizontal and vertical lines
(let ((w (SDL:surface:w SCREEN))
      (h (SDL:surface:h SCREEN)))
  (do ((i 0 (1+ i)))
      ((= i h))
    (SDL:draw-hline SCREEN (random w) (random w) i (random #xffffff)))
  (do ((i 0 (1+ i)))
      ((= i w))
    (SDL:draw-vline SCREEN i (random h) (random h) (random #xffffff)))
  (SDL:flip))

;; draw lines
(define (one start-l start-r start-t start-b)

  (define (line!-proc color)
    (lambda (x1 y1 x2 y2)
      (SDL:draw-line SCREEN x1 y1 x2 y2 color)))

  (define (span-points beg end steps)
    (let ((inc (/ (- end beg) steps)))
      (let loop ((ls (list end)))
        (if (<= (inexact->exact (car ls)) beg)
            (map inexact->exact ls)
            (loop (cons (- (car ls) inc) ls))))))

  (define (mesh l r t b steps)
    (let* ((LR (span-points l r steps))
           (RL (reverse LR))
           (TB (span-points t b steps))
           (BT (reverse TB))
           (L  (make-list (1+ steps) l))
           (R  (make-list (1+ steps) r))
           (T  (make-list (1+ steps) t))
           (B  (make-list (1+ steps) b))
           (d! (line!-proc (random #xffffff))))
      (map d! LR T R TB)
      (map d! R TB RL B)
      (map d! RL B L BT)
      (map d! L BT LR T)))

  (mesh start-l start-r start-t start-b 20)
  (mesh (+ start-l 75) (- start-r 75) (+ start-t 75) (- start-b 75) 15)
  (mesh (+ start-l 150) (- start-r 150) (+ start-t 150) (- start-b 150) 10))

(do ((i 0 (1+ i)))
    ((= i 30))
  (one (+ 10 i) (- 630 i) (+ 10 i) (- 470 i))
  (SDL:flip))

;; draw ellipses
(do ((x-radius 320 (1- x-radius))
     (y-radius 240 (1- y-radius)))
    ((= 200 y-radius))
  (SDL:draw-ellipse SCREEN 320 240 x-radius y-radius
                    (+ (ash (random #xffffff) 8) 1)
                    #t)
  (SDL:flip))

;; draw circles
(do ((radius 100 (1- radius)))
    ((> 70 radius))
  (let ((c! (lambda (x y)
              (SDL:draw-circle SCREEN x y radius
                               (+ (ash (random #xffffff) 8) 1)
                               #t))))
    (c! 100 100)
    (c! 100 380)
    (c! 540 380)
    (c! 540 100))
  (SDL:flip))

;; draw pie slices
(let ((w (SDL:surface:w SCREEN))
      (h (SDL:surface:h SCREEN)))
  (do ((slice 0 (1+ slice)))
      ((= 42 slice))
    (let* ((x (random w))
           (y (random h))
           (color (ash (random #xffffff) 8))
           (beg (random 360))
           (sub (quotient (- (+ beg (random 360)) beg) 16)))
      (do ((i 0 (1+ i)))
          ((= i 16))
        (SDL:draw-pie-slice SCREEN
                            x y (min x (- w x) y (- h y))
                            (+ beg (* sub i)) (+ beg (* sub (1+ i)))
                            (+ (* 9 i) color)
                            #t))
      (SDL:flip))))

;; draw polygons
(do ((poly 0 (1+ poly)))
    ((= 10 poly))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-vector n 0))
         (y-uv (make-vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (vector-set! x-uv i (random 640))
      (vector-set! y-uv i (random 480)))
    (SDL:draw-polygon SCREEN x-uv y-uv
                      (+ #x80 (ash (random #xffffff) 8))
                      #t))
  (SDL:flip))

;; draw textured polygons
(do ((poly 0 (1+ poly)))
    ((= 10 poly))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-vector n 0))
         (y-uv (make-vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (vector-set! x-uv i (random 640))
      (vector-set! y-uv i (random 480)))
    (SDL:draw-textured-polygon SCREEN x-uv y-uv
                               SCREEN (random 640) (random 480)))
  (SDL:flip))

;; clean up
(SDL:delay 2000)
(SDL:quit)

;;; gfx.scm ends here
