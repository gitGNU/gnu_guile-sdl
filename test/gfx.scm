;;; gfx.scm --- simple graphics primitives test

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl gfx) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL video
(SDL:init '(SDL_INIT_VIDEO))

;; initialize the video mode
(define test-rect (SDL:make-rect 0 0 640 480))
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16)

(seed->random-state (SDL:get-ticks))

;; clear the screen
(SDL:fill-rect (SDL:get-video-surface) test-rect #xffffff)
(SDL:flip)

;; draw lines
(define (one start-l start-r start-t start-b)
  (let ((surface (SDL:get-video-surface)))

    (define (line!-proc color)
      (lambda (x1 y1 x2 y2)
        (SDL:draw-line surface x1 y1 x2 y2 color)))

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
    (mesh (+ start-l 150) (- start-r 150) (+ start-t 150) (- start-b 150) 10)))

(do ((i 0 (1+ i)))
    ((= i 30))
  (one (+ 10 i) (- 630 i) (+ 10 i) (- 470 i))
  (SDL:flip))

;; draw ellipses
(do ((x-radius 320 (1- x-radius))
     (y-radius 240 (1- y-radius)))
    ((= 200 y-radius))
  (SDL:draw-ellipse (SDL:get-video-surface) 320 240 x-radius y-radius
                    (+ (ash (random #xffffff) 8) 1)
                    #t)
  (SDL:flip))

;; draw circles
(do ((radius 100 (1- radius)))
    ((> 70 radius))
  (let ((c! (lambda (x y)
              (SDL:draw-circle (SDL:get-video-surface) x y radius
                               (+ (ash (random #xffffff) 8) 1)
                               #t))))
    (c! 100 100)
    (c! 100 380)
    (c! 540 380)
    (c! 540 100))
  (SDL:flip))

;; clean up
(SDL:delay 2000)
(SDL:quit)

;;; gfx.scm ends here
