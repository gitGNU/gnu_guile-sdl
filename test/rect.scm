;;; rect.scm --- simple rectangle test

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules (sdl sdl))

;; initialize the SDL video module
(sdl-init '(SDL_INIT_VIDEO))

(and debug?
     (for-each (lambda (x) (format #t "video-info: ~S\n" x))
               (sdl-get-video-info)))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (sdl-list-modes)))
    (and debug? (format #t "hmm: (sdl-list-modes) => ~A\n" modes))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (sdl-make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))
(and debug? (format #t "test-rect => ~A\n" test-rect))

(seed->random-state (sdl-get-ticks))

(define (rand-rect limit)
  (let ((x (+ (sdl-rect:x limit) (random (sdl-rect:w limit))))
        (y (+ (sdl-rect:y limit) (random (sdl-rect:h limit)))))
    (sdl-make-rect x y
                   (random (1+ (- (sdl-rect:w limit) x)))
                   (random (1+ (- (sdl-rect:h limit) y))) )))

;; set the video mode to the dimensions of our rect
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16
                    '(SDL_HWSURFACE))

;; draw some rectangles filled with random colors
(let ((src-rect (sdl-make-surface (sdl-rect:w test-rect)
                                  (sdl-rect:h test-rect))))
  (do ((i 0 (1+ i)))
      ((> i 10))
    (let ((c (random #xffffff)))
      (sdl-with-clip-rect (rand-rect test-rect)
        (sdl-fill-rect src-rect test-rect c)
        (sdl-blit-surface src-rect)
        (sdl-flip)
        (sdl-delay 100)))))

;; quit SDL
(sdl-quit)

;;; rect.scm ends here
