;;; rect.scm --- simple rectangle test

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

(and debug?
     (for-each (lambda (x) (format #t "video-info: ~S\n" x))
               (SDL:get-video-info)))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (and debug? (format #t "hmm: (SDL:list-modes) => ~A\n" modes))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (SDL:make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))
(and debug? (format #t "test-rect => ~A\n" test-rect))

(seed->random-state (SDL:get-ticks))

(define (rand-rect limit)
  (let ((x (+ (SDL:rect:x limit) (random (SDL:rect:w limit))))
        (y (+ (SDL:rect:y limit) (random (SDL:rect:h limit)))))
    (SDL:make-rect x y
                   (random (1+ (- (SDL:rect:w limit) x)))
                   (random (1+ (- (SDL:rect:h limit) y))) )))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16
                    '(SDL_HWSURFACE))

;; draw some rectangles filled with random colors
(let ((src-rect (SDL:make-surface (SDL:rect:w test-rect)
                                  (SDL:rect:h test-rect))))
  (do ((i 0 (1+ i)))
      ((> i 10))
    (let ((c (random #xffffff)))
      (SDL:call-with-clip-rect
       (rand-rect test-rect)
       (lambda ()
         (SDL:fill-rect src-rect test-rect c)
         (SDL:blit-surface src-rect)
         (SDL:flip)
         (SDL:delay 100))))))

;; quit SDL
(SDL:quit)

;;; rect.scm ends here
