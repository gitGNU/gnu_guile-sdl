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

;; draw a line
(SDL:draw-line (SDL:get-video-surface) 10 10 630 870 (random #xffff))
(SDL:flip)

;; clean up
(SDL:delay 1000)
(SDL:quit)

;;; gfx.scm ends here
