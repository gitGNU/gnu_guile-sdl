#! /usr/local/bin/guile -s
!#

;; simple true type font test

(use-modules (sdl sdl)
             (sdl gfx))

;; initialize SDL video
(sdl-init '(SDL_INIT_VIDEO))

;; initialize the video mode
(define test-rect (sdl-make-rect 0 0 640 480))
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16)

(seed->random-state (sdl-get-ticks))

;; clear the screen
(sdl-fill-rect (sdl-get-video-surface) test-rect #xffffff)
(sdl-flip)

;; draw a line
(sdl-draw-line (sdl-get-video-surface) 10 10 630 870 (random #xffff))
(sdl-flip)

;; clean up
(sdl-delay 1000)
(sdl-quit)

