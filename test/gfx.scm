#!/bin/sh
exec ${GUILE-guile} -s $0 "$@" # -*-scheme-*-
!#
(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

;; simple graphics primitives test

(use-modules (sdl sdl))                 ; fixme: these must be separate due
(use-modules (sdl gfx))                 ;        to compiled modules weirdness

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

;;; gfx.scm ends here
