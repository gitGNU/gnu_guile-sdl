#! /usr/local/bin/guile -s
!#

;; simple event test
;; 
;; Created:    <2001-06-04 00:42:41 foof>
;; Time-stamp: <2001-06-18 01:00:19 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules (sdl sdl))

;; initialize the SDL video (and event) module
(sdl-init sdl-init/video)

;; get a sample rect size from a list of available modes
(define test-rect (sdl-make-rect 0 0 320 200))

;; set the video mode to the dimensions of our rect
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16 1)

;; loop until we get a keypress
(define input-loop
  (lambda (e)
    (let* ((next-event (sdl-wait-event e))
           (event-type (sdl-event:type e)))
      (display "type: ")
      (display event-type)
      (newline)
      (if (eq? event-type 'event/key-down)
        (let ((sym (sdl-event:key:keysym:sym e)))
          (display "sym: ")
          (display sym)
          (newline)
          (if (eq? sym 'key/escape)
            (newline)
            (input-loop e)))
        (input-loop e)))))

(input-loop (sdl-make-event 0))

;; quit SDL
(sdl-quit)

