#! /usr/local/bin/guile -s
!#

;; simple cursor test
;; 
;; Created:    <2001-06-13 00:38:13 foof>
;; Time-stamp: <2001-06-13 00:45:03 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules ((sdl sdl)
              :rename (symbol-prefix-proc 'sdl-)))

;; initialize the SDL video module
(sdl-init sdl-init/video)

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (sdl-list-modes)))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (sdl-make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))

;; set the video mode to the dimensions of our rect
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16 1)
(sdl-fill-rect (sdl-get-video-surface) test-rect #xffffff)
(sdl-flip)

;; primitive cursor creation (need a higher level one)
(let* ((data #(85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85))
       (mask data)
       (cursor (sdl-create-cursor data mask 8 16 0 0)))
  (sdl-set-cursor cursor))

(sdl-delay 1000)

;; quit SDL
(sdl-quit-all)

