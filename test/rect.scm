#! /usr/local/bin/guile -s
!#

;; simple rectangle test
;; 
;; Created:    <2001-06-02 21:47:41 foof>
;; Time-stamp: <2001-06-02 21:54:40 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules ((sdl sdl)
              #:renamer (symbol-prefix-proc 'sdl-)))

;; the size of our rectangle
(define test-rect (make-rect 0 0 320 200))

;; initialize the SDL video module
(init init-video)

;; set the video mode to the dimensions of our image
(set-video-mode 320 200 16 1)

;; fill the rectangle with a few colors
(for-each
 (lambda (c)
   (fill-rect (get-video-surface) test-rect c)
   (flip (get-video-surface))
   (sleep 1))
 ;; random values - need to use real colors
 '(#xffffff #xcc00cc #x00ff00 #x0000ff))

;; quit SDL
(quit-sdl)

