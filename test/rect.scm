#! /usr/local/bin/guile -s
!#

;; simple rectangle test
;; 
;; Created:    <2001-06-02 21:47:41 foof>
;; Time-stamp: <2001-06-04 00:24:09 foof>
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

;; fill the rectangle with a few colors
(for-each
 (lambda (c)
   (sdl-fill-rect (sdl-get-video-surface) test-rect c)
   (sdl-flip)
   (usleep 500000))
 ;; random values - need to use real colors
 '(#xffffff #xcc00cc #x00ff00 #x0000ff))

;; quit SDL
(sdl-quit-all)

