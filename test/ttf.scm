#! /usr/local/bin/guile -s
!#

;; simple true type font test
;; 
;; Created:    <2001-06-10 19:14:30 foof>
;; Time-stamp: <2001-06-25 01:03:07 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules (sdl sdl)
             (sdl ttf)
             (ice-9 format))

;; initialize SDL video
(sdl-init sdl-init/video)

;; initialize the font lib
(sdl-ttf-init)

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; the text to display
(define sentence "The quick brown fox jumped over the lazy sleeping dog.")

;; load a font file
(define font (sdl-load-font (string-append datadir "crystal.ttf") 16))

;; initialize the video mode
(define test-rect (sdl-make-rect 0 0 640 480))
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16 1)

;; clear the rect and write the text
(let ((screen (sdl-get-video-surface))
      (text (sdl-render-text font sentence (sdl-make-color 0 0 0))))
  (sdl-fill-rect screen test-rect #xffffff)  
  (sdl-blit-surface text test-rect screen test-rect))

;; flip and wait
(sdl-flip)
(sdl-delay 1000)

;; clean up
(sdl-ttf-quit)
(sdl-quit)

