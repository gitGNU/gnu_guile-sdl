#! /usr/local/bin/guile -s
!#

;; simple image test
;; 
;; Created:    <2001-05-29 20:38:26 foof>
;; Time-stamp: <2001-06-02 22:04:48 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules ((sdl sdl)
              :renamer (symbol-prefix-proc 'sdl-)))

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; the size of our test image
(define gnu-rect (make-rect 0 0 200 153))

;; initialize the SDL video module
(init init-video)

;; set the video mode to the dimensions of our image
(set-video-mode 200 153 16 1)

;; load and blit the image
(let ((gnu-head (load-image (string-append datadir "gnu-goatee.jpg"))))
  (blit-surface gnu-head gnu-rect (get-video-surface) gnu-rect))

;; flip the double buffer
(flip (get-video-surface))

;; wait a second then quit
(sleep 1)
(quit-sdl)

