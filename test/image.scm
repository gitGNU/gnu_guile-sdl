#! /usr/local/bin/guile -s
!#

;; simple image test
;; 
;; Created:    <2001-05-29 20:38:26 foof>
;; Time-stamp: <2001-06-01 21:39:03 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules ((sdl sdl)
              :renamer (symbol-prefix-proc 'sdl-)))

(init init-video)

(define gnu-rect (make-rect 0 0 200 153))

(set-video-mode 200 153 16 1)

(let ((gnu-head (load-image "gnu-goatee.jpg")))
  (blit-surface gnu-head gnu-rect (get-video-surface) gnu-rect))

(flip (get-video-surface))

(sleep 3)

(quit-sdl)

