;;; sdl.scm --- SDL for Guile

;; 	Copyright (C) 2003 Thien-Thi Nguyen
;; 	Copyright (C) 2001 Alex Shinn <foof@debian.org>
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.  */

;;; Commentary:

;;; Code:

(define-module (sdl sdl)
  #:use-module (sdl sdl-sup)
  #:use-module (ice-9 syncase))

;; first things first: jump in the air a little and forget to hit the ground

(set-module-public-interface!
 (current-module)
 (module-public-interface (resolve-module '(sdl sdl-sup))))

;;; some utility functions, need to organize these into modules

;; perform a thunk with a clipping rect temporarily in place
(define-syntax sdl-with-clip-rect
  (syntax-rules ()
    ((_ rect body ...)
     (let ((orig-rect (sdl-get-clip-rect (sdl-get-video-surface))))
       (sdl-set-clip-rect! (sdl-get-video-surface) rect)
       body ...
       (sdl-set-clip-rect! (sdl-get-video-surface) orig-rect)))))


;; rotate a square retaining its original size
(define (sdl-rotate-square src angle)
  (let* ((width (sdl-surface:w src))
         (height (sdl-surface:h src))
         (rotated (sdl-roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (sdl-surface:w rotated))
         (new-height (sdl-surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (sdl-make-rect width-offset height-offset width height))
         (dst (sdl-make-surface width height))
         (dst-rect (sdl-make-rect 0 0 width height)))
    (sdl-blit-surface rotated src-rect dst dst-rect)
    dst))

(export-syntax sdl-with-clip-rect)

(export sdl-rotate-square)

(define-public sdl-wait-event wait-event)

;;; sdl.scm ends here
