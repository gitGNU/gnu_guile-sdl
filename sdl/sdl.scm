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

;;;; perform a thunk with a clipping rect temporarily in place
;;(define-syntax with-clip-rect
;;  (syntax-rules ()
;;    ((_ rect body ...)
;;     (let ((orig-rect (get-clip-rect (get-video-surface))))
;;       (set-clip-rect! (get-video-surface) rect)
;;       body ...
;;       (set-clip-rect! (get-video-surface) orig-rect)))))

(define (call-with-clip-rect rect thunk)
  (let* ((s (get-video-surface))
         (orig (get-clip-rect s)))
    (set-clip-rect! s rect)
    (thunk)
    (set-clip-rect! s orig)))

;; rotate a square retaining its original size
(define (rotate-square src angle)
  (let* ((width (surface:w src))
         (height (surface:h src))
         (rotated (roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (surface:w rotated))
         (new-height (surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (make-rect width-offset height-offset width height))
         (dst (make-surface width height))
         (dst-rect (make-rect 0 0 width height)))
    (blit-surface rotated src-rect dst dst-rect)
    dst))

(export call-with-clip-rect)

(export rotate-square)

;;; sdl.scm ends here
