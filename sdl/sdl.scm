;; sdl.scm -- SDL for Guile                       -*- Guile-Scheme -*-
;;
;; Copyright (C) 2001 Alex Shinn <foof@debian.org>
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

(define-module (sdl sdl))

(use-modules (ice-9 syncase))

(export-syntax sdl-with-clip-rect)

(define sdl-version "0.1.0")

(if (and (dynamic-object? (dynamic-link "libpthread"))
         (dynamic-object? (dynamic-link "libSDL"))
         (dynamic-object? (dynamic-link "libSDL_image")))
         ;(dynamic-object? (dynamic-link "libSDL_ttf"))
         ;(dynamic-object? (dynamic-link "libSDL_mixer")))
    (let ((lib (dynamic-link "libguileSDL")))
      (if (dynamic-object? lib)
          (dynamic-call "guile_sdl_init" lib)
          (error "could not find libguileSDL") ))
    (error "could not init libSDL or libpthread") )


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

