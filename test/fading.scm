;;; fading.scm --- iterative alpha blending

;; Copyright (C) 2005, 2007, 2009, 2011, 2013 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

(use-modules
 ((sdl misc-utils) #:select (copy-surface fader/3p))
 ((sdl simple) #:select (simple-canvas))
 ((sdl sdl) #:prefix SDL:)
 ((sdl gfx) #:prefix GFX:))

(define (as-four surface)
  (let* ((w (SDL:surface:w surface))
         (h (SDL:surface:h surface))
         (w/2 (half w))
         (h/2 (half h))
         (hoh (GFX:shrink-surface surface 2 2)) ; half-of-half
         (result (SDL:display-format (SDL:make-surface w h)))
         (drect (SDL:make-rect 0 0 w/2 h/2)))
    (define (move/blit! munge value)
      (and munge (munge drect value))
      (SDL:blit-surface hoh #f result drect))
    (move/blit! #f #f)
    (move/blit! SDL:rect:set-x! w/2)
    (move/blit! SDL:rect:set-y! h/2)
    (move/blit! SDL:rect:set-x! 0)
    result))

;; do it!
(let* ((canvas (simple-canvas #t 200 153 24))
       (img1 (SDL:load-image (datafile "gnu-goatee.jpg")))
       (img2 (as-four img1))
       (img3 (as-four img2))
       (void (SDL:make-surface 200 153)))
  (define (fade/wait! bef aft)
    (call-with-values (lambda ()
                        (fader/3p 0.420 (canvas) #f bef aft))
      (lambda (init! fade! done!)
        (init!)
        (let loop ()
          (and (fade!)
               (loop)))
        (done!)))
    (SDL:delay 420))
  (SDL:fill-rect void #f
                 (SDL:map-rgb (SDL:surface-get-format (canvas)) 0 0 0))
  (SDL:blit-surface img1)
  (SDL:flip)
  (fade/wait! img1 void)                ; out
  (fade/wait! void img1)                ; in
  (fade/wait! img1 img2)                ; out w/ replacement (cross-fade)
  (fade/wait! img2 img3)                ; again!
  (fade/wait! img3 img2)                ; again!
  (fade/wait! img2 img1)                ; again!
  (exit (SDL:quit)))

;;; fading.scm ends here
