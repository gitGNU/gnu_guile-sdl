;;; roto.scm --- simple rotozoom test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2011 Thien-Thi Nguyen
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

(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl gfx) #:prefix GFX:))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; load the image
(define gnu-head (SDL:load-image (datafile "gnu-goatee.jpg")))

(define-macro (spin theta-update mag-vars turns roto wait)
  ;; the size of our test image
  `(let ((w (SDL:surface:w gnu-head))
         (h (SDL:surface:h gnu-head))
         (screen #f))
     ;; set the video mode to the dimensions of our image
     (SDL:set-video-mode w h 16 '(SDL_HWSURFACE))
     (set! screen (SDL:get-video-surface))
     (do ((theta 0 (+ ,theta-update theta)) ,@mag-vars)
         ((>= theta (* ,turns 360)))    ; a few times around
       (let* ((image ,roto)
              (iw    (SDL:surface:w image))
              (ih    (SDL:surface:h image))
              (drect (SDL:make-rect (quotient (- w iw) 2)
                                    (quotient (- h ih) 2)
                                    iw ih)))
         (SDL:fill-rect screen #f #xffff)
         (SDL:blit-surface image #f #f drect)
         (SDL:flip)
         (SDL:delay ,wait)))))

(spin 27 ((mag 1.0 (* mag 0.9)))
      3 (GFX:roto-zoom-surface gnu-head theta mag #t)
      50)

(spin (+ 10 (random 17)) ((magx 1.0 (* magx (+ 0.75 (random 0.5))))
                          (magy 1.0 (* magy (+ 0.75 (random 0.5)))))
      10 (GFX:roto-zoom-surface-xy gnu-head theta magx magy #t)
      10)

;; quit
(exit (SDL:quit))

;;; roto.scm ends here
