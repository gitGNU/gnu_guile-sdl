;;; image.scm --- simple image test

;; Copyright (C) 2003, 2004, 2009, 2011 Thien-Thi Nguyen
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

(use-modules ((sdl sdl) #:prefix SDL:))

;; the size of our test image
(define gnu-rect (SDL:make-rect 0 0 200 153))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; set the video mode to the dimensions of our image
(define screen (SDL:set-video-mode 200 153 32))

(define (check-depth what surface)
  (let ((exp (SDL:surface:depth screen))
        (got (SDL:surface:depth surface)))
    (or (= exp got)
        (error (fs "~A depth (~A) different than screen (~A)"
                   what got exp)))))

;; load and blit the image
(let ((gnu-head (SDL:load-image (datafile "gnu-goatee.jpg"))))
  (SDL:blit-surface gnu-head gnu-rect screen gnu-rect))

;; flip the double buffer
(SDL:flip screen)

;; wait a half-second, then flip it upside-down
(SDL:delay 500)
(let ((upside-down (SDL:vertical-flip-surface screen)))
  (check-depth 'upside-down upside-down)
  (SDL:blit-surface upside-down gnu-rect screen gnu-rect))
(SDL:flip screen)

;; now flip horizontally
(SDL:delay 500)
(let ((left-right (SDL:horizontal-flip-surface screen)))
  (check-depth 'left-right left-right)
  (SDL:blit-surface left-right gnu-rect screen gnu-rect))
(SDL:flip screen)

;; ... and finally flip back
(SDL:delay 500)
(let ((orig (SDL:vh-flip-surface screen)))
  (check-depth 'orig orig)
  (SDL:blit-surface orig gnu-rect screen gnu-rect))
(SDL:flip screen)

;; round-trip via a .bmp file
(SDL:delay 500)
(let ((filename "image.bmp"))
  (SDL:save-bmp screen filename)
  (SDL:fill-rect screen gnu-rect 0)
  (SDL:update-rect screen gnu-rect)
  (SDL:delay 100)
  (SDL:blit-surface (SDL:load-bmp filename) gnu-rect screen gnu-rect)
  (SDL:flip)
  (delete-file filename))

;; wait then quit
(SDL:delay 500)
(exit (SDL:quit))

;;; image.scm ends here
