;;; image.scm --- simple image test

;; Copyright (C) 2003, 2004, 2009, 2011, 2013 Thien-Thi Nguyen
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
(use-modules ((sdl misc-utils) #:select (exact-truncate)))
(use-modules ((srfi srfi-4) #:select (u8vector?
                                      u8vector-length
                                      u8vector-ref
                                      u16vector?
                                      u16vector-length
                                      u16vector-ref
                                      u32vector?
                                      u32vector-length
                                      u32vector-ref)))
(use-modules ((ice-9 format) #:select (format)))

;; the size of our test image
(define gnu-rect (SDL:make-rect 0 0 200 153))

;; initialize the SDL video module
(SDL:init 'video)

;; set the video mode to the dimensions of our image
(define screen (SDL:set-video-mode 200 153 32))

(define (check-depth what surface)
  (let ((exp (SDL:surface:depth screen))
        (got (SDL:surface:depth surface)))
    (or (= exp got)
        (error (fs "~A depth (~A) different than screen (~A)"
                   what got exp)))))

(define (standard-blit! surface)
  (SDL:blit-surface surface gnu-rect screen gnu-rect))

;; load and blit the image
(define gnu-head (SDL:load-image (datafile "gnu-goatee.jpg")))
(info "gnu-head => ~S" gnu-head)

;; pixel data
(SDL:surface-color-key! gnu-head #xFEFEFE #t)
(let* ((w (SDL:surface:w gnu-head))
       (h (SDL:surface:h gnu-head))
       (exp-len (* w h))
       (interesting (map (lambda (n)
                           (exact-truncate (* 0.1 n exp-len)))
                         (iota 10))))

  (define (resampled depth)
    (let ((rv (apply SDL:create-rgb-surface
                     #f w h
                     (assq depth '((8 #x03
                                      #x0c
                                      #x30
                                      #xc0)
                                   (16 #x000f
                                       #x00f0
                                       #x0f00
                                       #xf000)
                                   (24 #x000000ff
                                       #x0000ff00
                                       #x00ff0000
                                       #x00000000)
                                   (32 #x000000ff
                                       #x0000ff00
                                       #x00ff0000
                                       #xff000000))))))
      (SDL:blit-surface gnu-head #f rv)
      (standard-blit! rv)
      (SDL:flip)
      rv))

  (define (try depth)
    (let* ((stage (resampled depth))
           (v (SDL:surface-pixels stage))
           (len (cond ((u8vector? v) (u8vector-length v))
                      ((u16vector? v) (u16vector-length v))
                      ((u32vector? v) (u32vector-length v))
                      (else #f))))
      (info "~Sbpp, ~A elems" depth (or len 'unavailable))
      (or (eqv? len exp-len)
          (error (fs "unexpected length u~Avector length: ~S"
                     depth len)))
      (for-each (lambda (i)
                  (info "\t\t~A\t~A" i
                        (hex ((cond ((u8vector? v) u8vector-ref)
                                    ((u16vector? v) u16vector-ref)
                                    ((u32vector? v) u32vector-ref)
                                    (else #f))
                              v i))))
                interesting)
      (SDL:delay 20)))

  (let ((v (SDL:surface-pixels gnu-head #t)))
    (info "squashed, len:\t~S" (if (u8vector? v)
                                   (u8vector-length v)
                                   'unavailable)))
  (try 8)
  (try 16)
  (try 24)
  (try 32))
(SDL:set-color-key! gnu-head #f 0) ; aka (SDL:surface-color-key! gnu-head #f)
(standard-blit! gnu-head)

;; flip the double buffer
(SDL:flip screen)

;; convert explicitly
(SDL:delay 100)
(let ((orig (SDL:surface-get-format gnu-head))
      (want (SDL:surface-get-format screen)))

  (define (show! correct)
    (check-depth 'correct correct)
    (SDL:fill-rect screen #f #x880088)
    (SDL:flip)
    (SDL:delay 50)
    (standard-blit! correct)
    (SDL:flip)
    (SDL:delay 50))

  (info "orig format => ~S" orig)
  (info "want format => ~S" want)
  (show! (SDL:convert-surface gnu-head want))
  (show! (SDL:display-format-alpha gnu-head)))

;; wait a half-second, then flip it upside-down
(SDL:delay 500)
(let ((upside-down (SDL:vertical-flip-surface screen)))
  (check-depth 'upside-down upside-down)
  (standard-blit! upside-down))
(SDL:flip screen)

;; now flip horizontally
(SDL:delay 500)
(let ((left-right (SDL:horizontal-flip-surface screen)))
  (check-depth 'left-right left-right)
  (standard-blit! left-right))
(SDL:flip screen)

;; ... and finally flip back
(SDL:delay 500)
(let ((orig (SDL:vh-flip-surface screen)))
  (check-depth 'orig orig)
  (standard-blit! orig))
(SDL:flip screen)

;; clipping rectangle
(SDL:delay 500)
(let ((cr (SDL:get-clip-rect screen)))
  (or cr (error "get-clip-rect failed to return a rect"))
  (or (equal? '(200 153 0 0)
              (map (lambda (proc)
                     (proc cr))
                   (list SDL:rect:w
                         SDL:rect:h
                         SDL:rect:x
                         SDL:rect:y)))
      (error "unexpected clip rect on screen:" cr)))
(SDL:fill-rect screen #f #x880088)
(let ((w (SDL:rect:w gnu-rect))
      (h (SDL:rect:h gnu-rect)))
  (do ((i 0 (1+ i)))
      ((= 9 i))
    (let* ((x (random w))
           (y (random h)))
      (SDL:set-clip-rect! screen (SDL:make-rect x y
                                                (random (- w x))
                                                (random (- h y))))
      (SDL:blit-surface gnu-head #f screen)
      (SDL:flip)
      (SDL:delay 40))))
(SDL:set-clip-rect! screen #f)

;; locking
(info "lock-surface => ~S" (SDL:lock-surface gnu-head))
(info "unlock-surface => ~S" (SDL:unlock-surface gnu-head))

;; alpha
(SDL:delay 200)
(do ((i 0 (1+ i))
     (alpha 9 (+ 9 alpha)))
    ((= 9 i))
  (SDL:surface-alpha! gnu-head alpha)
  (SDL:set-alpha! gnu-head 'src-alpha alpha)
  (standard-blit! gnu-head)
  (SDL:flip)
  (SDL:delay 20))

;; round-trip via a .bmp file
(SDL:delay 500)
(let ((filename "image.bmp"))
  (SDL:save-bmp screen filename)
  (SDL:fill-rect screen gnu-rect 0)
  (SDL:update-rect screen gnu-rect)
  (SDL:delay 100)
  (standard-blit! (SDL:load-bmp filename))
  (SDL:flip)
  (delete-file filename))

;; wait then quit
(SDL:delay 500)
(exit (SDL:quit))

;;; image.scm ends here
