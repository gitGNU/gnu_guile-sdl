;;; cursor.scm --- simple cursor and set-gamma-ramp test

;; Copyright (C) 2003, 2004, 2007, 2011, 2012, 2013 Thien-Thi Nguyen
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

(use-modules (srfi srfi-4))
(use-modules ((sdl sdl) #:prefix SDL:))

;; initialize the SDL video module
(SDL:init 'video)

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use 2x2 of gnu-goatee.jpg (200x153)
           (SDL:make-rect 0 0 400 306))
          (else
           ;; a list - choose the first mode
           (car modes)))))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 8
                    '(hw-surface))
(let ((gnu-head (SDL:load-image (datafile "gnu-goatee.jpg")))
      (rect (SDL:make-rect 0 0 200 153)))
  (do ((x 0 (1+ x)))
      ((= 2 x))
    (do ((y 0 (1+ y)))
        ((= 2 y))
      (SDL:rect:set-x! rect (* 200 x))
      (SDL:rect:set-y! rect (* 153 y))
      (or (zero? (logxor x y))
          (SDL:blit-surface gnu-head #f (SDL:get-video-surface) rect)))))
(SDL:flip)

;; move the mouse (rude, but we can live w/ that)
(let ((screen (SDL:get-video-surface)))
  (SDL:warp-mouse (- (ash (SDL:surface:w screen) -1) 4)
                  (- (ash (SDL:surface:h screen) -1) 8)))

;; make sure the cursor is visible
(SDL:show-cursor #t)

(define (spew-ramp r g b)
  (define (hex2 n)
    (let ((s (hex n)))
      (if (> 2 (string-length s))
          (string-append "0" s)
          s)))
  (define (hex4 n)
    (let* ((s (hex n))
           (pad (- 4 (string-length s))))
      (if (zero? pad)
          s
          (string-append (make-string pad #\0) s))))
  (info "(gamma ramp)\tr\tg\tb")
  (do ((i 0 (1+ i)))
      ((= 256 i))
    (info "\t~A\t~A\t~A\t~A"
         (hex2 i)
         (hex4 (u16vector-ref r i))
         (hex4 (u16vector-ref g i))
         (hex4 (u16vector-ref b i)))))

(apply spew-ramp (SDL:get-gamma-ramp))

;; primitive cursor creation (need a higher level one)
(define (random-16)
  (list->u8vector (map (lambda ignored
                         (random 256))
                       (iota 16))))
;; gyrate
(do ((i 0 (1+ i)))
    ((= 50 i))
  (SDL:set-cursor (SDL:create-cursor (random-16) (random-16) 8 16 0 0))
  (and (zero? (modulo i 5))
       (let ((rt (make-u16vector 256))
             (gt (make-u16vector 256))
             (bt (make-u16vector 256)))
         (SDL:show-cursor (not (SDL:show-cursor)))
         (do ((j 0 (1+ j)))
             ((= 256 j))
           (u16vector-set! rt j (random 65536))
           (u16vector-set! gt j (random 65536))
           (u16vector-set! bt j (random 65536)))
         (SDL:set-gamma-ramp rt gt bt)))
  (SDL:delay 50))

(let* ((data (u8vector 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85))
       (mask data)
       (cursor (SDL:create-cursor data mask 8 16 0 0)))
  (SDL:set-cursor cursor))

(SDL:delay 420)

;; quit SDL
(exit (SDL:quit))

;;; cursor.scm ends here
