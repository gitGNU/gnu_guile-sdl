;;; gfx.scm --- simple graphics primitives test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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
(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl gfx) #:prefix SDL:))

;; initialize SDL video
(SDL:init '(SDL_INIT_VIDEO))

;; initialize the video mode
(SDL:set-video-mode 640 480 16)

(set! *random-state* (seed->random-state (current-time)))

(define SCREEN (SDL:get-video-surface))

;; clear the screen
(SDL:fill-rect SCREEN #f #xffffff)
(SDL:flip)

;; character (font) stuff prep
(define draw-characters!
  (let ((max-x (- 640 8))
        (max-y (- 480 8))
        (all (map integer->char (iota 256))))
    (define (one char)
      (SDL:draw-character SCREEN (random max-x) (random max-y) char
                          (+ #xff (ash (random #x1000000) 8))))
    (lambda ()
      (for-each one (list-tail all (random (- 256 (random 256))))))))

(define mmx? (SDL:imfi-mmx?))
(and debug? (fso "mmx: ~A~%" (if mmx? 'yes 'no)))
(and (getenv "NOMMX") (SDL:imfi-mmx? #f))

;; character (font) stuff and blitting
(let* ((screen (SDL:get-video-surface))
       (cx (ash (SDL:surface:w screen) -1))
       (cy (ash (SDL:surface:h screen) -1))
       (head-file (datafile "gnu-goatee.jpg"))
       (head (SDL:load-image head-file))
       (iw (SDL:surface:w head))
       (ih (SDL:surface:h head))
       (lox (/ (- 640 (* 2 iw)) 3))
       (hix (+ lox iw lox))
       (loy (/ (- 480 (* 2 ih)) 3))
       (hiy (+ loy ih loy))
       (frot (list->vector (SDL:enumstash-enums SDL:font-rotations)))
       (frot-count (vector-length frot))
       (place (SDL:make-rect 0 0 iw ih)))
  (define (draw! n)
    (SDL:rect:set-x! place (if (zero? (logand n 1)) lox hix))
    (SDL:rect:set-y! place (if (zero? (logand n 2)) loy hiy))
    (let ((rot (vector-ref frot (random frot-count))))
      (SDL:font-rotation! rot)
      (draw-characters!)
      (SDL:draw-string screen
                       (+ cx (* 20 (case rot
                                     ((clockwise) (random 10))
                                     ((counter-clockwise) (- (random 10)))
                                     (else 0))))
                       (+ cy (* 20 (case rot
                                     ((none) (random 10))
                                     ((upside-down) (- (random 10)))
                                     (else 0))))
                       "ZOW!" #xff0000ff))
    (SDL:blit-surface head #f screen place))
  (define (two-op op)
    (op head head))
  (define (three-op op)
    (op head head head))
  (define (three-op-c op c)
    (op head head (random c)))
  (let ((v (vector
            (lambda () (three-op SDL:imfi-add))
            (lambda () (three-op SDL:imfi-mean))
            (lambda () (three-op SDL:imfi-sub))
            (lambda () (three-op SDL:imfi-abs-diff))
            (lambda () (three-op SDL:imfi-mult))
            (lambda () (three-op SDL:imfi-mulnor))
            (lambda () (three-op SDL:imfi-muldiv2))
            (lambda () (three-op SDL:imfi-muldiv4))
            (lambda () (three-op SDL:imfi-logand))
            (lambda () (three-op SDL:imfi-logior))
            (lambda () (SDL:imfi-add-c head head 1) ; avoid div-by-zero
                    (three-op SDL:imfi-div))
            (lambda () (two-op SDL:imfi-not))
            (lambda () (three-op-c SDL:imfi-add-c 128))
            (lambda () (three-op-c SDL:imfi-add-c-to-half 128))
            (lambda () (three-op-c SDL:imfi-sub-c 128))
            (lambda () (three-op-c SDL:imfi-ashr 32))
            (lambda () (three-op-c SDL:imfi-lshr 32))
            (lambda () (three-op-c SDL:imfi-mul-c 8))
            (lambda () (SDL:imfi-ashr-mul-c
                        head head (random 32) (random 128)))
            (lambda () (three-op-c SDL:imfi-bshl 32))
            (lambda () (three-op-c SDL:imfi-lshl 32))
            (lambda () (three-op-c SDL:imfi-ashl 32))
            (lambda () (three-op-c SDL:imfi-binarize 256))
            (lambda () (SDL:imfi-clip
                        head head (random 128) (+ 128 (random 128))))
            (lambda () (SDL:imfi-normalize-linear
                        head head
                        (random 256) (random 256)
                        (random 256) (random 256))))))
    (do ((i 0 (1+ i)))
        ((= 100 i))
      (if (zero? (random 5))
          (set! head (SDL:load-image head-file))
          ((vector-ref v (random (vector-length v)))))
      (draw! i)
      (SDL:flip))))

;; draw beziers
(do ((bez 0 (1+ bez)))
    ((= 100 bez))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-s16vector n 0))
         (y-uv (make-s16vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (s16vector-set! x-uv i (random 640))
      (s16vector-set! y-uv i (random 480)))
    (SDL:draw-bezier SCREEN x-uv y-uv 5
                     (+ #x80 (ash (random #xffffff) 8))))
  (SDL:flip))

;; draw horizontal and vertical lines
(let ((w (SDL:surface:w SCREEN))
      (h (SDL:surface:h SCREEN)))
  (do ((i 0 (1+ i)))
      ((= i h))
    (SDL:draw-hline SCREEN (random w) (random w) i (random #xffffff)))
  (do ((i 0 (1+ i)))
      ((= i w))
    (SDL:draw-vline SCREEN i (random h) (random h) (random #xffffff)))
  (SDL:flip))

;; draw lines
(define (one start-l start-r start-t start-b)

  (define (line!-proc color)
    (let ((barest-hint (logior #x08 (logand color (lognot #xff)))))
      (lambda (x1 y1 x2 y2)
        (SDL:draw-thick-line SCREEN x1 y1 x2 y2 7 barest-hint)
        (SDL:draw-line SCREEN x1 y1 x2 y2 color))))

  (define (span-points beg end steps)
    (define (integer<- x)
      (inexact->exact (round x)))
    (let ((inc (/ (- end beg) steps)))
      (let loop ((ls (list end)))
        (if (<= (inexact->exact (car ls)) beg)
            (map integer<- ls)
            (loop (cons (- (car ls) inc) ls))))))

  (define (mesh l r t b steps)
    (let* ((LR (span-points l r steps))
           (RL (reverse LR))
           (TB (span-points t b steps))
           (BT (reverse TB))
           (L  (make-list (1+ steps) l))
           (R  (make-list (1+ steps) r))
           (T  (make-list (1+ steps) t))
           (B  (make-list (1+ steps) b))
           (d! (line!-proc (random #xffffff))))
      (map d! LR T R TB)
      (map d! R TB RL B)
      (map d! RL B L BT)
      (map d! L BT LR T)))

  (mesh start-l start-r start-t start-b 20)
  (mesh (+ start-l 75) (- start-r 75) (+ start-t 75) (- start-b 75) 15)
  (mesh (+ start-l 150) (- start-r 150) (+ start-t 150) (- start-b 150) 10))

(do ((i 0 (1+ i)))
    ((= i 30))
  (one (+ 10 i) (- 630 i) (+ 10 i) (- 470 i))
  (SDL:flip))

;; draw ellipses
(do ((x-radius 320 (1- x-radius))
     (y-radius 240 (1- y-radius)))
    ((= 200 y-radius))
  (SDL:draw-ellipse SCREEN 320 240 x-radius y-radius
                    (+ (ash (random #xffffff) 8) 1)
                    #t)
  (SDL:flip))

;; draw circles
(do ((radius 100 (1- radius)))
    ((> 70 radius))
  (let ((c! (lambda (x y)
              (SDL:draw-circle SCREEN x y radius
                               (+ (ash (random #xffffff) 8) 1)
                               #t))))
    (c! 100 100)
    (c! 100 380)
    (c! 540 380)
    (c! 540 100))
  (SDL:flip))

;; draw pie slices and arcs
(let ((w (SDL:surface:w SCREEN))
      (h (SDL:surface:h SCREEN)))
  (do ((slice 0 (1+ slice)))
      ((= 42 slice))
    (let* ((x (random w))
           (y (random h))
           (r (min x (- w x) y (- h y)))
           (color (ash (random #xffffff) 8))
           (beg (random 360))
           (sub (quotient (- (+ beg (random 360)) beg) 16)))
      (do ((i 0 (1+ i)))
          ((= i 16))
        (SDL:draw-pie-slice SCREEN
                            x y r
                            (+ beg (* sub i)) (+ beg (* sub (1+ i)))
                            (+ (* 9 i) color)
                            #t)
        (SDL:draw-arc SCREEN
                      x y (- r (quotient (* r i) 16))
                      (+ beg (* sub i)) (+ beg (* sub (1+ i)))
                      (logior color #xff)))
      (SDL:draw-arc SCREEN
                    x y r
                    beg (+ beg (* sub 16))
                    (logior color #xff))
      (SDL:flip))))

;; draw polygons
(do ((poly 0 (1+ poly)))
    ((= 10 poly))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-s16vector n 0))
         (y-uv (make-s16vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (s16vector-set! x-uv i (random 640))
      (s16vector-set! y-uv i (random 480)))
    (SDL:draw-polygon SCREEN x-uv y-uv
                      (+ #x80 (ash (random #xffffff) 8))
                      #t))
  (SDL:flip))

;; draw textured polygons
(do ((poly 0 (1+ poly)))
    ((= 10 poly))
  (let* ((n (+ 3 (random 5)))
         (x-uv (make-s16vector n 0))
         (y-uv (make-s16vector n 0)))
    (do ((i 0 (1+ i)))
        ((= i n))
      (s16vector-set! x-uv i (random 640))
      (s16vector-set! y-uv i (random 480)))
    (SDL:draw-textured-polygon SCREEN x-uv y-uv
                               SCREEN (random 640) (random 480)))
  (SDL:flip))

;; mess w/ alpha
(let ((s-32 (SDL:create-rgb-surface #f 640 480 32
                                    #xFF000000
                                    #x00FF0000
                                    #x0000FF00
                                    #x000000FF))
      (blot (SDL:create-rgb-surface #f 10 10 32
                                    #xFF000000
                                    #x00FF0000
                                    #x0000FF00
                                    #x000000FF))
      (brect (SDL:make-rect 0 0 10 10))
      (srect (SDL:make-rect 0 0 10 10)))
  (SDL:blit-surface SCREEN #f s-32 #f)
  (do ((i 0 (1+ i)))
      ((= 20 i))
    (do ((x 0 (+ x 10)))
        ((= 640 x))
      (do ((y 0 (+ y 10)))
          ((= 480 y))
        (SDL:rect:set-x! srect x)
        (SDL:rect:set-y! srect y)
        (SDL:blit-rgba s-32 srect blot brect)
        (SDL:set-pixel-alpha! blot (random 192))
        (SDL:blit-surface blot brect SCREEN srect)))
    (SDL:flip)))

;; draw rounded rectangles
(do ((inc 0 (random 8))
     (rad 0 (+ rad inc))
     (x1 0 (+ x1 inc))
     (y1 0 (+ y1 inc))
     (x2 (1- (SDL:surface:w SCREEN)) (- x2 inc))
     (y2 (1- (SDL:surface:h SCREEN)) (- y2 inc)))
    ((<= 240 rad))
  (SDL:draw-rounded-rectangle
   SCREEN x1 y1 x2 y2
   rad (+ #x30 (ash (random #xffffff) 8))
   (zero? (random 8)))
  (SDL:flip))

;; clean up
(SDL:delay 500)
(SDL:quit)

;;; gfx.scm ends here
