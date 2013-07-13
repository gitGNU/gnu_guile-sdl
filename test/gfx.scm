;;; gfx.scm --- simple graphics primitives test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009,
;;   2011, 2013 Thien-Thi Nguyen
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
             ((sdl gfx) #:prefix GFX:))

(define (random-rgb)
  (random #x1000000))

(define (random-rgba alpha)
  (+ alpha (ash (random-rgb) 8)))

;; initialize SDL video
(SDL:init 'video)

(define DEPTH 32)                       ; 16 ok, but slower

;; initialize the video mode
(SDL:set-video-mode 640 480 DEPTH)

(set! *random-state* (seed->random-state (current-time)))

(define SCREEN (SDL:get-video-surface))

(define ?x (let ((w (SDL:surface:w SCREEN))) (lambda () (random w))))
(define ?y (let ((h (SDL:surface:h SCREEN))) (lambda () (random h))))

;; clear the screen
(SDL:fill-rect SCREEN #f #xffffff)
(SDL:flip)

;; character (font) stuff prep
(define draw-characters!
  ;; Although glyph bb is 8x8, the last row and column are always empty.
  ;; So, allow 1-pixel display "outside", for uniform aesthetics "inside".
  ;; (This is apparent only w/ high iteration count.)
  (let ((max-x (- 640 8 -1))
        (max-y (- 480 8 -1))
        (all (map integer->char (iota 256))))
    (define (one char)
      (GFX:draw-character SCREEN (random max-x) (random max-y) char
                          (random-rgba #xff)))
    (lambda ()
      (for-each one (list-tail all (random (- 256 (random 256))))))))

(define mmx? (GFX:imfi-mmx?))
(info "MMX ~AAVAILABLE" (if mmx? "" "NOT "))
(and (getenv "NOMMX") (GFX:imfi-mmx? #f))

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
       (frot (vector 'none 'clockwise 'upside-down 'counter-clockwise))
       (place (SDL:make-rect 0 0 iw ih)))
  (define (draw! n)
    (SDL:rect:set-x! place (if (zero? (logand n 1)) lox hix))
    (SDL:rect:set-y! place (if (zero? (logand n 2)) loy hiy))
    (let ((rot (vector-ref frot (random 4))))
      (GFX:font-rotation! rot)
      (draw-characters!)
      (GFX:draw-string screen
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
            (lambda () (three-op GFX:imfi-add))
            (lambda () (three-op GFX:imfi-mean))
            (lambda () (three-op GFX:imfi-sub))
            (lambda () (three-op GFX:imfi-abs-diff))
            (lambda () (three-op GFX:imfi-mult))
            (lambda () (three-op GFX:imfi-mulnor))
            (lambda () (three-op GFX:imfi-muldiv2))
            (lambda () (three-op GFX:imfi-muldiv4))
            (lambda () (three-op GFX:imfi-logand))
            (lambda () (three-op GFX:imfi-logior))
            (lambda () (GFX:imfi-add-c head head 1) ; avoid div-by-zero
                    (three-op GFX:imfi-div))
            (lambda () (two-op GFX:imfi-not))
            (lambda () (three-op-c GFX:imfi-add-c 128))
            (lambda () (three-op-c GFX:imfi-add-c-to-half 128))
            (lambda () (three-op-c GFX:imfi-sub-c 128))
            (lambda () (three-op-c GFX:imfi-ashr 32))
            (lambda () (three-op-c GFX:imfi-lshr 32))
            (lambda () (three-op-c GFX:imfi-mul-c 8))
            (lambda () (GFX:imfi-ashr-mul-c
                        head head (random 32) (random 128)))
            (lambda () (three-op-c GFX:imfi-bshl 32))
            (lambda () (three-op-c GFX:imfi-lshl 32))
            (lambda () (three-op-c GFX:imfi-ashl 32))
            (lambda () (three-op-c GFX:imfi-binarize 256))
            (lambda () (GFX:imfi-clip
                        head head (random 128) (+ 128 (random 128))))
            (lambda () (GFX:imfi-normalize-linear
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
    (GFX:draw-bezier SCREEN x-uv y-uv 5
                     (random-rgba #x80)))
  (SDL:flip))

;; draw horizontal and vertical lines
(let ((w (SDL:surface:w SCREEN))
      (h (SDL:surface:h SCREEN)))
  (do ((i 0 (1+ i)))
      ((= i h))
    (GFX:draw-hline SCREEN (random w) (random w) i (random-rgb)))
  (do ((i 0 (1+ i)))
      ((= i w))
    (GFX:draw-vline SCREEN i (random h) (random h) (random-rgb)))
  (SDL:flip))

;; draw lines
(define (one start-l start-r start-t start-b)

  (define (line!-proc color)
    (let ((barest-hint (logior #x08 (logand color (lognot #xff)))))
      (lambda (x1 y1 x2 y2)
        (GFX:draw-thick-line SCREEN x1 y1 x2 y2 7 barest-hint)
        (GFX:draw-line SCREEN x1 y1 x2 y2 color))))

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
           (d! (line!-proc (random-rgb))))
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
  (GFX:draw-ellipse SCREEN 320 240 x-radius y-radius
                    (if (= 32 DEPTH)
                        #x040201
                        (random-rgba 1))
                    #t)
  (SDL:flip))

;; draw circles
(do ((radius 100 (1- radius)))
    ((> 70 radius))
  (let ((c! (lambda (x y)
              (GFX:draw-circle SCREEN x y radius
                               (if (= 32 DEPTH)
                                   #x040201
                                   (random-rgba 1))
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
           (color (random-rgba 0))
           (beg (random 360))
           (sub (ash (- (+ beg (random 360)) beg) -4)))
      (do ((i 0 (1+ i)))
          ((= i 16))
        (GFX:draw-pie-slice SCREEN
                            x y r
                            (+ beg (* sub i)) (+ beg (* sub (1+ i)))
                            (+ (* 9 i) color)
                            #t)
        (GFX:draw-arc SCREEN
                      x y (- r (ash (* r i) -4))
                      (+ beg (* sub i)) (+ beg (* sub (1+ i)))
                      (logior color #xff)))
      (GFX:draw-arc SCREEN
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
    (GFX:draw-polygon SCREEN x-uv y-uv
                      (random-rgba #x80)
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
    (GFX:draw-textured-polygon SCREEN x-uv y-uv
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
        (GFX:blit-rgba s-32 srect blot brect)
        (GFX:set-pixel-alpha! blot (random 192))
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
  (GFX:draw-rounded-rectangle
   SCREEN x1 y1 x2 y2
   rad (random-rgba #x30)
   (zero? (random 8)))
  (SDL:flip))

;; singular stuff
(let ((green #x00ff00ff))
  (define (nine gen)
    (list->s16vector (map (lambda ignored
                            (gen))
                          (iota 9))))
  (GFX:draw-point SCREEN (?x) (?y) green)
  (GFX:draw-rectangle SCREEN (?x) (?y) (?x) (?y) green)
  (GFX:draw-aa-line SCREEN (?x) (?y) (?x) (?y) green)
  (let ((x (?x))
        (y (?y)))
    (GFX:draw-aa-circle SCREEN x y (min x y) green))
  (let ((x (?x))
        (y (?y)))
    (GFX:draw-aa-ellipse SCREEN x y (max x y) (min x y) green))
  (GFX:draw-trigon SCREEN (?x) (?y) (?x) (?y) (?x) (?y) green)
  (GFX:draw-aa-trigon SCREEN (?x) (?y) (?x) (?y) (?x) (?y) green)
  (GFX:draw-aa-polygon SCREEN (nine ?x) (nine ?y) green)
  (SDL:flip))

;; clean up
(SDL:delay 500)
(exit (SDL:quit))

;;; gfx.scm ends here
