#! /usr/local/bin/guile -s
!#

;; SICP Picture Language Example
;; 
;; Created:    <2001-06-24 16:36:53 foof>
;; Time-stamp: <2001-06-30 01:07:42 foof>
;; Author:     Alex Shinn <foof@debian.org>


;; load the SDL module
(use-modules (sdl sdl))

;; Primitive functions

;; Functions accept and return SDL surfaces.  For this simple example
;; we assume all images are the same size, and obtain the size from
;; the first argument.  Frames (to shape the surfaces in different
;; parallellograms) are left as an exercise for the reader :-)

(define (beside left right)
  (let* ((width (sdl-surface:w left))
         (height (sdl-surface:h right))
         (width/2 (quotient width 2))
         (src-rect (sdl-make-rect 0 0 width/2 height))
         (result (sdl-make-surface width height))
         (new-left (sdl-zoom-surface left 1/1 1 #t))
         (new-right (sdl-zoom-surface right 1/2 1 #t)))
    (sdl-blit-surface new-left src-rect result
                      (sdl-make-rect 0 0 width/2 height))
    (sdl-blit-surface new-right src-rect result
                      (sdl-make-rect width/2 0 width height))
    result))

(define (below top bottom)
  (let* ((width (sdl-surface:w top))
         (height (sdl-surface:h top))
         (height/2 (quotient height 2))
         (src-rect (sdl-make-rect 0 0 width height/2))
         (result (sdl-make-surface width height))
         (new-top (sdl-zoom-surface top 1 1/2 #t))
         (new-bottom (sdl-zoom-surface bottom 1 1/2 #t)))
    (sdl-blit-surface new-top src-rect result
                      (sdl-make-rect 0 0 width height/2))
    (sdl-blit-surface new-bottom src-rect result
                      (sdl-make-rect 0 height/2 width height))
    result))

(define flip-vert sdl-vertical-flip-surface)
(define flip-horiz sdl-vertical-flip-surface)


;; Higher order functions

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


;; A quick example, press escape to quit, any other key to increase to
;; corner-split count.

;; initialize the video subsystem
(sdl-init sdl-init/video)

(let* ((painter (sdl-load-image "william-rogers.png"))
       (w (sdl-surface:w painter))
       (h (sdl-surface:h painter))
       (depth (sdl-surface:depth painter))
       (e (sdl-make-event))
       (i 1))
  (sdl-set-video-mode w h depth 1)
  (sdl-blit-surface (corner-split painter i))
  (sdl-flip)
  (while (not (and e (eq? (sdl-event:type e) 'event/key-down)
                   (eq? (sdl-event:key:keysym:sym e) 'key/escape)))
    (sdl-wait-event e)
    (if (eq? (sdl-event:type e) 'event/key-down)
      (begin (set! i (1+ i))
             (sdl-blit-surface (corner-split painter i))
             (sdl-flip)))))

;; clean up
(sdl-quit)

