#!/bin/sh
exec ${GUILE-guile} -s $0 "$@" # -*-scheme-*-
!#

;; SICP Picture Language Example
;;
;; Created:    <2001-06-24 16:36:53 foof>
;; Time-stamp: <2005-01-05 14:55:28 ttn>
;; Author:     Alex Shinn <foof@debian.org>


;; load the SDL module
(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; Primitive functions

;; Functions accept and return SDL surfaces.  For this simple example
;; we assume all images are the same size, and obtain the size from
;; the first argument.  Frames (to shape the surfaces in different
;; parallellograms) are left as an exercise for the reader :-)

(define (beside left right)
  (let* ((width (SDL:surface:w left))
         (height (SDL:surface:h right))
         (width/2 (quotient width 2))
         (src-rect (SDL:make-rect 0 0 width/2 height))
         (result (SDL:make-surface width height))
         (new-left (SDL:zoom-surface left 1/2 1 #t))
         (new-right (SDL:zoom-surface right 1/2 1 #t)))
    (SDL:blit-surface new-left src-rect result
                      (SDL:make-rect 0 0 width/2 height))
    (SDL:blit-surface new-right src-rect result
                      (SDL:make-rect width/2 0 width height))
    result))

(define (below top bottom)
  (let* ((width (SDL:surface:w top))
         (height (SDL:surface:h top))
         (height/2 (quotient height 2))
         (src-rect (SDL:make-rect 0 0 width height/2))
         (result (SDL:make-surface width height))
         (new-top (SDL:zoom-surface top 1 1/2 #t))
         (new-bottom (SDL:zoom-surface bottom 1 1/2 #t)))
    (SDL:blit-surface new-top src-rect result
                      (SDL:make-rect 0 0 width height/2))
    (SDL:blit-surface new-bottom src-rect result
                      (SDL:make-rect 0 height/2 width height))
    result))

(define flip-vert SDL:vertical-flip-surface)
(define flip-horiz SDL:horizontal-flip-surface)


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
(SDL:init '(SDL_INIT_VIDEO))

(let* ((painter (SDL:load-image "william-rogers.png"))
       (w (SDL:surface:w painter))
       (h (SDL:surface:h painter))
       (depth (SDL:surface:depth painter))
       (e (SDL:make-event 'SDL_USEREVENT))
       (i 1))
  (SDL:set-video-mode w h depth)
  (SDL:blit-surface (corner-split painter i))
  (SDL:flip)
  (while (not (and e (eq? (SDL:event:type e) 'SDL_KEYDOWN)
                   (eq? (SDL:event:key:keysym:sym e) 'SDLK_ESCAPE)))
         (SDL:wait-event e)
         (if (eq? (SDL:event:type e) 'SDL_KEYDOWN)
             (begin (set! i (1+ i))
                    (SDL:blit-surface (corner-split painter i))
                    (SDL:flip)))))

;; clean up
(SDL:quit)

;;; sicp-painter.scm ends here
