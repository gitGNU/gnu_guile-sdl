#!/bin/sh
exec ${GUILE-guile} -s $0 "$@" # -*- scheme -*-
!#
;;; Read Vertically-Packed Image

;;; Author: Thien-Thi Nguyen <ttn@glug.org>

;;; Commentary:

;; Usage: read-vpacked.scm [width [height]]
;;
;; Read blueball.png and redball.png, and display their unpacked elements.
;; "Packing" means vertically-abutted images of dimensions NxN through Nx1.
;; SPC pauses, q or ESC quits.
;;
;; Optional args specify the window geometry (default: 800 x 600).

;;; Code:

;; load the SDL module
(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; return a vector of rectangles on the image, index is size (0 is "invalid")
(define (unpack image)
  (let loop ((size (SDL:surface:w image)) (offset 0) (acc '()))
    (if (= 0 size)
        (list->vector (cons #f acc))
        (loop (1- size) (+ offset size)
              (cons (SDL:make-rect 0 offset size size) acc)))))

;; draw the vector elements in random locations on surface
(define (show image v surface)
  (let ((len (vector-length v))
        (loc (SDL:make-rect 0 0 0 0))
        (w   (SDL:surface:w surface))
        (h   (SDL:surface:h surface)))
    (do ((i 0 (1+ i)))
        ((= i len))
      (and=> (vector-ref v i)
             (lambda (src-rect)
               ;; the bigger it is, the further from the edge
               (SDL:rect:set-x! loc (+ i (random (- w (* 3 i)))))
               (SDL:rect:set-y! loc (+ i (random (- h (* 3 i)))))
               (SDL:rect:set-w! loc i)
               (SDL:rect:set-h! loc i)
               (SDL:blit-surface image src-rect surface loc))))))

;; initialize the video subsystem
(SDL:init '(SDL_INIT_VIDEO))

;; the images and their unpacked-rectangles vectors
(define *blue-image* (SDL:load-image "blueball.png"))
(define *blue-rvect* (unpack *blue-image*))
(define *red-image* (SDL:load-image "redball.png"))
(define *red-rvect* (unpack *red-image*))

;; a place to show things
(define *screen* (let ((args (cdr (command-line))))
                   (or (SDL:set-video-mode
                        (max 100 (or (and (< 0 (length args))
                                          (string->number (car args)))
                                     800))
                        (max 100 (or (and (< 1 (length args))
                                          (string->number (cadr args)))
                                     600))
                        0 '(SDL_HWSURFACE SDL_DOUBLEBUF))
                       (error "could not set *screen*"))))

;; handler
(let ((pause #f))
  (format #t "blue: ~Ax~A (~A elements)\n"
          (SDL:surface:w *blue-image*)
          (SDL:surface:h *blue-image*)
          (1- (vector-length *blue-rvect*)))
  (format #t "red: ~Ax~A (~A elements)\n"
          (SDL:surface:w *red-image*)
          (SDL:surface:h *red-image*)
          (1- (vector-length *red-rvect*)))
  (let handle ((e (SDL:make-event)))
    (if (SDL:poll-event e)
        (case (SDL:event:type e)
          ((SDL_KEYDOWN)
           (case (SDL:event:key:keysym:sym e)
             ((SDLK_SPACE)
              (set! pause (not pause)))
             ((SDLK_ESCAPE SDLK_q)
              (SDL:quit)
              (quit))))))
    (or pause (begin (show *blue-image* *blue-rvect* *screen*)
                     (show *red-image* *red-rvect* *screen*)
                     (SDL:flip)))
    (handle e)))

;;; read-vpacked.scm ends here
