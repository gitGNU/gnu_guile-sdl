#! /usr/local/bin/guile -s
!#

;; Simple Image Browser
;; 
;; Created:    <2001-06-17 18:08:20 foof>
;; Time-stamp: <2001-06-17 19:46:20 foof>
;; Author:     Alex Shinn <foof@debian.org>


;; use the SDL module (prefixing symbols with "sdl-")
(use-modules ((sdl sdl)
              :rename (symbol-prefix-proc 'sdl-)))

;; and srfi-1 for list processing
(use-modules (srfi srfi-1)
             (srfi srfi-2))

;; initialize the video subsystem
(init-sdl init/video)

;; directory to search for images in
(define image-dir "/usr/share/pixmaps/")

;; utility to test if a path is a directory
(define (file? f)
  (let* ((stats (stat f))
         (type (stat:type stats)))
    (eq? type 'regular)))

;; build a ring of image file names
(define image-ring
  (let ((dir (opendir image-dir)))
    (letrec ((D (lambda (ls)
                  (let ((file (readdir dir)))
                    (if (eof-object? file)
                        (begin (closedir dir) ls)
                        (D (cons (string-append image-dir file)
                                 ls)))))))
      (apply circular-list (reverse (filter file? (D '())))))))

;; functions to cycle through the ring
(define (next-image)
  (let ((next (car image-ring)))
    (set! image-ring (cdr image-ring))
    next))

(define (prev-image)
  (let ((orig image-ring))
    (while (not (eq? (cddr image-ring) orig))
      (set! image-ring (cdr image-ring)))
    (let ((image (car image-ring)))
      (set! image-ring (cdr image-ring))
      image)))

;; display an image given a filename
(define (show file)
  (and-let* ((image (load-image file)))
    (set-video-mode (surface:w image) (surface:h image)
                    24 video/hwsurface)
    (blit-surface image)
    (flip)))

;; show the first image
(show (next-image))

;; event handler
(let handle ((e (make-event)))
  (if (wait-event e)
    (case (event:type e)
      ((event/key-down)
       (case (event:key:keysym:sym e)
         ((key/left key/backspace)
          (show (prev-image)))
         ((key/right key/space)
          (show (next-image)))
         ((key/escape key/q)
          (quit-sdl)
          (quit))))
      (else
        (handle e))))
    (handle e))
