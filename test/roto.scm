#!/usr/local/bin/guile -s
!#

;; simple rotozoom test

(use-modules (sdl sdl))

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; initialize the SDL video module
(sdl-init '(SDL_INIT_VIDEO))

;; load the image
(define gnu-head (sdl-load-image (string-append datadir "gnu-goatee.jpg")))

;; the size of our test image
(let* ((w (sdl-surface:w gnu-head))
       (h (sdl-surface:h gnu-head))
       (gnu-rect (sdl-make-rect 0 0 w h)))
  ;; set the video mode to the dimensions of our image
  (sdl-set-video-mode w h 16 '(SDL_HWSURFACE))
  ;; rotate the image 15 degrees at a time
  (do ((theta 0 (+ 15 theta)))
      ((>= theta 360))
    (let ((image (sdl-roto-zoom-surface gnu-head theta 1.0 #t)))
      (sdl-fill-rect (sdl-get-video-surface) gnu-rect #xffff)
      (sdl-blit-surface image)
      (sdl-flip)
      (sdl-delay 100))))

;; quit
(sdl-quit)

