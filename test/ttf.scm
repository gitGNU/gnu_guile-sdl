#! /usr/local/bin/guile -s
!#

;; simple true type font test

(use-modules (sdl sdl)
             (sdl ttf)
             (ice-9 format))

;; initialize SDL video
(sdl-init '(SDL_INIT_VIDEO))

;; initialize the font lib
(sdl-ttf-init)

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; the text to display
(define sentence "The quick brown fox jumped over the lazy sleeping dog.")

;; load a font file
(define font (sdl-load-font (string-append datadir "crystal.ttf") 16))

;; initialize the video mode
(define test-rect (sdl-make-rect 0 0 640 480))
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16)

(seed->random-state (sdl-get-ticks))

(define rand-rect
  (let* ((dimensions (sdl-font:size-text font sentence))
         (w (car dimensions))
         (h (cadr dimensions)))
    (lambda ()
      (sdl-make-rect (random (sdl-rect:w test-rect))
                     (random (sdl-rect:h test-rect))
                     w h))))

(define rand-color
  (lambda ()
    (sdl-make-color (random #xff) (random #xff) (random #xff))))

;; clear the screen
(sdl-fill-rect (sdl-get-video-surface) test-rect #xffffff)
(sdl-flip)

;; write the text in random locations with random colors
(let ((src-rect (sdl-make-surface (sdl-rect:w test-rect)
                                  (sdl-rect:h test-rect)))
      (screen (sdl-get-video-surface)))
  (do ((i 0 (1+ i)))
      ((> i 50))
    (let ((text (sdl-render-text font sentence (rand-color) #t))
          (dst-rect (rand-rect)))
      (sdl-blit-surface text test-rect screen dst-rect)
      (sdl-update-rect screen dst-rect))))

;; clean up
(sdl-delay 1000)
(sdl-ttf-quit)
(sdl-quit)

