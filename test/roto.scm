;;; roto.scm --- simple rotozoom test

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; load the image
(define gnu-head (SDL:load-image (string-append datadir "gnu-goatee.jpg")))

;; the size of our test image
(let* ((w (SDL:surface:w gnu-head))
       (h (SDL:surface:h gnu-head))
       (gnu-rect (SDL:make-rect 0 0 w h)))
  ;; set the video mode to the dimensions of our image
  (SDL:set-video-mode w h 16 '(SDL_HWSURFACE))
  ;; rotate the image 27 degrees at a time
  (do ((theta 0 (+ 27 theta)))
      ((>= theta (* 3 360)))            ; a few times around
    (let ((image (SDL:roto-zoom-surface gnu-head theta 1.0 #t)))
      (SDL:fill-rect (SDL:get-video-surface) gnu-rect #xffff)
      (SDL:blit-surface image)
      (SDL:flip)
      (SDL:delay 100))))

;; quit
(SDL:quit)

;;; roto.scm ends here
