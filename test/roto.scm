;;; roto.scm --- simple rotozoom test

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl gfx) #:renamer (symbol-prefix-proc 'GFX:)))

;; the directory to find the image in
(define (datafile name)
  (in-vicinity (or (and=> (getenv "srcdir")
                          (lambda (d) (in-vicinity d "test")))
                   ".")
               name))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; load the image
(define gnu-head (SDL:load-image (datafile "gnu-goatee.jpg")))

;; the size of our test image
(let ((w (SDL:surface:w gnu-head))
      (h (SDL:surface:h gnu-head)))
  ;; set the video mode to the dimensions of our image
  (SDL:set-video-mode w h 16 '(SDL_HWSURFACE))
  ;; rotate the image 27 degrees at a time, reducing its magnitude by 10%
  (do ((theta 0 (+ 27 theta)) (mag 1.0 (* mag 0.9)))
      ((>= theta (* 3 360)))            ; a few times around
    (let* ((image (GFX:roto-zoom-surface gnu-head theta mag #t))
           (iw    (SDL:surface:w image))
           (ih    (SDL:surface:h image))
           (drect (SDL:make-rect (quotient (- w iw) 2)
                                 (quotient (- h ih) 2)
                                 iw ih)))
      (SDL:fill-rect (SDL:get-video-surface) #f #xffff)
      (SDL:blit-surface image #f #f drect)
      (SDL:flip)
      (SDL:delay 100))))

;; quit
(SDL:quit)

;;; roto.scm ends here
