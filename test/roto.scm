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

(define-macro (spin theta-update mag-vars turns roto wait)
  ;; the size of our test image
  `(let ((w (SDL:surface:w gnu-head))
         (h (SDL:surface:h gnu-head))
         (screen #f))
     ;; set the video mode to the dimensions of our image
     (SDL:set-video-mode w h 16 '(SDL_HWSURFACE))
     (set! screen (SDL:get-video-surface))
     (do ((theta 0 (+ ,theta-update theta)) ,@mag-vars)
         ((>= theta (* ,turns 360)))    ; a few times around
       (let* ((image ,roto)
              (iw    (SDL:surface:w image))
              (ih    (SDL:surface:h image))
              (drect (SDL:make-rect (quotient (- w iw) 2)
                                    (quotient (- h ih) 2)
                                    iw ih)))
         (SDL:fill-rect screen #f #xffff)
         (SDL:blit-surface image #f #f drect)
         (SDL:flip)
         (SDL:delay ,wait)))))

(spin 27 ((mag 1.0 (* mag 0.9)))
      3 (GFX:roto-zoom-surface gnu-head theta mag #t)
      100)

(spin (+ 10 (random 17)) ((magx 1.0 (* magx (+ 0.75 (random 0.5))))
                          (magy 1.0 (* magy (+ 0.75 (random 0.5)))))
      10 (GFX:roto-zoom-surface-xy gnu-head theta magx magy #t)
      10)

;; quit
(SDL:quit)

;;; roto.scm ends here
