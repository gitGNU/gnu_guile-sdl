;;; event.scm --- simple event test

(or *interactive* (exit-77 "interactive"))
(or *have-ttf* (exit-77 "ttf disabled"))

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video (and event) module
(let ((res (SDL:init '(SDL_INIT_VIDEO))))
  (and debug? (simple-format #t "SDL:init: ~S\n" res)))

;; initialize the font lib
(let ((res (SDL:ttf-init)))
  (and debug? (simple-format #t "SDL:ttf-init: ~S\n" res)))

;; get a sample rect size from a list of available modes
(define test-rect (SDL:make-rect 0 0 600 200))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 8
                    '(SDL_HWSURFACE SDL_DOUBLEBUF))

;; the directory to find the image in
(define (datafile name)
  (in-vicinity (or (and=> (getenv "srcdir")
                          (lambda (d) (in-vicinity d "test")))
                   ".")
               name))

;; load a font file
(define font (SDL:load-font (datafile "crystal.ttf") 16))

;; presize some stuff
(define height (SDL:font:height font))
(define top (quotient (- (SDL:rect:h test-rect) height) 2))
(define text-rect (SDL:make-rect 0 top (SDL:rect:w test-rect) height))

;; color to write in
(define white (SDL:make-color #xff #xff #xff))

;; write text centered on screen
(define (display-centered text)
  (let* ((rendered (SDL:render-text font text white #t))
         (dimensions (SDL:font:size-text font text))
         (width (cdr (assq 'w dimensions)))
         (screen (SDL:get-video-surface))
         (left (quotient (- (SDL:rect:w test-rect) width) 2))
         (dst-rect (SDL:make-rect left top width height))
         (src-rect (SDL:make-rect 0 0 width height)))
    (SDL:fill-rect screen text-rect 0)
    (SDL:blit-surface rendered src-rect screen dst-rect)
    (SDL:flip)))

;; event loop
(define input-loop
  (lambda (e)
    (let* ((next-event (SDL:wait-event e))
           (event-type (SDL:event:type e)))
      (case event-type
        ((SDL_KEYDOWN SDL_KEYUP)
         (let ((sym (SDL:event:key:keysym:sym e))
               (mods (SDL:event:key:keysym:mod e)))
           (display-centered (simple-format #f "~A: ~A ~A" event-type sym mods))
           (if (eq? sym 'SDLK_ESCAPE)
             #f
             (input-loop e))))
        ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
         (let ((button (SDL:event:button:button e)))
           (display-centered (simple-format #f "~A: ~A" event-type button)))
         (input-loop e))
        ((SDL_MOUSEMOTION)
         (let ((x (SDL:event:motion:x e))
               (y (SDL:event:motion:y e)))
           (display-centered (simple-format #f "~A: ~Ax~A" event-type x y)))
         (input-loop e))
        (else
         (display-centered (simple-format #f "~A" event-type))
         (input-loop e))))))

;; display an explanatory message
(let* ((text "(Press Escape to Quit)")
       (rendered (SDL:render-text font text white #t))
       (dimensions (SDL:font:size-text font text))
       (width (cdr (assq 'w dimensions)))
       (screen (SDL:get-video-surface))
       (left (quotient (- (SDL:rect:w test-rect) width) 2))
       (top (- (SDL:rect:h test-rect) height 5))
       (dst-rect (SDL:make-rect left top width height))
       (src-rect (SDL:make-rect 0 0 width height)))
  (SDL:fill-rect screen text-rect 0)
  (SDL:blit-surface rendered src-rect screen dst-rect)
  (SDL:flip))

;; main loop
(input-loop (SDL:make-event 0))

;; quit SDL
(SDL:quit)

;;; event.scm ends here
