#!/bin/sh
test x"$NONINTERACTIVE" = x || { echo "INFO: $0: INTERACTIVE" ; exit 77 ; }
test x"$HAVE_TTF" = x && { echo "INFO: $0: TTF DISABLED" ; exit 77 ; }
exec ${GUILE-guile} -s $0 "$@" # -*-scheme-*-
!#
(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

;; simple event test

(use-modules (sdl sdl))                 ; fixme: these must be separate due
(use-modules (sdl ttf))                 ;        to compiled modules weirdness

;; initialize the SDL video (and event) module
(let ((res (sdl-init '(SDL_INIT_VIDEO))))
  (and debug? (format #t "sdl-init: ~S\n" res)))

;; initialize the font lib
(let ((res (sdl-ttf-init)))
  (and debug? (format #t "sdl-ttf-init: ~S\n" res)))

;; get a sample rect size from a list of available modes
(define test-rect (sdl-make-rect 0 0 600 200))

;; set the video mode to the dimensions of our rect
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 8
                    '(SDL_HWSURFACE SDL_DOUBLEBUF))

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; load a font file
(define font (sdl-load-font (string-append datadir "crystal.ttf") 16))

;; presize some stuff
(define height (sdl-font:height font))
(define top (quotient (- (sdl-rect:h test-rect) height) 2))
(define text-rect (sdl-make-rect 0 top (sdl-rect:w test-rect) height))

;; color to write in
(define white (sdl-make-color #xff #xff #xff))

;; write text centered on screen
(define (display-centered text)
  (let* ((rendered (sdl-render-text font text white #t))
         (dimensions (sdl-font:size-text font text))
         (width (cdr (assq 'w dimensions)))
         (screen (sdl-get-video-surface))
         (left (quotient (- (sdl-rect:w test-rect) width) 2))
         (dst-rect (sdl-make-rect left top width height))
         (src-rect (sdl-make-rect 0 0 width height)))
    (sdl-fill-rect screen text-rect 0)
    (sdl-blit-surface rendered src-rect screen dst-rect)
    (sdl-flip)))

;; event loop
(define input-loop
  (lambda (e)
    (let* ((next-event (sdl-wait-event e))
           (event-type (sdl-event:type e)))
      (case event-type
        ((SDL_KEYDOWN SDL_KEYUP)
         (let ((sym (sdl-event:key:keysym:sym e))
               (mods (sdl-event:key:keysym:mod e)))
           (display-centered (format #f "~A: ~A ~A" event-type sym mods))
           (if (eq? sym 'SDLK_ESCAPE)
             #f
             (input-loop e))))
        ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
         (let ((button (sdl-event:button:button e)))
           (display-centered (format #f "~A: ~A" event-type button)))
         (input-loop e))
        ((SDL_MOUSEMOTION)
         (let ((x (sdl-event:motion:x e))
               (y (sdl-event:motion:y e)))
           (display-centered (format #f "~A: ~Ax~A" event-type x y)))
         (input-loop e))
        (else
         (display-centered (format #f "~A" event-type))
         (input-loop e))))))

;; display an explanatory message
(let* ((text "(Press Escape to Quit)")
       (rendered (sdl-render-text font text white #t))
       (dimensions (sdl-font:size-text font text))
       (width (cdr (assq 'w dimensions)))
       (screen (sdl-get-video-surface))
       (left (quotient (- (sdl-rect:w test-rect) width) 2))
       (top (- (sdl-rect:h test-rect) height 5))
       (dst-rect (sdl-make-rect left top width height))
       (src-rect (sdl-make-rect 0 0 width height)))
  (sdl-fill-rect screen text-rect 0)
  (sdl-blit-surface rendered src-rect screen dst-rect)
  (sdl-flip))

;; main loop
(input-loop (sdl-make-event 0))

;; quit SDL
(sdl-quit)

;;; event.scm ends here
