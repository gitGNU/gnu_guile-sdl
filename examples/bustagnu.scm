#! /usr/local/bin/guile -s
!#

;; Bust'a Move Clone

;; load the SDL module and some useful srfi's
(use-modules (sdl sdl))

;; initialize the video subsystem
(sdl-init '(SDL_INIT_VIDEO))

;; the directory to find the images in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/examples/")
                  "./"))

;; initialize the images
(define launcher (sdl-load-image (string-append datadir "launcher.png")))
(define ball (sdl-load-image (string-append datadir "circle.png")))
(define gnu-head (sdl-load-image (string-append datadir "gnu-head.png")))

;; setup the video mode
(define rows 8)
(define columns 12)
(define ball-size (sdl-surface:w ball))
(define launcher-offset (quotient (sdl-surface:w launcher) 2))
(define screen-width (* rows ball-size))
(define screen-height (+ (* columns ball-size) launcher-offset))
(define screen-top-border 0)
(define screen-bottom-border (+ screen-height ball-size))
(define screen-left-border 0)
(define screen-right-border (- screen-width ball-size))
(sdl-set-video-mode screen-width screen-height 24)

(define colors `((red     . ,(sdl-make-color #xff #x00 #x00))
                 (green   . ,(sdl-make-color #x00 #xff #x00))
                 (blue    . ,(sdl-make-color #x00 #x00 #xff))
                 (cyan    . ,(sdl-make-color #x00 #xff #xff))
                 (yellow  . ,(sdl-make-color #xff #xff #x00))
                 (magenta . ,(sdl-make-color #xff #x00 #xff))
                 (orange  . ,(sdl-make-color #xff #xbb #x00))
                 (grey    . ,(sdl-make-color #x88 #x88 #x88))
                 ))

(define color-balls
  (let ((overlay (sdl-make-surface 32 32))
        (rect (sdl-make-rect 0 0 32 32)))
    (map
     (lambda (color-pair)
       (let* ((name (car color-pair))
              (color (cdr color-pair))
              (result (sdl-make-surface 32 32))
              (color-value (sdl-map-rgb (sdl-surface:format overlay)
                                        (sdl-color:r color)
                                        (sdl-color:g color)
                                        (sdl-color:b color))))
         ;; copy the gnu head
         (sdl-blit-surface gnu-head rect result rect)
         ;; fill the overlay with the color-value
         (sdl-fill-rect overlay rect color-value)
         ;; blit the black border onto the overlay
         (sdl-blit-surface ball rect overlay rect)
         ;; make the circle translucent
         (sdl-set-color-key! overlay '(SDL_SRCCOLORKEY SDL_RLEACCEL) 0)
         (sdl-set-alpha! overlay '(SDL_SRCALPHA SDL_RLEACCEL) 128)
         ;; blit it over the gnu-head
         (sdl-blit-surface overlay rect result rect)
         ;; now make that border transparent, leaving just the circle
         (sdl-set-color-key! result '(SDL_SRCCOLORKEY SDL_RLEACCEL) 0)
         ;; and return this in a named pair
         (cons name result)))
     colors)))

(define rand-ball
  (let ((l (length color-balls)))
    (lambda ()
      (cdr (list-ref color-balls (random l))))))

;; colors
(define black (sdl-map-rgb (sdl-surface:format (sdl-get-video-surface))
                             #x00 #x00 #x00))

;; global settings
(define busta-angle 90)
(define busta-score 0)
(define pi 3.14159)

;; rotate a square and blit it from the centered, original size
(define (sdl-blit-rotated src angle dst dst-rect)
  (let* ((width (sdl-surface:w src))
         (height (sdl-surface:h src))
         (src-rect (sdl-make-rect 0 0 width height))
         (rotated (sdl-roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (sdl-surface:w rotated))
         (new-height (sdl-surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (sdl-make-rect width-offset height-offset width height)))
    (sdl-blit-surface rotated src-rect dst dst-rect)))

;; redisplay the launcher
(define busta-redisplay-launcher
  (let* ((launcher-width (sdl-surface:w launcher))
         (dst-rect
          (sdl-make-rect (- (quotient screen-width 2) launcher-offset)
                         (- screen-height launcher-offset)
                         launcher-width launcher-offset))
         (src-rect (sdl-make-rect 0 0 launcher-width launcher-offset)))
    (lambda ()
      (let ((screen (sdl-get-video-surface)))
        (sdl-fill-rect screen dst-rect black)
        (sdl-blit-rotated launcher busta-angle screen dst-rect)
        (sdl-flip)))))

;; pivot the launcher by an offset
(define (busta-pivot-launcher offset)
  (let ((new-angle (+ busta-angle offset)))
    (if (and (>= new-angle 0) (<= new-angle 180))
      (begin (set! busta-angle new-angle)
             (busta-redisplay-launcher)))))

;; propagate an image at a given point with a given angle
(define (follow image bg x y angle)
  (let* ((off-x (cos angle))
         (off-y (sin angle))
         (new-x (+ x (* 8 off-x)))
         (new-y (- y (* 8 off-y)))
         (width (sdl-surface:w image))
         (height (sdl-surface:h image))
         (screen (sdl-get-video-surface))
         (continue #t))
    (cond ((< new-y screen-top-border)
           ;; top of screen, anchor
           (set! new-y 0)
           (set! continue #f))
          ((> new-y screen-bottom-border)
           ;; bottom of screen, disappear
           (set! continue #f))
          ((< new-x screen-left-border)
           ;; left edge, bounce
           (set! angle (- pi angle)))
          ((> new-x screen-right-border)
           ;; right edge, bounce
           (set! angle (- pi angle))))
    (let ((src-rect (sdl-make-rect 0 0 width height))
          (orig-rect (sdl-make-rect (inexact->exact x) (inexact->exact y)
                                    width height))
          (dst-rect (sdl-make-rect (inexact->exact new-x)
                                   (inexact->exact new-y) width height)))
      ;; repaint the background and remember it
      (if bg
        (sdl-blit-surface bg src-rect screen orig-rect)
        (set! bg (sdl-make-surface width height)))
      (sdl-blit-surface screen dst-rect bg src-rect)
      ;; blit the ball
      (sdl-blit-surface image src-rect screen dst-rect)
      (sdl-update-rect screen orig-rect)
      (sdl-update-rect screen dst-rect)
      ;; follow through
      (if continue
        (begin
          (sdl-delay 20)
          (follow image bg new-x new-y angle))))))

;; Launch the ball from the current angle
;;   angle is translated to radians, which is what guile uses for
;;   sin/cos, unlike sdl-roto-zoom-surface which uses degrees.
(define (busta-launch)
  (follow (rand-ball) #f (quotient (- screen-width ball-size) 2)
          (- screen-height ball-size)
          (* busta-angle (/ pi 180))))

;; initialize screen
(sdl-set-caption "Bust'a GNU")
(sdl-fill-rect (sdl-get-video-surface)
               (sdl-make-rect 0 0 screen-width screen-height)
               black)
(busta-pivot-launcher 0)

;; event handler
(let handle ((e (sdl-make-event)))
  (sdl-poll-event e)
  (case (sdl-event:type e)
    ((SDL_KEYDOWN)
     (case (sdl-event:key:keysym:sym e)
       ((SDLK_LEFT)
        ;; pivot left (by 5 degrees, or 1 degree if control is held)
        (busta-pivot-launcher
         (let ((mods (sdl-event:key:keysym:mod e)))
           (if (or (memq 'KMOD_LCTRL mods)
                   (memq 'KMOD_LCTRL mods))
             +1 +5))))
       ((SDLK_RIGHT)
        ;; pivot right (by 5 degrees, or 1 degree if control is held)
        (busta-pivot-launcher
         (let ((mods (sdl-event:key:keysym:mod e)))
           (if (or (memq 'KMOD_LCTRL mods)
                   (memq 'KMOD_LCTRL mods))
             -1 -5))))
       ((SDLK_SPACE)
        ;; space to shoot
        ;; (wait until they release the key)
        (while (not (eq? (sdl-event:type e) 'SDL_KEYUP))
               (sdl-poll-event e))
        ;; then launch
        (busta-launch))
       ((SDLK_ESCAPE)
        ;; escape to quit (XXXX add confirmation)
        (sdl-quit)
        (quit)))))
  (handle e))
