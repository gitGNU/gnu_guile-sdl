#! /usr/local/bin/guile -s
!#

;; Bust-A-Move Clone

(read-set! keywords 'prefix)

;; load the SDL module and some useful srfi's
(use-modules (sdl sdl)
             (srfi srfi-1)
             (srfi srfi-2)
             (ice-9 format)
             (oop goops))

;; seed the RNG
(seed->random-state (sdl-get-ticks))

;; initialize the video subsystem
(sdl-init '(SDL_INIT_VIDEO))

;; the directory to find the images in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/examples/")
                  "./"))

;; a sprite in a grid
(define-class <sprite> ()
  ;; circular linked list of images
  (frames :init-value '()
          :accessor sprite:frames)
  ;; src and dst rects
  (dimensions :init-keyword :dimensions
              :accessor sprite:dimensions)
  (location :init-keyword :location
            :accessor sprite:location)
  ;; define movment
  (move-proc :init-keyword :move-proc
             :init-value #f
             :accessor sprite:move-proc)
  ;; handle collisions
  (collision-hooks :init-value '()
                   :accessor sprite:collision-hooks))

;; overload initialize to take a surface frame list as an arg to the
;; :frames keyword, and make it into a circular list.
(define-method (initialize (sprite <sprite>) initargs)
  ;(display "initialize\n")
  (and-let* ((frame-args (memq :frames initargs)))
    (set! (sprite:frames sprite) (cadr frame-args))
    (let* ((surface (caadr frame-args))
           (width (sdl-surface:w surface))
           (height (sdl-surface:h surface))
           (rect (sdl-make-rect 0 0 width height)))
      (set! (sprite:dimensions sprite) rect)
      (if (not (memq :location initargs))
        (set! (sprite:location sprite) (sdl-copy-rect rect)))))
  ;(and-let* ((rect-args (memq :rect initargs)))
  ;  (set! (sprite:location sprite) (cadr rect-args)))
  (next-method))

;; the background space
(define-class <space> (<sprite>))

;; edges
(define-class <edge> (<sprite>)
  (reflector :init-value (lambda (x) (- pi x))
             :accessor sprite:reflector))

;; balls
(define-class <ball> (<sprite>)
  (color :accessor sprite:color))

(define (sdl-copy-rect rect)
  (sdl-make-rect (sdl-rect:x rect) (sdl-rect:y rect)
                 (sdl-rect:w rect) (sdl-rect:h rect)))

;; cycle through the frames in a sprite list
(define (show-next-frame! sprite surface)
  (let ((frame-list (sprite:frames sprite)))
    (cond ((or (not frame-list) (null? frame-list))
           #f)
          ((pair? frame-list)
           ;(display (format #f "(pre-blit ~A)\n" (sprite:location sprite)))
           (sdl-blit-surface (car frame-list) (sprite:dimensions sprite)
                             surface (sdl-copy-rect (sprite:location sprite)))
           ;(display (format #f "(post-blit ~A)\n" (sprite:location sprite)))
           (set! (sprite:frames sprite) (cdr frame-list))
           ;(display (format #f "(post-set! ~A)\n" (sprite:location sprite)))
           ))))

(define (remove-sprite sprite)
  (set! busta-sprites (delq! sprite busta-sprites)))

;; move and redisplay the sprites
(define (update-sprites)
  ;; first clear the background
  (let ((screen (sdl-get-video-surface)))
    (for-each
     (lambda (sprite)
       ;(display (format #f "(fill-rect ~A)\n" (sprite:location sprite)))
       (sdl-fill-rect screen (sdl-copy-rect (sprite:location sprite)) bgcolor))
     busta-sprites))
  ;; then redisplay the launcher
  (redisplay-launcher)
  ;; then handle movement & redisplay
  (let ((screen (sdl-get-video-surface)))
    (for-each
     (lambda (sprite)
       (and-let* ((move-proc (sprite:move-proc sprite)))
         ;(display (format #f "(move-proc ~A)\n" (sprite:location sprite)))
         (move-proc sprite))
       ;(display (format #f "(show-next-frame! ~A)\n" (sprite:location sprite)))
       (show-next-frame! sprite screen))
     busta-sprites))
  ;; finally update the screen
  (let ((screen (sdl-get-video-surface)))
    (for-each
     (lambda (sprite)
       ;(display (format #f "(update-rect ~A)\n" (sprite:location sprite)))
       ;(sdl-update-rect screen (sdl-copy-rect (sprite:location sprite)))
       (sdl-flip)
       (sdl-delay 10)
       )
     busta-sprites)))


;; initialize the images
(define launcher (sdl-load-image (string-append datadir "launcher.png")))
(define launcher-rotated launcher)
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
         ;; blit the bgcolor border onto the overlay
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

(define (rand-ball-sprite)
  (make <ball>
    :frames (circular-list (rand-ball))
    :location (sdl-make-rect (quotient (- screen-width ball-size) 2)
                             (- screen-height ball-size)
                             (sdl-surface:w ball)
                             (sdl-surface:h ball))))

;; colors
(define bgcolor (sdl-map-rgb (sdl-surface:format (sdl-get-video-surface))
                             #x00 #x00 #x00))

;; global settings
(define busta-angle 90)
(define busta-score 0)
(define busta-sprites '())
(define busta-balls-in-motion 0)
(define busta-max-balls 1)
(define pi 3.14159)

;; the grid
(define busta-grid
  (make-array #f rows (+ 2 columns)))

;; rotate a square retaining its original size
(define (sdl-rotate-square src angle)
  (let* ((width (sdl-surface:w src))
         (height (sdl-surface:h src))
         (rotated (sdl-roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (sdl-surface:w rotated))
         (new-height (sdl-surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (sdl-make-rect width-offset height-offset width height))
         (dst (sdl-make-surface width height)))
    (sdl-blit-surface rotated src-rect dst)
    dst))

;; redisplay the launcher
(define redisplay-launcher
  (let* ((launcher-width (sdl-surface:w launcher))
         (dst-rect
          (sdl-make-rect (- (quotient screen-width 2) launcher-offset)
                         (- screen-height launcher-offset)
                         launcher-width launcher-offset))
         (src-rect (sdl-make-rect 0 0 launcher-width launcher-offset)))
    (lambda ()
      (let ((screen (sdl-get-video-surface)))
        (sdl-fill-rect screen dst-rect bgcolor)
        (sdl-blit-surface launcher-rotated src-rect screen dst-rect)
        (sdl-update-rect screen dst-rect)))))

;; pivot the launcher by an offset
(define (pivot-launcher offset)
  (let ((new-angle (+ busta-angle offset)))
    (if (and (>= new-angle 0) (<= new-angle 180))
      (begin (set! busta-angle new-angle)
             (set! launcher-rotated
                   (sdl-rotate-square launcher new-angle))))))

(define (make-angle-mover angle)
  (let ((off-x (cos angle))
        (off-y (sin angle)))
    (lambda (sprite)
      (let* ((ball (car (sprite:frames sprite)))
             (src-rect (sprite:dimensions sprite))
             (dst-rect (sprite:location sprite))
             (x (sdl-rect:x dst-rect))
             (y (sdl-rect:y dst-rect))
             (new-x (+ x (* 8 off-x)))
             (new-y (- y (* 8 off-y)))
             (width (sdl-surface:w ball))
             (height (sdl-surface:h ball))
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
               (set! (sprite:move-proc sprite)
                     (make-angle-mover (- pi angle))))
              ((> new-x screen-right-border)
               ;; right edge, bounce
               (set! (sprite:move-proc sprite)
                     (make-angle-mover (- pi angle)))))
        ;;(display (format #f "(angle-mover ~A) ~A => " angle dst-rect))
        (sdl-rect:set-x! dst-rect (inexact->exact new-x))
        (sdl-rect:set-y! dst-rect (inexact->exact new-y))
        ;;(display (format #f "~A\n" dst-rect))
        (cond ((not continue)
               (remove-sprite sprite)
               (set! busta-balls-in-motion (- busta-balls-in-motion 1))))))))

(define (degrees->radians deg)
  (* deg (/ pi 180)))

;; Launch the ball from the current angle
;;   angle is translated to radians, which is what guile uses for
;;   sin/cos, unlike sdl-roto-zoom-surface which uses degrees.
(define (launch)
  (if (< busta-balls-in-motion busta-max-balls)
    (let ((ball (rand-ball-sprite)))
      (set! (sprite:move-proc ball)
            (make-angle-mover (degrees->radians busta-angle)))
      (set! busta-balls-in-motion (1+ busta-balls-in-motion))
      (set! busta-sprites (cons ball busta-sprites)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize screen
(sdl-set-caption "Bust-A-GNU" "Bust-A-GNU")
(sdl-fill-rect (sdl-get-video-surface)
               (sdl-make-rect 0 0 screen-width screen-height)
               bgcolor)
(redisplay-launcher)

;; event handler
(let ((next-update-ticks (+ 10 (sdl-get-ticks))))
  (let handle ((e (sdl-make-event)))
    ;; poll for the next event
    (sdl-poll-event e)
    (case (sdl-event:type e)
      ((SDL_KEYDOWN)
       (case (sdl-event:key:keysym:sym e)
         ((SDLK_LEFT)
          ;; pivot left (by 4 degrees, or 1 degree if control is held)
          (pivot-launcher
           (let ((mods (sdl-event:key:keysym:mod e)))
             (if (or (memq 'KMOD_LCTRL mods)
                     (memq 'KMOD_RCTRL mods))
               +1 +4))))
         ((SDLK_RIGHT)
          ;; pivot right (by 4 degrees, or 1 degree if control is held)
          (pivot-launcher
           (let ((mods (sdl-event:key:keysym:mod e)))
             (if (or (memq 'KMOD_LCTRL mods)
                     (memq 'KMOD_RCTRL mods))
               -1 -4))))
         ((SDLK_SPACE)
          ;; space to shoot
          ;; (wait until they release the key)
          (while (not (eq? (sdl-event:type e) 'SDL_KEYUP))
                 (sdl-poll-event e))
          ;; then launch
          (launch))
         ((SDLK_ESCAPE)
          ;; escape to quit (XXXX add confirmation)
          (sdl-quit)
          (quit)))))
    ;; update the sprites
    (let ((ticks (sdl-get-ticks)))
      (cond ((> ticks next-update-ticks)
             (set! next-update-ticks (+ 10 ticks))
             (update-sprites))))
    ;; repeat
    (handle e)))

