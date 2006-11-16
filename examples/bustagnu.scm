#!/bin/sh
exec ${GUILE-guile} -s $0 "$@" # -*-scheme-*-
!#

;; Bust-A-Move Clone

(read-set! keywords 'prefix)

;; load the SDL module and some useful srfi's
(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl gfx) #:renamer (symbol-prefix-proc 'GFX:))
             (srfi srfi-1)
             (srfi srfi-2)
             (ice-9 format)
             (oop goops))

;; seed the RNG
(seed->random-state (SDL:get-ticks))

;; initialize the video subsystem
(SDL:init '(SDL_INIT_VIDEO))

;; global settings
(define launcher-angle 90)
(define current-score 0)
(define active-sprites '())
(define removed-sprites '())
(define update-rects '())
(define balls-in-motion 0)
(define max-balls 1)
(define pi 3.14159)
(define default-velocity 8)

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
  ;; define movement
  (angle :init-keyword :angle
         :accessor sprite:angle)
  (move-proc :init-keyword :move-proc
             :init-value #f
             :accessor sprite:move-proc)
  ;; grid position
  (row :init-keyword :row :accessor sprite:row)
  (col :init-keyword :col :accessor sprite:col)
  )

;; overload initialize to take a surface frame list as an arg to the
;; :frames keyword, and make it into a circular list.
(define-method (initialize (sprite <sprite>) initargs)
  (and-let* ((frame-args (memq :frames initargs)))
    (set! (sprite:frames sprite) (cadr frame-args))
    (let* ((surface (caadr frame-args))
           (width (SDL:surface:w surface))
           (height (SDL:surface:h surface))
           (rect (SDL:make-rect 0 0 width height)))
      (set! (sprite:dimensions sprite) rect)
      (if (not (memq :location initargs))
        (set! (sprite:location sprite) (SDL:copy-rect rect)))))
  (next-method))

;; the background space
(define-class <space> (<sprite>))

;; edges
(define-class <edge> (<sprite>))

(define-class <bounce-edge> (<edge>))
(define-class <left-edge> (<bounce-edge>))
(define-class <right-edge> (<bounce-edge>))

(define-class <anchor-edge> (<edge>))
(define-class <top-edge> (<anchor-edge>))
(define-class <bottom-edge> (<anchor-edge>))

;; non-collidable movers
(define-class <ghost> (<sprite>))

;; balls
(define-class <ball> (<sprite>)
  (color :accessor sprite:color))

(define-generic collide)

(define-method (collide sprite obj)
  ;;(display "default collision\n")
  #f)

(define-method (collide (sprite <ghost>) obj)
  #f)

(define-method (collide sprite (obj <bounce-edge>))
  (let ((new-angle (- pi (sprite:angle sprite))))
    ;;(display "bounce collision\n")
    (set! (sprite:angle sprite) new-angle)
    (set! (sprite:move-proc sprite) (make-angle-mover sprite new-angle))))

(define-method (collide sprite (obj <left-edge>))
  (let ((angle (sprite:angle sprite)))
    ;; only bounce if we're moving towards the left
    (if (>= angle (/ pi 2))
        ;; set x to 0, and adjust y to keep it on the same line
        (let* ((sprite-rect (sprite:location sprite))
               (x (SDL:rect:x sprite-rect))
               (y (SDL:rect:y sprite-rect))
               (off-x (* default-velocity (cos angle)))
               (off-y (* default-velocity (sin angle)))
               (old-x (+ x off-x))
               (old-y (- y off-y))
               (slope (/ (- y old-y) (- x old-x)))
               (y-intercept (inexact->exact (- y (* slope x)))))
                                        ;(display "left collision\n")
          (SDL:rect:set-x! sprite-rect 0)
          (SDL:rect:set-y! sprite-rect y-intercept)
          (next-method)))))

(define-method (collide sprite (obj <right-edge>))
  (let ((angle (sprite:angle sprite)))
    ;; only bounce if we're moving towards the right
    (if (<= angle (/ pi 2))
        (let* ((sprite-rect (sprite:location sprite))
               )
          (SDL:rect:set-x! sprite-rect (- screen-width ball-size))
          (next-method)))))

(define-method (collide sprite (obj <anchor-edge>))
  (remove-sprite sprite)
  (set! balls-in-motion (- balls-in-motion 1)))

(define-method (collide sprite (obj <top-edge>))
  (let* ((sprite-rect (sprite:location sprite)))
    (SDL:rect:set-y! sprite-rect 0))
  (next-method))

(define-method (collide sprite (obj <bottom-edge>))
  (let* ((sprite-rect (sprite:location sprite)))
    (SDL:rect:set-y! sprite-rect screen-height))
  (next-method))


(define (SDL:copy-rect rect)
  (SDL:make-rect (SDL:rect:x rect) (SDL:rect:y rect)
                 (SDL:rect:w rect) (SDL:rect:h rect)))

;; cycle through the frames in a sprite list
(define (show-next-frame! sprite surface)
  (let ((frame-list (sprite:frames sprite)))
    (cond ((or (not frame-list) (null? frame-list))
           #f)
          ((pair? frame-list)
           ;;(display (format #f "(pre-blit ~A)\n" (sprite:location sprite)))
           (SDL:blit-surface (car frame-list) (sprite:dimensions sprite)
                             surface (SDL:copy-rect (sprite:location sprite)))
           ;;(display (format #f "(post-blit ~A)\n" (sprite:location sprite)))
           (set! (sprite:frames sprite) (cdr frame-list))
           ;;(display (format #f "(post-set! ~A)\n" (sprite:location sprite)))
           ))))

(define (remove-sprite sprite)
  (set! removed-sprites (cons sprite removed-sprites)))

;; move and redisplay the sprites
(define (update-sprites)
  ;; first clear the background
  (for-each
   (lambda (sprite)
     (let ((rect (sprite:location sprite)))
       (SDL:blit-surface bgsurface (sprite:location sprite))
       (set! update-rects (cons (SDL:copy-rect (sprite:location sprite))
                                update-rects))))
   active-sprites)
  ;; then redisplay the launcher
  (redisplay-launcher)
  ;; then handle movement & redisplay
  (let ((screen (SDL:get-video-surface)))
    (for-each
     (lambda (sprite)
       (and-let* ((move-proc (sprite:move-proc sprite)))
                                        ;(display (format #f "(move-proc ~A)\n" (sprite:location sprite)))
         (move-proc sprite)
         (let* ((loc (sprite:location sprite))
                (x (SDL:rect:x loc))
                (y (SDL:rect:y loc))
                (grid-coords (xy->grid x y)))
                                        ;(display (format #f "hit: (~A ~A) => ~A\n" x y grid-coords))
           (cond ((or (not (= (car grid-coords) (sprite:row sprite)))
                      (not (= (cadr grid-coords) (sprite:col sprite))))
                  (set! (sprite:row sprite) (car grid-coords))
                  (set! (sprite:col sprite) (cadr grid-coords))
                  (and-let* ((obj (apply grid-ref grid-coords)))
                    ;; we've hit something!!
                                        ;(display "we've hit something!!\n")
                    (collide sprite obj))))
           (if (< (SDL:rect:x loc) 0)
               (SDL:rect:set-x! loc 0)
               (if (> (SDL:rect:x loc) (- screen-width ball-size))
                   (SDL:rect:set-x! loc (- screen-width ball-size))))
           (if (< (SDL:rect:y loc) 0)
               (SDL:rect:set-y! loc 0))
           ))
                                        ;(display (format #f "(show-next-frame! ~A)\n" (sprite:location sprite)))
       (show-next-frame! sprite screen))
     active-sprites))
  ;; update the screen
  (let ((screen (SDL:get-video-surface)))
    (for-each
     (lambda (sprite)
                                        ;(display (format #f "(update-rect ~A)\n" (sprite:location sprite)))
       (SDL:update-rect screen (sprite:location sprite)))
     active-sprites)
    (for-each
     (lambda (rect)
       (SDL:update-rect screen rect))
     update-rects))
  ;; remove any dead sprites
  (set! active-sprites
        (remove! (lambda (x) (memq x removed-sprites)) active-sprites))
  (set! removed-sprites '())
  (set! update-rects '()))


;; initialize the images
(define launcher (SDL:load-image (string-append datadir "launcher.png")))
(define launcher-moved #t)
(define launcher-rotated launcher)
(define ball (SDL:load-image (string-append datadir "circle.png")))
(define gnu-head (SDL:load-image (string-append datadir "gnu-head.png")))

;; setup the video mode
(define rows 10)
(define columns 12)
(define ball-size (SDL:surface:w ball))
(define launcher-offset (quotient (SDL:surface:w launcher) 2))
(define screen-width (* rows ball-size))
(define screen-height (+ (* columns ball-size) launcher-offset))
(define screen-top-border 0)
(define screen-bottom-border (+ screen-height ball-size))
(define screen-left-border 0)
(define screen-right-border (- screen-width ball-size))
(SDL:set-video-mode screen-width screen-height 24)

;; the grid
(define the-grid
  (make-vector (+ 4 rows) #f))

(do ((i (- (vector-length the-grid) 1) (- i 1)))
    ((< i 0))
  (vector-set! the-grid i (make-vector (+ 4 columns) #f)))

(define (make-grid-rect row col)
  (let* ((ball-size/4 (quotient ball-size 4))
         (y (* row ball-size))
         (x (* col ball-size)))
    (cond ((odd? row)
           (set! y (- y ball-size/4))
           (set! x (- x ball-size/4))))
    (SDL:make-rect x y ball-size ball-size)))

(define (grid-ref x y)
  (vector-ref (vector-ref the-grid x) y))

(define (grid-set! x y value)
  (vector-set! (vector-ref the-grid x) y value))

(do ((row 0 (1+ row)))
    ((>= row rows))
  (grid-set! row 0 (make <left-edge> :location (make-grid-rect row 0)))
  (grid-set! row 1 (make <left-edge> :location (make-grid-rect row 1)))
  (grid-set! row 2 (make <left-edge> :location (make-grid-rect row 2)))
  (grid-set! row columns
             (make <right-edge> :location (make-grid-rect row columns)))
  (grid-set! row (- columns 1)
             (make <right-edge> :location (make-grid-rect row (- columns 1))))
  (grid-set! row (- columns 2)
             (make <right-edge> :location (make-grid-rect row (- columns 2)))))

(do ((col 0 (1+ col)))
    ((>= col columns))
  (grid-set! 0 col (make <top-edge> :location (make-grid-rect 0 col)))
  (if (even? col) (grid-set! 1 col (make <top-edge> :location (make-grid-rect 1 col)))))

;(display the-grid) (newline)

(define xy->grid
  (let ((ball-size*2 (* ball-size 2))
        (ball-size/2 (quotient ball-size 2))
        (ball-size/4 (quotient ball-size 4)))
    (lambda (x y)
      (let* ((grid-row (1+ (quotient y ball-size)))
             (grid-col (1+ (quotient x ball-size)))
             (grid-row/2 (quotient y ball-size*2))
             (grid-col/2 (quotient x ball-size*2))
             (unit-x (- x (* grid-col/2 ball-size/2)))
             (unit-y (- y (* grid-row/2 ball-size/2))))
        (cond ((and (even? grid-row/2) (even? grid-col/2))
               ;; y = -2x + 0.5
               ;;(display (format #f "unit-x = ~A,  ball/4 = ~A\n" unit-x ball-size/4))
               ;;(display (format #f "-2x+1/2 = ~A,  unit-y = ~A\n" (+ (* -2 unit-x) 0.5) unit-y))
               (if (or (>= unit-x ball-size/4)
                       (<= (+ (* -2 unit-x) ball-size/2) unit-y))
                   (list grid-row (1+ grid-col))
                   (list grid-row grid-col)))
              ((and (even? grid-row/2) (odd? grid-col/2))
               ;; y = 2x + 0.5
               (if (or (>= unit-x ball-size/4)
                       (>= (+ (* 2 unit-x) ball-size/2) unit-y))
                   (list grid-row grid-col)
                   (list grid-row (1+ grid-col))))
              ((and (odd? grid-row/2) (even? grid-col/2))
               ;; y = 2x - 1.5
               (if (or (<= unit-x ball-size/4)
                       (<= (- (* 2 unit-x) (+ ball-size ball-size/2)) unit-y))
                   (list (- grid-row 1) (1+ grid-col))
                   (list grid-row grid-col)))
              ((and (odd? grid-row/2) (odd? grid-col/2))
               ;; y = -2x + 2.5
               (if (or (<= unit-x ball-size/4)
                       (>= (+ (* -2 unit-x) (* 2.5 ball-size)) unit-y))
                   (list (- grid-row 1) grid-col)
                   (list grid-row (1+ grid-col))))
              )))))

(define colors `((red     . ,(SDL:make-color #xff #x00 #x00))
                 (green   . ,(SDL:make-color #x00 #xff #x00))
                 (blue    . ,(SDL:make-color #x00 #x00 #xff))
                 (cyan    . ,(SDL:make-color #x00 #xff #xff))
                 (yellow  . ,(SDL:make-color #xff #xff #x00))
                 (magenta . ,(SDL:make-color #xff #x00 #xff))
                 (orange  . ,(SDL:make-color #xff #xbb #x00))
                 (grey    . ,(SDL:make-color #x88 #x88 #x88))
                 ))

(define color-balls
  (let ((overlay (SDL:make-surface 32 32))
        (rect (SDL:make-rect 0 0 32 32)))
    (map
     (lambda (color-pair)
       (let* ((name (car color-pair))
              (color (cdr color-pair))
              (result (SDL:make-surface 32 32))
              (color-value (SDL:map-rgb (SDL:surface:format overlay)
                                        (SDL:color:r color)
                                        (SDL:color:g color)
                                        (SDL:color:b color))))
         ;; copy the gnu head
         (SDL:blit-surface gnu-head rect result rect)
         ;; fill the overlay with the color-value
         (SDL:fill-rect overlay rect color-value)
         ;; blit the bgcolor border onto the overlay
         (SDL:blit-surface ball rect overlay rect)
         ;; make the circle translucent
         (SDL:set-color-key! overlay '(SDL_SRCCOLORKEY SDL_RLEACCEL) 0)
         (SDL:set-alpha! overlay '(SDL_SRCALPHA SDL_RLEACCEL) 128)
         ;; blit it over the gnu-head
         (SDL:blit-surface overlay rect result rect)
         ;; now make that border transparent, leaving just the circle
         (SDL:set-color-key! result '(SDL_SRCCOLORKEY SDL_RLEACCEL) 0)
         ;; and return this in a named pair
         (cons name result)))
     colors)))

(define rand-ball
  (let ((l (length color-balls)))
    (lambda ()
      (cdr (list-ref color-balls (random l))))))

(define rand-ball-sprite
  (let* ((x (quotient (- screen-width ball-size) 2))
         (y (- screen-height ball-size))
         (rect (SDL:make-rect x y (SDL:surface:w ball) (SDL:surface:h ball)))
         (coords (xy->grid x y))
         (row (car coords))
         (col (cadr coords)))
    (lambda ()
      (make <ball>
        :frames (circular-list (rand-ball))
        :location (SDL:copy-rect rect)
        :row row
        :col col))))

;; colors
(define bgcolor (SDL:map-rgb (SDL:surface:format (SDL:get-video-surface))
                             #x00 #x00 #x00))

;; rotate a square retaining its original size
(define (SDL:rotate-square src angle)
  (let* ((width (SDL:surface:w src))
         (height (SDL:surface:h src))
         (rotated (GFX:roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (SDL:surface:w rotated))
         (new-height (SDL:surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (SDL:make-rect width-offset height-offset width height))
         (dst (SDL:make-surface width height))
         (dst-rect (SDL:make-rect 0 0 width height)))
    (SDL:blit-surface rotated src-rect dst dst-rect)
    dst))

;; redisplay the launcher
(define redisplay-launcher
  (let* ((launcher-width (SDL:surface:w launcher))
         (dst-rect
          (SDL:make-rect (- (quotient screen-width 2) launcher-offset)
                         (- screen-height launcher-offset)
                         launcher-width launcher-offset))
         (src-rect (SDL:make-rect 0 0 launcher-width launcher-offset)))
    (lambda ()
      (cond (launcher-moved
             (set! launcher-moved #f)
             (SDL:blit-surface launcher-rotated src-rect bgsurface dst-rect)
             (SDL:blit-surface bgsurface dst-rect)
             (SDL:update-rect (SDL:get-video-surface) dst-rect))))))

;; pivot the launcher by an offset
(define (pivot-launcher offset)
  (let ((new-angle (+ launcher-angle offset)))
    (if (and (>= new-angle 0) (<= new-angle 180))
        (begin (set! launcher-angle new-angle)
               (set! launcher-moved #t)
               (set! launcher-rotated
                     (SDL:rotate-square launcher new-angle))))))


(define (make-angle-mover sprite angle . args)
  (let* ((ball (car (sprite:frames sprite)))
         (src-rect (sprite:dimensions sprite))
         (velocity (if (null? args) default-velocity (car args)))
         (off-x (* velocity (cos angle)))
         (off-y (* velocity (sin angle)))
         (width (SDL:surface:w ball))
         (height (SDL:surface:h ball)))
    (lambda (sprite)
      (let* ((dst-rect (sprite:location sprite))
             (x (SDL:rect:x dst-rect))
             (y (SDL:rect:y dst-rect))
             (new-x (+ x off-x))
             (new-y (- y off-y)))
        ;;(display (format #f "(angle-mover ~A) ~A => " angle dst-rect))
        (SDL:rect:set-x! dst-rect (inexact->exact new-x))
        (SDL:rect:set-y! dst-rect (inexact->exact new-y))))))

(define (degrees->radians deg)
  (* deg (/ pi 180)))

;; Launch the ball from the current angle
;;   angle is translated to radians, which is what guile uses for
;;   sin/cos, unlike GFX:roto-zoom-surface which uses degrees.
(define (launch)
  (if (< balls-in-motion max-balls)
      (let ((ball (rand-ball-sprite))
            (ball-angle (degrees->radians launcher-angle)))
        (set! (sprite:angle ball) ball-angle)
        (set! (sprite:move-proc ball) (make-angle-mover ball ball-angle))
        (set! balls-in-motion (1+ balls-in-motion))
        (set! active-sprites (cons ball active-sprites)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize screen
(SDL:set-caption "Bust-A-GNU" "Bust-A-GNU")
(define bgsurface (SDL:make-surface screen-width screen-height))
(SDL:fill-rect bgsurface #f bgcolor)
(redisplay-launcher)
(SDL:blit-surface bgsurface)
(SDL:flip)

;; event handler
(let ((next-update-ticks (+ 10 (SDL:get-ticks))))
  (let handle ((e (SDL:make-event)))
    ;; poll for the next event
    (SDL:poll-event e)
    (case (SDL:event:type e)
      ((SDL_MOUSEBUTTONUP)
       (set! e (SDL:make-event 'SDL_USEREVENT))
       (let* ((xy-coords (SDL:get-mouse-state))
              (x (cdr (assq 'x xy-coords)))
              (y (cdr (assq 'y xy-coords)))
              (grid-coords (xy->grid x y)))
         (display (format #f "(xy->grid ~A ~A) => (~A ~A)\n" x y
                          (car grid-coords) (cadr grid-coords)))))
      ((SDL_KEYDOWN)
       (case (SDL:event:key:keysym:sym e)
         ((SDLK_LEFT)
          ;; pivot left (by 4 degrees, or 1 degree if control is held)
          (pivot-launcher
           (let ((mods (SDL:event:key:keysym:mod e)))
             (if (or (memq 'KMOD_LCTRL mods)
                     (memq 'KMOD_RCTRL mods))
                 +1 +4))))
         ((SDLK_RIGHT)
          ;; pivot right (by 4 degrees, or 1 degree if control is held)
          (pivot-launcher
           (let ((mods (SDL:event:key:keysym:mod e)))
             (if (or (memq 'KMOD_LCTRL mods)
                     (memq 'KMOD_RCTRL mods))
                 -1 -4))))
         ((SDLK_SPACE)
          ;; space to shoot
          ;; (wait until they release the key)
          (while (not (eq? (SDL:event:type e) 'SDL_KEYUP))
                 (SDL:poll-event e))
          ;; then launch
          (launch))
         ((SDLK_ESCAPE)
          ;; escape to quit (XXXX add confirmation)
          (SDL:quit)
          (quit)))))
    ;; update the sprites
    (let ((ticks (SDL:get-ticks)))
      (cond ((> ticks next-update-ticks)
             (set! next-update-ticks (+ 50 ticks))
             (update-sprites))))
    ;; repeat
    (handle e)))

;;; bustagnu.scm ends here
