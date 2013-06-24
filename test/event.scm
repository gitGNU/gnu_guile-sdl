;;; event.scm --- simple event test

;; Copyright (C) 2003, 2004, 2005, 2008, 2009, 2011, 2013 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

(use-modules ((srfi srfi-11) #:select (let-values))
             ((srfi srfi-13) #:select (string-suffix?)))
(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl gfx) #:prefix GFX:))

(define (check-joystick-maybe)
  (let ((count (SDL:num-joysticks))
        (name (SDL:joystick-name))
        (joy (SDL:joystick-open)))
    (info "joysticks count => ~S" count)
    (info "default joystick name => ~S" name)
    (if (zero? count)
        (and joy (error "phantom joystick:" joy))
        (or joy (error "joystick-open failed")))
    (or (not joy)
        (SDL:joystick? joy)
        (error "joystick-open returned something weird:" joy))
    (and joy
         (let ((opened? (SDL:joystick-opened?))
               (index (SDL:joystick-index joy))
               (num-axes (SDL:joystick-num-axes joy))
               (num-balls (SDL:joystick-num-balls joy))
               (num-hats (SDL:joystick-num-hats joy))
               (num-buttons (SDL:joystick-num-buttons joy)))
           (info "joystick ~S (~A), ~A ~A, ~A ~A, ~A ~A, ~A ~A, ~A ~A"
                 name (if opened? 'open 'still-closed!)
                 'index index
                 'axes num-axes
                 'balls num-balls
                 'hats num-hats
                 'buttons num-buttons)))
    joy))

;; initialize the SDL video (and event) module
(info "init => ~S" (SDL:init '(SDL_INIT_VIDEO SDL_INIT_JOYSTICK)))

;; enable unicode
(define unicode-enabled
  (let ((try (string-suffix? "UTF-8" (or (getenv "LANG")
                                         ""))))
    (info "(enable-unicode)    => ~S"     (SDL:enable-unicode))
    (info "(enable-unicode ~S) => ~S" try (SDL:enable-unicode try))
    (info "(enable-unicode)    => ~S"     (SDL:enable-unicode))
    (SDL:enable-unicode)))

(define JOY (check-joystick-maybe))

;; get a sample rect size from a list of available modes
(define test-rect (SDL:make-rect 0 0 400 600))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 8
                    '(SDL_HWSURFACE SDL_DOUBLEBUF))

;; presize some stuff
(define height 10)
(define top (half (- (SDL:rect:h test-rect) height)))

;; color to write in
(define WHITE #xFFFFFFFF)

;; proc to write text centered on screen at a certain vertical position
(define (display-centered-w/height-proc y)
  (let ((text-rect (SDL:make-rect 0 y (SDL:rect:w test-rect) height))
        (full-width (SDL:rect:w test-rect))
        (screen (SDL:get-video-surface)))
    ;; rv
    (lambda (fstr . args)
      (let* ((text (apply fs fstr args))
             (width (* 8 (string-length text)))
             (left (half (- full-width width))))
        (SDL:fill-rect screen text-rect 0)
        (GFX:draw-string screen left y text WHITE)
        (SDL:flip)))))

(define scroll-up!
  (let* ((screen (SDL:get-video-surface))
         (w (SDL:rect:w test-rect))
         (srect (SDL:make-rect 0 height w (+ top height)))
         (drect (SDL:make-rect 0 0 w top)))
    ;; scroll-up!
    (lambda ()
      (SDL:blit-surface screen srect
                        screen drect))))

;; write text centered on screen
(define display-centered
  (display-centered-w/height-proc top))
(define display-centered/next-line
  (display-centered-w/height-proc (+ 3 height top)))
(define display-centered/next-next-line
  (display-centered-w/height-proc
   (+ 3 (ash height 1) top)))

;; set the event filter: ignore the mouse every other second
(define (ignore-maybe event-type)
  (not (and (eq? 'SDL_MOUSEMOTION event-type)
            (even? (car (gettimeofday))))))
(SDL:set-event-filter ignore-maybe #f)
(info "get-event-filter => ~S" (SDL:get-event-filter))

;; mod and mouse state
(SDL:set-mod-state '(KMOD_LALT KMOD_RALT))
(info "(jammed) mod state => ~S" (SDL:get-mod-state))
(info "(actual) mouse state => ~S" (SDL:get-mouse-state #t))

(define (fake type . rest)
  (info "(fake) ~A" type)
  (let ((ev (SDL:make-event type)))
    ;; exercise
    (SDL:event:set-type! ev type)
    (let loop ((spec rest))
      (or (null? spec)
          (let ((setter (car spec))
                (value (cadr spec)))
            (info " ~A ~S" (procedure-name setter) value)
            (setter ev value)
            (loop (cddr spec)))))
    (SDL:push-event ev)))

(define (fake-key-down/up mod sym)
  (define (ok type state)
    (fake type
          SDL:event:key:set-state!           state
          SDL:event:key:keysym:set-mod!      mod
          SDL:event:key:keysym:set-sym!      sym
          ;; These two are pure exercise.
          SDL:event:key:keysym:set-scancode! 0
          SDL:event:key:keysym:set-unicode!  0))
  (ok 'SDL_KEYDOWN 'pressed)
  (ok 'SDL_KEYUP 'released))

(define draw-relative-rectangle!
  (let* ((screen (SDL:get-video-surface))
         (cx (half (SDL:rect:w test-rect)))
         (cy (+ top (half top) 30))
         (full (SDL:make-rect (- cx 100) (- cy 100) 201 201))
         (rect (SDL:make-rect cx cy 100 100)))
    ;; draw-relative-rectangle!
    (lambda (rx ry)
      (cond ((positive? rx)
             (SDL:rect:set-x! rect cx)
             (SDL:rect:set-w! rect rx))
            (else
             (SDL:rect:set-x! rect (+ cx rx))
             (SDL:rect:set-w! rect (- rx))))
      (cond ((positive? ry)
             (SDL:rect:set-y! rect cy)
             (SDL:rect:set-h! rect ry))
            (else
             (SDL:rect:set-y! rect (+ cy ry))
             (SDL:rect:set-h! rect (- ry))))
      (SDL:fill-rect screen full #xffffff)
      (SDL:fill-rect screen rect 0)
      (SDL:update-rects screen (list full rect)))))

(define (nicer count)
  (lambda (symbol)
    (substring (symbol->string symbol) count)))

(define nice-type (nicer 4))

(define no-5 (nicer 5))

;; event loop
(define input-loop
  (lambda (e)
    (let* ((next-event (SDL:wait-event e))
           (event-type (SDL:event:type e))
           (nice (nice-type event-type)))

      (define (check-updn state)
        (or (eq? (case event-type
                   ((SDL_KEYDOWN SDL_MOUSEBUTTONDOWN SDL_JOYBUTTONDOWN)
                    'pressed)
                   ((SDL_KEYUP SDL_MOUSEBUTTONUP SDL_JOYBUTTONUP)
                    'released)
                   (else #f))
                 state)
            (error "unexpected state:" state)))

      (scroll-up!)
      (display-centered/next-line
       "~A" (map no-5 (SDL:get-key-state)))
      (let-values (((buttons x y) (SDL:mouse-bxy #t)))
        (draw-relative-rectangle! x y)
        (display-centered/next-next-line "~A ~A ~A" buttons x y)
        (let* ((alist (SDL:get-mouse-relative-state #t))
               (gmrs-state (assq-ref alist 'state))
               (gmrs-x     (assq-ref alist 'x))
               (gmrs-y     (assq-ref alist 'y)))
          (or (null? buttons)
              (SDL:button? buttons)
              (error "mouse-bxy and button? do not agree!"))
          (or (equal? buttons gmrs-state)
              (error "mouse-bxy and get-mouse-relative-state disagree:"
                     (list 'bxy: buttons
                           'rel: gmrs-state)))
          (or (zero? gmrs-x)
              (error "get-mouse-relative-state x non-zero:" gmrs-x))
          (or (zero? gmrs-y)
              (error "get-mouse-relative-state y non-zero:" gmrs-y))))
      (case event-type
        ((SDL_KEYDOWN SDL_KEYUP)
         (let ((sym (SDL:event:key:keysym:sym e))
               (mods (SDL:event:key:keysym:mod e))
               (uni (if unicode-enabled
                        (SDL:event:key:keysym:unicode e)
                        0)))
           (display-centered "~A -- ~A~A -- ~A~A" nice
                             (SDL:event:key:keysym:scancode e)
                             (if (zero? uni)
                                 ""
                                 (fs " U+~A" (number->string uni 16)))
                             (no-5 sym)
                             (if (null? mods)
                                 ""
                                 (fs " -- ~A" (map no-5 mods))))
           (check-updn (SDL:event:key:state e))
           (and (eq? sym 'SDLK_SPACE)
                (if (SDL:get-event-filter)
                    (SDL:set-event-filter #f #f)
                    (SDL:set-event-filter ignore-maybe #f)))
           (if (eq? sym 'SDLK_ESCAPE)
               #f
               (input-loop e))))
        ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
         (let ((button (SDL:event:button:button e)))
           (display-centered "~A -- ~A -- ~Ax~A" nice button
                             (SDL:event:button:x e)
                             (SDL:event:button:y e)))
         (check-updn (SDL:event:button:state e))
         (input-loop e))
        ((SDL_MOUSEMOTION)
         (let ((x (SDL:event:motion:x e))
               (y (SDL:event:motion:y e)))
           (display-centered "~A -- ~Ax~A ~S" nice x y
                             (cons* (SDL:event:motion:xrel e)
                                    (SDL:event:motion:yrel e)
                                    (SDL:event:motion:state e))))
         (input-loop e))
        ((SDL_JOYBUTTONDOWN SDL_JOYBUTTONUP)
         (display-centered "~A -- j~A b~A"
                           nice
                           (SDL:event:jbutton:which e)
                           (SDL:event:jbutton:button e))
         (check-updn (SDL:event:jbutton:state e))
         (input-loop e))
        ((SDL_JOYAXISMOTION)
         (display-centered "~A -- j~S a~S ~S" nice
                           (SDL:event:jaxis:which e)
                           (SDL:event:jaxis:axis e)
                           (SDL:event:jaxis:value e))
         (input-loop e))
        ((SDL_JOYBALLMOTION)
         (display-centered "~A -- j~S b~S (~S, ~S)" nice
                           (SDL:event:jball:which e)
                           (SDL:event:jball:ball e)
                           (SDL:event:jball:xrel e)
                           (SDL:event:jball:yrel e))
         (input-loop e))
        ((SDL_JOYHATMOTION)
         (display-centered "~A -- j~S h~S ~S" nice
                           (SDL:event:jhat:which e)
                           (SDL:event:jhat:hat e)
                           (SDL:event:jhat:value e))
         (input-loop e))
        ((SDL_ACTIVEEVENT)
         (display-centered "~A -- ~S ~S" nice
                           (SDL:event:active:gain e)
                           (SDL:event:active:state e))
         (input-loop e))
        ((SDL_VIDEORESIZE)
         (display-centered "~A -- ~Sx~S" nice
                           (SDL:event:resize:w e)
                           (SDL:event:resize:h e))
         (input-loop e))
        (else
         (display-centered "~A" nice)
         (input-loop e))))))

;; report app state
(display-centered "app state: ~S" (SDL:get-app-state))

;; report event states
(for-each (lambda (type)
            (scroll-up!)
            (display-centered "~A : ~A"
                              (nice-type type)
                              (SDL:event-state type 'SDL_QUERY)))
          (SDL:enumstash-enums SDL:event-types))

;; display an explanatory message
((display-centered-w/height-proc (- (SDL:rect:h test-rect) height 5))
 "(Press Escape to Quit, Space to Toggle Filter)")

;; enable keyboard repeat
(SDL:enable-key-repeat 250 6)

;; be grabby, but not for long
(let ((cur (SDL:grab-input 'query)))
  (or (eq? 'on (SDL:grab-input 'on))
      (error "could not grab input"))
  (or (eq? 'off (SDL:grab-input 'off))
      (error "could not ungrab input"))
  (or (eq? cur (SDL:grab-input cur))
      (error "could not restore grab state")))

;; synthesize some events
(let ((x (half (SDL:rect:w test-rect)))
      (y (+ top (half top) 30)))
  (and *interactive* (SDL:delay 420))
  ;; mouse
  (SDL:warp-mouse x y)                  ; produces MOUSEMOTION
  (fake 'SDL_MOUSEBUTTONDOWN
        SDL:event:button:set-button! 'middle
        SDL:event:button:set-state!  'pressed
        SDL:event:button:set-x!      x
        SDL:event:button:set-y!      y)
  (fake 'SDL_MOUSEMOTION
        SDL:event:motion:set-state! 'released
        SDL:event:motion:set-x!     (1+ x)
        SDL:event:motion:set-xrel!   1
        SDL:event:motion:set-y!     (1+ y)
        SDL:event:motion:set-yrel!   1)
  (fake 'SDL_MOUSEBUTTONUP
        SDL:event:button:set-button! 'middle
        SDL:event:button:set-state!  'released
        SDL:event:button:set-x!      (1+ x)
        SDL:event:button:set-y!      (1+ y))
  ;; keyboard
  (for-each (lambda (m c)
              (define (mod prefix)
                (string->symbol (fs "KMOD_~A~A" prefix m)))
              (fake-key-down/up (list (mod 'L)
                                      (mod 'R))
                                (string->symbol
                                 (fs "SDLK_~A" c))))
            (list 'SHIFT 'ALT 'CTRL)
            (string->list "gnu"))
  ;; active-ness
  (let ((cur (delq 'active (SDL:get-app-state))))
    (fake 'SDL_ACTIVEEVENT
          SDL:event:active:set-gain!  'lost
          SDL:event:active:set-state!  cur)
    (fake 'SDL_ACTIVEEVENT
          SDL:event:active:set-gain!  'gained
          SDL:event:active:set-state!  cur))
  ;; joystick
  (fake 'SDL_JOYAXISMOTION
        SDL:event:jaxis:set-which! 0
        SDL:event:jaxis:set-axis!  42
        SDL:event:jaxis:set-value! 420)
  (fake 'SDL_JOYBALLMOTION
        SDL:event:jball:set-which! 0
        SDL:event:jball:set-ball!  4
        SDL:event:jball:set-xrel!  42
        SDL:event:jball:set-yrel!  420)
  (fake 'SDL_JOYBUTTONDOWN
        SDL:event:jbutton:set-which!  0
        SDL:event:jbutton:set-button! 42
        SDL:event:jbutton:set-state!  'pressed)
  (fake 'SDL_JOYBUTTONUP
        SDL:event:jbutton:set-which!  0
        SDL:event:jbutton:set-button! 42
        SDL:event:jbutton:set-state!  'released)
  (fake 'SDL_JOYHATMOTION
        SDL:event:jhat:set-which! 0
        SDL:event:jhat:set-hat!   42
        SDL:event:jhat:set-value! '(left up))
  ;; resize
  (fake 'SDL_VIDEORESIZE
        SDL:event:resize:set-h! 420
        SDL:event:resize:set-w! 420)
  ;; bail if not interactive
  (or *interactive* (fake-key-down/up '() 'SDLK_ESCAPE)))

;; main loop
(scroll-up!)
(input-loop (SDL:make-event 0))

;; quit SDL
(and JOY (SDL:joystick-close JOY))
(or *interactive* (SDL:delay 420))
(exit (SDL:quit))

;;; event.scm ends here
