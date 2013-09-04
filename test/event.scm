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

(use-modules ((srfi srfi-1) #:select (car+cdr))
             ((srfi srfi-11) #:select (let-values))
             ((srfi srfi-13) #:select (string-suffix?)))
(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl gfx) #:prefix GFX:))
(use-modules ((sdl misc-utils) #:select (ignore-all-event-types-except)))

(define (check-joystick-maybe)
  (info "joystick polling => ~S"
        (let* ((ans (SDL:joystick-polling))
               (raw (SDL:joystick-event-state 'SDL_QUERY))
               (old (case raw
                      ((SDL_ENABLE) #t)
                      ((SDL_IGNORE) #f)
                      (else (error "WTF:" raw)))))
          (or (eq? ans old)
              (error "discrepency:" (list 'ans: ans 'old: old)))
          ans))
  (or (SDL:joystick-polling)
      (SDL:joystick-polling #t)
      (error "could not enable joystick polling"))
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
                 'buttons num-buttons)
           ;; rv
           (list joy
                 num-axes
                 num-balls
                 num-hats
                 num-buttons)))))

;; initialize the SDL video (and event) module
(info "init => ~S" (SDL:init 'video))
(info "joystick init => ~S" (SDL:init-subsystem 'joystick))

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
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 32
                    '(hw-surface doublebuf))

;; presize some stuff
(define height 10)
(define top (half (- (SDL:rect:h test-rect) height)))

;; color to write in
(define WHITE #xFFFFFFFF)

;; color of the relative-rectangle background
(define relrect-bg #f)

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

;; set the event filter: rat (non)* grata
(define rat-grata!
  (let ((idx 0)
        (all (vector
              (cons
               #x00ff00                 ; green
               #f)
              (cons
               #xffff00                 ; yellow
               (lambda (type)
                 (not (and (eq? 'mouse-motion type)
                           (even? (car (gettimeofday)))))))
              (cons
               #xff0000                 ; red
               (lambda (type)
                 (not (eq? 'mouse-motion type)))))))
    ;; rat-grata!
    (lambda ()
      (set! idx (modulo (1+ idx) 3))
      (let-values (((color proc) (car+cdr (vector-ref all idx))))
        (set! relrect-bg color)
        (SDL:set-event-filter proc #f)))))

(rat-grata!)
(info "get-event-filter => ~S" (SDL:get-event-filter))

;; mod and mouse state
(SDL:set-mod-state '(L-alt R-alt))
(info "(jammed) mod state => ~S" (SDL:get-mod-state))
(info "(actual) mouse state => ~S" (SDL:get-mouse-state #t))

(define (fake type . rest)
  (info "(fake)\t~A" type)
  (let ((ev (SDL:make-event type)))
    ;; exercise
    (SDL:event:set-type! ev type)
    (let loop ((spec rest))
      (or (null? spec)
          (let ((setter (car spec))
                (value (cadr spec)))
            (info "\t~A ~S" (procedure-name setter) value)
            (setter ev value)
            (loop (cddr spec)))))
    (or (SDL:push-event ev)
        (error (fs "could not push ~A event" type)))))

(define (fake-key-down/up mod sym)
  (define (ok type state)
    (fake type
          SDL:event:key:set-state!           state
          SDL:event:key:keysym:set-mod!      mod
          SDL:event:key:keysym:set-sym!      sym
          ;; These two are pure exercise.
          SDL:event:key:keysym:set-scancode! 0
          SDL:event:key:keysym:set-unicode!  0))
  (ok 'key-down 'pressed)
  (ok 'key-up 'released))

(define tickle-joy-spew! (display-centered-w/height-proc
                          (+ 3 (* 3 height) top)))

(define (tickle-joy! joy num-axes num-balls num-hats num-buttons)

  (define (axis n)
    (let ((v (SDL:joystick-get-axis joy n)))
      (and (not (zero? v))
           (fs "a~A:~A" n v))))

  (define (ball n)
    (let* ((sel (random num-balls))
           (alist (SDL:joystick-get-ball joy n))
           (dx (assq-ref alist 'dx))
           (dy (assq-ref alist 'dy)))
      (let-values (((x y) (SDL:joystick-ball-xy joy n)))
        (or (and (= dx x) (dy y))
            (error "joystick-{ball-xy,get-ball} mismatch:"
                   (list 'dx: dx
                         'dy: dy
                         'x: x
                         'y: y)))
        (and (not (and (zero? x)
                       (zero? y)))
             (fs "b~A:~A|~A" n x y)))))

  (define (hat n)
    (let ((v (SDL:joystick-get-hat joy n)))
      (and (not (zero? v))
           (fs "h~A:~A" n v))))

  (define (button n)
    (and (eq? 'pressed (SDL:joystick-get-button joy n))
         (fs ":~A" n)))

  (define (interesting)
    (delq #f (append (map axis (iota num-axes))
                     (map ball (iota num-balls))
                     (map hat (iota num-hats))
                     (map button (iota num-buttons)))))

  (SDL:joystick-update)
  (let ((ls (interesting)))
    (or (null? ls)
        (tickle-joy-spew! "~A" ls))))

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
      (SDL:fill-rect screen full relrect-bg)
      (SDL:fill-rect screen rect 0)
      (SDL:update-rects screen (list full rect)))))

(define (nicer count)
  (lambda (symbol)
    (substring (symbol->string symbol) count)))

(define nice-type symbol->string)

;; event loop
(define input-loop
  (lambda (e)
    (let* ((next-event (SDL:wait-event e))
           (event-type (SDL:event:type e))
           (nice (nice-type event-type)))

      (define (check-updn state)
        (or (eq? (case event-type
                   ((key-down mouse-button-down joy-button-down)
                    'pressed)
                   ((key-up mouse-button-up joy-button-up)
                    'released)
                   (else #f))
                 state)
            (error "unexpected state:" state)))

      (and JOY (apply tickle-joy! JOY))
      (scroll-up!)
      (display-centered/next-line
       "~A" (SDL:get-key-state))
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
        ((key-down key-up)
         (let ((sym (SDL:event:key:keysym:sym e))
               (mods (SDL:event:key:keysym:mod e))
               (uni (if unicode-enabled
                        (SDL:event:key:keysym:unicode e)
                        0)))
           (display-centered "~A . ~A~A . ~A~A" nice
                             (SDL:event:key:keysym:scancode e)
                             (if (zero? uni)
                                 ""
                                 (fs " U+~A" (hex uni)))
                             sym
                             (if (null? mods)
                                 ""
                                 (fs " . ~A" mods)))
           (check-updn (SDL:event:key:state e))
           (and (eq? sym 'space)
                (eq? event-type 'key-up)
                (begin
                  (rat-grata!)
                  (draw-relative-rectangle! 0 0)))
           (if (eq? sym 'escape)
               #f
               (input-loop e))))
        ((mouse-button-down mouse-button-up)
         (let ((button (SDL:event:button:button e)))
           (display-centered "~A . ~A . ~Ax~A" nice button
                             (SDL:event:button:x e)
                             (SDL:event:button:y e)))
         (check-updn (SDL:event:button:state e))
         (input-loop e))
        ((mouse-motion)
         (let ((x (SDL:event:motion:x e))
               (y (SDL:event:motion:y e)))
           (display-centered "~A . ~Ax~A ~S" nice x y
                             (cons* (SDL:event:motion:xrel e)
                                    (SDL:event:motion:yrel e)
                                    (SDL:event:motion:state e))))
         (input-loop e))
        ((joy-button-down joy-button-up)
         (display-centered "~A . j~A b~A"
                           nice
                           (SDL:event:jbutton:which e)
                           (SDL:event:jbutton:button e))
         (check-updn (SDL:event:jbutton:state e))
         (input-loop e))
        ((joy-axis-motion)
         (display-centered "~A . j~S a~S ~S" nice
                           (SDL:event:jaxis:which e)
                           (SDL:event:jaxis:axis e)
                           (SDL:event:jaxis:value e))
         (input-loop e))
        ((joy-ball-motion)
         (display-centered "~A . j~S b~S (~S, ~S)" nice
                           (SDL:event:jball:which e)
                           (SDL:event:jball:ball e)
                           (SDL:event:jball:xrel e)
                           (SDL:event:jball:yrel e))
         (input-loop e))
        ((joy-hat-motion)
         (display-centered "~A . j~S h~S ~S" nice
                           (SDL:event:jhat:which e)
                           (SDL:event:jhat:hat e)
                           (SDL:event:jhat:value e))
         (input-loop e))
        ((active)
         (display-centered "~A . ~S ~S" nice
                           (SDL:event:active:gain e)
                           (SDL:event:active:state e))
         (input-loop e))
        ((video-resize)
         (display-centered "~A . ~Sx~S" nice
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
            (display-centered
             "~A . ~A"
             (nice-type type)
             (let* ((ans (if (SDL:event-type-handling type)
                             'yes
                             'no))
                    (raw (SDL:event-state type 'SDL_QUERY))
                    (old (case raw
                           ((SDL_ENABLE) 'yes)
                           ((SDL_IGNORE) 'no)
                           (else (error "WTF:" raw)))))
               (or (eq? ans old)
                   (error "discrepency:" (list 'ans: ans 'old: old)))
               ans)))
          (map car (SDL:kotk 'event-type)))

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
  (fake 'mouse-button-down
        SDL:event:button:set-button! 'middle
        SDL:event:button:set-state!  'pressed
        SDL:event:button:set-x!      x
        SDL:event:button:set-y!      y)
  (fake 'mouse-motion
        SDL:event:motion:set-state! 'wheel-up
        SDL:event:motion:set-x!     (1+ x)
        SDL:event:motion:set-xrel!   1
        SDL:event:motion:set-y!     (1+ y)
        SDL:event:motion:set-yrel!   1)
  (fake 'mouse-button-up
        SDL:event:button:set-button! 'middle
        SDL:event:button:set-state!  'released
        SDL:event:button:set-x!      (1+ x)
        SDL:event:button:set-y!      (1+ y))
  ;; keyboard
  (for-each (lambda (m c)
              (define (mod prefix)
                (symbol-append prefix '- m))
              (fake-key-down/up (list (mod 'L)
                                      (mod 'R))
                                (string->symbol (string c))))
            (list 'shift 'alt 'ctrl)
            (string->list "gnu"))
  ;; active-ness
  (let ((cur (delq 'active (SDL:get-app-state))))
    (fake 'active
          SDL:event:active:set-gain!  'lost
          SDL:event:active:set-state!  cur)
    (fake 'active
          SDL:event:active:set-gain!  'gained
          SDL:event:active:set-state!  cur))
  ;; joystick
  (fake 'joy-axis-motion
        SDL:event:jaxis:set-which! 0
        SDL:event:jaxis:set-axis!  42
        SDL:event:jaxis:set-value! 420)
  (fake 'joy-ball-motion
        SDL:event:jball:set-which! 0
        SDL:event:jball:set-ball!  4
        SDL:event:jball:set-xrel!  42
        SDL:event:jball:set-yrel!  420)
  (fake 'joy-button-down
        SDL:event:jbutton:set-which!  0
        SDL:event:jbutton:set-button! 42
        SDL:event:jbutton:set-state!  'pressed)
  (fake 'joy-button-up
        SDL:event:jbutton:set-which!  0
        SDL:event:jbutton:set-button! 42
        SDL:event:jbutton:set-state!  'released)
  (fake 'joy-hat-motion
        SDL:event:jhat:set-which! 0
        SDL:event:jhat:set-hat!   42
        SDL:event:jhat:set-value! '(left up))
  ;; resize
  (fake 'video-resize
        SDL:event:resize:set-h! 420
        SDL:event:resize:set-w! 420))

;; queue interaction
(SDL:pump-events)
(let ((count (SDL:peep-events #f 10 'SDL_PEEKEVENT '())))
  (or (zero? count)
      (error "pending events not properly masked:"
             (cons 'count: count))))
(or (SDL:poll-event)
    (error "poll-event => #f"))
(let* ((count (SDL:evqueue-peek 99 #t))
       (ls (SDL:evqueue-get count #t))
       (len (length ls))
       (ok (= len count)))
  (info "~A pending events / ~A retrieved"
        count (if ok 'all (fs "but only ~A" len)))
  (or ok (error (fs "evqueue-get only retrieved ~A events!" len)))
  (let ((back (apply SDL:evqueue-add ls)))
    (info "~A events successfully re-enqueud" back)
    (or (= count back)
        (error "evqueue-add rv:" back))))
(scroll-up!)

;; fleeting ignorance
(let ((blurb! (display-centered-w/height-proc
               (- (SDL:rect:h test-rect) height 5))))
  (blurb! "WAIT A (QUARTER) SECOND!")
  (ignore-all-event-types-except)
  (SDL:delay 250)
  (ignore-all-event-types-except #f)
  (or *interactive*
      ;; arrange to bail
      (fake-key-down/up '() 'escape))
  (blurb! "(Press ESC to Quit, SPC to Cycle Filter)"))

;; main loop
(input-loop (SDL:make-event 0))

;; quit SDL
(and JOY (SDL:joystick-close (car JOY)))
(SDL:quit-subsystem 'joystick)
(or *interactive* (SDL:delay 420))
(exit (SDL:quit))

;;; event.scm ends here
