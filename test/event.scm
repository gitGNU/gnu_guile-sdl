;;; event.scm --- simple event test

;; Copyright (C) 2003, 2004, 2005, 2008, 2009 Thien-Thi Nguyen
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

(or *interactive* (exit-77 "interactive"))
(or *have-ttf* (exit-77 "ttf disabled"))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video (and event) module
(let ((res (SDL:init '(SDL_INIT_VIDEO))))
  (and debug? (fso "SDL:init: ~S\n" res)))

;; initialize the font lib
(let ((res (SDL:ttf-init)))
  (and debug? (fso "SDL:ttf-init: ~S\n" res)))

;; get a sample rect size from a list of available modes
(define test-rect (SDL:make-rect 0 0 600 200))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 8
                    '(SDL_HWSURFACE SDL_DOUBLEBUF))

;; load a font file
(define font (SDL:load-font (datafile "crystal.ttf") 16))

;; presize some stuff
(define height (SDL:font:height font))
(define top (quotient (- (SDL:rect:h test-rect) height) 2))

;; color to write in
(define white (SDL:make-color #xff #xff #xff))

;; proc to write text centered on screen at a certain vertical position
(define (display-centered-w/height-proc y)
  (let ((text-rect (SDL:make-rect 0 y (SDL:rect:w test-rect) height)))
    ;; rv
    (lambda (fstr . args)
      (let* ((text (apply simple-format #f fstr args))
             (rendered (SDL:render-text font text white #t))
             (dimensions (SDL:font:size-text font text))
             (width (assq-ref dimensions 'w))
             (screen (SDL:get-video-surface))
             (left (quotient (- (SDL:rect:w test-rect) width) 2))
             (dst-rect (SDL:make-rect left y width height))
             (src-rect (SDL:make-rect 0 0 width height)))
        (SDL:fill-rect screen text-rect 0)
        (SDL:blit-surface rendered src-rect screen dst-rect)
        (SDL:flip)))))

;; write text centered on screen
(define display-centered
  (display-centered-w/height-proc
   (quotient (- (SDL:rect:h test-rect) height) 2)))
(define display-centered/next-line
  (display-centered-w/height-proc
   (+ 3 height (quotient (- (SDL:rect:h test-rect) height) 2))))

;; set the event filter: ignore the mouse every other second
(define (ignore-maybe event-type)
  (not (and (eq? 'SDL_MOUSEMOTION event-type)
            (zero? (remainder (car (gettimeofday)) 2)))))
(SDL:set-event-filter ignore-maybe #f)
(and debug? (fso "event-filter: ~S~%" (SDL:get-event-filter)))

;; event loop
(define input-loop
  (lambda (e)
    (let* ((next-event (SDL:wait-event e))
           (event-type (SDL:event:type e)))
      (case event-type
        ((SDL_KEYDOWN SDL_KEYUP)
         (let ((sym (SDL:event:key:keysym:sym e))
               (mods (SDL:event:key:keysym:mod e)))
           (display-centered "~A: ~A ~A" event-type sym mods)
           (display-centered/next-line "~S" (SDL:get-key-state))
           (and (eq? sym 'SDLK_SPACE)
                (if (SDL:get-event-filter)
                    (SDL:set-event-filter #f #f)
                    (SDL:set-event-filter ignore-maybe #f)))
           (if (eq? sym 'SDLK_ESCAPE)
               #f
               (input-loop e))))
        ((SDL_MOUSEBUTTONDOWN SDL_MOUSEBUTTONUP)
         (let ((button (SDL:event:button:button e)))
           (display-centered "~A: ~A" event-type button)
           (display-centered/next-line "~S" (SDL:get-key-state)))
         (input-loop e))
        ((SDL_MOUSEMOTION)
         (let ((x (SDL:event:motion:x e))
               (y (SDL:event:motion:y e)))
           (display-centered "~A: ~Ax~A" event-type x y)
           (display-centered/next-line "~S" (SDL:get-key-state)))
         (input-loop e))
        (else
         (display-centered "~A" event-type)
         (input-loop e))))))

;; report event states
(for-each (lambda (type)
            (display-centered "~A : ~A"
                              type (SDL:event-state type 'SDL_QUERY))
            (SDL:delay 42))
          (SDL:enumstash-enums SDL:event-types))

;; display an explanatory message
((display-centered-w/height-proc (- (SDL:rect:h test-rect) height 5))
 "(Press Escape to Quit, Space to Toggle Filter)")

;; main loop
(input-loop (SDL:make-event 0))

;; quit SDL
(SDL:quit)

;;; event.scm ends here
