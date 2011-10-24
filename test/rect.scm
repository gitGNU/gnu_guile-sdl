;;; rect.scm --- simple rectangle test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008 Thien-Thi Nguyen
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

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

(and debug?
     (for-each (lambda (x) (fso "video-info: ~S~%" x))
               (SDL:get-video-info)))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (and debug? (fso "hmm: (SDL:list-modes) => ~A~%" modes))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (SDL:make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))
(and debug? (fso "test-rect => ~A~%" test-rect))

(set! *random-state* (seed->random-state (current-time)))

(define (rand-rect limit)
  (let* ((limit-w (SDL:rect:w limit))
         (limit-h (SDL:rect:h limit))
         (two-x (list (random limit-w) (random limit-w)))
         (two-y (list (random limit-h) (random limit-h)))
         (x0 (apply min two-x))
         (y0 (apply min two-y))
         (x1 (apply max two-x))
         (y1 (apply max two-y)))
    (SDL:make-rect (- x1 x0 -1) (- y1 y0 -1) x0 y0)))

;; set the video mode to the dimensions of our rect
(define screen (SDL:set-video-mode (SDL:rect:w test-rect)
                                   (SDL:rect:h test-rect)
                                   16 '(SDL_HWSURFACE)))

;; draw some rectangles filled with random colors
(do ((i 0 (1+ i)))
    ((= i 20))
  (let ((sample (rand-rect test-rect)))
    (SDL:fill-rect screen sample (random #xffffff))
    (SDL:update-rect screen sample))
  (SDL:delay 100))

;; quit SDL
(SDL:quit)

;;; rect.scm ends here
