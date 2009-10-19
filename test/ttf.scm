;;; ttf.scm --- simple true type font test

;; Copyright (C) 2003, 2004, 2007, 2008 Thien-Thi Nguyen
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

(or *have-ttf* (exit-77 "ttf disabled"))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL video
(SDL:init '(SDL_INIT_VIDEO))

;; initialize the font lib
(or (= 0 (SDL:ttf-init)) (error "could not init font lib"))

;; the text to display
(define sentence (let ((ls (map symbol->string
                                '(The quick brown fox
                                      jumped over the
                                      lazy sleeping dog
                                      !!!))))
                   (set-cdr! (last-pair ls) ls)
                   ls))

;; load a font file
(define fonts (let* ((filename (datafile "crystal.ttf"))
                     (ls (map (lambda (size)
                                (SDL:load-font filename size))
                              '(16 32 64 128))))
                (set-cdr! (last-pair ls) ls)
                ls))

;; initialize the video mode
(define test-rect (SDL:make-rect 0 0 640 480))
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16)

(set! *random-state* (seed->random-state (current-time)))

(define (rand-rect font word)
  (let* ((dimensions (SDL:font:size-text font word))
         (w (assq-ref dimensions 'w))
         (h (assq-ref dimensions 'h)))
    (SDL:make-rect (random (- (SDL:rect:w test-rect) w))
                   (random (- (SDL:rect:h test-rect) h))
                   w h)))

(define rand-color
  (lambda ()
    (SDL:make-color (random #xff) (random #xff) (random #xff))))

;; clear the screen
(SDL:fill-rect (SDL:get-video-surface) test-rect #xffffff)
(SDL:flip)

;; write the text in random locations with random colors
(let ((src-rect (SDL:make-surface (SDL:rect:w test-rect)
                                  (SDL:rect:h test-rect)))
      (screen (SDL:get-video-surface)))
  (let loop ((i 420) (words sentence) (fonts fonts))
    (or (zero? i)
        (let* ((word (car words))
               (font (car fonts))
               (text (SDL:render-text font word (rand-color)
                                      (case (random 3)
                                        ((0) #t)
                                        ((1) #f)
                                        ((2) (rand-color)))))
               (dst-rect (rand-rect font word)))
          (SDL:blit-surface text test-rect screen dst-rect)
          (SDL:update-rect screen dst-rect)
          (loop (1- i) (cdr words) (cdr fonts))))))

;; clean up
(SDL:delay 1000)
(SDL:ttf-quit)
(SDL:quit)

;;; ttf.scm ends here
