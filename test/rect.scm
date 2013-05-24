;;; rect.scm --- simple rectangle test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2011 Thien-Thi Nguyen
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

(use-modules ((sdl sdl) #:prefix SDL:))

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
(or (and (SDL:rect? test-rect)
         (zero? (SDL:rect:x test-rect))
         (zero? (SDL:rect:y test-rect)))
    (error "weird test-rect" test-rect))

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

;; set the icon
(let ((icon (SDL:make-surface 149 149))
      (at (SDL:make-rect 0 0 0 0)))
  (SDL:fill-rect icon #f #x424242)
  (do ((i 0 (1+ i)))
      ((= 42 i))
    (let ((x (random 149))
          (y (random 149)))
      (SDL:rect:set-x! at x)
      (SDL:rect:set-y! at y)
      (SDL:rect:set-w! at (random (- 149 x)))
      (SDL:rect:set-h! at (random (- 149 y))))
    (SDL:fill-rect icon at (random #xffffff)))
  (SDL:set-icon icon))

;; set the video mode to the dimensions of our rect
(define screen (SDL:set-video-mode (SDL:rect:w test-rect)
                                   (SDL:rect:h test-rect)
                                   8 '(SDL_HWSURFACE)))
(cond ((SDL:surface? screen))
      (else (fse "ERROR: Not a surface: ~S~%" screen)
            (exit #f)))

;; get some info on ‘screen’
(let ((depth (SDL:surface:depth screen))
      (flags (SDL:surface:flags screen)))
  (and verbose? (fso "INFO: ‘screen’ depth ~S flags ~S~%"
                     depth flags)))

;; futz w/ the window-manager
(let ((info (SDL:get-wm-info))
      (caption (SDL:get-caption)))
  (cond (verbose?
         (fso "INFO: get-wm-info => ~S~%" info)
         (fso "INFO: get-caption => ~S~%" caption)))
  (and *interactive* (SDL:delay 1000))
  (SDL:set-caption "and so it goes")
  (cond (verbose?
         (fso "INFO: get-wm-info => ~S~%" (SDL:get-wm-info))
         (fso "INFO: get-caption => ~S~%" (SDL:get-caption)))))

;; draw some rectangles filled with random colors
(do ((i 0 (1+ i)))
    ((= i 20))
  (let ((sample (rand-rect test-rect)))
    (SDL:fill-rect screen sample (random #xffffff))
    (SDL:update-rect screen sample)))

(SDL:delay (* 200 (if *interactive* 10 1)))

;; futz w/ the colormap
(let ((cm (list->vector
           (map (lambda (n)
                  ;; grayscale
                  (SDL:make-color n n n))
                (iota 256)))))
  (define (randomize-cm!)
    (do ((i 0 (1+ i)))
        ((= 256 i))
      (let ((elem (vector-ref cm i)))
        (SDL:color:set-r! elem (random 256))
        (SDL:color:set-g! elem (random 256))
        (SDL:color:set-b! elem (random 256)))))
  (define (jam!)
    (SDL:set-colors! screen cm)
    (SDL:delay 200))
  ;; do it
  (for-each (lambda (thunk)
              (thunk))
            (list jam!
                  randomize-cm!
                  jam!
                  randomize-cm!
                  jam!)))

;; futz w/ full screen, maybe
;; gate on interactive to allow manual intervention on weirdness
(and *interactive*
     (let ((res (SDL:toggle-full-screen)))
       (and verbose? (fso "INFO: toggle-full-screen => ~S~%" res))
       (SDL:delay 1000)
       (and res (SDL:toggle-full-screen))
       (SDL:delay 1000)))

;; more wm futzing -- do this after pausing to avoid
;;                    disconcerting flashing
(let ((rv (SDL:iconify-window)))
  (and verbose? (fso "INFO: iconify-window => ~S~%" rv))
  (SDL:delay (* 200 (if *interactive* 10 1))))

;; quit SDL
(exit (SDL:quit))

;;; rect.scm ends here
