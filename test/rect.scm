;;; rect.scm --- simple rectangle test

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2011, 2013 Thien-Thi Nguyen
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

(use-modules
 ((sdl sdl) #:prefix SDL:)
 ((sdl misc-utils) #:select (copy-rectangle
                             rect<-surface
                             rectangle-closure
                             rectangle<-geometry-string
                             call-with-clip-rect)))

;; initialize the SDL video module
(SDL:init 'video)

(let ((name (SDL:video-driver-name)))
  (info "video-driver-name => ~S" name))

(and (SDL:video-mode-ok -1 -1 -1)
     (error "impossible video-mode is \"OK\"!"))

(call-with-values SDL:video-cmf
  (lambda (capabilities memory format)
    (info "video-cmf => ~S / => ~S / => ~S"
          capabilities memory format)))
(let ((alist (SDL:get-video-info)))
  (for-each (lambda (k v)
              (info "~A => ~S" k v))
            (map car alist)
            (map cdr alist)))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (info "(SDL:list-modes) => ~A" modes)
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (SDL:make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))
(info "test-rect => ~A" test-rect)
(or (and (SDL:rect? test-rect)
         (zero? (SDL:rect:x test-rect))
         (zero? (SDL:rect:y test-rect)))
    (error "weird test-rect" test-rect))

(define (same-as-test-rect? rect)
  (and (SDL:rect? rect)
       (zero? (SDL:rect:x rect))
       (zero? (SDL:rect:y rect))
       (= (SDL:rect:w test-rect)
          (SDL:rect:w rect))
       (= (SDL:rect:h test-rect)
          (SDL:rect:h rect))))

;; copy
(let ((copy (copy-rectangle test-rect)))
  (or (same-as-test-rect? copy)
      (error "weird copy" copy)))

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
                                   8 '(hw-surface)))
(cond ((SDL:surface? screen))
      (else (fse "ERROR: Not a surface: ~S~%" screen)
            (exit #f)))

(define fmt (SDL:surface-get-format screen))

;; get some info on ‘screen’
(let ((depth (SDL:surface:depth screen))
      (flags (SDL:surface:flags screen)))
  (info "‘screen’ depth ~S flags ~S" depth flags))

;; from surface
(let ((again (rect<-surface screen)))
  (or (same-as-test-rect? again)
      (error "weird ‘rect<-surface’ rv" again)))

;; closure
(let ((rect (rectangle-closure test-rect)))
  (or (eq? test-rect (rect))
      (error "closure too closed!" (rect)))
  (let ((components (map rect '(#:x #:y #:w #:h))))
    (or (same-as-test-rect? (apply SDL:make-rect components))
        (error "closure member mismatch" components))))

;; from geom
(let* ((s (fs "~Ax~A+~A+~A"
              (SDL:rect:w test-rect)
              (SDL:rect:h test-rect)
              (SDL:rect:x test-rect)
              (SDL:rect:y test-rect)))
       (yar (rectangle<-geometry-string s)))
  (info "~S => ~S" s yar)
  (or (same-as-test-rect? yar)
      (error "weird" yar)))

;; futz w/ the window-manager
(let ((wminfo (SDL:get-wm-info))
      (caption (SDL:get-caption)))
  (info "get-wm-info => ~S" wminfo)
  (info "get-caption => ~S" caption)
  (and *interactive* (SDL:delay 1000))
  (SDL:set-caption "and so it goes")
  (info "get-wm-info => ~S" (SDL:get-wm-info))
  (set! caption (SDL:get-caption))
  (call-with-values SDL:caption-ti
    (lambda (title icon)
      (or (equal? (list title icon) (map cdr caption))
          (error "discrepency in caption-ti:"
                 (list title icon) 'vs caption))))
  (info "get-caption => ~S" caption))

;; draw some rectangles filled with random colors
(do ((i 0 (1+ i)))
    ((= i 20))
  (let ((sample (rand-rect test-rect)))

    (define (w/random-args make unpack vunpack n)
      (let* ((full (apply make fmt (map (lambda (x)
                                          (random #x100))
                                        (iota n))))
             (vpart (call-with-values (lambda () (vunpack full fmt))
                      list))
             (part (unpack full fmt)))
        (or (and (pair? part)
                 (= n (length part)))
            (error (fs "bad ~A result: ~S"
                       (procedure-name unpack)
                       part)))
        (info "color ~S ~S ~S" full vpart part)
        (or (equal? vpart (map cdr part))
            (error "discrepency between part and vpart!"))
        full))

    (call-with-clip-rect
     (rand-rect test-rect)
     (lambda ()
       (SDL:fill-rect screen #f (random #x1000000))
       (SDL:flip)))
    (SDL:fill-rect screen sample (case (modulo i 3)
                                   ((0) (random #x100))
                                   ((1) (w/random-args SDL:map-rgb
                                                       SDL:get-rgb
                                                       SDL:pixel-rgb
                                                       3))
                                   ((2) (w/random-args SDL:map-rgba
                                                       SDL:get-rgba
                                                       SDL:pixel-rgba
                                                       4))))
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
    (define (pal! flags)
      (SDL:set-palette screen flags cm (random 256))
      (SDL:delay 30))
    (pal! 'physical)
    (randomize-cm!)
    (pal! 'logical)
    (randomize-cm!)
    (pal! '(physical logical))
    (randomize-cm!)
    (SDL:set-colors! screen cm (random 256))
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
       (info "toggle-full-screen => ~S" res)
       (SDL:delay 1000)
       (and res (SDL:toggle-full-screen))
       (SDL:delay 1000)))

;; more wm futzing -- do this after pausing to avoid
;;                    disconcerting flashing
(let ((rv (SDL:iconify-window)))
  (info "iconify-window => ~S" rv)
  (SDL:delay (* 200 (if *interactive* 10 1))))

;; quit SDL
(exit (SDL:quit))

;;; rect.scm ends here
