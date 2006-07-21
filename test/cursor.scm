;;; cursor.scm --- simple cursor test

(or *interactive* (exit-77 "interactive"))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use 2x2 of gnu-goatee.jpg (200x153)
           (SDL:make-rect 0 0 400 306))
          (else
           ;; a list - choose the first mode
           (car modes)))))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16
                    '(SDL_HWSURFACE))
(let* ((datadir (or (and=> (getenv "srcdir")
                           (lambda (d) (in-vicinity d "test")))
                    "."))
       (gnu-head (SDL:load-image (in-vicinity datadir "gnu-goatee.jpg")))
       (rect (SDL:make-rect 0 0 200 153)))
  (do ((x 0 (1+ x)))
      ((= 2 x))
    (do ((y 0 (1+ y)))
        ((= 2 y))
      (SDL:rect:set-x! rect (* 200 x))
      (SDL:rect:set-y! rect (* 153 y))
      (or (= 0 (logxor x y))
          (SDL:blit-surface gnu-head #f (SDL:get-video-surface) rect)))))
(SDL:flip)

;; move the mouse (rude, but we can live w/ that)
(SDL:warp-mouse (ash (SDL:surface:w (SDL:get-video-surface)) -1)
                (ash (SDL:surface:h (SDL:get-video-surface)) -1))

(SDL:delay 1000)

;; primitive cursor creation (need a higher level one)
(define (random-16)
  (list->vector (map (lambda ignored
                       (random 256))
                     (iota 16))))
(do ((i 0 (1+ i)))
    ((= 50 i))
  (SDL:set-cursor (SDL:create-cursor (random-16) (random-16) 8 16 0 0))
  (SDL:delay 200))

(let* ((data #(85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85))
       (mask data)
       (cursor (SDL:create-cursor data mask 8 16 0 0)))
  (SDL:set-cursor cursor))

(SDL:delay 1000)

;; quit SDL
(SDL:quit)

;;; cursor.scm ends here
