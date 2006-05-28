;;; ttf.scm --- simple true type font test

(or *have-ttf* (exit-77 "ttf disabled"))

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL video
(SDL:init '(SDL_INIT_VIDEO))

;; initialize the font lib
(or (= 0 (SDL:ttf-init)) (error "could not init font lib"))

;; the directory to find the image in
(define (datafile name)
  (in-vicinity (or (and=> (getenv "srcdir")
                          (lambda (d) (in-vicinity d "test")))
                   ".")
               name))

;; the text to display
(define sentence "The quick brown fox jumped over the lazy sleeping dog.")

;; load a font file
(define font (SDL:load-font (datafile "crystal.ttf") 16))

;; initialize the video mode
(define test-rect (SDL:make-rect 0 0 640 480))
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16)

(set! *random-state* (seed->random-state (current-time)))

(define rand-rect
  (let* ((dimensions (SDL:font:size-text font sentence))
         (w (assq-ref dimensions 'w))
         (h (assq-ref dimensions 'h)))
    (lambda ()
      (SDL:make-rect (random (SDL:rect:w test-rect))
                     (random (SDL:rect:h test-rect))
                     w h))))

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
  (do ((i 0 (1+ i)))
      ((> i 50))
    (let ((text (SDL:render-text font sentence (rand-color) #t))
          (dst-rect (rand-rect)))
      (SDL:blit-surface text test-rect screen dst-rect)
      (SDL:update-rect screen dst-rect))))

;; clean up
(SDL:delay 1000)
(SDL:ttf-quit)
(SDL:quit)

;;; ttf.scm ends here
