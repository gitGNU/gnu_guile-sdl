;;; ttf.scm --- simple true type font test

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
  (do ((i 0 (1+ i)))
      ((> i 50))
    (let* ((word (return-it (car sentence)
                   (set! sentence (cdr sentence))))
           (font (return-it (car fonts)
                   (set! fonts (cdr fonts))))
           (text (SDL:render-text font word (rand-color)
                                  (case (random 3)
                                    ((0) #t)
                                    ((1) #f)
                                    ((2) (rand-color)))))
           (dst-rect (rand-rect font word)))
      (SDL:blit-surface text test-rect screen dst-rect)
      (SDL:update-rect screen dst-rect))))

;; clean up
(SDL:delay 1000)
(SDL:ttf-quit)
(SDL:quit)

;;; ttf.scm ends here
