;;; leak.scm --- check for memory leaks

(or *interactive* (exit-77 "interactive"))

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL
(let ((res (SDL:init '(SDL_INIT_VIDEO))))
  (and debug? (simple-format #t "SDL:init: ~S\n" res)))

(define (stress count thunk)
  (or debug? (set! count 10000))
  (do ((i 1 (1+ i)))
      ((> i count))
    (thunk)
    (and (= 0 (remainder i 10000)) debug? (write-line i))))

(stress 100000 (lambda () (SDL:make-surface 123 79 '())))

(stress 1000000 (lambda () (SDL:make-rect 0 0 123 79)))

(stress 1000000 SDL:make-event)

(stress 1000000 SDL:make-keysym)

(stress 1000000 (lambda () (SDL:make-color #xaa #x88 #x55)))

(stress 1000000 SDL:joystick-open)

(stress 1000000 SDL:cd-open)

(let* ((data #(85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85))
       (mask data)
       (cursor (SDL:create-cursor data mask 8 16 0 0)))
  (stress 1000000 (lambda () (SDL:get-cursor) (SDL:set-cursor cursor))))

;; quit SDL
(SDL:quit)

;;; leak.scm ends here
