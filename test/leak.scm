;;; leak.scm --- check for memory leaks

(or *interactive* (exit-77 "interactive"))

(define debug? (getenv "DEBUG"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize SDL
(let ((res (SDL:init '(SDL_INIT_VIDEO))))
  (and debug? (simple-format #t "SDL:init: ~S\n" res)))

(define (malloced)
  (gc)
  (cdr (assq 'bytes-malloced (gc-stats))))

(define (stress title count thunk)
  (or debug? (set! count 10000))
  (and debug? (for-each display (list "stressing: " title
                                      " (" (malloced) ")\n")))
  (malloced)
  (do ((i 1 (1+ i)))
      ((> i count))
    (thunk)
    (and (= 0 (remainder i 10000)) debug?
         (write-line (list i (malloced))))))

(define stress-tests
  `(("surface" 100000 ,(lambda () (SDL:make-surface 123 79 '())))
    ("rect" 1000000 ,(lambda () (SDL:make-rect 0 0 123 79)))
    ("event" 1000000 ,SDL:make-event)
    ("keysym" 1000000 ,SDL:make-keysym)
    ("color" 1000000 ,(lambda () (SDL:make-color #xaa #x88 #x55)))
    ("joystick" 1000000 ,SDL:joystick-open)
    ("cd" 1000000 ,SDL:cd-open)
    ("cursor" 1000000 ,(let* ((data (make-vector 16 85))
                              (mask data)
                              (c (SDL:create-cursor data mask 8 16 0 0)))
                         (lambda () (SDL:get-cursor) (SDL:set-cursor c))))))

;; do it!
(for-each (lambda (args)
            (apply stress args))
          stress-tests)

;; quit SDL
(SDL:quit)

;;; leak.scm ends here
