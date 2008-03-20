;;; leak.scm --- check for memory leaks

(define debug? (getenv "DEBUG"))
(or debug? (exit-77 "debug only"))
(and debug? (debug-enable 'debug 'backtrace))

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl ttf) #:renamer (symbol-prefix-proc 'SDL:)))

(define exit-value #t)

;; initialize SDL
(let ((res (SDL:init '(SDL_INIT_VIDEO))))
  (and debug? (fso "SDL:init: ~S\n" res)))

(define lots (make-vector 1000 #f))

(define (malloced)
  ;; prudence or superstition?  you be the judge!
  (gc) (gc)
  (assq-ref (gc-stats) 'bytes-malloced))

(define (check-alloc/dealloc title thunk)
  (define (jam! x)
    (do ((i 0 (1+ i)))
        ((= i 1000))
      (vector-set! lots i (and x (x)))))
  (let* ((start #f) (fully #f) (final #f))
    (jam! #f)
    (set! start (malloced))
    (jam! thunk)
    (set! fully (malloced))
    (jam! #f)
    (set! final (malloced))
    (fso "~A:~A\t~A\t+~A\t~A~A\n"
         title (make-string (- 12 (string-length title)) #\space)
         start (number->string (- fully start) 16)
         final (if (= start final)
                   ""
                   "\tDIFFERENT!"))
    (set! exit-value (and exit-value (= start final)))
    (malloced)))

(define alloc/dealloc-tests
  `(("surface" ,(lambda () (SDL:make-surface 123 79 '())))
    ("rectangle" ,(lambda () (SDL:make-rect 0 0 123 79)))
    ("event" ,SDL:make-event)
    ("keysym" ,SDL:make-keysym)
    ("color" ,(lambda () (SDL:make-color #xaa #x88 #x55)))
    ("joystick" ,SDL:joystick-open)
    ("cd" ,SDL:cd-open)
    ("cursor" ,(let* ((data (make-vector 16 85))
                      (mask data))
                 (lambda ()
                   (SDL:get-cursor)
                   (SDL:create-cursor data mask 8 16 0 0))))))

;; do it!
(for-each (lambda (args)
            (apply check-alloc/dealloc args))
          alloc/dealloc-tests)

;; quit SDL
(SDL:quit)

(exit exit-value)

;;; leak.scm ends here
