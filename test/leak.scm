;;; leak.scm --- check for memory leaks

;; Copyright (C) 2005, 2008, 2011, 2013 Thien-Thi Nguyen
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

(cond-expand (guile-2 (exit-77 "Guile 2 does not track freed memory"))
             (else #f))

(use-modules
 ((srfi srfi-4) #:select (make-u8vector)))
(use-modules ((sdl sdl) #:prefix SDL:))

(define exit-value #t)

;; initialize SDL
(info "init => ~S" (SDL:init 'video))

(define LOTS (cond ((getenv "LEAK_LOTS") => string->number)
                   (else #x1000)))

(define lots (make-vector LOTS #f))

(define (malloced)
  ;; prudence or superstition?  you be the judge!
  (gc) (gc)
  (assq-ref (gc-stats) 'bytes-malloced))

(define (check-alloc/dealloc title thunk)
  (define (jam! x)
    (do ((i 0 (1+ i)))
        ((= i LOTS))
      (vector-set! lots i (and x (x)))))
  (let* ((start #f) (fully #f) (final #f))
    (jam! #f)
    (set! start (malloced))
    (jam! thunk)
    (set! fully (malloced))
    (jam! #f)
    (set! final (malloced))
    (info "~A:~A\t~A\t+~A\t~A~A"
          title (make-string (- 12 (string-length title)) #\space)
          start (hex (- fully start))
          final (if (= start final)
                    ""
                    "\tDIFFERENT!"))
    (set! exit-value (and exit-value (= start final)))
    (malloced)))

(define alloc/dealloc-tests
  `(("surface" ,(lambda () (SDL:make-surface 123 79 '())))
    ("rectangle" ,(lambda () (SDL:make-rect 0 0 123 79)))
    ("event" ,SDL:make-event)
    ("color" ,(lambda () (SDL:make-color #xaa #x88 #x55)))
    ("joystick" ,SDL:joystick-open)
    ("cd" ,SDL:cd-open)
    ("cursor" ,(let* ((data (make-u8vector 16 85))
                      (mask data))
                 (lambda ()
                   (SDL:get-cursor)
                   (SDL:create-cursor data mask 8 16 0 0))))))

(and *have-ttf*
     (begin
       (use-modules ((sdl ttf) #:prefix TTF:))
       (TTF:ttf-init)
       (append! alloc/dealloc-tests
                `(("ttf" ,(let ((f (datafile "crystal.ttf")))
                            (lambda () (TTF:load-font f 24))))))))

(and *have-mixer*
     (begin
       (use-modules ((sdl mixer) #:prefix MIXER:))
       (MIXER:open-audio)
       (append! alloc/dealloc-tests
                `(("music" ,(let ((f (datafile "fx.ogg")))
                              (lambda () (MIXER:load-music f))))
                  ("wave" ,(let ((f (datafile "fx.wav")))
                             (lambda () (MIXER:load-wave f))))))))

;; do it!
(for-each (lambda (args)
            (apply check-alloc/dealloc args))
          alloc/dealloc-tests)

;; quit SDL
(SDL:quit)

(exit exit-value)

;;; leak.scm ends here
