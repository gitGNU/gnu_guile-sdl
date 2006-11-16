;;; misc-utils.scm --- grab bag of utility procs

;;	Copyright (C) 2004,2005,2006 Thien-Thi Nguyen
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

;;; Commentary:

;;; Code:

(define-module (sdl misc-utils)
  #:use-module ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
  #:export (call-with-clip-rect
            rotate-square
            rectangle-closure
            rectangle<-geometry-string
            poll-with-push-on-timeout-proc
            copy-surface
            ignore-all-event-types-except
            fade-loop!))

;; Set default clip rect to @var{rect}, call @var{thunk}, and restore it.
;; @var{thunk} is a procedure that takes no arguments.
;;
(define (call-with-clip-rect rect thunk)
  (let* ((s (SDL:get-video-surface))
         (orig (SDL:get-clip-rect s)))
    (SDL:set-clip-rect! s rect)
    (thunk)
    (SDL:set-clip-rect! s orig)))

;; Return a new surface made by rotating @var{square} by @var{angle} degrees.
;; The square retains its original size.
;;
;;-sig: (square angle)
;;
(define (rotate-square src angle)
  (let* ((width (SDL:surface:w src))
         (height (SDL:surface:h src))
         (rotated (SDL:roto-zoom-surface src (- angle 90) 1.0 #t))
         (new-width (SDL:surface:w rotated))
         (new-height (SDL:surface:h rotated))
         (width-offset (quotient (- new-width width) 2))
         (height-offset (quotient (- new-height height) 2))
         (src-rect (SDL:make-rect width-offset height-offset width height))
         (dst (SDL:make-surface width height))
         (dst-rect (SDL:make-rect 0 0 width height)))
    (SDL:blit-surface rotated src-rect dst dst-rect)
    dst))

;; Return a closure that manages a single rectangle object.
;; Calling the closure with no args returns the rectangle object.
;; Otherwise, the messages @code{#:w}, @code{#:h}, @code{#:x}
;; and @code{#:y} return the rectangle's width, height, horizontal
;; offset and vertical offset, respectively;
;; and the messages @code{#:w!}, @code{#:h!}, @code{#:x!}
;; and @code{#:y!}, followed by an integer, update the rectangle's
;; width, height, horizontal offset and vertical offset, respectively.
;;
;; Optional arg @var{rect} specifies a rectangle object to manage
;; instead of allocating a new one.
;;
;;-sig: ([rect])
;;
(define (rectangle-closure . opt)
  (let ((rect (if (null? opt)
                  (SDL:make-rect 0 0 0 0)
                  (car opt))))
    (lambda args
      (if (null? args)
          rect
          (let ((head (car args))
                (tail (cdr args)))
            (cond ((null? tail) ((case head
                                   ((#:w) SDL:rect:w)
                                   ((#:h) SDL:rect:h)
                                   ((#:x) SDL:rect:x)
                                   ((#:y) SDL:rect:y))
                                 rect))
                  (else (set! tail (car tail))
                        ((case head
                           ((#:w!) SDL:rect:set-w!)
                           ((#:h!) SDL:rect:set-h!)
                           ((#:x!) SDL:rect:set-x!)
                           ((#:y!) SDL:rect:set-y!))
                         rect tail)
                        tail)))))))

;; Return a rectangle made from parsing the @dfn{geometry string} @var{spec},
;; which typically has the form @code{WxH+X+Y}, where @code{+X+Y} is optional
;; (defaults to ``+0+0''), and @code{W}, @code{H}, @code{X} and @code{Y} are
;; integers.  Actually, the @code{+} can also be a @code{-}.  If @var{spec}
;; cannot be parsed, return @code{#f}.  Examples:
;;
;; @example
;; (rectangle<-geometry-string "42x43+44+45")
;; @result{} #<SDL-Rect 42x43+44+45>
;;
;; (rectangle<-geometry-string "42x43-10-20")
;; @result{} #<SDL-Rect 42x43+-10+-20>
;;
;; (rectangle<-geometry-string "42x43")
;; @result{} #<SDL-Rect 42x43+0+0>
;;
;; (rectangle<-geometry-string "42")
;; @result{} #f
;; @end example
;;
;; Note that the print representation of a rectangle always has ``+''.  The
;; term ``geometry string'' derives from the X Window System, where many
;; programs take a @code{--geometry} (or @code{-g} for short) command-line
;; option.
;;
(define (rectangle<-geometry-string spec)
  (define (->n b e)
    (string->number (substring spec b e)))
  (define (plus-or-minus b)
    (let ((p (string-index spec #\+ b))
          (m (string-index spec #\- b)))
      (and (or p m) (min (or p 999999) (or m 999999)))))
  (let ((len (string-length spec))
        (x (string-index spec #\x))
        (p/m (plus-or-minus 0))
        (R (rectangle-closure)))
    (and x
         (not (= 0 x))
         (< 0 (R #:w! (->n 0 x)))
         (if p/m
             (and (< x p/m)
                  (< 0 (R #:h! (->n (1+ x) p/m)))
                  (let ((p2 (plus-or-minus (1+ p/m))))
                    (and p2
                         (not (= p/m p2))
                         (R #:x! (->n p/m p2))
                         (R #:y! (->n p2 len)))))
             (< 0 (R #:h! (->n (1+ x) len))))
         (R))))

;; Return a procedure @code{P} that checks the event queue for @var{timeout} ms,
;; polling every @var{slice} ms.  If an event arrives during that time, return
;; #t.  Otherwise return #f.  Optional arg @var{get-timeout-events} is either
;; a list of events to be pushed on the queue in the case of timeout, or a
;; thunk to be called that produces such a list.  If @var{get-timeout-events}
;; is specified, return the result of another event queue polling.  (This may
;; still be #f if the pushed events are masked in some way.)
;;
;; @code{P} is called with a single arg, a pre-constructed event object.  This
;; interface is congruent with that of @code{wait-event} and @code{poll-event}.
;; @xref{Events}.
;;
;;-sig: (timeout slice [get-timeout-events])
;;
(define (poll-with-push-on-timeout-proc timeout slice . get-timeout-events)
  (let* ((fresh (and (not (null? get-timeout-events))
                     (car get-timeout-events)))
         (ls-ev (cond ((procedure? fresh) fresh)
                      ((pair? fresh)      (lambda () fresh))
                      (else               #f)))
         (push! (if ls-ev
                    (lambda (ev)
                      (for-each SDL:push-event (ls-ev))
                      (SDL:poll-event ev))
                    (lambda (ev) #f))))
    ;; rv
    (lambda (ev)
      (let loop ((still timeout))
        (cond ((SDL:poll-event ev))
              ((= 0 still)          (push! ev))
              (else                 (SDL:delay slice)
                                    (loop (max 0 (- still slice)))))))))

;; Create a new surface and blit @var{surface} onto it.
;; The new surface has the same pixel format as @var{surface}.
;; Return the new surface.
;;
;; Optional second arg @var{clip} is a rectangle describing the
;; portion of @var{surface} to copy (default is the entire surface).
;;
;;-sig: (surface [clip])
;;
(define (copy-surface surface . clip)
  (define (conv new)
    (SDL:convert-surface
     new (SDL:surface-get-format surface)
     (map string->symbol (SDL:surface:flags surface))))
  (cond ((and (not (null? clip))
              (car clip))
         => (lambda (rect)
              (let* ((rw (SDL:rect:w rect))
                     (rh (SDL:rect:h rect))
                     (s (conv (SDL:make-surface rw rh))))
                (SDL:blit-surface surface rect s (SDL:make-rect 0 0 rw rh))
                s)))
        (else (conv surface))))

;; Arrange to ignore all event types except those in @var{ls} (zero or
;; more symbols from @code{event-types}).  As a special case, if @var{ls}
;; is #f, arrange to not ignore any event types (all are enabled).
;;
(define (ignore-all-event-types-except . ls)
  (let ((proc (if (and (not (null? ls)) (not (car ls)))
                  (lambda (type)
                    (SDL:event-state type 'SDL_ENABLE))
                  (lambda (type)
                    (or (memq type ls)
                        (SDL:event-state type 'SDL_IGNORE))))))
    (for-each proc (SDL:enumstash-enums SDL:event-types))))

;; Loop for @var{sec} seconds, iteratively blitting onto @var{realized} at
;; @var{location} (a rectangle or #f to indicate the origin) the composition
;; of @var{image} and its @var{replacement} (both surfaces), to effect a
;; @dfn{fade-in} of @var{replacement} over @var{image}.
;;
;; @var{realized} may be either a surface, in which case at the end of each
;; loop it is shown by calling @code{flip} on it; or a pair whose @sc{car} is
;; a surface and whose @sc{cdr} is a thunk that should do the showing.
;;
;; Note that @var{location} is used for blitting, so its width and height
;; should match those of @var{image} and @var{replacement}.
;;
(define (fade-loop! sec realized location image replacement)
  (let* ((base (if (pair? realized)
                   (car realized)
                   realized))
         (loc (or location (SDL:make-rect
                            0 0
                            (SDL:surface:w base)
                            (SDL:surface:h base))))
         (show (if (pair? realized)
                   (cdr realized)
                   (lambda () (SDL:update-rect base loc))))
         (one (if (eq? base image)
                  (copy-surface image)
                  image))
         (two (if (eq? base replacement)
                  (copy-surface replacement)
                  replacement))
         (msec (* 1000 sec))
         (now (SDL:get-ticks)))
    (let loop ((bef (SDL:get-ticks)) (alpha 0.0) (last? #f))
      (SDL:blit-surface one #f base loc)
      (SDL:set-alpha! two 'SDL_SRCALPHA (inexact->exact alpha))
      (SDL:blit-surface two #f base loc)
      (show)
      (let* ((now (SDL:get-ticks))
             (new-alpha (+ alpha (* 256 (/ (- now bef) msec)))))
        (if (< 255 new-alpha)
            (or last? (loop now 255 #t))
            (loop now new-alpha last?))))))

;;; misc-utils.scm ends here
