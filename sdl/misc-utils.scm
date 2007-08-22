;;; misc-utils.scm --- grab bag of utility procs

;;	Copyright (C) 2004,2005,2006,2007 Thien-Thi Nguyen
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
            rect<-surface
            copy-rectangle
            copy-surface
            ignore-all-event-types-except
            fade-loop!
            toroidal-panner))

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

;; Return a new rectangle with the same width and height as @var{surface}.
;; Optional second and third arg (which must appear together or not at all)
;; specifies the @var{x} and @var{y} components, respectively, to use instead
;; of the default of 0 (zero).
;;
;;-sig: (surface [x y])
;;
(define (rect<-surface surface . xy)
  (let ((x (if (null? xy) 0 (car  xy)))
        (y (if (null? xy) 0 (cadr xy)))
        (w (SDL:surface:w surface))
        (h (SDL:surface:h surface)))
    (SDL:make-rect x y  w h)))

;; Return a new rectangle copied from @var{rect}.
;;
;; Optional second arg @var{modify} specifies which portions,
;; if any, to modify using the values in the rest @var{args}.
;; If @var{modify} is @code{#:xy}, the two @var{args} specify
;; new @code{x} and @code{y} values.  If @var{modify} is
;; @code{#:wh}, the two @var{args} specify new @code{w} and
;; @code{h} values.
;;
;; @example
;; rect
;; @result{} #<SDL-Rect 3x4+1+2>
;;
;; (copy-rectangle rect)
;; @result{} #<SDL-Rect 3x4+1+2>
;;
;; (copy-rectangle rect #:xy 11 22)
;; @result{} #<SDL-Rect 3x4+11+22>
;;
;; (copy-rectangle rect #:wh 33 44)
;; @result{} #<SDL-Rect 33x44+1+2>
;; @end example
;;
;;-sig: (rect [modify args...])
;;
(define (copy-rectangle rect . opt)
  (let* ((modify (and (not (null? opt)) (car opt)))
         (args (and modify (cdr opt)))
         (x (if (eq? #:xy modify) (car  args) (SDL:rect:x rect)))
         (y (if (eq? #:xy modify) (cadr args) (SDL:rect:y rect)))
         (w (if (eq? #:wh modify) (car  args) (SDL:rect:w rect)))
         (h (if (eq? #:wh modify) (cadr args) (SDL:rect:h rect))))
    (SDL:make-rect x y w h)))

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
;; loop it is shown via @code{update-rect}; or a pair whose @sc{car} is
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

;; Return a procedure that can toroidally pan @var{surface}
;; by @var{dx} and @var{dy} pixels.  This means that data
;; disappearing from one side of the surface (left, right, top, bottom)
;; is rotated to appear at the other side (right, left, bottom, top).
;; The procedure takes one arg @var{count}, the number of pans to do.
;;
;; Positive @var{dx} moves surface data to the left (panning right),
;; and likewise, positive @var{dy}, up (panning down).
;;
;; Optional third arg @var{sub} is a rectangle object specifying a subset
;; of the surface.  The default is to pan the entire surface.
;;
;; Optional fourth arg @var{batch?} non-@code{#f} means to call
;; @code{update-rect} on the (sub)surface after all the panning is done.
;; The default is to update the surface after each pan.  Batch mode is
;; useful for implementing variable-speed panning, for example:
;;
;; @example
;; (define pan-away (toroidal-panner screen  21  12 #f #t))
;; (define pan-back (toroidal-panner screen -21 -12 #f #t))
;; (define ramp (map 1+ (append (make-list 21 0)
;;                              (identity (iota 12))
;;                              (reverse! (iota 12))
;;                              (make-list 21 0))))
;; (for-each pan-away ramp)
;; (for-each pan-back ramp)
;; @end example
;;
;;-sig: (surface dx dy [[sub] batch?])
;;
(define (toroidal-panner surface dx dy . opts)
  (define (r/new-xy r new-x new-y)
    (copy-rectangle r #:xy new-x new-y))

  (let* ((bb (if (or (null? opts) (not (car opts)))
                 (rect<-surface surface)
                 (car opts)))
         (w (SDL:rect:w bb))
         (h (SDL:rect:h bb))
         (x (SDL:rect:x bb))
         (y (SDL:rect:y bb))
         (dx (- dx))                    ; "pan right" on positive dx
         (dy (- dy))                    ; "pan down" on positive dy
         (L? (> 0 dx))
         (U? (> 0 dy))
         (LR (SDL:make-surface w (abs dy)))
         (LR-all (rect<-surface LR))
         (rd-LR (rect<-surface LR x (+ y (if U? 0 (- h dy)))))
         (wr-LR (rect<-surface LR x (+ y (if U? (+ h dy) 0))))
         (sh-rd-LR (SDL:make-rect x (+ y (if U? (- dy) 0))
                                  w (- h (abs dy))))
         (sh-wr-LR (r/new-xy sh-rd-LR x (+ (SDL:rect:y sh-rd-LR) dy)))
         (UD (SDL:make-surface (abs dx) h))
         (UD-all (rect<-surface UD))
         (rd-UD (rect<-surface UD (+ x (if L? 0 (- w dx))) y))
         (wr-UD (rect<-surface UD (+ x (if L? (+ w dx) 0)) y))
         (sh-rd-UD (SDL:make-rect (+ x (if L? (- dx) 0)) y
                                  (- w (abs dx)) h))
         (sh-wr-UD (r/new-xy sh-rd-UD (+ (SDL:rect:x sh-rd-UD) dx) y)))

    (define (do-blits!)
      (SDL:blit-surface surface rd-LR LR LR-all)
      (SDL:blit-surface surface sh-rd-LR surface sh-wr-LR)
      (SDL:blit-surface LR #f surface wr-LR)
      (SDL:blit-surface surface rd-UD UD UD-all)
      (SDL:blit-surface surface sh-rd-UD surface sh-wr-UD)
      (SDL:blit-surface UD #f surface wr-UD))

    (define (do-nothing!)
      #f)

    (define (do-update!)
      (SDL:update-rect surface bb))

    (let* ((batch? (and (not (null? opts))
                       (not (null? (cdr opts)))
                       (cadr opts)))
           (middle! (if batch? do-nothing! do-update!))
           (finish! (if batch? do-update! do-nothing!)))
      ;; rv
      (lambda (count)
        (do ((i 0 (1+ i)))
            ((= i count))
          (do-blits!)
          (middle!))
        (finish!)))))

;;; misc-utils.scm ends here
