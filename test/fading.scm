;;; fading.scm --- iterative alpha blending

(use-modules
 ((sdl misc-utils) #:select (copy-surface))
 ((sdl simple) #:select (simple-canvas))
 ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
 ((sdl gfx) #:renamer (symbol-prefix-proc 'GFX:)))

(define (as-four surface)
  (let* ((w (SDL:surface:w surface))
         (h (SDL:surface:h surface))
         (w/2 (quotient w 2))
         (h/2 (quotient h 2))
         (hoh (GFX:zoom-surface surface 1/2 1/2 #t)) ; half-of-half
         (result (SDL:display-format (SDL:make-surface w h)))
         (drect (SDL:make-rect 0 0 w/2 h/2)))
    (define (move/blit! munge value)
      (and munge (munge drect value))
      (SDL:blit-surface hoh #f result drect))
    (move/blit! #f #f)
    (move/blit! SDL:rect:set-x! w/2)
    (move/blit! SDL:rect:set-y! h/2)
    (move/blit! SDL:rect:set-x! 0)
    result))

(define (fade! base image sec replacement)
  (let* ((now (SDL:get-ticks))
         (w (SDL:surface:w base))
         (h (SDL:surface:h base))
         (one (if (eq? base image)
                  (copy-surface image)
                  image))
         (two (if (SDL:surface? replacement)
                  (if (eq? base replacement)
                      (copy-surface replacement)
                      replacement)
                  (let ((new (SDL:make-surface w h)))
                    (SDL:fill-rect
                     new
                     (SDL:make-rect 0 0 w h)
                     (SDL:map-rgb (SDL:surface-get-format base) 0 0 0))
                    new)))
         (msec (* 1000 sec))
         (out? (->bool replacement))
         (done? (if out?
                    (lambda (x) (< 255 x))
                    (lambda (x) (> 0 x))))
         (adjust (if out? + -)))
    (let loop ((bef (SDL:get-ticks)) (alpha (if out? 0.0 255.0)) (last? #f))
      (SDL:blit-surface one #f base)
      (SDL:set-alpha! two 'SDL_SRCALPHA (inexact->exact alpha))
      (SDL:blit-surface two #f base)
      (SDL:flip base)
      (let* ((now (SDL:get-ticks))
             (new-alpha (adjust alpha (* 256 (/ (- now bef) msec)))))
        (if (done? new-alpha)
            (or last? (loop now (if out? 255 0) #t))
            (loop now new-alpha last?))))))

;; do it!
(let* ((canvas (simple-canvas #t 200 153 24))
       (img1 (SDL:load-image (in-vicinity
                              (or (and=> (getenv "srcdir")
                                         (lambda (d)
                                           (in-vicinity d "test")))
                                  ".")
                              "gnu-goatee.jpg")))
       (img2 (as-four img1))
       (img3 (as-four img2)))
  (define (fade/wait! bef aft)
    (fade! (canvas) bef 2 aft)
    (SDL:delay 1234))
  (SDL:blit-surface img1)
  (SDL:flip)
  (fade/wait! img1 #t)                  ; out
  (fade/wait! img1 #f)                  ; in
  (fade/wait! img1 img2)                ; out w/ replacement (cross-fade)
  (fade/wait! img2 img3)                ; again!
  (fade/wait! img3 img2)                ; again!
  (fade/wait! img2 img1)                ; again!
  (SDL:quit))

;;; fading.scm ends here
