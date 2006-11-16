;;; fading.scm --- iterative alpha blending

(use-modules
 ((sdl misc-utils) #:select (copy-surface fade-loop!))
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

;; do it!
(let* ((canvas (simple-canvas #t 200 153 24))
       (img1 (SDL:load-image (in-vicinity
                              (or (and=> (getenv "srcdir")
                                         (lambda (d)
                                           (in-vicinity d "test")))
                                  ".")
                              "gnu-goatee.jpg")))
       (img2 (as-four img1))
       (img3 (as-four img2))
       (void (SDL:make-surface 200 153)))
  (define (fade/wait! bef aft)
    (fade-loop! 2 (canvas) #f bef aft)
    (SDL:delay 1234))
  (SDL:fill-rect void #f
                 (SDL:map-rgb (SDL:surface-get-format (canvas)) 0 0 0))
  (SDL:blit-surface img1)
  (SDL:flip)
  (fade/wait! img1 void)                ; out
  (fade/wait! void img1)                ; in
  (fade/wait! img1 img2)                ; out w/ replacement (cross-fade)
  (fade/wait! img2 img3)                ; again!
  (fade/wait! img3 img2)                ; again!
  (fade/wait! img2 img1)                ; again!
  (SDL:quit))

;;; fading.scm ends here
