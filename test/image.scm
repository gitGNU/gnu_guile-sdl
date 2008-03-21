;;; image.scm --- simple image test

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; the size of our test image
(define gnu-rect (SDL:make-rect 0 0 200 153))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; set the video mode to the dimensions of our image
(SDL:set-video-mode 200 153 16)

;; load and blit the image
(let ((gnu-head (SDL:load-image (datafile "gnu-goatee.jpg"))))
  (SDL:blit-surface gnu-head gnu-rect (SDL:get-video-surface) gnu-rect))

;; flip the double buffer
(SDL:flip (SDL:get-video-surface))

;; wait a half-second, then flip it upside-down
(SDL:delay 500)
(let ((upside-down (SDL:vertical-flip-surface (SDL:get-video-surface))))
  (SDL:blit-surface upside-down gnu-rect (SDL:get-video-surface) gnu-rect))
(SDL:flip (SDL:get-video-surface))

;; now flip horizontally
(SDL:delay 500)
(let ((left-right (SDL:horizontal-flip-surface (SDL:get-video-surface))))
  (SDL:blit-surface left-right gnu-rect (SDL:get-video-surface) gnu-rect))
(SDL:flip (SDL:get-video-surface))

;; ... and finally flip back
(SDL:delay 500)
(let ((orig (SDL:vh-flip-surface (SDL:get-video-surface))))
  (SDL:blit-surface orig gnu-rect (SDL:get-video-surface) gnu-rect))
(SDL:flip (SDL:get-video-surface))

;; wait then quit
(SDL:delay 500)
(SDL:quit)

;;; image.scm ends here
