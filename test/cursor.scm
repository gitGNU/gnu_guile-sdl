;;; cursor.scm --- simple cursor test

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL video module
(SDL:init '(SDL_INIT_VIDEO))

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (SDL:list-modes)))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (SDL:make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))

;; set the video mode to the dimensions of our rect
(SDL:set-video-mode (SDL:rect:w test-rect) (SDL:rect:h test-rect) 16
                    '(SDL_HWSURFACE))
(SDL:fill-rect (SDL:get-video-surface) test-rect #xffffff)
(SDL:flip)

;; primitive cursor creation (need a higher level one)
(let* ((data #(85 85 85 85 85 85 85 85 85 85 85 85 85 85 85 85))
       (mask data)
       (cursor (SDL:create-cursor data mask 8 16 0 0)))
  (SDL:set-cursor cursor))

(SDL:delay 1000)

;; quit SDL
(SDL:quit)

;;; cursor.scm ends here
