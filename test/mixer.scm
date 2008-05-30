;;; mixer.scm

(or *have-mixer* (exit-77 "mixer disabled"))
(or *interactive* (exit-77 "interactive"))

;; simple mixer test

(use-modules ((sdl sdl) #:renamer (symbol-prefix-proc 'SDL:))
             ((sdl mixer) #:renamer (symbol-prefix-proc 'SDL:)))

;; initialize the SDL mixer module
(SDL:init '(SDL_INIT_AUDIO))

;; initialize the audio device
(SDL:open-audio)

;; display audio device info
(let ((specs (SDL:query-spec)))
  (cond (specs
         (fso "Opened audio at ~A Hz ~A bit ~A\n"
              (assq-ref specs 'freq)
              (logand (assq-ref specs 'format) #xFF)
              (if (> (assq-ref specs 'channels) 1)
                  "stereo" "mono")))
        (else
         (SDL:quit)
         (exit-77 "no mixer specs available"))))


;; load the files
(define background (SDL:load-music (datafile "background.ogg")))
(define fx (SDL:load-wave (datafile "fx.ogg")))

;; play background
(SDL:volume 128)
(SDL:play-music background)

(define angle 90)

;; loop until it's done, playing a sound effect every 1500ms
(while (SDL:playing-music?)
       (let ((ch (SDL:play-channel fx)))
         (SDL:set-position ch angle 0))
       (set! angle (- angle))
       (SDL:delay 1500))

;; close the audio and quit SDL
(SDL:close-audio)
(SDL:quit)

;;; mixer.scm ends here
