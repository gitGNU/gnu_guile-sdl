#!/bin/sh
test x"$HAVE_MIXER" = x && { echo "INFO: $0: MIXER DISABLED" ; exit 77 ; }
exec ${GUILE-guile} -s $0 "$@" # -*-scheme-*-
!#

;; simple mixer test

(use-modules (sdl sdl))                 ; fixme: these must be separate due
(use-modules (sdl mixer))               ;        to compiled modules weirdness

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; initialize the SDL mixer module
(sdl-init '(SDL_INIT_AUDIO))

;; initialize the audio device
(sdl-open-audio)

;; display audio device info
(let ((specs (sdl-query-spec)))
  (cond (specs
         (display (format #f "Opened audio at ~A Hz ~A bit ~A\n"
                          (cdr (assq 'freq specs))
                          (logand (cdr (assq 'format specs)) #xFF)
                          (if (> (cdr (assq 'channels specs)) 1)
                            "stereo" "mono"))))
        (else
         (sdl-quit)
         (format #t "INFO: ~A: NO MIXER SPECS AVAILABLE\n"
                 (car (command-line)))
         (exit 77))))


;; load a wav file
(define background (sdl-load-music (string-append datadir "test.wav")))
(define fx (sdl-load-wave (string-append datadir "noise.wav")))

;; play the wav
(sdl-volume 128)
(sdl-play-music background)

;; loop until it's done, playing a sound effect every 500ms
(while (sdl-playing-music?)
       (sdl-play-channel fx)
       (sdl-delay 500))

;; close the audio and quit SDL
(sdl-close-audio)
(sdl-quit)

;;; mixer.scm ends here
