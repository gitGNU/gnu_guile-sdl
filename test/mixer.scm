#! /usr/local/bin/guile -s
!#

;; simple mixer test
;; 
;; Created:    <2001-06-10 19:14:30 foof>
;; Time-stamp: <2001-06-10 20:23:08 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules ((sdl sdl)
              :rename (symbol-prefix-proc 'sdl-))
             (ice-9 format))

;; the directory to find the image in
(define datadir (if (getenv "srcdir")
                  (string-append (getenv "srcdir") "/test/")
                  "./"))

;; initialize the SDL mixer module
(sdl-init sdl-init/audio)

;; initialize the audio device
(sdl-open-audio)

;; display audio device info
(let ((specs (sdl-query-spec)))
  (display (format #f "Opened audio at ~A Hz ~A bit ~A\n"
                   (car specs) (logand (cadr specs) #xFF)
                   (if (> (caddr specs) 1) "stereo" "mono"))))


;; load a wav file
(define my-wav (sdl-load-wave (string-append datadir "test.wav")))

;; play the wav
(sdl-volume 128)
(sdl-play-channel my-wav)

;; loop until it's done
(while (sdl-playing? 0)
       (sdl-delay 100))

;; close the audio and quit SDL
(sdl-close-audio)
(sdl-quit-all)

