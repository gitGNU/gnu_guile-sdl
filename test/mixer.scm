#! /usr/local/bin/guile -s
!#

;; simple mixer test
;; 
;; Created:    <2001-06-10 19:14:30 foof>
;; Time-stamp: <2001-06-18 01:01:07 foof>
;; Author:     Alex Shinn <foof@debian.org>

(use-modules (sdl sdl)
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

