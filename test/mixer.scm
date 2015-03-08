;;; mixer.scm

;; Copyright (C) 2003-2005, 2007-2009, 2011, 2013, 2015 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(or *have-mixer* (exit-77 "mixer disabled"))

;; simple mixer test

(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl mixer) #:prefix MIXER:))

;; initialize the SDL mixer module
(SDL:init 'audio)

;; initialize the audio device
(MIXER:open-audio)

;; display audio device info
(call-with-values MIXER:device-ffc
  (lambda (freq format channels)
    (info "Opened audio at ~A Hz ~A bit ~A"
          freq
          (logand format #xFF)
          (case channels
            ((1) "mono")
            ((2) "stereo")
            (else (fs "~A channels" channels))))))

(define (load-music)
  (MIXER:load-music (datafile "background.ogg")))

;; load the files
(define background (load-music))
(define fx (MIXER:load-wave (datafile "fx.ogg")))

(define (set-music-command! command)
  (let ((res (MIXER:set-music-command command)))
    (info "(set-music-command ~S) => ~S" command res)
    (or (zero? res)
        (error "set-music-command failed"))))

(set-music-command! #f)
(MIXER:music-volume 42)

;; The test doesn't actually require user interaction.  However,
;; it makes noise, which might require user to decrease volume.
(or *interactive* (exit-77 "interactive"))

;; play background
(MIXER:play-music background)

;; loop until it's done, playing a sound effect every so often
(MIXER:volume 24)
(let loop ((angle -110))
  (and (MIXER:playing-music?)
       (or (not (MIXER:paused-music?))
           (error "(paused-music?) => #t !!!"))
       (let ((ch (MIXER:play-channel fx)))
         (or (not (MIXER:paused? ch))
             (error (fs "(paused? ~S) => #t !!!" ch)))
         (MIXER:set-position ch angle 0)
         (or (MIXER:playing? ch)
             (error (fs "(playing? ~S) => #f !!!" ch)))
         (or (not (= 10 angle))
             (let ((ogg-player (getenv "OGGPLAYER")))
               (or (not ogg-player)
                   (string-null? ogg-player)
                   (and (set-music-command! ogg-player)
                        (or (zero? (MIXER:play-music (load-music)))
                            (error (fs "play-music via ‘~A’ failed"
                                       ogg-player)))))))
         (SDL:delay 500)
         (loop (+ 20 angle)))))

;; close the audio and quit SDL
(MIXER:close-audio)
(exit (SDL:quit))

;;; mixer.scm ends here
