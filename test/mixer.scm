;;; mixer.scm

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009,
;;   2011, 2013 Thien-Thi Nguyen
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
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA  02110-1301  USA

(or *have-mixer* (exit-77 "mixer disabled"))

;; simple mixer test

(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl mixer) #:prefix MIXER:))

;; initialize the SDL mixer module
(SDL:init 'audio)

;; initialize the audio device
(MIXER:open-audio)

;; display audio device info
(let ((specs (MIXER:query-spec)))
  (cond (specs
           (call-with-values MIXER:device-ffc
             (lambda (freq format channels)
               (info "device-ffc => ~S / => ~S (~S) / => ~S"
                     freq format (logand format #xFF) channels)
               (or (equal? freq (assq-ref specs 'freq))
                   (error "discrepency in freq:" freq 'vs spec))
               (or (equal? format (assq-ref specs 'format))
                   (error "discrepency in format:" format 'vs spec))
               (or (equal? channels (assq-ref specs 'channels))
                   (error "discrepency in channels:" channels 'vs spec))))
         (info "Opened audio at ~A Hz ~A bit ~A"
               (assq-ref specs 'freq)
               (logand (assq-ref specs 'format) #xFF)
               (case (assq-ref specs 'channels)
                 ((#f) "(no channel info!)")
                 ((1) "mono")
                 ((2) "stereo")
                 (else (fs "~A channels" (assq-ref specs 'channels))))))
        (else
         (SDL:quit)
         (exit-77 "no mixer specs available"))))

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
