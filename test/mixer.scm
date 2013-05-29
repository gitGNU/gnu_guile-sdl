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
(or *interactive* (exit-77 "interactive"))

;; simple mixer test

(use-modules ((sdl sdl) #:prefix SDL:)
             ((sdl mixer) #:prefix MIXER:))

;; initialize the SDL mixer module
(SDL:init '(SDL_INIT_AUDIO))

;; initialize the audio device
(MIXER:open-audio)

;; display audio device info
(let ((specs (MIXER:query-spec)))
  (cond (specs
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


;; load the files
(define background (MIXER:load-music (datafile "background.ogg")))
(define fx (MIXER:load-wave (datafile "fx.ogg")))

;; play background
(MIXER:volume 128)
(MIXER:play-music background)

(define angle -110)

;; loop until it's done, playing a sound effect every so often
(while (MIXER:playing-music?)
       (let ((ch (MIXER:play-channel fx)))
         (MIXER:set-position ch angle 0)
         (or (MIXER:playing? ch)
             (error (fs "(playing? ~S) => #f !!!" ch))))
       (set! angle (+ 20 angle))
       (SDL:delay 500))

;; close the audio and quit SDL
(MIXER:close-audio)
(exit (SDL:quit))

;;; mixer.scm ends here
