;;; mixer.scm

;; Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009 Thien-Thi Nguyen
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
             ((sdl mixer) #:prefix SDL:))

;; initialize the SDL mixer module
(SDL:init '(SDL_INIT_AUDIO))

;; initialize the audio device
(SDL:open-audio)

;; display audio device info
(let ((specs (SDL:query-spec)))
  (cond (specs
         (fso "Opened audio at ~A Hz ~A bit ~A~%"
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
