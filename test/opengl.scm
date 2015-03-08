;;; opengl.scm --- simple opengl test

;; Copyright (C) 2003, 2013 Thien-Thi Nguyen
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

(use-modules (sdl sdl)
             (opengl opengl))

;; initialize the SDL video module
(sdl-init 'video)

;; get a sample rect size from a list of available modes
(define test-rect
  (let ((modes (sdl-list-modes)))
    (cond ((eq? modes #f)
           (error "no supported video modes"))
          ((eq? modes #t)
           ;; any mode, use arbitrary 800x600
           (sdl-make-rect 0 0 800 600))
          (else
           ;; a list - choose the first mode
           (car modes)))))

;; set the video mode with opengl
(sdl-set-video-mode (sdl-rect:w test-rect) (sdl-rect:h test-rect) 16
                    '(SDL_VIDEO_HWSURFACE SDL_VIDEO_OPENGL))

;; draw some lines
(gl-begin)
(gl-vertex2d 0 0)
(gl-vertex2d 0 100)
(gl-vertex2d 100 100)
(gl-vertex2d 100 0)
(gl-end)

;; swap the double buffer
(sdl-gl-swap-buffers)

;; wait then quit
(sdl-delay 3000)
(sdl-quit)

;;; opengl.scm ends here
