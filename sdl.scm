;; sdl.scm -- SDL for Guile                       -*- Guile-Scheme -*-
;;
;; Copyright (C) 2001 Alex Shinn <foof@debian.org>
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.  */

(define-module (sdl sdl))

(define sdl-version "0.1.0")

(export sdl-set-video-mode sdl-version sdl-init sdl-quit sdl-create-rgb-surface
        sdl-create-rgb-surface-from sdl-create-cursor sdl-fill-rect
        sdl-create-yuv-overlay sdl-make-rect sdl-make-color sdl-make-palette
        sdl-make-pixel-format sdl-get-video-surface sdl-get-video-info
        sdl-video-driver-name sdl-list-modes sdl-video-mode-ok sdl-update-rect
        sdl-update-rects sdl-flip sdl-set-colors sdl-set-palette sdl-set-gamma
        sdl-get-gamma-ramp sdl-set-gamma-ramp sdl-map-rgb sdl-map-rgba
        sdl-get-rgb sdl-get-rgba sdl-free-surface sdl-lock-surface
        sdl-unlock-surface sdl-load-bmp sdl-save-bmp sdl-set-color-key
        sdl-set-alpha sdl-set-clip-rect sdl-get-clip-rect sdl-convert-surface
        sdl-blit-surface sdl-display-format sdl-display-format-alpha
        sdl-warp-mouse sdl-free-cursor sdl-set-cursor sdl-get-cursor
        sdl-show-cursor sdl-gl-load-library sdl-gl-get-proc-address
        sdl-gl-get-attribute sdl-gl-set-attribute sdl-gl-swap-buffers
        sdl-lock-yuv-overlay sdl-unlock-yuv-overlay sdl-display-yuv-overlay
        sdl-free-yuv-overlay
        sdl-load-image
        )

(if (and (dynamic-object? (dynamic-link "libpthread"))
         (dynamic-object? (dynamic-link "libSDL"))
         (dynamic-object? (dynamic-link "libSDL_image")))
    (let ((lib (dynamic-link "libguileSDL")))
      (if (dynamic-object? lib)
          (dynamic-call "guile_sdl_init" lib)
          (error "could not find libguileSDL") ))
    (error "could not init libSDL or libpthread") )
