;; gfx.scm -- SDL_gfxPrimitives for Guile           -*- Guile-Scheme -*-
;;
;; Copyright (C) 2001 Alex Shinn <foof@synthcode.com>
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

(define-module (sdl gfx))

(use-modules (sdl sdl))

(if (and (dynamic-object? (dynamic-link "libpthread"))
         (dynamic-object? (dynamic-link "libSDL"))
         (dynamic-object? (dynamic-link "libguileSDL")))
    (let ((lib (dynamic-link "libguileSDL_gfx")))
      (if (dynamic-object? lib)
          (dynamic-call "sdl_gfx_init" lib)
          (error "could not find libguileSDL_gfx") ))
    (error "could not init libSDL, libguileSDL or libpthread") )

