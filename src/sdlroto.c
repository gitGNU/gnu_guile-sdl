#include <SDL/SDL.h>
#include <SDL_rotozoom.h>
#include <guile/gh.h>
#include <libguile.h>
#include "sdlvideo.h"
#include "sdlroto.h"


SCM_DEFINE( sdl_roto_zoom_surface, "sdl-roto-zoom-surface", 2, 2, 0,
            (SCM surface_smob,
             SCM s_angle,
             SCM s_zoom,
             SCM s_smooth),
"Returns a new rotated and zoomed copy of a surface.
Zoom defaults to 1.0, and smooth defaults to #f.")
#define FUNC_NAME s_sdl_roto_zoom_surface
{
  SDL_Surface *surface, *new_surface;  
  int smooth=0;
  double angle=0.0, zoom=1.0;
  SCM new_smob;

  SCM_ASSERT (SMOB_SURFACEP (surface_smob), surface_smob, SCM_ARG1, 
	      "sdl-roto-zoom-surface");
  surface = (SDL_Surface *) SCM_CDR (surface_smob);
  
  SCM_ASSERT (gh_number_p (s_angle), s_angle, SCM_ARG2, 
	      "sdl-roto-zoom-surface");
  angle = scm_num2dbl (s_angle, "sdl-roto-zoom-surface");

  if (s_zoom != SCM_UNDEFINED) {
    SCM_ASSERT (gh_number_p (s_zoom), s_zoom, SCM_ARG3, 
                "sdl-roto-zoom-surface");
    angle = scm_num2dbl (s_angle, "sdl-roto-zoom-surface");
  }

  if (s_smooth != SCM_UNDEFINED) {
    SCM_ASSERT (gh_boolean_p (s_smooth), s_smooth, SCM_ARG4, 
                "sdl-roto-zoom-surface");
    smooth = (s_smooth != SCM_BOOL_F);
  }

  new_surface = rotozoomSurface (surface, angle, zoom, smooth);
  
  SCM_NEWSMOB (new_smob, surface_tag, new_surface);
  return new_smob;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_zoom_surface, "sdl-zoom-surface", 2, 2, 0,
            (SCM surface_smob,
             SCM s_zoomx,
             SCM s_zoomy,
             SCM s_smooth),
"Returns a new scaled copy of a surface.
ZoomY defaults to ZoomX, and smooth defaults to #f.")
#define FUNC_NAME s_sdl_zoom_surface
{
  SDL_Surface *surface, *new_surface;  
  double zoomx=1.0, zoomy=1.0;
  int smooth=0;
  SCM new_smob;

  SCM_ASSERT (SMOB_SURFACEP (surface_smob), surface_smob, SCM_ARG1, 
	      "sdl-zoom-surface");
  surface = (SDL_Surface *) SCM_CDR (surface_smob);
  
  SCM_ASSERT (gh_number_p (s_zoomx), s_zoomx, SCM_ARG2, 
	      "sdl-zoom-surface");
  zoomx = scm_num2dbl (s_zoomx, "sdl-zoom-surface");

  if (s_zoomy != SCM_UNDEFINED) {
    SCM_ASSERT (gh_number_p (s_zoomy), s_zoomy, SCM_ARG3, 
                "sdl-zoom-surface");
    zoomy = scm_num2dbl (s_zoomy, "sdl-zoom-surface");
  } else {
    zoomy = zoomx;
  }

  if (s_smooth != SCM_UNDEFINED) {
    SCM_ASSERT (gh_boolean_p (s_smooth), s_smooth, SCM_ARG4, 
                "sdl-zoom-surface");
    smooth = (s_smooth != SCM_BOOL_F);
  }

  new_surface = zoomSurface (surface, zoomx, zoomy, smooth);

  SCM_NEWSMOB (new_smob, surface_tag, new_surface);
  return new_smob;
}
#undef FUNC_NAME


SCM_DEFINE( vertical_flip_surface, "sdl-vertical-flip-surface", 1, 0, 0,
            (SCM s_surface),
"Returns a new vertically flipped copy of a surface.")
#define FUNC_NAME s_vertical_flip_surface
{
   int i, w, h;
   SDL_Surface *src, *dst;
   SDL_Rect srcrect, dstrect;

   /* verify args */
   SCM_ASSERT ((SCM_NIMP (s_surface)
                && (long) SCM_CAR (s_surface) == surface_tag),
               s_surface, SCM_ARG1, "sdl-vertical-flip-surface");

   /* get source and dimensions */
   src = (SDL_Surface *) SCM_CDR (s_surface);
   w = src->w;
   h = src->h;

   /* create a new surface */
   dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

   /* initialize the rects */
   srcrect.x = 0;  srcrect.y = 0;    srcrect.w = w;  srcrect.h = 1;
   dstrect.x = 0;  dstrect.y = h-1;  dstrect.w = w;  dstrect.h = 1;

   /* loop through, copying lines from top to bottom */
   for (i=h; i>=0; i--) {
      SDL_BlitSurface (src, &srcrect, dst, &dstrect);
      srcrect.y++;
      dstrect.y--;
   }

   /* return the surface */
   SCM_RETURN_NEWSMOB (surface_tag, dst);
}
#undef FUNC_NAME


SCM_DEFINE( horiztonal_flip_surface, "sdl-horizontal-flip-surface", 1, 0, 0,
            (SCM s_surface),
"Returns a new horizontally flipped copy of a surface.")
#define FUNC_NAME s_horiztonal_flip_surface
{
   int i, w, h;
   SDL_Surface *src, *dst;
   SDL_Rect srcrect, dstrect;

   /* verify args */
   SCM_ASSERT ((SCM_NIMP (s_surface)
                && (long) SCM_CAR (s_surface) == surface_tag),
               s_surface, SCM_ARG1, "sdl-horizontal-flip-surface");

   /* get source and dimensions */
   src = (SDL_Surface *) SCM_CDR (s_surface);
   w = src->w;
   h = src->h;

   /* create a new surface */
   dst = SDL_CreateRGBSurface (src->flags, w, h, 16, 0, 0, 0, 0);

   /* initialize the rects */
   srcrect.x = 0;    srcrect.y = 0;  srcrect.w = 1;  srcrect.h = h;
   dstrect.x = w-1;  dstrect.y = 0;  dstrect.w = 1;  dstrect.h = h;

   /* loop through, copying lines from left to right */
   for (i=w; i>=0; i--) {
      SDL_BlitSurface (src, &srcrect, dst, &dstrect);
      srcrect.x++;
      dstrect.x--;
   }

   /* return the surface */
   SCM_RETURN_NEWSMOB (surface_tag, dst);
}
#undef FUNC_NAME


SCM_DEFINE( vh_flip_surface, "sdl-vh-flip-surface", 1, 0, 0,
            (SCM s_surface),
"Returns a new vertically & horizontally flipped copy of a surface.")
#define FUNC_NAME s_vh_flip_surface
{
   SCM temp = vertical_flip_surface (s_surface);
   return horiztonal_flip_surface (temp);
}
#undef FUNC_NAME


void 
sdl_init_rotozoom ()
{  
  /* exported symbols */
  scm_c_export (
     "sdl-roto-zoom-surface", "sdl-zoom-surface",
     "sdl-vertical-flip-surface", "sdl-horizontal-flip-surface",
     "sdl-vh-flip-surface", NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlroto.x"
#endif

}

