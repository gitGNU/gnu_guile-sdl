#include <SDL/SDL.h>
#include <SDL_rotozoom.h>
#include <guile/gh.h>
#include <libguile.h>
#include "sdlvideo.h"
#include "sdlroto.h"


void 
sdl_init_rotozoom ()
{  
  /* Register Scheme functions */
  scm_c_define_gsubr ("sdl-roto-zoom-surface", 4, 0, 0, sdl_roto_zoom_surface);
  scm_c_define_gsubr ("sdl-zoom-surface", 4, 0, 0, sdl_zoom_surface);

  /* Transpositions */
  scm_c_define_gsubr ("sdl-vertical-flip-surface",    1, 0, 0, vertical_flip_surface);
  scm_c_define_gsubr ("sdl-horizontal-flip-surface",  1, 0, 0, horiztonal_flip_surface);
  scm_c_define_gsubr ("sdl-vh-flip-surface",          1, 0, 0, vh_flip_surface);

  /* exported symbols */
  scm_c_export (
     "sdl-roto-zoom-surface", "sdl-zoom-surface",
     "sdl-vertical-flip-surface", "sdl-horizontal-flip-surface",
     "sdl-vh-flip-surface", NULL);
}



SCM 
sdl_roto_zoom_surface (SCM surface_smob, SCM s_angle, SCM s_zoom, SCM s_smooth)
{
  SDL_Surface *surface, *new_surface;  
  int smooth;
  SCM new_smob;

  SCM_ASSERT (SMOB_SURFACEP (surface_smob), surface_smob, SCM_ARG1, 
	      "sdl-roto-zoom-surface");
  
  SCM_ASSERT (gh_number_p (s_angle), s_angle, SCM_ARG2, 
	      "sdl-roto-zoom-surface");
  
  SCM_ASSERT (gh_number_p (s_zoom), s_zoom, SCM_ARG3, 
	      "sdl-roto-zoom-surface");

  SCM_ASSERT (gh_boolean_p (s_smooth), s_smooth, SCM_ARG4, 
	      "sdl-roto-zoom-surface");

  surface = (SDL_Surface *) SCM_CDR (surface_smob);
  smooth = (s_smooth == SCM_BOOL_T);

  new_surface = rotozoomSurface (surface, gh_scm2double(s_angle), 
				 gh_scm2double(s_zoom), smooth);
  
  SCM_NEWSMOB (new_smob, surface_tag, new_surface);
  return new_smob;
}


SCM 
sdl_zoom_surface (SCM surface_smob, SCM s_zoomx, SCM s_zoomy, SCM s_smooth)
{
  SDL_Surface *surface, *new_surface;  
  int smooth;
  SCM new_smob;

  SCM_ASSERT (SMOB_SURFACEP (surface_smob), surface_smob, SCM_ARG1, 
	      "sdl-zoom-surface");
  
  SCM_ASSERT (gh_number_p (s_zoomx), s_zoomx, SCM_ARG2, 
	      "sdl-zoom-surface");
  
  SCM_ASSERT (gh_number_p (s_zoomy), s_zoomy, SCM_ARG3, 
	      "sdl-zoom-surface");

  SCM_ASSERT (gh_boolean_p (s_smooth), s_smooth, SCM_ARG4, 
	      "sdl-zoom-surface");

  surface = (SDL_Surface *) SCM_CDR (surface_smob);
  smooth = (s_smooth == SCM_BOOL_T);

  new_surface = zoomSurface (surface, gh_scm2double(s_zoomx), 
			     gh_scm2double(s_zoomy), smooth);

  SCM_NEWSMOB (new_smob, surface_tag, new_surface);
  return new_smob;
}


SCM
vertical_flip_surface (SCM s_surface)
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

SCM
horiztonal_flip_surface (SCM s_surface)
{
   int i, w, h;
   SDL_Surface *src, *dst;
   SDL_Rect srcrect, dstrect;

   /* verify args */
   SCM_ASSERT ((SCM_NIMP (s_surface)
                && (long) SCM_CAR (s_surface) == surface_tag),
               s_surface, SCM_ARG1, "sdl-horiztonal-flip-surface");

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

SCM
vh_flip_surface (SCM s_surface)
{
   SCM temp = vertical_flip_surface (s_surface);
   return horiztonal_flip_surface (temp);
}

