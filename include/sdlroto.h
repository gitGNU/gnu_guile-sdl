#ifndef __SDLROTO_H
#define __SDLROTO_H

/*--------------------Scheme functions-------------------------*/ 

SCM sdl_roto_zoom_surface (SCM surface_smob, SCM s_angle, SCM s_zoom,
                           SCM s_smooth);


SCM sdl_zoom_surface (SCM surface_smob, SCM s_zoomx, SCM s_zoomy,
                      SCM s_smooth);

SCM vertical_flip_surface (SCM surface);
SCM horiztonal_flip_surface (SCM surface);
SCM vh_flip_surface (SCM surface);

/*-------------------------------------------------------------*/

/* Init bindings for SDL_rotozoom */
void sdl_init_rotozoom ();

#endif
