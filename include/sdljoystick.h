#ifndef __SDLJOYSTICK_H
#define __SDLJOYSTICK_H

extern long sdl_joystick_tag;

#define SMOB_JOYSTICKP(x) (SCM_NIMP (x)\
		       && (long) SCM_CAR (x) == sdl_joystick_tag)

void sdl_init_joystick ();

#endif
