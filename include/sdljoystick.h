#ifndef __SDLJOYSTICK_H
#define __SDLJOYSTICK_H

extern long sdl_joystick_tag;

/*---------------------SMOB functions--------------------------*/ 
scm_sizet free_joy (SCM joy_smob);
int print_joy (SCM joy_smob, SCM port, scm_print_state *pstate);

/*--------------------Scheme functions-------------------------*/ 

SCM sdl_joystick_null_p (SCM joy_smob);

SCM sdl_num_joysticks (void);
SCM sdl_joystick_name (SCM s_index);

SCM sdl_joystick_open (SCM s_index);
SCM sdl_joystick_opened_p (SCM s_index);

SCM sdl_joystick_index       (SCM joy_smob);
SCM sdl_joystick_num_axes    (SCM joy_smob);
SCM sdl_joystick_num_balls   (SCM joy_smob);
SCM sdl_joystick_num_hats    (SCM joy_smob);
SCM sdl_joystick_num_buttons (SCM joy_smob);

SCM sdl_joystick_update      (void);
SCM sdl_joystick_event_state (SCM s_state);

SCM sdl_joystick_get_axis   (SCM joy_smob, SCM s_index);
SCM sdl_joystick_get_ball   (SCM joy_smob, SCM s_index);
SCM sdl_joystick_get_hat    (SCM joy_smob, SCM s_index);
SCM sdl_joystick_get_button (SCM joy_smob, SCM s_index);

SCM sdl_joystick_close  (SCM joy_smob);

#define SMOB_JOYSTICKP(x) (SCM_NIMP (x)\
		       && (long) SCM_CAR (x) == sdl_joystick_tag)

void sdl_init_joystick ();

#endif
