#include <SDL/SDL.h>
#include <guile/gh.h>
#include <libguile.h>
#include "sdljoystick.h"

long sdl_joystick_tag;


SCM_DEFINE( sdl_joystick_p, "sdl-joystick?", 1, 0, 0,
            (SCM joy_smob),
"Returns #t if arg is a joystick smob, #f otherwise.")
#define FUNC_NAME s_sdl_joystick_p
{
  return SMOB_JOYSTICKP (joy_smob) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_null_p, "sdl-joystick-null?", 1, 0, 0,
            (SCM joy_smob),
"Returns #t if arg is a NULL joystick smob, #f otherwise.")
#define FUNC_NAME s_sdl_joystick_null_p
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob),
	      joy_smob, SCM_ARG1, "sdl-joystick-null?");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);

  if (joy == NULL)
    return SCM_BOOL_T;
  else 
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_num_joysticks, "sdl-num-joysticks", 0, 0, 0,
            (void),
"Returns the number of joysticks.")
#define FUNC_NAME s_sdl_num_joysticks
{
  return (gh_long2scm (SDL_NumJoysticks ()));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_name, "sdl-joystick-name", 0, 1, 0,
            (SCM s_index),
"Get joystick name.")
#define FUNC_NAME s_sdl_joystick_name
{
  const char *name;
  int index=0;

  if (s_index != SCM_UNDEFINED) {
    SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1,
                "sdl-joystick-name");
    index = gh_scm2long (s_index);
  }

  name = SDL_JoystickName (index);
  return gh_str02scm (name);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_open, "sdl-joystick-open", 0, 1, 0,
            (SCM s_index),
"Opens a joystick for use.")
#define FUNC_NAME s_sdl_joystick_open
{
  SDL_Joystick *joy;
  SCM joy_smob;
  int index=0;

  if (s_index != SCM_UNDEFINED) {
    SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1,
                "sdl-joystick-open");
    index = gh_scm2long (s_index);
  }
  
  joy = SDL_JoystickOpen (index);
  
  SCM_NEWSMOB (joy_smob, sdl_joystick_tag, joy);
  return joy_smob;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_opened_p, "sdl-joystick-opened?", 0, 1, 0,
            (SCM s_index),
"Determine if a joystick has been opened.")
#define FUNC_NAME s_sdl_joystick_opened_p
{
  int ret;
  int index=0;

  if (s_index != SCM_UNDEFINED) {
    SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1,
                "sdl-joystick-opened?");
    index = gh_scm2long (s_index);
  }
  
  ret = SDL_JoystickOpened (index);
  
  if (ret == 1) 
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_index, "sdl-joystick-index", 1, 0, 0,
            (SCM joy_smob),
"Get the index of an SDL-Joystick.")
#define FUNC_NAME s_sdl_joystick_index
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-index");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    return gh_long2scm (SDL_JoystickIndex (joy));
  }
  else {
    return gh_long2scm (-1);
  }    
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_num_axes, "sdl-joystick-num-axes", 1, 0, 0,
            (SCM joy_smob),
"Get the number of Joystick axes.")
#define FUNC_NAME s_sdl_joystick_num_axes
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-num-axes");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    return gh_long2scm (SDL_JoystickNumAxes (joy));
  }
  else {
    return gh_long2scm (-1);
  }    
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_num_balls, "sdl-joystick-num-balls", 1, 0, 0,
            (SCM joy_smob),
"Get the number of Joystick trackballs.")
#define FUNC_NAME s_sdl_joystick_num_balls
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-num-balls");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    return gh_long2scm (SDL_JoystickNumBalls (joy));
  }
  else {
    return gh_long2scm (-1);
  }    
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_num_hats, "sdl-joystick-num-hats", 1, 0, 0,
            (SCM joy_smob),
"Get the number of Joystick hats.")
#define FUNC_NAME s_sdl_joystick_num_hats
{
    SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-num-hats");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    return gh_long2scm (SDL_JoystickNumHats (joy));
  }
  else {
    return gh_long2scm (-1);
  }    
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_num_buttons, "sdl-joystick-num-buttons", 1, 0, 0,
            (SCM joy_smob),
"Get the number of Joystick buttons.")
#define FUNC_NAME s_sdl_joystick_num_buttons
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-num-buttons");
  
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    return gh_long2scm (SDL_JoystickNumButtons (joy));
  }
  else {
    return gh_long2scm (-1);
  }    
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_update, "sdl-joystick-update", 0, 0, 0,
            (void),
"Updates the state of all Joysticks.")
#define FUNC_NAME s_sdl_joystick_update
{
  SDL_JoystickUpdate ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_event_state, "sdl-joystick-event-state", 1, 0, 0,
            (SCM s_state),
"Set the Joystick event processing state.")
#define FUNC_NAME s_sdl_joystick_event_state
{
  int ret;

  SCM_ASSERT (gh_exact_p (s_state), s_state, SCM_ARG1, 
	      "sdl-joystick-event-state");
  
  ret = SDL_JoystickEventState (gh_scm2long (s_state));

  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_get_axis, "sdl-joystick-get-axis", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
"Get the current state of an axis.")
#define FUNC_NAME s_sdl_joystick_get_axis
{
  SDL_Joystick *joy;
  int ret;

  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-get-axis");
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG2, 
	      "sdl-joystick-get-axis");

  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) 
    ret = SDL_JoystickGetAxis (joy, gh_scm2long (s_index));
  else 
    ret = -1;

  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_get_ball, "sdl-joystick-get-ball", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
"Get relative trackball motion.")
#define FUNC_NAME s_sdl_joystick_get_ball
{
  SDL_Joystick *joy;
  SCM s_ret;
  int dx, dy;

  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-get-ball");
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG2, 
	      "sdl-joystick-get-ball");

  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  s_ret = scm_list_n (NULL);
  if (joy != NULL) {
    int ret;
    
    ret = SDL_JoystickGetBall (joy, gh_scm2long (s_index), &dx, &dy);
  
    if (ret != -1) {
      s_ret = scm_acons (gh_symbol2scm ("dy"), gh_long2scm (dy), s_ret);
      s_ret = scm_acons (gh_symbol2scm ("dx"), gh_long2scm (dx), s_ret);
    }
  }
  
  return s_ret;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_get_hat, "sdl-joystick-get-hat", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
"Get the current state of the Joystick hat.")
#define FUNC_NAME s_sdl_joystick_get_hat
{
  SDL_Joystick *joy;
  int ret;

  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-get-hat");
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG2, 
	      "sdl-joystick-get-hat");

  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) 
    ret = SDL_JoystickGetHat (joy, gh_scm2long (s_index));
  else 
    ret = -1;

  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_get_button, "sdl-joystick-get-button", 2, 0, 0,
            (SCM joy_smob,
             SCM s_index),
"Get the current state of a given button on a given joystick.")
#define FUNC_NAME s_sdl_joystick_get_button
{
  SDL_Joystick *joy;
  int ret;

  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-get-button");
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG2, 
	      "sdl-joystick-get-button");

  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) 
    ret = SDL_JoystickGetButton (joy, gh_scm2long (s_index));
  else 
    ret = -1;

  return gh_long2scm (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_joystick_close, "sdl-joystick-close", 1, 0, 0,
            (SCM joy_smob),
"Close a previously opened Joystick.")
#define FUNC_NAME s_sdl_joystick_close
{
  SDL_Joystick *joy;
  
  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-close");
  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  if (joy != NULL) {
    SDL_JoystickClose (joy);
    SCM_SETCDR (joy_smob, 0); /* Set to NULL */
  }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/*-------------------------------------------------------------*/


size_t 
free_joy (SCM joy_smob) 
{
  SDL_Joystick *joy = (SDL_Joystick *) SCM_SMOB_DATA (joy_smob);

  if (joy != NULL)
    SDL_JoystickClose (joy);
 
  return 0; /* No idea of how much is actually freed */
}


int 
print_joy (SCM joy_smob, SCM port, scm_print_state *pstate)
{
  SDL_Joystick *joy = (SDL_Joystick *) SCM_SMOB_DATA (joy_smob);
  

  if (joy != NULL) { /* Print the device index */
    int index;

    scm_puts ("#<SDL-Joystick ", port);
    
    index = SDL_JoystickIndex (joy);
    scm_display (gh_long2scm (index), port);

    scm_puts (">", port);
  }
  else {
    scm_puts ("#<SDL-Joystick NULL>", port);
  }

  /* Non-zero means success */
  return 1;
}


void 
sdl_init_joystick ()
{  
  /* A SMOB for Joystick */
  sdl_joystick_tag = scm_make_smob_type_mfpe ("SDL-Joystick",
					      /* Hope it doesn't matter */
					      sizeof(SDL_Joystick *),
					      NULL, 
					      free_joy, 
					      print_joy, 
					      NULL);
  
/*   /\* Check for NULL drive object *\/ */
/*   scm_c_define_gsubr ("sdl-joystick-null?", 1, 0, 0, sdl_joystick_null_p);   */
   
/*   /\* Register Scheme functions *\/ */
/*   scm_c_define_gsubr ("sdl-num-joysticks", 0, 0, 0, sdl_num_joysticks); */
/*   scm_c_define_gsubr ("sdl-joystick-name", 1, 0, 0, sdl_joystick_name); */

/*   scm_c_define_gsubr ("sdl-joystick-open", 1, 0, 0, sdl_joystick_open); */
/*   scm_c_define_gsubr ("sdl-joystick-opened?", 1, 0, 0, sdl_joystick_opened_p); */

/*   scm_c_define_gsubr ("sdl-joystick-index", 1, 0, 0, sdl_joystick_index); */
/*   scm_c_define_gsubr ("sdl-joystick-num-axes", 1, 0, 0, sdl_joystick_num_axes); */
/*   scm_c_define_gsubr ("sdl-joystick-num-balls", 1, 0, 0, sdl_joystick_num_balls); */
/*   scm_c_define_gsubr ("sdl-joystick-num-hats", 1, 0, 0, sdl_joystick_num_hats); */
/*   scm_c_define_gsubr ("sdl-joystick-num-buttons",1, 0, 0,sdl_joystick_num_buttons); */

/*   scm_c_define_gsubr ("sdl-joystick-update",0, 0, 0,sdl_joystick_update); */
/*   scm_c_define_gsubr ("sdl-joystick-event_state",0, 0, 0,sdl_joystick_event_state); */

/*   scm_c_define_gsubr ("sdl-joystick-get-axis",2, 0, 0,sdl_joystick_get_axis); */
/*   scm_c_define_gsubr ("sdl-joystick-get-ball",2, 0, 0,sdl_joystick_get_ball); */
/*   scm_c_define_gsubr ("sdl-joystick-get-hat",2, 0, 0,sdl_joystick_get_hat); */
/*   scm_c_define_gsubr ("sdl-joystick-get-button",2, 0, 0,sdl_joystick_get_button); */

/*   scm_c_define_gsubr ("sdl-joystick-close", 1, 0, 0, sdl_joystick_close); */

  /* exported symbols */
  scm_c_export ("sdl-joystick?",
                "sdl-joystick-null?",    "sdl-num-joysticks",
                "sdl-joystick-name",     "sdl-joystick-open",
                "sdl-joystick-opened?",  "sdl-joystick-index",
                "sdl-joystick-num-axes", "sdl-joystick-num-balls",
                "sdl-joystick-num-hats", "sdl-joystick-num-buttons",
                "sdl-joystick-update",   "sdl-joystick-event_state",
                "sdl-joystick-get-axis", "sdl-joystick-get-ball",
                "sdl-joystick-get-hat",  "sdl-joystick-get-button",
                "sdl-joystick-close",    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdljoystick.x"
#endif

}


