#include <SDL/SDL.h>
#include <guile/gh.h>
#include "sdljoystick.h"

long sdl_joystick_tag;

/*---------------------SMOB functions--------------------------*/ 
static scm_sizet free_joy (SCM joy_smob);
static int print_joy (SCM joy_smob, SCM port, scm_print_state *pstate);

/*--------------------Scheme functions-------------------------*/ 

static SCM sdl_joystick_null_p (SCM joy_smob);

static SCM sdl_num_joysticks (void);
static SCM sdl_joystick_name (SCM s_index);

static SCM sdl_joystick_open (SCM s_index);
static SCM sdl_joystick_opened_p (SCM s_index);

static SCM sdl_joystick_index       (SCM joy_smob);
static SCM sdl_joystick_num_axes    (SCM joy_smob);
static SCM sdl_joystick_num_balls   (SCM joy_smob);
static SCM sdl_joystick_num_hats    (SCM joy_smob);
static SCM sdl_joystick_num_buttons (SCM joy_smob);

static SCM sdl_joystick_update      (void);
static SCM sdl_joystick_event_state (SCM s_state);

static SCM sdl_joystick_get_axis   (SCM joy_smob, SCM s_index);
static SCM sdl_joystick_get_ball   (SCM joy_smob, SCM s_index);
static SCM sdl_joystick_get_hat    (SCM joy_smob, SCM s_index);
static SCM sdl_joystick_get_button (SCM joy_smob, SCM s_index);

static SCM sdl_joystick_close  (SCM joy_smob);


/*-------------------------------------------------------------*/
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
  
  /* Check for NULL drive object */
  scm_make_gsubr ("sdl-joystick-null?", 1, 0, 0, sdl_joystick_null_p);  
   
  /* Register Scheme functions */
  scm_make_gsubr ("sdl-num-joysticks", 0, 0, 0, sdl_num_joysticks);
  scm_make_gsubr ("sdl-joystick-name", 1, 0, 0, sdl_joystick_name);

  scm_make_gsubr ("sdl-joystick-open", 1, 0, 0, sdl_joystick_open);
  scm_make_gsubr ("sdl-joystick-opened?", 1, 0, 0, sdl_joystick_opened_p);

  scm_make_gsubr ("sdl-joystick-index", 1, 0, 0, sdl_joystick_index);
  scm_make_gsubr ("sdl-joystick-num-axes", 1, 0, 0, sdl_joystick_num_axes);
  scm_make_gsubr ("sdl-joystick-num-balls", 1, 0, 0, sdl_joystick_num_balls);
  scm_make_gsubr ("sdl-joystick-num-hats", 1, 0, 0, sdl_joystick_num_hats);
  scm_make_gsubr ("sdl-joystick-num-buttons",1, 0, 0,sdl_joystick_num_buttons);

  scm_make_gsubr ("sdl-joystick-update",0, 0, 0,sdl_joystick_update);
  scm_make_gsubr ("sdl-joystick-event_state",0, 0, 0,sdl_joystick_event_state);

  scm_make_gsubr ("sdl-joystick-get-axis",2, 0, 0,sdl_joystick_get_axis);
  scm_make_gsubr ("sdl-joystick-get-ball",2, 0, 0,sdl_joystick_get_ball);
  scm_make_gsubr ("sdl-joystick-get-hat",2, 0, 0,sdl_joystick_get_hat);
  scm_make_gsubr ("sdl-joystick-get-button",2, 0, 0,sdl_joystick_get_button);

  scm_make_gsubr ("sdl-joystick-close", 1, 0, 0, sdl_joystick_close);
}


static SCM 
sdl_joystick_null_p (SCM joy_smob)
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

static SCM 
sdl_num_joysticks (void)
{
  return (gh_long2scm (SDL_NumJoysticks ()));
}


static SCM 
sdl_joystick_name (SCM s_index)
{
  const char *name;

  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1, "sdl-joystick-name");

  name = SDL_JoystickName (gh_scm2long (s_index));
  return gh_str02scm (name);
}

static SCM 
sdl_joystick_open (SCM s_index)
{
  SDL_Joystick *joy;
  SCM joy_smob;
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1, "sdl-joystick-open");
  
  joy = SDL_JoystickOpen( gh_scm2long (s_index));
  
  SCM_NEWSMOB (joy_smob, sdl_joystick_tag, joy);
  return joy_smob;
}

static SCM 
sdl_joystick_opened_p (SCM s_index)
{
  int ret;

  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG1, "sdl-joystick-opened?");
  
  ret = SDL_JoystickOpened (gh_scm2long (s_index));
  
  if (ret == 1) 
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

static SCM sdl_joystick_index       (SCM joy_smob)
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


static SCM 
sdl_joystick_num_axes    (SCM joy_smob)
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

static SCM sdl_joystick_num_balls   (SCM joy_smob)
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


static SCM 
sdl_joystick_num_hats    (SCM joy_smob)
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


static SCM 
sdl_joystick_num_buttons (SCM joy_smob)
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


static 
SCM sdl_joystick_update      (void)
{
  SDL_JoystickUpdate ();
  return SCM_UNSPECIFIED;
}


static 
SCM sdl_joystick_event_state (SCM s_state)
{
  int ret;

  SCM_ASSERT (gh_exact_p (s_state), s_state, SCM_ARG1, 
	      "sdl-joystick-event-state");
  
  ret = SDL_JoystickEventState (gh_scm2long (s_state));

  return gh_long2scm (ret);
}


static SCM 
sdl_joystick_get_axis   (SCM joy_smob, SCM s_index)
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

static SCM 
sdl_joystick_get_ball   (SCM joy_smob, SCM s_index)
{
  SDL_Joystick *joy;
  SCM s_ret;
  int dx, dy;

  SCM_ASSERT (SMOB_JOYSTICKP (joy_smob), joy_smob,  SCM_ARG1, 
	      "sdl-joystick-get-ball");
  
  SCM_ASSERT (gh_exact_p (s_index), s_index, SCM_ARG2, 
	      "sdl-joystick-get-ball");

  joy = (SDL_Joystick *) SCM_CDR (joy_smob);
  
  s_ret = SCM_LIST0;
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

static SCM 
sdl_joystick_get_hat    (SCM joy_smob, SCM s_index)
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


static SCM 
sdl_joystick_get_button (SCM joy_smob, SCM s_index)
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

static SCM 
sdl_joystick_close  (SCM joy_smob)
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


/*-------------------------------------------------------------*/


static scm_sizet 
free_joy (SCM joy_smob) 
{
  SDL_Joystick *joy = (SDL_Joystick *) SCM_SMOB_DATA (joy_smob);

  if (joy != NULL)
    SDL_JoystickClose (joy);
 
  return 0; /* No idea of how much is actually freed */
}


static int 
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
