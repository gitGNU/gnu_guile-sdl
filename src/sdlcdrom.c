#include <SDL/SDL.h>
#include <libguile.h>
#include "sdlcdrom.h"

long sdl_cdrom_tag;

void 
sdl_init_cdrom ()
{  
  /* A SMOB for CD drive */
  sdl_cdrom_tag = scm_make_smob_type_mfpe ("SDL-CD",
					   sizeof(SDL_CD),
					   NULL, 
					   free_cd, 
					   print_cd, 
					   NULL);
  
  /* Check for NULL drive object */
  scm_c_define_gsubr ("sdl-cd-null?", 1, 0, 0, sdl_cd_null_p);  
   
  /* Register Scheme functions */
  scm_c_define_gsubr ("sdl-cd-num-drives", 0, 0, 0, sdl_cd_num_drives);
  scm_c_define_gsubr ("sdl-cd-name", 1, 0, 0, sdl_cd_name);

  scm_c_define_gsubr ("sdl-cd-open", 1, 0, 0, sdl_cd_open);

  scm_c_define_gsubr ("sdl-cd-status", 1, 0, 0, sdl_cd_status);
  scm_c_define_gsubr ("sdl-cd-in-drive?", 1, 0, 0, sdl_cd_in_drive_p);

  /* Next 4 Should be called after calling sdl-cd-status or sdl-cd-in-drive? */
  scm_c_define_gsubr ("sdl-cd-get-num-tracks", 1, 0, 0, sdl_cd_get_num_tracks);
  scm_c_define_gsubr ("sdl-cd-get-cur-track", 1, 0, 0, sdl_cd_get_cur_track);
  scm_c_define_gsubr ("sdl-cd-get-cur-frame", 1, 0, 0, sdl_cd_get_cur_frame);
  scm_c_define_gsubr ("sdl-cd-get-nth-track", 2, 0, 0, sdl_cd_get_nth_track);

  /* Play Pause Stop Eject */
  scm_c_define_gsubr ("sdl-cd-play-tracks", 5, 0, 0, sdl_cd_play_tracks);
  scm_c_define_gsubr ("sdl-cd-play", 3, 0, 0, sdl_cd_play);
  scm_c_define_gsubr ("sdl-cd-pause", 1, 0, 0, sdl_cd_pause);
  scm_c_define_gsubr ("sdl-cd-resume", 1, 0, 0, sdl_cd_resume);
  scm_c_define_gsubr ("sdl-cd-stop", 1, 0, 0, sdl_cd_stop);
  scm_c_define_gsubr ("sdl-cd-eject", 1, 0, 0, sdl_cd_eject);
  
  scm_c_define_gsubr ("sdl-cd-close", 1, 0, 0, sdl_cd_close);

  scm_c_define_gsubr ("sdl-cd-msf-to-frames", 3, 0, 0, sdl_cd_msf_to_frames);
  scm_c_define_gsubr ("sdl-cd-frames-to-msf", 1, 0, 0, sdl_cd_frames_to_msf);
}


SCM 
sdl_cd_null_p (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob),
	      cd_smob, SCM_ARG1, "sdl-cd-null?");
  
  cd = (SDL_CD *) SCM_CDR (cd_smob);

  if (cd == NULL)
    return SCM_BOOL_T;
  else 
    return SCM_BOOL_F;
}

SCM 
sdl_cd_num_drives (void)
{
  return (scm_long2num (SDL_CDNumDrives ()));
}


SCM 
sdl_cd_name (SCM s_drive)
{
  const char *name;

  SCM_ASSERT (scm_exact_p (s_drive), s_drive, SCM_ARG1, "sdl-cd-name");

  name = SDL_CDName (scm_num2long (s_drive, SCM_ARG1, "sdl-cd-name"));
  return scm_makfrom0str (name);
}

SCM 
sdl_cd_open (SCM s_drive)
{
  SDL_CD *cd;
  SCM cd_smob;
  
  SCM_ASSERT (scm_exact_p (s_drive), s_drive, SCM_ARG1, "sdl-cd-open");
  
  cd = SDL_CDOpen (scm_num2long (s_drive, SCM_ARG1, "sdl-cd-open"));
  
  SCM_NEWSMOB (cd_smob, sdl_cdrom_tag, cd);
  return cd_smob;
}

SCM 
sdl_cd_status (SCM cd_smob)
{
  SDL_CD *cd;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob, SCM_ARG1, "sdl-cd-status");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return scm_long2num (SDL_CDStatus (cd));
  }
  else {
    return scm_long2num (CD_ERROR);
  }
}

SCM 
sdl_cd_in_drive_p (SCM cd_smob)
{
  SDL_CD *cd;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob, SCM_ARG1, "sdl-cd-in-drive?");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    if ( CD_INDRIVE (SDL_CDStatus (cd)))
      return SCM_BOOL_T;
    else 
      return SCM_BOOL_F;
  }
  else {
    return SCM_BOOL_F;
  }
}


SCM 
sdl_cd_get_num_tracks (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob, SCM_ARG1, "sdl-cd-get-num-tracks");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return (scm_long2num (cd->numtracks));
  }
  else {
    return (scm_long2num (-1));
  }
}

SCM 
sdl_cd_get_cur_track  (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob, SCM_ARG1, "sdl-get-cur-track");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return (scm_long2num (cd->cur_track));
  }
  else {
    return (scm_long2num (-1));
  }
}


SCM 
sdl_cd_get_cur_frame  (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob, SCM_ARG1, "sdl-get-cur-frame");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return (scm_long2num (cd->cur_frame));
  }
  else {
    return (scm_long2num (-1));
  }
}


SCM 
sdl_cd_get_nth_track  (SCM cd_smob, SCM s_n)
{
  SDL_CD *cd;
  int n;
  SCM s_track;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-get-nth-track");
  SCM_ASSERT (scm_exact_p (s_n), s_n, SCM_ARG2, "sdl-cd-get-nth-track");
  
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  n  = scm_num2ulong (s_n, SCM_ARG2, "sdl-cd-get-nth-track");
  
  if ((cd != NULL) && (n < cd->numtracks)) {
    
    /* Form an assoc list */
    s_track = SCM_LIST0;
    s_track = scm_acons (scm_str2symbol ("offset"), 
			 scm_ulong2num (cd->track[n].offset), s_track);

    s_track = scm_acons (scm_str2symbol ("length"), 
			 scm_ulong2num (cd->track[n].length), s_track);

    s_track = scm_acons (scm_str2symbol ("type"), 
			 scm_long2num (cd->track[n].type), s_track);
    
    s_track = scm_acons (scm_str2symbol ("id"), 
			 scm_long2num (cd->track[n].id), s_track);

    return s_track;
  }
  else {
    return (SCM_LIST0);
  }
}


SCM 
sdl_cd_play_tracks (SCM cd_smob, 
		    SCM s_start_track, SCM s_start_frame, 
		    SCM s_n_tracks, SCM s_n_frames)
{
  SDL_CD *cd;
  int ret;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-play-tracks");
  
  SCM_ASSERT (scm_exact_p (s_start_track), s_start_track, SCM_ARG2, 
	      "sdl-cd-play-tracks");
  SCM_ASSERT (scm_exact_p (s_start_frame), s_start_frame, SCM_ARG3,
	      "sdl-cd-play-tracks");
  SCM_ASSERT (scm_exact_p (s_n_tracks), s_n_tracks, SCM_ARG4,
	      "sdl-cd-play-tracks");
  SCM_ASSERT (scm_exact_p (s_n_frames), s_n_frames, SCM_ARG5,
	      "sdl-cd-play-tracks");
  
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    ret = SDL_CDPlayTracks (cd, 
			    scm_num2ulong (s_start_track, SCM_ARG2,
                                           "sdl-cd-play-tracks"),
			    scm_num2ulong (s_start_frame, SCM_ARG3,
                                           "sdl-cd-play-tracks"),
			    scm_num2ulong (s_n_tracks, SCM_ARG4,
                                           "sdl-cd-play-tracks"),
			    scm_num2ulong (s_n_frames, SCM_ARG5,
                                           "sdl-cd-play-tracks"));
  }
  else {
    ret = -1;
  }
  
  return scm_long2num (ret);
}


SCM 
sdl_cd_play   (SCM cd_smob, SCM s_start, SCM s_length)
{
  SDL_CD *cd;
  int ret;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-play");
  
  SCM_ASSERT (scm_exact_p (s_start), s_start, SCM_ARG2, 
	      "sdl-cd-play");
  SCM_ASSERT (scm_exact_p (s_length), s_length, SCM_ARG3,
	      "sdl-cd-play");
  
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    ret = SDL_CDPlay (cd,
		      scm_num2ulong (s_start, SCM_ARG2, "sdl-cd-play"),
		      scm_num2ulong (s_length, SCM_ARG3, "sdl-cd-play"));
  }
  else {
    ret = -1;
  }
  
  return scm_long2num (ret);

}

SCM 
sdl_cd_pause  (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-pause");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return scm_long2num (SDL_CDPause (cd));
  }
  else {
    return scm_long2num (-1);
  }
}

SCM 
sdl_cd_resume (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-resume");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return scm_long2num (SDL_CDResume (cd));
  }
  else {
    return scm_long2num (-1);
  }
}


SCM 
sdl_cd_stop   (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-stop");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return scm_long2num (SDL_CDStop (cd));
  }
  else {
    return scm_long2num (-1);
  }
}

SCM 
sdl_cd_eject  (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-eject");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    return scm_long2num (SDL_CDEject (cd));
  }
  else {
    return scm_long2num (-1);
  }
}

SCM
sdl_cd_close  (SCM cd_smob)
{
  SDL_CD *cd;
  
  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-close");
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    SDL_CDClose (cd);
    SCM_SETCDR (cd_smob, 0); /* Set to NULL */
  }
  
  return SCM_UNSPECIFIED;
}


SCM 
sdl_cd_msf_to_frames  (SCM s_m, SCM s_s, SCM s_f)
{
  int frames;

  SCM_ASSERT (scm_exact_p (s_m), s_m, SCM_ARG1, 
	      "sdl-cd-msf-to-frames");
  
  SCM_ASSERT (scm_exact_p (s_s), s_s, SCM_ARG2,
	      "sdl-cd-msf-to-frames");

  SCM_ASSERT (scm_exact_p (s_f), s_f, SCM_ARG3,
	      "sdl-cd-msf-to-frames");
  
  frames = MSF_TO_FRAMES(scm_num2ulong(s_m, SCM_ARG1, "sdl-cd-msf-to-frames"),
                         scm_num2ulong(s_s, SCM_ARG2, "sdl-cd-msf-to-frames"),
			 scm_num2ulong(s_f, SCM_ARG3, "sdl-cd-msf-to-frames"));

  return scm_long2num (frames);
}


SCM 
sdl_cd_frames_to_msf  (SCM s_frames)
{
  int frames, m, s, f;
  SCM s_msf;
  
  SCM_ASSERT (scm_exact_p (s_frames), s_frames, SCM_ARG1,
	      "sdl-cd-frames-to-msf");
  frames = scm_num2ulong (s_frames, SCM_ARG1, "sdl-cd-frames-to-msf");

  FRAMES_TO_MSF (frames, &m , &s, &f);
  s_msf = SCM_LIST0;
  
  s_msf = scm_acons (scm_str2symbol ("f"), scm_ulong2num (f) , s_msf);
  s_msf = scm_acons (scm_str2symbol ("s"), scm_ulong2num (s) , s_msf);
  s_msf = scm_acons (scm_str2symbol ("m"), scm_ulong2num (m) , s_msf);
  
  return s_msf;
}

/*-------------------------------------------------------------*/


scm_sizet 
free_cd (SCM cd_smob) 
{
  SDL_CD *cd = (SDL_CD *) SCM_SMOB_DATA (cd_smob);
  scm_sizet size = sizeof(SDL_CD);

  if (cd != NULL)
    SDL_CDClose(cd);
 
  return size;
}


int 
print_cd (SCM cd_smob, SCM port, scm_print_state *pstate)
{
  SDL_CD *cd = (SDL_CD *) SCM_SMOB_DATA (cd_smob);
  
  if (cd != NULL) { /* Print the current status */
    
    scm_puts ("#<SDL-CD ", port);

    switch (cd->status) {

    case CD_TRAYEMPTY:
      scm_puts (" [TRAY EMPTY]>", port);
      break;
      
    case CD_STOPPED:
      scm_puts (" [STOPPED]>", port);
      break;
    
    case CD_PLAYING:
      scm_puts (" [PLAYING]>", port);
      break;

    case CD_PAUSED:
      scm_puts (" [PAUSED]>", port);
      break;

    case CD_ERROR:
      scm_puts (" [DRIVE ERROR]>", port);
      break;
      
    }
  }
  else {
    scm_puts ("#<SDL-CD NULL>", port);
  }
  
  /* Non-zero means success */
  return 1;
}
