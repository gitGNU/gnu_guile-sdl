#include <SDL/SDL.h>
#include <libguile.h>
#include "sdlcdrom.h"

long sdl_cdrom_tag;


SCM_DEFINE( sdl_cd_p, "sdl-cd?", 1, 0, 0,
            (SCM cd_smob),
"Returns #t if arg is a cd smob, #f otherwise.")
#define FUNC_NAME s_sdl_cd_p
{
  return SMOB_CDROMP (cd_smob) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_null_p, "sdl-cd-null?", 1, 0, 0,
            (SCM cd_smob),
"Returns #t if cd is a null pointer, #f otherwise.")
#define FUNC_NAME s_sdl_cd_null_p
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_num_drives, "sdl-cd-num-drives", 0, 0, 0,
            (),
"Returns the number of CD drives.")
#define FUNC_NAME s_sdl_cd_num_drives
{
  return (scm_long2num (SDL_CDNumDrives ()));
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_name, "sdl-cd-name", 0, 1, 0,
            (SCM s_drive),
"Returns a human-readable, system-dependent identifier for the CD-ROM.")
#define FUNC_NAME s_sdl_cd_name
{
  const char *name;
  int drive=0;

  if (s_drive != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_drive), s_drive, SCM_ARG1, "sdl-cd-name");
    drive = scm_num2int (s_drive, SCM_ARG1, "sdl-cd-name");
  }

  name = SDL_CDName (drive);
  return scm_makfrom0str (name);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_open, "sdl-cd-open", 0, 1, 0,
            (SCM s_drive),
"Opens a CD-ROM drive for access.")
#define FUNC_NAME s_sdl_cd_open
{
  SDL_CD *cd;
  SCM cd_smob;
  int drive=0;

  if (s_drive != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_drive), s_drive, SCM_ARG1, "sdl-cd-open");
    drive = scm_num2long (s_drive, SCM_ARG1, "sdl-cd-open");
  }

  cd = SDL_CDOpen (drive);

  SCM_NEWSMOB (cd_smob, sdl_cdrom_tag, cd);
  return cd_smob;
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_status, "sdl-cd-status", 1, 0, 0,
            (SCM cd_smob),
"Returns the current status of the given drive.")
#define FUNC_NAME s_sdl_cd_status
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_in_drive_p, "sdl-cd-in-drive?", 1, 0, 0,
            (SCM cd_smob),
"Returns #t if there is a CD in the drive, #f otherwise.")
#define FUNC_NAME s_sdl_cd_in_drive_p
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_get_num_tracks, "sdl-cd-get-num-tracks", 1, 0, 0,
            (SCM cd_smob),
"Returns the number of tracks on the CD.")
#define FUNC_NAME s_sdl_cd_get_num_tracks
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_get_cur_track, "sdl-cd-get-cur-track", 1, 0, 0,
            (SCM cd_smob),
"Returns the current track on the CD.")
#define FUNC_NAME s_sdl_cd_get_cur_track
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_get_cur_frame, "sdl-cd-get-cur-frame", 1, 0, 0,
            (SCM cd_smob),
"Returns the current frame of the CD.")
#define FUNC_NAME s_sdl_cd_get_cur_frame
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_get_nth_track, "sdl-cd-get-nth-track", 1, 1, 0,
            (SCM cd_smob,
             SCM s_n),
"Return the info for the given track as an alist.")
#define FUNC_NAME s_sdl_cd_get_nth_track
{
  SDL_CD *cd;
  int n=0;
  SCM s_track;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1, "sdl-cd-get-nth-track");
  cd = (SDL_CD *) SCM_CDR (cd_smob);

  if (s_n != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_n), s_n, SCM_ARG2, "sdl-cd-get-nth-track");
    n = scm_num2ulong (s_n, SCM_ARG2, "sdl-cd-get-nth-track");
  }

  if ((cd != NULL) && (n < cd->numtracks)) {

    /* Form an assoc list */
    s_track = scm_list_n (NULL);
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
    return (scm_list_n (NULL));
  }
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_play_tracks, "sdl-cd-play-tracks", 1, 4, 0,
            (SCM cd_smob,
             SCM s_start_track,
             SCM s_start_frame,
             SCM s_n_tracks,
             SCM s_n_frames),
"Play the given CD tracks.")
#define FUNC_NAME s_sdl_cd_play_tracks
{
  SDL_CD *cd;
  int start_track=0, start_frame=0, n_tracks=1, n_frames=1;
  int ret;

  SCM_ASSERT (SMOB_CDROMP (cd_smob), cd_smob,  SCM_ARG1,
              "sdl-cd-play-tracks");

  if (s_start_track != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_start_track), s_start_track, SCM_ARG2, 
                "sdl-cd-play-tracks");
    start_track = scm_num2ulong (s_start_track, SCM_ARG2,
                                 "sdl-cd-play-tracks");
  }

  if (s_start_frame != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_start_frame), s_start_frame, SCM_ARG3,
                "sdl-cd-play-tracks");
    start_frame = scm_num2ulong (s_start_frame, SCM_ARG3,
                                 "sdl-cd-play-tracks");
  }

  if (s_n_tracks != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_n_tracks), s_n_tracks, SCM_ARG4,
                "sdl-cd-play-tracks");
    n_tracks = scm_num2ulong (s_n_tracks, SCM_ARG4, "sdl-cd-play-tracks");;
  }

  if (s_n_frames != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_n_frames), s_n_frames, SCM_ARG5,
                "sdl-cd-play-tracks");
    n_frames = scm_num2ulong (s_n_frames, SCM_ARG5, "sdl-cd-play-tracks");
  } else {
    n_frames = cd->track[start_track+n_tracks-1].length;
  }
  
  cd = (SDL_CD *) SCM_CDR (cd_smob);
  
  if (cd != NULL) {
    ret = SDL_CDPlayTracks (cd, start_track, start_frame, n_tracks, n_frames);
  }
  else {
    ret = -1;
  }
  
  return scm_long2num (ret);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_play, "sdl-cd-play", 3, 0, 0,
            (SCM cd_smob,
             SCM s_start,
             SCM s_length),
"Play a CD.")
#define FUNC_NAME s_sdl_cd_play
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_pause, "sdl-cd-pause", 1, 0, 0,
            (SCM cd_smob),
"Pause a CD.")
#define FUNC_NAME s_sdl_cd_pause
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_resume, "sdl-cd-resume", 1, 0, 0,
            (SCM cd_smob),
"Resume (unpause) a CD.")
#define FUNC_NAME s_sdl_cd_resume
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_stop, "sdl-cd-stop", 1, 0, 0,
            (SCM cd_smob),
"Stop a CD.")
#define FUNC_NAME s_sdl_cd_stop
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_eject, "sdl-cd-eject", 1, 0, 0,
            (SCM cd_smob),
"Eject a CD.")
#define FUNC_NAME s_sdl_cd_eject
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_close, "sdl-cd-close", 1, 0, 0,
            (SCM cd_smob),
"Close a CD handle.")
#define FUNC_NAME s_sdl_cd_close
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
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_msf_to_frames, "sdl-cd-msf->frames", 1, 2, 0,
            (SCM s_m,
             SCM s_s,
             SCM s_f),
"Convert Minute/Second/Frames to number of frames.")
#define FUNC_NAME s_sdl_cd_msf_to_frames
{
  int frames;
  int m, s=0, f=0;

  SCM_ASSERT (scm_exact_p (s_m), s_m, SCM_ARG1, 
	      "sdl-cd-msf->frames");
  m = scm_num2ulong(s_m, SCM_ARG1, "sdl-cd-msf->frames");

  if (s_s != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_s), s_s, SCM_ARG2, "sdl-cd-msf->frames");
    s = scm_num2ulong(s_s, SCM_ARG2, "sdl-cd-msf->frames");
  }

  if (s_f != SCM_UNDEFINED) {
    SCM_ASSERT (scm_exact_p (s_f), s_f, SCM_ARG3, "sdl-cd-msf->frames");
    f = scm_num2ulong(s_f, SCM_ARG3, "sdl-cd-msf->frames");
  }
  
  frames = MSF_TO_FRAMES(m, s, f);
  return scm_long2num (frames);
}
#undef FUNC_NAME


SCM_DEFINE( sdl_cd_frames_to_msf, "sdl-cd-frames->msf", 1, 0, 0,
            (SCM s_frames),
"Convert number of frames to a Minute/Second/Frames alist.")
#define FUNC_NAME s_sdl_cd_frames_to_msf
{
  int frames, m, s, f;
  SCM s_msf;
  
  SCM_ASSERT (scm_exact_p (s_frames), s_frames, SCM_ARG1,
	      "sdl-cd-frames->msf");
  frames = scm_num2ulong (s_frames, SCM_ARG1, "sdl-cd-frames->msf");

  FRAMES_TO_MSF (frames, &m , &s, &f);
  s_msf = scm_list_n (NULL);
  
  s_msf = scm_acons (scm_str2symbol ("f"), scm_ulong2num (f) , s_msf);
  s_msf = scm_acons (scm_str2symbol ("s"), scm_ulong2num (s) , s_msf);
  s_msf = scm_acons (scm_str2symbol ("m"), scm_ulong2num (m) , s_msf);
  
  return s_msf;
}
#undef FUNC_NAME

/*-------------------------------------------------------------*/


size_t 
free_cd (SCM cd_smob) 
{
  SDL_CD *cd = (SDL_CD *) SCM_SMOB_DATA (cd_smob);
  size_t size = sizeof(SDL_CD);

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
  
/*   /\* Check for NULL drive object *\/ */
/*   scm_c_define_gsubr ("sdl-cd-null?", 1, 0, 0, sdl_cd_null_p);   */
   
/*   /\* Register Scheme functions *\/ */
/*   scm_c_define_gsubr ("sdl-cd-num-drives", 0, 0, 0, sdl_cd_num_drives); */
/*   scm_c_define_gsubr ("sdl-cd-name", 1, 0, 0, sdl_cd_name); */

/*   scm_c_define_gsubr ("sdl-cd-open", 1, 0, 0, sdl_cd_open); */

/*   scm_c_define_gsubr ("sdl-cd-status", 1, 0, 0, sdl_cd_status); */
/*   scm_c_define_gsubr ("sdl-cd-in-drive?", 1, 0, 0, sdl_cd_in_drive_p); */

/*   /\* Next 4 Should be called after calling sdl-cd-status or sdl-cd-in-drive? *\/ */
/*   scm_c_define_gsubr ("sdl-cd-get-num-tracks", 1, 0, 0, sdl_cd_get_num_tracks); */
/*   scm_c_define_gsubr ("sdl-cd-get-cur-track", 1, 0, 0, sdl_cd_get_cur_track); */
/*   scm_c_define_gsubr ("sdl-cd-get-cur-frame", 1, 0, 0, sdl_cd_get_cur_frame); */
/*   scm_c_define_gsubr ("sdl-cd-get-nth-track", 2, 0, 0, sdl_cd_get_nth_track); */

/*   /\* Play Pause Stop Eject *\/ */
/*   scm_c_define_gsubr ("sdl-cd-play-tracks", 5, 0, 0, sdl_cd_play_tracks); */
/*   scm_c_define_gsubr ("sdl-cd-play", 3, 0, 0, sdl_cd_play); */
/*   scm_c_define_gsubr ("sdl-cd-pause", 1, 0, 0, sdl_cd_pause); */
/*   scm_c_define_gsubr ("sdl-cd-resume", 1, 0, 0, sdl_cd_resume); */
/*   scm_c_define_gsubr ("sdl-cd-stop", 1, 0, 0, sdl_cd_stop); */
/*   scm_c_define_gsubr ("sdl-cd-eject", 1, 0, 0, sdl_cd_eject); */
  
/*   scm_c_define_gsubr ("sdl-cd-close", 1, 0, 0, sdl_cd_close); */

/*   scm_c_define_gsubr ("sdl-cd-msf-to-frames", 3, 0, 0, sdl_cd_msf_to_frames); */
/*   scm_c_define_gsubr ("sdl-cd-frames-to-msf", 1, 0, 0, sdl_cd_frames_to_msf); */

  /* exported symbols */
  scm_c_export ("sdl-cd?",
                "sdl-cd-null?",          "sdl-cd-num-drives",
                "sdl-cd-name",           "sdl-cd-open",
                "sdl-cd-status",         "sdl-cd-in-drive?",
                "sdl-cd-get-num-tracks", "sdl-cd-get-cur-track",
                "sdl-cd-get-cur-frame",  "sdl-cd-get-nth-track",
                "sdl-cd-play-tracks",    "sdl-cd-play",
                "sdl-cd-pause",          "sdl-cd-resume",
                "sdl-cd-stop",           "sdl-cd-eject",
                "sdl-cd-close",          "sdl-cd-msf->frames",
                "sdl-cd-frames->msf",    NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlcdrom.x"
#endif

}

