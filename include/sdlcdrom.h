#ifndef __SDLCDROM_H
#define __SDLCDROM_H

extern long sdl_cdrom_tag;

#define SMOB_CDROMP(x) (SCM_NIMP (x)\
		       && (long) SCM_CAR (x) == sdl_cdrom_tag)

void sdl_init_cdrom ();

/*---------------------SMOB functions--------------------------*/ 
size_t free_cd (SCM cd_smob);
int print_cd (SCM cd_smob, SCM port, scm_print_state *pstate);

/*--------------------Scheme functions-------------------------*/ 

SCM sdl_cd_null_p (SCM cd_smob);

SCM sdl_cd_num_drives (void);
SCM sdl_cd_name (SCM s_drive);
SCM sdl_cd_open (SCM s_drive);

SCM sdl_cd_status (SCM cd_smob);
SCM sdl_cd_in_drive_p (SCM cd_smob);

SCM sdl_cd_get_num_tracks (SCM cd_smob);
SCM sdl_cd_get_cur_track  (SCM cd_smob);
SCM sdl_cd_get_cur_frame  (SCM cd_smob);

/* Return an assoc list with details of the n-th track */
SCM sdl_cd_get_nth_track  (SCM cd_smob, SCM s_n);

/* Play Pause Stop Eject*/
SCM sdl_cd_play_tracks (SCM cd_smob, 
                        SCM s_start_track, SCM s_start_frame, 
                        SCM s_n_tracks, SCM s_n_frames);

SCM sdl_cd_play   (SCM cd_smob, SCM s_start, SCM s_length);
SCM sdl_cd_pause  (SCM cd_smob);
SCM sdl_cd_resume (SCM cd_smob);
SCM sdl_cd_stop   (SCM cd_smob);
SCM sdl_cd_eject  (SCM cd_smob);

SCM sdl_cd_close  (SCM cd_smob);

SCM sdl_cd_msf_to_frames  (SCM s_m, SCM s_s, SCM s_f);

/* Return an assoc list */ 
SCM sdl_cd_frames_to_msf  (SCM s_frames);

/*-------------------------------------------------------------*/

#endif
