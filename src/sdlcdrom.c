/* sdlcdrom.c --- SDL CDROM functions for Guile
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
 * 	Copyright (C) 2001 Alex Shinn
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#include <guile/gh.h>
#include <SDL/SDL.h>

#include "config.h"
#include "argcheck.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "sym.h"
#include "bool.h"


static long cdrom_tag;

#define ASSERT_CDROM(obj,which) \
  ASSERT_SMOB (obj, cdrom_tag, which)

#define UNPACK_CDROM(smob) \
  (SMOBGET (smob, SDL_CD *))

#define RETURN_NEW_CDROM(x) \
  SCM_RETURN_NEWSMOB (cdrom_tag, x)

#define CDROM_P(x) \
  (SCM_SMOB_PREDICATE (cdrom_tag, x))



GH_DEFPROC (cd_p, "cd?", 1, 0, 0,
            (SCM obj),
            "Return #t iff @var{obj} is a cd smob.")
{
#define FUNC_NAME s_cd_p
  RETURN_BOOL
    (CDROM_P (obj));
#undef FUNC_NAME
}


GH_DEFPROC (cd_null_p, "cd-null?", 1, 0, 0,
            (SCM cd_smob),
            "Return #t iff @var{cd} is a null pointer.")
{
#define FUNC_NAME s_cd_null_p
  ASSERT_CDROM (cd_smob, ARGH1);

  RETURN_BOOL
    (! UNPACK_CDROM (cd_smob));
#undef FUNC_NAME
}


GH_DEFPROC (cd_num_drives, "cd-num-drives", 0, 0, 0,
            (),
            "Return the number of CD drives.")
{
#define FUNC_NAME s_cd_num_drives
  RETURN_INT (SDL_CDNumDrives ());
#undef FUNC_NAME
}


GH_DEFPROC (cd_name, "cd-name", 0, 1, 0,
            (SCM s_drive),
            "Return a human-readable, system-dependent\n"
            "identifier (a string) for the CD-ROM.\n"
            "Optional arg @var{drive} is a number specifying which drive.")
{
#define FUNC_NAME s_cd_name
  const char *name;
  int drive = 0;

  if (BOUNDP (s_drive))
    {
      ASSERT_EXACT (s_drive, ARGH1);
      drive = gh_scm2int (s_drive);
    }

  name = SDL_CDName (drive);
  RETURN_0STR (name);
#undef FUNC_NAME
}

GH_DEFPROC (cd_open, "cd-open", 0, 1, 0,
            (SCM drive),
            "Open the CD-ROM drive for access and return its handle.\n"
            "If the drive is unavailable, return #f.\n"
            "Optional arg @var{drive} is a number specifying which drive.")
{
#define FUNC_NAME s_cd_open
  SDL_CD *cd;
  int cdrive = 0;

  if (BOUNDP (drive))
    {
      ASSERT_EXACT (drive, ARGH1);
      cdrive = gh_scm2int (drive);
    }

  cd = SDL_CDOpen (cdrive);
  if (! cd)
    RETURN_FALSE;
  else
    RETURN_NEW_CDROM (cd);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (TRAYEMPTY);
DECLARE_SIMPLE_SYM (STOPPED);
DECLARE_SIMPLE_SYM (PLAYING);
DECLARE_SIMPLE_SYM (PAUSED);
DECLARE_SIMPLE_SYM (ERROR);

GH_DEFPROC (cd_status, "cd-status", 1, 0, 0,
            (SCM cd_smob),
            "Return the current status of the drive @var{cd}\n"
            "as a symbol, one of: @code{TRAYEMTPY}, @code{STOPPED},\n"
            "@code{PLAYING}, @code{PAUSED} or @code{ERROR}.")
{
#define FUNC_NAME s_cd_status
  SDL_CD *cd;
  int ret = CD_ERROR;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDStatus (cd);

  switch (ret)
    {
    case CD_TRAYEMPTY: return SYM (TRAYEMPTY);
    case CD_STOPPED:   return SYM (STOPPED);
    case CD_PLAYING:   return SYM (PLAYING);
    case CD_PAUSED:    return SYM (PAUSED);
    default:           return SYM (ERROR);
    }
#undef FUNC_NAME
}


GH_DEFPROC (cd_in_drive_p, "cd-in-drive?", 1, 0, 0,
            (SCM cd_smob),
            "Return #t iff there is a CD in drive @var{cd}.")
{
#define FUNC_NAME s_cd_in_drive_p
  SDL_CD *cd;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    RETURN_BOOL (CD_INDRIVE (SDL_CDStatus (cd)));
  else
    RETURN_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_num_tracks, "cd-get-num-tracks", 1, 0, 0,
            (SCM cd_smob),
            "Return the number of tracks on the @var{cd}.")
{
#define FUNC_NAME s_cd_get_num_tracks
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = cd->numtracks;

  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_cur_track, "cd-get-cur-track", 1, 0, 0,
            (SCM cd_smob),
            "Return the current track on the @var{cd}.")
{
#define FUNC_NAME s_cd_get_cur_track
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = cd->cur_track;

  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_cur_frame, "cd-get-cur-frame", 1, 0, 0,
            (SCM cd_smob),
            "Return the current frame of the @var{cd}.")
{
#define FUNC_NAME s_cd_get_cur_frame
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = cd->cur_frame;

  RETURN_INT (ret);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (offset);
DECLARE_SIMPLE_SYM (length);
DECLARE_SIMPLE_SYM (type);
DECLARE_SIMPLE_SYM (id);

GH_DEFPROC (cd_get_nth_track, "cd-get-nth-track", 1, 1, 0,
            (SCM cd_smob, SCM s_n),
            "Return info for @var{cd} track @var{n} as an alist\n"
            "or #f if there were problems.")
{
#define FUNC_NAME s_cd_get_nth_track
  SDL_CD *cd;
  int n = 0;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (BOUNDP (s_n))
    {
      ASSERT_EXACT (s_n, ARGH2);
      n = gh_scm2ulong (s_n);
    }

  if (cd && (n < cd->numtracks))
    /* Form an assoc list.  */
    RETURN_LIST4
      (gh_cons (SYM (id),     gh_long2scm (cd->track[n].id)),
       gh_cons (SYM (type),   gh_long2scm (cd->track[n].type)),
       gh_cons (SYM (length), gh_ulong2scm (cd->track[n].length)),
       gh_cons (SYM (offset), gh_ulong2scm (cd->track[n].offset)));
  else
    RETURN_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (cd_play_tracks, "cd-play-tracks", 1, 4, 0,
            (SCM cd_smob,
             SCM s_start_track,
             SCM s_start_frame,
             SCM s_n_tracks,
             SCM s_n_frames),
            "Play the given CD tracks."
            "Play the @var{cd} starting at @var{start-track} and\n"
            "@var{start-frame} for @var{ntracks} tracks and @var{nframes}\n"
            "frames.  If both @var{ntrack} and @var{nframe} are 0, play\n"
            "until the end of the CD.  This procedure will skip data\n"
            "tracks, and should only be called after calling\n"
            "@code{cd-status} to get track information about the CD.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_cd_play_tracks
  SDL_CD *cd;
  int start_track = 0, start_frame = 0, n_tracks = 1, n_frames = 1;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);

  UNBOUND_MEANS_FALSE (s_start_track);
  if (NOT_FALSEP (s_start_track))
    {
      ASSERT_EXACT (s_start_track, ARGH2);
      start_track = gh_scm2ulong (s_start_track);
    }

  UNBOUND_MEANS_FALSE (s_start_frame);
  if (NOT_FALSEP (s_start_frame))
    {
      ASSERT_EXACT (s_start_frame, ARGH3);
      start_frame = gh_scm2ulong (s_start_frame);
    }

  UNBOUND_MEANS_FALSE (s_n_tracks);
  if (NOT_FALSEP (s_n_tracks))
    {
      ASSERT_EXACT (s_n_tracks, ARGH4);
      n_tracks = gh_scm2ulong (s_n_tracks);;
    }

  UNBOUND_MEANS_FALSE (s_n_frames);
  if (NOT_FALSEP (s_n_frames))
    {
      ASSERT_EXACT (s_n_frames, ARGH5);
      n_frames = gh_scm2ulong (s_n_frames);
    }
  else
    {
      cd = UNPACK_CDROM (cd_smob);
      n_frames = cd->track[start_track + n_tracks - 1].length;
    }

  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDPlayTracks (cd, start_track, start_frame, n_tracks, n_frames);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_play, "cd-play", 3, 0, 0,
            (SCM cd_smob,
             SCM s_start,
             SCM s_length),
            "Play a @var{cd} from @var{start} frame for\n"
            "@var{length} frames.  Return #t if successful.")
{
#define FUNC_NAME s_cd_play
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  ASSERT_EXACT (s_start, ARGH2);
  ASSERT_EXACT (s_length, ARGH3);

  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDPlay (cd,
                      gh_scm2ulong (s_start),
                      gh_scm2ulong (s_length));

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_pause, "cd-pause", 1, 0, 0,
            (SCM cd_smob),
            "Pause a @var{cd}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_pause
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDPause (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_resume, "cd-resume", 1, 0, 0,
            (SCM cd_smob),
            "Resume (unpause) a @var{cd}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_resume
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDResume (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_stop, "cd-stop", 1, 0, 0,
            (SCM cd_smob),
            "Stop a @var{cd}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_stop
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDStop (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_eject, "cd-eject", 1, 0, 0,
            (SCM cd_smob),
            "Eject a @var{cd}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_eject
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    ret = SDL_CDEject (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_close, "cd-close", 1, 0, 0,
            (SCM cd_smob),
            "Close a @var{cd}.  The return value is unspecified.")
{
#define FUNC_NAME s_cd_close
  SDL_CD *cd;

  ASSERT_CDROM (cd_smob, ARGH1);
  cd = UNPACK_CDROM (cd_smob);

  if (cd)
    {
      SDL_CDClose (cd);
      SMOBSET (cd_smob, NULL);
    }

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (cd_msf_to_frames, "cd-msf->frames", 1, 2, 0,
            (SCM s_m,
             SCM s_s,
             SCM s_f),
            "Return frames (an integer) computed from"
            "@var{m}, second @var{s} and frame @var{f}.\n"
            "@var{s} and @var{f} are optional.")
{
#define FUNC_NAME s_cd_msf_to_frames
  int frames;
  int m, s = 0, f = 0;

  ASSERT_EXACT (s_m, ARGH1);
  m = gh_scm2ulong (s_m);

  UNBOUND_MEANS_FALSE (s_s);
  if (NOT_FALSEP (s_s))
    {
      ASSERT_EXACT (s_s, ARGH2);
      s = gh_scm2ulong (s_s);
    }

  UNBOUND_MEANS_FALSE (s_f);
  if (NOT_FALSEP (s_f))
    {
      ASSERT_EXACT (s_f, ARGH3);
      f = gh_scm2ulong (s_f);
    }

  frames = MSF_TO_FRAMES (m, s, f);
  RETURN_INT (frames);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (f);
DECLARE_SIMPLE_SYM (s);
DECLARE_SIMPLE_SYM (m);

GH_DEFPROC (cd_frames_to_msf, "cd-frames->msf", 1, 0, 0,
            (SCM s_frames),
            "Return a minute/second/frames alist made from\n"
            "converting @var{frames} (a number).")
{
#define FUNC_NAME s_cd_frames_to_msf
  int frames, m, s, f;

  ASSERT_EXACT (s_frames, ARGH1);
  frames = gh_scm2ulong (s_frames);

  FRAMES_TO_MSF (frames, &m , &s, &f);
  RETURN_LIST3 (gh_cons (SYM (m), gh_ulong2scm (m)),
                gh_cons (SYM (s), gh_ulong2scm (s)),
                gh_cons (SYM (f), gh_ulong2scm (f)));
#undef FUNC_NAME
}

/*-------------------------------------------------------------*/

static
SCM
mark_cd (SCM cd_smob)
{
  return cd_smob;
}

static
size_t
free_cd (SCM cd_smob)
{
  SDL_CD *cd = UNPACK_CDROM (cd_smob);

  if (cd)
    SDL_CDClose (cd);

  return 0;
}

static
int
print_cd (SCM cd_smob, SCM port, scm_print_state *pstate)
{
  SDL_CD *cd = UNPACK_CDROM (cd_smob);

  /* Print the current status.  */
  if (cd)
    {
      scm_puts ("#<SDL-CD ", port);

      switch (cd->status)
        {
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
  else
    scm_puts ("#<SDL-CD NULL>", port);

  /* Non-zero means success.  */
  return 1;
}


void
gsdl_init_cdrom (void)
{
  cdrom_tag = scm_make_smob_type ("SDL-CD", sizeof (SDL_CD));
  scm_set_smob_mark  (cdrom_tag, mark_cd);
  scm_set_smob_free  (cdrom_tag, free_cd);
  scm_set_smob_print (cdrom_tag, print_cd);

#include "sdlcdrom.x"
}

/* sdlcdrom.c ends here */
