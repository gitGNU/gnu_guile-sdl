/* sdlcdrom.c --- SDL CDROM functions for Guile
 *
 * 	Copyright (C) 2003,2004 Thien-Thi Nguyen
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
            "Return #t iff @var{obj} is a CDROM drive object.")
{
#define FUNC_NAME s_cd_p
  RETURN_BOOL
    (CDROM_P (obj));
#undef FUNC_NAME
}


GH_DEFPROC (cd_null_p, "cd-null?", 1, 0, 0,
            (SCM cdrom),
            "Return #t iff @var{cdrom} is a null pointer.\n"
            "[What does that mean? --ttn]")
{
#define FUNC_NAME s_cd_null_p
  ASSERT_CDROM (cdrom, ARGH1);

  RETURN_BOOL
    (! UNPACK_CDROM (cdrom));
#undef FUNC_NAME
}


GH_DEFPROC (cd_num_drives, "cd-num-drives", 0, 0, 0,
            (),
            "Return the number of CDROM drives.")
{
#define FUNC_NAME s_cd_num_drives
  RETURN_INT (SDL_CDNumDrives ());
#undef FUNC_NAME
}


GH_DEFPROC (cd_name, "cd-name", 0, 1, 0,
            (SCM drive),
            "Return a human-readable, system-dependent\n"
            "identifier (a string) for the CDROM.\n"
            "Optional arg @var{drive} is a number specifying which drive.")
{
#define FUNC_NAME s_cd_name
  const char *name;
  int cdrive = 0;

  if (BOUNDP (drive))
    {
      ASSERT_EXACT (drive, ARGH1);
      cdrive = gh_scm2int (drive);
    }

  name = SDL_CDName (cdrive);
  RETURN_0STR (name);
#undef FUNC_NAME
}

GH_DEFPROC (cd_open, "cd-open", 0, 1, 0,
            (SCM drive),
            "Open the CDROM drive for access and return its handle.\n"
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
            (SCM cdrom),
            "Return the current status of the drive @var{cdrom}\n"
            "as a symbol, one of: @code{TRAYEMTPY}, @code{STOPPED},\n"
            "@code{PLAYING}, @code{PAUSED} or @code{ERROR}.")
{
#define FUNC_NAME s_cd_status
  SDL_CD *cd;
  int ret = CD_ERROR;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

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
            (SCM cdrom),
            "Return #t iff there is a CD in drive @var{cdrom}.")
{
#define FUNC_NAME s_cd_in_drive_p
  SDL_CD *cd;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    RETURN_BOOL (CD_INDRIVE (SDL_CDStatus (cd)));
  else
    RETURN_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_num_tracks, "cd-get-num-tracks", 1, 0, 0,
            (SCM cdrom),
            "Return the number of tracks on the CD in drive @var{cdrom}.")
{
#define FUNC_NAME s_cd_get_num_tracks
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = cd->numtracks;

  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_cur_track, "cd-get-cur-track", 1, 0, 0,
            (SCM cdrom),
            "Return the current track on the CD in drive @var{cdrom}.")
{
#define FUNC_NAME s_cd_get_cur_track
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = cd->cur_track;

  RETURN_INT (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_get_cur_frame, "cd-get-cur-frame", 1, 0, 0,
            (SCM cdrom),
            "Return the current frame of the CD in drive @var{cdrom}.")
{
#define FUNC_NAME s_cd_get_cur_frame
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

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
            (SCM cdrom, SCM n),
            "For CD in drive @var{cdrom}, return info on track @var{n}\n"
            "as an alist or #f if there were problems.")
{
#define FUNC_NAME s_cd_get_nth_track
  SDL_CD *cd;
  int cn = 0;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (BOUNDP (n))
    {
      ASSERT_EXACT (n, ARGH2);
      cn = gh_scm2ulong (n);
    }

  if (cd && (cn < cd->numtracks))
    /* Form an assoc list.  */
    RETURN_LIST4
      (gh_cons (SYM (id),     gh_long2scm (cd->track[cn].id)),
       gh_cons (SYM (type),   gh_long2scm (cd->track[cn].type)),
       gh_cons (SYM (length), gh_ulong2scm (cd->track[cn].length)),
       gh_cons (SYM (offset), gh_ulong2scm (cd->track[cn].offset)));
  else
    RETURN_FALSE;
#undef FUNC_NAME
}


GH_DEFPROC (cd_play_tracks, "cd-play-tracks", 1, 4, 0,
            (SCM cdrom,
             SCM start_track,
             SCM start_frame,
             SCM n_tracks,
             SCM n_frames),
            "Play the given CD tracks in drive @var{cdrom}.\n"
            "Play the CD starting at @var{start-track} and\n"
            "@var{start-frame} for @var{ntracks} tracks and @var{nframes}\n"
            "frames.  If both @var{ntrack} and @var{nframe} are 0, play\n"
            "until the end of the CD.  This procedure will skip data\n"
            "tracks, and should only be called after calling\n"
            "@code{cd-status} to get track information about the CD.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_cd_play_tracks
  SDL_CD *cd;
  int cstart_track = 0, cstart_frame = 0, cn_tracks = 1, cn_frames = 1;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);

  UNBOUND_MEANS_FALSE (start_track);
  if (NOT_FALSEP (start_track))
    {
      ASSERT_EXACT (start_track, ARGH2);
      cstart_track = gh_scm2ulong (start_track);
    }

  UNBOUND_MEANS_FALSE (start_frame);
  if (NOT_FALSEP (start_frame))
    {
      ASSERT_EXACT (start_frame, ARGH3);
      cstart_frame = gh_scm2ulong (start_frame);
    }

  UNBOUND_MEANS_FALSE (n_tracks);
  if (NOT_FALSEP (n_tracks))
    {
      ASSERT_EXACT (n_tracks, ARGH4);
      cn_tracks = gh_scm2ulong (n_tracks);;
    }

  UNBOUND_MEANS_FALSE (n_frames);
  if (NOT_FALSEP (n_frames))
    {
      ASSERT_EXACT (n_frames, ARGH5);
      cn_frames = gh_scm2ulong (n_frames);
    }
  else
    {
      cd = UNPACK_CDROM (cdrom);
      cn_frames = cd->track[cstart_track + cn_tracks - 1].length;
    }

  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDPlayTracks (cd, cstart_track, cstart_frame, cn_tracks, cn_frames);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_play, "cd-play", 3, 0, 0,
            (SCM cdrom,
             SCM start,
             SCM length),
            "Play CD in drive @var{cdrom} from @var{start} frame for\n"
            "@var{length} frames.  Return #t if successful.")
{
#define FUNC_NAME s_cd_play
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  ASSERT_EXACT (start, ARGH2);
  ASSERT_EXACT (length, ARGH3);

  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDPlay (cd,
                      gh_scm2ulong (start),
                      gh_scm2ulong (length));

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_pause, "cd-pause", 1, 0, 0,
            (SCM cdrom),
            "Pause the CD in drive @var{cdrom}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_pause
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDPause (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_resume, "cd-resume", 1, 0, 0,
            (SCM cdrom),
            "Resume (unpause) the CD in drive @var{cdrom}.\n"
            "Return #t if successful.")
{
#define FUNC_NAME s_cd_resume
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDResume (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_stop, "cd-stop", 1, 0, 0,
            (SCM cdrom),
            "Stop the CD in drive @var{cdrom}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_stop
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDStop (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_eject, "cd-eject", 1, 0, 0,
            (SCM cdrom),
            "Eject the CD from drive @var{cdrom}.  Return #t if successful.")
{
#define FUNC_NAME s_cd_eject
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDEject (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


GH_DEFPROC (cd_close, "cd-close", 1, 0, 0,
            (SCM cdrom),
            "Close the drive @var{cdrom}.  The return value is unspecified.")
{
#define FUNC_NAME s_cd_close
  SDL_CD *cd;

  ASSERT_CDROM (cdrom, ARGH1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    {
      SDL_CDClose (cd);
      SMOBSET (cdrom, NULL);
    }

  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC (cd_msf_to_frames, "cd-msf->frames", 1, 2, 0,
            (SCM m,
             SCM s,
             SCM f),
            "Return frames (an integer) computed from"
            "@var{m}, second @var{s} and frame @var{f}.\n"
            "@var{s} and @var{f} are optional.")
{
#define FUNC_NAME s_cd_msf_to_frames
  int frames;
  int cm, cs = 0, cf = 0;

  ASSERT_EXACT (m, ARGH1);
  cm = gh_scm2ulong (m);

  UNBOUND_MEANS_FALSE (s);
  if (NOT_FALSEP (s))
    {
      ASSERT_EXACT (s, ARGH2);
      cs = gh_scm2ulong (s);
    }

  UNBOUND_MEANS_FALSE (f);
  if (NOT_FALSEP (f))
    {
      ASSERT_EXACT (f, ARGH3);
      cf = gh_scm2ulong (f);
    }

  frames = MSF_TO_FRAMES (cm, cs, cf);
  RETURN_INT (frames);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (f);
DECLARE_SIMPLE_SYM (s);
DECLARE_SIMPLE_SYM (m);

GH_DEFPROC (cd_frames_to_msf, "cd-frames->msf", 1, 0, 0,
            (SCM frames),
            "Return a minute/second/frames alist made from\n"
            "converting @var{frames} (a number).")
{
#define FUNC_NAME s_cd_frames_to_msf
  int cframes, m, s, f;

  ASSERT_EXACT (frames, ARGH1);
  cframes = gh_scm2ulong (frames);

  FRAMES_TO_MSF (cframes, &m , &s, &f);
  RETURN_LIST3 (gh_cons (SYM (m), gh_ulong2scm (m)),
                gh_cons (SYM (s), gh_ulong2scm (s)),
                gh_cons (SYM (f), gh_ulong2scm (f)));
#undef FUNC_NAME
}

/*-------------------------------------------------------------*/

static
SCM
mark_cd (SCM cdrom)
{
  return cdrom;
}

static
size_t
free_cd (SCM cdrom)
{
  SDL_CD *cd = UNPACK_CDROM (cdrom);

  if (cd)
    SDL_CDClose (cd);

  return 0;
}

static
int
print_cd (SCM cdrom, SCM port, scm_print_state *pstate)
{
  SDL_CD *cd = UNPACK_CDROM (cdrom);

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
