/* sdlcdrom.c --- SDL CDROM functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009, 2011, 2013 Thien-Thi Nguyen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA  02110-1301  USA
 */

#include "guile-sdl.h"
#include <stdio.h>
#include "b-values.h"

static SCM cdrom_state;
#include "k/cdstate.c"

static SCM cd_track_type;
#include "k/cdtracktype.c"

static smob_tag_t cdrom_tag;

#define cdrom_nick "SDL-CD"

#define ASSERT_CDROM(obj,which) \
  ASSERT_SMOB (obj, cdrom, which)

#define UNPACK_CDROM(smob) \
  (SMOBGET (smob, SDL_CD *))

static SDL_CD *
assert_open_cdrom (const char *FUNC_NAME, SCM obj, int which)
{
  SDL_CD *cd;

  ASSERT_CDROM (obj, which);
  if (! (cd = UNPACK_CDROM (obj)))
    SCM_MISC_ERROR ("cdrom not open", SCM_EOL);

  return cd;
}

#define ASSERT_FIRST_ARG_OPEN_CDROM()                   \
  SDL_CD *cd = assert_open_cdrom (FUNC_NAME, cdrom, 1)

#define RETURN_NEW_CDROM(x) \
  NEWSMOB_OR_FALSE (cdrom_tag, x)

#define CDROM_P(x) \
  (SCM_SMOB_PREDICATE (cdrom_tag, x))



PRIMPROC
(cd_p, "cd?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return @code{#t} iff @var{obj} is a CDROM drive object.  */)
{
#define FUNC_NAME s_cd_p
  RETURN_BOOL
    (CDROM_P (obj));
#undef FUNC_NAME
}


PRIMPROC
(cd_num_drives, "cd-num-drives", 0, 0, 0,
 (),
 doc: /***********
Return the number of CDROM drives.  */)
{
#define FUNC_NAME s_cd_num_drives
  RETURN_INT (SDL_CDNumDrives ());
#undef FUNC_NAME
}


PRIMPROC
(cd_name, "cd-name", 0, 1, 0,
 (SCM drive),
 doc: /***********
Return a human-readable, system-dependent
identifier (a string) for the CDROM, or @code{#f}.
Optional arg @var{drive} is a number specifying which drive.  */)
{
#define FUNC_NAME s_cd_name
  int cdrive = 0;

  if (BOUNDP (drive))
    ASSERT_INT_COPY (drive, 1);

  RETURN_0STR_OR_FALSE (SDL_CDName (cdrive));
#undef FUNC_NAME
}

PRIMPROC
(cd_open, "cd-open", 0, 1, 0,
 (SCM drive),
 doc: /***********
Open the CDROM drive for access and return its handle.
If the drive is unavailable, return @code{#f}.
Optional arg @var{drive} is a number specifying which drive.  */)
{
#define FUNC_NAME s_cd_open
  int cdrive = 0;

  if (BOUNDP (drive))
    ASSERT_INT_COPY (drive, 1);

  RETURN_NEW_CDROM (SDL_CDOpen (cdrive));
#undef FUNC_NAME
}


PRIMPROC
(cd_status, "cd-status", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return the current status of the drive @var{cdrom}
as a symbol (@pxref{cdrom-state enums}).  */)
{
#define FUNC_NAME s_cd_status
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  return btw->long2enum (SDL_CDStatus (cd), cdrom_state);
#undef FUNC_NAME
}


PRIMPROC
(cd_in_drive_p, "cd-in-drive?", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return @code{#t} iff there is a CD in drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_in_drive_p
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  RETURN_BOOL (CD_INDRIVE (SDL_CDStatus (cd)));
#undef FUNC_NAME
}


#define GETCUR(field)                           \
  do {                                          \
    ASSERT_FIRST_ARG_OPEN_CDROM ();             \
    int ret = -1;                               \
                                                \
    if (CD_TRAYEMPTY != SDL_CDStatus (cd))      \
      ret = cd->field;                          \
    RETURN_INT (ret);                           \
  } while (0)

PRIMPROC
(cd_get_num_tracks, "cd-get-num-tracks", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return the number of tracks on the CD in drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_get_num_tracks
  GETCUR (numtracks);
#undef FUNC_NAME
}

PRIMPROC
(cd_get_cur_track, "cd-get-cur-track", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return the current track on the CD in drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_get_cur_track
  GETCUR (cur_track);
#undef FUNC_NAME
}

PRIMPROC
(cd_get_cur_frame, "cd-get-cur-frame", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return the current frame of the CD in drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_get_cur_frame
  GETCUR (cur_frame);
#undef FUNC_NAME
}

#undef GETCUR


PRIMPROC
(cd_nth_track_itlo, "cd-nth-track-itlo", 1, 1, 0,
 (SCM cdrom, SCM n),
 doc: /***********
For CD in drive @var{cdrom}, return four values describing track
@var{n} (zero if unspecified): @code{id}, @code{type}, @code{length}
and @code{offset}, all integers except for @code{type}, which is
a symbol, either @code{audio} or @code{data}.  */)
{
#define FUNC_NAME s_cd_nth_track_itlo
  ASSERT_FIRST_ARG_OPEN_CDROM ();
  int cn = 0;

  if (BOUNDP (n))
    ASSERT_ULONG_COPY (n, 2);

  SCM_ASSERT_RANGE (2, n, cn < cd->numtracks);
  RETURN_VALUES4
    (NUM_LONG (cd->track[cn].id),
     btw->long2enum (cd->track[cn].type, cd_track_type),
     NUM_ULONG (cd->track[cn].length),
     NUM_ULONG (cd->track[cn].offset));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (offset);
DECLARE_SIMPLE_SYM (length);
DECLARE_SIMPLE_SYM (type);
DECLARE_SIMPLE_SYM (id);

PRIMPROC
(cd_get_nth_track, "cd-get-nth-track", 1, 1, 0,
 (SCM cdrom, SCM n),
 doc: /***********
NB: This procedure is obsoleted by @code{cd-nth-track-itlo}
and @strong{will be removed} after 2013-12-31.

For CD in drive @var{cdrom}, return info on track @var{n}
as an alist or @code{#f} if there were problems.  */)
{
#define FUNC_NAME s_cd_get_nth_track
  ASSERT_FIRST_ARG_OPEN_CDROM ();
  int cn = 0;

  if (BOUNDP (n))
    ASSERT_ULONG_COPY (n, 2);

  if (cd && (cn < cd->numtracks))
    /* Form an assoc list.  */
    return LIST4
      (CONS (SYM (id),     NUM_LONG (cd->track[cn].id)),
       CONS (SYM (type),   NUM_LONG (cd->track[cn].type)),
       CONS (SYM (length), NUM_ULONG (cd->track[cn].length)),
       CONS (SYM (offset), NUM_ULONG (cd->track[cn].offset)));
  else
    return BOOL_FALSE;
#undef FUNC_NAME
}


PRIMPROC
(cd_play_tracks, "cd-play-tracks", 1, 4, 0,
 (SCM cdrom,
  SCM start_track,
  SCM start_frame,
  SCM n_tracks,
  SCM n_frames),
 doc: /***********
Play the given CD tracks in drive @var{cdrom}.
Play the CD starting at @var{start-track} and
@var{start-frame} for @var{ntracks} tracks and @var{nframes}
frames.  If both @var{ntrack} and @var{nframe} are 0, play
until the end of the CD.  This procedure will skip data
tracks, and should only be called after calling
@code{cd-status} to get track information about the CD.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_play_tracks
  ASSERT_FIRST_ARG_OPEN_CDROM ();
  int cstart_track = 0, cstart_frame = 0, cn_tracks = 1, cn_frames = -1;

  IF_BOUND_ASSERT_ULONG_COPY (start_track, 2);
  IF_BOUND_ASSERT_ULONG_COPY (start_frame, 3);
  IF_BOUND_ASSERT_ULONG_COPY (n_tracks, 4);
  IF_BOUND_ASSERT_ULONG_COPY (n_frames, 5);
  if (0 > cn_frames)
    cn_frames = cd->track[cstart_track + cn_tracks - 1].length;

  RETURN_TRUE_IF_0
    (SDL_CDPlayTracks (cd, cstart_track, cstart_frame,
                       cn_tracks, cn_frames));
#undef FUNC_NAME
}


PRIMPROC
(cd_play, "cd-play", 3, 0, 0,
 (SCM cdrom,
  SCM start,
  SCM length),
 doc: /***********
Play CD in drive @var{cdrom} from @var{start} frame for
@var{length} frames.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_play
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  ASSERT_INTEGER (start, 2);
  ASSERT_INTEGER (length, 3);

  RETURN_TRUE_IF_0
    (SDL_CDPlay (cd, C_ULONG (start),
                 C_ULONG (length)));
#undef FUNC_NAME
}


PRIMPROC
(cd_pause, "cd-pause", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Pause the CD in drive @var{cdrom}.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_pause
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  RETURN_TRUE_IF_0
    (SDL_CDPause (cd));
#undef FUNC_NAME
}


PRIMPROC
(cd_resume, "cd-resume", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Resume (unpause) the CD in drive @var{cdrom}.
Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_resume
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  RETURN_TRUE_IF_0
    (SDL_CDResume (cd));
#undef FUNC_NAME
}


PRIMPROC
(cd_stop, "cd-stop", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Stop the CD in drive @var{cdrom}.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_stop
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  RETURN_TRUE_IF_0
    (SDL_CDStop (cd));
#undef FUNC_NAME
}


PRIMPROC
(cd_eject, "cd-eject", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Eject the CD from drive @var{cdrom}.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_cd_eject
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  RETURN_TRUE_IF_0
    (SDL_CDEject (cd));
#undef FUNC_NAME
}


PRIMPROC
(cd_close, "cd-close", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Close the drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_close
  ASSERT_FIRST_ARG_OPEN_CDROM ();

  SDL_CDClose (cd);
  SMOBSET (cdrom, NULL);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(cd_msf_to_frames, "cd-msf->frames", 1, 2, 0,
 (SCM m,
  SCM s,
  SCM f),
 doc: /***********
Return frames (an integer) computed fr
@var{m}, second @var{s} and frame @var{f}.
@var{s} and @var{f} are optional.  */)
{
#define FUNC_NAME s_cd_msf_to_frames
  int frames;
  int cm, cs = 0, cf = 0;

  ASSERT_ULONG_COPY (m, 1);
  IF_BOUND_ASSERT_ULONG_COPY (s, 2);
  IF_BOUND_ASSERT_ULONG_COPY (f, 3);

  frames = MSF_TO_FRAMES (cm, cs, cf);
  RETURN_INT (frames);
#undef FUNC_NAME
}


PRIMPROC
(frames_msf, "frames-msf", 1, 0, 0,
 (SCM frames),
 doc: /***********
Break down @var{frames} (an integer) and return three values:
@code{minute}, @code{second} and @code{frames} (all integers).  */)
{
#define FUNC_NAME s_frames_msf
  int cframes, m, s, f;

  ASSERT_ULONG_COPY (frames, 1);

  FRAMES_TO_MSF (cframes, &m , &s, &f);
  RETURN_VALUES3
    (NUM_ULONG (m),
     NUM_ULONG (s),
     NUM_ULONG (f));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (f);
DECLARE_SIMPLE_SYM (s);
DECLARE_SIMPLE_SYM (m);

PRIMPROC
(cd_frames_to_msf, "cd-frames->msf", 1, 0, 0,
 (SCM frames),
 doc: /***********
NB: This procedure is obsoleted by @code{frames-msf}
and @strong{will be removed} after 2013-12-31.

Return a minute/second/frames alist made from
converting @var{frames} (a number).  */)
{
#define FUNC_NAME s_cd_frames_to_msf
  int cframes, m, s, f;

  ASSERT_ULONG_COPY (frames, 1);

  FRAMES_TO_MSF (cframes, &m , &s, &f);
  return LIST3 (CONS (SYM (m), NUM_ULONG (m)),
                CONS (SYM (s), NUM_ULONG (s)),
                CONS (SYM (f), NUM_ULONG (f)));
#undef FUNC_NAME
}

/*-------------------------------------------------------------*/

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
print_cd (SCM cdrom, SCM port, UNUSED scm_print_state *pstate)
{
  SDL_CD *cd = UNPACK_CDROM (cdrom);
  char buf[32], *status;

  if (cd)
    switch (cd->status)
      {
      case CD_TRAYEMPTY: status = "TRAY EMPTY";  break;
      case CD_STOPPED:   status = "STOPPED";     break;
      case CD_PLAYING:   status = "PLAYING";     break;
      case CD_PAUSED:    status = "PAUSED";      break;
      case CD_ERROR:     status = "DRIVE ERROR"; break;
      default:           status = "???";
      }
  else
    status = "-";
  snprintf (buf, 32, "#<%s [%s]>", cdrom_nick, status);
  scm_puts (buf, port);
  return 1;
}


void
gsdl_init_cdrom (void)
{
  DEFSMOB (cdrom_tag, cdrom_nick,
           NULL,
           free_cd,
           print_cd);

  {
    kp_init_t allp[] = {
      { &cdrom_state, &cdstate_kp },
      { &cd_track_type, &cdtracktype_kp }
    };

    REGISTER_KP_V (allp);
  }

#include "sdlcdrom.x"
}

/* sdlcdrom.c ends here */
