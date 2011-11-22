/* sdlcdrom.c --- SDL CDROM functions for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2009, 2011 Thien-Thi Nguyen
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
#include <SDL/SDL.h>

static long cdrom_tag;

#define cdrom_nick "SDL-CD"

#define ASSERT_CDROM(obj,which) \
  ASSERT_SMOB (obj, cdrom_tag, which)

#define UNPACK_CDROM(smob) \
  (SMOBGET (smob, SDL_CD *))

#define RETURN_NEW_CDROM(x) \
  NEWSMOB_OR_FALSE (cdrom_tag, x)

#define CDROM_P(x) \
  (SCM_SMOB_PREDICATE (cdrom_tag, x))



PRIMPROC
(cd_p, "cd?", 1, 0, 0,
 (SCM obj),
 doc: /***********
Return #t iff @var{obj} is a CDROM drive object.  */)
{
#define FUNC_NAME s_cd_p
  RETURN_BOOL
    (CDROM_P (obj));
#undef FUNC_NAME
}


PRIMPROC
(cd_null_p, "cd-null?", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return #t iff @var{cdrom} is a null pointer.
[What does that mean? --ttn]  */)
{
#define FUNC_NAME s_cd_null_p
  ASSERT_CDROM (cdrom, 1);

  RETURN_BOOL
    (! UNPACK_CDROM (cdrom));
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
identifier (a string) for the CDROM.
Optional arg @var{drive} is a number specifying which drive.  */)
{
#define FUNC_NAME s_cd_name
  const char *name;
  int cdrive = 0;

  if (BOUNDP (drive))
    {
      ASSERT_EXACT (drive, 1);
      cdrive = C_INT (drive);
    }

  name = SDL_CDName (cdrive);
  RETURN_0STR (name);
#undef FUNC_NAME
}

PRIMPROC
(cd_open, "cd-open", 0, 1, 0,
 (SCM drive),
 doc: /***********
Open the CDROM drive for access and return its handle.
If the drive is unavailable, return #f.
Optional arg @var{drive} is a number specifying which drive.  */)
{
#define FUNC_NAME s_cd_open
  SDL_CD *cd;
  int cdrive = 0;

  if (BOUNDP (drive))
    {
      ASSERT_EXACT (drive, 1);
      cdrive = C_INT (drive);
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

PRIMPROC
(cd_status, "cd-status", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return the current status of the drive @var{cdrom}
as a symbol, one of: @code{TRAYEMTPY}, @code{STOPPED},
@code{PLAYING}, @code{PAUSED} or @code{ERROR}.  */)
{
#define FUNC_NAME s_cd_status
  SDL_CD *cd;
  int ret = CD_ERROR;

  ASSERT_CDROM (cdrom, 1);
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


PRIMPROC
(cd_in_drive_p, "cd-in-drive?", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Return #t iff there is a CD in drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_in_drive_p
  SDL_CD *cd;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    RETURN_BOOL (CD_INDRIVE (SDL_CDStatus (cd)));
  else
    RETURN_FALSE;
#undef FUNC_NAME
}


#define GETCUR(field)                           \
  do {                                          \
    SDL_CD *cd;                                 \
    int ret = -1;                               \
                                                \
    ASSERT_CDROM (cdrom, 1);                    \
    if ((cd = UNPACK_CDROM (cdrom))             \
        && CD_TRAYEMPTY != SDL_CDStatus (cd))   \
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


DECLARE_SIMPLE_SYM (offset);
DECLARE_SIMPLE_SYM (length);
DECLARE_SIMPLE_SYM (type);
DECLARE_SIMPLE_SYM (id);

PRIMPROC
(cd_get_nth_track, "cd-get-nth-track", 1, 1, 0,
 (SCM cdrom, SCM n),
 doc: /***********
For CD in drive @var{cdrom}, return info on track @var{n}
as an alist or #f if there were problems.  */)
{
#define FUNC_NAME s_cd_get_nth_track
  SDL_CD *cd;
  int cn = 0;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (BOUNDP (n))
    {
      ASSERT_EXACT (n, 2);
      cn = C_ULONG (n);
    }

  if (cd && (cn < cd->numtracks))
    /* Form an assoc list.  */
    RETURN_LIST4
      (CONS (SYM (id),     NUM_LONG (cd->track[cn].id)),
       CONS (SYM (type),   NUM_LONG (cd->track[cn].type)),
       CONS (SYM (length), NUM_ULONG (cd->track[cn].length)),
       CONS (SYM (offset), NUM_ULONG (cd->track[cn].offset)));
  else
    RETURN_FALSE;
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
Return #t if successful.  */)
{
#define FUNC_NAME s_cd_play_tracks
  SDL_CD *cd;
  int cstart_track = 0, cstart_frame = 0, cn_tracks = 1, cn_frames = 1;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);

  UNBOUND_MEANS_FALSE (start_track);
  if (NOT_FALSEP (start_track))
    {
      ASSERT_EXACT (start_track, 2);
      cstart_track = C_ULONG (start_track);
    }

  UNBOUND_MEANS_FALSE (start_frame);
  if (NOT_FALSEP (start_frame))
    {
      ASSERT_EXACT (start_frame, 3);
      cstart_frame = C_ULONG (start_frame);
    }

  UNBOUND_MEANS_FALSE (n_tracks);
  if (NOT_FALSEP (n_tracks))
    {
      ASSERT_EXACT (n_tracks, 4);
      cn_tracks = C_ULONG (n_tracks);;
    }

  UNBOUND_MEANS_FALSE (n_frames);
  if (NOT_FALSEP (n_frames))
    {
      ASSERT_EXACT (n_frames, 5);
      cn_frames = C_ULONG (n_frames);
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


PRIMPROC
(cd_play, "cd-play", 3, 0, 0,
 (SCM cdrom,
  SCM start,
  SCM length),
 doc: /***********
Play CD in drive @var{cdrom} from @var{start} frame for
@var{length} frames.  Return #t if successful.  */)
{
#define FUNC_NAME s_cd_play
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);
  ASSERT_EXACT (start, 2);
  ASSERT_EXACT (length, 3);

  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDPlay (cd, C_ULONG (start), C_ULONG (length));

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


PRIMPROC
(cd_pause, "cd-pause", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Pause the CD in drive @var{cdrom}.  Return #t if successful.  */)
{
#define FUNC_NAME s_cd_pause
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDPause (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


PRIMPROC
(cd_resume, "cd-resume", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Resume (unpause) the CD in drive @var{cdrom}.
Return #t if successful.  */)
{
#define FUNC_NAME s_cd_resume
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDResume (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


PRIMPROC
(cd_stop, "cd-stop", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Stop the CD in drive @var{cdrom}.  Return #t if successful.  */)
{
#define FUNC_NAME s_cd_stop
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDStop (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


PRIMPROC
(cd_eject, "cd-eject", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Eject the CD from drive @var{cdrom}.  Return #t if successful.  */)
{
#define FUNC_NAME s_cd_eject
  SDL_CD *cd;
  int ret = -1;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    ret = SDL_CDEject (cd);

  RETURN_TRUE_IF_0 (ret);
#undef FUNC_NAME
}


PRIMPROC
(cd_close, "cd-close", 1, 0, 0,
 (SCM cdrom),
 doc: /***********
Close the drive @var{cdrom}.  */)
{
#define FUNC_NAME s_cd_close
  SDL_CD *cd;

  ASSERT_CDROM (cdrom, 1);
  cd = UNPACK_CDROM (cdrom);

  if (cd)
    {
      SDL_CDClose (cd);
      SMOBSET (cdrom, NULL);
    }

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

  ASSERT_EXACT (m, 1);
  cm = C_ULONG (m);

  UNBOUND_MEANS_FALSE (s);
  if (NOT_FALSEP (s))
    {
      ASSERT_EXACT (s, 2);
      cs = C_ULONG (s);
    }

  UNBOUND_MEANS_FALSE (f);
  if (NOT_FALSEP (f))
    {
      ASSERT_EXACT (f, 3);
      cf = C_ULONG (f);
    }

  frames = MSF_TO_FRAMES (cm, cs, cf);
  RETURN_INT (frames);
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (f);
DECLARE_SIMPLE_SYM (s);
DECLARE_SIMPLE_SYM (m);

PRIMPROC
(cd_frames_to_msf, "cd-frames->msf", 1, 0, 0,
 (SCM frames),
 doc: /***********
Return a minute/second/frames alist made from
converting @var{frames} (a number).  */)
{
#define FUNC_NAME s_cd_frames_to_msf
  int cframes, m, s, f;

  ASSERT_EXACT (frames, 1);
  cframes = C_ULONG (frames);

  FRAMES_TO_MSF (cframes, &m , &s, &f);
  RETURN_LIST3 (CONS (SYM (m), NUM_ULONG (m)),
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
print_cd (SCM cdrom, SCM port, scm_print_state *pstate)
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

#include "sdlcdrom.x"
}

/* sdlcdrom.c ends here */
