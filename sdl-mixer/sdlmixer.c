/* sdlmixer.c --- SDL_mixer for Guile
 *
 * 	Copyright (C) 2003,2004,2005,2007 Thien-Thi Nguyen
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

#include <guile/gh.h>
#include <SDL/SDL_mixer.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "sym.h"
#include "bool.h"

GH_USE_MODULE (sdlsup, "(sdl sdl)"); /* for various gsdl_* C funcs */


SCM fading_status_enum;
#define RETURN_FADINGSTATUS(x)  return gsdl_long2enum ((x), fading_status_enum)


static long mix_music_tag;
static long mix_audio_tag;

#define ASSERT_MUSIC(obj,which)   ASSERT_SMOB (obj, mix_music_tag, which)
#define ASSERT_AUDIO(obj,which)   ASSERT_SMOB (obj, mix_audio_tag, which)

#define UNPACK_MUSIC(smob)   (SMOBGET (smob, Mix_Music *))
#define UNPACK_AUDIO(smob)   (SMOBGET (smob, Mix_Chunk *))

#define RETURN_NEW_MUSIC(x)   NEWSMOB_OR_FALSE (mix_music_tag, x)
#define RETURN_NEW_AUDIO(x)   NEWSMOB_OR_FALSE (mix_audio_tag, x)

static
size_t
free_music (SCM music)
{
  Mix_FreeMusic (UNPACK_MUSIC (music));
  return sizeof (struct Mix_Music*);
}

static
size_t
free_audio (SCM chunk)
{
  Mix_FreeChunk (UNPACK_AUDIO (chunk));
  return sizeof (Mix_Chunk);
}


GH_DEFPROC
(mix_open_audio, "open-audio", 0, 4, 0,
 (SCM freq, SCM format, SCM stereo, SCM chunksize),
 doc: /***********
Open the mixer with a certain audio format.
Optional args @var{freq} (number), @var{format} (number),
@var{stereo} (boolean) and @var{chunksize} (number) specify
those aspects of the device.  Return #t if successful.  */)
{
#define FUNC_NAME s_mix_open_audio
  int cfreq = MIX_DEFAULT_FREQUENCY;
  Uint16 cformat = MIX_DEFAULT_FORMAT;
  int cchannels = 2;
  int cchunksize = 1024;

  UNBOUND_MEANS_FALSE (freq);
  if (NOT_FALSEP (freq))
    {
      ASSERT_EXACT (freq, ARGH1);
      cfreq = gh_scm2long (freq);
    }

  UNBOUND_MEANS_FALSE (format);
  if (NOT_FALSEP (format))
    {
      ASSERT_EXACT (format, ARGH2);
      cformat = gh_scm2long (format);
    }

  cchannels -= UNBOUNDP (stereo) ? 0 : EXACTLY_FALSEP (stereo);

  UNBOUND_MEANS_FALSE (chunksize);
  if (NOT_FALSEP (chunksize))
    {
      ASSERT_EXACT (chunksize, ARGH4);
      cchunksize = gh_scm2long (chunksize);
    }

  /* Open the audio device.  */
  RETURN_TRUE_IF_0
    (Mix_OpenAudio (cfreq, MIX_DEFAULT_FORMAT, cchannels, 1024));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_allocate_channels, "allocated-channels", 1, 0, 0,
 (SCM numchans),
 doc: /***********
Dynamically change the number of channels managed by
the mixer to @var{numchans}.  If decreasing the number
of channels, the upper channels are stopped.  Return the
new number of allocated channels.  */)
{
#define FUNC_NAME s_mix_allocate_channels
  ASSERT_EXACT (numchans, ARGH1);

  RETURN_INT (Mix_AllocateChannels (gh_scm2long (numchans)));
#undef FUNC_NAME
}

DECLARE_SIMPLE_SYM (freq);
DECLARE_SIMPLE_SYM (format);
DECLARE_SIMPLE_SYM (channels);

GH_DEFPROC
(mix_query_spec, "query-spec", 0, 0, 0,
 (void),
 doc: /***********
Return audio device parameters as an alist, or #f
if the audio has not yet been opened.
Keys are @code{freq} (frequency), @code{format},
and @code{channels} (the number of allocated channels).  */)
{
#define FUNC_NAME s_mix_query_spec
  int freq, channels;
  Uint16 format;

  if (! Mix_QuerySpec (&freq, &format, &channels))
    RETURN_FALSE;

  RETURN_LIST3 (gh_cons (SYM (freq), gh_long2scm (freq)),
                gh_cons (SYM (format), gh_long2scm (format)),
                gh_cons (SYM (channels), gh_long2scm (channels)));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_load_music, "load-music", 1, 0, 0,
 (SCM file),
 doc: /***********
Load a wave or a music (.mod .s3m .it .xm) @var{file}.
Return a handle to it.  */)
{
#define FUNC_NAME s_mix_load_music
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_MUSIC
    (Mix_LoadMUS (SCM_CHARS (file)));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_load_wave, "load-wave", 1, 0, 0,
 (SCM file),
 doc: /***********
Load a wave @var{file}. Return a handle to it.  */)
{
#define FUNC_NAME s_mix_load_wave
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_AUDIO
    (Mix_LoadWAV (SCM_CHARS (file)));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_reserve_channels, "reserve-channels", 1, 0, 0,
 (SCM num),
 doc: /***********
Reserve the first @var{num} channels (0 through @var{num}-1)
for the application.  In other words don't allocate them
dynamically to
the next sample if requested with a -1 value below.
Return the number of reserved channels.  */)
{
#define FUNC_NAME s_mix_reserve_channels
  ASSERT_EXACT (num, ARGH1);

  RETURN_INT (Mix_ReserveChannels (gh_scm2long (num)));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_channel, "group-channel", 1, 1, 0,
 (SCM channel, SCM tag),
 doc: /***********
Attach to @var{channel} a @var{tag}.
A tag can be assigned to several mixer channels, to
form groups of channels.  If @var{tag} is not specified, or
is -1, the tag is removed (actually -1 is the tag used
to represent the group of all the channels).  Return
#t if successful.  */)
{
#define FUNC_NAME s_mix_group_channel
  int ctag = -1;

  ASSERT_EXACT (channel, ARGH1);

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH2);
      ctag = gh_scm2long (tag);
    }

  RETURN_BOOL
    (Mix_GroupChannel (gh_scm2long (channel), ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_channels, "group-channels", 2, 1, 0,
 (SCM from, SCM to, SCM tag),
 doc: /***********
Assign channels in the range @var{from} through @var{to}
to the default group.  Optional arg @var{tag} specifies
the group to use.  Return #t if successful.  */)
{
#define FUNC_NAME s_mix_group_channels
  int ctag = -1;

  ASSERT_EXACT (from, ARGH1);
  ASSERT_EXACT (to, ARGH2);

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH3);
      ctag = gh_scm2long (tag);
    }

  RETURN_BOOL
    (Mix_GroupChannels (gh_scm2long (from),
                        gh_scm2long (to),
                        ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_available, "group-available", 0, 1, 0,
 (SCM tag),
 doc: /***********
Return the first available channel in the default
group of channels.
Optional arg @var{tag} specifies the group to check.  */)
{
#define FUNC_NAME s_mix_group_available
  int ctag = -1;

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  RETURN_INT (Mix_GroupAvailable (ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_count, "group-count", 0, 1, 0,
 (SCM tag),
 doc: /***********
Return the number of channels in the default group.
Optional arg @var{tag} specifies the group to check.  */)
{
#define FUNC_NAME s_mix_group_count
  int ctag = -1;

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  RETURN_INT (Mix_GroupCount (ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_oldest, "group-oldest", 0, 1, 0,
 (SCM tag),
 doc: /***********
Return the "oldest" sample playing in the default
group of channels.
Optional arg @var{tag} specifies the group to check.  */)
{
#define FUNC_NAME s_mix_group_oldest
  int ctag = -1;

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  RETURN_INT (Mix_GroupOldest (ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_group_newer, "group-newer", 0, 1, 0,
 (SCM tag),
 doc: /***********
Return the "most recent" (i.e. last) sample playing
in the default group of channels.
Optional arg @var{tag} specifies the group to check.  */)
{
#define FUNC_NAME s_mix_group_newer
  int ctag = -1;

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  RETURN_INT (Mix_GroupNewer (ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_play_channel, "play-channel", 1, 4, 0,
 (SCM chunk, SCM channel, SCM loops, SCM ticks, SCM fade),
 doc: /***********
Play an audio @var{chunk} on a specific @var{channel}.
If the channel is unspecified or is -1, play on the
first free channel.  If @var{loops} is specified and
greater than zero, loop the sound that many times.  If
@var{loops} is -1, loop infinitely (~65000 times).  If
@var{ticks} is specified, stop after that number of ticks.
If @var{fade} is specified, fade in over that number of
milliseconds.  Return which channel was used to play
the sound.  */)
{
#define FUNC_NAME s_mix_play_channel
  int cchannel = -1;
  Mix_Chunk *cchunk;
  int cloops = 0;
  int cticks = -1;
  long rv;

  ASSERT_AUDIO (chunk, ARGH1);
  cchunk = UNPACK_AUDIO (chunk);

  UNBOUND_MEANS_FALSE (channel);
  if (NOT_FALSEP (channel))
    {
      ASSERT_EXACT (channel, ARGH2);
      cchannel = gh_scm2long (channel);
    }

  UNBOUND_MEANS_FALSE (loops);
  if (NOT_FALSEP (loops))
    {
      ASSERT_EXACT (loops, ARGH3);
      cloops = gh_scm2long (loops);
    }

  UNBOUND_MEANS_FALSE (ticks);
  if (NOT_FALSEP (ticks))
    {
      ASSERT_EXACT (ticks, ARGH4);
      cticks = gh_scm2long (ticks);
    }

  if (UNBOUNDP (fade))
    /* No fade, normal Mix_PlayChannelTimed.  */
    rv = Mix_PlayChannelTimed (cchannel, cchunk, cloops, cticks);
  else
    {
      /* We have a fade.  */
      ASSERT_EXACT (fade, ARGH5);
      rv = Mix_FadeInChannelTimed (cchannel, cchunk, cloops,
                                   gh_scm2long (fade),
                                   cticks);
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


GH_DEFPROC
(mix_play_music, "play-music", 1, 2, 0,
 (SCM music, SCM loops, SCM fade),
 doc: /***********
Play a @var{music} track.
Optional args @var{loops} and @var{fade}
are as in @code{play-channel}.  */)
{
#define FUNC_NAME s_mix_play_music
  Mix_Music *cmusic;
  int cloops = 0;
  long rv;

  ASSERT_MUSIC (music, ARGH1);
  cmusic = UNPACK_MUSIC (music);

  UNBOUND_MEANS_FALSE (loops);
  if (NOT_FALSEP (loops))
    {
      ASSERT_EXACT (loops, ARGH2);
      cloops = gh_scm2long (loops);
    }

  if (UNBOUNDP (fade))
    /* No fade, normal Mix_PlayMusic.  */
    rv = Mix_PlayMusic (cmusic, cloops);
  else
    {
      /* We have a fade.  */
      ASSERT_EXACT (fade, ARGH3);
      rv = Mix_FadeInMusic (cmusic, cloops, gh_scm2long (fade));
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


GH_DEFPROC
(mix_volume, "volume", 0, 2, 0,
 (SCM volume, SCM which),
 doc: /***********
Return the current volume on the default channel.
Optional arg @var{volume} (a number in the range 0-128) means
set the volume to @var{volume} and return the original volume.
Optional second arg @var{which} specifies a chunk or
channel to check (or modify) instead of the default.
If @var{volume} is non-#f and @var{which} is #f, modify all
channels.

[Here is the original (perhaps clearer) docstring. ---ttn]

Set the volume in the range of 0-128 of a specific channel
or chunk.  If the channel is unspecified or is -1, set volume
for all channels.  Return the original volume.  If the volume
is unspecified or is -1, just return the current volume.  */)
{
#define FUNC_NAME s_mix_volume
  int cvolume = -1;
  long rv;

  UNBOUND_MEANS_FALSE (volume);
  if (NOT_FALSEP (volume))
    {
      ASSERT_EXACT (volume, ARGH1);
      cvolume = gh_scm2long (volume);
    }

  if (UNBOUNDP (which))
    /* No chunk or channel, call Mix_Volume on default channel.  */
    rv = Mix_Volume (-1, cvolume);
  else if (gh_exact_p (which))
    /* Numeric which, treat as channel number.  */
    rv = Mix_Volume (gh_scm2long (which), cvolume);
  else
    {
      /* No-numeric which, must be a chunk smob.  */
      ASSERT_AUDIO (which, ARGH2);
      rv = Mix_VolumeChunk (UNPACK_AUDIO (which), cvolume);
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


GH_DEFPROC
(mix_volume_music, "music-volume", 0, 1, 0,
 (SCM volume),
 doc: /***********
Return the current volume.
Optional arg @var{volume} (a number in the range 0-128)
means set the volume to @var{volume}.  */)
{
#define FUNC_NAME s_mix_volume_music
  int cvolume = -1;

  if (BOUNDP (volume))
    {
      ASSERT_EXACT (volume, ARGH1);
      cvolume = gh_scm2long (volume);
    }

  RETURN_INT (Mix_VolumeMusic (cvolume));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_halt_channel, "halt-channel", 0, 1, 0,
 (SCM channel),
 doc: /***********
Halt playing of the default channel.
Optional arg @var{channel} specifies a channel to halt.  */)
{
#define FUNC_NAME s_mix_halt_channel
  int cchannel = -1;

  if (BOUNDP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  RETURN_INT (Mix_HaltChannel (cchannel));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_halt_group, "halt-group", 0, 1, 0,
 (SCM tag),
 doc: /***********
Halt playing of the default group.
Optional arg @var{tag} specifies the group to halt.  */)
{
#define FUNC_NAME s_mix_halt_group
  int ctag = -1;

  if (BOUNDP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  RETURN_INT (Mix_HaltGroup (ctag));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_halt_music, "halt-music", 0, 0, 0,
 (void),
 doc: /***********
Halt playing of the music.  */)
{
#define FUNC_NAME s_mix_halt_music
  RETURN_INT (Mix_HaltMusic ());
#undef FUNC_NAME
}


GH_DEFPROC
(mix_expire_channel, "expire-channel", 0, 2, 0,
 (SCM channel, SCM ticks),
 doc: /***********
Turn off expiration for the default channel.
Optional arg @var{channel} specifies a channel to change.
Optional arg @var{ticks} (a number) means set the expiration
delay to that many milliseconds, rather than turning it off.  */)
{
#define FUNC_NAME s_mix_expire_channel
  int cchannel = -1;
  int cticks = -1;

  UNBOUND_MEANS_FALSE (channel);
  if (NOT_FALSEP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  if (BOUNDP (ticks))
    {
      ASSERT_EXACT (ticks, ARGH2);
      cticks = gh_scm2long (ticks);
    }

  RETURN_INT (Mix_ExpireChannel (cchannel, cticks));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_fade_out_channel, "fade-out-channel", 0, 2, 0,
 (SCM which, SCM ms),
 doc: /***********
Halt a channel, fading it out progressively until silent.
Optional arg @var{which} specifies a channel to halt.
Second optional arg @var{ms} specifies the number of
milliseconds the fading will take (default 0).  */)
{
#define FUNC_NAME s_mix_fade_out_channel
  int cchannel = -1;
  int cms = 0;

  UNBOUND_MEANS_FALSE (which);
  if (NOT_FALSEP (which))
    {
      ASSERT_EXACT (which, ARGH1);
      cchannel = gh_scm2long (which);
    }

  if (BOUNDP (ms))
    {
      ASSERT_EXACT (ms, ARGH2);
      cms = gh_scm2long (ms);
    }

  RETURN_INT (Mix_FadeOutChannel (cchannel, cms));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_fade_out_group, "fade-out-group", 0, 2, 0,
 (SCM tag, SCM ms),
 doc: /***********
Halt a group, fading it out progressively until silent.
Optional arg @var{tag} specifies a group to halt.
Second optional arg @var{ms} specifies the number of
milliseconds the fading will take (default 0).  */)
{
#define FUNC_NAME s_mix_fade_out_group
  int ctag = -1;
  int cms = 0;

  UNBOUND_MEANS_FALSE (tag);
  if (NOT_FALSEP (tag))
    {
      ASSERT_EXACT (tag, ARGH1);
      ctag = gh_scm2long (tag);
    }

  if (BOUNDP (ms))
    {
      ASSERT_EXACT (ms, ARGH2);
      cms = gh_scm2long (ms);
    }

  RETURN_INT (Mix_FadeOutGroup (ctag, cms));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_fade_out_music, "fade-out-music", 0, 1, 0,
 (SCM ms),
 doc: /***********
Halt the music, fading it out progressively until silent.
Optional arg @var{ms} specifies the number of milliseconds
the fading will take (default 0).  */)
{
#define FUNC_NAME s_mix_fade_out_music
  int cms = 0;

  if (BOUNDP (ms))
    {
      ASSERT_EXACT (ms, ARGH1);
      cms = gh_scm2long (ms);
    }

  RETURN_INT (Mix_FadeOutMusic (cms));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_fading_music, "fading-music", 0, 0, 0,
 (void),
 doc: /***********
Return the fading status of the music.
@xref{Enums and Constants}.  */)
{
#define FUNC_NAME s_mix_fading_music
  RETURN_FADINGSTATUS (Mix_FadingMusic ());
#undef FUNC_NAME
}


GH_DEFPROC
(mix_fading_channel, "fading-channel", 0, 1, 0,
 (SCM which),
 doc: /***********
Return the fading status of a the default channe
Optional arg @var{which} selects which channel to check.
@xref{Enums and Constants}.  */)
{
#define FUNC_NAME s_mix_fading_channel
  int cwhich = -1;

  if (BOUNDP (which))
    {
      ASSERT_EXACT (which, ARGH1);
      cwhich = gh_scm2long (which);
    }

  RETURN_FADINGSTATUS (Mix_FadingChannel (cwhich));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_pause, "pause", 0, 1, 0,
 (SCM channel),
 doc: /***********
Pause the default channe
Optional arg @var{channel} selects which channel to pause.
Return value unspecified.  */)
{
#define FUNC_NAME s_mix_pause
  int cchannel = -1;

  if (BOUNDP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  Mix_Pause (cchannel);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC
(mix_resume, "resume", 0, 1, 0,
 (SCM channel),
 doc: /***********
Resume (unpause) the default channel.
Optional arg @var{channel} selects which channel to resume.
Return value unspecified.  */)
{
#define FUNC_NAME s_mix_resume
  int cchannel = -1;

  if (BOUNDP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  Mix_Resume (cchannel);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC
(mix_paused, "paused?", 0, 1, 0,
 (SCM channel),
 doc: /***********
Return #t if the default channel is paused.
Optional arg @var{channel} selects a which channel to check.  */)
{
#define FUNC_NAME s_mix_resume
  int cchannel = -1;

  if (BOUNDP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  RETURN_BOOL
    (Mix_Paused (cchannel));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_pause_music, "pause-music", 0, 0, 0,
 (void),
 doc: /***********
Pause the music.  Return value unspecified.  */)
{
#define FUNC_NAME s_mix_pause_music
  Mix_PauseMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC
(mix_resume_music, "resume-music", 0, 0, 0,
 (void),
 doc: /***********
Resume (unpause) the music.  Return value unspecified.  */)
{
#define FUNC_NAME s_mix_resume_music
  Mix_ResumeMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC
(mix_rewind_music, "rewind-music", 0, 0, 0,
 (void),
 doc: /***********
Rewind the music.  Return value unspecified.  */)
{
#define FUNC_NAME s_mix_rewind_music
  Mix_RewindMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


GH_DEFPROC
(mix_paused_music, "paused-music?", 0, 0, 0,
 (void),
 doc: /***********
Return #t if the music is currently paused.  */)
{
#define FUNC_NAME s_mix_paused_music
  RETURN_BOOL
    (Mix_PausedMusic ());
#undef FUNC_NAME
}


GH_DEFPROC
(mix_playing, "playing?", 0, 1, 0,
 (SCM channel),
 doc: /***********
Return #t iff the default channel is playing.
Optional arg @var{channel} selects which channel to check.  */)
{
#define FUNC_NAME s_mix_playing
  int cchannel = -1;

  if (BOUNDP (channel))
    {
      ASSERT_EXACT (channel, ARGH1);
      cchannel = gh_scm2long (channel);
    }

  RETURN_BOOL
    (Mix_Playing (cchannel));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_playing_music, "playing-music?", 0, 0, 0,
 (void),
 doc: /***********
Return #t iff the music is currently playing.  */)
{
#define FUNC_NAME s_mix_playing_music
  RETURN_BOOL
    (Mix_PlayingMusic ());
#undef FUNC_NAME
}


GH_DEFPROC
(mix_set_music_cmd, "set-music-command", 1, 0, 0,
 (SCM command),
 doc: /***********
Stop music and set external music playback command
to @var{command}, a string.  */)
{
#define FUNC_NAME s_mix_set_music_cmd
  ASSERT_STRING (command, ARGH1);
  RETURN_INT (Mix_SetMusicCMD (SCM_CHARS (command)));
#undef FUNC_NAME
}


GH_DEFPROC
(mix_close_audio, "close-audio", 0, 0, 0,
 (void),
 doc: /***********
Close the mixer, halting all playing audio.  */)
{
#define FUNC_NAME s_mix_close_audio
  Mix_CloseAudio ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}



/* initialize the mixer subsystem */
static
void
init_module (void)
{
  /* smobs */
  mix_music_tag = scm_make_smob_type ("sdl-music", sizeof (struct Mix_Music*));
  scm_set_smob_free (mix_music_tag, free_music);

  mix_audio_tag = scm_make_smob_type ("sdl-audio", sizeof (Mix_Chunk));
  scm_set_smob_free (mix_audio_tag, free_audio);

  /* enums */
  fading_status_enum = gsdl_define_enum
    ("fading-status",
     GSDL_CSCS (MIX_NO_FADING),
     GSDL_CSCS (MIX_FADING_OUT),
     GSDL_CSCS (MIX_FADING_IN),
     NULL);

#include "sdlmixer.x"
}

GH_MODULE_LINK_FUNC ("sdl mixer", sdl_mixer, init_module)

/* sdlmixer.c ends here */
