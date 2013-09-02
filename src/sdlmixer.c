/* sdlmixer.c --- SDL_mixer for Guile
 *
 * Copyright (C) 2003, 2004, 2005, 2007, 2008,
 *   2009, 2011, 2013 Thien-Thi Nguyen
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

#define GUILE_SDL_OPTIONAL_MODULE  1
#include "guile-sdl.h"
#include "SDL_mixer.h"
#include "snuggle/finangle.h"
#include "b-values.h"

IMPORT_MODULE (sdlsup, "(sdl sdl)");
SELECT_MODULE_VAR (obtw, sdlsup, "%%Guile-SDL-obtw");


static SCM fading_status_enum;
#include "k/fading.c"

#define RETURN_FADINGSTATUS(x)  return btw->long2enum ((x), fading_status_enum)


static smob_tag_t mix_music_tag;
static smob_tag_t mix_audio_tag;

#define mix_music_nick "SDL-Music"
#define mix_audio_nick "SDL-Audio"

#define ASSERT_MUSIC(obj,which)   ASSERT_SMOB (obj, mix_music, which)
#define ASSERT_AUDIO(obj,which)   ASSERT_SMOB (obj, mix_audio, which)

#define UNPACK_MUSIC(smob)   (SMOBGET (smob, Mix_Music *))
#define UNPACK_AUDIO(smob)   (SMOBGET (smob, Mix_Chunk *))

#define RETURN_NEW_MUSIC(x)   NEWSMOB_OR_FALSE (mix_music_tag, x)
#define RETURN_NEW_AUDIO(x)   NEWSMOB_OR_FALSE (mix_audio_tag, x)

static
size_t
free_music (SCM music)
{
  Mix_FreeMusic (UNPACK_MUSIC (music));
  return 0;
}

static
size_t
free_audio (SCM chunk)
{
  Mix_FreeChunk (UNPACK_AUDIO (chunk));
  return 0;
}


PRIMPROC
(mix_open_audio, "open-audio", 0, 4, 0,
 (SCM freq, SCM format, SCM stereo, SCM chunksize),
 doc: /***********
Open the mixer with a certain audio format.
Optional args @var{freq} (number), @var{format} (number),
@var{stereo} (boolean) and @var{chunksize} (number) specify
those aspects of the device.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_mix_open_audio
  int cfreq = MIX_DEFAULT_FREQUENCY;
  Uint16 cformat = MIX_DEFAULT_FORMAT;
  int cchannels = 2;
  int cchunksize = 1024;

  IF_BOUND_ASSERT_LONG_COPY (freq, 1);
  IF_BOUND_ASSERT_LONG_COPY (format, 2);

  cchannels -= UNBOUNDP (stereo) ? 0 : EXACTLY_FALSEP (stereo);

  IF_BOUND_ASSERT_LONG_COPY (chunksize, 4);

  /* Open the audio device.  */
  RETURN_TRUE_IF_0
    (Mix_OpenAudio (cfreq, cformat, cchannels, cchunksize));
#undef FUNC_NAME
}


PRIMPROC
(mix_allocate_channels, "allocated-channels", 1, 0, 0,
 (SCM numchans),
 doc: /***********
Dynamically change the number of channels managed by
the mixer to @var{numchans}.  If decreasing the number
of channels, the upper channels are stopped.  Return the
new number of allocated channels.  */)
{
#define FUNC_NAME s_mix_allocate_channels
  ASSERT_INTEGER (numchans, 1);

  RETURN_INT (Mix_AllocateChannels (C_LONG (numchans)));
#undef FUNC_NAME
}


PRIMPROC
(device_ffc, "device-ffc", 0, 0, 0,
 (void),
 doc: /***********
Return audio device parameters as three values: @code{frequency} (Hz),
@code{format} (number of bits) and @code{channels} (number of
allocated channels).  */)
{
#define FUNC_NAME s_device_ffc
  int freq, channels;
  Uint16 format;

  if (! Mix_QuerySpec (&freq, &format, &channels))
    SCM_MISC_ERROR ("audio not open", SCM_EOL);

  RETURN_VALUES3
    (NUM_LONG (freq),
     NUM_LONG (format),
     NUM_LONG (channels));
#undef FUNC_NAME
}


DECLARE_SIMPLE_SYM (freq);
DECLARE_SIMPLE_SYM (format);
DECLARE_SIMPLE_SYM (channels);

PRIMPROC
(mix_query_spec, "query-spec", 0, 0, 0,
 (void),
 doc: /***********
NB: This procedure is obsoleted by @code{device-ffc}
and @strong{will be removed} after 2013-12-31.

Return audio device parameters as an alist, or @code{#f}
if the audio has not yet been opened.
Keys are @code{freq} (frequency), @code{format},
and @code{channels} (the number of allocated channels).  */)
{
#define FUNC_NAME s_mix_query_spec
  int freq, channels;
  Uint16 format;

  if (! Mix_QuerySpec (&freq, &format, &channels))
    return BOOL_FALSE;

  return LIST3 (CONS (SYM (freq), NUM_LONG (freq)),
                CONS (SYM (format), NUM_LONG (format)),
                CONS (SYM (channels), NUM_LONG (channels)));
#undef FUNC_NAME
}


PRIMPROC
(mix_load_music, "load-music", 1, 0, 0,
 (SCM filename),
 doc: /***********
Load music data (.mod .s3m .it .xm) from @var{filename}.
Return a new music object if successful, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_mix_load_music
  range_t cfilename;
  Mix_Music *rv;

  ASSERT_STRING (filename, 1);
  FINANGLE (filename);
  rv = Mix_LoadMUS (RS (filename));
  UNFINANGLE (filename);

  RETURN_NEW_MUSIC (rv);
#undef FUNC_NAME
}


PRIMPROC
(mix_load_wave, "load-wave", 1, 0, 0,
 (SCM filename),
 doc: /***********
Load wave data from @var{filename}.
Return a new audio object if succesful, otherwise @code{#f}.  */)
{
#define FUNC_NAME s_mix_load_wave
  range_t cfilename;
  Mix_Chunk *rv;

  ASSERT_STRING (filename, 1);
  FINANGLE (filename);
  rv = Mix_LoadWAV (RS (filename));
  UNFINANGLE (filename);

  RETURN_NEW_AUDIO (rv);
#undef FUNC_NAME
}


PRIMPROC
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
  ASSERT_INTEGER (num, 1);

  RETURN_INT (Mix_ReserveChannels (C_LONG (num)));
#undef FUNC_NAME
}


PRIMPROC
(mix_group_channel, "group-channel", 1, 1, 0,
 (SCM channel, SCM tag),
 doc: /***********
Attach to @var{channel} a @var{tag}.
A tag can be assigned to several mixer channels, to
form groups of channels.  If @var{tag} is not specified, or
is -1, the tag is removed (actually -1 is the tag used
to represent the group of all the channels).  Return
@code{#t} if successful.  */)
{
#define FUNC_NAME s_mix_group_channel
  int ctag = -1;

  ASSERT_INTEGER (channel, 1);

  if (BOUNDP (tag))
    ASSERT_LONG_COPY (tag, 2);

  RETURN_BOOL
    (Mix_GroupChannel (C_LONG (channel), ctag));
#undef FUNC_NAME
}


PRIMPROC
(mix_group_channels, "group-channels", 2, 1, 0,
 (SCM from, SCM to, SCM tag),
 doc: /***********
Assign channels in the range @var{from} through @var{to}
to the default group.  Optional arg @var{tag} specifies
the group to use.  Return @code{#t} if successful.  */)
{
#define FUNC_NAME s_mix_group_channels
  int ctag = -1;

  ASSERT_INTEGER (from, 1);
  ASSERT_INTEGER (to, 2);

  if (BOUNDP (tag))
    ASSERT_LONG_COPY (tag, 3);

  RETURN_BOOL
    (Mix_GroupChannels (C_LONG (from), C_LONG (to), ctag));
#undef FUNC_NAME
}


PRIMPROC
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
    ASSERT_LONG_COPY (tag, 1);

  RETURN_INT (Mix_GroupAvailable (ctag));
#undef FUNC_NAME
}


PRIMPROC
(mix_group_count, "group-count", 0, 1, 0,
 (SCM tag),
 doc: /***********
Return the number of channels in the default group.
Optional arg @var{tag} specifies the group to check.  */)
{
#define FUNC_NAME s_mix_group_count
  int ctag = -1;

  if (BOUNDP (tag))
    ASSERT_LONG_COPY (tag, 1);

  RETURN_INT (Mix_GroupCount (ctag));
#undef FUNC_NAME
}


PRIMPROC
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
    ASSERT_LONG_COPY (tag, 1);

  RETURN_INT (Mix_GroupOldest (ctag));
#undef FUNC_NAME
}


PRIMPROC
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
    ASSERT_LONG_COPY (tag, 1);

  RETURN_INT (Mix_GroupNewer (ctag));
#undef FUNC_NAME
}


PRIMPROC
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

  ASSERT_AUDIO (chunk, 1);
  cchunk = UNPACK_AUDIO (chunk);

  IF_BOUND_ASSERT_LONG_COPY (channel, 2);
  IF_BOUND_ASSERT_LONG_COPY (loops, 3);
  IF_BOUND_ASSERT_LONG_COPY (ticks, 4);

  if (UNBOUNDP (fade))
    /* No fade, normal Mix_PlayChannelTimed.  */
    rv = Mix_PlayChannelTimed (cchannel, cchunk, cloops, cticks);
  else
    {
      /* We have a fade.  */
      ASSERT_INTEGER (fade, 5);
      rv = Mix_FadeInChannelTimed (cchannel, cchunk, cloops,
                                   C_LONG (fade), cticks);
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
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

  ASSERT_MUSIC (music, 1);
  cmusic = UNPACK_MUSIC (music);

  IF_BOUND_ASSERT_LONG_COPY (loops, 2);

  if (UNBOUNDP (fade))
    /* No fade, normal Mix_PlayMusic.  */
    rv = Mix_PlayMusic (cmusic, cloops);
  else
    {
      /* We have a fade.  */
      ASSERT_INTEGER (fade, 3);
      rv = Mix_FadeInMusic (cmusic, cloops, C_LONG (fade));
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(mix_volume, "volume", 0, 2, 0,
 (SCM volume, SCM which),
 doc: /***********
Return the current volume on the default channel.
Optional arg @var{volume} (a number in the range 0-128) means
set the volume to @var{volume} and return the original volume.
Optional second arg @var{which} specifies a chunk or
channel to check (or modify) instead of the default.
If @var{volume} is non-@code{#f} and @var{which} is @code{#f}, modify all
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

  IF_BOUND_ASSERT_LONG_COPY (volume, 1);

  if (UNBOUNDP (which))
    /* No chunk or channel, call Mix_Volume on default channel.  */
    rv = Mix_Volume (-1, cvolume);
  else if (INTEGERP (which))
    /* Numeric which, treat as channel number.  */
    rv = Mix_Volume (C_LONG (which), cvolume);
  else
    {
      /* No-numeric which, must be a chunk smob.  */
      ASSERT_AUDIO (which, 2);
      rv = Mix_VolumeChunk (UNPACK_AUDIO (which), cvolume);
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
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
    ASSERT_LONG_COPY (volume, 1);

  RETURN_INT (Mix_VolumeMusic (cvolume));
#undef FUNC_NAME
}


PRIMPROC
(mix_halt_channel, "halt-channel", 0, 1, 0,
 (SCM channel),
 doc: /***********
Halt playing of the default channel.
Optional arg @var{channel} specifies a channel to halt.  */)
{
#define FUNC_NAME s_mix_halt_channel
  int cchannel = -1;

  if (BOUNDP (channel))
    ASSERT_LONG_COPY (channel, 1);

  RETURN_INT (Mix_HaltChannel (cchannel));
#undef FUNC_NAME
}


PRIMPROC
(mix_halt_group, "halt-group", 0, 1, 0,
 (SCM tag),
 doc: /***********
Halt playing of the default group.
Optional arg @var{tag} specifies the group to halt.  */)
{
#define FUNC_NAME s_mix_halt_group
  int ctag = -1;

  if (BOUNDP (tag))
    ASSERT_LONG_COPY (tag, 1);

  RETURN_INT (Mix_HaltGroup (ctag));
#undef FUNC_NAME
}


PRIMPROC
(mix_halt_music, "halt-music", 0, 0, 0,
 (void),
 doc: /***********
Halt playing of the music.  */)
{
#define FUNC_NAME s_mix_halt_music
  RETURN_INT (Mix_HaltMusic ());
#undef FUNC_NAME
}


PRIMPROC
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

  IF_BOUND_ASSERT_LONG_COPY (channel, 1);
  IF_BOUND_ASSERT_LONG_COPY (ticks, 2);

  RETURN_INT (Mix_ExpireChannel (cchannel, cticks));
#undef FUNC_NAME
}


PRIMPROC
(mix_fade_out_channel, "fade-out-channel", 0, 2, 0,
 (SCM which, SCM ms),
 doc: /***********
Halt a channel, fading it out progressively until silent.
Optional arg @var{which} specifies a channel to halt.
Second optional arg @var{ms} specifies the number of
milliseconds the fading will take (default 0).  */)
{
#define FUNC_NAME s_mix_fade_out_channel
  int cwhich = -1;
  int cms = 0;

  IF_BOUND_ASSERT_LONG_COPY (which, 1);
  IF_BOUND_ASSERT_LONG_COPY (ms, 2);

  RETURN_INT (Mix_FadeOutChannel (cwhich, cms));
#undef FUNC_NAME
}


PRIMPROC
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

  IF_BOUND_ASSERT_LONG_COPY (tag, 1);
  IF_BOUND_ASSERT_LONG_COPY (ms, 2);

  RETURN_INT (Mix_FadeOutGroup (ctag, cms));
#undef FUNC_NAME
}


PRIMPROC
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
    ASSERT_LONG_COPY (ms, 1);

  RETURN_INT (Mix_FadeOutMusic (cms));
#undef FUNC_NAME
}


PRIMPROC
(mix_fading_music, "fading-music", 0, 0, 0,
 (void),
 doc: /***********
Return the fading status of the music, one of the symbols:
@code{no}, @code{out}, @code{in}.  */)
{
#define FUNC_NAME s_mix_fading_music
  RETURN_FADINGSTATUS (Mix_FadingMusic ());
#undef FUNC_NAME
}


PRIMPROC
(mix_fading_channel, "fading-channel", 0, 1, 0,
 (SCM which),
 doc: /***********
Return the fading status (a symbol, see @code{fading-music})
of the default channel.
Optional arg @var{which} selects which channel to check.  */)
{
#define FUNC_NAME s_mix_fading_channel
  int cwhich = -1;

  if (BOUNDP (which))
    ASSERT_LONG_COPY (which, 1);

  RETURN_FADINGSTATUS (Mix_FadingChannel (cwhich));
#undef FUNC_NAME
}


PRIMPROC
(mix_pause, "pause", 0, 1, 0,
 (SCM channel),
 doc: /***********
Pause the default channel.
Optional arg @var{channel} selects which channel to pause.  */)
{
#define FUNC_NAME s_mix_pause
  int cchannel = -1;

  if (BOUNDP (channel))
    ASSERT_LONG_COPY (channel, 1);

  Mix_Pause (cchannel);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(mix_resume, "resume", 0, 1, 0,
 (SCM channel),
 doc: /***********
Resume (unpause) the default channel.
Optional arg @var{channel} selects which channel to resume.  */)
{
#define FUNC_NAME s_mix_resume
  int cchannel = -1;

  if (BOUNDP (channel))
    ASSERT_LONG_COPY (channel, 1);

  Mix_Resume (cchannel);
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(mix_paused, "paused?", 0, 1, 0,
 (SCM channel),
 doc: /***********
Return @code{#t} if the default channel is paused.
Optional arg @var{channel} selects a which channel to check.  */)
{
#define FUNC_NAME s_mix_paused
  int cchannel = -1;

  if (BOUNDP (channel))
    ASSERT_LONG_COPY (channel, 1);

  RETURN_BOOL
    (Mix_Paused (cchannel));
#undef FUNC_NAME
}


PRIMPROC
(mix_pause_music, "pause-music", 0, 0, 0,
 (void),
 doc: /***********
Pause the music.  */)
{
#define FUNC_NAME s_mix_pause_music
  Mix_PauseMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(mix_resume_music, "resume-music", 0, 0, 0,
 (void),
 doc: /***********
Resume (unpause) the music.  */)
{
#define FUNC_NAME s_mix_resume_music
  Mix_ResumeMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(mix_rewind_music, "rewind-music", 0, 0, 0,
 (void),
 doc: /***********
Rewind the music.  */)
{
#define FUNC_NAME s_mix_rewind_music
  Mix_RewindMusic ();
  RETURN_UNSPECIFIED;
#undef FUNC_NAME
}


PRIMPROC
(mix_paused_music, "paused-music?", 0, 0, 0,
 (void),
 doc: /***********
Return @code{#t} if the music is currently paused.  */)
{
#define FUNC_NAME s_mix_paused_music
  RETURN_BOOL
    (Mix_PausedMusic ());
#undef FUNC_NAME
}


PRIMPROC
(mix_playing, "playing?", 0, 1, 0,
 (SCM channel),
 doc: /***********
Return @code{#t} iff the default channel is playing.
Optional arg @var{channel} selects which channel to check.  */)
{
#define FUNC_NAME s_mix_playing
  int cchannel = -1;

  if (BOUNDP (channel))
    ASSERT_LONG_COPY (channel, 1);

  RETURN_BOOL
    (Mix_Playing (cchannel));
#undef FUNC_NAME
}


PRIMPROC
(mix_playing_music, "playing-music?", 0, 0, 0,
 (void),
 doc: /***********
Return @code{#t} iff the music is currently playing.  */)
{
#define FUNC_NAME s_mix_playing_music
  RETURN_BOOL
    (Mix_PlayingMusic ());
#undef FUNC_NAME
}


PRIMPROC
(mix_set_music_cmd, "set-music-command", 1, 0, 0,
 (SCM command),
 doc: /***********
Stop music and set external music playback command
to @var{command}, a string.  As a special case, if @var{command}
is @code{#f}, arrange to use internal playback, instead.  */)
{
#define FUNC_NAME s_mix_set_music_cmd
  range_t ccommand;
  int rv;

  if (EXACTLY_FALSEP (command))
    rv = Mix_SetMusicCMD (NULL);
  else
    {
      ASSERT_STRING (command, 1);
      FINANGLE (command);
      rv = Mix_SetMusicCMD (RS (command));
      UNFINANGLE (command);
    }
  RETURN_INT (rv);
#undef FUNC_NAME
}


PRIMPROC
(mix_set_panning, "set-panning", 3, 0, 0,
 (SCM channel, SCM l, SCM r),
 doc: /***********
Set panning for (stereo) @var{channel} with @var{l} and @var{r}.
Both @var{l} and @var{r} are integers 0--255, inclusive, where
0 is quietest and 255 is loudest.

To get ``true'' panning, use @code{(set-panning CH N (- 255 N))}.  */)
{
#define FUNC_NAME s_mix_set_panning
  ASSERT_INTEGER (channel, 1);
  ASSERT_INTEGER (l, 2);
  ASSERT_INTEGER (r, 3);

  RETURN_INT (Mix_SetPanning (C_LONG (channel), C_ULONG (l), C_ULONG (r)));
#undef FUNC_NAME
}


PRIMPROC
(mix_set_distance, "set-distance", 2, 0, 0,
 (SCM channel, SCM distance),
 doc: /***********
Set the ``distance'' of @var{channel} to @var{distance} (integer, 0--255).
This controls the location of the sound with respect to the listener.

Distance 0 is overlapping the listener, and 255 is as far away as possible.
A distance of 255 does not guarantee silence; in such a case, you might
want to try changing the chunk's volume, or just cull the sample from the
mixing process with @code{halt-channel}.

For efficiency, the precision of this effect may be limited (distances 1
through 7 might all produce the same effect, 8 through 15 are equal, etc).

Setting (distance) to 0 unregisters this effect, since the data would be
unchanged.  */)
{
#define FUNC_NAME s_mix_set_distance
  ASSERT_INTEGER (channel, 1);
  ASSERT_INTEGER (distance, 2);

  RETURN_INT (Mix_SetDistance (C_LONG (channel), C_ULONG (distance)));
#undef FUNC_NAME
}


PRIMPROC
(mix_set_position, "set-position", 3, 0, 0,
 (SCM channel, SCM angle, SCM distance),
 doc: /***********
Set the ``position'' of @var{channel} to @var{angle}, @var{distance}.
In this polar coordinate, @var{angle} is in degrees (integer modulo 360),
and @var{distance} is an integer 0--255 (and is treated as in proc
@code{set-distance} -- see notes there).

Angle 0 is due north, and rotates clockwise as the value increases.
For efficiency, the precision of this effect may be limited (angles 1
through 7 might all produce the same effect, 8 through 15 are equal, etc).

Setting @var{angle} and @var{distance} to 0 unregisters this effect,
since the data would be unchanged.

Additionally, the C header says:
@quotation
If the audio device is configured for mono output, then you won't get
any effectiveness from the angle; however, distance attenuation on the
channel will still occur. While this effect will function with stereo
voices, it makes more sense to use voices with only one channel of sound,
so when they are mixed through this effect, the positioning will sound
correct. You can convert them to mono through SDL before giving them to
the mixer in the first place if you like.
@end quotation  */)
{
#define FUNC_NAME s_mix_set_position
  ASSERT_INTEGER (channel, 1);
  ASSERT_INTEGER (angle, 2);
  ASSERT_INTEGER (distance, 3);

  RETURN_INT (Mix_SetPosition (C_LONG (channel), C_LONG (angle),
                               C_ULONG (distance)));
#undef FUNC_NAME
}


PRIMPROC
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
  DEFSMOB (mix_music_tag, mix_music_nick,
           NULL,
           free_music,
           NULL);

  DEFSMOB (mix_audio_tag, mix_audio_nick,
           NULL,
           free_audio,
           NULL);

#include "sdlmixer.x"

  btw = UNPACK_POINTER (CALL0 (obtw));

  {
    kp_init_t allp[] = {
      { &fading_status_enum, &fading_kp }
    };

    REGISTER_KP_V (allp);
  }
}

MOD_INIT_LINK_THUNK ("sdl mixer", sdl_mixer, init_module)

/* sdlmixer.c ends here */
