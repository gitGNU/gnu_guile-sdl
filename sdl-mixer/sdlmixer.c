/* sdlmixer.c --- SDL_mixer for Guile
 *
 * 	Copyright (C) 2003 Thien-Thi Nguyen
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
#include <SDL/SDL_mixer.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"


SCM fading_status_enum;


static long mix_music_tag;
static long mix_audio_tag;

#define ASSERT_MUSIC(obj,which)   ASSERT_SMOB (obj, mix_music_tag, which)
#define ASSERT_AUDIO(obj,which)   ASSERT_SMOB (obj, mix_audio_tag, which)

#define UNPACK_MUSIC(smob)   (SMOBGET (smob, Mix_Music *))
#define UNPACK_AUDIO(smob)   (SMOBGET (smob, Mix_Chunk *))

#define RETURN_NEW_MUSIC(x)   SCM_RETURN_NEWSMOB (mix_music_tag, x)
#define RETURN_NEW_AUDIO(x)   SCM_RETURN_NEWSMOB (mix_audio_tag, x)

static
SCM
mark_music (SCM s_music)
{
  return s_music;
}

static
size_t
free_music (SCM s_music)
{
  Mix_FreeMusic (UNPACK_MUSIC (s_music));
  return sizeof (struct Mix_Music*);
}

static
SCM
mark_audio (SCM s_chunk)
{
  return s_chunk;
}

static
size_t
free_audio (SCM s_chunk)
{
  Mix_FreeChunk (UNPACK_AUDIO (s_chunk));
  return sizeof (Mix_Chunk);
}


MDEFLOCEXP (mix_open_audio, "sdl-open-audio", 0, 4, 0,
            (SCM s_freq, SCM s_format, SCM s_stereo, SCM s_chunksize),
            "Open the mixer with a certain audio format.\n"
            "Optional args @var{freq} (number), @var{format} (number),\n"
            "@var{stereo} (boolean) and @var{chunksize} (number) specify\n"
            "those aspects of the device.  Return #t if successful.")
#define FUNC_NAME s_mix_open_audio
{
  int freq = MIX_DEFAULT_FREQUENCY;
  Uint16 format = MIX_DEFAULT_FORMAT;
  int channels = 2;
  int chunksize = 1024;

  if (! SCM_UNBNDP (s_freq)) {
    ASSERT_EXACT (s_freq, ARGH1);
    freq = gh_scm2long (s_freq);
  }

  if (! SCM_UNBNDP (s_format)) {
    ASSERT_EXACT (s_format, ARGH2);
    format = gh_scm2long (s_format);
  }

  channels -= SCM_UNBNDP (s_stereo) ? 0 : SCM_FALSEP (s_stereo);

  if (! SCM_UNBNDP (s_chunksize)) {
    ASSERT_EXACT (s_chunksize, ARGH4);
    chunksize = gh_scm2long (s_chunksize);
  }

  /* open the audio device */
  RETURN_TRUE_IF_0
    (Mix_OpenAudio (freq, MIX_DEFAULT_FORMAT, channels, 1024));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_allocate_channels, "sdl-allocated-channels", 1, 0, 0,
            (SCM s_numchans),
            "Dynamically change the number of channels managed by\n"
            "the mixer to @var{numchans}.  If decreasing the number\n"
            "of channels, the upper channels are stopped.  Return the\n"
            "new number of allocated channels.")
#define FUNC_NAME s_mix_allocate_channels
{
  ASSERT_EXACT (s_numchans, ARGH1);

  return gh_long2scm (Mix_AllocateChannels (gh_scm2long (s_numchans)));
}
#undef FUNC_NAME


SCM_SYMBOL (gsdl_sym_freq, "freq");
SCM_SYMBOL (gsdl_sym_format, "format");
SCM_SYMBOL (gsdl_sym_channels, "channels");

MDEFLOCEXP (mix_query_spec, "sdl-query-spec", 0, 0, 0,
            (void),
            "Return audio device parameters as an alist, or #f\n"
            "if the audio has not yet been opened.\n"
            "Keys are @code{freq} (frequency), @code{format},\n"
            "and @code{channels} (the number of allocated channels).")
#define FUNC_NAME s_mix_query_spec
{
  int freq, channels;
  Uint16 format;

  if (! Mix_QuerySpec (&freq, &format, &channels)) {
    return SCM_BOOL_F;
  }

  return SCM_LIST3 (gh_cons (gsdl_sym_freq, gh_long2scm (freq)),
                    gh_cons (gsdl_sym_format, gh_long2scm (format)),
                    gh_cons (gsdl_sym_channels, gh_long2scm (channels)));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_load_music, "sdl-load-music", 1, 0, 0,
            (SCM file),
            "Load a wave or a music (.mod .s3m .it .xm) @var{file}.\n"
            "Return a handle to it.")
#define FUNC_NAME s_mix_load_music
{
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_MUSIC
    (Mix_LoadMUS (SCM_CHARS (file)));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_load_wave, "sdl-load-wave", 1, 0, 0,
            (SCM file),
            "Load a wave @var{file}. Return a handle to it.")
#define FUNC_NAME s_mix_load_wave
{
  ASSERT_STRING (file, ARGH1);

  RETURN_NEW_AUDIO
    (Mix_LoadWAV (SCM_CHARS (file)));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_reserve_channels, "sdl-reserve-channels", 1, 0, 0,
            (SCM num),
            "Reserve the first @var{num} channels (0 through @var{num}-1)\n"
            "for the application.  I.E. don't allocate them dynamically to\n"
            "the next sample if requested with a -1 value below.\n"
            "Return the number of reserved channels.")
#define FUNC_NAME s_mix_reserve_channels
{
  ASSERT_EXACT (num, ARGH1);

  return gh_long2scm (Mix_ReserveChannels (gh_scm2long (num)));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_channel, "sdl-group-channel", 1, 1, 0,
            (SCM s_which, SCM s_tag),
            "Attach to @var{channel} a @var{tag}.\n"
            "A tag can be assigned to several mixer channels, to\n"
            "form groups of channels.  If @var{tag} is not specified, or\n"
            "is -1, the tag is removed (actually -1 is the tag used\n"
            "to represent the group of all the channels).  Return\n"
            "#t if successful.")
#define FUNC_NAME s_mix_group_channel
{
  int tag = -1;

  ASSERT_EXACT (s_which, ARGH1);

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH2);
    tag = gh_scm2long (s_tag);
  }

  return (Mix_GroupChannel (gh_scm2long (s_which), tag)
          ? SCM_BOOL_T
          : SCM_BOOL_F);
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_channels, "sdl-group-channels", 2, 1, 0,
            (SCM s_from, SCM s_to, SCM s_tag),
            "Assign channels in the range @var{from} through @var{to}\n"
            "to the default group.  Optional arg @var{tag} specifies\n"
            "the group to use.  Return #t if successful.")
#define FUNC_NAME s_mix_group_channels
{
  int tag = -1;

  ASSERT_EXACT (s_from, ARGH1);
  ASSERT_EXACT (s_to, ARGH2);

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH3);
    tag = gh_scm2long (s_tag);
  }

  return (Mix_GroupChannels (gh_scm2long (s_from),
                             gh_scm2long (s_to),
                             tag)
          ? SCM_BOOL_T
          : SCM_BOOL_F);
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_available, "sdl-group-available", 0, 1, 0,
            (SCM s_tag),
            "Return the first available channel in the default\n"
            "group of channels.\n"
            "Optional arg @var{tag} specifies the group to check.")
#define FUNC_NAME s_mix_group_available
{
  int tag = -1;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  return gh_long2scm (Mix_GroupAvailable (tag));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_count, "sdl-group-count", 0, 1, 0,
            (SCM s_tag),
            "Return the number of channels in the default group.\n"
            "Optional arg @var{tag} specifies the group to check.")
#define FUNC_NAME s_mix_group_count
{
  int tag = -1;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  return gh_long2scm (Mix_GroupCount (tag));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_oldest, "sdl-group-oldest", 0, 1, 0,
            (SCM s_tag),
            "Return the \"oldest\" sample playing in the default\n"
            "group of channels.\n"
            "Optional arg @var{tag} specifies the group to check.")
#define FUNC_NAME s_mix_group_oldest
{
  int tag = -1;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  return gh_long2scm (Mix_GroupOldest (tag));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_group_newer, "sdl-group-newer", 0, 1, 0,
            (SCM s_tag),
            "Return the \"most recent\" (i.e. last) sample playing\n"
            "in the default group of channels.\n"
            "Optional arg @var{tag} specifies the group to check.")
#define FUNC_NAME s_mix_group_newer
{
  int tag = -1;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  return gh_long2scm (Mix_GroupNewer (tag));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_play_channel, "sdl-play-channel", 1, 4, 0,
            (SCM s_chunk, SCM s_channel, SCM s_loops, SCM s_ticks, SCM s_fade),
            "Play an audio @var{chunk} on a specific @var{channel}.\n"
            "If the channel is unspecified or is -1, play on the\n"
            "first free channel.  If @var{loops} is specified and\n"
            "greater than zero, loop the sound that many times.  If\n"
            "@var{loops} is -1, loop infinitely (~65000 times).  If\n"
            "@var{ticks} is specified, stop after that number of ticks.\n"
            "If @var{fade} is specified, fade in over that number of\n"
            "milliseconds.  Return which channel was used to play\n"
            "the sound.")
#define FUNC_NAME s_mix_play_channel
{
  int channel = -1;
  Mix_Chunk *chunk;
  int loops = 0;
  int ticks = -1;
  long rv;

  ASSERT_AUDIO (s_chunk, ARGH1);
  chunk = UNPACK_AUDIO (s_chunk);

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH2);
    channel = gh_scm2long (s_channel);
  }

  if (! SCM_UNBNDP (s_loops)) {
    ASSERT_EXACT (s_loops, ARGH3);
    loops = gh_scm2long (s_loops);
  }

  if (! SCM_UNBNDP (s_ticks)) {
    ASSERT_EXACT (s_ticks, ARGH4);
    ticks = gh_scm2long (s_ticks);
  }

  if (SCM_UNBNDP (s_fade)) {
    /* no fade, normal Mix_PlayChannelTimed */
    rv = Mix_PlayChannelTimed (channel, chunk, loops, ticks);
  } else {
    /* we have a fade */
    ASSERT_EXACT (s_fade, ARGH5);
    rv = Mix_FadeInChannelTimed (channel, chunk, loops,
                                 gh_scm2long (s_fade),
                                 ticks);
  }
  return gh_long2scm (rv);
}
#undef FUNC_NAME


MDEFLOCEXP (mix_play_music, "sdl-play-music", 1, 2, 0,
            (SCM s_music, SCM s_loops, SCM s_fade),
            "Play a @var{music} track.\n"
            "Optional args @var{loops} and @var{fade}\n"
            "are as in @code{sdl-play-channel}.")
#define FUNC_NAME s_mix_play_music
{
  Mix_Music *music;
  int loops = 0;
  long rv;

  ASSERT_MUSIC (s_music, ARGH1);
  music = UNPACK_MUSIC (s_music);

  if (! SCM_UNBNDP (s_loops)) {
    ASSERT_EXACT (s_loops, ARGH2);
    loops = gh_scm2long (s_loops);
  }

  if (SCM_UNBNDP (s_fade)) {
    /* no fade, normal Mix_PlayMusic */
    rv = Mix_PlayMusic (music, loops);
  } else {
    /* we have a fade */
    ASSERT_EXACT (s_fade, ARGH3);
    rv = Mix_FadeInMusic (music, loops, gh_scm2long (s_fade));
  }
  return gh_long2scm (rv);
}
#undef FUNC_NAME


MDEFLOCEXP (mix_volume, "sdl-volume", 0, 2, 0,
            (SCM s_volume, SCM s_which),
            "Return the current volume on the default channel.\n"
            "Optional arg @var{v} (a number in the range 0-128) means\n"
            "set the volume to @var{v} and return the original volume.\n"
            "Optional second arg @var{which} specifies a chunk or\n"
            "channel to check (or modify) instead of the default.\n"
            "If @var{v} is non-#f and @var{which} is #f, modify all\n"
            "channels.\n\n"
            "[Here is the original (perhaps clearer) docstring. ---ttn]\n\n"
            "Set the volume in the range of 0-128 of a specific channel\n"
            "or chunk.  If the channel is unspecified or is -1, set volume\n"
            "for all channels.  Return the original volume.  If the volume\n"
            "is unspecified or is -1, just return the current volume.")
#define FUNC_NAME s_mix_volume
{
  int volume = -1;
  long rv;

  if (! SCM_UNBNDP (s_volume)) {
    ASSERT_EXACT (s_volume, ARGH1);
    volume = gh_scm2long (s_volume);
  }

  if (SCM_UNBNDP (s_which)) {
    /* no chunk or channel, call Mix_Volume on default channel */
    rv = Mix_Volume (-1, volume);
  } else if (gh_exact_p (s_which)) {
    /* numeric which, treat as channel number */
    rv = Mix_Volume (gh_scm2long (s_which), volume);
  } else {
    /* no-numeric which, must be a chunk smob */
    ASSERT_AUDIO (s_which, ARGH2);
    rv = Mix_VolumeChunk (UNPACK_AUDIO (s_which), volume);
  }
  return gh_long2scm (rv);
}
#undef FUNC_NAME


MDEFLOCEXP (mix_volume_music, "sdl-music-volume", 0, 1, 0,
            (SCM s_volume),
            "Return the current volume.\n"
            "Optional arg @var{v} (a number in the range 0-128)\n"
            "means set the volume to @var{v}.")
#define FUNC_NAME s_mix_volume_music
{
  int volume = -1;

  if (! SCM_UNBNDP (s_volume)) {
    ASSERT_EXACT (s_volume, ARGH1);
    volume = gh_scm2long (s_volume);
  }

  return gh_long2scm (Mix_VolumeMusic (volume));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_halt_channel, "sdl-halt-channel", 0, 1, 0,
            (SCM s_channel),
            "Halt playing of the default channel.\n"
            "Optional arg @var{channel} specifies a channel to halt.")
#define FUNC_NAME s_mix_halt_channel
{
  int channel = -1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  return gh_long2scm (Mix_HaltChannel (channel));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_halt_group, "sdl-halt-group", 0, 1, 0,
            (SCM s_tag),
            "Halt playing of the default group.\n"
            "Optional arg @var{tag} specifies the group to halt.")
#define FUNC_NAME s_mix_halt_group
{
  int tag = -1;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  return gh_long2scm (Mix_HaltGroup (tag));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_halt_music, "sdl-halt-music", 0, 0, 0,
            (void),
            "Halt playing of the music.")
#define FUNC_NAME s_mix_halt_music
{
  return gh_long2scm (Mix_HaltMusic ());
}
#undef FUNC_NAME


MDEFLOCEXP (mix_expire_channel, "sdl-expire-channel", 0, 2, 0,
            (SCM s_channel, SCM s_ticks),
            "Turn off expiration for the default channel.\n"
            "Optional arg @var{channel} specifies a channel to change.\n"
            "Optional arg @var{ticks} (a number) means set the expiration\n"
            "delay to that many milliseconds, rather than turning it off.")
#define FUNC_NAME s_mix_expire_channel
{
  int channel = -1;
  int ticks = -1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  if (! SCM_UNBNDP (s_ticks)) {
    ASSERT_EXACT (s_ticks, ARGH2);
    ticks = gh_scm2long (s_ticks);
  }

  return gh_long2scm (Mix_ExpireChannel (channel, ticks));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_fade_out_channel, "sdl-fade-out-channel", 0, 2, 0,
            (SCM s_which, SCM s_ms),
            "Halt a channel, fading it out progressively until silent.\n"
            "Optional arg @var{which} specifies a channel to halt.\n"
            "Second optional arg @var{ms} specifies the number of\n"
            "milliseconds the fading will take (default 0).")
#define FUNC_NAME s_mix_fade_out_channel
{
  int channel = -1;
  int ms = 0;

  if (! SCM_UNBNDP (s_which)) {
    ASSERT_EXACT (s_which, ARGH1);
    channel = gh_scm2long (s_which);
  }

  if (! SCM_UNBNDP (s_ms)) {
    ASSERT_EXACT (s_ms, ARGH2);
    ms = gh_scm2long (s_ms);
  }

  return gh_long2scm (Mix_FadeOutChannel (channel, ms));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_fade_out_group, "sdl-fade-out-group", 0, 2, 0,
            (SCM s_tag, SCM s_ms),
            "Halt a group, fading it out progressively until silent.\n"
            "Optional arg @var{tag} specifies a group to halt.\n"
            "Second optional arg @var{ms} specifies the number of\n"
            "milliseconds the fading will take (default 0).")
#define FUNC_NAME s_mix_fade_out_group
{
  int tag = -1;
  int ms = 0;

  if (! SCM_UNBNDP (s_tag)) {
    ASSERT_EXACT (s_tag, ARGH1);
    tag = gh_scm2long (s_tag);
  }

  if (! SCM_UNBNDP (s_ms)) {
    ASSERT_EXACT (s_ms, ARGH2);
    ms = gh_scm2long (s_ms);
  }

  return gh_long2scm (Mix_FadeOutGroup (tag, ms));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_fade_out_music, "sdl-fade-out-music", 0, 1, 0,
            (SCM s_ms),
            "Halt the music, fading it out progressively until silent.\n"
            "Optional arg @var{ms} specifies the number of milliseconds\n"
            "the fading will take (default 0).")
#define FUNC_NAME s_mix_fade_out_music
{
  int ms = 0;

  if (! SCM_UNBNDP (s_ms)) {
    ASSERT_EXACT (s_ms, ARGH1);
    ms = gh_scm2long (s_ms);
  }

  return gh_long2scm (Mix_FadeOutMusic (ms));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_fading_music, "sdl-fading-music", 0, 0, 0,
            (void),
            "Return the fading status of the music.")
#define FUNC_NAME s_mix_fading_music
{
  return gh_long2scm (Mix_FadingMusic ());
}
#undef FUNC_NAME


MDEFLOCEXP (mix_fading_channel, "sdl-fading-channel", 0, 1, 0,
            (SCM s_which),
            "Return the fading status of a the default channel."
            "Optional arg @var{which} selects which channel to check.")
#define FUNC_NAME s_mix_fading_channel
{
  int which = -1;

  if (! SCM_UNBNDP (s_which)) {
    ASSERT_EXACT (s_which, ARGH1);
    which = gh_scm2long (s_which);
  }

  return gh_long2scm (Mix_FadingChannel (which));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_pause, "sdl-pause", 0, 1, 0,
            (SCM s_channel),
            "Pause the default channel."
            "Optional arg @var{channel} selects which channel to pause.\n"
            "Return value unspecified.")
#define FUNC_NAME s_mix_pause
{
  int channel = -1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  Mix_Pause (channel);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_resume, "sdl-resume", 0, 1, 0,
            (SCM s_channel),
            "Resume (unpause) the default channel.\n"
            "Optional arg @var{channel} selects which channel to resume.\n"
            "Return value unspecified.")
#define FUNC_NAME s_mix_resume
{
  int channel=-1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  Mix_Resume (channel);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_paused, "sdl-paused?", 0, 1, 0,
            (SCM s_channel),
            "Return #t if the default channel is paused.\n"
            "Optional arg @var{channel} selects a which channel to check.")
#define FUNC_NAME s_mix_resume
{
  int channel = -1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  return Mix_Paused (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_pause_music, "sdl-pause-music", 0, 0, 0,
            (void),
            "Pause the music.  Return value unspecified.")
#define FUNC_NAME s_mix_pause_music
{
  Mix_PauseMusic ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_resume_music, "sdl-resume-music", 0, 0, 0,
            (void),
            "Resume (unpause) the music.  Return value unspecified.")
#define FUNC_NAME s_mix_resume_music
{
  Mix_ResumeMusic ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_rewind_music, "sdl-rewind-music", 0, 0, 0,
            (void),
            "Rewind the music.  Return value unspecified.")
#define FUNC_NAME s_mix_rewind_music
{
  Mix_RewindMusic ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_paused_music, "sdl-paused-music?", 0, 0, 0,
            (void),
            "Return #t if the music is currently paused.")
#define FUNC_NAME s_mix_paused_music
{
  return Mix_PausedMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_playing, "sdl-playing?", 0, 1, 0,
            (SCM s_channel),
            "Return #t iff the default channel is playing.\n"
            "Optional arg @var{channel} selects which channel to check.")
#define FUNC_NAME s_mix_playing
{
  int channel = -1;

  if (! SCM_UNBNDP (s_channel)) {
    ASSERT_EXACT (s_channel, ARGH1);
    channel = gh_scm2long (s_channel);
  }

  return Mix_Playing (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_playing_music, "sdl-playing-music?", 0, 0, 0,
            (void),
            "Return #t iff the music is currently playing.")
#define FUNC_NAME s_mix_playing_music
{
  return Mix_PlayingMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


MDEFLOCEXP (mix_set_music_cmd, "sdl-set-music-command", 1, 0, 0,
            (SCM command),
            "Stop music and set external music playback command\n"
            "to @var{command}, a string.")
#define FUNC_NAME s_mix_set_music_cmd
{
  ASSERT_STRING (command, ARGH1);
  return gh_long2scm (Mix_SetMusicCMD (SCM_CHARS (command)));
}
#undef FUNC_NAME


MDEFLOCEXP (mix_close_audio, "sdl-close-audio", 0, 0, 0,
            (void),
            "Close the mixer, halting all playing audio.")
#define FUNC_NAME s_mix_close_audio
{
  Mix_CloseAudio ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* initialize the mixer subsystem */
static
void
init_module (void)
{
  /* smobs */
  mix_music_tag = scm_make_smob_type ("sdl-music", sizeof (struct Mix_Music*));
  scm_set_smob_mark (mix_music_tag, mark_music);
  scm_set_smob_free (mix_music_tag, free_music);

  mix_audio_tag = scm_make_smob_type ("sdl-audio", sizeof (Mix_Chunk));
  scm_set_smob_mark (mix_audio_tag, mark_audio);
  scm_set_smob_free (mix_audio_tag, free_audio);

  /* enums */
  fading_status_enum = gsdl_define_enum (
      "sdl-fading-status",
      "MIX_NO_FADING",      MIX_NO_FADING,
      "MIX_FADING_OUT",     MIX_FADING_OUT,
      "MIX_FADING_IN",      MIX_FADING_IN,
      NULL);

#include "sdlmixer.x"
}

MDEFLINKFUNC ("sdl mixer", sdl_mixer, init_module)

/* sdlmixer.c ends here */
