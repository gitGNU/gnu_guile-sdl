/*******************************************************************
 *  sdlmixer.c -- SDL_mixer for Guile                              *
 *                                                                 *
 *  Created:    <2001-06-10 16:45:57 foof>                         *
 *  Time-stamp: <2001-08-05 15:27:12 foof>                         *
 *                                                                 *
 *  This program is free software; you can redistribute it and/or  *
 * modify it under the terms of the GNU General Public License as  *
 * published by the Free Software Foundation; either version 2 of  *
 * the License, or (at your option) any later version.             *
 *                                                                 *
 * This program is distributed in the hope that it will be useful, *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of  *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   *
 * GNU General Public License for more details.                    *
 *                                                                 *
 * You should have received a copy of the GNU General Public       *
 * License along with this program; if not, write to the Free      *
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,  *
 * MA 02111-1307 USA                                               *
 ******************************************************************/

/* guile headers */
#include <sdlmixer.h>
#include <sdlenums.h>
#include <sdlsmobs.h>

long mix_music_tag;
long mix_audio_tag;
SCM fading_status_enum;

scm_sizet
free_music (SCM s_music)
{
   Mix_Music *music = (Mix_Music*) SCM_SMOB_DATA (s_music);
   Mix_FreeMusic (music);
   return sizeof (struct Mix_Music*);
}

scm_sizet
free_audio (SCM s_chunk)
{
   Mix_Chunk *chunk = (Mix_Chunk*) SCM_SMOB_DATA (s_chunk);
   Mix_FreeChunk (chunk);
   return sizeof (Mix_Chunk);
}


SCM_DEFINE( mix_open_audio, "sdl-open-audio", 0, 4, 0,
            (SCM s_freq,
             SCM s_format,
             SCM s_stereo,
             SCM s_chunksize),
"Open the mixer with a certain audio format.")
#define FUNC_NAME s_mix_open_audio
{
   int freq = MIX_DEFAULT_FREQUENCY;
   Uint16 format = MIX_DEFAULT_FORMAT;
   int channels = 2;
   int chunksize = 1024;

   /* handle optional args */
   if (s_freq != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_freq),  s_freq,  SCM_ARG1, "sdl-open-audio");
      freq = scm_num2long (s_freq, SCM_ARG1, "scm_num2long");
   }

   if (s_format != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_format),  s_format,  SCM_ARG2, "sdl-open-audio");
      format = scm_num2long (s_format, SCM_ARG1, "scm_num2long");
   }

   if (s_stereo != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_BOOLP (s_stereo),  s_stereo,  SCM_ARG3, "sdl-open-audio");
      if (SCM_FALSEP (s_stereo)) {
         channels = 1;
      }
   }

   if (s_chunksize != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_chunksize),  s_format,  SCM_ARG4, "sdl-open-audio");
      chunksize = scm_num2long (s_chunksize, SCM_ARG1, "scm_num2long");
   }

   /* open the audio device */
   return (Mix_OpenAudio (freq, MIX_DEFAULT_FORMAT, channels, 1024) == 0)
      ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE( mix_allocate_channels, "sdl-allocated-channels", 1, 0, 0,
            (SCM s_numchans),
"Dynamically change the number of channels managed by the mixer.
If decreasing the number of channels, the upper channels are stopped.
This function returns the new number of allocated channels.")
#define FUNC_NAME s_mix_allocate_channels
{
   int numchans;

   SCM_ASSERT (scm_exact_p (s_numchans),  s_numchans,  SCM_ARG1, "sdl-allocate-channels");
   numchans = scm_num2long (s_numchans, SCM_ARG1, "scm_num2long");

   return scm_long2num (Mix_AllocateChannels (numchans));
}
#undef FUNC_NAME


SCM_DEFINE( mix_query_spec, "sdl-query-spec", 0, 0, 0,
            (),
"Find out what the actual audio device parameters are.
If the audio has not been opened, returns #f.  Otherwise returns the
audio parameters as an alist with the keys 'freq (frequency), 'format,
and 'channels (the number of allocated channels).")
#define FUNC_NAME s_mix_query_spec
{
  int freq, channels;
  Uint16 format;

  if (! Mix_QuerySpec (&freq, &format, &channels)) {
    return SCM_BOOL_F;
  }

  return SCM_LIST3 (scm_cons (scm_str2symbol ("freq"),
                              scm_long2num (freq)),
                    scm_cons (scm_str2symbol ("format"),
                              scm_long2num (format)),
                    scm_cons (scm_str2symbol ("channels"),
                              scm_long2num (channels)));
}
#undef FUNC_NAME


SCM_DEFINE (mix_load_music, "sdl-load-music", 1, 0, 0,
            (SCM file),
"Load a wave file or a music (.mod .s3m .it .xm) file.")
#define FUNC_NAME s_mix_load_music
{
  Mix_Music *music;

  SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
              file, SCM_ARG1, "sdl-load-music");

  music = Mix_LoadMUS (SCM_CHARS (file));
  SCM_RETURN_NEWSMOB (mix_music_tag, music);
}
#undef FUNC_NAME


SCM_DEFINE (mix_load_wave, "sdl-load-wave", 1, 0, 0,
            (SCM file),
"Load a wave file.")
#define FUNC_NAME s_mix_load_wave
{
   Mix_Chunk *chunk;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "sdl-load-wave");

   chunk = Mix_LoadWAV (SCM_CHARS (file));
   SCM_RETURN_NEWSMOB (mix_audio_tag, chunk);
}
#undef FUNC_NAME


SCM_DEFINE (mix_reserve_channels, "sdl-reserve-channels", 1, 0, 0,
            (SCM num),
"Reserve the first channels (0 -> num-1) for the application.
I.E. don't allocate them dynamically to the next sample if requested
with a -1 value below.  Returns the number of reserved channels.")
#define FUNC_NAME s_mix_reserve_channels
{
   SCM_ASSERT (scm_exact_p (num),  num,  SCM_ARG1, "sdl-reserve-channels");
   return scm_long2num (Mix_ReserveChannels (scm_num2long (num, SCM_ARG1, "scm_num2long")));
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_channel, "sdl-group-channel", 1, 1, 0,
            (SCM s_which,
             SCM s_tag),
"Attach a tag to a channel.
A tag can be assigned to several mixer channels, to form groups of
channels.  If 'tag' is not specified, or is -1, the tag is removed
(actually -1 is the tag used to represent the group of all the
channels).  Returns #t if everything was OK.")
#define FUNC_NAME s_mix_group_channel
{
   int which, tag=-1;

   SCM_ASSERT (scm_exact_p (s_which), s_which, SCM_ARG1, "sdl-group-channel");
   which = scm_num2long (s_which, SCM_ARG1, "scm_num2long");

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG2, "sdl-group-channel");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return Mix_GroupChannel (which, tag) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_channels, "sdl-group-channels", 2, 1, 0,
            (SCM s_from,
             SCM s_to,
             SCM s_tag),
"Assign several consecutive channels to a group.")
#define FUNC_NAME s_mix_group_channels
{
   int from, to, tag=-1;

   SCM_ASSERT (scm_exact_p (s_from), s_from, SCM_ARG1, "sdl-group-channels");
   from = scm_num2long (s_from, SCM_ARG1, "scm_num2long");

   SCM_ASSERT (scm_exact_p (s_to), s_to, SCM_ARG2, "sdl-group-channels");
   to = scm_num2long (s_to, SCM_ARG1, "scm_num2long");

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG3, "sdl-group-channels");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return Mix_GroupChannels (from, to, tag) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_available, "sdl-group-available", 0, 1, 0,
            (SCM s_tag),
"Finds the first available channel in a group of channels.")
#define FUNC_NAME s_mix_group_available
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-group-available");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_GroupAvailable (tag));
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_count, "sdl-group-count", 0, 1, 0,
            (SCM s_tag),
"Returns the number of channels in a group.
This is also a subtle way to get the total number of channels when
'tag' is not given or is -1.")
#define FUNC_NAME s_mix_group_count
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-group-count");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_GroupCount (tag));
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_oldest, "sdl-group-oldest", 0, 1, 0,
            (SCM s_tag),
"Finds the 'oldest' sample playing in a group of channels.")
#define FUNC_NAME s_mix_group_oldest
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-group-oldest");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_GroupOldest (tag));
}
#undef FUNC_NAME


SCM_DEFINE (mix_group_newer, "sdl-group-newer", 0, 1, 0,
            (SCM s_tag),
"Finds the 'most recent' (i.e. last) sample playing in a group of channels.")
#define FUNC_NAME s_mix_group_newer
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-group-newer");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_GroupNewer (tag));
}
#undef FUNC_NAME


SCM_DEFINE (mix_play_channel, "sdl-play-channel", 1, 4, 0,
            (SCM s_chunk,
             SCM s_channel,
             SCM s_loops,
             SCM s_ticks,
             SCM s_fade),
"Play an audio chunk on a specific channel.
If the channel is unspecified or is -1, play on the first free
channel.  If 'loops' is specified and greater than zero, loop the
sound that many times.  If 'loops' is -1, loop inifinitely (~65000
times).  If 'ticks' is specified, stop after that number of ticks.  If
'fade' is specified, fade in over that number of milliseconds.
Returns which channel was used to play the sound.")
#define FUNC_NAME s_mix_play_channel
{
   int channel=-1;
   Mix_Chunk *chunk;
   int loops=0;
   int ticks=-1;
   int fade=0;

   SCM_ASSERT_SMOB (s_chunk, mix_audio_tag, SCM_ARG1, "sdl-play-channel");
   chunk = (Mix_Chunk*) SCM_SMOB_DATA (s_chunk);

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG2, "sdl-play-channel");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   if (s_loops != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_loops), s_loops, SCM_ARG3, "sdl-play-channel");
      loops = scm_num2long (s_loops, SCM_ARG1, "scm_num2long");
   }

   if (s_ticks != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_ticks), s_ticks, SCM_ARG4, "sdl-play-channel");
      ticks = scm_num2long (s_ticks, SCM_ARG1, "scm_num2long");
   }

   if (s_fade == SCM_UNDEFINED) {
      /* no fade, normal Mix_PlayChannelTimed */
      return scm_long2num (Mix_PlayChannelTimed (channel, chunk, loops, ticks));
   } else {
      /* we have a fade */
      SCM_ASSERT (scm_exact_p (s_fade), s_fade, SCM_ARG5, "sdl-play-channel");
      fade = scm_num2long (s_fade, SCM_ARG1, "scm_num2long");
      return scm_long2num (Mix_FadeInChannelTimed (channel, chunk, loops, fade, ticks));
   }
}
#undef FUNC_NAME


SCM_DEFINE (mix_play_music, "sdl-play-music", 1, 2, 0,
            (SCM s_music,
             SCM s_loops,
             SCM s_fade),
"Play a music track.
Loops and fade are as in sdl-play-channel.")
#define FUNC_NAME s_mix_play_music
{
   Mix_Music *music;
   int loops=0;
   int fade=0;

   SCM_ASSERT_SMOB (s_music, mix_music_tag, SCM_ARG1, "sdl-play-music");
   music = (Mix_Music*) SCM_SMOB_DATA (s_music);

   if (s_loops != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_loops), s_loops, SCM_ARG2, "sdl-play-music");
      loops = scm_num2long (s_loops, SCM_ARG1, "scm_num2long");
   }

   if (s_fade == SCM_UNDEFINED) {
      /* no fade, normal Mix_PlayMusic */
      return scm_long2num (Mix_PlayMusic (music, loops));
   } else {
      /* we have a fade */
      SCM_ASSERT (scm_exact_p (s_fade), s_fade, SCM_ARG3, "sdl-play-music");
      fade = scm_num2long (s_fade, SCM_ARG1, "scm_num2long");
      return scm_long2num (Mix_FadeInMusic (music, loops, fade));
   }
}
#undef FUNC_NAME


SCM_DEFINE (mix_volume, "sdl-volume", 0, 2, 0,
            (SCM s_volume,
             SCM s_which),
"Set the volume in the range of 0-128 of a specific channel or chunk.
If the channel is unspecified or is -1, set volume for all channels.
Returns the original volume.
If the volume is unspecified or is -1, just return the current volume.")
#define FUNC_NAME s_mix_volume
{
   int volume=-1;
   int channel=-1;
   Mix_Chunk *chunk;

   if (s_volume != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_volume), s_volume, SCM_ARG1, "sdl-volume");
      volume = scm_num2long (s_volume, SCM_ARG1, "scm_num2long");
   }

   if (s_which == SCM_UNDEFINED) {
      /* no chunk or channel, call Mix_Volume on default channel */
      return scm_long2num (Mix_Volume (channel, volume));
   } else if (scm_exact_p (s_which)) {
      /* numeric which, treat as channel number */
      channel = scm_num2long (s_which, SCM_ARG1, "scm_num2long");
      return scm_long2num (Mix_Volume (channel, volume));
   } else {
      /* no-numeric which, must be a chunk smob */
      SCM_ASSERT_SMOB (s_which, mix_audio_tag, SCM_ARG2, "sdl-volume");
      chunk = (Mix_Chunk*) SCM_SMOB_DATA (s_which);
      return scm_long2num (Mix_VolumeChunk (chunk, volume));
   }
}
#undef FUNC_NAME


SCM_DEFINE (mix_volume_music, "sdl-music-volume", 0, 1, 0,
            (SCM s_volume),
"Set the volume in the range of 0-128 for the music.
If the volume is unspecified or is -1, just return the current volume.")
#define FUNC_NAME s_mix_volume_music
{
   int volume=-1;

   if (s_volume != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_volume), s_volume, SCM_ARG1, "sdl-music-volume");
      volume = scm_num2long (s_volume, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_VolumeMusic (volume));
}
#undef FUNC_NAME


SCM_DEFINE (mix_halt_channel, "sdl-halt-channel", 0, 1, 0,
            (SCM s_channel),
"Halt playing of a particular channel.")
#define FUNC_NAME s_mix_halt_channel
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-halt-channel");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_HaltChannel (channel));
}
#undef FUNC_NAME


SCM_DEFINE (mix_halt_group, "sdl-halt-group", 0, 1, 0,
            (SCM s_tag),
"Halt playing of a particular group.")
#define FUNC_NAME s_mix_halt_group
{
   int tag=-1;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-halt-group");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_HaltGroup (tag));
}
#undef FUNC_NAME


SCM_DEFINE (mix_halt_music, "sdl-halt-music", 0, 0, 0,
            (),
"Halt playing of the music.")
#define FUNC_NAME s_mix_halt_music
{
   return scm_long2num (Mix_HaltMusic ());
}
#undef FUNC_NAME


SCM_DEFINE (mix_expire_channel, "sdl-expire-channel", 0, 2, 0,
            (SCM s_channel,
             SCM s_ticks),
"Change the expiration delay for a particular channel.
The sample will stop playing after the 'ticks' milliseconds have
elapsed, or remove the expiration if 'ticks' is -1")
#define FUNC_NAME s_mix_expire_channel
{
   int channel=-1;
   int ticks=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-expire-channel");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   if (s_ticks != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_ticks), s_ticks, SCM_ARG2, "sdl-expire-channel");
      ticks = scm_num2long (s_ticks, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_ExpireChannel (channel, ticks));
}
#undef FUNC_NAME


SCM_DEFINE (mix_fade_out_channel, "sdl-fade-out-channel", 0, 2, 0,
            (SCM s_which,
             SCM s_ms),
"Halt a channel, fading it out progressively till it's silent.
The ms parameter indicates the number of milliseconds the fading will
take (default 0).")
#define FUNC_NAME s_mix_fade_out_channel
{
   int channel=-1;
   int ms=0;

   if (s_which != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_which), s_which, SCM_ARG1, "sdl-fade-out-channel");
      channel = scm_num2long (s_which, SCM_ARG1, "scm_num2long");
   }

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_ms), s_ms, SCM_ARG2, "sdl-fade-out-channel");
      ms = scm_num2long (s_ms, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_FadeOutChannel (channel, ms));
}
#undef FUNC_NAME


SCM_DEFINE (mix_fade_out_group, "sdl-fade-out-group", 0, 2, 0,
            (SCM s_tag,
             SCM s_ms),
"Halt a group, fading it out progressively till it's silent.
The ms parameter indicates the number of milliseconds the fading will
take (default 0).")
#define FUNC_NAME s_mix_fade_out_group
{
   int tag=-1;
   int ms=0;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_tag), s_tag, SCM_ARG1, "sdl-fade-out-group");
      tag = scm_num2long (s_tag, SCM_ARG1, "scm_num2long");
   }

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_ms), s_ms, SCM_ARG2, "sdl-fade-out-group");
      ms = scm_num2long (s_ms, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_FadeOutGroup (tag, ms));
}
#undef FUNC_NAME


SCM_DEFINE (mix_fade_out_music, "sdl-fade-out-music", 0, 1, 0,
            (SCM s_ms),
"Halt the music, fading it out progressively till it's silent.
The ms parameter indicates the number of milliseconds the fading will
take (default 0).")
#define FUNC_NAME s_mix_fade_out_music
{
   int ms=0;

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_ms), s_ms, SCM_ARG1, "sdl-fade-out-music");
      ms = scm_num2long (s_ms, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_FadeOutMusic (ms));
}
#undef FUNC_NAME


SCM_DEFINE (mix_fading_music, "sdl-fading-music", 0, 0, 0,
            (),
"Query the fading status of the music.")
#define FUNC_NAME s_mix_fading_music
{
   return scm_long2num (Mix_FadingMusic ());
}


SCM_DEFINE (mix_fading_channel, "sdl-fading-channel", 0, 1, 0,
            (SCM s_which),
"Query the fading status of a channel.")
#define FUNC_NAME s_mix_fading_channel
{
   int which=-1;

   if (s_which != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_which), s_which, SCM_ARG1, "sdl-fading-channel");
      which = scm_num2long (s_which, SCM_ARG1, "scm_num2long");
   }

   return scm_long2num (Mix_FadingChannel (which));
}
#undef FUNC_NAME


SCM_DEFINE (mix_pause, "sdl-pause", 0, 1, 0,
            (SCM s_channel),
"Pause a particular channel.")
#define FUNC_NAME s_mix_pause
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-pause");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   Mix_Pause (channel);

   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (mix_resume, "sdl-resume", 0, 1, 0,
            (SCM s_channel),
"Resume (unpause) a particular channel.")
#define FUNC_NAME s_mix_resume
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-resume");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   Mix_Resume (channel);

   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (mix_paused, "sdl-paused?", 0, 1, 0,
            (SCM s_channel),
"Query the paused status of a particular channel.")
#define FUNC_NAME s_mix_resume
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-paused?");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   return Mix_Paused (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_pause_music, "sdl-pause-music", 0, 0, 0,
            (),
"Pause the music.")
#define FUNC_NAME s_mix_pause_music
{
   Mix_PauseMusic ();
   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (mix_resume_music, "sdl-resume-music", 0, 0, 0,
            (),
"Resume (unpause) the music.")
#define FUNC_NAME s_mix_resume_music
{
   Mix_ResumeMusic ();
   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (mix_rewind_music, "sdl-rewind-music", 0, 0, 0,
            (),
"Rewind the music.")
#define FUNC_NAME s_mix_rewind_music
{
   Mix_RewindMusic ();
   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


SCM_DEFINE (mix_paused_music, "sdl-paused-music?", 0, 0, 0,
            (),
"Query the paused status of the music.")
#define FUNC_NAME s_mix_paused_music
{
   return Mix_PausedMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_playing, "sdl-playing?", 0, 1, 0,
            (SCM s_channel),
"Check the playing status of a specific channel.")
#define FUNC_NAME s_mix_playing
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (scm_exact_p (s_channel), s_channel, SCM_ARG1, "sdl-playing?");
      channel = scm_num2long (s_channel, SCM_ARG1, "scm_num2long");
   }

   return Mix_Playing (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_playing_music, "sdl-playing-music?", 0, 0, 0,
            (),
"Check the playing status of the music.")
#define FUNC_NAME s_mix_playing_music
{
   return Mix_PlayingMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (mix_set_music_cmd, "sdl-set-music-command", 1, 0, 0,
            (SCM command),
"Stop music and set external music playback command.")
#define FUNC_NAME s_mix_set_music_cmd
{
   SCM_ASSERT ((SCM_NIMP (command) && SCM_STRINGP (command)),
               command, SCM_ARG1, "sdl-set-music-command");
   return scm_long2num (Mix_SetMusicCMD (SCM_CHARS (command)));
}
#undef FUNC_NAME


SCM_DEFINE (mix_close_audio, "sdl-close-audio", 0, 0, 0,
            (),
"Close the mixer, halting all playing audio.")
#define FUNC_NAME s_mix_close_audio
{
   Mix_CloseAudio ();
   return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* initialize the mixer subsystem */
void
sdl_mixer_init (void)
{
   /* smobs */
   mix_music_tag = scm_make_smob_type ("sdl-music", sizeof (struct Mix_Music*));
   mix_audio_tag = scm_make_smob_type ("sdl-audio", sizeof (Mix_Chunk));
   scm_set_smob_free (mix_music_tag, free_music);
   scm_set_smob_free (mix_audio_tag, free_audio);

   /* enums */
   fading_status_enum = scm_c_define_enum (
      "sdl-fading-status",
      "MIX_NO_FADING",      MIX_NO_FADING,
      "MIX_FADING_OUT",     MIX_FADING_OUT,
      "MIX_FADING_IN",      MIX_FADING_IN,
      NULL);

   /* exported symbols */
   scm_c_export (
      "sdl-fading-status",
      "sdl-open-audio",
      "sdl-allocate-channels",
      "sdl-query-spec",
      "sdl-load-music",
      "sdl-load-wave",
      "sdl-reserve-channels",
      "sdl-group-channel",
      "sdl-group-channels",
      "sdl-group-available",
      "sdl-group-count",
      "sdl-group-oldest",
      "sdl-group-newer",
      "sdl-play-channel",
      "sdl-play-music",
      "sdl-volume",
      "sdl-volume-music",
      "sdl-halt-channel",
      "sdl-halt-group",
      "sdl-halt-music",
      "sdl-expire-channel",
      "sdl-fade-out-channel",
      "sdl-fade-out-group",
      "sdl-fade-out-music",
      "sdl-pause",
      "sdl-resume",
      "sdl-paused?",
      "sdl-pause-music",
      "sdl-resume-music",
      "sdl-rewind-music",
      "sdl-paused-music?",
      "sdl-playing?",
      "sdl-playing-music?",
      "sdl-set-music-command",
      "sdl-close-audio",
      NULL);

#ifndef SCM_MAGIC_SNARFER
#include "sdlmixer.x"
#endif

}

