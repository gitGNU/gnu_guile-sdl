/*******************************************************************
 *  mixer.c -- SDL_mixer for Guile                                 *
 *                                                                 *
 *  Created:    <2001-06-10 16:45:57 foof>                         *
 *  Time-stamp: <2001-06-18 01:08:31 foof>                         *
 *  Author:     Alex Shinn <foof@debian.org>                       *
 *                                                                 *
 *  Copyright (C) 2001 Alex Shinn                                  *
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
#include <mixer.h>

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

/* Open the mixer with a certain audio format */
SCM
mix_open_audio (SCM s_freq, SCM s_format, SCM s_stereo, SCM s_chunksize)
{
   int freq = MIX_DEFAULT_FREQUENCY;
   Uint16 format = MIX_DEFAULT_FORMAT;
   int channels = 2;
   int chunksize = 1024;

   /* handle optional args */
   if (s_freq != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_freq),  s_freq,  SCM_ARG1, "sdl-open-audio");
      freq = SCM_INUM (s_freq);
   }

   if (s_format != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_format),  s_format,  SCM_ARG2, "sdl-open-audio");
      format = SCM_INUM (s_format);
   }

   if (s_stereo != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_BOOLP (s_stereo),  s_stereo,  SCM_ARG3, "sdl-open-audio");
      if (SCM_FALSEP (s_stereo)) {
         channels = 1;
      }
   }

   if (s_chunksize != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_chunksize),  s_format,  SCM_ARG4, "sdl-open-audio");
      chunksize = SCM_INUM (s_chunksize);
   }

   /* open the audio device */
   SCM_RETURN_TRUE_IF_0 (Mix_OpenAudio (freq, MIX_DEFAULT_FORMAT, channels, 1024));
}

/* Dynamically change the number of channels managed by the mixer.
   If decreasing the number of channels, the upper channels are
   stopped.
   This function returns the new number of allocated channels.
 */
SCM
mix_allocate_channels (SCM s_numchans)
{
   int numchans;

   SCM_ASSERT (SCM_INUMP (s_numchans),  s_numchans,  SCM_ARG1, "sdl-allocate-channels");
   numchans = SCM_INUM (s_numchans);

   return SCM_MAKINUM (Mix_AllocateChannels (numchans));
}

/* Find out what the actual audio device parameters are.
   This function returns 1 if the audio has been opened, 0 otherwise.
 */
SCM
mix_query_spec (void)
{
   int freq, channels;
   Uint16 format;

   if (! Mix_QuerySpec (&freq, &format, &channels)) {
      return SCM_BOOL_F;
   }

   return SCM_LIST3 (SCM_MAKINUM (freq),
                     SCM_MAKINUM (format),
                     SCM_MAKINUM (channels));
}

/* Load a wave file or a music (.mod .s3m .it .xm) file */
SCM
mix_load_music (SCM file)
{
   Mix_Music *music;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "sdl-load-music");

   music = Mix_LoadMUS (SCM_CHARS (file));
   SCM_RETURN_NEWSMOB (mix_music_tag, music);
}

/* Load a wave file */
SCM
mix_load_wave (SCM file)
{
   Mix_Chunk *chunk;

   SCM_ASSERT ((SCM_NIMP (file) && SCM_STRINGP (file)),
               file, SCM_ARG1, "sdl-load-wave");

   chunk = Mix_LoadWAV (SCM_CHARS (file));
   SCM_RETURN_NEWSMOB (mix_audio_tag, chunk);
}

/* Free an audio chunk previously loaded */
/* extern DECLSPEC void Mix_FreeChunk(Mix_Chunk *chunk); */
/* extern DECLSPEC void Mix_FreeMusic(Mix_Music *music); */

/* Reserve the first channels (0 -> n-1) for the application, i.e. don't allocate
   them dynamically to the next sample if requested with a -1 value below.
   Returns the number of reserved channels.
 */
SCM
mix_reserve_channels (SCM num)
{
   SCM_ASSERT (SCM_INUMP (num),  num,  SCM_ARG1, "sdl-reserve-channels");
   return SCM_MAKINUM (Mix_ReserveChannels (SCM_INUM (num)));
}

/* Attach a tag to a channel. A tag can be assigned to several mixer
   channels, to form groups of channels.
   If 'tag' is -1, the tag is removed (actually -1 is the tag used to
   represent the group of all the channels).
   Returns true if everything was OK.
 */
SCM
mix_group_channel (SCM s_which, SCM s_tag)
{
   int which, tag=-1;

   SCM_ASSERT (SCM_INUMP (s_which), s_which, SCM_ARG1, "sdl-group-channel");
   which = SCM_INUM (s_which);

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG2, "sdl-group-channel");
      tag = SCM_INUM (s_tag);
   }

   return Mix_GroupChannel (which, tag) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Assign several consecutive channels to a group */
SCM
mix_group_channels (SCM s_from, SCM s_to, SCM s_tag)
{
   int from, to, tag=-1;

   SCM_ASSERT (SCM_INUMP (s_from), s_from, SCM_ARG1, "sdl-group-channels");
   from = SCM_INUM (s_from);

   SCM_ASSERT (SCM_INUMP (s_to), s_to, SCM_ARG2, "sdl-group-channels");
   to = SCM_INUM (s_to);

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG3, "sdl-group-channels");
      tag = SCM_INUM (s_tag);
   }

   return Mix_GroupChannels (from, to, tag) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Finds the first available channel in a group of channels */
SCM
mix_group_available (SCM s_tag)
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-group-available");
      tag = SCM_INUM (s_tag);
   }

   return SCM_MAKINUM (Mix_GroupAvailable (tag));
}

/* Returns the number of channels in a group. This is also a subtle
   way to get the total number of channels when 'tag' is -1
 */
SCM
mix_group_count (SCM s_tag)
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-group-count");
      tag = SCM_INUM (s_tag);
   }

   return SCM_MAKINUM (Mix_GroupCount (tag));
}

/* Finds the "oldest" sample playing in a group of channels */
SCM
mix_group_oldest (SCM s_tag)
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-group-oldest");
      tag = SCM_INUM (s_tag);
   }

   return SCM_MAKINUM (Mix_GroupOldest (tag));
}

/* Finds the "most recent" (i.e. last) sample playing in a group of channels */
SCM
mix_group_newer (SCM s_tag)
{
   int tag;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-group-newer");
      tag = SCM_INUM (s_tag);
   }

   return SCM_MAKINUM (Mix_GroupNewer (tag));
}

/* Play an audio chunk on a specific channel.
   If the specified channel is -1, play on the first free channel.
   If 'loops' is greater than zero, loop the sound that many times.
   If 'loops' is -1, loop inifinitely (~65000 times).
   Returns which channel was used to play the sound.
*/
SCM
mix_play_channel (SCM s_chunk, SCM s_channel, SCM s_loops,
                  SCM s_ticks, SCM s_fade)
{
   int channel=-1;
   Mix_Chunk *chunk;
   int loops=0;
   int ticks=-1;
   int fade=0;

   SCM_ASSERT_SMOB (s_chunk, mix_audio_tag, SCM_ARG1, "sdl-play-channel");
   chunk = (Mix_Chunk*) SCM_SMOB_DATA (s_chunk);

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG2, "sdl-play-channel");
      channel = SCM_INUM (s_channel);
   }

   if (s_loops != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_loops), s_loops, SCM_ARG3, "sdl-play-channel");
      loops = SCM_INUM (s_loops);
   }

   if (s_ticks != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_ticks), s_ticks, SCM_ARG4, "sdl-play-channel");
      ticks = SCM_INUM (s_ticks);
   }

   if (s_fade == SCM_UNDEFINED) {
      /* no fade, normal Mix_PlayChannelTimed */
      return SCM_MAKINUM (Mix_PlayChannelTimed (channel, chunk, loops, ticks));
   } else {
      /* we have a fade */
      SCM_ASSERT (SCM_INUMP (s_fade), s_fade, SCM_ARG5, "sdl-play-channel");
      fade = SCM_INUM (s_fade);
      return SCM_MAKINUM (Mix_FadeInChannelTimed (channel, chunk, loops, fade, ticks));
   }
}

SCM
mix_play_music (SCM s_music, SCM s_loops, SCM s_fade)
{
   Mix_Music *music;
   int loops=0;
   int fade=0;

   SCM_ASSERT_SMOB (s_music, mix_music_tag, SCM_ARG1, "sdl-play-music");
   music = (Mix_Music*) SCM_SMOB_DATA (s_music);

   if (s_loops != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_loops), s_loops, SCM_ARG2, "sdl-play-music");
      loops = SCM_INUM (s_loops);
   }

   if (s_fade == SCM_UNDEFINED) {
      /* no fade, normal Mix_PlayMusic */
      return SCM_MAKINUM (Mix_PlayMusic (music, loops));
   } else {
      /* we have a fade */
      SCM_ASSERT (SCM_INUMP (s_fade), s_fade, SCM_ARG3, "sdl-play-music");
      fade = SCM_INUM (s_fade);
      return SCM_MAKINUM (Mix_FadeInMusic (music, loops, fade));
   }
}

/* Set the volume in the range of 0-128 of a specific channel or chunk.
   If the specified channel is -1, set volume for all channels.
   Returns the original volume.
   If the specified volume is -1, just return the current volume.
*/
SCM
mix_volume (SCM s_volume, SCM s_which)
{
   int volume=-1;
   int channel=-1;
   Mix_Chunk *chunk;

   if (s_volume != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_volume), s_volume, SCM_ARG1, "sdl-volume");
      volume = SCM_INUM (s_volume);
   }

   if (s_which == SCM_UNDEFINED) {
      /* no chunk or channel, call Mix_Volume on default channel */
      return SCM_MAKINUM (Mix_Volume (channel, volume));
   } else if (SCM_INUMP (s_which)) {
      /* numeric which, treat as channel number */
      channel = SCM_INUM (s_which);
      return SCM_MAKINUM (Mix_Volume (channel, volume));
   } else {
      /* no-numeric which, must be a chunk smob */
      SCM_ASSERT_SMOB (s_which, mix_audio_tag, SCM_ARG2, "sdl-volume");
      chunk = (Mix_Chunk*) SCM_SMOB_DATA (s_which);
      return SCM_MAKINUM (Mix_VolumeChunk (chunk, volume));
   }
}

SCM
mix_volume_music (SCM s_volume)
{
   int volume=-1;

   if (s_volume != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_volume), s_volume, SCM_ARG1, "sdl-music-volume");
      volume = SCM_INUM (s_volume);
   }

   return SCM_MAKINUM (Mix_VolumeMusic (volume));
}

/* Halt playing of a particular channel */
SCM
mix_halt_channel (SCM s_channel)
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-halt-channel");
      channel = SCM_INUM (s_channel);
   }

   return SCM_MAKINUM (Mix_HaltChannel (channel));
}

SCM
mix_halt_group (SCM s_tag)
{
   int tag=-1;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-halt-group");
      tag = SCM_INUM (s_tag);
   }

   return SCM_MAKINUM (Mix_HaltGroup (tag));
}

SCM
mix_halt_music (void)
{
   return SCM_MAKINUM (Mix_HaltMusic ());
}

/* Change the expiration delay for a particular channel.
   The sample will stop playing after the 'ticks' milliseconds have elapsed,
   or remove the expiration if 'ticks' is -1
*/
SCM
mix_expire_channel (SCM s_channel, SCM s_ticks)
{
   int channel=-1;
   int ticks=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-expire-channel");
      channel = SCM_INUM (s_channel);
   }

   if (s_ticks != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_ticks), s_ticks, SCM_ARG2, "sdl-expire-channel");
      ticks = SCM_INUM (s_ticks);
   }

   return SCM_MAKINUM (Mix_ExpireChannel (channel, ticks));
}

/* Halt a channel, fading it out progressively till it's silent
   The ms parameter indicates the number of milliseconds the fading
   will take.
 */
SCM
mix_fade_out_channel (SCM s_which, SCM s_ms)
{
   int channel=-1;
   int ms=0;

   if (s_which != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_which), s_which, SCM_ARG1, "sdl-fade-out-channel");
      channel = SCM_INUM (s_which);
   }

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_ms), s_ms, SCM_ARG2, "sdl-fade-out-channel");
      ms = SCM_INUM (s_ms);
   }

   return SCM_MAKINUM (Mix_FadeOutChannel (channel, ms));
}

SCM
mix_fade_out_group (SCM s_tag, SCM s_ms)
{
   int tag=-1;
   int ms=0;

   if (s_tag != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_tag), s_tag, SCM_ARG1, "sdl-fade-out-group");
      tag = SCM_INUM (s_tag);
   }

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_ms), s_ms, SCM_ARG2, "sdl-fade-out-group");
      ms = SCM_INUM (s_ms);
   }

   return SCM_MAKINUM (Mix_FadeOutGroup (tag, ms));
}

SCM
mix_fade_out_music (SCM s_ms)
{
   int ms=0;

   if (s_ms != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_ms), s_ms, SCM_ARG1, "sdl-fade-out-music");
      ms = SCM_INUM (s_ms);
   }

   return SCM_MAKINUM (Mix_FadeOutMusic (ms));
}

/* Query the fading status of a channel */
SCM
mix_fading_music (void)
{
   return SCM_MAKINUM (Mix_FadingMusic ());
}

SCM
mix_fading_channel (SCM s_which)
{
   int which=-1;

   if (s_which != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_which), s_which, SCM_ARG1, "sdl-fading-channel");
      which = SCM_INUM (s_which);
   }

   return SCM_MAKINUM (Mix_FadingChannel (which));
}

/* Pause/Resume a particular channel */
SCM
mix_pause (SCM s_channel)
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-pause");
      channel = SCM_INUM (s_channel);
   }

   Mix_Pause (channel);

   return SCM_UNSPECIFIED;
}

SCM
mix_resume (SCM s_channel)
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-resume");
      channel = SCM_INUM (s_channel);
   }

   Mix_Resume (channel);

   return SCM_UNSPECIFIED;
}

SCM
mix_paused (SCM s_channel)
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-paused?");
      channel = SCM_INUM (s_channel);
   }

   return Mix_Paused (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Pause/Resume the music stream */
SCM
mix_pause_music (void)
{
   Mix_PauseMusic ();
   return SCM_UNSPECIFIED;
}

SCM
mix_resume_music (void)
{
   Mix_ResumeMusic ();
   return SCM_UNSPECIFIED;
}

SCM
mix_rewind_music (void)
{
   Mix_RewindMusic ();
   return SCM_UNSPECIFIED;
}

SCM
mix_paused_music (void)
{
   return Mix_PausedMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Check the status of a specific channel.
   If the specified channel is -1, check all channels.
*/
SCM
mix_playing (SCM s_channel)
{
   int channel=-1;

   if (s_channel != SCM_UNDEFINED) {
      SCM_ASSERT (SCM_INUMP (s_channel), s_channel, SCM_ARG1, "sdl-playing?");
      channel = SCM_INUM (s_channel);
   }

   return Mix_Playing (channel) ? SCM_BOOL_T : SCM_BOOL_F;
}

SCM
mix_playing_music (void)
{
   return Mix_PlayingMusic () ? SCM_BOOL_T : SCM_BOOL_F;
}

/* Stop music and set external music playback command */
SCM
mix_set_music_cmd (SCM command)
{
   SCM_ASSERT ((SCM_NIMP (command) && SCM_STRINGP (command)),
               command, SCM_ARG1, "sdl-set-music-command");
   return SCM_MAKINUM (Mix_SetMusicCMD (SCM_CHARS (command)));
}

/* Close the mixer, halting all playing audio */
SCM
mix_close_audio (void)
{
   Mix_CloseAudio ();
   return SCM_UNSPECIFIED;
}


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
      "sdl-fading/none",    MIX_NO_FADING,
      "sdl-fading/out",     MIX_FADING_OUT,
      "sdl-fading/in",      MIX_FADING_IN,
      NULL);

   /* functions */
   scm_c_define_gsubr ("sdl-open-audio",               0, 4, 0, mix_open_audio);
   scm_c_define_gsubr ("sdl-allocate-channels",        1, 0, 0, mix_allocate_channels);
   scm_c_define_gsubr ("sdl-query-spec",               0, 0, 0, mix_query_spec);
   scm_c_define_gsubr ("sdl-load-music",               1, 0, 0, mix_load_music);
   scm_c_define_gsubr ("sdl-load-wave",                1, 0, 0, mix_load_wave);
   scm_c_define_gsubr ("sdl-reserve-channels",         1, 0, 0, mix_reserve_channels);
   scm_c_define_gsubr ("sdl-group-channel",            1, 1, 0, mix_group_channel);
   scm_c_define_gsubr ("sdl-group-channels",           2, 1, 0, mix_group_channels);
   scm_c_define_gsubr ("sdl-group-available",          0, 1, 0, mix_group_available);
   scm_c_define_gsubr ("sdl-group-count",              0, 1, 0, mix_group_count);
   scm_c_define_gsubr ("sdl-group-oldest",             0, 1, 0, mix_group_oldest);
   scm_c_define_gsubr ("sdl-group-newer",              0, 1, 0, mix_group_newer);
   scm_c_define_gsubr ("sdl-play-channel",             1, 4, 0, mix_play_channel);
   scm_c_define_gsubr ("sdl-play-music",               1, 2, 0, mix_play_music);
   scm_c_define_gsubr ("sdl-volume",                   0, 2, 0, mix_volume);
   scm_c_define_gsubr ("sdl-volume-music",             0, 1, 0, mix_volume_music);
   scm_c_define_gsubr ("sdl-halt-channel",             0, 1, 0, mix_halt_channel);
   scm_c_define_gsubr ("sdl-halt-group",               0, 1, 0, mix_halt_group);
   scm_c_define_gsubr ("sdl-halt-music",               0, 0, 0, mix_halt_music);
   scm_c_define_gsubr ("sdl-expire-channel",           0, 2, 0, mix_expire_channel);
   scm_c_define_gsubr ("sdl-fade-out-channel",         0, 2, 0, mix_fade_out_channel);
   scm_c_define_gsubr ("sdl-fade-out-group",           0, 2, 0, mix_fade_out_group);
   scm_c_define_gsubr ("sdl-fade-out-music",           0, 1, 0, mix_fade_out_music);
   scm_c_define_gsubr ("sdl-pause",                    0, 1, 0, mix_pause);
   scm_c_define_gsubr ("sdl-resume",                   0, 1, 0, mix_resume);
   scm_c_define_gsubr ("sdl-paused?",                  0, 1, 0, mix_paused);
   scm_c_define_gsubr ("sdl-pause-music",              0, 0, 0, mix_pause_music);
   scm_c_define_gsubr ("sdl-resume-music",             0, 0, 0, mix_resume_music);
   scm_c_define_gsubr ("sdl-rewind-music",             0, 0, 0, mix_rewind_music);
   scm_c_define_gsubr ("sdl-paused-music?",            0, 0, 0, mix_paused_music);
   scm_c_define_gsubr ("sdl-playing?",                 0, 1, 0, mix_playing);
   scm_c_define_gsubr ("sdl-playing-music?",           0, 0, 0, mix_playing_music);
   scm_c_define_gsubr ("sdl-set-music-command",        1, 0, 0, mix_set_music_cmd);
   scm_c_define_gsubr ("sdl-close-audio",              0, 0, 0, mix_close_audio);

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
}

