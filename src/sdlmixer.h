/*******************************************************************
 *  mixer.h -- SDL_mixer for Guile                                 *
 *                                                                 *
 *  Created:    <2001-06-10 16:45:57 foof>                         *
 *  Time-stamp: <2001-06-10 21:26:29 foof>                         *
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

#ifndef _GUILE_SDL_MIXER_H
#define _GUILE_SDL_MIXER_H

/* guile headers */
#include <libguile.h>
/* sdl headers */
#include <SDL/SDL.h>
#include <SDL/SDL_mixer.h>
#include "scm.h"

extern long mix_music_tag;
extern long mix_audio_tag;

scm_sizet free_music (SCM music);
scm_sizet free_audio (SCM chunk);
/* int print_music (SCM port, SCM music, void *state); */

SCM mix_open_audio (SCM freq, SCM format, SCM channels, SCM chunksize);

SCM mix_allocate_channels (SCM numchans);

SCM mix_query_spec (void);

SCM mix_load_music (SCM file);

SCM mix_load_wave (SCM file);

/* Free an audio chunk previously loaded */
/* extern DECLSPEC void Mix_FreeChunk(Mix_Chunk *chunk); */
/* extern DECLSPEC void Mix_FreeMusic(Mix_Music *music); */

SCM mix_reserve_channels (SCM num);

/* Attach a tag to a channel. A tag can be assigned to several mixer
   channels, to form groups of channels.
   If 'tag' is -1, the tag is removed (actually -1 is the tag used to
   represent the group of all the channels).
   Returns true if everything was OK.
 */
SCM mix_group_channel (SCM which, SCM tag);
/* Assign several consecutive channels to a group */
SCM mix_group_channels (SCM from, SCM to, SCM tag);
/* Finds the first available channel in a group of channels */
SCM mix_group_available (SCM tag);

/* Returns the number of channels in a group. This is also a subtle
   way to get the total number of channels when 'tag' is -1
 */
SCM mix_group_count (SCM tag);
/* Finds the "oldest" sample playing in a group of channels */
SCM mix_group_oldest (SCM tag);
/* Finds the "most recent" (i.e. last) sample playing in a group of channels */
SCM mix_group_newer (SCM tag);

/* Play an audio chunk on a specific channel.
   If the specified channel is -1, play on the first free channel.
   If 'loops' is greater than zero, loop the sound that many times.
   If 'loops' is -1, loop inifinitely (~65000 times).
   Returns which channel was used to play the sound.
*/
SCM mix_play_channel (SCM chunk, SCM channel, SCM loops, SCM ticks, SCM fade);
SCM mix_play_music (SCM music, SCM loops, SCM fade);

/* Set the volume in the range of 0-128 of a specific channel or chunk.
   If the specified channel is -1, set volume for all channels.
   Returns the original volume.
   If the specified volume is -1, just return the current volume.
*/
SCM mix_volume (SCM volume, SCM which);
SCM mix_volume_music (SCM volume);

/* Halt playing of a particular channel */
SCM mix_halt_channel (SCM channel);
SCM mix_halt_group (SCM tag);
SCM mix_halt_music (void);

/* Change the expiration delay for a particular channel.
   The sample will stop playing after the 'ticks' milliseconds have elapsed,
   or remove the expiration if 'ticks' is -1
*/
SCM mix_expire_channel (SCM channel, SCM ticks);

/* Halt a channel, fading it out progressively till it's silent
   The ms parameter indicates the number of milliseconds the fading
   will take.
 */
SCM mix_fade_out_channel (SCM which, SCM ms);
SCM mix_fade_out_group (SCM tag, SCM ms);
SCM mix_fade_out_music (SCM ms);

/* Query the fading status of a channel */
SCM mix_fading_music (void);
SCM mix_fading_channel (SCM which);

/* Pause/Resume a particular channel */
SCM mix_pause (SCM channel);
SCM mix_resume (SCM channel);
SCM mix_paused (SCM channel);

/* Pause/Resume the music stream */
SCM mix_pause_music (void);
SCM mix_resume_music (void);
SCM mix_rewind_music (void);
SCM mix_paused_music (void);

/* Check the status of a specific channel.
   If the specified channel is -1, check all channels.
*/
SCM mix_playing (SCM channel);
SCM mix_playing_music (void);

/* Stop music and set external music playback command */
SCM mix_set_music_cmd (SCM command);

/* Close the mixer, halting all playing audio */
SCM mix_close_audio (void);

/* initialize the mixer subsystem */
void sdl_mixer_init (void);

#endif /* ! _GUILE_SDL_MIXER_H */
