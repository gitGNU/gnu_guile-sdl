/*******************************************************************
 *  guile-sdl.h -- SDL Video Wrappers for Guile                    *
 *                                                                 *
 *  Created:    <2001-04-08 13:48:18 foof>                         *
 *  Time-stamp: <2001-06-10 19:51:52 foof>                         *
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


/* SDL Initialization */
SCM sdl_init (SCM s_subsystems);
SCM sdl_init_subsystem (SCM s_subsystems);

/* Termination */
SCM sdl_quit (void);
SCM sdl_quit_subsystem (SCM s_subsystems);

/* Info */
SCM sdl_was_init (SCM s_subsystems);

/* Time */
SCM sdl_get_ticks (void);
SCM sdl_delay (SCM ms);

/* Guile Initialization */
void guile_sdl_init (void);
