/*******************************************************************
 *  sdlenums.h -- Enum helper functions                            *
 *                                                                 *
 *  Created:    <2001-06-03 20:07:15 foof>                         *
 *  Time-stamp: <01/11/25 12:49:23 foof>                         *
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

#ifndef _GUILE_SDL_ENUMS_H
#define _GUILE_SDL_ENUMS_H

#include <libguile.h>

/* define a numeric constant */
#define SCM_DEFINE_CONST(name, value) \
   scm_c_define (name, scm_long2num (value))


/* enum smob */

extern long enum_tag;

typedef struct {
  SCM vec, table;
  long min, max;
} enum_struct;

size_t free_enum (SCM s_enum);

/* initialize enum smob and scheme functions */
void sdl_init_enums (void);


/* enums */
SCM scm_c_define_enum (const char *name, ...);
long scm_enum2long (SCM s_enum, SCM enum_type, int pos, const char *func);
SCM scm_long2enum (long value, SCM enum_type);
SCM scm_enum_to_number (SCM enum_pair, SCM symbol);
SCM scm_number_to_enum (SCM enum_pair, SCM number);


/* flags (constants typically used as a logical or'ed group) */

SCM scm_c_define_flag (const char *name, ...);
unsigned long scm_flags2ulong (SCM s_flags, SCM flag_type, int pos,
                               const char *func);
SCM scm_ulong2flags (unsigned long value, SCM flag_type);
SCM scm_flags_to_number (SCM flag_type, SCM symbol);
SCM scm_number_to_flags (SCM flag_type, SCM number);

#endif /* ! _GUILE_SDL_ENUMS_H */

