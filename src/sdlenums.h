/*******************************************************************
 *  sdlenums.h -- Enum helper functions                            *
 *                                                                 *
 *  Created:    <2001-06-03 20:07:15 foof>                         *
 *  Time-stamp: <2001-06-25 00:28:39 foof>                         *
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

/* register a C enum */
SCM scm_c_define_enum (const char *name, ...);
SCM scm_enum_to_number (SCM enum_pair, SCM symbol);
SCM scm_number_to_enum (SCM enum_pair, SCM number);

#endif /* ! _GUILE_SDL_ENUMS_H */

