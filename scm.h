/*******************************************************************
 *  scm.h -- Guile helper functions                                *
 *                                                                 *
 *  Created:    <2001-06-03 20:07:15 foof>                         *
 *  Time-stamp: <2001-06-09 21:24:08 foof>                         *
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

#ifndef _GUILE_SDL_SCM_H
#define _GUILE_SDL_SCM_H

#include <libguile.h>

/* lots of SDL functions return 0 for true, -1 otherwise */
#define SCM_RETURN_TRUE_IF_0(exp)  \
  return ((exp) == 0) ? SCM_BOOL_T : SCM_BOOL_F

/* useful type-checking for smobs */
#define SCM_ASSERT_SMOB(arg, tag, error, function)     \
   SCM_ASSERT ((SCM_NIMP (arg)                         \
                 && (long) SCM_CAR (arg) == tag),      \
                arg, error, function)

/* define a numeric constant */
#define SCM_DEFINE_CONST(name, value) \
   scm_c_define (name, SCM_MAKINUM (value))

/* define a simple numeric getter */
#define SCM_DEFINE_INUM_GETTER(s_func, c_func, c_tag, c_type, c_field) \
SCM c_func (SCM s_smob)                                                \
{                                                                      \
   SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                  \
   return SCM_MAKINUM (((c_type) SCM_CDR (s_smob))->c_field);          \
}

/* define a simple numeric setter */
#define SCM_DEFINE_INUM_SETTER(s_func, c_func, c_tag, c_type, c_field) \
SCM c_func (SCM s_smob, SCM s_value)                                   \
{                                                                      \
   SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                  \
   SCM_ASSERT (SCM_INUMP (s_value), s_value, SCM_ARG2, s_func);        \
   (((c_type) SCM_CDR (s_smob))->c_field) = SCM_INUM (s_value);        \
   return SCM_UNSPECIFIED;                                             \
}

/* register a C enum */
SCM scm_c_define_enum (const char *name, ...);
SCM scm_enum_to_number (SCM enum_pair, SCM symbol);
SCM scm_number_to_enum (SCM enum_pair, SCM number);

#endif /* ! _GUILE_SDL_SCM_H */

