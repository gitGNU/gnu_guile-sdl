/*******************************************************************
 *  sdlsmobs.h -- Smob helper definitions                          *
 *                                                                 *
 *  Created:    <2001-06-03 20:07:15 foof>                         *
 *  Time-stamp: <2001-07-03 19:04:49 foof>                         *
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

#ifndef _GUILE_SDL_SMOBS_H
#define _GUILE_SDL_SMOBS_H

#include <libguile.h>

/* lots of SDL functions return 0 for true, -1 otherwise */
#define SCM_RETURN_TRUE_IF_0(exp)  \
  return ((exp) == 0) ? SCM_BOOL_T : SCM_BOOL_F

/* useful type-checking for smobs */
#define SCM_ASSERT_SMOB(arg, tag, error, function)     \
   SCM_ASSERT ((SCM_NIMP (arg)                         \
                 && (long) SCM_CAR (arg) == tag),      \
                arg, error, function)

/**********************************************************************/

/* define a simple numeric getter */
#define SCM_DEFINE_NUMBER_GETTER(s_func, c_func, c_tag, c_type, c_field) \
SCM c_func (SCM s_smob)                                                  \
{                                                                        \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                     \
  return scm_long2num (((c_type) SCM_CDR (s_smob))->c_field);            \
}

/* define a simple numeric setter */
#define SCM_DEFINE_NUMBER_SETTER(s_func, c_func, c_tag, c_type, c_field)    \
SCM c_func (SCM s_smob, SCM s_value)                                        \
{                                                                           \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                        \
  SCM_ASSERT (scm_exact_p (s_value), s_value, SCM_ARG2, s_func);            \
  (((c_type) SCM_CDR (s_smob))->c_field) = scm_num2long (s_value, SCM_ARG1, \
                                                         "scm_num2long");   \
  return SCM_UNSPECIFIED;                                                   \
}

/**********************************************************************/

/* define a simple enum getter */
#define SCM_DEFINE_ENUM_GETTER(s_func, c_func, c_tag, c_type, c_field, s_enum_type) \
SCM c_func (SCM s_smob)                                                      \
{                                                                            \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                         \
  return scm_long2enum (((c_type) SCM_CDR (s_smob))->c_field, s_enum_type);  \
}

/* define a simple enum setter */
#define SCM_DEFINE_ENUM_SETTER(s_func, c_func, c_tag, c_type, c_field, s_enum_type) \
SCM c_func (SCM s_smob, SCM s_value)                                         \
{                                                                            \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                         \
  SCM_ASSERT (scm_exact_p (s_value), s_value, SCM_ARG2, s_func);             \
  (((c_type) SCM_CDR (s_smob))->c_field) = scm_enum2long (s_value,           \
                                                          s_enum_type,       \
                                                          SCM_ARG1,          \
                                                          "scm_enum2long");  \
  return SCM_UNSPECIFIED;                                                    \
}

/**********************************************************************/

/* define a simple flag getter */
#define SCM_DEFINE_FLAG_GETTER(s_func, c_func, c_tag, c_type, c_field, s_flag_type) \
SCM c_func (SCM s_smob)                                                      \
{                                                                            \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                         \
  return scm_ulong2flags (((c_type) SCM_CDR (s_smob))->c_field, s_flag_type);\
}

/* define a simple flag setter */
#define SCM_DEFINE_FLAG_SETTER(s_func, c_func, c_tag, c_type, c_field, s_flag_type) \
SCM c_func (SCM s_smob, SCM s_value)                                         \
{                                                                            \
  SCM_ASSERT_SMOB (s_smob, c_tag, SCM_ARG1, s_func);                         \
  SCM_ASSERT (scm_exact_p (s_value), s_value, SCM_ARG2, s_func);             \
  (((c_type) SCM_CDR (s_smob))->c_field) = scm_flags2ulong (s_value,         \
                                                            s_flag_type,     \
                                                            SCM_ARG1,        \
                                                            "scm_flag2long");\
  return SCM_UNSPECIFIED;                                                    \
}

/**********************************************************************/

#endif /* ! _GUILE_SDL_SMOBS_H */

