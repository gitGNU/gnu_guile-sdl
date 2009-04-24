/* bool.h */

/*
 * Copyright (C) 2004 Thien-Thi Nguyen
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

#ifndef GUILE_SDL_BOOL_H
#define GUILE_SDL_BOOL_H

#define BOOL_FALSE (SCM_BOOL_F)
#define BOOL_TRUE  (SCM_BOOL_T)

#define NOT_FALSEP(x)      (SCM_NFALSEP (x))
#define EXACTLY_FALSEP(x)  (SCM_FALSEP (x))
#define EXACTLY_TRUEP(x)   (gh_eq_p ((x), BOOL_TRUE))

#define SET_FALSE(cvar) \
  cvar = BOOL_FALSE

#define SET_TRUE(cvar) \
  cvar = BOOL_TRUE

#define RETURN_FALSE \
  return BOOL_FALSE

#define RETURN_TRUE \
  return BOOL_TRUE

#if 0 /* experimental */
#define RETURN_FALSE                            \
  do {                                          \
    SCM __rv;                                   \
    SET_FALSE (__rv);                           \
    return __rv;                                \
  } while (0)

#define RETURN_TRUE                             \
  do {                                          \
    SCM __rv;                                   \
    SET_TRUE (__rv);                            \
    return __rv;                                \
  } while (0)
#endif /* 0 (experimental) */

#endif /* ! defined (GUILE_SDL_BOOL_H) */

/* bool.h ends here */
