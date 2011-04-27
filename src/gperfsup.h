/* gperfsup.h */

/*
 * Copyright (C) 2004, 2009 Thien-Thi Nguyen
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

#ifndef GUILE_SDL_GPERFSUP_H
#define GUILE_SDL_GPERFSUP_H 1

#include <libguile/tags.h>
#include "guile-sdl.h"

#define GPERFSUP_PRE_BOILERPLATE \
static struct val_and_name * \
in_word_set (register const char *str, register unsigned int len);

#define GPERFSUP_POST_BOILERPLATE(cvar,name)    \
flagstash_t gsdl_ ## cvar = {                   \
  name,                                         \
  &(wordlist[0]),                               \
  NULL,                                         \
  TOTAL_KEYWORDS,                               \
  (in_word_set_t *) &in_word_set,               \
  SCM_BOOL_F                                    \
};

#endif /* !defined (GUILE_SDL_GPERFSUP_H) */

/* gperfsup.h ends here */
