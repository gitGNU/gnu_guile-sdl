/* sdlenums.h --- Enum helper functions
 *
 * Copyright (C) 2003, 2004, 2005 Thien-Thi Nguyen
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

#ifndef GUILE_SDL_ENUMS_H
#define GUILE_SDL_ENUMS_H

/* enums */

#define GSDL_CSCS(x)  #x, x  /* "C String Comma Symbol" */
SCM gsdl_define_enum (const char *name, ...);
long gsdl_enum2long (SCM s_enum, SCM enum_type, int pos, const char *func);
SCM gsdl_long2enum (long value, SCM enum_type);

#define GSDL_ENUM2LONG(enums,table,pos) \
  gsdl_enum2long ((enums), (table), (pos), FUNC_NAME)

/* flags (constants typically used as a logical or'ed group) */

unsigned long gsdl_flags2ulong (SCM flags, SCM table,
                                int pos, const char *func);

SCM gsdl_ulong2flags (unsigned long value, SCM stash);

#define GSDL_FLAGS2ULONG(flags,table,pos) \
  gsdl_flags2ulong ((flags), (table), (pos), FUNC_NAME)


/* val_and_name and flagstash */

typedef struct val_and_name {
  char *name;
  unsigned long int val;
  SCM sname;
  SCM sval;
} val_and_name_t;

typedef struct val_and_name * (in_word_set_t)
     (register const char *str, register unsigned int len);

typedef struct flagstash {
  char *name;
  val_and_name_t  *sparse;
  val_and_name_t **linear;
  int total;
  in_word_set_t *lookup;
  SCM reverse_lookup_cache;
} flagstash_t;

SCM gsdl_make_flagstash (flagstash_t *stash);

#endif /* ! defined (GUILE_SDL_ENUMS_H) */

/* sdlenums.h ends here */
