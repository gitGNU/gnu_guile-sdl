/*******************************************************************
 *  sdlenums.c -- Enum helper functions                            *
 *                                                                 *
 *  Created:    <2001-06-09 19:22:27 foof>                         *
 *  Time-stamp: <2001-07-09 21:39:37 foof>                         *
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

#include <libguile.h>
#include <stdarg.h>
#include "sdlenums.h"
#include "sdlsmobs.h"

#define MAX_FLAGS (8*sizeof(long))

long enum_tag;

scm_sizet
free_enum (SCM s_enum)
{
  enum_struct *enum_smob = (enum_struct*) SCM_SMOB_DATA (s_enum);
  free(enum_smob);
  return sizeof (enum_struct*);
}

SCM
mark_enum (SCM s_enum)
{
  enum_struct *enum_smob = (enum_struct*) SCM_SMOB_DATA (s_enum);
  scm_gc_mark(enum_smob->vec);
  scm_gc_mark(enum_smob->table);
  return SCM_BOOL_F;
}

void
sdl_init_enums (void)
{
   /* smobs */
   enum_tag   = scm_make_smob_type ("SDL-enum", sizeof (enum_struct));
   scm_set_smob_free (enum_tag, free_enum);
   scm_set_smob_mark (enum_tag, mark_enum);

   /* functions */
   scm_c_define_gsubr ("enum->number",   2, 0, 0, scm_enum_to_number);
   scm_c_define_gsubr ("number->enum",   2, 0, 0, scm_number_to_enum);
   scm_c_define_gsubr ("flags->number",  2, 0, 0, scm_flags_to_number);
   scm_c_define_gsubr ("number->flags",  2, 0, 0, scm_number_to_flags);

   /* exported symbols */
   scm_c_export (
      "enum->number",  "number->enum",
      "flags->number", "number->flags",
      NULL);
}


/* register a C enum*/
SCM
scm_c_define_enum (const char *name, ...)
{
  va_list ap;
  char *symname;
  long value, max=0, min=0xffff, i=0;
  SCM s_enum, vec, table, sym;
  enum_struct *new_enum;

  /* initialize the argument list */
  va_start (ap, name);

  /* first pass: count the args, find the max and min */
  symname = va_arg (ap, char*);
  while (symname) {
    i++;
    value = va_arg (ap, long);
    if (value > max) {
      max = value;
    }
    if (value < min) {
      min = value;
    }
    /* next name */
    symname = va_arg (ap, char*);
  }

  /* add one to make room for largest value */
  max++;

  /* create an enum struct to hold our values */
  new_enum = (enum_struct*) malloc (sizeof (enum_struct));

  new_enum->min = min;
  new_enum->max = max;

  /* create the enum table */
  vec = scm_c_make_vector (max - min + 1, SCM_BOOL_F);
  /* vec = scm_make_uve (max, scm_str2symbol (name)); */
  new_enum->vec = vec;

  /* create the enum hash */
  table = scm_c_make_hash_table (i);
  /* SCM_SETCDR (s_enum, table); */
  new_enum->table = table;

  /* reset the argument list (is this safe?) */
  va_start (ap, name);

  /* second pass: fill the table and hash */
  for (i--; i>=0; i--) {
    symname = va_arg (ap, char*);
    value = va_arg (ap, long);
    sym = scm_str2symbol (symname);
    /* scm_vector_set_x (vec, scm_ulong2num (value-min), sym); */
    scm_vector_set_x (vec, scm_long2num (value-min), sym);
    scm_hashq_set_x (table, sym, scm_long2num (value));
  }

  /* clean up */
  va_end (ap);

  /* build and define the enum smob instance */
  SCM_NEWSMOB (s_enum, enum_tag, new_enum);
  scm_c_define (name, s_enum);
  return s_enum;
}


/* C level conversions */

long
scm_enum2long (SCM s_enum, SCM s_enum_type, int pos, const char *func)
{
  enum_struct *enum_type;
  SCM index;
  long result=0;

  enum_type = (enum_struct*) SCM_SMOB_DATA (s_enum_type);

  if (SCM_SYMBOLP (s_enum)) {
    index = scm_hashq_ref (enum_type->table, s_enum, SCM_BOOL_F);
    if (index != SCM_BOOL_F) {
      result = scm_num2long (index, pos, func);
    }
  } else {
    SCM_ASSERT (scm_exact_p (s_enum), s_enum, pos, func);
    result = scm_num2long (s_enum, pos, func);
  }

  return result;
}

SCM
scm_long2enum (long value, SCM s_enum_type)
{
  enum_struct *enum_type = (enum_struct*) SCM_SMOB_DATA (s_enum_type);
  return scm_vector_ref (enum_type->vec, scm_long2num (value - enum_type->min));
}


/* scheme level conversions */

SCM_DEFINE( scm_enum_to_number, "enum->number", 2, 0, 0,
            (SCM s_enum_type,
             SCM symbol),
"Converts an enum number or symbol to a number.")
#define FUNC_NAME s_scm_enum_to_number
{
  SCM table;
  enum_struct *enum_type;

  SCM_ASSERT_SMOB (s_enum_type, enum_tag, SCM_ARG1, "enum->number");

  enum_type = (enum_struct*) SCM_SMOB_DATA (s_enum_type);
  table = enum_type->table;

  SCM_ASSERT (SCM_SYMBOLP (symbol), symbol, SCM_ARG2, "enum->number");

  /* lookup and return the number in the pair */
  return scm_hashq_ref (table, symbol, SCM_BOOL_F);
}
#undef FUNC_NAME


SCM
scm_number_to_enum (SCM s_enum_type, SCM number)
{
  SCM vec;
  long index;
  enum_struct *enum_type;

  SCM_ASSERT_SMOB (s_enum_type, enum_tag, SCM_ARG1, "number->enum");

  enum_type = (enum_struct*) SCM_SMOB_DATA (s_enum_type);
  vec = enum_type->vec;

  SCM_ASSERT (scm_exact_p (number), number, SCM_ARG2, "number->enum");
  index = scm_num2long (number, SCM_ARG2, "number->enum") - enum_type->min;

  /* return the numbered index into the vector */
  return scm_vector_ref (vec, scm_long2num (index));
}


/* flags */

/* register a C flag group (stored as a vector/hash pair) */
SCM
scm_c_define_flag (const char *name, ...)
{
  va_list ap;
  char *symname;
  unsigned long value, bit, max=0;
  int i=0, j;
  SCM s_flag, vec, table, sym;

  /* initialize the argument list */
  va_start (ap, name);

  /* first pass: count the args */
  symname = va_arg (ap, char*);
  while (symname) {
    i++;
    value = va_arg (ap, unsigned long);
    if (value > max) {
      max = value;
    }
    /* next name */
    symname = va_arg (ap, char*);
  }

  /* create a pair to hold the table and hash */
  s_flag = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
  scm_c_define (name, s_flag);

  /* create the flag table */
  vec = scm_c_make_vector (MAX_FLAGS, SCM_BOOL_F);
  /* vec = scm_make_uve (max, scm_str2symbol (name)); */
  SCM_SETCAR (s_flag, vec);

  /* create the flag hash */
  table = scm_c_make_hash_table (i);
  SCM_SETCDR (s_flag, table);

  /* reset the argument list (is this safe?) */
  va_start (ap, name);

  /* second pass: fill the table and hash */
  for (i--; i>=0; i--) {
    symname = va_arg (ap, char*);
    value = va_arg (ap, unsigned long);
    sym = scm_str2symbol (symname);
    /* scm_vector_set_x (vec, scm_ulong2num (value), sym); */
    scm_hashq_set_x (table, sym, scm_ulong2num (value));
    /* only add single-bit values to the vector */
    bit = 0;
    for (j=MAX_FLAGS; j; j--) {
      if (value == (1<<(j-1))) {
        bit = j;
      }
    }
    if (bit) {
      scm_vector_set_x (vec, scm_ulong2num (bit-1), sym);
    }
  }

  /* clean up */
  va_end (ap);

  return s_flag;
}


unsigned long
scm_flags2ulong (SCM s_flags, SCM flag_type, int pos, const char *func)
{
  SCM index, table;
  unsigned long result=0, elt;

  if (scm_pair_p (s_flags)) {
    /* a list of symbols representing flags */
    table = SCM_CDR (flag_type);
    while (s_flags != SCM_EOL) {
      index = scm_hashq_ref (table, SCM_CAR (s_flags), SCM_BOOL_F);
      if (index != SCM_BOOL_F) {
        elt = scm_num2ulong (index, pos, func);
        result = result | elt;
      }
      s_flags = SCM_CDR (s_flags);
    }
  } else {
    /* the numeric flag value */
    SCM_ASSERT (scm_exact_p (s_flags), s_flags, pos, func);
    result = scm_num2ulong (s_flags, pos, func);
  }

  return result;
}


SCM
scm_ulong2flags (unsigned long value, SCM flag_type)
{
  SCM vec = SCM_CAR (flag_type);
  SCM result = SCM_EOL;
  unsigned long i;

  for (i=MAX_FLAGS; i>0; i--) {
    if (value & (1<<(i-1))) {
      result = scm_cons (scm_vector_ref (vec, scm_ulong2num (i-1)),
                         result);
    }
  }

  return result;
}


/* scheme level conversions */

SCM
scm_flags_to_number (SCM flag_type, SCM symbol)
{
  SCM table;

  /* loose check: make sure we have a pair and the cdr is a vector */
  SCM_ASSERT (scm_pair_p (flag_type), flag_type, SCM_ARG1, "flags->number");
  table = SCM_CDR (flag_type);
  SCM_ASSERT (scm_vector_p (table), flag_type, SCM_ARG1, "flags->number");

  /* lookup and return the number in the pair */
  return scm_ulong2num (scm_flags2ulong (symbol, flag_type, SCM_ARG2,
                                         "flags->number"));
}

SCM
scm_number_to_flags (SCM flag_type, SCM number)
{
  SCM vec;

  /* loose check: make sure we have a pair and the car is a vector */
  SCM_ASSERT (scm_pair_p (flag_type), flag_type, SCM_ARG1, "number->flags");
  vec = SCM_CAR (flag_type);
  SCM_ASSERT (scm_vector_p (vec), flag_type, SCM_ARG1, "number->flags");

  SCM_ASSERT (scm_exact_p (number), number, SCM_ARG2, "number->flags");

  /* return the numbered index into the vector */
  return scm_ulong2flags (scm_num2ulong (number, SCM_ARG2, "number->flags"),
                          flag_type);
}

