/*******************************************************************
 *  scm.c -- Guile helper functions                                *
 *                                                                 *
 *  Created:    <2001-06-09 19:22:27 foof>                         *
 *  Time-stamp: <2001-06-09 21:47:34 foof>                         *
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

/* register a C enum*/
SCM
scm_c_define_enum (const char *name, ...)
{
   va_list ap;
   char *symname;
   int value, max=0, i=0;
   SCM s_enum, vec, table, sym;

   /* initialize the argument list */
   va_start (ap, name);

   /* first pass: count the args */
   symname = va_arg (ap, char*);
   while (symname) {
      i++;
      value = va_arg (ap, int);
      if (value > max) {
         max = value;
      }
      /* next name */
      symname = va_arg (ap, char*);
   }

   /* add one to make room for largest value */
   max++;

   /* create a pair to hold the table and hash */
   s_enum = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
   scm_c_define (name, s_enum);

   /* create the enum table */
   vec = scm_c_make_vector (max, SCM_BOOL_F);
   /* vec = scm_make_uve (max, scm_str2symbol (name)); */
   SCM_SETCAR (s_enum, vec);

   /* create the enum hash */
   table = scm_c_make_hash_table (i);
   SCM_SETCDR (s_enum, table);

   /* reset the argument list (is this safe?) */
   va_start (ap, name);

   /* second pass: fill the table and hash */
   for (i--; i>=0; i--) {
      symname = va_arg (ap, char*);
      value = va_arg (ap, int);
      sym = scm_str2symbol (symname);
      /* scm_vector_set_x (vec, scm_long2num (value), sym); */
      scm_vector_set_x (vec, scm_long2num (value), sym);
      scm_hashq_set_x (table, sym, scm_long2num (value));
   }

   /* clean up */
   va_end (ap);

   return s_enum;
}

SCM
scm_enum_to_number (SCM enum_pair, SCM symbol)
{
   SCM table;

   /* loose check: make sure we have a pair and the cdr is a vector */
   SCM_ASSERT (scm_pair_p (enum_pair), enum_pair, SCM_ARG1, "enum->number");
   table = SCM_CDR (enum_pair);
   SCM_ASSERT (scm_vector_p (table), enum_pair, SCM_ARG1, "enum->number");

   SCM_ASSERT (SCM_SYMBOLP (symbol), symbol, SCM_ARG2, "enum->number");

   /* lookup and return the number in the pair */
   return scm_hashq_ref (table, symbol, SCM_BOOL_F);
}

SCM
scm_number_to_enum (SCM enum_pair, SCM number)
{
   SCM vec;

   /* loose check: make sure we have a pair and the car is a vector */
   SCM_ASSERT (scm_pair_p (enum_pair), enum_pair, SCM_ARG1, "number->enum");
   vec = SCM_CAR (enum_pair);
   SCM_ASSERT (scm_vector_p (vec), enum_pair, SCM_ARG1, "number->enum");

   SCM_ASSERT (scm_exact_p (number), number, SCM_ARG2, "number->enum");

   /* return the numbered index into the vector */
   return scm_vector_ref (vec, number);
}

