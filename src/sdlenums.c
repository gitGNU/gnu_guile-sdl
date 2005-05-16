/* sdlenums.c -- Enum helper functions
 *
 * 	Copyright (C) 2003,2004,2005 Thien-Thi Nguyen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the Free
 * Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307 USA
 */

#include <guile/gh.h>
#include <stdarg.h>

#include "config.h"
#include "argcheck.h"
#include "sdlenums.h"
#include "sdlsmobs.h"
#include "retval.h"
#include "bool.h"


static SCM hfold;
static SCM acons;

static long enum_tag;

typedef struct {
  SCM vec, table;
  long min, max;
} enum_struct;

#define ASSERT_ENUM(obj,which) \
  ASSERT_SMOB (obj, enum_tag, which)

#define UNPACK_ENUM(smob) \
  (SMOBGET (smob, enum_struct *))

static
SCM
mark_enum (SCM enumstash)
{
  enum_struct *enum_smob = UNPACK_ENUM (enumstash);

  scm_gc_mark (enum_smob->vec);
  scm_gc_mark (enum_smob->table);
  RETURN_FALSE;
}

static
size_t
free_enum (SCM enumstash)
{
  free (UNPACK_ENUM (enumstash));
  return sizeof (enum_struct);
}


#define REASONABLE_BUCKET_COUNT(count) \
  (SCM_MAKINUM                         \
   (((count) <= 11) ? 11               \
    : (((count) <= 23) ? 23            \
       : (((count) <= 57) ? 57         \
          : (((count) <= 113) ? 113    \
             : (((count) <= 211) ? 211 \
                : 431))))))

#define MAKE_HASH_TABLE(size) \
  (scm_make_vector (REASONABLE_BUCKET_COUNT (size), SCM_EOL))


/* Register a C enum.  */
SCM
gsdl_define_enum (const char *name, ...)
{
  va_list ap;
  char *symname;
  long value, max = 0, min = 0, count = 0; /* min was 0xffff --ttn */
  SCM enumstash, vec, table, sym;
  enum_struct *new_enum;

  /* Initialize the argument list.  */
  va_start (ap, name);

  /* First pass: count the args, find the max and min.  */
  symname = va_arg (ap, char*);
  while (symname)
    {
      count++;
      value = va_arg (ap, long);
      if (value > max)
        max = value;
      if (value < min)
        min = value;
      symname = va_arg (ap, char*);
    }

  /* Add one to make room for largest value.  */
  max++;

  /* Create an enum struct to hold our values.  */
  new_enum = (enum_struct *) malloc (sizeof (enum_struct));

  new_enum->min = min;
  new_enum->max = max;

  /* Create the enum table.  */
  vec = scm_dimensions_to_uniform_array (SCM_MAKINUM (max - min + 1),
                                         SCM_EOL,
                                         BOOL_FALSE);
  new_enum->vec = vec;

  /* Create the enum hash.  */
  table = MAKE_HASH_TABLE (count);
  new_enum->table = table;

  /* Reset the argument list.  (Is this safe?)  */
  va_start (ap, name);

  /* Second pass: fill the table and hash.  */
  while (count > 0)
    {
      symname = va_arg (ap, char*);
      value = va_arg (ap, long);
      sym = gh_symbol2scm (symname);
      gh_vector_set_x (vec, gh_long2scm (value - min), sym);
      scm_hashq_set_x (table, sym, gh_long2scm (value));
      count--;
    }

  /* Clean up.  */
  va_end (ap);

  /* Build and define the enum smob instance.  */
  SCM_NEWSMOB (enumstash, enum_tag, new_enum);
  gh_define (name, enumstash);
  return enumstash;
}


/* C level conversions */

long
gsdl_enum2long (SCM enumstash, SCM enumstash_type, int pos, const char *FUNC_NAME)
{
  enum_struct *enum_type;
  SCM index;
  long result = 0;

  enum_type = UNPACK_ENUM (enumstash_type);

  if (SCM_SYMBOLP (enumstash))
    {
      index = scm_hashq_ref (enum_type->table, enumstash, BOOL_FALSE);
      if (NOT_FALSEP (index))
        result = gh_scm2long (index);
    }
  else
    {
      ASSERT_EXACT (enumstash, pos);
      result = gh_scm2long (enumstash);
    }

  return result;
}

SCM
gsdl_long2enum (long value, SCM enumstash_type)
{
  enum_struct *enum_type = UNPACK_ENUM (enumstash_type);
  return gh_vector_ref (enum_type->vec, gh_long2scm (value - enum_type->min));
}


/* Scheme level conversions */

GH_DEFPROC (enumstash_enums, "enumstash-enums", 1, 0, 0,
            (SCM enumstash_type),
            "Return the list of symbols associated with @var{enum-type}.")
{
#define FUNC_NAME s_enumstash_enums
  SCM rv;
  enum_struct *enum_type;

  ASSERT_ENUM (enumstash_type, ARGH1);

  enum_type = UNPACK_ENUM (enumstash_type);
  rv = gh_call3 (hfold, acons, SCM_EOL, enum_type->table);
  {
    SCM ls = rv;
    while (! gh_null_p (ls))
      {
        gh_set_car_x (ls, gh_caar (ls));
        ls = gh_cdr (ls);
      }
  }
  return rv;
#undef FUNC_NAME
}

GH_DEFPROC (enum_to_number, "enum->number", 2, 0, 0,
            (SCM enumstash_type,
             SCM symbol),
            "Convert an enum number or symbol to a number.")
{
#define FUNC_NAME s_enum_to_number
  SCM table;
  enum_struct *enum_type;

  ASSERT_ENUM (enumstash_type, ARGH1);

  enum_type = UNPACK_ENUM (enumstash_type);
  table = enum_type->table;

  ASSERT_SYMBOL (symbol, ARGH2);

  /* Lookup and return the number in the pair.  */
  return scm_hashq_ref (table, symbol, BOOL_FALSE);
#undef FUNC_NAME
}

GH_DEFPROC (number_to_enum, "number->enum", 2, 0, 0,
            (SCM enumstash_type, SCM number),
            "Convert a number to an enum.")
{
#define FUNC_NAME s_number_to_enum
  SCM vec;
  long index;
  enum_struct *enum_type;

  ASSERT_ENUM (enumstash_type, ARGH1);

  enum_type = UNPACK_ENUM (enumstash_type);
  vec = enum_type->vec;

  ASSERT_EXACT (number, ARGH2);
  index = gh_scm2long (number) - enum_type->min;

  /* Return the numbered index into the vector.  */
  return gh_vector_ref (vec, gh_long2scm (index));
#undef FUNC_NAME
}


/* flagstash smob */

static unsigned long int flagstash_tag;

#define ASSERT_FLAGSTASH(obj,which) \
  ASSERT_SMOB (obj, flagstash_tag, which)

#define UNPACK_FLAGSTASH(smob) \
  (SMOBGET (smob, flagstash_t *))

static
size_t
free_flagstash (SCM smob)
{
  flagstash_t *stash = UNPACK_FLAGSTASH (smob);
  int count = stash->total;
  scm_must_free (stash->linear);

  return (count * sizeof (val_and_name_t *));
}

static
int
print_flagstash (SCM smob, SCM port, scm_print_state *ps)
{
  flagstash_t *stash = UNPACK_FLAGSTASH (smob);

  scm_puts      ("#<", port);
  scm_intprint                  (stash->total, 10, port);
  scm_putc      (' ', port);
  scm_puts                      (stash->name, port);
  scm_putc      ('>', port);
  return 1;                             /* non-zero => ok */
}

SCM
gsdl_make_flagstash (flagstash_t *stash)
{
  SCM smob;

  SCM_NEWSMOB (smob, flagstash_tag, stash);
  smob = scm_permanent_object (smob);
  {
    val_and_name_t *cur = stash->sparse;
    int count = stash->total;

    stash->linear =
      (val_and_name_t **) scm_must_malloc (count * sizeof (val_and_name_t *),
                                           "linear flag pointers");

    while (count)
      {
        if (cur->name && *(cur->name))
          {
            cur->sval = scm_permanent_object (gh_ulong2scm (cur->val));
            cur->sname = scm_permanent_object (gh_str02scm (cur->name));
            count--;
            stash->linear[count] = cur;
          }
        cur++;
      }
#if 0
    fprintf (stderr, "%s: %d (%d bytes) / %d (%d bytes)\n",
             stash->name,
             stash->total,
             stash->total * sizeof (val_and_name_t),
             cur - stash->sparse,
             (cur - stash->sparse) * sizeof (val_and_name_t));
#endif
  }
  stash->reverse_lookup_cache =
    scm_permanent_object (MAKE_HASH_TABLE (stash->total));

  return smob;
}


/* Converting from flags to ulong and back */

unsigned long
gsdl_flags2ulong (SCM flags, SCM stash, int pos, const char *FUNC_NAME)
{
  flagstash_t *s = UNPACK_FLAGSTASH (stash);
  val_and_name_t *hit;
  unsigned long result = 0;

  if (EXACTLY_FALSEP (flags) || gh_null_p (flags))
    return 0;

  if (gh_pair_p (flags))
    {
      SCM head;
      /* A list of symbols representing flags.  */
      while (! gh_null_p (flags))
        {
          ASSERT_SYMBOL (gh_car (flags), pos);
          head = gh_car (flags);
          hit = s->lookup (SCM_CHARS (head), SCM_LENGTH (head));
          if (hit)
            result |= hit->val;
          flags = gh_cdr (flags);
        }
    }
  else
    {
      ASSERT_SYMBOL (flags, pos);
      hit = s->lookup (SCM_CHARS (flags), SCM_LENGTH (flags));
      if (hit)
        result = hit->val;
    }

  return result;
}

SCM
gsdl_ulong2flags (unsigned long value, SCM stash)
{
  flagstash_t *s = UNPACK_FLAGSTASH (stash);
  int i;
  SCM rv = SCM_EOL;

  for (i = 0; i < s->total; i++)
    {
      val_and_name_t *cur = s->linear[i];
      if (cur->val == value)
        return gh_cons (cur->sname, rv);
      if (cur->val & value)
        {
          rv = gh_cons (cur->sname, rv);
          value &= ~(cur->val);
          if (! value)
            /* If we were to cache the translation, it would be done here.
               Probably `flagstash_t' needs to include info on disjointness
               and cache preference/tuning hints first.  */
            return rv;
        }
    }
  /* If we get here, that means `value' was not covered by the stash,
     which is not really an exceptional situation, but nonetheless one
     we should not gloss over (by returning `rv', for example).  We can
     always add "gsdl_ulong2flags_plus_remainder" later if needed.  */
  RETURN_FALSE;
}


/* Scheme level conversions */

GH_DEFPROC (flagstash_flags, "flagstash-flags", 1, 0, 0,
            (SCM stash),
            "Return a list of all the flags (symbols) in @var{stash},\n"
            "a flagstash object, in unspecified order.")
{
#define FUNC_NAME s_flagstash_flags
  int i;
  flagstash_t *cstash;
  SCM rv = SCM_EOL;

  ASSERT_FLAGSTASH (stash, ARGH1);
  cstash = UNPACK_FLAGSTASH (stash);

  for (i = 0; i < cstash->total; i++)
    rv = gh_cons (gh_symbol2scm (cstash->linear[i]->name), rv);
  return rv;
#undef FUNC_NAME
}

GH_DEFPROC (flags_to_number, "flags->number", 2, 0, 0,
            (SCM stash, SCM flags),
            "Use @var{stash} to convert @var{flags} to a number.\n"
            "@var{flags} is a list of symbols.")
{
#define FUNC_NAME s_flags_to_number
  ASSERT_FLAGSTASH (stash, ARGH1);

  RETURN_UINT (GSDL_FLAGS2ULONG (flags, stash, ARGH2));
#undef FUNC_NAME
}

GH_DEFPROC (number_to_flags, "number->flags", 2, 0, 0,
            (SCM stash, SCM number),
            "Use @var{stash} to convert @var{number} to a list\n"
            "of symbols.")
{
#define FUNC_NAME s_number_to_flags
  ASSERT_FLAGSTASH (stash, ARGH1);
  ASSERT_EXACT (number, ARGH2);

  return gsdl_ulong2flags (gh_scm2ulong (number), stash);
#undef FUNC_NAME
}


void
gsdl_init_enums (void)
{
  enum_tag = scm_make_smob_type ("SDL-enum", sizeof (enum_struct));
  scm_set_smob_mark (enum_tag, mark_enum);
  scm_set_smob_free (enum_tag, free_enum);

  flagstash_tag = scm_make_smob_type ("flagstash", sizeof (flagstash_t *));
  scm_set_smob_free  (flagstash_tag, free_flagstash);
  scm_set_smob_print (flagstash_tag, print_flagstash);

  acons = gh_lookup ("acons");
  hfold = gh_lookup ("hash-fold");

#include "sdlenums.x"
}

/* sdlenums.c ends here */
