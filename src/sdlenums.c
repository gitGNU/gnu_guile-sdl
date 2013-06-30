/* sdlenums.c -- Enum helper functions
 *
 * Copyright (C) 2003, 2004, 2005, 2009, 2011, 2012, 2013 Thien-Thi Nguyen
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

#include "guile-sdl.h"
#include <stdlib.h>
#include <stdio.h>
#include "snuggle/mkhash.h"

#define enum_nick "SDL-enum"

static long enum_tag;

#define ASSERT_ENUM(obj,which) \
  ASSERT_SMOB (obj, enum, which)

#define UNPACK_ENUM(smob) \
  (SMOBGET (smob, enum_struct *))

static
SCM
mark_enum (SCM enumstash)
{
  enum_struct *enum_smob = UNPACK_ENUM (enumstash);

  scm_gc_mark (enum_smob->table);
  RETURN_FALSE;
}

static
int
print_enum (SCM smob, SCM port, scm_print_state *ps)
{
  enum_struct *stash = UNPACK_ENUM (smob);
  char buf[64];

  snprintf (buf, 64, "#<%zu SDL %s enums>", stash->count,
            stash->name ? stash->name : "(anonymous)");
  scm_puts (buf, port);
  return 1;                             /* non-zero => ok */
}


/* Register a C enum.  */
static SCM
define_enum (const char *name, size_t count, valaka_t *backing)
{
  size_t i;
  SCM enumstash, table, sym;
  enum_struct *s;
  valaka_t *b;

  /* Create an enum struct to hold our values.  */
  s = malloc (sizeof (enum_struct));
  s->count = count;
  s->backing = backing;
  s->name = name;

  /* Create the enum hash.  */
  table = MAKE_HASH_TABLE (count);
  for (i = 0; i < count; i++)
    {
      b = backing + i;
      sym = b->aka.symbol = SYMBOL (b->aka.rozt);
      scm_hashq_set_x (table, sym, NUM_INT (i));
      scm_hashq_set_x (table, NUM_LONG (b->value), sym);
    }
  s->table = table;

  /* Build and define the enum smob instance.  */
  SCM_NEWSMOB (enumstash, enum_tag, s);
  if (name)
    DEFINE_PUBLIC (name, enumstash);
  else
    enumstash = PERMANENT (enumstash);
  return enumstash;
}

static inline SCM
lookup (SCM key, enum_struct *e)
{
  return scm_hashq_ref (e->table, key, BOOL_FALSE);
}


/* C level conversions */

static long
enum2long (SCM obj, SCM enumstash, int pos, const char *FUNC_NAME)
{
  long result = 0;
  enum_struct *e = UNPACK_ENUM (enumstash);

  if (SCM_SYMBOLP (obj))
    {
      obj = lookup (obj, e);
      if (NOT_FALSEP (obj))
        result = e->backing[C_INT (obj)].value;
    }
  else
    {
      ASSERT_INTEGER (obj, pos);
      result = C_LONG (obj);
    }

  return result;
}

static SCM
long2enum (long value, SCM enumstash)
{
  return lookup (NUM_LONG (value), UNPACK_ENUM (enumstash));
}


/* Scheme level conversions */

PRIMPROC
(enumstash_enums, "enumstash-enums", 1, 0, 0,
 (SCM enumstash),
 doc: /***********
Return the list of symbols belonging to @var{enumstash}.  */)
{
#define FUNC_NAME s_enumstash_enums
  SCM rv;
  enum_struct *enum_type;
  size_t i;

  ASSERT_ENUM (enumstash, 1);

  enum_type = UNPACK_ENUM (enumstash);
  rv = SCM_EOL;
  for (i = 0; i < enum_type->count; i++)
    rv = CONS (enum_type->backing[i].aka.symbol, rv);
  return rv;
#undef FUNC_NAME
}

PRIMPROC
(enum_to_number, "enum->number", 2, 0, 0,
 (SCM enumstash,
  SCM symbol),
 doc: /***********
Return the number associated with @var{symbol}, or @code{#f}
if it does not belong to @var{enumstash}.  */)
{
#define FUNC_NAME s_enum_to_number
  enum_struct *e;
  SCM idx;

  ASSERT_ENUM (enumstash, 1);
  ASSERT_SYMBOL (symbol, 2);

  e = UNPACK_ENUM (enumstash);
  idx = lookup (symbol, e);
  return EXACTLY_FALSEP (idx)
    ? idx
    : NUM_INT (e->backing[C_INT (idx)].value);
#undef FUNC_NAME
}

PRIMPROC
(number_to_enum, "number->enum", 2, 0, 0,
 (SCM enumstash, SCM number),
 doc: /***********
Return the symbol associated with @var{number}, or @code{#f}
if it does not belong to @var{enumstash}.  */)
{
#define FUNC_NAME s_number_to_enum
  ASSERT_ENUM (enumstash, 1);
  ASSERT_INTEGER (number, 2);
  return lookup (number, UNPACK_ENUM (enumstash));
#undef FUNC_NAME
}


/* flagstash smob */

#define flagstash_nick "SDL-flagstash"

static unsigned long int flagstash_tag;

#define ASSERT_FLAGSTASH(obj,which) \
  ASSERT_SMOB (obj, flagstash, which)

#define UNPACK_FLAGSTASH(smob) \
  (SMOBGET (smob, flagstash_t *))

static
SCM
mark_flagstash (SCM smob)
{
  flagstash_t *fs = UNPACK_FLAGSTASH (smob);

  return fs->ht;
}

static
int
print_flagstash (SCM smob, SCM port, scm_print_state *ps)
{
  flagstash_t *stash = UNPACK_FLAGSTASH (smob);
  const struct symset *ss = &stash->ss;
  char buf[64];

  snprintf (buf, 64, "#<%zu SDL %s flags>", ss->count, ss->name);
  scm_puts (buf, port);
  return 1;                             /* non-zero => ok */
}

static SCM
make_flagstash (flagstash_t *stash)
{
  const struct symset *ss = &stash->ss;
  const uint8_t *pool = ss->pool;
  size_t count = ss->count;
  size_t i;
  SCM ht, smob;

  if (! (stash->linear = malloc (count * sizeof (SCM))))
    abort ();

  ht = GC_PROTECT (MAKE_HASH_TABLE (count));
  for (i = 0; i < count; i++)
    {
      uint8_t len = *pool;
      SCM *sym = stash->linear + i;

      *sym = GC_PROTECT (SYMBOLN ((char *) ++pool, len));
      scm_hashq_set_x (ht, GC_UNPROTECT (*sym), NUM_INT (i));
      pool += len;
    }
  stash->ht = ht;
  SCM_NEWSMOB (smob, flagstash_tag, stash);
  GC_UNPROTECT (ht);

  return PERMANENT (smob);
}


/* Converting from flags to ulong and back */

static unsigned long
flags2ulong (SCM flags, SCM stash, int pos, const char *FUNC_NAME)
{
  flagstash_t *s = UNPACK_FLAGSTASH (stash);
  unsigned long result = 0;

  if (EXACTLY_FALSEP (flags) || NULLP (flags))
    return 0;

#define LOOKUP_IOR(x)  do                               \
    {                                                   \
      SCM sidx;                                         \
                                                        \
      ASSERT_SYMBOL (x, pos);                           \
      sidx = scm_hashq_ref (s->ht, x, BOOL_FALSE);      \
      if (NOT_FALSEP (sidx))                            \
        result |= s->val[C_INT (sidx)];                 \
    }                                                   \
  while (0)

  if (PAIRP (flags))
    {
      /* A list of symbols representing flags.  */
      while (! NULLP (flags))
        {
          LOOKUP_IOR (CAR (flags));
          flags = CDR (flags);
        }
    }
  else
    LOOKUP_IOR (flags);

#undef LOOKUP_IOR

  return result;
}

static SCM
ulong2flags (unsigned long value, SCM stash)
{
  flagstash_t *s = UNPACK_FLAGSTASH (stash);
  size_t count = s->ss.count;
  int i;
  SCM rv = SCM_EOL;

  /* Is nothing really nothing?  A stash is in descending order, so
     check the last item to see if 0x0 is legit.  If not, return early;
     don't bother looking for it and, more importantly, avoid producing
     a nonsensical "remainder 0" result.  */
  if (! value
      && s->val[count - 1])
    return rv;

  for (i = 0; i < count; i++)
    {
      unsigned long cur = s->val[i];

      if (cur == (cur & value))
        {
          rv = CONS (s->linear[i], rv);
          value &= ~cur;
          if (! value)
            return rv;
        }
    }

  /* If we get here, that means `value' was not covered by the stash,
     which is not really an exceptional situation, but nonetheless one
     we should not gloss over (by returning naked `rv', for example).  */
  return CONS (NUM_ULONG (value), rv);
}


/* Scheme level conversions */

PRIMPROC
(flagstash_flags, "flagstash-flags", 1, 0, 0,
 (SCM stash),
 doc: /***********
Return a list of all the flags (symbols) in @var{stash},
a flagstash object, in unspecified order.  */)
{
#define FUNC_NAME s_flagstash_flags
  int i;
  flagstash_t *cstash;
  size_t count;
  SCM rv = SCM_EOL;

  ASSERT_FLAGSTASH (stash, 1);
  cstash = UNPACK_FLAGSTASH (stash);
  count = cstash->ss.count;

  for (i = 0; i < count; i++)
    rv = CONS (cstash->linear[i], rv);
  return rv;
#undef FUNC_NAME
}

PRIMPROC
(flags_to_number, "flags->number", 2, 0, 0,
 (SCM stash, SCM flags),
 doc: /***********
Use @var{stash} to convert @var{flags} to a number.
@var{flags} is a list of symbols.  */)
{
#define FUNC_NAME s_flags_to_number
  ASSERT_FLAGSTASH (stash, 1);

  RETURN_UINT (GSDL_FLAGS2ULONG (flags, stash, 2));
#undef FUNC_NAME
}

PRIMPROC
(number_to_flags, "number->flags", 2, 0, 0,
 (SCM stash, SCM number),
 doc: /***********
Use @var{stash} to convert @var{number} to a list of symbols.
If the flags in @var{stash} are not sufficient to decode
@var{number}, the first element of the list is the numeric
remainder.  */)
{
#define FUNC_NAME s_number_to_flags
  ASSERT_FLAGSTASH (stash, 1);
  ASSERT_INTEGER (number, 2);

  return ulong2flags (C_ULONG (number), stash);
#undef FUNC_NAME
}


void
gsdl_init_enums (void)
{
  btw->make_flagstash = make_flagstash;
  btw->flags2ulong = flags2ulong;
  btw->ulong2flags = ulong2flags;
  btw->define_enum = define_enum;
  btw->enum2long = enum2long;
  btw->long2enum = long2enum;

  DEFSMOB (enum_tag, enum_nick,
           mark_enum,
           NULL,
           print_enum);

  DEFSMOB (flagstash_tag, flagstash_nick,
           mark_flagstash,
           NULL,
           print_flagstash);

#include "sdlenums.x"
}

/* sdlenums.c ends here */
