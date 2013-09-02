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
#include "snuggle/fastint.h"


/* keeper of the konstants */

#include "k-count.h"

enum kdisp {
  peerless,
  flocking
};

static struct {
  unsigned long disposition;
  size_t        count;
  SCM           all[KOTK_N];
} konstants;

static void
note (enum kdisp disp, SCM k)
{
  size_t i = konstants.count++;

  if (KOTK_N == i)
    abort ();
  konstants.all[i] = k;
  konstants.disposition |= (disp << i);
}

#define KOTK_DISPOSITION(i)                     \
  (konstants.disposition & (1UL << (i))         \
   ? flocking                                   \
   : peerless)

DECLARE_SIMPLE_SYM (enums);
DECLARE_SIMPLE_SYM (flags);

typedef struct {
  const char   *who;
  kp_t        **p;
  flagstash_t **f;
} kref_mb;

static void kunpack (kp_t **p, flagstash_t **f, size_t i);

static SCM
kref (kref_mb *mb, SCM name)
{
  const char *FUNC_NAME = mb->who;
  kp_t *kp;
  flagstash_t *kf;
  size_t i;

  ASSERT_SYMBOL (name, 1);

  for (i = 0; i < konstants.count; i++)
    {
      kunpack (&kp, &kf, i);
      if (kp && EQ (SYMBOL (kp->ss.name), name))
        break;
      if (kf && EQ (SYMBOL (kf->ss.name), name))
        break;
    }
  if (konstants.count == i)
    SCM_MISC_ERROR ("no such stash: ~A", LIST1 (name));

  if (mb->p) *(mb->p) = kp;
  if (mb->f) *(mb->f) = kf;
  return konstants.all[i];
}


DECLARE_SYM (nonmember, "non-member-symbol");

static SCM_NORETURN void
sorry (const char *FUNC_NAME, const char *nick, SCM symbol)
{
  SCM args = LIST2 (STRING (nick), symbol);

  scm_error (SYM (nonmember), FUNC_NAME,
             "invalid ~A: ~A", args, BOOL_FALSE);
}

#define SORRY(nick,symbol)  sorry (FUNC_NAME, nick, symbol)

#define enum_nick "SDL-enum"

static smob_tag_t enum_tag;

#define ASSERT_ENUM(obj,which) \
  ASSERT_SMOB (obj, enum, which)

#define UNPACK_ENUM(smob) \
  (SMOBGET (smob, kp_t *))

static
SCM
mark_enum (SCM enumstash)
{
  kp_t *enum_smob = UNPACK_ENUM (enumstash);

  scm_gc_mark (enum_smob->table);
  return BOOL_FALSE;
}

static
int
print_enum (SCM smob, SCM port, UNUSED scm_print_state *ps)
{
  kp_t *stash = UNPACK_ENUM (smob);
  const struct symset *ss = &stash->ss;
  char buf[64];

  snprintf (buf, 64, "#<%zu SDL %s enums>", ss->count,
            ss->name ? ss->name : "(anonymous)");
  scm_puts (buf, port);
  return 1;                             /* non-zero => ok */
}


/* Register a C enum.  */
static SCM
register_kp (kp_t *kp)
{
  const struct symset *ss = &kp->ss;
  const uint8_t *pool = ss->pool;
  size_t count = ss->count;
  size_t i;
  SCM ht, smob;

  if (! (kp->linear = malloc (count * sizeof (SCM))))
    abort ();
  ht = GC_PROTECT (MAKE_HASH_TABLE (count));
  for (i = 0; i < count; i++)
    {
      /* A ridiculous variable solely to avoid a GCC "warning: signed
         and unsigned type in conditional expression [-Wsign-compare]".
         Maybe there's a better way?  */
      long ival = i;
      long value = (kp->classic
                    ? ival
                    : (kp->offset
                       ? ival + kp->val[0]
                       : kp->val[i]));
      uint8_t len = *pool++;
      SCM sym = GC_PROTECT (SYMBOLN ((char *) pool, len));

      kp->linear[i] = sym;
      scm_hashq_set_x (ht, sym, NUM_FASTINT (i));
      scm_hashq_set_x (ht, NUM_FASTINT (value), sym);
      GC_UNPROTECT (sym);
      pool += len;
    }
  kp->table = ht;
  SCM_NEWSMOB (smob, enum_tag, kp);
  GC_UNPROTECT (ht);

  note (peerless, smob);
  return PERMANENT (smob);
}

static void
register_kp_v (size_t count, kp_init_t v[count])
{
  size_t i;
  kp_init_t *e;

  for (i = 0, e = v;
       i < count;
       i++, e++)
    *(e->smob) = register_kp (e->kp);
}

static inline SCM
lookup (SCM key, const kp_t *e)
{
  return scm_hashq_ref (e->table, key, BOOL_FALSE);
}


/* C level conversions */

static long
enum2long (const sym2num_cc_t *cc, SCM obj)
{
  const char *FUNC_NAME = cc->who;
  long result = 0;
  kp_t *e = UNPACK_ENUM (cc->stash);
  long idx;

  if (SCM_SYMBOLP (obj))
    {
      SCM sidx = lookup (obj, e);

      if (EXACTLY_FALSEP (sidx))
        SORRY (e->ss.name, obj);
      idx = C_FASTINT (sidx);
      result = (e->classic
                ? idx
                : (e->offset
                   ? idx + e->val[0]
                   : e->val[idx]));
    }
  else
    {
      ASSERT_INTEGER (obj, cc->pos);
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

static SCM
resolve_kp (const char *FUNC_NAME, SCM stash)
{
  if (SYMBOLP (stash))
    {
      kp_t *kp;
      kref_mb mb = {
        .who = FUNC_NAME,
        .p = &kp,
        .f = NULL
      };
      SCM resolved = kref (&mb, stash);

      ASSERT_TYPE (kp, stash, 1, enum_nick);
      return resolved;
    }

  ASSERT_ENUM (stash, 1);
  return stash;
}

#define RESOLVE_KP(stash)  resolve_kp (FUNC_NAME, stash)

#define DECLINIT_KP2NUM_CC(POS,STASH)           \
  const sym2num_cc_t _CC_ ## POS =              \
    {                                           \
      .stash = RESOLVE_KP (STASH),              \
      .who = FUNC_NAME,                         \
      .pos = POS                                \
    }

PRIMPROC
(enum_to_number, "enum->number", 2, 0, 0,
 (SCM stash, SCM symbol),
 doc: /***********
Return the number in @var{stash} associated with @var{symbol}.  */)
{
#define FUNC_NAME s_enum_to_number
  DECLINIT_KP2NUM_CC (2, stash);

  ASSERT_SYMBOL (symbol, 2);

  return NUM_FASTINT
    (ENUM2LONG (2, symbol));
#undef FUNC_NAME
}

PRIMPROC
(number_to_enum, "number->enum", 2, 0, 0,
 (SCM stash, SCM number),
 doc: /***********
Return the symbol associated with @var{number}, or @code{#f}
if it does not belong to @var{stash}.  */)
{
#define FUNC_NAME s_number_to_enum
  stash = RESOLVE_KP (stash);
  ASSERT_INTEGER (number, 2);
  return lookup (number, UNPACK_ENUM (stash));
#undef FUNC_NAME
}


/* flagstash smob */

#define flagstash_nick "SDL-flagstash"

static smob_tag_t flagstash_tag;

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
print_flagstash (SCM smob, SCM port, UNUSED scm_print_state *ps)
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

  stash->full = 0;
  ht = GC_PROTECT (MAKE_HASH_TABLE (count));
  for (i = 0; i < count; i++)
    {
      uint8_t len = *pool;
      SCM *sym = stash->linear + i;

      stash->full |= stash->val[i];
      *sym = GC_PROTECT (SYMBOLN ((char *) ++pool, len));
      scm_hashq_set_x (ht, GC_UNPROTECT (*sym), NUM_FASTINT (i));
      pool += len;
    }
  stash->ht = ht;
  SCM_NEWSMOB (smob, flagstash_tag, stash);
  GC_UNPROTECT (ht);

  note (flocking, smob);
  return PERMANENT (smob);
}

static void
register_kf_v (size_t count, kf_init_t v[count])
{
  size_t i;
  kf_init_t *e;

  for (i = 0, e = v;
       i < count;
       i++, e++)
    *(e->smob) = make_flagstash (e->stash);
}


/* Converting from flags to ulong and back */

static unsigned long
flags2ulong (const sym2num_cc_t *cc, SCM flags)
{
  const char *FUNC_NAME = cc->who;
  flagstash_t *s = UNPACK_FLAGSTASH (cc->stash);
  unsigned long result = 0;

  if (EXACTLY_FALSEP (flags) || NULLP (flags))
    return 0;

  if (EXACTLY_TRUEP (flags))
    return s->full;

#define LOOKUP_IOR(x)  do                               \
    {                                                   \
      SCM sidx;                                         \
                                                        \
      ASSERT_SYMBOL (x, cc->pos);                       \
      sidx = scm_hashq_ref (s->ht, x, BOOL_FALSE);      \
      if (EXACTLY_FALSEP (sidx))                        \
        SORRY (s->ss.name, x);                          \
      result |= s->val[C_FASTINT (sidx)];               \
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
  size_t i;
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

static SCM
resolve_kf (const char *FUNC_NAME, SCM stash)
{
  if (SYMBOLP (stash))
    {
      flagstash_t *kf;
      kref_mb mb = {
        .who = FUNC_NAME,
        .f = &kf,
        .p = NULL
      };
      SCM resolved = kref (&mb, stash);

      ASSERT_TYPE (kf, stash, 1, flagstash_nick);
      return resolved;
    }

  ASSERT_FLAGSTASH (stash, 1);
  return stash;
}

#define RESOLVE_KF(stash)  resolve_kf (FUNC_NAME, stash)

#define DECLINIT_KF2NUM_CC(POS,STASH)           \
  const sym2num_cc_t _CC_ ## POS =              \
    {                                           \
      .stash = RESOLVE_KF (STASH),              \
      .who = FUNC_NAME,                         \
      .pos = POS                                \
    }

PRIMPROC
(flags_to_number, "flags->number", 2, 0, 0,
 (SCM stash, SCM flags),
 doc: /***********
Use @var{stash} to convert @var{flags} to a number.
@var{flags} is a list of symbols;
or @code{#f}, which is taken as the empty list;
or @code{#t}, which is taken as the list of all
possible symbols in @var{stash}.  */)
{
#define FUNC_NAME s_flags_to_number
  DECLINIT_KF2NUM_CC (2, stash);

  RETURN_UINT (FLAGS2ULONG (2, flags));
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
  stash = RESOLVE_KF (stash);
  ASSERT_INTEGER (number, 2);
  return ulong2flags (C_ULONG (number), stash);
#undef FUNC_NAME
}


static void
kunpack (kp_t **p, flagstash_t **f, size_t i)
{
  SCM k = konstants.all[i];

  switch (KOTK_DISPOSITION (i))
    {
    case peerless:
      *p = UNPACK_ENUM (k);
      *f = NULL;
      break;
    case flocking:
      *p = NULL;
      *f = UNPACK_FLAGSTASH (k);
    }
}

PRIMPROC
(kotk, "kotk", 0, 1, 0,
 (SCM name),
 doc: /***********
Return the contents of stash @var{name} (a symbol), as
an alist with symbolic keys, integer values.
If @var{name} is omitted, the keys are the names of the all
the enum- and flagstashes, and the values have the form:

@example
(N TYPE)
@end example

@noindent
where @var{n} is the count of symbols in that stash,
and @var{type} is a symbol: @code{enums} or @code{flags}.  */)
{
#define FUNC_NAME s_kotk
  kp_t *kp;
  flagstash_t *kf;
  size_t i;
  SCM k, symbol, rv = SCM_EOL;

#define ACC(KEXP,VEXP)  do                      \
    {                                           \
      /* Stash ‘symbol’ for sake of ‘VEXP’.  */ \
      symbol = KEXP;                            \
      rv = scm_acons (symbol, VEXP, rv);        \
    }                                           \
  while (0)

  if (UNBOUNDP (name))
    {
      i = konstants.count;
      while (i--)
        {
          kunpack (&kp, &kf, i);
          ACC (SYMBOL (kp
                       ? kp->ss.name
                       : kf->ss.name),
               LIST2 (NUM_FASTINT (kp
                                   ? kp->ss.count
                                   : kf->ss.count),
                      (kp
                       ? SYM (enums)
                       : SYM (flags))));
        }
      return rv;
    }

  {
    kref_mb mb = {
      .who = FUNC_NAME,
      .p = &kp,
      .f = &kf
    };

    k = kref (&mb, name);
  }

  if (kp)
    for (i = kp->ss.count; i--;)
      ACC (kp->linear[i], enum_to_number (k, symbol));
  else
    for (i = 0; i < kf->ss.count; i++)
      ACC (kf->linear[i], flags_to_number (k, symbol));

  return rv;
#undef ACC
#undef FUNC_NAME
}


void
gsdl_init_enums (void)
{
  btw->flags2ulong = flags2ulong;
  btw->ulong2flags = ulong2flags;
  btw->enum2long = enum2long;
  btw->long2enum = long2enum;

  btw->register_kp_v = register_kp_v;
  btw->register_kf_v = register_kf_v;

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
