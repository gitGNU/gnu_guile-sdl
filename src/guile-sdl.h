/* guile-sdl.h */

#ifndef __GUILE_SDL_H
#define __GUILE_SDL_H

/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2011 Thien-Thi Nguyen
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


/* The build environment.  */
#include "config.h"

/* System support.

   Ideally, we would wrap each #include with the appropriate
   ‘#ifdef HAVE_foo_H’ ... ‘#endif’, but that's ugly and the ones
   we are interested in are all pretty much standardized nowadays.

   Let's see how long this insufferable anti-portability attitude
   stands in the Real World!  :-D  */
#include <stdint.h>
#include <SDL/SDL.h>


/* Guile.  */

#include <libguile.h>

#define PERMANENT  scm_permanent_object

#if  defined (SCM_MAJOR_VERSION)                                \
  && defined (SCM_MINOR_VERSION)                                \
  && (2 == SCM_MAJOR_VERSION                                    \
      || (1 == SCM_MAJOR_VERSION && 8 <= SCM_MINOR_VERSION))
#define GI_LEVEL_NOT_YET_1_8  0
#else
#define GI_LEVEL_NOT_YET_1_8  1
#endif

#if GI_LEVEL_NOT_YET_1_8
#include <guile/gh.h>
#define NULLP(obj)        (gh_null_p (obj))
#define PAIRP(obj)        (gh_pair_p (obj))
#define EXACTP(obj)       (gh_exact_p (obj))
#define SYMBOLP(obj)      (gh_symbol_p (obj))
#define VECTORP            gh_vector_p
#define STRINGP            gh_string_p
#define BOOLEAN            gh_bool2scm
#define NUM_INT            gh_int2scm
#define NUM_LONG           gh_long2scm
#define NUM_ULONG          gh_ulong2scm
#define SYMBOL             gh_symbol2scm
#define STRING             gh_str02scm
#define BSTRING            gh_str2scm
#define C_BOOL             gh_scm2bool
#define C_CHAR             gh_scm2char
#define C_INT              gh_scm2int
#define C_LONG             gh_scm2long
#define C_ULONG            gh_scm2ulong
#define C_DOUBLE           gh_scm2double
#define EQ                 gh_eq_p
#define CONS               gh_cons
#define CAR                gh_car
#define CAAR               gh_caar
#define CDR                gh_cdr
#define SETCAR             gh_set_car_x
#define VECLENGTH          gh_vector_length
#define UVECLENGTH         gh_uniform_vector_length
#define CALL0              gh_call0
#define CALL1              gh_call1
#define CALL3              gh_call3
#define GC_PROTECT         scm_protect_object
#define GC_UNPROTECT       scm_unprotect_object
#define LOOKUP             gh_lookup
#define DEFINE_PUBLIC      gh_define
#define MODULE_LOOKUP      gh_module_lookup
#else
#define NULLP(obj)        (scm_is_null (obj))
#define PAIRP(obj)        (scm_is_true (scm_pair_p (obj)))
#define EXACTP(obj)       (scm_is_true (scm_exact_p (obj)))
#define SYMBOLP(obj)      (scm_is_symbol (obj))
#define VECTORP            scm_is_vector
#define STRINGP            scm_is_string
#define BOOLEAN            scm_from_bool
#define NUM_INT            scm_from_int
#define NUM_LONG           scm_from_long
#define NUM_ULONG          scm_from_ulong
#define SYMBOL             scm_from_locale_symbol
#define STRING             scm_from_locale_string
#define BSTRING            scm_from_locale_stringn
#define C_BOOL             scm_to_bool
#define C_CHAR(c)          C_INT (scm_char_to_integer (c))
#define C_INT              scm_to_int
#define C_LONG             scm_to_long
#define C_ULONG            scm_to_ulong
#define C_DOUBLE           scm_to_double
#define EQ                 scm_is_eq
#define CONS               scm_cons
#define CAR                scm_car
#define CAAR               scm_caar
#define CDR                scm_cdr
#define SETCAR             scm_set_car_x
#define VECLENGTH          scm_c_vector_length
#define UVECLENGTH         scm_c_uniform_vector_length
#define CALL0              scm_call_0
#define CALL1              scm_call_1
#define CALL3              scm_call_3
#define GC_PROTECT         scm_gc_protect_object
#define GC_UNPROTECT       scm_gc_unprotect_object
#define LOOKUP(name)      (scm_variable_ref (scm_c_lookup (name)))
#define DEFINE_PUBLIC(name,value)  do           \
    {                                           \
      scm_c_define (name, value);               \
      scm_c_export (name, NULL);                \
    }                                           \
  while (0)
#define MODULE_LOOKUP(module, name)                     \
  scm_variable_ref (scm_c_module_lookup (module, name))
#endif


/* Abstractions for Scheme objects to C string conversion.  */

typedef struct {
  char *s;
  size_t len;
} range_t;

#define RS(svar)    c ## svar .s
#define RLEN(svar)  c ## svar .len

#if GI_LEVEL_NOT_YET_1_8

#define FINANGLABLE_SCHEME_STRING_FROM_SYMBOL(sym)      \
  scm_string_copy (scm_symbol_to_string (sym))

/* Coerce a Scheme (sub)string that is to be used in contexts where the
   extracted C string is expected to be read-only and zero-terminated.  We
   check this condition precisely instead of simply coercing all (sub)strings,
   to avoid waste for those substrings that may in fact (by lucky accident)
   already satisfy the condition.  */
#define ROZT_X(x)                                       \
  if (SCM_ROCHARS (x) [SCM_ROLENGTH (x)])               \
    x = BSTRING (SCM_ROCHARS (x), SCM_ROLENGTH (x))

#define _FINANGLE(svar,p1)  do                  \
    {                                           \
      if (p1)                                   \
        ROZT_X (svar);                          \
      RS (svar) = SCM_ROCHARS (svar);           \
      RLEN (svar) = SCM_ROLENGTH (svar);        \
    }                                           \
  while (0)

#define UNFINANGLE(svar)

#else  /* !GI_LEVEL_NOT_YET_1_8 */

#define FINANGLABLE_SCHEME_STRING_FROM_SYMBOL  scm_symbol_to_string

#define REND(svar)          RS (svar) [RLEN (svar)]
#define NUL_AT_END_X(svar)  REND (svar) = '\0'

#define _FINANGLE(svar,p1)  do                                  \
    {                                                           \
      RS (svar) = scm_to_locale_stringn (svar, &RLEN (svar));   \
      if (RS (svar))                                            \
        {                                                       \
          if (p1 && REND (svar))                                \
            {                                                   \
              RS (svar) = realloc (RS (svar), 1 + RLEN (svar)); \
              NUL_AT_END_X (svar);                              \
            }                                                   \
        }                                                       \
      else                                                      \
        RS (svar) = strdup ("");                                \
    }                                                           \
  while (0)

#define UNFINANGLE(svar)  free (RS (svar))

#endif  /* !GI_LEVEL_NOT_YET_1_8 */

/* Use ‘FINANGLE_RAW’ when the consumer of the C string takes full range
   (start address plus length) info.  Otherwise, ‘FINANGLE’.  */

#define FINANGLE_RAW(svar)  _FINANGLE (svar, 0)
#define FINANGLE(svar)      _FINANGLE (svar, 1)


#ifdef HAVE_GUILE_MODSUP_H
#include <guile/modsup.h>

#define IMPORT_MODULE      GH_USE_MODULE
#define SELECT_MODULE_VAR  GH_SELECT_MODULE_VAR

#define MOD_INIT_LINK_THUNK  GH_MODULE_LINK_FUNC

#else  /* ! HAVE_GUILE_MODSUP_H */

/*:Declare, define and document a C function callable from Scheme.

   The C function is declared `static' and is "exported" later by the the
   module initialization routine.  @var{fname} is the name of the C function
   (must be a valid C identifier).  @var{primname}, a string, is the name
   visible from Scheme code.  @var{req}, @var{opt} and @var{var} are each
   integers quantifying the number of required, optional and rest args are in
   the @var{arglist}.  Note that @var{var} can be only 0 or 1.  @var{arglist}
   is the C-style argument list for the function.  The type for every element
   must be @code{SCM}.  @code{docstring} is one or more strings that describe
   the function.  Each string except the last must end in @code{\n}.
*/
#define GH_DEFPROC(fname, primname, req, opt, var, arglist, docstring) \
  SCM_SNARF_HERE (static SCM fname arglist;)                           \
  SCM_DEFINE (fname, primname, req, opt, var, arglist, docstring)

#define IMPORT_MODULE(cvar,fullname)                    \
SCM_SNARF_HERE (static const char s_ ## cvar[] =        \
                fullname "#_#_"; static SCM cvar)       \
SCM_SNARF_INIT (cvar = PERMANENT                        \
                (scm_resolve_module                     \
                 (scm_c_read_string (s_ ## cvar)));)

#define SELECT_MODULE_VAR(cvar,m_cvar,s_name)                           \
SCM_SNARF_HERE (static SCM cvar)                                        \
SCM_SNARF_INIT (cvar = PERMANENT (MODULE_LOOKUP (m_cvar, s_name));)

#define MOD_INIT_LINK_THUNK(pretty,frag,func)  \
void scm_init_ ## frag ## _module (void) { func (); }

#endif  /* ! HAVE_GUILE_MODSUP_H */

#if      defined GH_DEFPROC
#define PRIMPROC GH_DEFPROC
#elif    defined SCM_DEFINE_PUBLIC
#define PRIMPROC SCM_DEFINE_PUBLIC
#elif    defined SCM_DEFINE
#define PRIMPROC SCM_DEFINE
#else
#error "guile deficient: not even SCM_DEFINE available!"
#endif

#define PACK_POINTER(x)    (scm_object_address (PTR2SCM (x)))
#define UNPACK_POINTER(x)  ((void *) C_ULONG (x))

/* Booleans.  */

#define BOOL_FALSE (SCM_BOOL_F)
#define BOOL_TRUE  (SCM_BOOL_T)

#define NOT_FALSEP(x)      (SCM_NFALSEP (x))
#define EXACTLY_FALSEP(x)  (SCM_FALSEP (x))
#define EXACTLY_TRUEP(x)   (EQ ((x), BOOL_TRUE))

#define SET_FALSE(cvar) \
  cvar = BOOL_FALSE

#define SET_TRUE(cvar) \
  cvar = BOOL_TRUE

#define RETURN_FALSE \
  return BOOL_FALSE

#define RETURN_TRUE \
  return BOOL_TRUE

/* Argument validation.  */

#define ASSERT_EXACT(obj,n) \
  SCM_ASSERT (NOT_FALSEP (scm_exact_p ((obj))), (obj), n, FUNC_NAME)

#define ASSERT_VECTOR(obj,n) \
  SCM_ASSERT (VECTORP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_SYMBOL(obj,n) \
  SCM_ASSERT (SCM_SYMBOLP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_STRING(obj,n) \
  SCM_ASSERT (STRINGP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_NUMBER(obj,n) \
  SCM_ASSERT (NOT_FALSEP (scm_number_p ((obj))), (obj), n, FUNC_NAME)

#define ASSERT_CHAR(obj,n) \
  SCM_ASSERT (SCM_CHARP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_LIST(obj,n) \
  SCM_ASSERT (NULLP (obj) || PAIRP (obj), (obj), n, FUNC_NAME)

#define BOUNDP(x)    (! SCM_EQ_P (x, SCM_UNDEFINED))
#define UNBOUNDP(x)    (SCM_EQ_P (x, SCM_UNDEFINED))

#define UNBOUND_MEANS_FALSE(x)  if (UNBOUNDP (x)) SET_FALSE (x)

/* Enums and flagstashes.  */

typedef union {
  const char const *rozt;
  SCM symbol;
} aka_t;

typedef struct {
  const long int value;
  aka_t aka;
} valaka_t;

#define VALAKA(x)  { x, { #x } }

typedef SCM (define_enum_t) (const char *name, size_t count,
                             valaka_t *backing);

SCM gsdl_define_enum (const char *name, size_t count, valaka_t *backing);

#define DEFINE_ENUM(name,backing)               \
  btw->define_enum                              \
  (name,                                        \
   sizeof (backing) / sizeof (valaka_t),        \
   backing)

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

typedef struct flagstash {
  const char const *name;
  const size_t total;
  const unsigned long const *val;
  aka_t *aka;
  SCM ht;
} flagstash_t;

typedef SCM (make_flagstash_t) (flagstash_t *stash);

SCM gsdl_make_flagstash (flagstash_t *stash);

/* Symbols.  */

#define DECLARE_SYM(frag,string) \
  SCM_SYMBOL (gsdl_sym_ ## frag, string)

#define DECLARE_SIMPLE_SYM(frag) \
  DECLARE_SYM (frag, # frag)

#define SYM(frag)  (gsdl_sym_ ## frag)

/* Smobs.  */

#if GI_LEVEL_NOT_YET_1_8

#define DEFSMOB(tagvar,name,m,f,p)                              \
  tagvar = scm_make_smob_type_mfpe (name, 0, m, f, p, NULL)

#define GCMALLOC(sz,what)    scm_must_malloc (sz, what)
#define GCFREE(ptr,what)     scm_must_free (ptr)
#define GCRV(ptr)            sizeof (*ptr)

#else  /* !GI_LEVEL_NOT_YET_1_8 */

#define DEFSMOB(tagvar,name,m,f,p)  do          \
    {                                           \
      tagvar = scm_make_smob_type (name, 0);    \
      if (NULL != m)                            \
        scm_set_smob_mark (tagvar, m);          \
      if (NULL != f)                            \
        scm_set_smob_free (tagvar, f);          \
      if (NULL != p)                            \
        scm_set_smob_print (tagvar, p);         \
    }                                           \
  while (0)

#define GCMALLOC(sz,what)    scm_gc_malloc (sz, what)
#define GCFREE(ptr,what)     scm_gc_free (ptr, sizeof (*ptr), what)
#define GCRV(ptr)            0

#endif  /* !GI_LEVEL_NOT_YET_1_8 */

/* useful type-checking for smobs */
#define ASSERT_SMOB(smob, tag, which)                   \
  SCM_ASSERT ((SCM_NIMP (smob)                          \
               && (long) SCM_CAR (smob) == (tag)),      \
              (smob), (which), FUNC_NAME)

#define SMOBGET(smob,c_type)       ((c_type) SCM_SMOB_DATA (smob))
#define SMOBSET(smob,val)          (SCM_SET_SMOB_DATA (smob, val))

#define SMOBFIELD(c_type,c_field)  (SMOBGET (obj, c_type)->c_field)

/* {wrapping "current" (libsdl internal) objects}

   Some libsdl funcs return, instead of a newly constructed object, an
   an object currently in use internal to libsdl.  For these, it is
   incorrect to call the (SDL) type's free func in the smob's free func.

   To distinguish these objects, we use a "possibly freeable" structure
   to wrap the actual object, and set ‘.internalp’ as appropriate.
   In the smob free func, an object with ‘.internalp’ set is not
   passed to its (SDL) type's free func.  */

#define DECLARE_PF(ACTUAL)                      \
  typedef struct {                              \
    SDL_ ## ACTUAL *object;                     \
    int internalp;                              \
  } PF_ ## ACTUAL

struct obtw
{
  long smob_tags[4];
  /* These are: color, rect, surface, pixel format.  */

  make_flagstash_t *make_flagstash;
  define_enum_t *define_enum;
};

#if 1 == GUILE_SDL_OPTIONAL_MODULE
static
#else
#define btw  gsdl_btw
extern
#endif
struct obtw *btw;

/* Return values.  */

#define color_tag           (btw->smob_tags[0])
#define rect_tag            (btw->smob_tags[1])
#define surface_tag         (btw->smob_tags[2])
#define pixel_format_tag    (btw->smob_tags[3])

#define rect_nick "SDL-Rect"
#define GCMALLOC_RECT()  GCMALLOC (sizeof (SDL_Rect), rect_nick)

#define ASSERT_COLOR(obj,n)         ASSERT_SMOB (obj, color_tag, n)
#define ASSERT_RECT(obj,n)          ASSERT_SMOB (obj, rect_tag, n)
#define ASSERT_SURFACE(obj,n)       ASSERT_SMOB (obj, surface_tag, n)
#define ASSERT_PIXEL_FORMAT(obj,n)  ASSERT_SMOB (obj, pixel_format_tag, n)

#define UNPACK_COLOR(smob)         (SMOBGET (smob, SDL_Color *))
#define UNPACK_RECT(smob)          (SMOBGET (smob, SDL_Rect *))
#define UNPACK_SURFACE(smob)       (SMOBGET (smob, SDL_Surface *))
#define UNPACK_PIXEL_FORMAT(smob)  (SMOBGET (smob, SDL_PixelFormat *))

#define NEWSMOB_OR_FALSE(tag,x)                 \
  do {                                          \
    void *__p;                                  \
                                                \
    if ((__p = (x)))                            \
      SCM_RETURN_NEWSMOB (tag, __p);            \
    else                                        \
      RETURN_FALSE;                             \
  } while (0)

#define RETURN_NEW_PF_OR_FALSE(ACTUAL,LOCALPREFIX,INTERNALP,X)  do      \
    {                                                                   \
      SDL_ ## ACTUAL *__p = X;                                          \
                                                                        \
      if (NULL != __p)                                                  \
        {                                                               \
          PF_ ## ACTUAL *pf = GCMALLOC (sizeof (PF_ ## ACTUAL),         \
                                        LOCALPREFIX ## _nick);          \
          pf->object = __p;                                             \
          pf->internalp = INTERNALP;                                    \
          SCM_RETURN_NEWSMOB (LOCALPREFIX ## _tag, pf);                 \
        }                                                               \
      else                                                              \
        RETURN_FALSE;                                                   \
    }                                                                   \
  while (0)

#define RETURN_NEW_COLOR(x)         NEWSMOB_OR_FALSE (color_tag, x)
#define RETURN_NEW_RECT(x)          NEWSMOB_OR_FALSE (rect_tag, x)
#define RETURN_NEW_SURFACE(x)       NEWSMOB_OR_FALSE (surface_tag, x)
#define RETURN_NEW_PIXEL_FORMAT(x)  NEWSMOB_OR_FALSE (pixel_format_tag, x)

/* Lots of SDL functions return 0 for true, -1 otherwise.  */
#define RETURN_TRUE_IF_0(exp) \
  return ((exp) == 0) ? BOOL_TRUE : BOOL_FALSE

/* Some SDL functions have uninteresting return values.  */
#define RETURN_UNSPECIFIED \
  return SCM_UNSPECIFIED

/* Return a converted integer.  */
#define RETURN_INT(exp) \
  return NUM_LONG (exp)

/* Return a converted unsigned integer.  */
#define RETURN_UINT(exp) \
  return NUM_ULONG (exp)

/* Return a converted boolean.  */
#define RETURN_BOOL(exp) \
  return BOOLEAN (exp)

/* Return a converted 0-terminated string.  */
#define RETURN_0STR(exp) \
  return STRING (exp)

/* Return various lists of various length.  */

#define RETURN_LIST0 \
  return SCM_EOL

#define _rv_PUSH(x) \
  _rv = CONS ((x), _rv)

#define RETURN_LIST2(a,b)                       \
  do {                                          \
    SCM _rv = SCM_EOL;                          \
    _rv_PUSH (b);                               \
    _rv_PUSH (a);                               \
    return _rv;                                 \
  } while (0)

#define RETURN_LIST3(a,b,c)                     \
  do {                                          \
    SCM _rv = SCM_EOL;                          \
    _rv_PUSH (c);                               \
    _rv_PUSH (b);                               \
    _rv_PUSH (a);                               \
    return _rv;                                 \
  } while (0)

#define RETURN_LIST4(a,b,c,d)                   \
  do {                                          \
    SCM _rv = SCM_EOL;                          \
    _rv_PUSH (d);                               \
    _rv_PUSH (c);                               \
    _rv_PUSH (b);                               \
    _rv_PUSH (a);                               \
    return _rv;                                 \
  } while (0)

#define RETURN_LIST5(a,b,c,d,e)                 \
  do {                                          \
    SCM _rv = SCM_EOL;                          \
    _rv_PUSH (e);                               \
    _rv_PUSH (d);                               \
    _rv_PUSH (c);                               \
    _rv_PUSH (b);                               \
    _rv_PUSH (a);                               \
    return _rv;                                 \
  } while (0)

#define RETURN_LIST11(a,b,c,d,e,f,g,h,i,j,k)    \
  do {                                          \
    SCM _rv = SCM_EOL;                          \
    _rv_PUSH (k);                               \
    _rv_PUSH (j);                               \
    _rv_PUSH (i);                               \
    _rv_PUSH (h);                               \
    _rv_PUSH (g);                               \
    _rv_PUSH (f);                               \
    _rv_PUSH (e);                               \
    _rv_PUSH (d);                               \
    _rv_PUSH (c);                               \
    _rv_PUSH (b);                               \
    _rv_PUSH (a);                               \
    return _rv;                                 \
  } while (0)

/* Throws.  */

#define THROW_NOT_YET_IMPLEMENTED \
  scm_misc_error (FUNC_NAME, "not yet implemented (sorry)", SCM_EOL)

/* Whole functions.  */

/* number getter and setter */

#define GSDL_NUMBER_GETTER(s_func, c_func, tag, c_type, c_field)        \
PRIMPROC (c_func, s_func, 1, 0, 0, (SCM obj),                           \
          "Get @code{" #c_field "} from the\n"                          \
          "@code{" #c_type "} @var{obj}.")                              \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, 1);                                            \
  RETURN_INT (SMOBFIELD (c_type, c_field));                             \
}

#define GSDL_NUMBER_SETTER(s_func, c_func, tag, c_type, c_field, conv)  \
PRIMPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value),                \
          "Set @code{" #c_field "} in the\n"                            \
          "@code{" #c_type "} @var{obj}\n"                              \
          "to @var{value}.")                                            \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, 1);                                            \
  ASSERT_EXACT (value, 2);                                              \
  SMOBFIELD (c_type, c_field) = conv (value);                           \
  RETURN_UNSPECIFIED;                                                   \
}

/* enum getter and setter */

#define GSDL_ENUM_GETTER(s_func, c_func, tag, c_type, c_field, etype)   \
PRIMPROC (c_func, s_func, 1, 0, 0, (SCM obj), "")                       \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, 1);                                            \
  return gsdl_long2enum (SMOBFIELD (c_type, c_field), etype);           \
}

#define GSDL_ENUM_SETTER(s_func, c_func, tag, c_type, c_field, etype)   \
PRIMPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value), "")            \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, 1);                                            \
  SMOBFIELD (c_type, c_field) = GSDL_ENUM2LONG (value, etype, 1);       \
  RETURN_UNSPECIFIED;                                                   \
}

/* flag getter and setter*/

#define GSDL_FLAG_GETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
PRIMPROC (c_func, s_func, 1, 0, 0, (SCM obj), "")                       \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, c_tag, 1);                                          \
  return gsdl_ulong2flags (SMOBFIELD (c_type, c_field), stash);         \
}

#define GSDL_FLAG_SETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
PRIMPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value), "")            \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, c_tag, 1);                                          \
  SMOBFIELD (c_type, c_field)                                           \
    = GSDL_FLAGS2ULONG (value, stash, 2);                               \
  RETURN_UNSPECIFIED;                                                   \
}

#if !defined GUILE_SDL_OPTIONAL_MODULE

/* Misc shared state.  */

extern SCM gsdl_video_flags;

/* Vector conversion funcs.  */

uint16_t *gsdl_scm_to_uint16s (SCM obj, uint16_t *data);
uint8_t *gsdl_scm_to_uint8s (SCM obj, uint8_t *data);
SCM gsdl_scm_from_uint16s (uint16_t *data, size_t n);

#endif  /* !defined GUILE_SDL_OPTIONAL_MODULE */

#endif /* !defined (__GUILE_SDL_H) */

/* guile-sdl.h ends here */
