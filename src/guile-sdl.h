/* guile-sdl.h */

#ifndef __GUILE_SDL_H
#define __GUILE_SDL_H

/*
 * Copyright (C) 2003, 2004, 2005, 2007, 2008,
 *   2009, 2011, 2013 Thien-Thi Nguyen
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


#define GCC_VERSION  (__GNUC__ * 10000           \
                      + __GNUC_MINOR__ * 100     \
                      + __GNUC_PATCHLEVEL__)
#if GCC_VERSION >= 40100
#define DSOPRIVATE  __attribute__ ((__visibility__ ("hidden")))
#else
#define DSOPRIVATE
#endif

#define GBO  DSOPRIVATE extern


/* Guile.  */

#include <libguile.h>
#include "snuggle/level.h"
#include "snuggle/humdrum.h"

#define GI_LEVEL_NOT_YET_1_8  (! GI_LEVEL_1_8)

#define PERMANENT  scm_permanent_object

/* ZRSN X ≡ ZONKABLE RSN (after #include "snuggle/humdrum.h" serial X) */

#undef VECREF                           /* ZRSN 2 */
#if GI_LEVEL_NOT_YET_1_8
#include <guile/gh.h>
#define LIST2              SCM_LIST2
#define LIST3              SCM_LIST3
#define LIST4              SCM_LIST4
#define LIST5              SCM_LIST5
#define SYMBOLP(obj)      (gh_symbol_p (obj))
#define INTEGERP(obj)      NOT_FALSEP (scm_integer_p (obj))
#define VECTORP            gh_vector_p
#define C_CHAR             gh_scm2char
#define C_LONG             gh_scm2long
#define C_DOUBLE           gh_scm2double
#define VECREF(vec,idx)    (SCM_VELTS (vec)[idx]) /* ZRSN 2 */
#define GC_PROTECT         scm_protect_object
#define GC_UNPROTECT       scm_unprotect_object
#define DEFINE_PUBLIC      gh_define
#define MODULE_LOOKUP      gh_module_lookup
#else
#define LIST2              scm_list_2
#define LIST3              scm_list_3
#define LIST4              scm_list_4
#define LIST5              scm_list_5
#define SYMBOLP(obj)      (scm_is_symbol (obj))
#define INTEGERP           scm_is_integer
#define VECTORP            scm_is_vector
#define C_CHAR(c)          C_INT (scm_char_to_integer (c))
#define C_LONG             scm_to_long
#define C_DOUBLE           scm_to_double
#define VECREF             scm_c_vector_ref /* ZRSN 2 */
#define GC_PROTECT         scm_gc_protect_object
#define GC_UNPROTECT       scm_gc_unprotect_object
#define DEFINE_PUBLIC(name,value)  do           \
    {                                           \
      scm_c_define (name, value);               \
      scm_c_export (name, NULL);                \
    }                                           \
  while (0)
#define MODULE_LOOKUP(module, name)                     \
  scm_variable_ref (scm_c_module_lookup (module, name))
#endif


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

#define ASSERT_TYPE(condition,object,position,expected) \
  SCM_ASSERT_TYPE (condition, object, position, FUNC_NAME, expected)

#define ASSERT_INTEGER(obj,n) \
  ASSERT_TYPE (INTEGERP (obj), obj, n, "integer")

#define ASSERT_INTEGER_COPY(name,pos,change)  do        \
    {                                                   \
      ASSERT_INTEGER (name, pos);                       \
      c ## name = C_ ## change (name);                  \
    }                                                   \
  while (0)

#define ASSERT_INT_COPY(name,pos)    ASSERT_INTEGER_COPY (name, pos, INT)
#define ASSERT_LONG_COPY(name,pos)   ASSERT_INTEGER_COPY (name, pos, LONG)
#define ASSERT_ULONG_COPY(name,pos)  ASSERT_INTEGER_COPY (name, pos, ULONG)

#define ASSERT_VECTOR(obj,n) \
  SCM_VALIDATE_VECTOR (n, obj)

#define ASSERT_SYMBOL(obj,n) \
  SCM_VALIDATE_SYMBOL (n, obj)

#define ASSERT_STRING(obj,n) \
  SCM_VALIDATE_STRING (n, obj)

#ifndef SCM_VALIDATE_NUMBER
#define SCM_VALIDATE_NUMBER(n,obj) \
  SCM_VALIDATE_TYPE (n, obj, SCM_NUMBERP, "number")
#endif

#define ASSERT_NUMBER(obj,n) \
  SCM_VALIDATE_NUMBER (n, obj)

#define ASSERT_CHAR(obj,n) \
  SCM_VALIDATE_CHAR (n, obj)

#define ASSERT_LIST(obj,n) \
  SCM_VALIDATE_LIST (n, obj)

#define BOUNDP(x)    (! SCM_EQ_P (x, SCM_UNDEFINED))
#define UNBOUNDP(x)    (SCM_EQ_P (x, SCM_UNDEFINED))

#define UNBOUND_MEANS_FALSE(x)  if (UNBOUNDP (x)) SET_FALSE (x)

#define IF_BOUND_ASSERT_INTEGER_COPY(name,pos,change)   \
  if (BOUNDP (name) && NOT_FALSEP (name))               \
    ASSERT_INTEGER_COPY (name, pos, change)

#define IF_BOUND_ASSERT_LONG_COPY(name,pos) \
  IF_BOUND_ASSERT_INTEGER_COPY (name, pos, LONG)

#define IF_BOUND_ASSERT_ULONG_COPY(name,pos) \
  IF_BOUND_ASSERT_INTEGER_COPY (name, pos, ULONG)

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

#define VAL_AND_AKA(val,rozt)  { .value = val, .aka = { rozt } }

#define enum_nick "SDL-enum"

typedef SCM (define_enum_t) (const char *name, size_t count,
                             valaka_t *backing);

#define DEFINE_ENUM(name,backing)               \
  btw->define_enum                              \
  (name,                                        \
   sizeof (backing) / sizeof (valaka_t),        \
   backing)

typedef long (enum2long_t) (SCM s_enum, SCM enum_type,
                            int pos, const char *func);

typedef SCM (long2enum_t) (long value, SCM enum_type);

#define GSDL_ENUM2LONG(enums,table,pos) \
  btw->enum2long ((enums), (table), (pos), FUNC_NAME)

/* flags (constants typically used as a logical or'ed group) */

#define flagstash_nick "SDL-flagstash"

typedef unsigned long (flags2ulong_t) (SCM flags, SCM table,
                                       int pos, const char *func);

typedef SCM (ulong2flags_t) (unsigned long value, SCM stash);

#define GSDL_FLAGS2ULONG(flags,table,pos) \
  btw->flags2ulong ((flags), (table), (pos), FUNC_NAME)

typedef struct flagstash {
  const char const *name;
  const size_t total;
  const unsigned long const *val;
  aka_t *aka;
  SCM ht;
} flagstash_t;

typedef SCM (make_flagstash_t) (flagstash_t *stash);

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
#define ASSERT_SMOB(smob, lpre, which)                          \
  ASSERT_TYPE (SCM_SMOB_PREDICATE (lpre ## _tag, smob),         \
                   (smob), (which), lpre ## _nick)

#define SMOBGET(smob,c_type)       ((c_type) SCM_SMOB_DATA (smob))
#define SMOBSET(smob,val)          (SCM_SET_SMOB_DATA (smob, val))

#define SMOBF(obj,ctype,field)     SMOBGET (obj, ctype)->field
#define SMOBFIELD(ctype,field)     SMOBF (obj, ctype, field)

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

  SCM video_flags;
  /* Flags used in various sdl{surface,video}.c funcs.  */

  make_flagstash_t *make_flagstash;
  flags2ulong_t *flags2ulong;
  ulong2flags_t *ulong2flags;
  define_enum_t *define_enum;
  enum2long_t *enum2long;
  long2enum_t *long2enum;
};

#if 1 == GUILE_SDL_OPTIONAL_MODULE
static
#else
#define btw  gsdl_btw
GBO
#endif
struct obtw *btw;

/* Return values.  */

#define color_tag           (btw->smob_tags[0])
#define rect_tag            (btw->smob_tags[1])
#define surface_tag         (btw->smob_tags[2])
#define pixel_format_tag    (btw->smob_tags[3])

#define color_nick "SDL-Color"

#define rect_nick "SDL-Rect"
#define GCMALLOC_RECT()  GCMALLOC (sizeof (SDL_Rect), rect_nick)

#define surface_nick "SDL-Surface"

#define pixel_format_nick "SDL-Pixel-Format"

#define ASSERT_COLOR(obj,n)         ASSERT_SMOB (obj, color, n)
#define ASSERT_RECT(obj,n)          ASSERT_SMOB (obj, rect, n)
#define ASSERT_SURFACE(obj,n)       ASSERT_SMOB (obj, surface, n)
#define ASSERT_PIXEL_FORMAT(obj,n)  ASSERT_SMOB (obj, pixel_format, n)

DECLARE_PF (Surface);
#define UNPACK_PF_SURFACE(smob)    (SMOBGET (smob, PF_Surface *))

#define UNPACK_COLOR(smob)         (SMOBGET (smob, SDL_Color *))
#define UNPACK_RECT(smob)          (SMOBGET (smob, SDL_Rect *))
#define UNPACK_SURFACE(smob)       (UNPACK_PF_SURFACE (smob)->object)
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

#define RETURN_PF_SURFACE(x,INTERNALP)                          \
  RETURN_NEW_PF_OR_FALSE (Surface, surface, INTERNALP, x)

#define RETURN_NEW_COLOR(x)         NEWSMOB_OR_FALSE (color_tag, x)
#define RETURN_NEW_RECT(x)          NEWSMOB_OR_FALSE (rect_tag, x)
#define RETURN_NEW_SURFACE(x)       RETURN_PF_SURFACE (x, 0)
#define RETURN_INT_SURFACE(x)       RETURN_PF_SURFACE (x, 1)
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

/* Like ‘RETURN_0STR’, but if ‘exp’ evaluates to ‘NULL’,
   return ‘#f’ instead.  */
#define RETURN_0STR_OR_FALSE(exp)  do           \
      {                                         \
        const char *__p = (exp);                \
                                                \
        return __p                              \
          ? STRING (__p)                        \
          : BOOL_FALSE;                         \
      }                                         \
    while (0)

/* Whole functions.  */

/* number getter and setter */

#define GSDL_NUMBER_GETTER(s_func, c_func, lpre, actual, c_field)       \
PRIMPROC (c_func, s_func, 1, 0, 0, (SCM lpre),                          \
          "Get @code{" #c_field "} from @var{" #lpre "}.")              \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  RETURN_INT (SMOBF (lpre, SDL_ ##actual *, c_field));                  \
}

#define GSDL_PF_NUMBER_GETTER(sname, cname, lpre, actual, field)        \
PRIMPROC (cname, sname, 1, 0, 0, (SCM lpre),                            \
          "Get @code{" #field "} from @var{" #lpre "}.")                \
{                                                                       \
  const char *FUNC_NAME = s_ ## cname;                                  \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  RETURN_INT (SMOBF (lpre, PF_ ##actual *, object->field));             \
}

#define GSDL_NUMBER_SETTER(s_func, c_func, lpre, actual, c_field, conv) \
PRIMPROC (c_func, s_func, 2, 0, 0, (SCM lpre, SCM value),               \
          "Set @code{" #c_field "} in @var{" #lpre "}\n"                \
          "to @var{value}.")                                            \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  ASSERT_INTEGER (value, 2);                                            \
  SMOBF (lpre, SDL_ ##actual *, c_field) = conv (value);                \
  RETURN_UNSPECIFIED;                                                   \
}

/* enum getter and setter */

#define GSDL_ENUM_GETTER(sname,cname,lpre,ctype,field,etype)    \
PRIMPROC (cname, sname, 1, 0, 0, (SCM lpre),                    \
          "Return the symbolic @code{" #field "}"               \
          " from @var{" #lpre "}.")                             \
{                                                               \
  const char *FUNC_NAME = s_ ## cname;                          \
  ASSERT_SMOB (lpre, lpre, 1);                                  \
  return btw->long2enum (SMOBF (lpre, ctype, field), etype);    \
}

#define GSDL_ENUM_SETTER(sname,cname,lpre,ctype,field,etype)            \
PRIMPROC (cname, sname, 2, 0, 0, (SCM lpre, SCM value),                 \
          "Set @code{" #field "} in @var{" #lpre "}"                    \
          " to @var{value}, a symbol or integer.")                      \
{                                                                       \
  const char *FUNC_NAME = s_ ## cname;                                  \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  SMOBF (lpre, ctype, field) = GSDL_ENUM2LONG (value, etype, 1);        \
  RETURN_UNSPECIFIED;                                                   \
}

/* flag getter and setter*/

#define GSDL_FLAG_GETTER(sname,cname,lpre,ctype,field,fname,stash)      \
PRIMPROC (cname, sname, 1, 0, 0, (SCM lpre),                            \
          "Return @code{" #fname "} from @var{" #lpre "}"               \
          " as a (possibly empty) list of symbols.")                    \
{                                                                       \
  const char *FUNC_NAME = s_ ## cname;                                  \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  return btw->ulong2flags (SMOBF (lpre, ctype, field), stash);          \
}

#define GSDL_FLAG_SETTER(sname,cname,lpre,ctype,field,fname,stash)      \
PRIMPROC (cname, sname, 2, 0, 0, (SCM lpre, SCM value),                 \
          "Set @code{" #fname "} in @var{" #lpre "}"                    \
          " to @var{value}, a (possibly empty) list of symbols.")       \
{                                                                       \
  const char *FUNC_NAME = s_ ## cname;                                  \
  ASSERT_SMOB (lpre, lpre, 1);                                          \
  SMOBF (lpre, ctype, field) = GSDL_FLAGS2ULONG (value, stash, 2);      \
  RETURN_UNSPECIFIED;                                                   \
}

#endif /* !defined (__GUILE_SDL_H) */

/* guile-sdl.h ends here */
