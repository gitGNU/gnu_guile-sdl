/* wholefns.h --- various macros for declaring/defining whole functions */

#ifndef GUILE_SDL_WHOLEFNS_H
#define GUILE_SDL_WHOLEFNS_H

#include "argcheck.h"
#include "sdlsmobs.h"
#include "retval.h"


/* number getter and setter */

#define GSDL_NUMBER_GETTER(s_func, c_func, tag, c_type, c_field)        \
GH_DEFPROC (c_func, s_func, 1, 0, 0, (SCM obj),                         \
            "Get @code{" #c_field "} from @code{" #c_type "} object.")  \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, ARGH1);                                        \
  RETURN_INT (SMOBFIELD (c_type, c_field));                             \
}

#define GSDL_NUMBER_SETTER(s_func, c_func, tag, c_type, c_field)        \
GH_DEFPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value),              \
            "Set @code{" #c_field "} in @code{" #c_type "} object\n"    \
            "to @var{value}.")                                          \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, ARGH1);                                        \
  ASSERT_EXACT (value, ARGH2);                                          \
  SMOBFIELD (c_type, c_field) = gh_scm2long (value);                    \
  RETURN_UNSPECIFIED;                                                   \
}


/* enum getter and setter */

#define GSDL_ENUM_GETTER(s_func, c_func, tag, c_type, c_field, etype)   \
GH_DEFPROC (c_func, s_func, 1, 0, 0, (SCM obj),                         \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, ARGH1);                                        \
  return gsdl_long2enum (SMOBFIELD (c_type, c_field), etype);           \
}

#define GSDL_ENUM_SETTER(s_func, c_func, tag, c_type, c_field, etype)   \
GH_DEFPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value),              \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, tag, ARGH1);                                        \
  SMOBFIELD (c_type, c_field) = GSDL_ENUM2LONG (value, etype, ARGH1);   \
  RETURN_UNSPECIFIED;                                                   \
}


/* flag getter and setter*/

#define GSDL_FLAG_GETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
GH_DEFPROC (c_func, s_func, 1, 0, 0, (SCM obj),                         \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, c_tag, ARGH1);                                      \
  return gsdl_ulong2flags (SMOBFIELD (c_type, c_field), stash);         \
}

#define GSDL_FLAG_SETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
GH_DEFPROC (c_func, s_func, 2, 0, 0, (SCM obj, SCM value),              \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (obj, c_tag, ARGH1);                                      \
  SMOBFIELD (c_type, c_field)                                           \
    = GSDL_FLAGS2ULONG (value, stash, ARGH2);                           \
  RETURN_UNSPECIFIED;                                                   \
}

#endif /* ! GUILE_SDL_WHOLEFNS_H */

/* wholefns.h ends here */
