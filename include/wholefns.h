/* wholefns.h --- various macros for declaring/defining whole functions */

#ifndef GUILE_SDL_WHOLEFNS_H
#define GUILE_SDL_WHOLEFNS_H

#include "argcheck.h"
#include "sdlsmobs.h"


/* number getter and setter */

#define GSDL_NUMBER_GETTER(s_func, c_func, tag, c_type, c_field)        \
MDEFLOCEXP (c_func, s_func, 1, 0, 0, (SCM smob),                        \
            "Get @code{" #c_field "} from @code{" #c_type "} object.")  \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, tag, ARGH1);                                       \
  return gh_long2scm (SMOBFIELD (c_type, c_field));                     \
}

#define GSDL_NUMBER_SETTER(s_func, c_func, tag, c_type, c_field)        \
MDEFLOCEXP (c_func, s_func, 2, 0, 0, (SCM smob, SCM value),             \
            "Set @code{" #c_field "} in @code{" #c_type "} object\n"    \
            "to @var{value}.")                                          \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, tag, ARGH1);                                       \
  ASSERT_EXACT (value, ARGH2);                                          \
  SMOBFIELD (c_type, c_field) = gh_scm2long (value);                    \
  return SCM_UNSPECIFIED;                                               \
}


/* enum getter and setter */

#define GSDL_ENUM_GETTER(s_func, c_func, tag, c_type, c_field, etype)   \
MDEFLOCEXP (c_func, s_func, 1, 0, 0, (SCM smob),                        \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, tag, ARGH1);                                       \
  return gsdl_long2enum (SMOBFIELD (c_type, c_field), etype);           \
}

#define GSDL_ENUM_SETTER(s_func, c_func, tag, c_type, c_field, etype)   \
MDEFLOCEXP (c_func, s_func, 2, 0, 0, (SCM smob, SCM value),             \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, tag, ARGH1);                                       \
  SMOBFIELD (c_type, c_field)                                           \
    = gsdl_enum2long (value, etype, ARGH1, FUNC_NAME);                  \
  return SCM_UNSPECIFIED;                                               \
}


/* flag getter and setter*/

#define GSDL_FLAG_GETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
MDEFLOCEXP (c_func, s_func, 1, 0, 0, (SCM smob),                        \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, c_tag, ARGH1);                                     \
  return gsdl_ulong2flags (SMOBFIELD (c_type, c_field), stash);         \
}

#define GSDL_FLAG_SETTER(s_func, c_func, c_tag, c_type, c_field, stash) \
MDEFLOCEXP (c_func, s_func, 2, 0, 0, (SCM smob, SCM value),             \
            "")                                                         \
{                                                                       \
  const char *FUNC_NAME = s_ ## c_func;                                 \
  ASSERT_SMOB (smob, c_tag, ARGH1);                                     \
  SMOBFIELD (c_type, c_field)                                           \
    = GSDL_FLAGS2ULONG (value, stash, ARGH2);                           \
  return SCM_UNSPECIFIED;                                               \
}

#endif /* ! GUILE_SDL_WHOLEFNS_H */

/* wholefns.h ends here */
