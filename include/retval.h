/* retval.h --- abstractions for `return FOO' */

#ifndef GUILE_SDL_RETVAL_H
#define GUILE_SDL_RETVAL_H

#include "bool.h"

/* Lots of SDL functions return 0 for true, -1 otherwise.  */
#define RETURN_TRUE_IF_0(exp) \
  return ((exp) == 0) ? BOOL_TRUE : BOOL_FALSE

/* Some SDL functions have uninteresting return values.  */
#define RETURN_UNSPECIFIED \
  return SCM_UNSPECIFIED

/* Return a converted integer.  */
#define RETURN_INT(exp) \
  return gh_long2scm (exp)

/* Return a converted unsigned integer.  */
#define RETURN_UINT(exp) \
  return gh_ulong2scm (exp)

/* Return a converted boolean.  */
#define RETURN_BOOL(exp) \
  return gh_bool2scm (exp)

/* Return a converted 0-terminated string.  */
#define RETURN_0STR(exp) \
  return gh_str02scm (exp)

#endif /* ! defined (GUILE_SDL_RETVAL_H) */

/* retval.h ends here */
