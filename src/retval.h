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


/* Return various lists of various length.  */

#define RETURN_LIST0 \
  return SCM_EOL

#define _rv_PUSH(x) \
  _rv = gh_cons ((x), _rv)

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

#endif /* ! defined (GUILE_SDL_RETVAL_H) */

/* retval.h ends here */
