/* argcheck.h */

#ifndef GUILE_SDL_ARGCHECK_H
#define GUILE_SDL_ARGCHECK_H 1

#include "bool.h"

#define ASSERT_EXACT(obj,n) \
  SCM_ASSERT (NOT_FALSEP (scm_exact_p ((obj))), (obj), n, FUNC_NAME)

#define ASSERT_VECTOR(obj,n) \
  SCM_ASSERT (SCM_VECTORP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_SYMBOL(obj,n) \
  SCM_ASSERT (SCM_SYMBOLP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_STRING(obj,n) \
  SCM_ASSERT (SCM_STRINGP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_NUMBER(obj,n) \
  SCM_ASSERT (NOT_FALSEP (scm_number_p ((obj))), (obj), n, FUNC_NAME)

#define ASSERT_CHAR(obj,n) \
  SCM_ASSERT (SCM_CHARP ((obj)), (obj), n, FUNC_NAME)

#define ASSERT_LIST(obj,n) \
  SCM_ASSERT (gh_null_p (obj) || gh_pair_p (obj), (obj), n, FUNC_NAME)


#define ARGH1 1
#define ARGH2 2
#define ARGH3 3
#define ARGH4 4
#define ARGH5 5
#define ARGH6 6
#define ARGH7 7
#define ARGH8 8
#define ARGHn SCM_ARGn


#define BOUNDP(x)    (! SCM_EQ_P (x, SCM_UNDEFINED))
#define UNBOUNDP(x)    (SCM_EQ_P (x, SCM_UNDEFINED))

#define UNBOUND_MEANS_FALSE(x)  if (UNBOUNDP (x)) SET_FALSE (x)

#endif /* ! defined (GUILE_SDL_ARGCHECK_H) */

/* argcheck.h ends here */
