/* retval.h --- abstractions for `return FOO' */

/* Lots of SDL functions return 0 for true, -1 otherwise.  */
#define RETURN_TRUE_IF_0(exp) \
  return ((exp) == 0) ? SCM_BOOL_T : SCM_BOOL_F

/* Some SDL functions have uninteresting return values.  */
#define RETURN_UNSPECIFIED \
  return SCM_UNSPECIFIED

/* Return a converted integer.  */
#define RETURN_INT(exp) \
  return gh_long2scm (exp)

/* Return a converted boolean.  */
#define RETURN_BOOL(exp) \
  return gh_bool2scm (exp)

/* Return a converted 0-terminated string.  */
#define RETURN_0STR(exp) \
  return gh_str02scm (exp)

/* retval.h ends here */
