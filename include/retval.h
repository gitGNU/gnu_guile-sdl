/* retval.h --- abstractions for `return FOO' */

/* Lots of SDL functions return 0 for true, -1 otherwise.  */
#define RETURN_TRUE_IF_0(exp) \
  return ((exp) == 0) ? SCM_BOOL_T : SCM_BOOL_F

/* Some SDL functions have uninteresting return values.  */
#define RETURN_UNSPECIFIED \
  return SCM_UNSPECIFIED

/* Integer return values.  */
#define RETURN_INT(exp) \
  return gh_long2scm (exp)

/* retval.h ends here */
