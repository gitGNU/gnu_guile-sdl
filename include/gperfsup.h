#ifndef GUILE_SDL_GPERFSUP_H
#define GUILE_SDL_GPERFSUP_H 1

#include <libguile/tags.h>
#include "sdlenums.h"

#define GPERFSUP_PRE_BOILERPLATE \
static struct val_and_name * \
in_word_set (register const char *str, register unsigned int len);

#define GPERFSUP_POST_BOILERPLATE(cvar,name)    \
flagstash_t gsdl_ ## cvar = {                   \
  name,                                         \
  &(wordlist[0]),                               \
  NULL,                                         \
  TOTAL_KEYWORDS,                               \
  (in_word_set_t *) &in_word_set,               \
  SCM_BOOL_F                                    \
};

#endif /* !defined (GUILE_SDL_GPERFSUP_H) */
