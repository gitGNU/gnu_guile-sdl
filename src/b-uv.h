/* b-uv.h --- base uvec wrangling macros
 *
 * Copyright (C) 2011, 2013 Thien-Thi Nguyen
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

#define IMPORT_SRFI4()  IMPORT_MODULE (srfi4, "(srfi srfi-4)")

#define SELECT_UVEC_PREDICATE(TT)                       \
  SELECT_MODULE_VAR (TT ## v_p, srfi4, #TT "vector?")

#define ASSERT_UVEC(tt,obj,n)                                   \
  SCM_ASSERT_TYPE (NOT_FALSEP (scm_## tt ##vector_p (obj)),     \
                   (obj), n, FUNC_NAME, #tt "vector")

/* NB: The expansion may have a trailing semicolon!  */
#if ! GI_LEVEL_1_8
#define DECL_HANDLE_MAYBE
#else
#define DECL_HANDLE_MAYBE  scm_t_array_handle handle;
#endif

#define DECLARE_UV_STRUCT(CONST,TT,TSDL)                \
struct TT ##_stuff                                      \
{                                                       \
  TSDL *bits;                                           \
  CONST TSDL *elt;                                      \
  size_t len;                                           \
  ssize_t inc;                                          \
  DECL_HANDLE_MAYBE                                     \
}

#define DEFINE_STRUCT_AND_COPY_FUNC(TT,TSDL)            \
                                                        \
DECLARE_UV_STRUCT (const, TT, TSDL);                    \
                                                        \
static inline void                                      \
copy_## TT (struct TT ##_stuff *stuff)                  \
{                                                       \
  size_t i;                                             \
  TSDL *dst = stuff->bits;                              \
  const TSDL *src = stuff->elt;                         \
                                                        \
  for (i = 0; i < stuff->len; i++, src += stuff->inc)   \
    dst[i] = *src;                                      \
}

#define ST(v,member)  v ## _stuff.member
#define VBITS(v)      ST (v, bits)
#define VLEN(v)       ST (v, len)

#define STUFF(tt,v)  struct  tt ## _stuff  v ## _stuff

#if ! GI_LEVEL_1_8

/* DWR: Abstraction violation!  */
#define GET_PARTICULARS(tt,v)  do                       \
    {                                                   \
      VLEN (v) = (size_t) SCM_CELL_WORD_2 (v);          \
      ST (v, elt) = (void *) SCM_CELL_OBJECT_3 (v);     \
      ST (v, inc) = 1;                                  \
    }                                                   \
  while (0)

#define GET_WRITABLE_PARTICULARS(tt,v)          \
  GET_PARTICULARS (tt,v)

#else  /* GI_LEVEL_1_8 */

#define GET_PARTICULARS(tt,v)                           \
  ST (v, elt) = scm_## tt ##vector_elements             \
    (v, &ST (v, handle), &ST (v, len), &ST (v, inc))

#define GET_WRITABLE_PARTICULARS(tt,v)                  \
  ST (v, elt) = scm_## tt ##vector_writable_elements    \
    (v, &ST (v, handle), &ST (v, len), &ST (v, inc))

#endif  /* GI_LEVEL_1_8 */

/* NB: The cast to ‘Uint16 *’ avoids a "discards qualifiers" warning
   from GCC.  Ideally, SDL_gfx/SDL_gfxPrimitives.h would declare the
   inputs ‘const’, in which case no cast would be necessary.  */
#define HOWDY(tt,tsdl,v)  do                                    \
    {                                                           \
      GET_PARTICULARS (tt, v);                                  \
      if (1 == ST (v, inc))                                     \
        VBITS (v) = (tsdl *) ST (v, elt);                       \
      else                                                      \
        {                                                       \
          VBITS (v) = alloca (sizeof (tsdl) * VLEN (v));        \
          copy_## tt (&v ## _stuff);                            \
        }                                                       \
    }                                                           \
  while (0)

#if ! GI_LEVEL_1_8
#define LATER(v)
#else
#define LATER(v)  scm_array_handle_release (&ST (v, handle))
#endif

/* b-uv.h ends here */
