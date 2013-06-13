/* b-values.h --- smuggle ‘scm_values’ from Scheme
 *
 * Copyright (C) 2013 Thien-Thi Nguyen
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

#if !HAVE_DECL_SCM_VALUES

IMPORT_MODULE (values_gu, "(guile-user)");
SELECT_MODULE_VAR (values_vvv, values_gu, "values");
#define scm_values(ls)  gh_apply (values_vvv, (ls))

#endif  /* !HAVE_DECL_SCM_VALUES */

#define RETURN_VALUES2(a,b)                     \
  do                                            \
    {                                           \
      SCM _rv = LIST2 ((a), (b));               \
      return scm_values (_rv);                  \
    }                                           \
  while (0)

#define RETURN_VALUES3(a,b,c)                   \
  do                                            \
    {                                           \
      SCM _rv = LIST3 ((a), (b), (c));          \
      return scm_values (_rv);                  \
    }                                           \
  while (0)

#define RETURN_VALUES4(a,b,c,d)                 \
  do                                            \
    {                                           \
      SCM _rv = LIST4 ((a), (b), (c), (d));     \
      return scm_values (_rv);                  \
    }                                           \
  while (0)

#define RETURN_VALUES5(a,b,c,d,e)                       \
  do                                                    \
    {                                                   \
      SCM _rv = LIST5 ((a), (b), (c), (d), (e));        \
      return scm_values (_rv);                          \
    }                                                   \
  while (0)

/* b-values.h ends here */
