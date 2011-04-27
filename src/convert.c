/* convert.c
 *
 * Copyright (C) 2011 Thien-Thi Nguyen
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

#include "guile-sdl.h"

int16_t *
gsdl_scm_to_int16s (SCM obj, int16_t *data)
{
#ifndef HAVE_GUILE_GH_H
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const int16_t *elt;

  obj = scm_any_to_s16vector (obj);
  elt = scm_s16vector_elements (obj, &handle, &len, &inc);
  for (i = 0; i < len;  i++, elt += inc)
    data[i] = *elt;
  scm_array_handle_release (&handle);
#else  /* HAVE_GUILE_GH_H */
  gh_scm2shorts (obj, data);
#endif  /* HAVE_GUILE_GH_H */
  return data;
}

uint16_t *
gsdl_scm_to_uint16s (SCM obj, uint16_t *data)
{
#ifndef HAVE_GUILE_GH_H
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const uint16_t *elt;

  obj = scm_any_to_u16vector (obj);
  elt = scm_u16vector_elements (obj, &handle, &len, &inc);
  for (i = 0; i < len;  i++, elt += inc)
    data[i] = *elt;
  scm_array_handle_release (&handle);
#else  /* HAVE_GUILE_GH_H */
  gh_scm2shorts (obj, (short *) data);
#endif  /* HAVE_GUILE_GH_H */
  return data;
}

uint8_t *
gsdl_scm_to_uint8s (SCM obj, uint8_t *data)
{
#ifndef HAVE_GUILE_GH_H
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  const uint8_t *elt;

  obj = scm_any_to_u8vector (obj);
  elt = scm_u8vector_elements (obj, &handle, &len, &inc);
  for (i = 0; i < len;  i++, elt += inc)
    data[i] = *elt;
  scm_array_handle_release (&handle);
#else  /* HAVE_GUILE_GH_H */
  gh_scm2chars (obj, (char *) data);
#endif  /* HAVE_GUILE_GH_H */
  return data;
}

SCM
gsdl_scm_from_uint16s (uint16_t *data, size_t n)
{
  SCM obj;
#ifndef HAVE_GUILE_GH_H
  scm_t_array_handle handle;
  size_t i, len;
  ssize_t inc;
  uint16_t *elt;

  obj = scm_make_u16vector (scm_from_size_t (n), SCM_UNDEFINED);
  elt = scm_u16vector_writable_elements (obj, &handle, &len, &inc);
  for (i = 0; i < len; i++, elt += inc)
    *elt = data[i];
  scm_array_handle_release (&handle);
#else  /* HAVE_GUILE_GH_H */
  obj = gh_shorts2svect ((short *) data, n);
#endif  /* HAVE_GUILE_GH_H */
  return obj;
}

/* convert.c ends here */
