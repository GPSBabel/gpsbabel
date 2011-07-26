/*
    Functions to manage the format_specific_data chain

    Copyright (C) 2005 Ron Parker and Robert Lipe.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <stddef.h>
#include <stdio.h>
#include <string.h>

#include "defs.h"

format_specific_data* fs_chain_copy(format_specific_data* source)
{
  format_specific_data* result = NULL;

  format_specific_data** copy = &result;
  while (source) {
    source->copy((void**)copy, (void*)source);
    /* prevent segfaults from badly-behaved copy functions */
    (*copy)->next = NULL;
    copy = &((*copy)->next);
    source = source->next;
  }
  return result;
}

void fs_chain_destroy(format_specific_data* chain)
{
  format_specific_data* cur = chain;
  format_specific_data* next = NULL;
  while (cur) {
    next = cur->next;
    cur->destroy(cur);
    cur = next;
  }
}

format_specific_data* fs_chain_find(format_specific_data* chain, long type)
{
  format_specific_data* cur = chain;
  while (cur) {
    if (cur->type == type) {
      return cur;
    }
    cur = cur->next;
  }
  return NULL;
}

void fs_chain_add(format_specific_data** chain, format_specific_data* data)
{
  data->next = *chain;
  *chain = data;
}

