/*
    vmem utilities.  Manipulate allocated object optimized for
    long-term persistence over raw speed.

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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

#include "defs.h"
#include <stdlib.h>

vmem_t
vmem_alloc(size_t size, int flags)
{
  vmem_t vm;
  /*
   * By default, zero the allocated thingy.
   */
  if (flags & VMFL_NOZERO) {
    vm.mem = xmalloc(size);
  } else {
    vm.mem = (char *) xcalloc(size, 1);
  }
  vm.size = size;
  return vm;
}

void
vmem_free(vmem_t *vm)
{
  if (vm->mem) {
    xfree(vm->mem);
  }
  vm->mem = NULL;
  vm->size = 0;
  return;
}

/*
 * We never shrink a vmem object on the premise that over time, object
 * will only grow for a while then reach a steady state.
 */
void
vmem_realloc(vmem_t *vm, size_t size)
{
  /*
   * Reallocate only if we must.
   */
  if (size > vm->size) {
    vm->mem = xrealloc(vm->mem, size);
    vm->size = size;
  }
  return;
}
