/*
    Generic queue utilities.

    Copyright (C) 2002 Robert Lipe, robertlipe+source@gpsbabel.org

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

#include "queue.h"
#include <cstddef>

void
enqueue(queue* new_el, queue* old)
{
  new_el->next = old->next;
  new_el->prev = old;
  old->next->prev = new_el;
  old->next = new_el;
}

queue*
dequeue(queue* element)
{
  queue* prev = element->prev;
  queue* next = element->next;

  next->prev = prev;
  prev->next = next;

  QUEUE_INIT(element);
  return element;
}
