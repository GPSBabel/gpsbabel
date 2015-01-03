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
#include <stddef.h>

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

/*
 * The following sorting code was derived from linked-list mergesort
 * sample code by Simon Tatham, code obtained from:
 *  http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
 * Modified for use with gpsbabel's queues by Paul Fox, October 2006.
 *
 * Original description and copyright messages follow...
 */

/*
 * Demonstration code for sorting a linked list.
 *
 * The algorithm used is Mergesort, because that works really well
 * on linked lists, without requiring the O(N) extra space it needs
 * when you do it on arrays.
 *
 * ...
 */

/*
 * This file is copyright 2001 Simon Tatham.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL SIMON TATHAM BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */


void
sortqueue(queue* qh, int (*cmp)(const queue*, const queue*))
{

  queue* p, *q, *e, *tail, *oldhead, *list;
  int insize, nmerges, psize, qsize, i;

  /*
   * Special case: if `list' is empty, we're done.
   */
  if (QUEUE_EMPTY(qh)) {
    return;
  }

  /*
   * The algorithm doesn't really want the extra list head
   * element.	 So remove the list head for now.  Put it back later.
   */

  list = QUEUE_FIRST(qh);
  dequeue(qh);

  insize = 1;

  while (1) {
    p = list;
    oldhead = list;	 /* only used for circular linkage */
    list = NULL;
    tail = NULL;

    nmerges = 0;  /* count number of merges we do in this pass */

    while (p) {
      nmerges++;	/* there exists a merge to be done */
      /* step `insize' places along from p */
      q = p;
      psize = 0;
      for (i = 0; i < insize; i++) {
        psize++;
        q = (q->next == oldhead ? NULL : q->next);
        if (!q) {
          break;
        }
      }

      /* if q hasn't fallen off end, we have
       * two lists to merge */
      qsize = insize;

      /* now we have two lists; merge them */
      while (psize > 0 || (qsize > 0 && q)) {

        /* decide whether next element of
         * merge comes from p or q
         */
        if (psize == 0) {
          /* p is empty; e must come from q. */
          e = q;
          q = q->next;
          qsize--;
          if (q == oldhead) {
            q = NULL;
          }
        } else if (qsize == 0 || !q) {
          /* q is empty; e must come from p. */
          e = p;
          p = p->next;
          psize--;
          if (p == oldhead) {
            p = NULL;
          }
        } else if (cmp(p,q) <= 0) {
          /* First element of p is
           * lower (or same); e must
           * come from p.
           */
          e = p;
          p = p->next;
          psize--;
          if (p == oldhead) {
            p = NULL;
          }
        } else {
          /* First element of q is
           * lower; e must come from
           * q.
           */
          e = q;
          q = q->next;
          qsize--;
          if (q == oldhead) {
            q = NULL;
          }
        }

        /* add the next element to the merged list */
        if (tail) {
          tail->next = e;
        } else {
          list = e;
        }

        /* Maintain reverse pointers in a
         * doubly linked list.  */
        e->prev = tail;

        tail = e;
      }

      /* now p has stepped `insize' places
       * along, and q has too */
      p = q;
    }
    tail->next = list;
    list->prev = tail;

    /* If we have done only one merge, we're finished.
     * Allow for nmerges==0, the empty list case.
     */
    if (nmerges <= 1) {

      /* Put the list head back at the start of the list */
      ENQUEUE_TAIL(list, qh);
      return;

    }

    /* Otherwise repeat, merging lists twice the size */
    insize *= 2;
  }
}
