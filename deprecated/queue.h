/*
    Generic queueing utilities.

    Copyright (C) 2002-2005 Robert Lipe, robertlipe@usa.net

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
#ifndef QUEUE_H_INCLUDED_
#define QUEUE_H_INCLUDED_

#include <iterator>

typedef struct queue {
  struct queue* next;
  struct queue* prev;
} queue;

void enqueue(queue* new_el, queue* old);
queue* dequeue(queue* element);

//void sortqueue(queue* qh, int (*cmp)(const queue*, const queue*)); /* template */

#define QUEUE_INIT(head) (head)->next = ((head)->prev = head)
#define QUEUE_FIRST(head) ((head)->next)
#define QUEUE_NEXT(element) ((element)->next)
#define QUEUE_LAST(head) ((head)->prev)
#define QUEUE_EMPTY(head) ((head)->next == (head))
#define QUEUE_MOVE(newhead,oldhead) \
        if ( (oldhead)->next == (oldhead) ) {\
		(newhead)->next = (newhead)->prev = (newhead); \
	} \
	else { \
		(newhead)->next = (oldhead)->next; \
		(newhead)->prev = (oldhead)->prev; \
		(newhead)->next->prev = (newhead); \
		(newhead)->prev->next = (newhead); \
	} \
	(oldhead)->next = (oldhead)->prev = (oldhead)

#define ENQUEUE_TAIL(listhead, element) \
		enqueue(element, (listhead)->prev)
#define ENQUEUE_HEAD(listhead, element) \
		enqueue(element, listhead)
#define ENQUEUE_AFTER(predecessor, element) \
		enqueue(element, predecessor)

#define QUEUE_FOR_EACH(listhead, element, tmp) \
	for ((element) = QUEUE_FIRST(listhead); \
		(tmp) = QUEUE_NEXT(element), \
		(element) != (listhead); \
		(element) = (tmp))

// FIXME: g++ 7.3.0, -O2, with T=route_head yields warnings.
//        implementing QueueList as a template was meant to 
//        i) avoid reinterpret_casts all over the code as with QUEUE_FOR_EACH
//        ii) allow use of range based for loops.
//        If this isn't fixed then QueueList doesn't need to be a template, T == queue.
// In file included from defs.h:27:0,
//                  from bend.cc:23:
// queue.h: In instantiation of ‘const T*& QueueList<T>::ConstIterator::operator*() [with T = route_head; QueueList<T>::ConstIterator::reference = const route_head*&]’:
// bend.cc:162:24:   required from here
// queue.h:143:14: warning: dereferencing type-punned pointer will break strict-aliasing rules [-Wstrict-aliasing]
//        return reinterpret_cast<reference>(ptr_);
//               ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
template <typename T>
class QueueList
{
public:

  QueueList(queue* head, int* ct) : head_{head}, ct_{ct} {}

  class Iterator
  {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T*;
    using difference_type = std::ptrdiff_t;
    using pointer = T** ;
    using reference = T* &;

    Iterator() = default;
    explicit Iterator(queue* p) : ptr_{p} {}

    reference operator*()
    {
      return reinterpret_cast<reference>(ptr_);
    }
    pointer operator->()
    {
      return reinterpret_cast<pointer>(&ptr_);
    }
    Iterator& operator++()   // pre-increment
    {
      ptr_ = ptr_->next;
      return *this;
    }
    Iterator operator++(int)   // post-increment
    {
      Iterator tmp = *this;
      ++*this;
      return tmp;
    }
    Iterator& operator--()   // pre-decrement
    {
      ptr_ = ptr_->prev;
      return *this;
    }
    Iterator operator--(int)   // post-decrement
    {
      Iterator tmp = *this;
      --*this;
      return tmp;
    }
    bool operator==(const Iterator& other) const
    {
      return ptr_ == other.ptr_;
    }
    bool operator!=(const Iterator& other) const
    {
      return ptr_ != other.ptr_;
    }

  private:
    queue* ptr_{nullptr};
  };
  //friend class Iterator;

  class ConstIterator
  {
  public:
    using iterator_category = std::bidirectional_iterator_tag;
    using value_type = T*;
    using difference_type = std::ptrdiff_t;
    using pointer = const T** ;
    using reference = const T* &;

    ConstIterator() = default;
    explicit ConstIterator(const queue* p) : ptr_{p} {}

    reference operator*()
    {
      return reinterpret_cast<reference>(ptr_);
    }
    pointer operator->()
    {
      return reinterpret_cast<pointer>(&ptr_);
    }
    ConstIterator& operator++()   // pre-increment
    {
      ptr_ = ptr_->next;
      return *this;
    }
    ConstIterator operator++(int)   // post-increment
    {
      ConstIterator tmp = *this;
      ++*this;
      return tmp;
    }
    ConstIterator& operator--()   // pre-decrement
    {
      ptr_ = ptr_->prev;
      return *this;
    }
    ConstIterator operator--(int)   // post-decrement
    {
      ConstIterator tmp = *this;
      --*this;
      return tmp;
    }
    bool operator==(const ConstIterator& other) const
    {
      return ptr_ == other.ptr_;
    }
    bool operator!=(const ConstIterator& other) const
    {
      return ptr_ != other.ptr_;
    }

  private:
    const queue* ptr_{nullptr};
  };
  //friend class ConstIterator;

  Iterator begin()
  {
    return Iterator(head_->next);
  }
  ConstIterator begin() const
  {
    return ConstIterator(head_->next);
  }
  Iterator end()
  {
    return Iterator(head_);
  }
  ConstIterator end() const
  {
    return ConstIterator(head_);
  }
  ConstIterator cbegin()
  {
    return ConstIterator(head_->next);
  }
  ConstIterator cend()
  {
    return ConstIterator(head_);
  }

  bool empty() const
  {
    return begin() == end();
  }

  T& front()
  {
    return reinterpret_cast<T&>(**begin());
  }

  const T& front() const
  {
    return reinterpret_cast<const T&>(**begin());
  }

  T& back()
  {
    auto tmp = end();
    --tmp;
    return reinterpret_cast<T&>(**tmp);
  }

  const T& back() const
  {
    auto tmp = end();
    --tmp;
    return reinterpret_cast<const T&>(**tmp);
  }

private:
  queue* head_;
  int* ct_;
};


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


template <typename T>
void
sortqueue(queue* qh, T cmp)
{

  queue* e;

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

  queue* list = QUEUE_FIRST(qh);
  dequeue(qh);

  int insize = 1;

  while (true) {
    queue* p = list;
    queue* oldhead = list;	 /* only used for circular linkage */
    list = nullptr;
    queue* tail = nullptr;

    int nmerges = 0;  /* count number of merges we do in this pass */

    while (p) {
      nmerges++;	/* there exists a merge to be done */
      /* step `insize' places along from p */
      queue* q = p;
      int psize = 0;
      for (int i = 0; i < insize; i++) {
        psize++;
        q = (q->next == oldhead ? nullptr : q->next);
        if (!q) {
          break;
        }
      }

      /* if q hasn't fallen off end, we have
       * two lists to merge */
      int qsize = insize;

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
            q = nullptr;
          }
        } else if (qsize == 0 || !q) {
          /* q is empty; e must come from p. */
          e = p;
          p = p->next;
          psize--;
          if (p == oldhead) {
            p = nullptr;
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
            p = nullptr;
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
            q = nullptr;
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
#endif  // QUEUE_H_INCLUDED_
