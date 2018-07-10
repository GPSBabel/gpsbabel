/*
    Stack filter

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

#include "defs.h"
#include "filterdefs.h"
#include "stackfilter.h"
#include <cstdlib>

#if FILTERS_ENABLED

#define MYNAME "Stack filter"

void StackFilter::process()
{
  struct stack_elt* tmp_elt = nullptr;
  queue* elem = nullptr;
  queue* tmp = nullptr;
  queue tmp_queue;

  if (opt_push) {
    tmp_elt = (struct stack_elt*)xmalloc(sizeof(struct stack_elt));

    QUEUE_MOVE(&(tmp_elt->waypts), &waypt_head);
    tmp_elt->waypt_ct = waypt_count();
    set_waypt_count(0);
    tmp_elt->next = stack;
    stack = tmp_elt;
    if (opt_copy) {
      QUEUE_FOR_EACH(&(stack->waypts), elem, tmp) {
        waypt_add(new Waypoint(*(Waypoint*)elem));
      }
    }

    tmp = nullptr;
    route_backup(&(tmp_elt->route_count), &tmp);
    QUEUE_MOVE(&(tmp_elt->routes), tmp);
    xfree(tmp);
    if (!opt_copy) {
      route_flush_all_routes();
    }

    tmp = nullptr;
    track_backup(&(tmp_elt->track_count), &tmp);
    QUEUE_MOVE(&(tmp_elt->tracks), tmp);
    xfree(tmp);
    if (!opt_copy) {
      route_flush_all_tracks();
    }

  } else if (opt_pop) {
    tmp_elt = stack;
    if (!tmp_elt) {
      fatal(MYNAME ": stack empty\n");
    }
    if (opt_append) {
      QUEUE_FOR_EACH(&(stack->waypts), elem, tmp) {
        waypt_add((Waypoint*)elem);
      }
      route_append(&(stack->routes));
      route_flush(&(stack->routes));
      track_append(&(stack->tracks));
      route_flush(&(stack->tracks));
    } else if (opt_discard) {
      waypt_flush(&(stack->waypts));
      route_flush(&(stack->routes));
      route_flush(&(stack->tracks));
    } else {
      waypt_flush(&waypt_head);
      QUEUE_MOVE(&(waypt_head), &(stack->waypts));
      set_waypt_count(stack->waypt_ct);

      route_restore(&(stack->routes));
      track_restore(&(stack->tracks));
    }

    stack = tmp_elt->next;
    xfree(tmp_elt);
  } else if (opt_swap) {
    tmp_elt = stack;
    while (swapdepth > 1) {
      if (!tmp_elt->next) {
        fatal(MYNAME ": swap with nonexistent element\n");
      }
      tmp_elt = tmp_elt->next;
      swapdepth--;
    }
    QUEUE_MOVE(&tmp_queue, &(tmp_elt->waypts));
    QUEUE_MOVE(&(tmp_elt->waypts), &waypt_head);
    QUEUE_MOVE(&waypt_head, &tmp_queue);

    QUEUE_MOVE(&tmp_queue, &(tmp_elt->routes));
    tmp = nullptr;
    route_backup(&(tmp_elt->route_count), &tmp);
    QUEUE_MOVE(&(tmp_elt->routes), tmp);
    xfree(tmp);
    route_restore(&tmp_queue);

    QUEUE_MOVE(&tmp_queue, &(tmp_elt->tracks));
    tmp = nullptr;
    track_backup(&(tmp_elt->track_count), &tmp);
    QUEUE_MOVE(&(tmp_elt->tracks), tmp);
    xfree(tmp);
    track_restore(&tmp_queue);

    unsigned int tmp_count = waypt_count();
    set_waypt_count(tmp_elt->waypt_ct);
    tmp_elt->waypt_ct = tmp_count;
  }
}

void StackFilter::init()
{

  int invalid = 0;

  if (nowarn) {
    warnings_enabled = 0;
  }

  if (opt_depth) {
    swapdepth = atoi(opt_depth);
  }
  if (opt_push) {
    if (opt_pop || opt_append || opt_discard || opt_replace ||
        opt_swap || opt_depth) {
      invalid = 1;
    }
  } else if (opt_pop) {
    if (opt_push || opt_copy || opt_swap || opt_depth) {
      invalid = 1;
    }
    if (!!opt_append + !!opt_discard + !!opt_replace > 1) {
      invalid = 1;
    }
  } else if (opt_swap) {
    if (opt_push || opt_copy  || opt_pop || opt_append ||
        opt_discard || opt_replace) {
      invalid = 1;
    }
  } else {
    invalid = 1;
  }

  if (invalid) {
    fatal(MYNAME ": invalid combination of options\n");
  }

}

void StackFilter::deinit()
{
  swapdepth = 0;
}

void StackFilter::exit()
{
  struct stack_elt* tmp_elt = nullptr;

  if (warnings_enabled && stack) {
    warning(MYNAME " Warning: leftover stack entries; "
            "check command line for mistakes\n");
  }
  while (stack) {
    waypt_flush(&(stack->waypts));
    tmp_elt = stack;
    stack = stack->next;
    xfree(tmp_elt);
  }
}

#endif // FILTERS_ENABLED
