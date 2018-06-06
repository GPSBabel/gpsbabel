/*
    exact duplicate point filter utility.

    Copyright (C) 2002-2014 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include <cstdio>
#include <cstdlib> // qsort

#if FILTERS_ENABLED
static char* snopt = nullptr;
static char* lcopt = nullptr;
static char* purge_duplicates = nullptr;
static char* correct_coords = nullptr;

static
arglist_t dup_args[] = {
  {
    "shortname", &snopt, "Suppress duplicate waypoints based on name",
    nullptr, ARGTYPE_BEGIN_REQ | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "location", &lcopt, "Suppress duplicate waypoint based on coords",
    nullptr, ARGTYPE_END_REQ | ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "all", &purge_duplicates, "Suppress all instances of duplicates",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  {
    "correct", &correct_coords, "Use coords from duplicate points",
    nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
  },
  ARG_TERMINATOR
};


typedef struct btree_node {
  struct btree_node* left, *right;
  unsigned long data;
  Waypoint* wpt;
} btree_node;

static btree_node*
addnode(btree_node* tree, btree_node* newnode, btree_node** oldnode)
{
  btree_node* tmp, * last = nullptr;

  if (*oldnode) {
    *oldnode = nullptr;
  }

  if (!tree) {
    return (newnode);
  }

  tmp = tree;

  while (tmp) {
    last = tmp;
    if (newnode->data < tmp->data) {
      tmp = tmp->right;
    } else if (newnode->data > tmp->data) {
      tmp = tmp->left;
    } else {
      if (oldnode) {
        *oldnode = tmp;
      }
      return (nullptr);
    }
  }

  if (newnode->data < last->data) {
    last->right = newnode;
  } else {
    last->left = newnode;
  }

  return (tree);
}

static void
free_tree(btree_node* tree)
{
  if (tree->left) {
    free_tree(tree->left);
  }
  if (tree->right) {
    free_tree(tree->right);
  }
  xfree(tree);
}

typedef struct {
  Waypoint* wpt;
  int index;
} wpt_ptr;

/*

It looks odd that we have different comparisons for date and index.
	If exported if a < b return 1
	if index    if a < b return -1

The reason is that we want to sort in reverse order by date, but forward
order by index.  So if we have four records:

    date      index
    June 24    0
    June 25    1
    June 25    2
    June 24    3

we want to sort them like this:

    date      index
    June 25    1
    June 25    2
    June 24    0
    June 24    3

Thus, the first point we come across is the latest point, but if we
have two points with the same export date/time, we will first see the
one with the smaller index (i.e. the first of those two points that we
came across while importing waypoints.)

In the (common) case that we have no exported dates, the dates will all
be zero so the sort will end up being an expensive no-op (expensive
because, sadly, quicksort can be O(n^2) on presorted elements.)
*/


static
int
compare(const void* a, const void* b)
{
  const wpt_ptr* wa = (wpt_ptr*)a;
  const wpt_ptr* wb = (wpt_ptr*)b;

  if (wa->wpt->gc_data->exported < wb->wpt->gc_data->exported) {
    return 1;
  } else if (wa->wpt->gc_data->exported > wb->wpt->gc_data->exported) {
    return -1;
  }

  /* If the exported dates are the same, sort by index. */
  if (wa->index < wb->index) {
    return -1;
  } else if (wa->index > wb->index) {
    return 1;
  }

  /* If index and date are the same, it's the same element. */
  return 0;

}


static void
duplicate_process(void)
{
  Waypoint* waypointp;
  btree_node* newnode, * btmp, * sup_tree = nullptr;
  btree_node* oldnode = nullptr;
  unsigned long crc = 0;
  struct {
    char shortname[32];
    char lat[13];
    char lon[13];
  } dupe;
  Waypoint* delwpt = nullptr;

  int i, ct = waypt_count();
  wpt_ptr* htable, *bh;
  queue* elem, *tmp;

  htable = (wpt_ptr*) xmalloc(ct * sizeof(*htable));
  bh = htable;

  i = 0;
#if NEWQ
  foreach(Waypoint* waypointp, waypt_list) {
    bh->wpt = waypointp;
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    bh->wpt = (Waypoint*) elem;
#endif
    bh->index = i;
    i ++;
    bh ++;
  }
  qsort(htable, ct, sizeof(*htable), compare);

  for (i=0; i<ct; i++) {
    waypointp = htable[i].wpt;

    memset(&dupe, '\0', sizeof(dupe));

    if (snopt) {
      strncpy(dupe.shortname, CSTRc(waypointp->shortname), sizeof(dupe.shortname) - 1);
    }

    if (lcopt) {
      /* let sprintf take care of rounding */
      sprintf(dupe.lat, "%11.4f", waypointp->latitude);
      sprintf(dupe.lon, "%11.4f", waypointp->longitude);
      /* The degrees2ddmm stuff is a feeble attempt to
       * get everything rounded the same way in a precision
       * that's "close enough" for determining duplicates.
       */
      sprintf(dupe.lat, "%11.3f", degrees2ddmm(waypointp->latitude));
      sprintf(dupe.lon, "%11.3f", degrees2ddmm(waypointp->longitude));

    }

    crc = get_crc32(&dupe, sizeof(dupe));

    newnode = (btree_node*)xcalloc(sizeof(btree_node), 1);
    newnode->data = crc;
    newnode->wpt = waypointp;

    btmp = addnode(sup_tree, newnode, &oldnode);

    if (btmp == nullptr) {
      if (delwpt) {
        delete delwpt;
      }
      if (correct_coords && oldnode && oldnode->wpt) {
        oldnode->wpt->latitude = waypointp->latitude;
        oldnode->wpt->longitude = waypointp->longitude;
      }
      delwpt = waypointp;
      waypt_del(waypointp); /* collision */
      xfree(newnode);
      if (purge_duplicates && oldnode) {
        if (oldnode->wpt) {
          waypt_del(oldnode->wpt);
          delete oldnode->wpt;
          oldnode->wpt = nullptr;
        }
      }

    } else {
      sup_tree = btmp;
    }
  }

  if (delwpt) {
    delete delwpt;
  }

  xfree(htable);
  if (sup_tree) {
    free_tree(sup_tree);
  }
}

filter_vecs_t duplicate_vecs = {
  nullptr,
  duplicate_process,
  nullptr,
  nullptr,
  dup_args
};
#endif
