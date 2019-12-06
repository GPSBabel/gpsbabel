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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#ifndef DUPLICATE_H_INCLUDED_
#define DUPLICATE_H_INCLUDED_

#include <QtCore/QVector>  // for QVector

#include "defs.h"    // for ARGTYPE_BOOL, ARG_NOMINMAX, Waypoint (ptr only)
#include "filter.h"  // for Filter

#if FILTERS_ENABLED

class DuplicateFilter:public Filter
{
public:
  QVector<arglist_t>* get_args() override
  {
    return &args;
  }
  void process() override;

private:
  char* snopt = nullptr;
  char* lcopt = nullptr;
  char* purge_duplicates = nullptr;
  char* correct_coords = nullptr;

  QVector<arglist_t> args = {
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
  };

  struct btree_node {
    btree_node* left;
    btree_node* right;
    unsigned long data;
    Waypoint* wpt;
  };

  btree_node* addnode(btree_node* tree, btree_node* newnode, btree_node** oldnode);
  void free_tree(btree_node* tree);

  struct wpt_ptr {
    Waypoint* wpt;
    int index;
  };

  static int compare(const void* a, const void* b);

};
#endif
#endif // DUPLICATE_H_INCLUDED_
