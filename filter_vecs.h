/*
    Describe vectors containing filter operations.

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
#ifndef FILTER_VECS_H_INCLUDED_
#define FILTER_VECS_H_INCLUDED_

#include <QtCore/QString>      // for QString
#include <QtCore/QVector>      // for QVector<>::iterator, QVector

#include "defs.h"           // for arglist_t
#include "arcdist.h"        // for ArcDistanceFilter
#include "bend.h"           // for BendFilter
#include "discard.h"        // for DiscardFilter
#include "duplicate.h"      // for DuplicateFilter
#include "filter.h"         // for Filter
#include "height.h"         // for HeightFilter
#include "interpolate.h"    // for InterpolateFilter
#include "kalmanfilter.h"   // for KalmanFilter
#include "nukedata.h"       // for NukeDataFilter
#include "polygon.h"        // for PolygonFilter
#include "position.h"       // for PositionFilter
#include "radius.h"         // for RadiusFilter
#include "reverse_route.h"  // for ReverseRouteFilter
#include "smplrout.h"       // for SimplifyRouteFilter
#include "sort.h"           // for SortFilter
#include "stackfilter.h"    // for StackFilter
#include "swapdata.h"       // for SwapDataFilter
#include "trackfilter.h"    // for TrackFilter
#include "transform.h"      // for TransformFilter
#include "validate.h"       // for ValidateFilter


class FilterVecs
{
// Meyers Singleton
public:
  static FilterVecs& Instance()
  {
    static FilterVecs instance;
    return instance;
  }
  FilterVecs(const FilterVecs&) = delete;
  FilterVecs& operator= (const FilterVecs&) = delete;
  FilterVecs(FilterVecs&&) = delete;
  FilterVecs& operator=(FilterVecs&&) = delete;

private:
  FilterVecs() = default;
  ~FilterVecs() = default;

private:
  struct fl_vecs_t {
    Filter* vec;
    QString name;
    QString desc;
  };

public:
Filter* find_filter_vec(const QString& vecname);
static void free_filter_vec(Filter* filter);
void init_filter_vecs();
void exit_filter_vecs();
void disp_filter_vecs() const;
void disp_filter_vec(const QString& vecname) const;
void disp_filters(int version) const;
bool validate_filters() const;

private:
static void disp_help_url(const fl_vecs_t& vec, const arglist_t* arg);
static void disp_v1(const fl_vecs_t& vec);
static bool validate_filter_vec(const fl_vecs_t& vec);

private:
  ArcDistanceFilter arcdist;
  BendFilter bend;
  DiscardFilter discard;
  DuplicateFilter duplicate;
  HeightFilter height;
  InterpolateFilter interpolate;
  KalmanFilter kalmanfilter;
  NukeDataFilter nukedata;
  PolygonFilter polygon;
  PositionFilter position;
  RadiusFilter radius;
  ReverseRouteFilter reverse_route;
  SimplifyRouteFilter routesimple;
  SortFilter sort;
  StackFilter stackfilt;
  SwapDataFilter swapdata;
  TrackFilter trackfilter;
  TransformFilter transform;
  ValidateFilter validate;

  const QVector<fl_vecs_t> filter_vec_list = {
#if FILTERS_ENABLED
    {
      &arcdist,
      "arc",
      "Include Only Points Within Distance of Arc",
    },
    {
      &bend,
      "bend",
      "Add points before and after bends in routes"
    },
    {
      &discard,
      "discard",
      "Remove unreliable points with high hdop or vdop"
    },
    {
      &duplicate,
      "duplicate",
      "Remove Duplicates",
    },
    {
      &interpolate,
      "interpolate",
      "Interpolate between trackpoints"
    },
    {
      &kalmanfilter,
      "kalman",
      "Kalman filtering for noisy tracks"
    },
    {
      &nukedata,
      "nuketypes",
      "Remove all waypoints, tracks, or routes"
    },
    {
      &polygon,
      "polygon",
      "Include Only Points Inside Polygon",
    },
    {
      &position,
      "position",
      "Remove Points Within Distance",
    },
    {
      &radius,
      "radius",
      "Include Only Points Within Radius",
    },
    {
      &routesimple,
      "simplify",
      "Simplify routes",
    },
    {
      &sort,
      "sort",
      "Rearrange waypoints, routes and/or tracks by resorting",
    },
    {
      &stackfilt,
      "stack",
      "Save and restore waypoint lists"
    },
    {
      &reverse_route,
      "reverse",
      "Reverse stops within routes",
    },
    {
      &trackfilter,
      "track",
      "Manipulate track lists"
    },
    {
      &transform,
      "transform",
      "Transform waypoints into a route, tracks into routes, ..."
    },
    {
      &height,
      "height",
      "Manipulate altitudes"
    },
    {
      &swapdata,
      "swap",
      "Swap latitude and longitude of all loaded points"
    },
    {
      &validate,
      "validate",
      "Validate internal data structures"
    }
#elif defined (MINIMAL_FILTERS)
    {
      &trackfilter,
      "track",
      "Manipulate track lists"
    }
#endif
  };
};
#endif // FILTER_VECS_H_INCLUDED_
