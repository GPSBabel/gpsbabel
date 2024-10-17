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

#include "filter_vecs.h"

#include <QByteArray>       // for QByteArray
#include <QString>          // for QString
#include <QStringList>      // for QStringList
#include <QVector>          // for QVector
#include <Qt>               // for CaseInsensitive
#include <QtGlobal>         // for qPrintable

#include <algorithm>        // for sort
#include <cassert>          // for assert
#include <cstdio>           // for printf
#include <type_traits>      // for is_base_of

#include "arcdist.h"        // for ArcDistanceFilter
#include "bend.h"           // for BendFilter
#include "defs.h"           // for arglist_t, CSTR, xfree, ARGTYPE_HIDDEN, fatal, global_options, global_opts, ARGTYPE_REQUIRED
#include "discard.h"        // for DiscardFilter
#include "duplicate.h"      // for DuplicateFilter
#include "filter.h"         // for Filter
#include "gbversion.h"      // for WEB_DOC_DIR
#include "height.h"         // for HeightFilter
#include "inifile.h"        // for inifile_readstr
#include "interpolate.h"    // for InterpolateFilter
#include "nukedata.h"       // for NukeDataFilter
#include "polygon.h"        // for PolygonFilter
#include "position.h"       // for PositionFilter
#include "radius.h"         // for RadiusFilter
#include "resample.h"       // for ResampleFilter
#include "reverse_route.h"  // for ReverseRouteFilter
#include "smplrout.h"       // for SimplifyRouteFilter
#include "sort.h"           // for SortFilter
#include "stackfilter.h"    // for StackFilter
#include "swapdata.h"       // for SwapDataFilter
#include "trackfilter.h"    // for TrackFilter
#include "transform.h"      // for TransformFilter
#include "validate.h"       // for ValidateFilter
#include "vecs.h"           // for Vecs

template <typename T>
Filter* fltfactory()
{
  static_assert(std::is_base_of<Filter, T>::value, "T must be derived from Filter");
  return new T();
}

struct FilterVecs::Impl {
  StackFilter stackfilt;

  const QVector<fl_vecs_t> filter_vec_list = {
#if FILTERS_ENABLED
    {
      nullptr,
      "arc",
      "Include Only Points Within Distance of Arc",
      &fltfactory<ArcDistanceFilter>
    },
    {
      nullptr,
      "bend",
      "Add points before and after bends in routes",
      &fltfactory<BendFilter>
    },
    {
      nullptr,
      "discard",
      "Remove unreliable points with high hdop or vdop",
      &fltfactory<DiscardFilter>
    },
    {
      nullptr,
      "duplicate",
      "Remove Duplicates",
      &fltfactory<DuplicateFilter>
    },
    {
      nullptr,
      "interpolate",
      "Interpolate between trackpoints",
      &fltfactory<InterpolateFilter>
    },
    {
      nullptr,
      "nuketypes",
      "Remove all waypoints, tracks, or routes",
      &fltfactory<NukeDataFilter>
    },
    {
      nullptr,
      "polygon",
      "Include Only Points Inside Polygon",
      &fltfactory<PolygonFilter>
    },
    {
      nullptr,
      "position",
      "Remove Points Within Distance",
      &fltfactory<PositionFilter>
    },
    {
      nullptr,
      "radius",
      "Include Only Points Within Radius",
      &fltfactory<RadiusFilter>
    },
    {
      nullptr,
      "resample",
      "Resample Track",
      &fltfactory<ResampleFilter>
    },
    {
      nullptr,
      "simplify",
      "Simplify routes",
      &fltfactory<SimplifyRouteFilter>
    },
    {
      nullptr,
      "sort",
      "Rearrange waypoints, routes and/or tracks by resorting",
      &fltfactory<SortFilter>
    },
    {
      &stackfilt,
      "stack",
      "Save and restore waypoint lists",
      nullptr
    },
    {
      nullptr,
      "reverse",
      "Reverse stops within routes",
      &fltfactory<ReverseRouteFilter>
    },
    {
      nullptr,
      "track",
      "Manipulate track lists",
      &fltfactory<TrackFilter>
    },
    {
      nullptr,
      "transform",
      "Transform waypoints into a route, tracks into routes, ...",
      &fltfactory<TransformFilter>
    },
    {
      nullptr,
      "height",
      "Manipulate altitudes",
      &fltfactory<HeightFilter>
    },
    {
      nullptr,
      "swap",
      "Swap latitude and longitude of all loaded points",
      &fltfactory<SwapDataFilter>
    },
    {
      nullptr,
      "validate",
      "Validate internal data structures",
      &fltfactory<ValidateFilter>
    }
#elif defined (MINIMAL_FILTERS)
    {
      nullptr,
      "track",
      "Manipulate track lists",
      &fltfactory<TrackFilter>
    }
#endif
  };
};

FilterVecs& FilterVecs::Instance()
{
  static Impl impl;
  static FilterVecs instance(&impl);
  return instance;
}

void FilterVecs::prepare_filter(const fltinfo_t& fltdata)
{
  QVector<arglist_t>* args = fltdata->get_args();

  Vecs::validate_options(fltdata.options, args, fltdata.fltname);

  /* step 1: initialize by inifile or default values */
  if (args && !args->isEmpty()) {
    assert(args->isDetached());
    for (auto& arg : *args) {
      QString qtemp = inifile_readstr(global_opts.inifile, fltdata.fltname, arg.argstring);
      if (qtemp.isNull()) {
        qtemp = inifile_readstr(global_opts.inifile, "Common filter settings", arg.argstring);
      }
      if (qtemp.isNull()) {
        Vecs::assign_option(fltdata.fltname, arg, arg.defaultvalue);
      } else {
        Vecs::assign_option(fltdata.fltname, arg, qtemp);
      }
    }
  }

  /* step 2: override settings with command-line values */
  if (!fltdata.options.isEmpty()) {
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        const QString opt = Vecs::get_option(fltdata.options, arg.argstring);
        if (!opt.isNull()) {
          Vecs::assign_option(fltdata.fltname, arg, opt);
        }
      }
    }
  }

  if (global_opts.debug_level >= 1) {
    Vecs::disp_vec_options(fltdata.fltname, args);
  }

}

FilterVecs::fltinfo_t FilterVecs::find_filter_vec(const QString& fltargstring)
{
  QStringList options = fltargstring.split(',');
  if (options.isEmpty()) {
    fatal("A filter name is required.\n");
  }
  const QString fltname = options.takeFirst();

  for (const auto& vec : d_ptr_->filter_vec_list) {
    if (fltname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    return {vec.vec, vec.name, options, vec.factory};
  }

  /*
   * Not found.
   */
  return {};
}

void FilterVecs::free_filter_vec(Filter* flt)
{
  Vecs::free_options(flt->get_args());
}

void FilterVecs::init_filter_vec(Filter* flt)
{
  QVector<arglist_t>* args = flt->get_args();
  if (args && !args->isEmpty()) {
    assert(args->isDetached());
  }
}

void FilterVecs::init_filter_vecs()
{
  for (const auto& vec : d_ptr_->filter_vec_list) {
    if (vec.vec != nullptr) {
      init_filter_vec(vec.vec);
    }
  }
}

void FilterVecs::exit_filter_vec(Filter* flt)
{
    (flt->exit)();
    Vecs::free_options(flt->get_args());
}

void FilterVecs::exit_filter_vecs()
{
  for (const auto& vec : d_ptr_->filter_vec_list) {
    if (vec.vec != nullptr) {
      exit_filter_vec(vec.vec);
    }
  }
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void FilterVecs::disp_filter_vec(const QString& vecname) const
{
  for (const auto& vec : d_ptr_->filter_vec_list) {
    /*
     * Display info for all filter is vecname is empty,
     * otherwise just display info for the matching filter.
     */
    if (!vecname.isEmpty() && (vecname.compare(vec.name, Qt::CaseInsensitive) != 0)) {
      continue;
    }
    Filter* flt = (vec.factory != nullptr)? vec.factory() : vec.vec;
    printf("	%-20.20s  %-50.50s\n",
           qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arglist_t>* args = flt->get_args();
    if (args) {
      for (const auto& arg : *args) {
        if (!(arg.argtype & ARGTYPE_HIDDEN)) {
          printf("	  %-18.18s    %-.50s %s\n",
                 qPrintable(arg.argstring), qPrintable(arg.helpstring),
                 (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
        }
      }
    }
    if (vec.factory != nullptr) {
      delete flt;
    }
  }
}

void FilterVecs::disp_help_url(const fl_vecs_t& vec, const arglist_t* arg)
{
  printf("\t" WEB_DOC_DIR "/filter_%s.html", CSTR(vec.name));
  if (arg) {
    printf("#fmt_%s_o_%s", CSTR(vec.name), CSTR(arg->argstring));
  }
}

void FilterVecs::disp_v1(const fl_vecs_t& vec)
{
  Filter* flt = (vec.factory != nullptr)? vec.factory() : vec.vec;
  disp_help_url(vec, nullptr);
  printf("\n");
  const QVector<arglist_t>* args = flt->get_args();
  if (args) {
    for (const auto& arg : *args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
               CSTR(vec.name),
               CSTR(arg.argstring),
               CSTR(arg.helpstring),
               Vecs::name_option(arg.argtype),
               CSTR(arg.defaultvalue),
               CSTR(arg.minvalue),
               CSTR(arg.maxvalue));
        disp_help_url(vec, &arg);
        printf("\n");
      }
    }
  }
  if (vec.factory != nullptr) {
    delete flt;
  }
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void FilterVecs::disp_filters(int version) const
{
  auto sorted_filter_vec_list = d_ptr_->filter_vec_list;

  auto alpha = [](const fl_vecs_t& a, const fl_vecs_t& b)->bool {
    return QString::compare(a.desc, b.desc, Qt::CaseInsensitive) < 0;
  };

  std::sort(sorted_filter_vec_list.begin(), sorted_filter_vec_list.end(), alpha);

  switch (version) {
  case 0:
  case 1:
    for (const auto& vec : sorted_filter_vec_list) {
      if (version == 0) {
        printf("%s\t%s\n", CSTR(vec.name), CSTR(vec.desc));
      } else {
        printf("%s\t%s", CSTR(vec.name), CSTR(vec.desc));
        disp_v1(vec);
      }
    }
    break;
  default:
    ;
  }
}

bool FilterVecs::validate_filter_vec(const fl_vecs_t& vec)
{
  Filter* flt = (vec.factory != nullptr)? vec.factory() : vec.vec;
  bool ok = Vecs::validate_args(vec.name, flt->get_args());
  if (vec.factory != nullptr) {
    delete flt;
  }

  return ok;
}

bool FilterVecs::validate_filters() const
{
  bool ok = true;

  for (const auto& vec : d_ptr_->filter_vec_list) {
    ok = validate_filter_vec(vec) && ok;
  }

  return ok;
}
