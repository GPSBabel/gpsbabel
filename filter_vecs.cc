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

#include "defs.h"
#include "arcdist.h"
#include "bend.h"
#include "discard.h"
#include "duplicate.h"
#include "filterdefs.h"
#include "filter.h"
#include "height.h"
#include "interpolate.h"
#include "nukedata.h"
#include "polygon.h"
#include "position.h"
#include "radius.h"
#include "reverse_route.h"
#include "smplrout.h"
#include "sort.h"
#include "stackfilter.h"
#include "swapdata.h"
#include "trackfilter.h"
#include "transform.h"
#include "validate.h"
#include "gbversion.h"
#include "inifile.h"
#include "vecs.h"

#include <QtCore/QByteArray>   // for QByteArray
#include <QtCore/QString>      // for QString
#include <QtCore/QStringList>  // for QStringList
#include <QtCore/QVector>      // for QVector<>::iterator, QVector
#include <QtCore/Qt>           // for CaseInsensitive
#include <QtCore/QtGlobal>     // for qPrintable

#include <algorithm>           // for sort
#include <cassert>             // for assert
#include <cstdio>              // for printf


struct fl_vecs_t {
  Filter* vec;
  QString name;
  QString desc;
};

ArcDistanceFilter arcdist;
BendFilter bend;
DiscardFilter discard;
DuplicateFilter duplicate;
HeightFilter height;
InterpolateFilter interpolate;
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


static
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

Filter*
find_filter_vec(const QString& vecname)
{
  QStringList options = vecname.split(',');
  if (options.isEmpty()) {
    fatal("A filter name is required.\n");
  }
  const QString svecname = options.takeFirst();

  for (const auto& vec : filter_vec_list) {
    if (svecname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    QVector<arglist_t>* args = vec.vec->get_args();

    Vecs::validate_options(options, args, vec.name);

    /* step 1: initialize by inifile or default values */
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        QString qtemp = inifile_readstr(global_opts.inifile, vec.name, arg.argstring);
        if (qtemp.isNull()) {
          qtemp = inifile_readstr(global_opts.inifile, "Common filter settings", arg.argstring);
        }
        if (qtemp.isNull()) {
          Vecs::assign_option(vec.name, &arg, arg.defaultvalue);
        } else {
          Vecs::assign_option(vec.name, &arg, CSTR(qtemp));
        }
      }
    }

    /* step 2: override settings with command-line values */
    if (!options.isEmpty()) {
      if (args && !args->isEmpty()) {
        assert(args->isDetached());
        for (auto& arg : *args) {
          const QString opt = Vecs::get_option(options, arg.argstring);
          if (!opt.isNull()) {
            Vecs::assign_option(vec.name, &arg, CSTR(opt));
          }
        }
      }
    }

    if (global_opts.debug_level >= 1) {
      Vecs::disp_vec_options(vec.name, args);
    }

    return vec.vec;

  }
  return nullptr;
}

void
free_filter_vec(Filter* filter)
{
  QVector<arglist_t>* args = filter->get_args();

  if (args && !args->isEmpty()) {
    assert(args->isDetached());
    for (auto& arg : *args) {
      if (arg.argvalptr) {
        xfree(arg.argvalptr);
        arg.argvalptr = *arg.argval = nullptr;
      }
    }
  }
}

void
init_filter_vecs()
{
  for (const auto& vec : filter_vec_list) {
    QVector<arglist_t>* args = vec.vec->get_args();
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        arg.argvalptr = nullptr;
      }
    }
  }
}

void
exit_filter_vecs()
{
  for (const auto& vec : filter_vec_list) {
    (vec.vec->exit)();
  }
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void
disp_filter_vecs()
{
  for (const auto& vec : filter_vec_list) {
    printf("	%-20.20s  %-50.50s\n",
           qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arglist_t>* args = vec.vec->get_args();
    if (args) {
      for (const auto& arg : *args) {
        if (!(arg.argtype & ARGTYPE_HIDDEN))
          printf("	  %-18.18s    %-.50s %s\n",
                 arg.argstring, arg.helpstring,
                 (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

void
disp_filter_vec(const QString& vecname)
{
  for (const auto& vec : filter_vec_list) {
    if (vecname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }
    printf("	%-20.20s  %-50.50s\n",
           qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arglist_t>* args = vec.vec->get_args();
    if (args) {
      for (const auto& arg : *args) {
        if (!(arg.argtype & ARGTYPE_HIDDEN))
          printf("	  %-18.18s    %-.50s %s\n",
                 arg.argstring, arg.helpstring,
                 (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

static
void disp_help_url(const fl_vecs_t& vec, const arglist_t* arg)
{
  printf("\t" WEB_DOC_DIR "/fmt_%s.html", CSTR(vec.name));
  if (arg) {
    printf("#fmt_%s_o_%s", CSTR(vec.name), arg->argstring);
  }
}

static void
disp_v1(const fl_vecs_t& vec)
{
  disp_help_url(vec, nullptr);
  printf("\n");
  const QVector<arglist_t>* args = vec.vec->get_args();
  if (args) {
    for (const auto& arg : *args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
               CSTR(vec.name),
               arg.argstring,
               arg.helpstring,
               Vecs::name_option(arg.argtype),
               arg.defaultvalue ? arg.defaultvalue : "",
               arg.minvalue ? arg.minvalue : "",
               arg.maxvalue ? arg.maxvalue : "");
        disp_help_url(vec, &arg);
        printf("\n");
      }
    }
  }
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void
disp_filters(int version)
{
  auto sorted_filter_vec_list = filter_vec_list;

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

static bool
validate_filter_vec(const fl_vecs_t& vec)
{
  bool ok = Vecs::validate_args(vec.name, vec.vec->get_args());

  return ok;
}

bool validate_filters()
{
  bool ok = true;

  for (const auto& vec : filter_vec_list) {
    ok = validate_filter_vec(vec) && ok;
  }

  return ok;
}
