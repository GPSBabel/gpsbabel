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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

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
#include <QtCore/QString>
#include <cstdio>
#include <cstdlib>
#include <cstdlib> // qsort

typedef struct {
  Filter* vec;
  const char* name;
  const char* desc;
} fl_vecs_t;

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
fl_vecs_t filter_vec_list[] = {
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
  },

#elif defined (MINIMAL_FILTERS)
  {
    &trackfilter,
    "track",
    "Manipulate track lists"
  },
#endif
  {
    nullptr,
    nullptr,
    nullptr
  }
};

Filter*
find_filter_vec(const char* const vecname, const char** opts)
{
  fl_vecs_t* vec = filter_vec_list;
  char* v = xstrdup(vecname);
  QString svecname = QString(v).split(",")[0];
  int found = 0;

  while (vec->vec) {
    arglist_t* ap;

    if (svecname.compare(vec->name, Qt::CaseInsensitive)) {
      vec++;
      continue;
    }

    /* step 1: initialize by inifile or default values */
    struct arglist* args = vec->vec->get_args();
    if (args) {
      for (ap = args; ap->argstring; ap++) {
        QString qtemp = inifile_readstr(global_opts.inifile, vec->name, ap->argstring);
        if (qtemp.isNull()) {
          qtemp = inifile_readstr(global_opts.inifile, "Common filter settings", ap->argstring);
        }
        if (qtemp.isNull()) {
          assign_option(vec->name, ap, ap->defaultvalue);
        } else {
          assign_option(vec->name, ap, CSTR(qtemp));
        }
      }
    }

    /* step 2: override settings with command-line values */
    const char* res = strchr(vecname, ',');
    if (res) {
      *opts = res+1;

      if (args) {
        for (ap = args; ap->argstring; ap++) {
          char* opt = get_option(*opts, ap->argstring);
          if (opt) {
            found = 1;
            assign_option(vec->name, ap, opt);
            xfree(opt);
          }
        }
      }
    } else {
      *opts = nullptr;
    }
    if (opts && opts[0] && !found) {
      warning("'%s' is an unknown option to %s.\n", *opts, vec->name);
    }

    if (global_opts.debug_level >= 1) {
      disp_vec_options(vec->name, args);
    }

    xfree(v);
    return vec->vec;

  }
  xfree(v);
  return nullptr;
}

void
free_filter_vec(Filter* filter)
{
  struct arglist* args = filter->get_args();

  if (args) {
    for (arglist_t* ap = args; ap->argstring; ap++) {
      if (ap->argvalptr) {
        xfree(ap->argvalptr);
        ap->argvalptr = *ap->argval = nullptr;
      }
    }
  }
}

void
init_filter_vecs()
{
  fl_vecs_t* vec = filter_vec_list;
  while (vec->vec) {
    struct arglist* args = vec->vec->get_args();
    if (args) {
      for (arglist_t* ap = args; ap->argstring; ap++) {
        ap->argvalptr = nullptr;
      }
    }
    vec++;
  }
}

void
exit_filter_vecs()
{
  fl_vecs_t* vec = filter_vec_list;
  while (vec->vec) {
      (vec->vec->exit)();
    vec++;
  }
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
void
disp_filter_vecs()
{
  for (fl_vecs_t* vec = filter_vec_list; vec->vec; vec++) {
    printf("	%-20.20s  %-50.50s\n",
           vec->name, vec->desc);
    struct arglist* args = vec->vec->get_args();
    for (arglist_t* ap = args; ap && ap->argstring; ap++) {
      if (!(ap->argtype & ARGTYPE_HIDDEN))
        printf("	  %-18.18s    %-.50s %s\n",
               ap->argstring, ap->helpstring,
               (ap->argtype&ARGTYPE_REQUIRED)?"(required)":"");
    }
  }
}

void
disp_filter_vec(const char* vecname)
{
  for (fl_vecs_t* vec = filter_vec_list; vec->vec; vec++) {
    if (case_ignore_strcmp(vec->name, vecname)) {
      continue;
    }
    printf("	%-20.20s  %-50.50s\n",
           vec->name, vec->desc);
    struct arglist* args = vec->vec->get_args();
    for (arglist_t* ap = args; ap && ap->argstring; ap++) {
      if (!(ap->argtype & ARGTYPE_HIDDEN))
        printf("	  %-18.18s    %-.50s %s\n",
               ap->argstring, ap->helpstring,
               (ap->argtype&ARGTYPE_REQUIRED)?"(required)":"");
    }
  }
}

static signed int
alpha(const void* a, const void* b)
{
  const fl_vecs_t* const ap = (const fl_vecs_t*) a;
  const fl_vecs_t* const bp = (const fl_vecs_t*) b;

  return case_ignore_strcmp(ap->desc, bp->desc);
}

static
void disp_help_url(const fl_vecs_t* vec, arglist_t* arg)
{
  printf("\t" WEB_DOC_DIR "/fmt_%s.html", vec->name);
  if (arg) {
    printf("#fmt_%s_o_%s",vec->name, arg->argstring);
  }
}

static void
disp_v1(const fl_vecs_t* vec)
{
  disp_help_url(vec, nullptr);
  printf("\n");
  struct arglist* args = vec->vec->get_args();
  for (arglist_t* ap = args; ap && ap->argstring; ap++) {
    if (!(ap->argtype & ARGTYPE_HIDDEN)) {
      printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
             vec->name,
             ap->argstring,
             ap->helpstring,
             name_option(ap->argtype),
             ap->defaultvalue? ap->defaultvalue : "",
             ap->minvalue? ap->minvalue : "",
             ap->maxvalue? ap->maxvalue : "");
      disp_help_url(vec, ap);
      printf("\n");
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
  fl_vecs_t* vec;

  qsort(filter_vec_list,
        sizeof(filter_vec_list) / sizeof(filter_vec_list[0]) - 1,
        sizeof(filter_vec_list[0]),
        alpha);


  switch (version) {
  case 0:
  case 1:
    for (vec = filter_vec_list; vec->vec; vec++) {
      if (version == 0) {
        printf("%s\t%s\n", vec->name, vec->desc);
      } else {
        printf("%s\t%s", vec->name, vec->desc);
        disp_v1(vec);
      }
    }
    break;
  default:
    ;
  }
}
