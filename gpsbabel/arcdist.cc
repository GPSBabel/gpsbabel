/*
    Distance from point to arc filter

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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

#include <math.h>

#include "defs.h"
#include "filterdefs.h"
#include "grtcirc.h"

#if FILTERS_ENABLED
#define MYNAME "Arc filter"

static double pos_dist;
static char* distopt = NULL;
static char* arcfileopt = NULL;
static char* rteopt = NULL;
static char* trkopt = NULL;
static char* exclopt = NULL;
static char* ptsopt = NULL;
static char* projectopt = NULL;

typedef struct {
  double distance;
  double prjlatitude, prjlongitude;
  double frac;
  waypoint* arcpt1, * arcpt2;
} extra_data;

static
arglist_t arcdist_args[] = {
  {
    "file", &arcfileopt,  "File containing vertices of arc",
    NULL, ARGTYPE_FILE, ARG_NOMINMAX
  },
  {
    "rte", &rteopt, "Route(s) are vertices of arc",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "trk", &trkopt, "Track(s) are vertices of arc",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "distance", &distopt, "Maximum distance from arc",
    NULL, ARGTYPE_FLOAT | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "exclude", &exclopt, "Exclude points close to the arc", NULL,
    ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "points", &ptsopt, "Use distance from vertices not lines",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  {
    "project", &projectopt, "Move waypoints to its projection on lines or vertices",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define BADVAL 999999

static void
arcdist_arc_disp_wpt_cb(const waypoint *arcpt2)
{
  queue* elem, * tmp;
  waypoint* waypointp;
  extra_data* ed;
  double dist;
  double prjlat, prjlon, frac;
  static waypoint* arcpt1 = NULL;

  if (arcpt2 && arcpt2->latitude != BADVAL && arcpt2->longitude != BADVAL &&
      (ptsopt || (arcpt1 &&
                  (arcpt1->latitude != BADVAL && arcpt1->longitude != BADVAL)))) {
    QUEUE_FOR_EACH(&waypt_head, elem, tmp) {

      waypointp = (waypoint*) elem;
      if (waypointp->extra_data) {
        ed = (extra_data*) waypointp->extra_data;
      } else {
        ed = (extra_data*) xcalloc(1, sizeof(*ed));
        ed->distance = BADVAL;
      }
      if (ed->distance == BADVAL || projectopt || ed->distance >= pos_dist) {
        if (ptsopt) {
          dist = gcdist(RAD(arcpt2->latitude),
                        RAD(arcpt2->longitude),
                        RAD(waypointp->latitude),
                        RAD(waypointp->longitude));
          prjlat =  arcpt2->latitude;
          prjlon = arcpt2->longitude;
          frac = 1.0;
        } else {
          dist = linedistprj(arcpt1->latitude,
                             arcpt1->longitude,
                             arcpt2->latitude,
                             arcpt2->longitude,
                             waypointp->latitude,
                             waypointp->longitude,
                             &prjlat, &prjlon, &frac);
        }

        /* convert radians to float point statute miles */
        dist = radtomiles(dist);

        if (ed->distance > dist) {
          ed->distance = dist;
          if (projectopt) {
            ed->prjlatitude = prjlat;
            ed->prjlongitude = prjlon;
            ed->frac = frac;
            ed->arcpt1 = arcpt1;
            ed->arcpt2 = (waypoint*) arcpt2;
          }
        }
        waypointp->extra_data = ed;
      }
    }
  }
  arcpt1 = (waypoint*) arcpt2;
}

static void
arcdist_arc_disp_hdr_cb(const route_head *rte)
{
  /* Set arcpt1 to NULL */
  arcdist_arc_disp_wpt_cb(NULL);
}

void
arcdist_process(void)
{
  queue* elem, * tmp;
  unsigned removed;

  if (arcfileopt) {
    int fileline = 0;
    char* line;
    gbfile* file_in;
    waypoint* arcpt2, * arcpt1;

    file_in = gbfopen(arcfileopt, "r", MYNAME);

    arcpt1 = waypt_new();
    arcpt2 = waypt_new();
    arcdist_arc_disp_hdr_cb(NULL);

    arcpt2->latitude = arcpt2->longitude = BADVAL;
    while ((line = gbfgetstr(file_in))) {
      char * pound = NULL;
      int argsfound = 0;

      fileline++;

      pound = strchr(line, '#');
      if (pound) {
        if (0 == strncmp(pound, "#break", 6)) {
         arcdist_arc_disp_hdr_cb(NULL);
        }
        *pound = '\0';
      }

      arcpt2->latitude = arcpt2->longitude = BADVAL;
      argsfound = sscanf(line, "%lf %lf", &arcpt2->latitude, &arcpt2->longitude);

      if (argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
        warning(MYNAME ": Warning: Arc file contains unusable vertex on line %d.\n", fileline);
      }
      else {
        waypoint* arcpttmp = arcpt1;
        arcdist_arc_disp_wpt_cb(arcpt2);
        arcpt1 = arcpt2;
        arcpt2 = arcpttmp;
      }
    }
    waypt_free(arcpt1);
    waypt_free(arcpt2);

    gbfclose(file_in);
  } else if (rteopt) {
    route_disp_all(arcdist_arc_disp_hdr_cb, NULL, arcdist_arc_disp_wpt_cb);
  } else if (trkopt) {
    track_disp_all(arcdist_arc_disp_hdr_cb, NULL, arcdist_arc_disp_wpt_cb);
  }

  removed = 0;
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypoint* wp = (waypoint*) elem;
    extra_data* ed;
    ed = (extra_data*) wp->extra_data;
    wp->extra_data = NULL;
    if (ed) {
      if ((ed->distance >= pos_dist) == (exclopt == NULL)) {
        waypt_del(wp);
        waypt_free(wp);
        removed++;
      } else if (projectopt) {
        wp->longitude = ed->prjlongitude;
        wp->latitude = ed->prjlatitude;
        wp->route_priority = 1;
        if (!arcfileopt &&
             (ed->arcpt2->altitude != unknown_alt) &&
             (ptsopt || (ed->arcpt1->altitude != unknown_alt))) {
          /* Interpolate alititude */
          if (ptsopt) {
            wp->altitude = ed->arcpt2->altitude;
          } else {
            wp->altitude = ed->arcpt1->altitude +
              ed->frac * (ed->arcpt2->altitude - ed->arcpt1->altitude);
          }
        }
        if (trkopt &&
             (ed->arcpt2->GetCreationTime().isValid()) &&
             (ptsopt || (ed->arcpt1->GetCreationTime().isValid()))) {
          /* Interpolate time */
          if (ptsopt) {
            wp->SetCreationTime(ed->arcpt2->GetCreationTime());;
          } else {
            // Apply the multiplier to the difference between the times
            // of the two points.   Add that to the first for the
            // interpolated time.
            int scaled_time = ed->frac *
                  ed->arcpt1->GetCreationTime().msecsTo(ed->arcpt2->GetCreationTime());
            QDateTime new_time(ed->arcpt1->GetCreationTime().addMSecs(scaled_time));
            wp->SetCreationTime(new_time);
          }
        }
        if (global_opts.debug_level >= 1) {
          warning("Including waypoint %s at dist:%f lat:%f lon:%f\n",
                  wp->shortname, ed->distance, wp->latitude, wp->longitude);
        }
      }
      xfree(ed);
    }
  }
  if (global_opts.verbose_status > 0) {
    printf(MYNAME "-arc: %d waypoint(s) removed.\n", removed);
  }
}

void
arcdist_init(const char* args)
{
  char* fm;

  if ((!arcfileopt && !rteopt && !trkopt) ||
      (arcfileopt && (rteopt || trkopt)) ||
      (rteopt && trkopt)) {
    fatal(MYNAME ": Incompatible or incomplete option values!\n");
  }

  pos_dist = 0;

  if (distopt) {
    pos_dist = strtod(distopt, &fm);

    if ((*fm == 'k') || (*fm == 'K')) {
      /* distance is kilometers, convert to mile */
      pos_dist *= .6214;
    }
  }
}

void
arcdist_deinit(void)
{
  /* do nothing */
}

filter_vecs_t arcdist_vecs = {
  arcdist_init,
  arcdist_process,
  arcdist_deinit,
  NULL,
  arcdist_args
};
#endif // FILTERS_ENABLED
