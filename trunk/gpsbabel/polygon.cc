/*
    Inside/Outside polygon filter

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
#include "defs.h"
#include "filterdefs.h"

#if FILTERS_ENABLED
#define MYNAME "Polygon filter"

static char *polyfileopt = NULL;
static char *exclopt = NULL;

/*
 * This test for insideness is essentially an odd/even test.  The
 * traditional (simple) implementation of the odd/even test counts
 * intersections along a test ray, and if it should happen to exactly hit
 * a vertex of the polygon, it throws away the result and fires a different
 * test ray.  Since we're potentially testing hundreds of test points against
 * a polygon with hundreds of sides, this seemed both wasteful and difficult
 * to coordinate, so instead I added extra state to try to figure out what we
 * would have seen if we had just missed the vertex.  The result is the
 * hodgepodge of "limbo" states explained below.  On the credit side of the
 * ledger, though, the tests for intersection are vastly simplified by always
 * having a horizontal test ray.
 *
 * The general structure of this filter is: we loop over the points in the
 * polygon.  For each point, we update the state of each of the waypoints in
 * the test set.  Thus, the state of every waypoint is indeterminate until
 * the end of the outer loop, at which point the state of every waypoint
 * should theoretically be completely determined.
 *
 * The bits following this comment encode the current state of the test point
 * as we go around the polygon.  OUTSIDE clearly isn't a bit; it's just here
 * for completeness.  If it's not INSIDE, and it's not something else, it's
 * clearly OUTSIDE.
 *
 * INSIDE is self-explanatory.  What it means is that the last time we were
 * certain of our state, we were inside of the polygon.
 *
 * LIMBO encodes a bit more state information, to handle the case where our
 * test ray (a horizontal line) has intersected one of the vertices of the
 * polygon exactly.  If the two lines that meet at that vertex are on
 * opposite sides of the test ray, it was an intersection.  Otherwise, it just
 * grazed a local minimum or maximum and so counted as either zero or two
 * intersections - not a change in state.  Horizontal lines encountered
 * while in limbo don't change the limbo state.  All other lines do.
 * The rest of the bits talk about how we got into limbo, and thus what to do
 * when we get out of limbo.
 *
 * LIMBO_UP means that the last line segment we saw going into limbo was
 * headed upward.  When we see another non-horizontal line segment, whether
 * we flip the INSIDE bit or not depends on whether it also goes upward.  If
 * it does, we flip the INSIDE bit.  Otherwise, we just clear our limbo state
 * and go on as if nothing had happened.
 *
 * LIMBO_BEGIN means that the very first vertex in the polygon put us into a
 * limbo state.  We won't be able to resolve that limbo state until we get to
 * the end of the cycle, and it can actually coexist with another more local
 * limbo state.  The next two bits talk about the beginning limbo state in
 * more detail.
 *
 * BEGIN_UP means that the first non-horizontal line segment we encountered
 * while in a LIMBO_BEGIN state went upward.  As with LIMBO_UP, this is used
 * to determine the final disposition of the limbo state when we get back
 * around to the other end of the cycle.
 *
 * BEGIN_HOR is fairly temporary.  It says that we've encountered one or more
 * horizontal line segments at the beginning of the cycle, so we haven't yet
 * been able to resolve the state of BEGIN_UP.  It's a way of propagating the
 * "firstness" forward until we can make a decision, without propagating it
 * for every test point.
 *
 * UP is used to remember which way we were going in case we encounter a
 * limbo state.
 *
 *  --  RLP
 */

#define OUTSIDE     0
#define INSIDE      1
#define LIMBO       2
#define LIMBO_UP    4
#define LIMBO_BEGIN 8
#define BEGIN_UP    16
#define BEGIN_HOR   32
#define UP          64

typedef struct {
  unsigned short state;
  unsigned short override;
} extra_data;

static
arglist_t polygon_args[] = {
  {
    "file", &polyfileopt,  "File containing vertices of polygon",
    NULL, ARGTYPE_FILE | ARGTYPE_REQUIRED, ARG_NOMINMAX
  },
  {
    "exclude", &exclopt, "Exclude points inside the polygon",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

static void polytest(double lat1, double lon1,
                     double lat2, double lon2,
                     double wlat, double wlon,
                     unsigned short *state, int first, int last)
{

  if (lat1 == wlat) {
    if (lat2 < wlat) {
      /* going down */
      if (*state & LIMBO) {
        if (*state & LIMBO_UP) {
          *state = *state ^ INSIDE;
        }
        *state = *state & ~LIMBO &~LIMBO_UP;
      } else if (*state & LIMBO_BEGIN) {
        if (*state & BEGIN_HOR) {
          *state = *state & ~BEGIN_HOR;
        } else if (last) {
          if (*state & BEGIN_UP) {
            *state = *state ^ INSIDE;
          }
          *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
        }
      } else if (first && (lon1 > wlon)) {
        *state |= LIMBO_BEGIN;
      }
    } else if (lat2 == wlat) {
      if (first & (lon1 > wlon || lon2 > wlon)) {
        *state |= LIMBO_BEGIN | BEGIN_HOR;
      } else if (last && (*state & LIMBO_BEGIN) && (*state & LIMBO)) {
        if ((!!(*state & LIMBO_UP)) != (!!(*state & BEGIN_UP))) {
          *state = *state ^ INSIDE;
        }
        *state = *state & ~LIMBO & ~LIMBO_UP &
                 ~LIMBO_BEGIN & ~BEGIN_UP;
      } else if (*state & LIMBO) {
        /* do nothing */
      } else {
        if (lon1 <= wlon && lon2 > wlon) {
          if (*state & UP) {
            *state &= ~UP;
            *state |= LIMBO_UP;
          }
          *state = *state | LIMBO;
        }
      }
    } else {
      /* going up */
      if (*state & LIMBO) {
        if (!(*state & LIMBO_UP)) {
          *state = *state ^ INSIDE;
        }
        *state = *state & ~LIMBO & ~LIMBO_UP;
      } else if (*state & LIMBO_BEGIN) {
        if (*state & BEGIN_HOR) {
          *state &= ~BEGIN_HOR;
          *state |= BEGIN_UP;
        } else if (last) {
          if (!(*state & BEGIN_UP)) {
            *state = *state ^ INSIDE;
          }
          *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
        }
      } else if (first && (lon1 > wlon)) {
        *state |= LIMBO_BEGIN | BEGIN_UP;
      }
    }
    *state = *state & ~UP;
  } else if (lat2 == wlat) {
    if (lat1 < wlat) {
      if (last) {
        if (*state & BEGIN_UP) {
          *state = *state ^ INSIDE;
        }
        *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
      } else if (lon2 > wlon) {
        *state |= LIMBO;
      }
    }
    /* no case for lat1==wlat; that's above */
    else {
      if (last) {
        if (!(*state & BEGIN_UP)) {
          *state = *state ^ INSIDE;
        }
        *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
      } else if (lon2 > wlon) {
        *state |= LIMBO | LIMBO_UP;
      } else {
        *state |= UP;
      }
    }
  } else {
    if ((lat1 > wlat && lat2 < wlat) ||
        (lat1 < wlat && lat2 > wlat)) {
      /* we only care if the lines might intersect */
      if (lon1 > wlon && lon2 > wlon) {
        *state = *state ^ INSIDE;
      } else if (!(lon1 <= wlon && lon2 <= wlon)) {
        /* we're inside the bbox of a diagonal line.  math time. */
        double loni = lon1+(lon2-lon1)/(lat2-lat1)*(wlat-lat1);
        if (loni > wlon) {
          *state = *state ^ INSIDE;
        }
      }
    }
  }

}

#define BADVAL 999999

void
polygon_process(void)
{
  queue * elem, * tmp;
  waypoint * waypointp;
  extra_data *ed;
  double lat1, lon1, lat2, lon2;
  double olat, olon;
  int fileline = 0;
  int first = 1;
  int last = 0;
  char *line;
  gbfile *file_in;

  file_in = gbfopen(polyfileopt, "r", MYNAME);

  olat = olon = lat1 = lon1 = lat2 = lon2 = BADVAL;
  while ((line = gbfgetstr(file_in))) {
    char *pound = NULL;
    int argsfound = 0;

    fileline++;

    pound = strchr(line, '#');
    if (pound) {
      *pound = '\0';
    }

    lat2 = lon2 = BADVAL;
    argsfound = sscanf(line, "%lf %lf", &lat2, &lon2);

    if (argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
      warning(MYNAME
              ": Warning: Polygon file contains unusable vertex on line %d.\n",
              fileline);
    } else if (lat1 != BADVAL && lon1 != BADVAL &&
               lat2 != BADVAL && lon2 != BADVAL) {
#if NEWQ
      foreach(waypoint* waypointp, waypt_list) {
#else
      QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
        waypointp = (waypoint *)elem;
#endif
        if (waypointp->extra_data) {
          ed = (extra_data *) waypointp->extra_data;
        } else {
          ed = (extra_data *) xcalloc(1, sizeof(*ed));
          ed->state = OUTSIDE;
          ed->override = 0;
          waypointp->extra_data = (extra_data *) ed;
        }
        if (lat2 == waypointp->latitude &&
            lon2 == waypointp->longitude) {
          ed->override = 1;
        }
        if (olat != BADVAL && olon != BADVAL &&
            olat == lat2 && olon == lon2) {
          last = 1;
        }
        polytest(lat1, lon1, lat2, lon2,
                 waypointp->latitude,
                 waypointp->longitude,
                 &ed->state, first, last);
        first = 0;
        last = 0;
      }
    }
    if (olat != BADVAL && olon != BADVAL &&
        olat == lat2 && olon == lon2) {
      olat = BADVAL;
      olon = BADVAL;
      lat1 = BADVAL;
      lon1 = BADVAL;
      first = 1;
    } else if (lat1 == BADVAL || lon1 == BADVAL) {
      olat = lat2;
      olon = lon2;
      lat1 = lat2;
      lon1 = lon2;
    } else {
      lat1 = lat2;
      lon1 = lon2;
    }
  }
  gbfclose(file_in);

#if NEWQ
  foreach(waypoint* wp, waypt_list) {
#else
  QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
    waypoint *wp = (waypoint *) elem;
#endif
    ed = (extra_data *) wp->extra_data;
    wp->extra_data = NULL;
    if (ed) {
      if (ed->override) {
        ed->state = INSIDE;
      }
      if (((ed->state & INSIDE) == OUTSIDE) == (exclopt == NULL)) {
        waypt_del(wp);
        waypt_free(wp);
      }
      xfree(ed);
    }
  }
}

void
polygon_init(const char *args)
{
  /* do nothing */
}

void
polygon_deinit(void)
{
  /* do nothing */
}

filter_vecs_t polygon_vecs = {
  polygon_init,
  polygon_process,
  polygon_deinit,
  NULL,
  polygon_args
};
#endif // FILTERS_ENABLED
