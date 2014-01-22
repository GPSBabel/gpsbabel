/*

    validate internal data structures.

    Copyright (C) 2013 Robert Lipe   robertlipe@usa.net

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
#define MYNAME "validate"

static char* opt_debug;
bool debug;
static char* opt_checkempty;
bool checkempty;
static unsigned int point_ct;
static unsigned int head_ct;
static unsigned int segment_ct_start;
static const char* segment_type;

static
arglist_t validate_args[] = {
  {
    "checkempty", &opt_checkempty, "Check for empty input",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX
  } ,
  {
    "debug", &opt_debug, "Output debug messages instead of possibly issuing a fatal error",
    "0", ARGTYPE_BOOL, ARG_NOMINMAX
  } ,
  ARG_TERMINATOR
};


static void validate_head(const route_head* header)
{
  head_ct += 1;
  segment_ct_start = point_ct;
}

static void validate_head_trl(const route_head* header)
{
  int segment_waypt_ct = point_ct - segment_ct_start;
  if (debug) {
    fprintf(stderr, "%s %d ct: %d, waypt_count: %d\n", segment_type, header->rte_num,  segment_waypt_ct, header->rte_waypt_ct);
  }
  if (!debug && (segment_waypt_ct != header->rte_waypt_ct)) {
    fatal(MYNAME ":%s %d count mismatch, expected %d, actual %d\n", segment_type, header->rte_num, header->rte_waypt_ct, segment_waypt_ct);
  }
}

static void validate_point(const Waypoint* waypointp)
{
  point_ct += 1;
}

static void
validate_process(void)
{

  debug = *opt_debug == '1';
  checkempty = *opt_checkempty == '1';

  point_ct = 0;
  if (debug) {
    fprintf(stderr, "\nProcessing waypts\n");
  }
  waypt_disp_all(validate_point);
  if (debug) {
    fprintf(stderr, "point ct: %d, waypt_count: %d\n", point_ct, waypt_count());
  }
  if (!debug && (point_ct != waypt_count())) {
    fatal(MYNAME ":Waypoint count mismatch, expected %d, actual %d\n", waypt_count(), point_ct);
  }

  head_ct = 0;
  point_ct = 0;
  segment_type = "route";
  if (debug) {
    fprintf(stderr, "\nProcessing routes\n");
  }
  route_disp_all(validate_head, validate_head_trl, validate_point);
  if (debug) {
    fprintf(stderr, "route head ct: %d, route_count: %d\n", head_ct, route_count());
    fprintf(stderr, "total route point ct: %d, route_waypt_count: %d\n", point_ct, route_waypt_count());
  }
  if (!debug && (head_ct != route_count())) {
    fatal(MYNAME ":Route count mismatch, expected %d, actual %d\n", route_count(), head_ct);
  }
  if (!debug && (point_ct != route_waypt_count())) {
    fatal(MYNAME ":Total route waypoint count mismatch, expected %d, actual %d\n", route_waypt_count(), point_ct);
  }

  head_ct = 0;
  point_ct = 0;
  segment_type = "track";
  if (debug) {
    fprintf(stderr, "\nProcessing tracks\n");
  }
  track_disp_all(validate_head, validate_head_trl, validate_point);
  if (debug) {
    fprintf(stderr, "track head ct: %d, track_count: %d\n", head_ct, track_count());
    fprintf(stderr, "total track point ct: %d, track_waypt_count: %d\n", point_ct, track_waypt_count());
  }
  if (!debug && (head_ct != track_count())) {
    fatal(MYNAME ":Track count mismatch, expected %d, actual %d\n", track_count(), head_ct);
  }
  if (!debug && (point_ct != track_waypt_count())) {
    fatal(MYNAME ":Total track waypoint count mismatch, expected %d, actual %d\n", track_waypt_count(), point_ct);
  }

  if (checkempty) {
    if (waypt_count()==0 && route_waypt_count()==0 && track_waypt_count()==0) {
      fatal(MYNAME ":No input\n");
    }
  }
}

filter_vecs_t validate_vecs = {
  NULL,
  validate_process,
  NULL,
  NULL,
  validate_args
};

#endif
