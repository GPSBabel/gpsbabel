/*

    validate internal data structures.

    Copyright (C) 2013 Robert Lipe   robertlipe+source@gpsbabel.org

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
#include "validate.h"
#include <cstdio>

#if FILTERS_ENABLED
#define MYNAME "validate"

void ValidateFilter::validate_head(const route_head*)
{
  head_ct += 1;
  segment_ct_start = point_ct;
}

void ValidateFilter::validate_head_trl(const route_head* header)
{
  int segment_waypt_ct = point_ct - segment_ct_start;
  if (debug) {
    fprintf(stderr, "%s %d ct: %d, waypt_count: %d\n", segment_type, header->rte_num,  segment_waypt_ct, header->rte_waypt_ct);
  }
  if (!debug && (segment_waypt_ct != header->rte_waypt_ct)) {
    fatal(MYNAME ":%s %d count mismatch, expected %d, actual %d\n", segment_type, header->rte_num, header->rte_waypt_ct, segment_waypt_ct);
  }
}

void ValidateFilter::validate_point(const Waypoint*)
{
  point_ct += 1;
}

void ValidateFilter::process()
{
  WayptFunctor<ValidateFilter> validate_point_f(this, &ValidateFilter::validate_point);
  RteHdFunctor<ValidateFilter> validate_head_f(this, &ValidateFilter::validate_head);
  RteHdFunctor<ValidateFilter> validate_head_trl_f(this, &ValidateFilter::validate_head_trl);

  debug = *opt_debug == '1';
  checkempty = *opt_checkempty == '1';

  point_ct = 0;
  if (debug) {
    fprintf(stderr, "\nProcessing waypts\n");
  }
  waypt_disp_all(validate_point_f);
  if (debug) {
    fprintf(stderr, "point ct: %u, waypt_count: %u\n", point_ct, waypt_count());
  }
  if (!debug && (point_ct != waypt_count())) {
    fatal(MYNAME ":Waypoint count mismatch, expected %u, actual %u\n", waypt_count(), point_ct);
  }

  head_ct = 0;
  point_ct = 0;
  segment_type = "route";
  if (debug) {
    fprintf(stderr, "\nProcessing routes\n");
  }
  route_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
  if (debug) {
    fprintf(stderr, "route head ct: %u, route_count: %u\n", head_ct, route_count());
    fprintf(stderr, "total route point ct: %u, route_waypt_count: %u\n", point_ct, route_waypt_count());
  }
  if (!debug && (head_ct != route_count())) {
    fatal(MYNAME ":Route count mismatch, expected %u, actual %u\n", route_count(), head_ct);
  }
  if (!debug && (point_ct != route_waypt_count())) {
    fatal(MYNAME ":Total route waypoint count mismatch, expected %u, actual %u\n", route_waypt_count(), point_ct);
  }

  head_ct = 0;
  point_ct = 0;
  segment_type = "track";
  if (debug) {
    fprintf(stderr, "\nProcessing tracks\n");
  }
  track_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
  if (debug) {
    fprintf(stderr, "track head ct: %u, track_count: %u\n", head_ct, track_count());
    fprintf(stderr, "total track point ct: %u, track_waypt_count: %u\n", point_ct, track_waypt_count());
  }
  if (!debug && (head_ct != track_count())) {
    fatal(MYNAME ":Track count mismatch, expected %u, actual %u\n", track_count(), head_ct);
  }
  if (!debug && (point_ct != track_waypt_count())) {
    fatal(MYNAME ":Total track waypoint count mismatch, expected %u, actual %u\n", track_waypt_count(), point_ct);
  }

  if (checkempty) {
    if (waypt_count()==0 && route_waypt_count()==0 && track_waypt_count()==0) {
      fatal(MYNAME ":No input\n");
    }
  }
}

#endif
