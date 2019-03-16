/*

    Track manipulation filter
    Copyright (c) 2009 - 2013 Robert Lipe, robertlipe+source@gpsbabel.org
    Copyright (C) 2005-2006 Olaf Klein, o.b.klein@gpsbabel.org

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
#ifndef TRACKFILTER_H_INCLUDED_
#define TRACKFILTER_H_INCLUDED_

#include <QtCore/QDateTime>  // for QDateTime
#include <QtCore/QList>      // for QList
#include <QtCore/QtGlobal>   // for qint64

#include "defs.h"            // for ARG_NOMINMAX, route_head (ptr only), ARG...
#include "filter.h"          // for Filter

#if FILTERS_ENABLED || MINIMAL_FILTERS

#define TRACKFILTER_PACK_OPTION		"pack"
#define TRACKFILTER_SPLIT_OPTION	"split"
#define TRACKFILTER_SDIST_OPTION	"sdistance"
#define TRACKFILTER_TITLE_OPTION	"title"
#define TRACKFILTER_MERGE_OPTION	"merge"
#define TRACKFILTER_NAME_OPTION		"name"
#define TRACKFILTER_STOP_OPTION		"stop"
#define TRACKFILTER_START_OPTION	"start"
#define TRACKFILTER_MOVE_OPTION		"move"
#define TRACKFILTER_FIX_OPTION          "fix"
#define TRACKFILTER_COURSE_OPTION       "course"
#define TRACKFILTER_SPEED_OPTION        "speed"
#define TRACKFILTER_SEG2TRK_OPTION      "seg2trk"
#define TRACKFILTER_TRK2SEG_OPTION      "trk2seg"
#define TRACKFILTER_SEGMENT_OPTION      "segment"
#define TRACKFILTER_FAKETIME_OPTION     "faketime"
#define TRACKFILTER_DISCARD_OPTION      "discard"
#define TRACKFILTER_MINPOINTS_OPTION    "minimum_points"

class TrackFilter:public Filter
{
public:
  arglist_t* get_args() override
  {
    return args;
  }
  void init() override;
  void process() override;
  void deinit() override;

private:
  char* opt_merge = nullptr;
  char* opt_pack = nullptr;
  char* opt_split = nullptr;
  char* opt_sdistance = nullptr;
  char* opt_move = nullptr;
  char* opt_title = nullptr;
  char* opt_start = nullptr;
  char* opt_stop = nullptr;
  char* opt_fix = nullptr;
  char* opt_course = nullptr;
  char* opt_speed = nullptr;
  char* opt_name = nullptr;
  char* opt_seg2trk = nullptr;
  char* opt_trk2seg = nullptr;
  char* opt_segment = nullptr;
  char* opt_faketime = nullptr;
  char* opt_discard = nullptr;
  char* opt_minpoints = nullptr;
  int minimum_points{0};

  arglist_t args[19] = {
    {
      TRACKFILTER_MOVE_OPTION, &opt_move,
      "Correct trackpoint timestamps by a delta", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_PACK_OPTION,  &opt_pack,
      "Pack all tracks into one", nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_SPLIT_OPTION, &opt_split,
      "Split by date or time interval (see README)", nullptr,
      ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_SDIST_OPTION, &opt_sdistance,
      "Split by distance", nullptr,
      ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_MERGE_OPTION, &opt_merge,
      "Merge multiple tracks for the same way", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_NAME_OPTION, &opt_name,
      "Use only track(s) where title matches given name", nullptr, ARGTYPE_STRING,
      ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_START_OPTION, &opt_start,
      "Use only track points after this timestamp", nullptr, ARGTYPE_INT,
      ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_STOP_OPTION, &opt_stop,
      "Use only track points before this timestamp", nullptr, ARGTYPE_INT,
      ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_TITLE_OPTION, &opt_title,
      "Basic title for new track(s)", nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_FIX_OPTION, &opt_fix,
      "Synthesize GPS fixes (PPS, DGPS, 3D, 2D, NONE)", nullptr,
      ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_COURSE_OPTION, &opt_course, "Synthesize course",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_SPEED_OPTION, &opt_speed, "Synthesize speed",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_SEG2TRK_OPTION, &opt_seg2trk,
      "Split track at segment boundaries into multiple tracks",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_TRK2SEG_OPTION, &opt_trk2seg,
      "Merge tracks inserting segment separators at boundaries",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_SEGMENT_OPTION, &opt_segment,
      "segment tracks with abnormally long gaps",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_FAKETIME_OPTION, &opt_faketime,
      "Add specified timestamp to each trackpoint",
      nullptr, ARGTYPE_STRING, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_DISCARD_OPTION,  &opt_discard,
      "Discard track points without timestamps during merge",
      nullptr, ARGTYPE_BOOL, ARG_NOMINMAX, nullptr
    },
    {
      TRACKFILTER_MINPOINTS_OPTION, &opt_minpoints,
      "Discard tracks with fewer than these points",
      nullptr, ARGTYPE_INT, "0", "50", nullptr
    },
    ARG_TERMINATOR
  };

  QList<route_head*> track_list;
  int opt_interval = 0;
  int opt_distance = 0;
  bool need_time;		/* initialized within trackfilter_init */

  int trackfilter_opt_count();
  qint64 trackfilter_parse_time_opt(const char* arg);
  static bool trackfilter_init_sort_cb(const route_head* ha, const route_head* hb);
  static bool trackfilter_merge_sort_cb(const Waypoint* wa, const Waypoint* wb);
  fix_type trackfilter_parse_fix(int* nsats);
  static QDateTime trackfilter_get_first_time(const route_head* track);
  static QDateTime trackfilter_get_last_time(const route_head* track);
  void trackfilter_fill_track_list_cb(const route_head* track); 	/* callback for track_disp_all */
  void trackfilter_minpoint_list_cb(const route_head* track);

  void trackfilter_split_init_rte_name(route_head* track, const QDateTime& dt);
  void trackfilter_pack_init_rte_name(route_head* track, const QDateTime& default_time);

  void trackfilter_title();

  void trackfilter_pack();

  void trackfilter_merge();

  void trackfilter_split();

  void trackfilter_move();

  void trackfilter_synth();

  QDateTime trackfilter_range_check(const char* timestr);
  void trackfilter_range();

  void trackfilter_seg2trk();

  void trackfilter_trk2seg();

  struct faketime_t {
    QDateTime start;
    int    step{0};
    bool   force{false};
  };

  faketime_t trackfilter_faketime_check(const char* timestr);
  void trackfilter_faketime();             /* returns number of track points left after filtering */
  bool trackfilter_points_are_same(const Waypoint* wpta, const Waypoint* wptb);

  void trackfilter_segment_head(const route_head* rte);

};

#endif // FILTERS_ENABLED
#endif // TRACKFILTER_H_INCLUDED_
