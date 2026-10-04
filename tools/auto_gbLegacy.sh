#!/bin/bash -e

# script to change gbDebug (printf like) usage to gbLogFatal (std::format)

#gbDebug("parse of string '%s' on line number %d as double failed.\n");
#gbDebug("parse of string '%s' on line number %d as time_t failed.\n",
for file in *.cc
do
# be careful, require ); or a \n", at end of line.
#  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/\\n"/"/' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%s/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%d/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%ld/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%lld/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%i/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%u/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%zu/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%llu/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%c/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%f/{:.6f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%g/{:g}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%" PRId64 "/{}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/0x%0\*X/0x{:0{}X-REORDER-}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%(0|\.)([1-9][0-9]*)([xX])/{:0\2\3}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([1-9][0-9]*)([xX])/{:\1\2}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([xX])/{:\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([1-9][0-9]*)[diu]/{:\1}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%0([1-9][0-9]*)[diu]/{:0\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([1-9][0-9]*)\.([1-9][0-9]*)s/{:>\1.\2}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([1-9][0-9]*)s/{:>\1}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\.([1-9][0-9]*)s/{:.\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\.f/{:.0f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\+f/{:+f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\.([0-9]+)f/{:.\1f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\+\.([0-9]+)f/{:+.\1f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%([1-9][0-9]*)\.([0-9]+)f/{:>\1.\2f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\+([1-9][0-9]*)\.([0-9]+)f/{:>+\1.\2f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%0\.([0-9]+)f/{:.\1f}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%\+#g/{:+#g}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/%%/%/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/gb(Debug|Fatal|Warning|Info)/gbLog\1/' "$file"
  sed -E -i '/gbLog(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/\\n"/"/' "$file"
done
rm -rf before
rm -rf after
mkdir before
mkdir after
cp *.cc before
# gbDebug patches

# fix the rest by hand:
# duplicate.cc needs logging include
# gbser_posic.cc needs logging include
# height.cc needs logging include
# mkshort.cc needs logging include
# parse.cc needs logging include
# reverse_route.cc needs logging include
# rgbcolors.cc needs logging include
# session.cc needs logging include
# units.cc needs logging include
# validate.cc needs logging include
# transform.cc needs logging include
# stackfilter.cc needs logging include
# radius.cc needs logging include
# smplrout.cc needs logging include
# sort.cc needs logging include
# position.cc needs logging include
# garmin_txt.cc 966 enum
# exif.cc 555 reorder

# gbWarning patches

# fix the rest by hand:
# parse.cc needs logging include
# unicsv.cc 425 is an enum
# mtk_logger.cc 509 is multiline
# shape.cc 152 enum
# gbser_posix.cc needs logging include
# gtm.cc 386 multiline literal, multiline
# stackfilter.cc 147 multiline literal, needs logging include
# trackfilter.cc 591 multiline literal

# gbFatal patches
#
# fix the rest by hand:
# duplicate.cc needs logging include
# gbser_posic.cc needs logging include
# height.cc needs logging include
# mkshort.cc needs logging include
# parse.cc needs logging include
# reverse_route.cc needs logging include
# rgbcolors.cc needs logging include
# session.cc needs logging include
# units.cc needs logging include
# validate.cc needs logging include
# transform.cc needs logging include
# stackfilter.cc needs logging include
# radius.cc needs logging include
# smplrout.cc needs logging include
# sort.cc needs logging include
# position.cc
# xcsv.cc 812 enum
# igc.cc 157, 167, 183, 227, 290, 306, 315, 353, 364, 443 multiline
echo "igc.cc has multiline gbLogFatal calls"

patch <<"EOJ"
diff -u before/duplicate.cc after/duplicate.cc
--- before/duplicate.cc	2026-10-04 10:06:13.345522952 -0600
+++ after/duplicate.cc	2026-10-04 10:06:13.355283219 -0600
@@ -27,6 +27,7 @@
 #include <QMultiHash>            // for QMultiHash
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -u before/garmin_txt.cc after/garmin_txt.cc
--- before/garmin_txt.cc	2026-10-04 10:06:13.345714496 -0600
+++ after/garmin_txt.cc	2026-10-04 10:06:13.355530013 -0600
@@ -963,7 +963,7 @@
       int field_no = field_idx + 1;
       header_mapping_info[ht].append(std::make_pair(name, field_no));
       if (global_opts.debug_level >= 2) {
-        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, ht, i);
+        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, static_cast<int>(ht), i);
       }
     } else {
       gbLogWarning("Field {} not recognized!", gbLogCStr(name));
diff -u before/gbser_posix.cc after/gbser_posix.cc
--- before/gbser_posix.cc	2026-10-04 10:06:13.345776990 -0600
+++ after/gbser_posix.cc	2026-10-04 10:06:13.355597239 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <cassert>
 #include <cerrno>
diff -u before/height.cc after/height.cc
--- before/height.cc	2026-10-04 10:06:13.346032876 -0600
+++ after/height.cc	2026-10-04 10:06:13.355841759 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "height.h"
+#include "src/core/logging.h"
 #include <cmath>    // for floor
 #include <cstdint>  // for int8_t
 
diff -u before/mkshort.cc after/mkshort.cc
--- before/mkshort.cc	2026-10-04 10:06:13.346244696 -0600
+++ after/mkshort.cc	2026-10-04 10:06:13.356049027 -0600
@@ -33,6 +33,7 @@
 
 #include "defs.h"
 #include "geocache.h"  // for Geocache
+#include "src/core/logging.h"
 
 
 const QByteArray MakeShort::vowels = "aeiouAEIOU";
diff -u before/parse.cc after/parse.cc
--- before/parse.cc	2026-10-04 10:06:13.346371586 -0600
+++ after/parse.cc	2026-10-04 10:06:13.356174632 -0600
@@ -32,6 +32,7 @@
 #include "parse.h"                 // for parse_double, parse_integer
 #include "defs.h"                  // for gbFatal, grid_type, KPH_TO_MPS, MPH_TO_MPS, gbWarning, FEET_TO_METERS, KNOTS_TO_MPS, kDatumWGS84, FATHOMS_TO_METERS, MILES_TO_METERS, NMILES_TO_METERS, parse_coordinates, CSTR, parse_distance, parse_speed
 #include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_UTM_EN_To_Known_Datum
+#include "src/core/logging.h"
 
 
 /*
diff -u before/position.cc after/position.cc
--- before/position.cc	2026-10-04 10:06:13.346406071 -0600
+++ after/position.cc	2026-10-04 10:06:13.356208937 -0600
@@ -30,6 +30,7 @@
 #include "defs.h"
 #include "grtcirc.h"            // for gcdist, radtometers
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -u before/radius.cc after/radius.cc
--- before/radius.cc	2026-10-04 10:06:13.346460571 -0600
+++ after/radius.cc	2026-10-04 10:06:13.356239058 -0600
@@ -28,6 +28,7 @@
 
 #include "defs.h"           // for Waypoint, del_marked_wpts, route_add_head, route_add_wpt, waypt_add, waypt_sort, waypt_swap, route_head, WaypointList, kMilesPerKilometer
 #include "grtcirc.h"         // for gcdist, radtomiles
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -u before/reverse_route.cc after/reverse_route.cc
--- before/reverse_route.cc	2026-10-04 10:06:13.346503925 -0600
+++ after/reverse_route.cc	2026-10-04 10:06:13.356279304 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "reverse_route.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -u before/rgbcolors.cc after/rgbcolors.cc
--- before/rgbcolors.cc	2026-10-04 10:06:13.346517202 -0600
+++ after/rgbcolors.cc	2026-10-04 10:06:13.356291937 -0600
@@ -28,6 +28,7 @@
 #include <QtGlobal>            // for qPrintable
 
 #include "defs.h"              // for gbFatal, color_to_bbggrr
+#include "src/core/logging.h"
 
 /*
  * Colors derived from http://www.w3.org/TR/SVG/types.html#ColorKeywords
diff -u before/session.cc after/session.cc
--- before/session.cc	2026-10-04 10:06:13.346570089 -0600
+++ after/session.cc	2026-10-04 10:06:13.356322816 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "session.h"
+#include "src/core/logging.h"
 
 #include <QList>         // for QList
 
diff -u before/shape.cc after/shape.cc
--- before/shape.cc	2026-10-04 10:06:13.346598510 -0600
+++ after/shape.cc	2026-10-04 10:06:13.356348316 -0600
@@ -149,7 +149,7 @@
   const int nFields = DBFGetFieldCount(ihandledb);
   for (int i = 0; i < nFields; i++) {
     DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
-    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}", i, name, type);
+    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}", i, name, static_cast<int>(type));
   }
   gbLogFatal("");
 }
diff -u before/smplrout.cc after/smplrout.cc
--- before/smplrout.cc	2026-10-04 10:06:13.346639640 -0600
+++ after/smplrout.cc	2026-10-04 10:06:13.356389681 -0600
@@ -67,6 +67,7 @@
 #include "smplrout.h"
 #include "grtcirc.h"            // for gcdist, linedist, radtometers, linepart
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -u before/sort.cc after/sort.cc
--- before/sort.cc	2026-10-04 10:06:13.346655646 -0600
+++ after/sort.cc	2026-10-04 10:06:13.356405202 -0600
@@ -27,6 +27,7 @@
 #include "defs.h"
 #include "geocache.h"           // for Geocache
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -u before/stackfilter.cc after/stackfilter.cc
--- before/stackfilter.cc	2026-10-04 10:06:13.346670210 -0600
+++ after/stackfilter.cc	2026-10-04 10:06:13.356434669 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "stackfilter.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -u before/transform.cc after/transform.cc
--- before/transform.cc	2026-10-04 10:06:13.346787378 -0600
+++ after/transform.cc	2026-10-04 10:06:13.356561046 -0600
@@ -26,6 +26,7 @@
 
 #include "defs.h"
 #include "transform.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -u before/unicsv.cc after/unicsv.cc
--- before/unicsv.cc	2026-10-04 10:06:13.346804280 -0600
+++ after/unicsv.cc	2026-10-04 10:06:13.356577417 -0600
@@ -422,7 +422,7 @@
       unicsv_fields_tab.last() = f.type;
 
       if (global_opts.debug_level) {
-        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), f.type);
+        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), static_cast<int>(f.type));
       }
 
       /* handle some special items */
diff -u before/units.cc after/units.cc
--- before/units.cc	2026-10-04 10:06:13.346829243 -0600
+++ after/units.cc	2026-10-04 10:06:13.356603390 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "units.h"
+#include "src/core/logging.h"
 
 
 void
diff -u before/validate.cc after/validate.cc
--- before/validate.cc	2026-10-04 10:06:13.346881411 -0600
+++ after/validate.cc	2026-10-04 10:06:13.356670612 -0600
@@ -22,6 +22,7 @@
 
 #include "defs.h"
 #include "validate.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
@@ -60,7 +61,8 @@
 
   point_ct = 0;
   if (opt_debug) {
-    gbLogDebug("\nProcessing waypts");
+    gbLogDebug("");
+    gbLogDebug("Processing waypts");
   }
   waypt_disp_all(validate_point_f);
   if (opt_debug) {
@@ -75,7 +77,8 @@
   total_segment_ct = 0;
   segment_type = "route";
   if (opt_debug) {
-    gbLogDebug("\nProcessing routes");
+    gbLogDebug("");
+    gbLogDebug("Processing routes");
   }
   route_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
   if (opt_debug) {
@@ -94,7 +97,8 @@
   total_segment_ct = 0;
   segment_type = "track";
   if (opt_debug) {
-    gbLogDebug("\nProcessing tracks");
+    gbLogDebug("");
+    gbLogDebug("Processing tracks");
   }
   track_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
   if (opt_debug) {
diff -u before/xcsv.cc after/xcsv.cc
--- before/xcsv.cc	2026-10-04 10:06:13.346996764 -0600
+++ after/xcsv.cc	2026-10-04 10:06:13.356798825 -0600
@@ -809,7 +809,7 @@
     break;
 
   default:
-    gbLogFatal("Unknown style directive: {} - {}", fmp.key.constData(), fmp.hashed_key);
+    gbLogFatal("Unknown style directive: {} - {}", fmp.key.constData(), static_cast<int>(fmp.hashed_key));
     break;
   }
 }
EOJ

# a couple that didn't match our replacement search pattern
patch <<"EOJ"
--- before/garmin_gpi.cc	2026-10-04 11:19:44.306571026 -0600
+++ after/garmin_gpi.cc	2026-10-04 11:20:14.071541333 -0600
@@ -59,7 +59,8 @@
 #define GPI_BITMAP_SIZE sizeof(gpi_bitmap)
 
 #define GPI_DBG global_opts.debug_level >= 3
-#define PP if (GPI_DBG) gbDebug("@%6x (%8d): ", gbftell(fin), gbftell(fin))
+#define PP if (GPI_DBG) gbLogDebug("@{:6x} ({:8}): ", gbftell(fin), gbftell(fin))
+
 
 /*******************************************************************************
 * %%%                             gpi reader                               %%% *
--- before/lowranceusr.cc	2026-10-04 11:19:44.307110293 -0600
+++ after/lowranceusr.cc	2026-10-04 11:27:05.898826439 -0600
@@ -632,11 +632,11 @@
       gbLogDebug(" {:08x} {:>8.3f} {:08x} {:08x} {:08x}",
              unused_byte, fsdata->depth, loran_GRI, loran_Tda, loran_Tdb);
     } else {
-      gbDebug("parse_waypoints: version = %d, name = %s, uid_unit = %u, "
-             "uid_seq_low = %d, uid_seq_high = %d, lat = %+.10f, lon = %+.10f, depth = %f\n",
-             waypoint_version, gbLogCStr(wpt_tmp->shortname), fsdata->uid_unit,
-             fsdata->uid_seq_low, fsdata->uid_seq_high,
-             wpt_tmp->longitude, wpt_tmp->latitude, fsdata->depth);
+      gbLogDebug("parse_waypoints: version = {}, name = {}, uid_unit = {},"
+                 "uid_seq_low = {}, uid_seq_high = {}, lat = {:+.10f}, lon = {:+.10f}, depth = {:.6f}",
+                  waypoint_version, gbLogCStr(wpt_tmp->shortname), fsdata->uid_unit,
+                  fsdata->uid_seq_low, fsdata->uid_seq_high,
+                  wpt_tmp->longitude, wpt_tmp->latitude, fsdata->depth);
     }
   }
 }
EOJ
cp *.cc after
echo "++++++++++ possible untranslatd print specifier ++++++++++"
grep -n gbLogDebug *.cc | grep % || true
grep -n gbLogFatal *.cc | grep % || true
grep -n gbLogWarning *.cc | grep % || true
grep -n gbLogInfo *.cc | grep % || true
echo "++++++++++ possible embedded newline, will not print identically ++++++++++"
grep -n gbLogDebug *.cc | grep '\\n' || true
grep -n gbLogFatal *.cc | grep '\\n' || true
grep -n gbLogWarning *.cc | grep '\\n' || true
grep -n gbLogInfo *.cc | grep '\\n' || true
echo "++++++++++ possible missed conversions ++++++++++"
grep -n gbDebug *.cc | grep -v \#include || true
grep -n gbFatal *.cc | grep -v FatalMsg\(\) | grep -v \#include || true
grep -n gbWarning *.cc | grep -v \#include || true
grep -n gbInfo *.cc | grep -v \#include || true
