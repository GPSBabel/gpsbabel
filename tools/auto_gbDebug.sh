#!/bin/bash -e

# script to change gbDebug (printf like) usage to gbLogFatal (std::format)

#gbDebug("parse of string '%s' on line number %d as double failed.\n");
#gbDebug("parse of string '%s' on line number %d as time_t failed.\n",
for file in *.cc
do
# be wimpy, require semicolon or a comma at end of line.
  sed -E -i '/gbDebug\(".*[;,]$/s/\\n"/"/' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%s/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%d/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%ld/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%lld/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%i/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%u/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%zu/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%llu/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%c/{}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%f/{:.6f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%g/{:g}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%" PRId64 "/{}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/0x%0\*X/0x{:0{}X-REORDER-}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%(0|\.)([1-9][0-9]*)([xX])/{:0\2\3}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%([1-9][0-9]*)([xX])/{:\1\2}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%([xX])/{:\1}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%([1-9][0-9]*)[diu]/{:\1}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%0([1-9][0-9]*)[diu]/{:0\1}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%([1-9][0-9]*)\.([1-9][0-9]*)s/{:>\1.\2}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%([1-9][0-9]*)s/{:>\1}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%\.([1-9][0-9]*)s/{:.\1}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%\.f/{:.0f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%\+f/{:+f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%\.([0-9]+)f/{:.\1f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%\+\.([0-9]+)f/{:+.\1f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%([1-9][0-9]*)\.([0-9]+)f/{:>\1.\2f}/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/%\+([1-9][0-9]*)\.([0-9]+)f/{:>+\1.\2f}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%\+#g/{:+#g}/g' "$file"

  sed -E -i '/gbDebug\(".*[;,]$/s/%%/%/g' "$file"
  sed -E -i '/gbDebug\(".*[;,]$/s/gbDebug/gbLogDebug/' "$file"
done
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
patch <<"EOJ"
--- duplicate.cc	2026-10-03 19:41:56.873947987 -0600
+++ bakfout/duplicate.cc	2026-10-03 19:40:13.581531239 -0600
@@ -27,6 +27,7 @@
 #include <QMultiHash>            // for QMultiHash
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- gbser_posix.cc	2026-10-03 19:41:57.261529088 -0600
+++ bakfout/gbser_posix.cc	2026-10-03 19:40:13.581567850 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <cassert>
 #include <cerrno>
--- height.cc	2026-10-03 19:41:57.593127909 -0600
+++ bakfout/height.cc	2026-10-03 19:40:13.581594452 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "height.h"
+#include "src/core/logging.h"
 #include <cmath>    // for floor
 #include <cstdint>  // for int8_t
 
--- mkshort.cc	2026-10-03 19:41:57.855704459 -0600
+++ bakfout/mkshort.cc	2026-10-03 19:40:13.581613360 -0600
@@ -33,6 +33,7 @@
 
 #include "defs.h"
 #include "geocache.h"  // for Geocache
+#include "src/core/logging.h"
 
 
 const QByteArray MakeShort::vowels = "aeiouAEIOU";
--- parse.cc	2026-10-03 19:41:58.048427145 -0600
+++ bakfout/parse.cc	2026-10-03 19:40:13.581639618 -0600
@@ -32,6 +32,7 @@
 #include "parse.h"                 // for parse_double, parse_integer
 #include "defs.h"                  // for gbFatal, grid_type, KPH_TO_MPS, MPH_TO_MPS, gbWarning, FEET_TO_METERS, KNOTS_TO_MPS, kDatumWGS84, FATHOMS_TO_METERS, MILES_TO_METERS, NMILES_TO_METERS, parse_coordinates, CSTR, parse_distance, parse_speed
 #include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_UTM_EN_To_Known_Datum
+#include "src/core/logging.h"
 
 
 /*
--- position.cc	2026-10-03 19:41:58.095780655 -0600
+++ bakfout/position.cc	2026-10-03 19:40:13.581882349 -0600
@@ -30,6 +30,7 @@
 #include "defs.h"
 #include "grtcirc.h"            // for gcdist, radtometers
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
--- radius.cc	2026-10-03 19:41:58.140903399 -0600
+++ bakfout/radius.cc	2026-10-03 19:40:13.581817658 -0600
@@ -28,6 +28,7 @@
 
 #include "defs.h"           // for Waypoint, del_marked_wpts, route_add_head, route_add_wpt, waypt_add, waypt_sort, waypt_swap, route_head, WaypointList, kMilesPerKilometer
 #include "grtcirc.h"         // for gcdist, radtomiles
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- reverse_route.cc	2026-10-03 19:41:58.206171496 -0600
+++ bakfout/reverse_route.cc	2026-10-03 19:40:13.581680855 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "reverse_route.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
--- session.cc	2026-10-03 19:41:58.273800921 -0600
+++ bakfout/session.cc	2026-10-03 19:40:13.581724400 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "session.h"
+#include "src/core/logging.h"
 
 #include <QList>         // for QList
 
--- smplrout.cc	2026-10-03 19:41:58.368444572 -0600
+++ bakfout/smplrout.cc	2026-10-03 19:40:13.581837300 -0600
@@ -67,6 +67,7 @@
 #include "smplrout.h"
 #include "grtcirc.h"            // for gcdist, linedist, radtometers, linepart
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- sort.cc	2026-10-03 19:41:58.392014240 -0600
+++ bakfout/sort.cc	2026-10-03 19:40:13.581861155 -0600
@@ -27,6 +27,7 @@
 #include "defs.h"
 #include "geocache.h"           // for Geocache
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- stackfilter.cc	2026-10-03 19:41:58.415068631 -0600
+++ bakfout/stackfilter.cc	2026-10-03 19:40:13.581799549 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "stackfilter.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
--- transform.cc	2026-10-03 19:41:58.596911677 -0600
+++ bakfout/transform.cc	2026-10-03 19:40:13.581779435 -0600
@@ -26,6 +26,7 @@
 
 #include "defs.h"
 #include "transform.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- units.cc	2026-10-03 19:41:58.647203644 -0600
+++ bakfout/units.cc	2026-10-03 19:40:13.581743080 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "units.h"
+#include "src/core/logging.h"
 
 
 void
--- validate.cc	2026-10-03 19:41:58.729947955 -0600
+++ bakfout/validate.cc	2026-10-03 19:40:13.581760585 -0600
@@ -22,6 +22,7 @@
 
 #include "defs.h"
 #include "validate.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
--- rgbcolors.cc	2026-10-03 19:45:07.501767410 -0600
+++ bakfout/rgbcolors.cc	2026-10-03 19:46:52.469837815 -0600
@@ -28,6 +28,7 @@
 #include <QtGlobal>            // for qPrintable
 
 #include "defs.h"              // for gbFatal, color_to_bbggrr
+#include "src/core/logging.h"
 
 /*
  * Colors derived from http://www.w3.org/TR/SVG/types.html#ColorKeywords
--- before/garmin_txt.cc	2026-10-04 08:34:19.645136333 -0600
+++ after/garmin_txt.cc	2026-10-04 08:34:57.040547347 -0600
@@ -963,7 +963,7 @@
       int field_no = field_idx + 1;
       header_mapping_info[ht].append(std::make_pair(name, field_no));
       if (global_opts.debug_level >= 2) {
-        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, ht, i);
+        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, static_cast<int>(ht), i);
       }
     } else {
       gbWarning("Field %s not recognized!\n", gbLogCStr(name));
--- before/exif.cc	2026-10-04 08:37:51.150240980 -0600
+++ after/exif.cc	2026-10-04 08:38:50.175216012 -0600
@@ -552,7 +552,7 @@
           } else if (tag->type == EXIF_TYPE_DOUBLE) {
             gbLogDebug(" {:+#g}", tag->data.at(idx).value<double>());
           } else {
-            gbLogDebug(" 0x{:0{}X-REORDER-}", 2 * exif_type_size(tag->type), tag->data.at(idx).value<uint32_t>());
+            gbLogDebug(" 0x{:0{}X}", tag->data.at(idx).value<uint32_t>(), 2 * exif_type_size(tag->type));
           }
         }
         if (tag->count > 4) {
--- before/validate.cc	2026-10-04 08:45:48.562320697 -0600
+++ after/validate.cc	2026-10-04 08:47:17.108141503 -0600
@@ -61,7 +61,8 @@
 
   point_ct = 0;
   if (opt_debug) {
-    gbLogDebug("\nProcessing waypts");
+    gbLogDebug("");
+    gbLogDebug("Processing waypts");
   }
   waypt_disp_all(validate_point_f);
   if (opt_debug) {
@@ -76,7 +77,8 @@
   total_segment_ct = 0;
   segment_type = "route";
   if (opt_debug) {
-    gbLogDebug("\nProcessing routes");
+    gbLogDebug("");
+    gbLogDebug("Processing routes");
   }
   route_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
   if (opt_debug) {
@@ -95,7 +97,8 @@
   total_segment_ct = 0;
   segment_type = "track";
   if (opt_debug) {
-    gbLogDebug("\nProcessing tracks");
+    gbLogDebug("");
+    gbLogDebug("Processing tracks");
   }
   track_disp_all(validate_head_f, validate_head_trl_f, validate_point_f);
   if (opt_debug) {
EOJ
grep gbLogDebug *.cc | grep %
grep gbLogDebug *.cc | grep '\\n'
