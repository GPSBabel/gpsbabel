#!/bin/bash -e

# script to change gbDebug (printf like) usage to gbLogFatal (std::format)

#gbDebug("parse of string '%s' on line number %d as double failed.\n");
#gbDebug("parse of string '%s' on line number %d as time_t failed.\n",
for file in *.cc
do
# be wimpy, require semicolon or a comma at end of line.
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/\\n"/"/' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%s/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%d/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%ld/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%lld/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%i/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%u/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%zu/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%llu/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%c/{}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%f/{:.6f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%g/{:g}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%" PRId64 "/{}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/0x%0\*X/0x{:0{}X-REORDER-}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%(0|\.)([1-9][0-9]*)([xX])/{:0\2\3}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([1-9][0-9]*)([xX])/{:\1\2}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([xX])/{:\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([1-9][0-9]*)[diu]/{:\1}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%0([1-9][0-9]*)[diu]/{:0\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([1-9][0-9]*)\.([1-9][0-9]*)s/{:>\1.\2}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([1-9][0-9]*)s/{:>\1}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\.([1-9][0-9]*)s/{:.\1}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\.f/{:.0f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\+f/{:+f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\.([0-9]+)f/{:.\1f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\+\.([0-9]+)f/{:+.\1f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%([1-9][0-9]*)\.([0-9]+)f/{:>\1.\2f}/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\+([1-9][0-9]*)\.([0-9]+)f/{:>+\1.\2f}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%\+#g/{:+#g}/g' "$file"

  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/%%/%/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning)\(".*[;,]$/s/gb(Debug|Fatal|Warning)/gbLog\1/' "$file"
done
rm -rf before
rm -rf after
mkdir before
mkdir after
cp *.cc before
# gbDebug patches
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
--- before/garmin_txt.cc	2026-10-04 09:03:15.004629269 -0600
+++ after/garmin_txt.cc	2026-10-04 09:03:15.013748729 -0600
@@ -963,7 +963,7 @@
       int field_no = field_idx + 1;
       header_mapping_info[ht].append(std::make_pair(name, field_no));
       if (global_opts.debug_level >= 2) {
-        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, ht, i);
+        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})", gbLogCStr(name), field_no, static_cast<int>(ht), i);
       }
     } else {
       gbLogWarning("Field {} not recognized!", gbLogCStr(name));
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
# gbWarning patches

# fix the rest by hand:
# parse.cc needs logging include (already done)
# unicsv.cc 425 is an enum
# mtk_logger.cc 509 is multiline
# shape.cc 152 enum
# gbser_posix.cc needs logging include (already done)
# gtm.cc 386 mutliline literal, multiline
# stackfilter.cc 147 multline literal, needs logging include (logging done)
# trackfilter.cc 591 multiline literal
patch <<"EOJ"
--- unicsv.cc	2026-10-03 18:29:33.336454392 -0600
+++ bak/unicsv.cc	2026-10-03 18:26:02.803105731 -0600
@@ -422,7 +422,7 @@
       unicsv_fields_tab.last() = f.type;
 
       if (global_opts.debug_level) {
-        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), f.type);
+        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), static_cast<int>(f.type));
       }
 
       /* handle some special items */
--- before/mtk_logger.cc	2026-10-04 09:13:56.684095684 -0600
+++ after/mtk_logger.cc	2026-10-04 09:22:24.093250813 -0600
@@ -506,7 +506,8 @@
             }
           } else {
             if (null_len == chunk_size) {  // 0x00 block - bad block....
-              gbLogWarning("FIXME -- read bad block at 0x{:06x} - retry ? skip ?\n{}", data_addr, line);
+              gbLogWarning("FIXME -- read bad block at 0x{:06x} - retry ? skip ?", data_addr);
+              gbLogWarning("{}", line);
             }
             if (ff_len == chunk_size) {  // 0xff block - read complete...
               len = ff_len;
--- before/shape.cc	2026-10-04 09:13:56.684368758 -0600
+++ after/shape.cc	2026-10-04 09:19:33.403167063 -0600
@@ -149,7 +149,7 @@
   const int nFields = DBFGetFieldCount(ihandledb);
   for (int i = 0; i < nFields; i++) {
     DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
-    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}", i, name, type);
+    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}", i, name, static_cast<int>(type));
   }
   gbLogFatal("");
 }
--- gtm.cc	2026-10-03 18:29:32.760848043 -0600
+++ bak/gtm.cc	2026-10-03 18:26:20.163148845 -0600
@@ -383,10 +383,10 @@
   //       If ts_count != real_track_list.size() we don't know how to line up
   //       the tracklogs, and the real tracks, with the tracklog styles.
   if (ts_count != real_track_list.size()) {
-    gbWarning("The number of tracklog entries with the new flag "
-           "set doesn't match the number of tracklog style entries.\n"
-           "  This is unexpected and may indicate a malformed input file.\n"
-           "  As a result the track names may be incorrect.\n");
+    gbLogWarning("The number of tracklog entries with the new flag "
+              "set doesn't match the number of tracklog style entries.");
+    gbLogWarning("  This is unexpected and may indicate a malformed input file.");
+    gbLogWarning("  As a result the track names may be incorrect.");
   }
   // Read the entire tracklog styles section whether we use it or not.
   for (i = 0; i != ts_count; i++) {
--- stackfilter.cc	2026-10-03 18:29:33.231489448 -0600
+++ bak/stackfilter.cc	2026-10-03 18:26:23.769212144 -0600
@@ -144,8 +145,7 @@
   stack_elt* tmp_elt = nullptr;
 
   if (warnings_enabled && stack) {
-    gbWarning("Warning: leftover stack entries; "
-            "check command line for mistakes\n");
+    gbLogWarning("Warning: leftover stack entries; check command line for mistakes");
   }
   while (stack) {
     stack->waypts.flush();
--- trackfilter.cc	2026-10-03 18:29:33.306618796 -0600
+++ bak/trackfilter.cc	2026-10-03 18:26:26.886529745 -0600
@@ -588,8 +588,8 @@
     }
   }
   if (timeless_points > 0) {
-    gbWarning("move: %d points out of %d total points didn't have "
-            "time information and could not be moved.\n",
+    gbLogWarning("move: {} points out of {} total points didn't have "
+            "time information and could not be moved.",
             timeless_points, track_waypt_count());
   }
 }
EOJ
# gbFatal
#
# fix the rest by hand:
# duplicate.cc needs logging include
# gbser_posic.cc needs logging include
# height.cc needs logging include
# mkshort.cc
# parse.cc needs logging include
# reverse_route.cc
# rgbcolors.cc
# session.cc
# units.cc
# validate.cc
# transform.cc
# stackfilter.cc
# radius.cc
# smplrout.cc
# sort.cc
# position.cc needs logging include
# xcsv.cc 812 enum
# igc.cc 10 lines with newlines 157, 167, 183, 227, 290, 306, 315, 353, 364, 443
echo "igc.cc has multiline gbLogFatal calls"
patch --verbose <<"EOJ"
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
# igc.cc 10 lines with newlines 157, 167, 183, 227, 290, 306, 315, 353, 364, 443
echo "igc.cc has multiline gbLogFatal calls"
patch <<"EOJ"
--- xcsv.cc	2026-10-03 19:41:58.875778694 -0600
+++ bakfout/xcsv.cc	2026-10-03 19:40:13.581903507 -0600
@@ -809,7 +809,7 @@
     break;
 
   default:
-    gbLogFatal("Unknown style directive: {} - {}", fmp.key.constData(), fmp.hashed_key);
+    gbLogFatal("Unknown style directive: {} - {}", fmp.key.constData(), static_cast<int>(fmp.hashed_key));
     break;
   }
 }
EOJ
cp *.cc after
echo "++++++++++ possible untranslatd print specifier ++++++++++"
grep gbLogDebug *.cc | grep %
grep gbLogFatal *.cc | grep %
echo "++++++++++ possible embedded newline, will not print identically ++++++++++"
grep gbLogWarning *.cc | grep %
grep gbLogDebug *.cc | grep '\\n'
grep gbLogFatal *.cc | grep '\\n'
grep gbLogWarning *.cc | grep '\\n'
