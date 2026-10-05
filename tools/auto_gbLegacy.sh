#!/bin/bash -e

# script to change gbDebug (printf like) usage to gbLogFatal (std::format)

#gbDebug("parse of string '%s' on line number %d as double failed.\n");
#gbDebug("parse of string '%s' on line number %d as time_t failed.\n",
sources=( \
*.cc \
format.h \
igc.h \
jeeps/*.cc \
src/core/textstream.cc \
src/core/xmlstreamwriter.cc \
src/core/matrix.cc \
src/core/codecdevice.cc \
)
for file in "${sources[@]}"
do
# be careful, require ); or a \n", at end of line.
#  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*\\n"(,|\);$)/s/\\n"/"/' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%s/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%d/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%ld/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%lld/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%i/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%u/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%zu/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%llu/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%c/{}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%f/{:.6f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%g/{:g}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%" PRId64 "/{}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/0x%0\*X/0x{:0{}X-REORDER-}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%(0|\.)([1-9][0-9]*)([xX])/{:0\2\3}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([1-9][0-9]*)([xX])/{:\1\2}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([xX])/{:\1}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([1-9][0-9]*)[diu]/{:\1}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%0([1-9][0-9]*)[diu]/{:0\1}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([1-9][0-9]*)\.([1-9][0-9]*)s/{:>\1.\2}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([1-9][0-9]*)s/{:>\1}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\.([1-9][0-9]*)s/{:.\1}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\.f/{:.0f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\+f/{:+f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\.([0-9]+)f/{:.\1f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\+\.([0-9]+)f/{:+.\1f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%([1-9][0-9]*)\.([0-9]+)f/{:>\1.\2f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\+([1-9][0-9]*)\.([0-9]+)f/{:>+\1.\2f}/g' "$file"
  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%0\.([0-9]+)f/{:.\1f}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%\+#g/{:+#g}/g' "$file"

  sed -E -i '/(dg100_log\("|dbg\(|gbDebug\("|gbFatal\("|gbWarning\("|gbInfo\(").*[^\\]"(,|\);$)/s/%%/%/g' "$file"
  sed -E -i '/gb(Debug|Fatal|Warning|Info)\(".*[^\\]"(,|\);$)/s/gb(Debug|Fatal|Warning|Info)/gbLog\1/' "$file"
done
rm -rf before
rm -rf after
mkdir before
mkdir before/jeeps
mkdir -p before/src/core
mkdir after
mkdir after/jeeps
mkdir -p after/src/core
cp *.cc *.h before
cp jeeps/*.cc jeeps/*.h before/jeeps
cp src/core/*.cc src/core/*.h before/src/core
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

patch <<"EOJ"
diff -u before/duplicate.cc after/duplicate.cc
--- before/duplicate.cc	2026-10-04 10:06:13.345522952 -0600
+++ after/duplicate.cc	2026-10-04 10:06:13.355283219 -0600
@@ -27,6 +27,7 @@
 #include <QMultiHash>            // for QMultiHash
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
--- before/garmin_txt.cc	2026-10-05 09:12:14.875203043 -0600
+++ after/garmin_txt.cc	2026-10-05 09:12:44.314562956 -0600
@@ -963,7 +963,7 @@
       int field_no = field_idx + 1;
       header_mapping_info[ht].append(std::make_pair(name, field_no));
       if (global_opts.debug_level >= 2) {
-        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})\n", gbLogCStr(name), field_no, ht, i);
+        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})\n", gbLogCStr(name), field_no, gpsbabel::to_underlying(ht), i);
       }
     } else {
       gbLogWarning("Field {} not recognized!\n", gbLogCStr(name));
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
 
--- before/shape.cc	2026-10-05 09:16:03.039363463 -0600
+++ after/shape.cc	2026-10-05 09:16:09.959643796 -0600
@@ -149,7 +149,7 @@
   const int nFields = DBFGetFieldCount(ihandledb);
   for (int i = 0; i < nFields; i++) {
     DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
-    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}\n", i, name, type);
+    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}\n", i, name, gpsbabel::to_underlying(type));
   }
   gbLogFatal("\n");
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
-        gbLogWarning("Interpreting column \"{}\" as {}({}).\n", gbLogCStr(value), gbLogCStr(f.name), f.type);
+        gbLogWarning("Interpreting column \"{}\" as {}({}).\n", gbLogCStr(value), gbLogCStr(f.name), gpsbabel::to_underlying(f.type));
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
 
diff -u before/xcsv.cc after/xcsv.cc
--- before/xcsv.cc	2026-10-04 10:06:13.346996764 -0600
+++ after/xcsv.cc	2026-10-04 10:06:13.356798825 -0600
@@ -809,7 +809,7 @@
     break;
 
   default:
-    gbLogFatal("Unknown style directive: {} - {}\n", fmp.key.constData(), fmp.hashed_key);
+    gbLogFatal("Unknown style directive: {} - {}\n", fmp.key.constData(), gpsbabel::to_underlying(fmp.hashed_key));
     break;
   }
 }
--- before/exif.cc	2026-10-05 09:26:11.147010741 -0600
+++ after/exif.cc	2026-10-05 09:30:25.139103222 -0600
@@ -552,7 +552,7 @@
           } else if (tag->type == EXIF_TYPE_DOUBLE) {
             gbLogDebug(" {:+#g}", tag->data.at(idx).value<double>());
           } else {
-            gbLogDebug(" 0x{:0{}X-REORDER-}", 2 * exif_type_size(tag->type), tag->data.at(idx).value<uint32_t>());
+            gbLogDebug(" 0x{:0{}X}", tag->data.at(idx).value<uint32_t>(), 2 * exif_type_size(tag->type));
           }
         }
         if (tag->count > 4) {
EOJ

# a couple that didn't match our replacement search pattern
patch <<"EOJ"
--- before/lowranceusr.cc	2026-10-05 09:24:44.942211145 -0600
+++ after/lowranceusr.cc	2026-10-05 09:24:44.948263858 -0600
@@ -632,11 +632,11 @@
       gbLogDebug(" {:08x} {:>8.3f} {:08x} {:08x} {:08x}\n",
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
--- before/dg-100.cc	2026-10-05 11:58:30.034886159 -0600
+++ after/dg-100.cc	2026-10-05 11:59:14.263096420 -0600
@@ -129,18 +129,6 @@
   }
 }
 
-void
-Dg100Format::dg100_log(const char* fmt, ...)
-{
-  if (global_opts.debug_level > 0) {
-    va_list ap;
-    va_start(ap, fmt);
-    gbVLegacyLog(QtDebugMsg, fmt, ap);
-    va_end(ap);
-  }
-}
-
-
 /* TODO: check whether negative lat/lon (West/South) are handled correctly */
 float
 Dg100Format::bin2deg(int val)
--- before/skytraq.cc	2026-10-05 11:59:42.441131074 -0600
+++ after/skytraq.cc	2026-10-05 12:00:04.900146582 -0600
@@ -65,17 +65,6 @@
 
 
 void
-SkytraqBase::dbg(int l, const char* msg, ...)
-{
-  if (global_opts.debug_level >= l) {
-    va_list ap;
-    va_start(ap, msg);
-    gbVLegacyLog(QtDebugMsg, msg, ap);
-    va_end(ap);
-  }
-}
-
-void
 SkytraqBase::rd_drain()
 {
   if (gbser_flush(serial_handle)) {
--- before/mtk_logger.cc	2026-10-05 12:00:24.462995712 -0600
+++ after/mtk_logger.cc	2026-10-05 12:00:40.343297581 -0600
@@ -98,17 +98,6 @@
 
 #define HOLUX245_MASK (1 << 27)
 
-// TODO: These should become Debug() from src/core/logging.
-void MtkLoggerBase::dbg(int l, const char* msg, ...)
-{
-  if (global_opts.debug_level >= l) {
-    va_list ap;
-    va_start(ap, msg);
-    gbVLegacyLog(QtDebugMsg, msg, ap);
-    va_end(ap);
-  }
-}
-
 // Returns a fully qualified pathname to a temporary file that is a copy
 // of the data downloaded from the device. Only two copies are ever in play,
 // the primary (e.g. "/tmp/data.bin") and the backup ("/tmp/data_old.bin").
--- before/format.h	2026-10-05 12:26:56.137005964 -0600
+++ after/format.h	2026-10-05 12:27:20.346483594 -0600
@@ -39,6 +39,7 @@
 #define FORMAT_H_INCLUDED_
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 class Format
--- before/gdb.cc	2026-10-05 12:34:33.642306660 -0600
+++ after/gdb.cc	2026-10-05 12:41:00.145453524 -0600
@@ -49,6 +49,7 @@
 #include "jeeps/gpsmath.h"          // for GPS_Math_Deg_To_Semi, GPS_Math_Semi_To_Deg
 #include "mkshort.h"                // for MakeShort
 #include "src/core/datetime.h"      // for DateTime
+#include "src/core/logging.h"       // for gbLogWarning, gbLogDebug, gbLogFatal, gbLogInfo
 
 
 #define GDB_DEF_CLASS		gt_waypt_class_user_waypoint
@@ -441,7 +442,7 @@
       if constexpr(GDB_DEBUG) {
         DBG(GDB_DBG_WPTe, true)
         gbLogDebug("wpt \"{}\" ({}): Altitude = {:.1f}\n",
-                gbLogCStr(res->shortname), wpt_class, alt);
+                gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), alt);
       }
     }
   }
@@ -456,7 +457,7 @@
   if constexpr(GDB_DEBUG) {
     DBG(GDB_DBG_WPTe, !res->notes.isNull())
     gbLogDebug("wpt \"{}\" ({}): notes = {}\n",
-            gbLogCStr(res->shortname), wpt_class,
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class),
             gbLogCStr(QString(res->notes).replace("\r\n", ", ")));
   }
   if (FREAD_C == 1) {
@@ -464,14 +465,14 @@
     if constexpr(GDB_DEBUG) {
       DBG(GDB_DBG_WPTe, res->proximity_has_value())
       gbLogDebug("wpt \"{}\" ({}): Proximity = {:.1f}\n",
-              gbLogCStr(res->shortname), wpt_class, res->proximity_value() / 1000);
+              gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), res->proximity_value() / 1000);
     }
   }
   int display = FREAD_i32;
   if constexpr(GDB_DEBUG) {
     DBG(GDB_DBG_WPTe, true)
     gbLogDebug("wpt \"{}\" ({}): display = {}\n",
-            gbLogCStr(res->shortname), wpt_class, display);
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), display);
   }
   switch (display) {			/* display value */
   case gt_gdb_display_mode_symbol:
@@ -500,7 +501,7 @@
     if constexpr(GDB_DEBUG) {
       DBG(GDB_DBG_WPTe, res->depth_has_value())
       gbLogDebug("wpt \"{}\" ({}): Depth = {:.1f}\n",
-              gbLogCStr(res->shortname), wpt_class, res->depth_value());
+              gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), res->depth_value());
     }
   }
 
@@ -520,7 +521,7 @@
       QString temp = FREAD_CSTR_AS_QSTR;				/* undocumented & unused string */
       DBG(GDB_DBG_WPTe, !temp.isEmpty())
       gbLogDebug("wpt \"{}\" ({}): Unknown string = {}\n",
-              gbLogCStr(res->shortname), wpt_class, gbLogCStr(temp));
+              gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), gbLogCStr(temp));
     } else {
       (void) FREAD_CSTR_AS_QSTR;				/* undocumented & unused string */
     }
@@ -549,7 +550,7 @@
       if constexpr(GDB_DEBUG) {
         DBG(GDB_DBG_WPTe, true)
         gbLogDebug("wpt \"{}\" ({}): duration = {}\n",
-                gbLogCStr(res->shortname), wpt_class, duration);
+                gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), duration);
       }
     }
     int url_ct = FREAD_i32;
@@ -560,7 +561,7 @@
         if constexpr(GDB_DEBUG) {
           DBG(GDB_DBG_WPTe, true)
           gbLogDebug("wpt \"{}\" ({}): url({}) = {}\n",
-                  gbLogCStr(res->shortname), wpt_class, url_ct - i, gbLogCStr(str));
+                  gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), url_ct - i, gbLogCStr(str));
         }
       }
     }
@@ -569,10 +570,10 @@
   if constexpr(GDB_DEBUG) {
     DBG(GDB_DBG_WPTe, !res->description.isNull())
     gbLogDebug("wpt \"{}\" ({}): description = {}\n",
-            gbLogCStr(res->shortname), wpt_class, gbLogCStr(res->description));
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), gbLogCStr(res->description));
     DBG(GDB_DBG_WPTe, res->urls.HasUrlLink())
     gbLogDebug("wpt \"{}\" ({}): url = {}\n",
-            gbLogCStr(res->shortname), wpt_class, gbLogCStr(res->urls.GetUrlLink().url_));
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), gbLogCStr(res->urls.GetUrlLink().url_));
   }
   int category = FREAD_i16;
   if (category != 0) {
@@ -581,7 +582,7 @@
   if constexpr(GDB_DEBUG) {
     DBG(GDB_DBG_WPTe, category)
     gbLogDebug("wpt \"{}\" ({}): category = {}\n",
-            gbLogCStr(res->shortname), wpt_class, category);
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), category);
   }
 
   if (FREAD_C == 1) {
@@ -589,7 +590,7 @@
     if constexpr(GDB_DEBUG) {
       DBG(GDB_DBG_WPTe, res->temperature_has_value())
       gbLogDebug("wpt \"{}\" ({}): temperature = {:.1f}\n",
-              gbLogCStr(res->shortname), wpt_class, res->temperature_value());
+              gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), res->temperature_value());
     }
   }
 
@@ -618,7 +619,7 @@
   if constexpr(GDB_DEBUG) {
     DBG(GDB_DBG_WPTe, icon != kGDBDefIcon)
     gbLogDebug("wpt \"{}\" ({}): icon = \"{}\" (MapSource symbol {})\n",
-            gbLogCStr(res->shortname), wpt_class, gbLogCStr(res->icon_descr), icon);
+            gbLogCStr(res->shortname), gpsbabel::to_underlying(wpt_class), gbLogCStr(res->icon_descr), icon);
   }
   QString str;
   if (!(str = garmin_fs_t::get_cc(gmsd, nullptr)).isEmpty()) {
@@ -1031,7 +1032,7 @@
       }
       if (typ == 'W')
         gbLogWarning("({}{}-{:02}): delta = {} (flag={:3}/{:02x})-",
-                  gdb_ver, typ, wpt_class, delta, waypt_flag, waypt_flag);
+                  gdb_ver, typ, gpsbabel::to_underlying(wpt_class), delta, waypt_flag, waypt_flag);
       else {
         gbLogWarning("({}{}): delta = {} -", gdb_ver, typ, delta);
       }
--- before/gdb.cc	2026-10-05 12:54:13.720486891 -0600
+++ after/gdb.cc	2026-10-05 12:54:57.288923146 -0600
@@ -828,8 +828,8 @@
       FREAD(tbuf, 8); /* unknown bytes */
       if constexpr(GDB_DEBUG) {
         DBG(GDB_DBG_RTE, true)
-        gbDebug("rte_pt: autoroute info: route style %d, calculation type %d, vehicle type %d, road selection %d\n"
-                "                            driving speeds (kph) %.0f, %.0f, %.0f, %.0f, %.0f\n",
+        gbLogDebug("rte_pt: autoroute info: route style {}, calculation type {}, vehicle type {}, road selection {}\n"
+                   "                            driving speeds (kph) {:.0f}, {:.0f}, {:.0f}, {:.0f}, {:.0f}\n",
                 route_style, calc_type, vehicle_type, road_selection,
                 driving_speed[0], driving_speed[1], driving_speed[2], driving_speed[3], driving_speed[4]);
       } else {
--- before/gdb.cc	2026-10-05 12:59:59.205232583 -0600
+++ after/gdb.cc	2026-10-05 13:00:12.849667809 -0600
@@ -235,10 +235,10 @@
     double dist = radtometers(gcdist(ref->position(), tmp->position()));
 
     if (fabs(dist) > 100) {
-      gbFatal("Route point mismatch!\n" \
-              "  \"%s\" from waypoints differs to \"%s\"\n" \
-              "  from route table by more than %0.1f meters!\n", \
-              gbLogCStr(tmp->shortname), gbLogCStr(ref->shortname), dist);
+      gbLogFatal("Route point mismatch!\n"
+                 "  \"{}\" from waypoints differs to \"{}\"\n"
+                 "  from route table by more than {:.1f} meters!\n",
+                 gbLogCStr(tmp->shortname), gbLogCStr(ref->shortname), dist);
     }
   }
   Waypoint* res = nullptr;
--- before/gtm.cc	2026-10-05 13:01:37.947131825 -0600
+++ after/gtm.cc	2026-10-05 13:02:07.531500645 -0600
@@ -383,10 +383,10 @@
   //       If ts_count != real_track_list.size() we don't know how to line up
   //       the tracklogs, and the real tracks, with the tracklog styles.
   if (ts_count != real_track_list.size()) {
-    gbWarning("The number of tracklog entries with the new flag "
-           "set doesn't match the number of tracklog style entries.\n"
-           "  This is unexpected and may indicate a malformed input file.\n"
-           "  As a result the track names may be incorrect.\n");
+    gbLogWarning("The number of tracklog entries with the new flag "
+                 "set doesn't match the number of tracklog style entries.\n"
+                 "  This is unexpected and may indicate a malformed input file.\n"
+                 "  As a result the track names may be incorrect.\n");
   }
   // Read the entire tracklog styles section whether we use it or not.
   for (i = 0; i != ts_count; i++) {
--- before/stackfilter.cc	2026-10-05 13:03:22.283744350 -0600
+++ after/stackfilter.cc	2026-10-05 13:03:47.723097433 -0600
@@ -145,8 +145,8 @@
   stack_elt* tmp_elt = nullptr;
 
   if (warnings_enabled && stack) {
-    gbWarning("Warning: leftover stack entries; "
-            "check command line for mistakes\n");
+    gbLogWarning("Warning: leftover stack entries; "
+                 "check command line for mistakes\n");
   }
   while (stack) {
     stack->waypts.flush();
--- before/trackfilter.cc	2026-10-05 13:06:04.440264270 -0600
+++ after/trackfilter.cc	2026-10-05 13:06:18.400676057 -0600
@@ -588,9 +588,9 @@
     }
   }
   if (timeless_points > 0) {
-    gbWarning("move: %d points out of %d total points didn't have "
-            "time information and could not be moved.\n",
-            timeless_points, track_waypt_count());
+    gbLogWarning("move: {} points out of {} total points didn't have "
+                 "time information and could not be moved.\n",
+                 timeless_points, track_waypt_count());
   }
 }
 
--- before/gbfile.cc	2026-10-05 13:07:27.513637089 -0600
+++ after/gbfile.cc	2026-10-05 13:07:43.301591073 -0600
@@ -547,7 +547,7 @@
       /* force gzipped files on output */
       file->gzapi = 1;
 #else
-      gbFatal(NO_ZLIB);
+      gbLogFatal(NO_ZLIB);
 #endif
     }
 
--- before/gbser_win.cc	2026-10-05 15:53:29.599743541 -0600
+++ gbser_win.cc	2026-10-05 15:54:02.966020620 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <windows.h>
 #include <setupapi.h>
EOJ
patch -p0 <<"EOJ"
--- jeeps/gpsapp.cc	2026-10-05 13:53:57.971024963 -0600
+++ jeeps/gpsapp.cc	2026-10-05 13:54:23.080778739 -0600
@@ -40,6 +40,7 @@
 #include "jeeps/garminusb.h"
 #include "jeeps/gpsserial.h"
 #include "jeeps/gpsusbint.h"
+#include "src/core/logging.h"
 
 time_t gps_save_time;
 double gps_save_lat;
--- jeeps/gpscom.cc	2026-10-05 13:29:17.737948350 -0600
+++ jeeps/gpscom.cc	2026-10-05 13:31:22.983365036 -0600
@@ -31,6 +31,8 @@
 
 #include <QByteArray>
 
+#include "src/core/logging.h"
+
 /* @func GPS_Command_Off ***********************************************
 **
 ** Turn off power on GPS
--- jeeps/gpsmath.cc	2026-10-05 13:34:02.083887520 -0600
+++ after/jeeps/gpsmath.cc	2026-10-05 13:34:36.536903244 -0600
@@ -35,6 +35,7 @@
 
 #include "defs.h"            // for gbFatal, CSTR
 #include "jeeps/gpsdatum.h"  // for GPS_ODatum, GPS_OEllipse, GPS_Datums, GPS_Ellipses, UKNG, GPS_SDatum_Alias, GPS_SDatum, GPS_DatumAliases, GPS_PDatum, GPS_PDatum_Alias
+#include "src/core/logging.h"
 
 static constexpr bool use_exact_helmert_inverse = false;
 
--- jeeps/gpsusbcommon.cc	2026-10-05 13:35:17.806650995 -0600
+++ jeeps/gpsusbcommon.cc	2026-10-05 13:38:39.593960049 -0600
@@ -22,6 +22,7 @@
 #include "jeeps/gps.h"
 #include "jeeps/garminusb.h"
 #include "jeeps/gpsusbcommon.h"
+#include "src/core/logging.h"
 
 /*
  * This receive logic is a little convoluted as we go to some efforts here
@@ -93,7 +94,7 @@
     rv = gusb_llops->llop_get_bulk(ibuf, sz);
     break;
   default:
-    gbLogFatal("Unknown receiver state {}\n", receive_state);
+    gbLogFatal("Unknown receiver state {}\n", gpsbabel::to_underlying(receive_state));
   }
 
   pkt_id = le_read16(&ibuf->gusb_pkt.pkt_id);
--- before/jeeps/gpslibusb.cc	2026-10-05 15:04:57.011302183 -0600
+++ jeeps/gpslibusb.cc	2026-10-05 15:04:37.266018361 -0600
@@ -37,6 +37,7 @@
 #include "jeeps/garminusb.h"
 #include "jeeps/gpsdevice.h"
 #include "jeeps/gpsusbcommon.h"
+#include "src/core/logging.h"
 
 #define GARMIN_VID 0x91e
 
@@ -312,8 +313,8 @@
      * kernel driver that bonds with the hardware.
      */
     usb_get_driver_np(udev, 0, drvnm, sizeof(drvnm)-1);
-    gbFatal("usb_set_configuration failed, probably because kernel driver '%s'\n is blocking our access to the USB device.\n"
-          "For more information see https://www.gpsbabel.org/os/Linux_Hotplug.html\n", drvnm);
+    gbLogFatal("usb_set_configuration failed, probably because kernel driver '{}'\n is blocking our access to the USB device.\n"
+               "For more information see https://www.gpsbabel.org/os/Linux_Hotplug.html\n", drvnm);
 #else
 
     gbLogFatal("usb_set_configuration failed: {}\n", usb_strerror());
@@ -421,9 +422,9 @@
     return;
   }
 
-  gbFatal("Could not identify endpoints on USB device.\n"
-        "Found endpoints Intr In 0x%x Bulk Out 0x%x Bulk In %0xx\n",
-        gusb_intr_in_ep, gusb_bulk_out_ep, gusb_bulk_in_ep);
+  gbLogFatal("Could not identify endpoints on USB device.\n"
+             "Found endpoints Intr In 0x{:x} Bulk Out 0x{:x} Bulk In %0xx\n",
+             gusb_intr_in_ep, gusb_bulk_out_ep, gusb_bulk_in_ep);
 }
 
 static
@@ -477,11 +478,11 @@
   if (0 == found_devices) {
     gbLogFatal("Found no Garmin USB devices.\n");
   } else if (req_unit_number >= found_devices) {
-    gbFatal("usb unit number(%d) too high.\n"
-          "The unit number must be either\n"
-          "1) nonnegative and less than the number of garmin devices found(%d), or\n"
-          "2) negative to list the garmin devices found.\n",
-          req_unit_number, found_devices);
+    gbLogFatal("usb unit number({}) too high.\n"
+               "The unit number must be either\n"
+               "1) nonnegative and less than the number of garmin devices found({}), or\n"
+               "2) negative to list the garmin devices found.\n",
+               req_unit_number, found_devices);
   } else {
     return 1;
   }
--- before/jeeps/gpsusbstub.cc	2026-10-05 15:12:03.434043221 -0600
+++ jeeps/gpsusbstub.cc	2026-10-05 15:12:42.004460399 -0600
@@ -21,6 +21,7 @@
 
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 #if !HAVE_LIBUSB_1_0
 
@@ -29,7 +30,7 @@
 int
 gusb_init(const char* portname, gpsdevh** dh)
 {
-  gbFatal(no_usb);
+  gbLogFatal(no_usb);
   return 0;
 }
 
--- before/jeeps/gpsusbwin.cc	2026-10-05 15:12:03.434054833 -0600
+++ jeeps/gpsusbwin.cc	2026-10-05 15:14:06.452031078 -0600
@@ -31,6 +31,7 @@
 #include "jeeps/gps.h"
 #include "jeeps/gpsapp.h"
 #include "jeeps/gpsusbcommon.h"
+#include "src/core/logging.h"
 
 /* Constants from Garmin doc. */
 
@@ -162,7 +163,7 @@
                           0, NULL, OPEN_EXISTING, 0, NULL);
   if (usb_handle == INVALID_HANDLE_VALUE) {
     if (GetLastError() == ERROR_ACCESS_DENIED) {
-      gbWarning(
+      gbLogWarning(
         "Exclusive access is denied.  It's likely that something else such as\n"
         "Garmin Lifetime Updater, Communicator, Basecamp, Nroute, Spanner,\n"
         "Google Earth, or GPSGate already has control of the device\n");
--- before/jeeps/gpsserial.cc	2026-10-05 16:17:22.286160835 -0600
+++ jeeps/gpsserial.cc	2026-10-05 16:17:44.053496283 -0600
@@ -26,6 +26,7 @@
 #include "jeeps/gps.h"
 #include "gbser.h"
 #include "jeeps/gpsserial.h"
+#include "src/core/logging.h"
 #include <QThread>
 #include <cerrno>
 #include <cstdio>
EOJ
patch -p0 <<"EOJ"
--- src/core/matrix.cc	2026-10-05 14:03:56.950517886 -0600
+++ src/core/matrix.cc	2026-10-05 14:04:13.590903113 -0600
@@ -25,6 +25,7 @@
 #include <QDebugStateSaver>  // for QDebugStateSaver
 
 #include "defs.h"            // For gbFatal
+#include "src/core/logging.h"
 
 Matrix::Matrix(int rows, int cols) : rows_(rows), cols_(cols), data_(rows * cols, 0.0) {}
 
--- src/core/xmlstreamwriter.cc	2026-10-05 14:04:51.854997658 -0600
+++ src/core/xmlstreamwriter.cc	2026-10-05 14:05:08.928901998 -0600
@@ -24,6 +24,7 @@
 #include <QtGlobal>                 // for QT_VERSION, QT_VERSION_CHECK
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 // As this code began in C, we have several hundred places that write
 // c strings.  Add a test that the string contains anything useful
EOJ
cp *.cc *.h after
cp jeeps/*.cc jeeps/*.h after/jeeps
cp src/core/*.cc src/core/*.h after/src/core
echo "++++++++++ possible untranslatd print specifier ++++++++++"
grep -n gbLogDebug "${sources[@]}" | grep % || true
grep -n gbLogFatal "${sources[@]}" | grep % || true
grep -n gbLogWarning "${sources[@]}" | grep % || true
grep -n gbLogInfo "${sources[@]}" | grep % || true
#echo "++++++++++ possible embedded newline, will not print identically ++++++++++"
#grep -n gbLogDebug *.cc | grep '\\n' || true
#grep -n gbLogFatal *.cc | grep '\\n' || true
#grep -n gbLogWarning *.cc | grep '\\n' || true
#grep -n gbLogInfo *.cc | grep '\\n' || true
echo "++++++++++ possible missed conversions ++++++++++"
grep -n gbDebug "${sources[@]}" | grep -v \#include || true
grep -n gbFatal "${sources[@]}" | grep -v FatalMsg\(\) | grep -v \#include || true
grep -n gbWarning "${sources[@]}" | grep -v \#include || true
grep -n gbInfo "${sources[@]}" | grep -v \#include || true
