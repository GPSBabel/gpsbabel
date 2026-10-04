#!/bin/bash -e

# script to change gbFatal (printf like) usage to gbLogFatal (std::format)

#gbFatal("parse of string '%s' on line number %d as double failed.\n");
#gbFatal("parse of string '%s' on line number %d as time_t failed.\n",
for file in *.cc
do
# be wimpy, require semicolon or a comma at end of line.
  sed -i -e '/gbFatal(".*[;,]$/s/\\n"/"/' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%s/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%d/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%ld/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%lld/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%i/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%u/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%zu/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%llu/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%c/{}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%f/{:.6f}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%02x/{:02x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%08x/{:08x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/0x%04x/{:#06x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/0x%04X/{:#06X}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/0x%08X/{:#010X}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/0x%\.6x/{:#08x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/0x%\.4x/{:#06x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%x/{:x}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%2d/{:2}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%12s/{:12}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%\.f/{:.0f}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%\.5f/{:.5f}/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/%%/%/g' $file
  sed -i -e '/gbFatal(".*[;,]$/s/gbFatal/gbLogFatal/' $file
done
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
# position.cc
# xcsv.cc 812 enum
# igc.cc 10 lines with newlines 157, 167, 183, 227, 290, 306, 315, 353, 364, 443
echo "igc.cc has multiline gbLogFatal calls"
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
--- rgbcolors.cc	2026-10-03 19:45:07.501767410 -0600
+++ bakfout/rgbcolors.cc	2026-10-03 19:46:52.469837815 -0600
@@ -28,6 +28,7 @@
 #include <QtGlobal>            // for qPrintable
 
 #include "defs.h"              // for gbFatal, color_to_bbggrr
+#include "src/core/logging.h"
 
 /*
  * Colors derived from http://www.w3.org/TR/SVG/types.html#ColorKeywords
EOJ
