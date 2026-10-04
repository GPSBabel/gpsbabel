#!/bin/bash -e

# script to change gbWarning (printf like) usage to gbLogWarning (std::format)

#gbWarning("parse of string '%s' on line number %d as double failed.\n");
#gbWarning("parse of string '%s' on line number %d as time_t failed.\n",
for file in *.cc
do
# be wimpy, require semicolon or a comma at end of line.
  sed -i -e '/gbWarning(".*[;,]$/s/\\n"/"/' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%s/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%d/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%lld/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%i/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%u/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%c/{}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%f/{:.6f}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%02x/{:02x}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/0x%\.6x/{:#08x}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%x/{:x}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%2d/{:2}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%12s/{:12}/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/%%/%/g' $file
  sed -i -e '/gbWarning(".*[;,]$/s/gbWarning/gbLogWarning/' $file
done
# fix the rest by hand:
# parse.cc needs logging include
# unicsv.cc 425 is an enum
# mtk_logger.cc 509 is multiline
# shape.cc 152 enum
# gbser_posic.cc needs logging include
# gtm.cc 386 mutliline literal, multiline
# stackfilter.cc 147 multline literal, needs logging include
# trackfilter.cc 591 multiline literal
patch <<"EOJ"
--- parse.cc	2026-10-03 18:29:33.034610937 -0600
+++ bak/parse.cc	2026-10-03 18:25:58.735929576 -0600
@@ -32,6 +32,7 @@
 #include "parse.h"                 // for parse_double, parse_integer
 #include "defs.h"                  // for gbFatal, grid_type, KPH_TO_MPS, MPH_TO_MPS, gbWarning, FEET_TO_METERS, KNOTS_TO_MPS, kDatumWGS84, FATHOMS_TO_METERS, MILES_TO_METERS, NMILES_TO_METERS, parse_coordinates, CSTR, parse_distance, parse_speed
 #include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_UTM_EN_To_Known_Datum
+#include "src/core/logging.h"
 
 
 /*
--- unicsv.cc	2026-10-03 18:29:33.336454392 -0600
+++ bak/unicsv.cc	2026-10-03 18:26:02.803105731 -0600
@@ -422,7 +422,7 @@
       unicsv_fields_tab.last() = f.type;
 
       if (global_opts.debug_level) {
-        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), f.type);
+        gbLogWarning("Interpreting column \"{}\" as {}({}).", gbLogCStr(value), gbLogCStr(f.name), static_cast<int>(f.type));
       }
 
       /* handle some special items */
--- mtk_logger.cc	2026-10-03 18:29:32.954006961 -0600
+++ bak/mtk_logger.cc	2026-10-03 18:26:08.164145153 -0600
@@ -506,7 +506,8 @@
             }
           } else {
             if (null_len == chunk_size) {  // 0x00 block - bad block....
-              gbLogWarning("FIXME -- read bad block at {:#08x} - retry ? skip ?\n{}", data_addr, line);
+              gbLogWarning("FIXME -- read bad block at {:#08x} - retry ? skip ?", data_addr);
+              gbLogWarning("{}", line);
             }
             if (ff_len == chunk_size) {  // 0xff block - read complete...
               len = ff_len;
--- shape.cc	2026-10-03 18:29:33.182278944 -0600
+++ bak/shape.cc	2026-10-03 18:26:11.209288487 -0600
@@ -149,7 +149,7 @@
   const int nFields = DBFGetFieldCount(ihandledb);
   for (int i = 0; i < nFields; i++) {
     DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
-    gbLogWarning("Field Index: {:2}, Field Name: {:12}, Field Type {}", i, name, type);
+    gbLogWarning("Field Index: {:2}, Field Name: {:12}, Field Type {}", i, name, static_cast<int>(type));
   }
   gbFatal("\n");
 }
--- gbser_posix.cc	2026-10-03 18:29:32.596157508 -0600
+++ bak/gbser_posix.cc	2026-10-03 18:26:17.260139172 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <cassert>
 #include <cerrno>
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
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "stackfilter.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
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
