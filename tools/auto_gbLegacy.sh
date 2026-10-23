#!/bin/bash -e

# script to change gbDebug (printf like) usage to gbLogFatal (std::format)

mapfile -d '' sources < <(find . ./src/core ./jeeps -maxdepth 1 \( -name "*.cc" -o -name "*.h" \) -print0)

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
cp ./*.cc ./*.h before
cp jeeps/*.cc jeeps/*.h before/jeeps
cp src/core/*.cc src/core/*.h before/src/core
patch -p1 <<"EOJ"
diff -ur before/dg-100.cc after/dg-100.cc
--- before/dg-100.cc	2026-10-06 12:24:15.526566902 -0600
+++ after/dg-100.cc	2026-10-06 12:24:15.542437051 -0600
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
diff -ur before/duplicate.cc after/duplicate.cc
--- before/duplicate.cc	2026-10-06 12:24:15.526608922 -0600
+++ after/duplicate.cc	2026-10-06 12:24:15.542480298 -0600
@@ -27,6 +27,7 @@
 #include <QMultiHash>            // for QMultiHash
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -ur before/exif.cc after/exif.cc
--- before/exif.cc	2026-10-06 12:24:15.526620536 -0600
+++ after/exif.cc	2026-10-06 12:24:15.542493547 -0600
@@ -552,7 +552,7 @@
           } else if (tag->type == EXIF_TYPE_DOUBLE) {
             gbLogDebug(" {:+#g}", tag->data.at(idx).value<double>());
           } else {
-            gbLogDebug(" 0x{:0{}X-REORDER-}", 2 * exif_type_size(tag->type), tag->data.at(idx).value<uint32_t>());
+            gbLogDebug(" 0x{:0{}X}", tag->data.at(idx).value<uint32_t>(), 2 * exif_type_size(tag->type));
           }
         }
         if (tag->count > 4) {
diff -ur before/format.h after/format.h
--- before/format.h	2026-10-06 12:24:15.528208291 -0600
+++ after/format.h	2026-10-06 12:24:15.544152163 -0600
@@ -39,6 +39,7 @@
 #define FORMAT_H_INCLUDED_
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 
 class Format
diff -ur before/garmin_txt.cc after/garmin_txt.cc
--- before/garmin_txt.cc	2026-10-06 12:24:15.526816186 -0600
+++ after/garmin_txt.cc	2026-10-06 12:24:15.542681339 -0600
@@ -963,7 +963,7 @@
       int field_no = field_idx + 1;
       header_mapping_info[ht].append(std::make_pair(name, field_no));
       if (global_opts.debug_level >= 2) {
-        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})\n", gbLogCStr(name), field_no, ht, i);
+        gbLogDebug("Binding field \"{}\" to internal number {} ({},{})\n", gbLogCStr(name), field_no, gpsbabel::to_underlying(ht), i);
       }
     } else {
       gbLogWarning("Field {} not recognized!\n", gbLogCStr(name));
diff -ur before/gbser_posix.cc after/gbser_posix.cc
--- before/gbser_posix.cc	2026-10-06 12:24:15.526877336 -0600
+++ after/gbser_posix.cc	2026-10-06 12:24:15.542745835 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <cassert>
 #include <cerrno>
diff -ur before/gbser_win.cc after/gbser_win.cc
--- before/gbser_win.cc	2026-10-06 12:24:15.526891449 -0600
+++ after/gbser_win.cc	2026-10-06 12:24:15.542760610 -0600
@@ -22,6 +22,7 @@
 #include "defs.h"
 #include "gbser.h"
 #include "gbser_private.h"
+#include "src/core/logging.h"
 
 #include <windows.h>
 #include <setupapi.h>
diff -ur before/gdb.cc after/gdb.cc
--- before/gdb.cc	2026-10-06 12:24:15.526905864 -0600
+++ after/gdb.cc	2026-10-06 12:24:15.542776540 -0600
@@ -49,6 +49,7 @@
 #include "jeeps/gpsmath.h"          // for GPS_Math_Deg_To_Semi, GPS_Math_Semi_To_Deg
 #include "mkshort.h"                // for MakeShort
 #include "src/core/datetime.h"      // for DateTime
+#include "src/core/logging.h"       // for gbLogWarning, gbLogDebug, gbLogFatal, gbLogInfo
 
 
 #define GDB_DEF_CLASS		gt_waypt_class_user_waypoint
@@ -234,10 +235,10 @@
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
@@ -827,8 +828,8 @@
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
@@ -1031,7 +1032,7 @@
       }
       if (typ == 'W')
         gbLogWarning("({}{}-{:02}): delta = {} (flag={:3}/{:02x})-",
-                  gdb_ver, typ, wpt_class, delta, waypt_flag, waypt_flag);
+                  gdb_ver, typ, gpsbabel::to_underlying(wpt_class), delta, waypt_flag, waypt_flag);
       else {
         gbLogWarning("({}{}): delta = {} -", gdb_ver, typ, delta);
       }
diff -ur before/gtm.cc after/gtm.cc
--- before/gtm.cc	2026-10-06 12:24:15.527070689 -0600
+++ after/gtm.cc	2026-10-06 12:24:15.542947203 -0600
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
diff -ur before/height.cc after/height.cc
--- before/height.cc	2026-10-06 12:24:15.527113738 -0600
+++ after/height.cc	2026-10-06 12:24:15.542980367 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "height.h"
+#include "src/core/logging.h"
 #include <cmath>    // for floor
 #include <cstdint>  // for int8_t
 
diff -ur before/jeeps/gpsapp.cc after/jeeps/gpsapp.cc
--- before/jeeps/gpsapp.cc	2026-10-06 12:24:15.530321207 -0600
+++ after/jeeps/gpsapp.cc	2026-10-06 12:24:15.546851754 -0600
@@ -40,6 +40,7 @@
 #include "jeeps/garminusb.h"
 #include "jeeps/gpsserial.h"
 #include "jeeps/gpsusbint.h"
+#include "src/core/logging.h"
 
 time_t gps_save_time;
 double gps_save_lat;
diff -ur before/jeeps/gpscom.cc after/jeeps/gpscom.cc
--- before/jeeps/gpscom.cc	2026-10-06 12:24:15.530385758 -0600
+++ after/jeeps/gpscom.cc	2026-10-06 12:24:15.546922099 -0600
@@ -31,6 +31,8 @@
 
 #include <QByteArray>
 
+#include "src/core/logging.h"
+
 /* @func GPS_Command_Off ***********************************************
 **
 ** Turn off power on GPS
diff -ur before/jeeps/gpslibusb.cc after/jeeps/gpslibusb.cc
--- before/jeeps/gpslibusb.cc	2026-10-06 12:24:15.530463109 -0600
+++ after/jeeps/gpslibusb.cc	2026-10-06 12:24:15.547000039 -0600
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
diff -ur before/jeeps/gpsmath.cc after/jeeps/gpsmath.cc
--- before/jeeps/gpsmath.cc	2026-10-06 12:24:15.530480061 -0600
+++ after/jeeps/gpsmath.cc	2026-10-06 12:24:15.547017776 -0600
@@ -35,6 +35,7 @@
 
 #include "defs.h"            // for gbFatal, CSTR
 #include "jeeps/gpsdatum.h"  // for GPS_ODatum, GPS_OEllipse, GPS_Datums, GPS_Ellipses, UKNG, GPS_SDatum_Alias, GPS_SDatum, GPS_DatumAliases, GPS_PDatum, GPS_PDatum_Alias
+#include "src/core/logging.h"
 
 static constexpr bool use_exact_helmert_inverse = false;
 
diff -ur before/jeeps/gpsserial.cc after/jeeps/gpsserial.cc
--- before/jeeps/gpsserial.cc	2026-10-06 12:24:15.530609141 -0600
+++ after/jeeps/gpsserial.cc	2026-10-06 12:24:15.547199253 -0600
@@ -26,6 +26,7 @@
 #include "jeeps/gps.h"
 #include "gbser.h"
 #include "jeeps/gpsserial.h"
+#include "src/core/logging.h"
 #include <QThread>
 #include <cerrno>
 #include <cstdio>
diff -ur before/jeeps/gpsusbcommon.cc after/jeeps/gpsusbcommon.cc
--- before/jeeps/gpsusbcommon.cc	2026-10-06 12:24:15.530622948 -0600
+++ after/jeeps/gpsusbcommon.cc	2026-10-06 12:24:15.547215006 -0600
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
diff -ur before/jeeps/gpsusbstub.cc after/jeeps/gpsusbstub.cc
--- before/jeeps/gpsusbstub.cc	2026-10-06 12:24:15.530656119 -0600
+++ after/jeeps/gpsusbstub.cc	2026-10-06 12:24:15.547254764 -0600
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
 
diff -ur before/jeeps/gpsusbwin.cc after/jeeps/gpsusbwin.cc
--- before/jeeps/gpsusbwin.cc	2026-10-06 12:24:15.530669079 -0600
+++ after/jeeps/gpsusbwin.cc	2026-10-06 12:24:15.547269067 -0600
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
diff -ur before/lowranceusr.cc after/lowranceusr.cc
--- before/lowranceusr.cc	2026-10-06 12:24:15.527254836 -0600
+++ after/lowranceusr.cc	2026-10-06 12:24:15.543144652 -0600
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
diff -ur before/mkshort.cc after/mkshort.cc
--- before/mkshort.cc	2026-10-06 12:24:15.527313447 -0600
+++ after/mkshort.cc	2026-10-06 12:24:15.543201750 -0600
@@ -33,6 +33,7 @@
 
 #include "defs.h"
 #include "geocache.h"  // for Geocache
+#include "src/core/logging.h"
 
 
 const QByteArray MakeShort::vowels = "aeiouAEIOU";
diff -ur before/mtk_logger.cc after/mtk_logger.cc
--- before/mtk_logger.cc	2026-10-06 12:24:15.527328180 -0600
+++ after/mtk_logger.cc	2026-10-06 12:24:15.543221786 -0600
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
diff -ur before/parse.cc after/parse.cc
--- before/parse.cc	2026-10-06 12:24:15.527425827 -0600
+++ after/parse.cc	2026-10-06 12:24:15.543330581 -0600
@@ -32,6 +32,7 @@
 #include "parse.h"                 // for parse_double, parse_integer
 #include "defs.h"                  // for gbFatal, grid_type, KPH_TO_MPS, MPH_TO_MPS, gbWarning, FEET_TO_METERS, KNOTS_TO_MPS, kDatumWGS84, FATHOMS_TO_METERS, MILES_TO_METERS, NMILES_TO_METERS, parse_coordinates, CSTR, parse_distance, parse_speed
 #include "jeeps/gpsmath.h"         // for GPS_Math_Known_Datum_To_WGS84_M, GPS_Math_Swiss_EN_To_WGS84, GPS_Math_UKOSMap_To_WGS84_H, GPS_Math_UTM_EN_To_Known_Datum
+#include "src/core/logging.h"
 
 
 /*
diff -ur before/position.cc after/position.cc
--- before/position.cc	2026-10-06 12:24:15.527453311 -0600
+++ after/position.cc	2026-10-06 12:24:15.543362836 -0600
@@ -30,6 +30,7 @@
 #include "defs.h"
 #include "grtcirc.h"            // for gcdist, radtometers
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -ur before/radius.cc after/radius.cc
--- before/radius.cc	2026-10-06 12:24:15.527479882 -0600
+++ after/radius.cc	2026-10-06 12:24:15.543391336 -0600
@@ -28,6 +28,7 @@
 
 #include "defs.h"           // for Waypoint, del_marked_wpts, route_add_head, route_add_wpt, waypt_add, waypt_sort, waypt_swap, route_head, WaypointList, kMilesPerKilometer
 #include "grtcirc.h"         // for gcdist, radtomiles
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -ur before/reverse_route.cc after/reverse_route.cc
--- before/reverse_route.cc	2026-10-06 12:24:15.527518116 -0600
+++ after/reverse_route.cc	2026-10-06 12:24:15.543432720 -0600
@@ -24,6 +24,7 @@
 
 #include "defs.h"
 #include "reverse_route.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -ur before/rgbcolors.cc after/rgbcolors.cc
--- before/rgbcolors.cc	2026-10-06 12:24:15.527530006 -0600
+++ after/rgbcolors.cc	2026-10-06 12:24:15.543445210 -0600
@@ -28,6 +28,7 @@
 #include <QtGlobal>            // for qPrintable
 
 #include "defs.h"              // for gbFatal, color_to_bbggrr
+#include "src/core/logging.h"
 
 /*
  * Colors derived from http://www.w3.org/TR/SVG/types.html#ColorKeywords
diff -ur before/session.cc after/session.cc
--- before/session.cc	2026-10-06 12:24:15.527557806 -0600
+++ after/session.cc	2026-10-06 12:24:15.543475880 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "session.h"
+#include "src/core/logging.h"
 
 #include <QList>         // for QList
 
diff -ur before/shape.cc after/shape.cc
--- before/shape.cc	2026-10-06 12:24:15.527582143 -0600
+++ after/shape.cc	2026-10-06 12:24:15.543507887 -0600
@@ -149,7 +149,7 @@
   const int nFields = DBFGetFieldCount(ihandledb);
   for (int i = 0; i < nFields; i++) {
     DBFFieldType type = DBFGetFieldInfo(ihandledb, i, name, nullptr, nullptr);
-    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}\n", i, name, type);
+    gbLogWarning("Field Index: {:2}, Field Name: {:>12}, Field Type {}\n", i, name, gpsbabel::to_underlying(type));
   }
   gbLogFatal("\n");
 }
diff -ur before/skytraq.cc after/skytraq.cc
--- before/skytraq.cc	2026-10-06 12:24:15.527598780 -0600
+++ after/skytraq.cc	2026-10-06 12:24:15.543525649 -0600
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
diff -ur before/smplrout.cc after/smplrout.cc
--- before/smplrout.cc	2026-10-06 12:24:15.527620135 -0600
+++ after/smplrout.cc	2026-10-06 12:24:15.543547576 -0600
@@ -67,6 +67,7 @@
 #include "smplrout.h"
 #include "grtcirc.h"            // for gcdist, linedist, radtometers, linepart
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -ur before/sort.cc after/sort.cc
--- before/sort.cc	2026-10-06 12:24:15.527633905 -0600
+++ after/sort.cc	2026-10-06 12:24:15.543562440 -0600
@@ -27,6 +27,7 @@
 #include "defs.h"
 #include "geocache.h"           // for Geocache
 #include "src/core/datetime.h"  // for DateTime
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -ur before/src/core/matrix.cc after/src/core/matrix.cc
--- before/src/core/matrix.cc	2026-10-06 12:24:15.531913740 -0600
+++ after/src/core/matrix.cc	2026-10-06 12:24:15.548658405 -0600
@@ -25,6 +25,7 @@
 #include <QDebugStateSaver>  // for QDebugStateSaver
 
 #include "defs.h"            // For gbFatal
+#include "src/core/logging.h"
 
 Matrix::Matrix(int rows, int cols) : rows_(rows), cols_(cols), data_(rows * cols, 0.0) {}
 
diff -ur before/src/core/xmlstreamwriter.cc after/src/core/xmlstreamwriter.cc
--- before/src/core/xmlstreamwriter.cc	2026-10-06 12:24:15.531971388 -0600
+++ after/src/core/xmlstreamwriter.cc	2026-10-06 12:24:15.548727606 -0600
@@ -24,6 +24,7 @@
 #include <QtGlobal>                 // for QT_VERSION, QT_VERSION_CHECK
 
 #include "defs.h"
+#include "src/core/logging.h"
 
 // As this code began in C, we have several hundred places that write
 // c strings.  Add a test that the string contains anything useful
diff -ur before/stackfilter.cc after/stackfilter.cc
--- before/stackfilter.cc	2026-10-06 12:24:15.527646266 -0600
+++ after/stackfilter.cc	2026-10-06 12:24:15.543577152 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "stackfilter.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
@@ -144,8 +145,8 @@
   stack_elt* tmp_elt = nullptr;
 
   if (warnings_enabled && stack) {
-    gbWarning("Warning: leftover stack entries; "
-            "check command line for mistakes\n");
+    gbLogWarning("Warning: leftover stack entries; "
+                 "check command line for mistakes\n");
   }
   while (stack) {
     stack->waypts.flush();
diff -ur before/trackfilter.cc after/trackfilter.cc
--- before/trackfilter.cc	2026-10-06 12:24:15.527733170 -0600
+++ after/trackfilter.cc	2026-10-06 12:24:15.543671305 -0600
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
 
diff -ur before/transform.cc after/transform.cc
--- before/transform.cc	2026-10-06 12:24:15.527751344 -0600
+++ after/transform.cc	2026-10-06 12:24:15.543692794 -0600
@@ -26,6 +26,7 @@
 
 #include "defs.h"
 #include "transform.h"
+#include "src/core/logging.h"
 
 
 #if FILTERS_ENABLED
diff -ur before/unicsv.cc after/unicsv.cc
--- before/unicsv.cc	2026-10-06 12:24:15.527765594 -0600
+++ after/unicsv.cc	2026-10-06 12:24:15.543710391 -0600
@@ -422,7 +422,7 @@
       unicsv_fields_tab.last() = f.type;
 
       if (global_opts.debug_level) {
-        gbLogWarning("Interpreting column \"{}\" as {}({}).\n", gbLogCStr(value), gbLogCStr(f.name), f.type);
+        gbLogWarning("Interpreting column \"{}\" as {}({}).\n", gbLogCStr(value), gbLogCStr(f.name), gpsbabel::to_underlying(f.type));
       }
 
       /* handle some special items */
diff -ur before/units.cc after/units.cc
--- before/units.cc	2026-10-06 12:24:15.527801759 -0600
+++ after/units.cc	2026-10-06 12:24:15.543740525 -0600
@@ -21,6 +21,7 @@
 
 #include "defs.h"
 #include "units.h"
+#include "src/core/logging.h"
 
 
 void
diff -ur before/validate.cc after/validate.cc
--- before/validate.cc	2026-10-06 12:24:15.527849729 -0600
+++ after/validate.cc	2026-10-06 12:24:15.543792411 -0600
@@ -22,6 +22,7 @@
 
 #include "defs.h"
 #include "validate.h"
+#include "src/core/logging.h"
 
 #if FILTERS_ENABLED
 
diff -ur before/xcsv.cc after/xcsv.cc
--- before/xcsv.cc	2026-10-06 12:24:15.527955212 -0600
+++ after/xcsv.cc	2026-10-06 12:24:15.543904857 -0600
@@ -809,7 +809,7 @@
     break;
 
   default:
-    gbLogFatal("Unknown style directive: {} - {}\n", fmp.key.constData(), fmp.hashed_key);
+    gbLogFatal("Unknown style directive: {} - {}\n", fmp.key.constData(), gpsbabel::to_underlying(fmp.hashed_key));
     break;
   }
 }
EOJ
cp ./*.cc ./*.h after
cp jeeps/*.cc jeeps/*.h after/jeeps
cp src/core/*.cc src/core/*.h after/src/core
# did we miss any print specifier to place holder tranformations?
echo "++++++++++ possible untranslated print specifier ++++++++++"
grep -n -E 'gbLog(Debug|Warning|Info|Fatal)'  "${sources[@]}" | grep %
# do we have any usages of the original names left?
echo "============== possible usages that weren't transformed =============="
grep -n -E 'gb(Debug|Warning|Info|Fatal)'  "${sources[@]}" | grep -v FatalMsg\(\) | grep -v '#include'
echo "============== renaming back to original names =============="
# name them back
for file in "${sources[@]}"
do
  sed -E -i 's/gbLog(Debug|Fatal|Warning|Info)/gb\1/' "$file"
done
