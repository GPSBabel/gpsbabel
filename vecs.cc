/*
    Describe vectors containing file operations.

    Copyright (C) 2002, 2004, 2005, 2006, 2007 Robert Lipe, robertlipe+source@gpsbabel.org

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

 */

#include "vecs.h"

#include <QByteArray>          // for QByteArray
#include <QDebug>              // for QDebug
#include <QDir>                // for QDir, QDir::Files, QDir::Name
#include <QFileInfo>           // for QFileInfo
#include <QFileInfoList>       // for QFileInfoList
#include <QString>             // for QString
#include <QStringList>         // for QStringList
#include <QVector>             // for QVector
#include <Qt>                  // for CaseInsensitive
#include <QtGlobal>            // for qPrintable, qAsConst

#include <algorithm>           // for sort
#include <cassert>             // for assert
#include <cctype>              // for isdigit
#include <cstdio>              // for printf, putchar, sscanf

#include "defs.h"              // for arglist_t, ff_vecs_t, ff_cap, fatal, CSTR, ARGTYPE_TYPEMASK, case_ignore_strcmp, global_options, global_opts, warning, xfree, ARGTYPE_BOOL, ff_cap_read, ff_cap_write, ARGTYPE_HIDDEN, ff_type_internal, xstrdup, ARGTYPE_INT, ARGTYPE_REQUIRED, ARGTYPE_FLOAT
#include "dg-100.h"            // for Dg100FileFormat, Dg100SerialFormat, Dg200FileFormat, Dg200SerialFormat
#include "energympro.h"        // for EnergymproFormat
#include "exif.h"              // for ExifFormat
#include "f90g_track.h"        // for F90gTrackFormat
#include "format.h"            // for Format
#include "garmin_fit.h"        // for GarminFitFormat
#include "garmin_gpi.h"        // for GarminGPIFormat
#include "gbversion.h"         // for WEB_DOC_DIR
#include "gdb.h"               // for GdbFormat
#include "geojson.h"           // for GeoJsonFormat
#include "ggv_bin.h"           // for GgvBinFormat
#include "globalsat_sport.h"   // for GlobalsatSportFormat
#include "gpx.h"               // for GpxFormat
#include "gtrnctr.h"           // for GtrnctrFormat
#include "html.h"              // for HtmlFormat
#include "humminbird.h"        // for HumminbirdFormat, HumminbirdHTFormat
#include "inifile.h"           // for inifile_readstr
#include "kml.h"               // for KmlFormat
#include "legacyformat.h"      // for LegacyFormat
#include "lowranceusr.h"       // for LowranceusrFormat
#include "mapbar_track.h"      // for MapbarTrackFormat
#include "mapfactor.h"         // for MapfactorFormat
#include "mynav.h"             // for MyNavFormat
#include "nmea.h"              // for NmeaFormat
#include "osm.h"               // for OsmFormat
#include "qstarz_bl_1000.h"    // for QstarzBL1000Format
#include "random.h"            // for RandomFormat
#include "shape.h"             // for ShapeFormat
#include "skytraq.h"           // for MinihomerFormat, SkytraqFormat, SkytraqfileFormat
#include "src/core/logging.h"  // for Warning, FatalMsg
#include "subrip.h"            // for SubripFormat
#include "tef_xml.h"           // for TefXMLFormat
#include "teletype.h"          // for TeletypeFormat
#include "text.h"              // for TextFormat
#include "unicsv.h"            // for UnicsvFormat
#include "wintec_tes.h"        // for WintecTesFormat
#include "xcsv.h"              // for XcsvStyle, XcsvFormat


extern ff_vecs_t geo_vecs;
extern ff_vecs_t mag_svecs;
extern ff_vecs_t mag_fvecs;
extern ff_vecs_t magX_fvecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t ozi_vecs;
#if MAXIMAL_ENABLED
extern ff_vecs_t holux_vecs;
extern ff_vecs_t tpg_vecs;
extern ff_vecs_t tpo2_vecs;
extern ff_vecs_t tpo3_vecs;
extern ff_vecs_t easygps_vecs;
extern ff_vecs_t saroute_vecs;
extern ff_vecs_t igc_vecs;
extern ff_vecs_t brauniger_iq_vecs;
extern ff_vecs_t mtk_vecs;
extern ff_vecs_t mtk_fvecs;
extern ff_vecs_t mtk_m241_vecs;
extern ff_vecs_t mtk_m241_fvecs;
extern ff_vecs_t mtk_locus_vecs;
#endif // MAXIMAL_ENABLED
extern ff_vecs_t wbt_svecs;
#if MAXIMAL_ENABLED
extern ff_vecs_t wbt_fvecs;
//extern ff_vecs_t wbt_fvecs;
extern ff_vecs_t hiketech_vecs;
extern ff_vecs_t glogbook_vecs;
extern ff_vecs_t vcf_vecs;
extern ff_vecs_t google_dir_vecs;
extern ff_vecs_t tomtom_vecs;
extern ff_vecs_t bcr_vecs;
extern ff_vecs_t ignr_vecs;
extern ff_vecs_t gtm_vecs;
extern ff_vecs_t gpssim_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t garmin_txt_vecs;
#endif // CSVFMTS_ENABLED
extern ff_vecs_t dmtlog_vecs;
extern ff_vecs_t raymarine_vecs;
extern ff_vecs_t ggv_log_vecs;
extern ff_vecs_t lmx_vecs;
extern ff_vecs_t xol_vecs;
extern ff_vecs_t navilink_vecs;
extern ff_vecs_t ik3d_vecs;
extern ff_vecs_t destinator_poi_vecs;
extern ff_vecs_t destinator_itn_vecs;
extern ff_vecs_t destinator_trl_vecs;
extern ff_vecs_t igo8_vecs;
extern ff_vecs_t mapasia_tr7_vecs;
extern ff_vecs_t gnav_trl_vecs;
extern ff_vecs_t navitel_trk_vecs;
extern ff_vecs_t ggv_ovl_vecs;
extern ff_vecs_t itracku_vecs;
extern ff_vecs_t itracku_fvecs;
extern ff_vecs_t sbp_vecs;
extern ff_vecs_t sbn_vecs;
extern ff_vecs_t mmo_vecs;
extern ff_vecs_t v900_vecs;
extern ff_vecs_t enigma_vecs;
extern ff_vecs_t format_garmin_xt_vecs;
#endif // MAXIMAL_ENABLED

#define MYNAME "vecs"

struct Vecs::Impl
{
  /*
   * Having these LegacyFormat instances be non-static data members
   * prevents the static initialization order fiasco because
   * the static vec that is used to construct a legacy format
   * instance is guaranteed to have already constructed when an instance
   * of this class is constructed.
   */
#if CSVFMTS_ENABLED
  XcsvFormat xcsv_fmt;
#endif // CSVFMTS_ENABLED
  LegacyFormat geo_fmt {geo_vecs};
  GpxFormat gpx_fmt;
  LegacyFormat mag_sfmt {mag_svecs};
  LegacyFormat mag_ffmt {mag_fvecs};
  LegacyFormat magX_ffmt {magX_fvecs};
  LegacyFormat garmin_fmt {garmin_vecs};
  GdbFormat gdb_fmt;
  NmeaFormat nmea_fmt;
  LegacyFormat ozi_fmt {ozi_vecs};
  KmlFormat kml_fmt;
#if MAXIMAL_ENABLED
  LowranceusrFormat lowranceusr_fmt;
  LegacyFormat holux_fmt {holux_vecs};
  LegacyFormat tpg_fmt {tpg_vecs};
  LegacyFormat tpo2_fmt {tpo2_vecs};
  LegacyFormat tpo3_fmt {tpo3_vecs};
  LegacyFormat easygps_fmt {easygps_vecs};
  LegacyFormat saroute_fmt {saroute_vecs};
#if SHAPELIB_ENABLED
  ShapeFormat shape_fmt;
#endif
  TextFormat text_fmt;
  HtmlFormat html_fmt;
  LegacyFormat igc_fmt {igc_vecs};
  LegacyFormat brauniger_iq_fmt {brauniger_iq_vecs};
  LegacyFormat mtk_fmt {mtk_vecs};
  LegacyFormat mtk_ffmt {mtk_fvecs};
  LegacyFormat mtk_m241_fmt {mtk_m241_vecs};
  LegacyFormat mtk_m241_ffmt {mtk_m241_fvecs};
  LegacyFormat mtk_locus_fmt {mtk_locus_vecs};
#endif // MAXIMAL_ENABLED
  LegacyFormat wbt_sfmt {wbt_svecs};
#if MAXIMAL_ENABLED
  LegacyFormat wbt_ffmt {wbt_fvecs};
//LegacyFormat wbt_ffmt {wbt_fvecs};
  LegacyFormat hiketech_fmt {hiketech_vecs};
  LegacyFormat glogbook_fmt {glogbook_vecs};
  LegacyFormat vcf_fmt {vcf_vecs};
  LegacyFormat google_dir_fmt {google_dir_vecs};
  LegacyFormat tomtom_fmt {tomtom_vecs};
  TefXMLFormat tef_xml_fmt;
  LegacyFormat bcr_fmt {bcr_vecs};
  LegacyFormat ignr_fmt {ignr_vecs};
  UnicsvFormat unicsv_fmt;
  LegacyFormat gtm_fmt {gtm_vecs};
  LegacyFormat gpssim_fmt {gpssim_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat garmin_txt_fmt {garmin_txt_vecs};
#endif // CSVFMTS_ENABLED
  GtrnctrFormat gtc_fmt;
  LegacyFormat dmtlog_fmt {dmtlog_vecs};
  LegacyFormat raymarine_fmt {raymarine_vecs};
  LegacyFormat ggv_log_fmt {ggv_log_vecs};
  GarminGPIFormat garmin_gpi_fmt;
  LegacyFormat lmx_fmt {lmx_vecs};
  RandomFormat random_fmt;
  LegacyFormat xol_fmt {xol_vecs};
  Dg100SerialFormat dg100_fmt;
  Dg100FileFormat dg100_ffmt;
  Dg200SerialFormat dg200_fmt;
  Dg200FileFormat dg200_ffmt;
  LegacyFormat navilink_fmt {navilink_vecs};
  LegacyFormat ik3d_fmt {ik3d_vecs};
  OsmFormat osm_fmt;
  LegacyFormat destinator_poi_fmt {destinator_poi_vecs};
  LegacyFormat destinator_itn_fmt {destinator_itn_vecs};
  LegacyFormat destinator_trl_fmt {destinator_trl_vecs};
  ExifFormat exif_fmt;
  LegacyFormat igo8_fmt {igo8_vecs};
  HumminbirdFormat humminbird_fmt;
  HumminbirdHTFormat humminbird_ht_fmt;
  LegacyFormat mapasia_tr7_fmt {mapasia_tr7_vecs};
  LegacyFormat gnav_trl_fmt {gnav_trl_vecs};
  LegacyFormat navitel_trk_fmt {navitel_trk_vecs};
  LegacyFormat ggv_ovl_fmt {ggv_ovl_vecs};
  LegacyFormat itracku_fmt {itracku_vecs};
  LegacyFormat itracku_ffmt {itracku_fvecs};
  LegacyFormat sbp_fmt {sbp_vecs};
  LegacyFormat sbn_fmt {sbn_vecs};
  LegacyFormat mmo_fmt {mmo_vecs};
  LegacyFormat v900_fmt {v900_vecs};
  LegacyFormat enigma_fmt {enigma_vecs};
  SkytraqFormat skytraq_fmt;
  TeletypeFormat teletype_fmt;
  SkytraqfileFormat skytraq_ffmt;
  MinihomerFormat miniHomer_fmt;
  WintecTesFormat wintec_tes_fmt;
  SubripFormat subrip_fmt;
  LegacyFormat format_garmin_xt_fmt {format_garmin_xt_vecs};
  GarminFitFormat format_fit_fmt;
  MapbarTrackFormat mapbar_track_fmt;
  F90gTrackFormat f90g_track_fmt;
  MapfactorFormat mapfactor_fmt;
  EnergymproFormat energympro_fmt;
  MyNavFormat mynav_fmt;
  GeoJsonFormat geojson_fmt;
  GgvBinFormat ggv_bin_fmt;
  GlobalsatSportFormat globalsat_sport_fmt;
  QstarzBL1000Format qstarz_bl_1000_fmt;
#endif // MAXIMAL_ENABLED

  const QVector<vecs_t> vec_list {
#if CSVFMTS_ENABLED
    /* XCSV must be the first entry in this table. */
    {
      &xcsv_fmt,
      "xcsv",
      "? Character Separated Values",
      nullptr,
      nullptr,
    },
#endif
    {
      &geo_fmt,
      "geo",
      "Geocaching.com .loc",
      "loc",
      nullptr,
    },
    {
      &gpx_fmt,
      "gpx",
      "GPX XML",
      "gpx",
      nullptr,
    },
    {
      &mag_sfmt,
      "magellan",
      "Magellan serial protocol",
      nullptr,
      nullptr,
    },
    {
      &mag_ffmt,
      "magellan",
      "Magellan SD files (as for Meridian)",
      nullptr,
      nullptr,
    },
    {
      &magX_ffmt,
      "magellanx",
      "Magellan SD files (as for eXplorist)",
      "upt",
      nullptr,
    },
    {
      &garmin_fmt,
      "garmin",
      "Garmin serial/USB protocol",
      nullptr,
      nullptr,
    },
    {
      &gdb_fmt,
      "gdb",
      "Garmin MapSource - gdb",
      "gdb",
      nullptr,
    },
    {
      &nmea_fmt,
      "nmea",
      "NMEA 0183 sentences",
      nullptr,
      nullptr,
    },
    {
      &ozi_fmt,
      "ozi",
      "OziExplorer",
      nullptr,
      nullptr,
    },
    {
      &kml_fmt,
      "kml",
      "Google Earth (Keyhole) Markup Language",
      "kml",
      nullptr,
    },
#if MAXIMAL_ENABLED
    {
      &lowranceusr_fmt,
      "lowranceusr",
      "Lowrance USR",
      "usr",
      nullptr,
    },
    {
      &holux_fmt,
      "holux",
      "Holux (gm-100) .wpo Format",
      "wpo",
      nullptr,
    },
    {
      &tpg_fmt,
      "tpg",
      "National Geographic Topo .tpg (waypoints)",
      "tpg",
      nullptr,
    },
    {
      &tpo2_fmt,
      "tpo2",
      "National Geographic Topo 2.x .tpo",
      "tpo",
      nullptr,
    },
    {
      &tpo3_fmt,
      "tpo3",
      "National Geographic Topo 3.x/4.x .tpo",
      "tpo",
      nullptr,
    },
    {
      &easygps_fmt,
      "easygps",
      "EasyGPS binary format",
      "loc",
      nullptr,
    },
    {
      &saroute_fmt,
      "saroute",
      "DeLorme Street Atlas Route",
      "anr",
      nullptr,
    },
#if SHAPELIB_ENABLED
    {
      &shape_fmt,
      "shape",
      "ESRI shapefile",
      "shp",
      nullptr,
    },
#endif
    {
      &text_fmt,
      "text",
      "Textual Output",
      "txt",
      nullptr,
    },
    {
      &html_fmt,
      "html",
      "HTML Output",
      "html",
      nullptr,
    },
    {
      &igc_fmt,
      "igc",
      "FAI/IGC Flight Recorder Data Format",
      nullptr,
      nullptr,
    },
    {
      &brauniger_iq_fmt,
      "baroiq",
      "Brauniger IQ Series Barograph Download",
      nullptr,
      nullptr,
    },
    {
      &mtk_fmt,
      "mtk",
      "MTK Logger (iBlue 747,Qstarz BT-1000,...) download",
      nullptr,
      nullptr,
    },
    {
      &mtk_ffmt,
      "mtk-bin",
      "MTK Logger (iBlue 747,...) Binary File Format",
      "bin",
      nullptr,
    },
    {
      &mtk_m241_fmt,
      "m241",
      "Holux M-241 (MTK based) download",
      nullptr,
      nullptr,
    },
    {
      &mtk_m241_ffmt,
      "m241-bin",
      "Holux M-241 (MTK based) Binary File Format",
      "bin",
      nullptr,
    },
    {
      &mtk_locus_fmt,
      "mtk_locus",
      "MediaTek Locus",
      nullptr,
      nullptr,
    },
#endif // MAXIMAL_ENABLED
    {
      &wbt_sfmt,
      "wbt",
      "Wintec WBT-100/200 GPS Download",
      nullptr,
      nullptr,
    },
#if MAXIMAL_ENABLED
    {
      &wbt_ffmt,
      "wbt-bin",
      "Wintec WBT-100/200 Binary File Format",
      "bin",
      nullptr,
    },
    {
      &wbt_ffmt,
      "wbt-tk1",
      "Wintec WBT-201/G-Rays 2 Binary File Format",
      "tk1",
      nullptr,
    },
    {
      &hiketech_fmt,
      "hiketech",
      "HikeTech",
      "gps",
      nullptr,
    },
    {
      &glogbook_fmt,
      "glogbook",
      "Garmin Logbook XML",
      "xml",
      nullptr,
    },
    {
      &vcf_fmt,
      "vcard",
      "Vcard Output (for iPod)",
      "vcf",
      nullptr,
    },
    {
      &google_dir_fmt,
      "googledir",
      "Google Directions XML",
      "xml",
      nullptr,
    },
    {
      &tomtom_fmt,
      "tomtom",
      "TomTom POI file (.ov2)",
      "ov2",
      nullptr,
    },
    {
      &tef_xml_fmt,
      "tef",
      "Map&Guide 'TourExchangeFormat' XML",
      "xml",
      nullptr,
    },
    {
      &bcr_fmt,
      "bcr",
      "Motorrad Routenplaner (Map&Guide) .bcr files",
      "bcr",
      nullptr,
    },
    {
      &ignr_fmt,
      "ignrando",
      "IGN Rando track files",
      "rdn",
      nullptr,
    },
    {
      &unicsv_fmt,
      "unicsv",
      "Universal csv with field structure in first line",
      nullptr,
      nullptr,
    },
    {
      &gtm_fmt,
      "gtm",
      "GPS TrackMaker",
      "gtm",
      nullptr,
    },
    {
      &gpssim_fmt,
      "gpssim",
      "Franson GPSGate Simulation",
      "gpssim",
      nullptr,
    },
#if CSVFMTS_ENABLED
    {
      &garmin_txt_fmt,
      "garmin_txt",
      "Garmin MapSource - txt (tab delimited)",
      "txt",
      nullptr,
    },
#endif // CSVFMTS_ENABLED
    {
      &gtc_fmt,
      "gtrnctr",
      "Garmin Training Center (.tcx/.crs/.hst/.xml)",
      "tcx/crs/hst/xml",
      nullptr,
    },
    {
      &dmtlog_fmt,
      "dmtlog",
      "TrackLogs digital mapping (.trl)",
      "trl",
      nullptr,
    },
    {
      &raymarine_fmt,
      "raymarine",
      "Raymarine Waypoint File (.rwf)",
      "rwf",
      nullptr,
    },
    {
      &ggv_log_fmt,
      "ggv_log",
      "Geogrid-Viewer tracklogs (.log)",
      "log",
      nullptr,
    },
    {
      &garmin_gpi_fmt,
      "garmin_gpi",
      "Garmin Points of Interest (.gpi)",
      "gpi",
      nullptr,
    },
    {
      &lmx_fmt,
      "lmx",
      "Nokia Landmark Exchange",
      nullptr,
      nullptr,
    },
    {
      &random_fmt,
      "random",
      "Internal GPS data generator",
      nullptr,
      nullptr,
    },
    {
      &xol_fmt,
      "xol",
      "Swiss Map 25/50/100 (.xol)",
      "xol",
      nullptr,
    },
    {
      &dg100_fmt,
      "dg-100",
      "GlobalSat DG-100/BT-335 Download",
      nullptr,
      nullptr,
    },
    {
      &dg100_ffmt,
      "dg-100-bin",
      "GlobalSat DG-100/BT-335 Binary File",
      nullptr,
      nullptr,
    },
    {
      &dg200_fmt,
      "dg-200",
      "GlobalSat DG-200 Download",
      nullptr,
      nullptr,
    },
    {
      &dg200_ffmt,
      "dg-200-bin",
      "GlobalSat DG-200 Binary File",
      nullptr,
      nullptr,
    },
    {
      &navilink_fmt,
      "navilink",
      "NaviGPS GT-11/BGT-11 Download",
      nullptr,
      nullptr,
    },
    {
      &ik3d_fmt,
      "ik3d",
      "MagicMaps IK3D project file (.ikt)",
      "ikt",
      nullptr,
    },
    {
      &osm_fmt,
      "osm",
      "OpenStreetMap data files",
      "osm",
      nullptr,
    },
    {
      &destinator_poi_fmt,
      "destinator_poi",
      "Destinator Points of Interest (.dat)",
      "dat",
      nullptr,
    },
    {
      &destinator_itn_fmt,
      "destinator_itn",
      "Destinator Itineraries (.dat)",
      "dat",
      nullptr,
    },
    {
      &destinator_trl_fmt,
      "destinator_trl",
      "Destinator TrackLogs (.dat)",
      "dat",
      nullptr,
    },
    {
      &exif_fmt,
      "exif",
      "Embedded Exif-GPS data (.jpg)",
      "jpg",
      nullptr,
    },
    {
      &igo8_fmt,
      "igo8",
      "IGO8 .trk",
      "trk",
      nullptr,
    },
    {
      &humminbird_fmt,
      "humminbird",
      "Humminbird waypoints and routes (.hwr)",
      "hwr",
      nullptr,
    },
    {
      &humminbird_ht_fmt,
      "humminbird_ht",
      "Humminbird tracks (.ht)",
      "ht",
      nullptr,
    },
    {
      &mapasia_tr7_fmt,
      "mapasia_tr7",
      "MapAsia track file (.tr7)",
      "tr7",
      nullptr,
    },
    {
      &gnav_trl_fmt,
      "gnav_trl",
      "Google Navigator Tracklines (.trl)",
      "trl",
      nullptr,
    },
    {
      &navitel_trk_fmt,
      "navitel_trk",
      "Navitel binary track (.bin)",
      "bin",
      nullptr,
    },
    {
      &ggv_ovl_fmt,
      "ggv_ovl",
      "Geogrid-Viewer ascii overlay file (.ovl)",
      "ovl",
      nullptr,
    },
    {
      &itracku_fmt,
      "itracku",
      "XAiOX iTrackU Logger",
      nullptr,
      nullptr,
    },
    {
      &itracku_ffmt,
      "itracku-bin",
      "XAiOX iTrackU Logger Binary File Format",
      "bin",
      nullptr,
    },
    {
      &sbp_fmt,
      "sbp",
      "NaviGPS GT-31/BGT-31 datalogger (.sbp)",
      "sbp",
      nullptr,
    },
    {
      &sbn_fmt,
      "sbn",
      "NaviGPS GT-31/BGT-31 SiRF binary logfile (.sbn)",
      "sbn",
      nullptr,
    },
    {
      &mmo_fmt,
      "mmo",
      "Memory-Map Navigator overlay files (.mmo)",
      "mmo",
      nullptr,
    },
    {
      &v900_fmt,
      "v900",
      "Columbus/Visiontac V900 files (.csv)",
      nullptr,
      nullptr,
    },
    {
      &enigma_fmt,
      "enigma",
      "Enigma binary waypoint file (.ert)",
      "ert",
      nullptr,
    },
    {
      &skytraq_fmt,
      "skytraq",
      "SkyTraq Venus based loggers (download)",
      nullptr,
      nullptr,
    },
    {
      &teletype_fmt,
      "teletype",
      "Teletype [ Get Jonathon Johnson to describe",
      nullptr,
      nullptr,
    },
    {
      &skytraq_ffmt,
      "skytraq-bin",
      "SkyTraq Venus based loggers Binary File Format",
      "bin",
      nullptr,
    },
    {
      &miniHomer_fmt,
      "miniHomer",
      "MiniHomer, a skyTraq Venus 6 based logger (download tracks, waypoints and get/set POI)",
      nullptr,
      nullptr,
    },
    {
      &wintec_tes_fmt,
      "wintec_tes",
      "Wintec TES file",
      "tes",
      nullptr,
    },
    {
      &subrip_fmt,
      "subrip",
      "SubRip subtitles for video mapping (.srt)",
      "srt",
      nullptr,
    },
    {
      &format_garmin_xt_fmt,
      "garmin_xt",
      "Mobile Garmin XT Track files",
      nullptr,
      nullptr,
    },
    {
      &format_fit_fmt,
      "garmin_fit",
      "Flexible and Interoperable Data Transfer (FIT) Activity file",
      "fit",
      nullptr,
    },
    {
      &mapbar_track_fmt,
      "mapbar",
      "Mapbar (China) navigation track for Sonim Xp3300",
      "trk",
      nullptr,
    },
    {
      &f90g_track_fmt,
      "f90g",
      "F90G Automobile DVR GPS log file",
      "map",
      nullptr,
    },
    {
      &mapfactor_fmt,
      "mapfactor",
      "Mapfactor Navigator",
      "xml",
      nullptr,
    },
    {
      &energympro_fmt,
      "energympro",
      "Energympro GPS training watch",
      "cpo",
      nullptr,
    },
    {
      &mynav_fmt,
      "mynav",
      "MyNav TRC format",
      "trc",
      nullptr,
    },
    {
      &geojson_fmt,
      "geojson",
      "GeoJson",
      "json",
      nullptr,
    },
    {
      &ggv_bin_fmt,
      "ggv_bin",
      "Geogrid-Viewer binary overlay file (.ovl)",
      "ovl",
      nullptr,
    },
    {
      &globalsat_sport_fmt,
      "globalsat",
      "GlobalSat GH625XT GPS training watch",
      nullptr,
      nullptr,
    },
    {
      &qstarz_bl_1000_fmt,
      "qstarz_bl-1000",
      "Qstarz BL-1000",
      nullptr,
      nullptr,
    }
#endif // MAXIMAL_ENABLED
  };
};

Vecs& Vecs::Instance()
{
  static Impl impl;
  static Vecs instance(&impl);
  return instance;
}

/*
 * When we modify an element on the list we need to be careful
 * that we are not modifying a Qt COW copy.
 * Qt has an undocumented but public member function isDetached().
 * If the list is detached it implies it is not shared, then functions
 * then might detach, like the iterator begin which is implicitly used
 * in the range based for loop, won't cause a copy to be created.
 * We can make sure this is true for at least our regression cases
 * with assertions.
 * There is an odd situation that an empty QVector is not detached,
 * so we have to exclude this from the check.
 * The possibility of detachment is also why the type of element
 * on the list must be default constructable. This is why we have
 * to supply a default for any const members of arglist_t.  Without
 * the default the default constructor would be implicitly deleted.
 */

void Vecs::init_vecs()
{
  for (const auto& vec : d_ptr_->vec_list) {
    QVector<arglist_t>* args = vec.vec->get_args();
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        arg.argvalptr = nullptr;
        if (arg.argval) {
          *arg.argval = nullptr;
        }
      }
    }
  }
  style_list = create_style_vec();
}

int Vecs::is_integer(const char* c)
{
  return isdigit(c[0]) || ((c[0] == '+' || c[0] == '-') && isdigit(c[1]));
}

void Vecs::exit_vecs()
{
  for (const auto& vec : d_ptr_->vec_list) {
    (vec.vec->exit)();
    QVector<arglist_t>* args = vec.vec->get_args();
    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        if (arg.argvalptr) {
          xfree(arg.argvalptr);
          *arg.argval = arg.argvalptr = nullptr;
        }
      }
    }
  }
  style_list.clear();
  style_list.squeeze();
}

void Vecs::assign_option(const QString& module, arglist_t* arg, const char* val)
{
  const char* c;

  if (arg->argval == nullptr) {
    fatal("%s: No local variable defined for option \"%s\"!", qPrintable(module), arg->argstring);
  }

  if (arg->argvalptr != nullptr) {
    xfree(arg->argvalptr);
    arg->argvalptr = nullptr;
  }
  if (arg->argval) {
    *arg->argval = nullptr;
  }

  if (val == nullptr) {
    return;
  }

  // Fixme - this is probably somewhere between wrong and less than great.  If you have an option "foo"
  // and want to set it to the value "foo", this code will prevent that from happening, but we seem to have
  // code all over the place that relies on this. :-/
  if (case_ignore_strcmp(val, arg->argstring) == 0) {
    c = "";
  } else {
    c = val;
  }

  switch (arg->argtype & ARGTYPE_TYPEMASK) {
  case ARGTYPE_INT:
    if (*c == '\0') {
      c = "0";
    } else {
      int test;
      if (1 != sscanf(c, "%d", &test)) {
        fatal("%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
      }
    }
    break;
  case ARGTYPE_FLOAT:
    if (*c == '\0') {
      c = "0";
    } else {
      double test;
      if (1 != sscanf(c, "%lf", &test)) {
        fatal("%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
      }
    }
    break;
  case ARGTYPE_BOOL:
    if (*c == '\0') {
      c = "1";
    } else {
      switch (*c) {
      case 'Y':
      case 'y':
        c = "1";
        break;
      case 'N':
      case 'n':
        c = "0";
        break;
      default:
        if (isdigit(*c)) {
          if (*c == '0') {
            c = "0";
          } else {
            c = "1";
          }
        } else {
          warning(MYNAME ": Invalid logical value '%s' (%s)!\n", c, qPrintable(module));
          c = "0";
        }
        break;
      }
    }
    break;
  }

  /* for bool options without default: don't set argval if "FALSE" */

  if (((arg->argtype & ARGTYPE_TYPEMASK) == ARGTYPE_BOOL) &&
      (*c == '0') && (arg->defaultvalue == nullptr)) {
    return;
  }
  *arg->argval = arg->argvalptr = xstrdup(c);
}

void Vecs::disp_vec_options(const QString& vecname, const QVector<arglist_t>* args)
{
  if (args) {
    for (const auto& arg : *args) {
      if (*arg.argval && arg.argval) {
        printf("options: module/option=value: %s/%s=\"%s\"",
               qPrintable(vecname), arg.argstring, *arg.argval);
        if (arg.defaultvalue && (case_ignore_strcmp(arg.defaultvalue, *arg.argval) == 0)) {
          printf(" (=default)");
        }
        printf("\n");
      }
    }
  }
}

void Vecs::validate_options(const QStringList& options, const QVector<arglist_t>* args, const QString& name)
{
  for (const auto& option : options) {
    const QString option_name = option.left(option.indexOf('='));
    bool valid = false;
    if (args) {
      for (const auto& arg : *args) {
        if (option_name.compare(arg.argstring, Qt::CaseInsensitive) == 0) {
          valid = true;
          break;
        }
      }
    }
    if (!valid) {
      warning("'%s' is an unknown option to %s.\n", qPrintable(option_name), qPrintable(name));
    }
  }
}

Format* Vecs::find_vec(const QString& vecname)
{
  QStringList options = vecname.split(',');
  if (options.isEmpty()) {
    fatal("A format name is required.\n");
  }
  const QString svecname = options.takeFirst();

  for (const auto& vec : d_ptr_->vec_list) {
    if (svecname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    QVector<arglist_t>* args = vec.vec->get_args();

    validate_options(options, args, vec.name);

    if (args && !args->isEmpty()) {
      assert(args->isDetached());
      for (auto& arg : *args) {
        if (!options.isEmpty()) {
          const QString opt = get_option(options, arg.argstring);
          if (!opt.isNull()) {
            assign_option(vec.name, &arg, CSTR(opt));
            continue;
          }
        }
        QString qopt = inifile_readstr(global_opts.inifile, vec.name, arg.argstring);
        if (qopt.isNull()) {
          qopt = inifile_readstr(global_opts.inifile, "Common format settings", arg.argstring);
        }
        if (qopt.isNull()) {
          assign_option(vec.name, &arg, arg.defaultvalue);
        } else {
          assign_option(vec.name, &arg, CSTR(qopt));
        }
      }
    }

    if (global_opts.debug_level >= 1) {
      disp_vec_options(vec.name, args);
    }

#if CSVFMTS_ENABLED
    /*
     * If this happens to be xcsv,style= and it was preceeded by an xcsv
     * format that utilized an internal style file, then we need to let
     * xcsv know the internal style file is no longer in play.
     */
    d_ptr_->xcsv_fmt.xcsv_setup_internal_style(nullptr);
#endif // CSVFMTS_ENABLED
    vec.vec->set_name(vec.name);	/* needed for session information */
    vec.vec->set_argstring(vecname);  /* needed for positional parameters */
    return vec.vec;

  }

  /*
   * Didn't find it in the table of "real" file types, so plan B
   * is to search the list of xcsv styles.
   */
  for (const auto& svec : qAsConst(style_list)) {
    if (svecname.compare(svec.name,  Qt::CaseInsensitive) != 0) {
      continue;
    }

    QVector<arglist_t>* xcsv_args = d_ptr_->vec_list.at(0).vec->get_args();

    validate_options(options, xcsv_args, svec.name);

    if (xcsv_args && !xcsv_args->isEmpty()) {
      assert(xcsv_args->isDetached());
      for (auto& arg : *xcsv_args) {
        if (!options.isEmpty()) {
          const QString opt = get_option(options, arg.argstring);
          if (!opt.isNull()) {
            assign_option(svec.name, &arg, CSTR(opt));
            continue;
          }
        }
        QString qopt = inifile_readstr(global_opts.inifile, svec.name, arg.argstring);
        if (qopt.isNull()) {
          qopt = inifile_readstr(global_opts.inifile, "Common format settings", arg.argstring);
        }
        if (qopt.isNull()) {
          assign_option(svec.name, &arg, arg.defaultvalue);
        } else {
          assign_option(svec.name, &arg, CSTR(qopt));
        }
      }
    }

    if (global_opts.debug_level >= 1) {
      disp_vec_options(svec.name, xcsv_args);
    }
#if CSVFMTS_ENABLED
    d_ptr_->xcsv_fmt.xcsv_setup_internal_style(svec.style_filename);
#endif // CSVFMTS_ENABLED

    d_ptr_->vec_list[0].vec->set_name(svec.name);	/* needed for session information */
    d_ptr_->vec_list[0].vec->set_argstring(vecname);  /* needed for positional parameters */
    return d_ptr_->vec_list[0].vec;
  }

  /*
   * Not found.
   */
  return nullptr;
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
QString Vecs::get_option(const QStringList& options, const char* argname)
{
  QString rval;

  for (const auto& option : options) {
    int split = option.indexOf('=');
    const QString option_name = option.left(split);
    if (option_name.compare(argname, Qt::CaseInsensitive) == 0) {
      /*
       * If we have something of the form "foo=bar"
       * return "bar".   Otherwise, we assume we have
       * simply "foo" so we return that.
       */
      if (split >= 0) { // we found an '='s.
        rval = option.mid(split + 1); // not null, possibly empty
        assert(!rval.isNull());
        break;
      } else {
        rval = option_name; // not null, possibly empty.
        assert(!rval.isNull());
        break;
      }
    }
  }
  return rval;
}

QVector<Vecs::style_vec_t> Vecs::create_style_vec()
{
  QString styledir(":/style");
  QDir dir(styledir);
  if (!dir.isReadable()) {
    fatal(FatalMsg() << "style directory" << QFileInfo(styledir).absoluteFilePath() << "not readable.");
  }

  dir.setNameFilters(QStringList("*.style"));
  dir.setFilter(QDir::Files);
  dir.setSorting(QDir::Name);
  QFileInfoList fileinfolist = dir.entryInfoList();
  QVector<style_vec_t> slist;
  for (const auto& fileinfo : fileinfolist) {
    if (!fileinfo.isReadable()) {
      fatal(FatalMsg() << "Cannot open style file" << fileinfo.absoluteFilePath() << ".");
    }

    style_vec_t entry;
    entry.name = fileinfo.baseName();
    entry.style_filename = fileinfo.filePath();
    slist.append(entry);
  }
  return slist;
}

/*
 * Gather information relevant to serialization from the
 * vecs and style lists.  Sort and return the information.
 */
QVector<Vecs::vecinfo_t> Vecs::sort_and_unify_vecs() const
{
  QVector<vecinfo_t> svp;
  svp.reserve(d_ptr_->vec_list.size() + style_list.size());

  /* Gather relevant information for normal formats. */
  for (const auto& vec : d_ptr_->vec_list) {
    vecinfo_t info;
    info.name = vec.name;
    info.desc = vec.desc;
    info.extensions = vec.extensions;
    if (vec.parent.isEmpty()) {
      info.parent = vec.name;
    } else {
      info.parent = vec.parent;
    }
    info.type = vec.vec->get_type();
    info.cap = vec.vec->get_cap();
    const QVector<arglist_t>* args = vec.vec->get_args();
    if (args != nullptr) {
      for (const auto& arg : *args) {
        info.arginfo.append(arginfo_t(arg));
      }
    }
    svp.append(info);
  }

#if CSVFMTS_ENABLED
  /* The style formats are based on the xcsv format,
   * Make sure we know which entry in the vector list that is.
   */
  assert(d_ptr_->vec_list.at(0).name.compare("xcsv", Qt::CaseInsensitive) == 0);
  /* The style formats use a modified xcsv argument list that doesn't include
   * the option to set the style file.  Make sure we know which entry in
   * the argument list that is.
   */
  assert(case_ignore_strcmp(d_ptr_->vec_list.at(0).vec->get_args()->at(0).helpstring,
                            "Full path to XCSV style file") == 0);

  /* Gather the relevant info for the style based formats. */
  for (const auto& svec : style_list) {
    XcsvStyle style = XcsvStyle::xcsv_read_style(svec.style_filename);
    vecinfo_t info;
    info.name = svec.name;
    info.desc = style.description;
    info.extensions = style.extension;
    info.parent = "xcsv";
    info.type = style.type;
    info.cap.fill(ff_cap_none, 3);
    switch (style.datatype) {
    case unknown_gpsdata:
    case wptdata:
      info.cap[ff_cap_rw_wpt] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case trkdata:
      info.cap[ff_cap_rw_trk] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case rtedata:
      info.cap[ff_cap_rw_rte] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    default:
      ;
    }
    /* Skip over the first help entry of the xcsv args.
     * We don't want to expose the
     * 'Full path to XCSV style file' argument to any
     * GUIs for an internal format.
     */
    const QVector<arglist_t>* args = d_ptr_->vec_list.at(0).vec->get_args();
    if (args != nullptr) {
      bool first = true;
      for (const auto& arg : *args) {
        if (!first) {
          info.arginfo.append(arginfo_t(arg));
        }
        first = false;
      }
    }
    svp.append(info);
  }
#endif // CSVFMTS_ENABLED

  /*
   *  Display the available formats in a format that's easy for humans to
   *  parse for help on available command line options.
   */
  auto alpha = [](const vecinfo_t& a, const vecinfo_t& b)->bool {
    return QString::compare(a.desc, b.desc, Qt::CaseInsensitive) < 0;
  };

  /* Now that we have everything in an array, alphabetize them */
  std::sort(svp.begin(), svp.end(), alpha);

  return svp;
}

#define VEC_FMT "	%-20.20s  %-.50s\n"

void Vecs::disp_vecs() const
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vec.type == ff_type_internal)  {
      continue;
    }
    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arginfo_t> args = vec.arginfo;
    for (const auto& arg : args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("	  %-18.18s    %s%-.50s %s\n",
               qPrintable(arg.argstring),
               (arg.argtype & ARGTYPE_TYPEMASK) ==
               ARGTYPE_BOOL ? "(0/1) " : "",
               qPrintable(arg.helpstring),
               (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

void Vecs::disp_vec(const QString& vecname) const
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vecname.compare(vec.name, Qt::CaseInsensitive) != 0)  {
      continue;
    }

    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    const QVector<arginfo_t> args = vec.arginfo;
    for (const auto& arg : args) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("	  %-18.18s    %s%-.50s %s\n",
               qPrintable(arg.argstring),
               (arg.argtype & ARGTYPE_TYPEMASK) ==
               ARGTYPE_BOOL ? "(0/1) " : "",
               qPrintable(arg.helpstring),
               (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

/*
 * Additional information for V1.
 * Output format type at front of line.
 */
void Vecs::disp_v1(ff_type t)
{
  const char* tstring;

  switch (t) {
  case ff_type_file:
    tstring = "file";
    break;
  case ff_type_serial:
    tstring = "serial";
    break;
  case ff_type_internal:
    tstring = "internal";
    break;
  default:
    tstring = "unknown";
    break;
  }
  printf("%s\t", tstring);
}

void Vecs::disp_v2(const vecinfo_t& v)
{
  for (const auto& i : v.cap) {
    putchar((i & ff_cap_read) ? 'r' : '-');
    putchar((i & ff_cap_write) ? 'w' : '-');
  }
  putchar('\t');
}

const char* Vecs::name_option(uint32_t type)
{
  const char* at[] = {
    "unknown",
    "integer",
    "float",
    "string",
    "boolean",
    "file",
    "outfile"
  };

  if ((type & ARGTYPE_TYPEMASK) <= 6) {
    return at[type & ARGTYPE_TYPEMASK];
  }
  return at[0];
}

void Vecs::disp_help_url(const vecinfo_t& vec, const QString& argstring)
{
  printf("\t" WEB_DOC_DIR "/fmt_%s.html", CSTR(vec.name));
  if (!argstring.isEmpty()) {
    printf("#fmt_%s_o_%s", CSTR(vec.name), CSTR(argstring));
  }
  printf("\n");
}


void Vecs::disp_v3(const vecinfo_t& vec)
{
  disp_help_url(vec, nullptr);
  const QVector<arginfo_t> args = vec.arginfo;
  for (const auto& arg : args) {
    if (!(arg.argtype & ARGTYPE_HIDDEN)) {
      printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
             CSTR(vec.name),
             CSTR(arg.argstring),
             CSTR(arg.helpstring),
             name_option(arg.argtype),
             arg.defaultvalue.isEmpty() ? "" : CSTR(arg.defaultvalue),
             arg.minvalue.isEmpty() ? "" : CSTR(arg.minvalue),
             arg.maxvalue.isEmpty() ? "" : CSTR(arg.maxvalue));
    }
    disp_help_url(vec, arg.argstring);
    printf("\n");
  }
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void Vecs::disp_formats(int version) const
{
  const auto svp = sort_and_unify_vecs();
  switch (version) {
  case 0:
  case 1:
  case 2:
  case 3:
    for (const auto& vec : svp) {
      /* Version 1 displays type at front of all types.
       * Version 0 skips internal types.
       */
      if (version > 0) {
        disp_v1(vec.type);
      } else {
        if (vec.type == ff_type_internal) {
          continue;
        }
      }
      if (version >= 2) {
        disp_v2(vec);
      }
      printf("%s\t%s\t%s%s%s\n", CSTR(vec.name),
             !vec.extensions.isEmpty() ? CSTR(vec.extensions) : "",
             CSTR(vec.desc),
             version >= 3 ? "\t" : "",
             version >= 3 ? CSTR(vec.parent) : "");
      if (version >= 3) {
        disp_v3(vec);
      }
    }
    break;
  default:
    ;
  }
}

//#define FIND_ALL_NULLPTR_ARGUMENTS
//#define FIND_ALL_EMPTY_ARGUMENT_LISTS

bool Vecs::validate_args(const QString& name, const QVector<arglist_t>* args)
{
  bool ok = true;

#ifdef FIND_ALL_NULLPTR_ARGUMENTS
  if (args == nullptr) {
    Warning() << name << "Is passing nullptr for arguments.";
  }
#endif

  if (args != nullptr) {
#ifdef FIND_ALL_EMPTY_ARGUMENT_LISTS
    if (args->isEmpty()) {
      Warning() << name << "It isn't necessary to use an empty argument list, you can pass nullptr.";
    }
#endif
    for (const auto& arg : *args) {
      if ((arg.argtype & ARGTYPE_TYPEMASK) == ARGTYPE_INT) {
        if (arg.defaultvalue &&
            ! is_integer(arg.defaultvalue)) {
          Warning() << name << "Int option" << arg.argstring << "default value" << arg.defaultvalue << "is not an integer.";
          ok = false;
        }
        if (arg.minvalue &&
            ! is_integer(arg.minvalue)) {
          Warning() << name << "Int option" << arg.argstring << "minimum value" << arg.minvalue << "is not an integer.";
          ok = false;
        }
        if (arg.maxvalue &&
            ! is_integer(arg.maxvalue)) {
          Warning() << name << "Int option" << arg.argstring << "maximum value" << arg.maxvalue << "is not an integer.";
          ok = false;
        }
      }
    }
  }

  return ok;
}

bool Vecs::validate_vec(const vecs_t& vec)
{
  bool ok = validate_args(vec.name, vec.vec->get_args());

  return ok;
}

bool Vecs::validate_formats() const
{
  bool ok = true;

  for (const auto& vec : d_ptr_->vec_list) {
    ok = validate_vec(vec) && ok;
  }

  return ok;
}
