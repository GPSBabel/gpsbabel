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

#include <QtCore/QByteArray>    // for QByteArray
#include <QtCore/QString>       // for QString
#include <QtCore/QStringList>   // for QStringList
#include <QtCore/QVector>       // for QVector<>::iterator, QVector
#include <QtCore/Qt>            // for CaseInsensitive
#include <QtCore/QtGlobal>      // for qPrintable

#include <algorithm>            // for sort
#include <cassert>              // for assert
#include <cctype>               // for isdigit
#include <cstdio>               // for printf, putchar, sscanf, size_t
#include <cstdint>
#include <cstring>              // for strchr, strtok, memset, strlen

#include "defs.h"
#include "gbversion.h"          // for WEB_DOC_DIR
#include "inifile.h"            // for inifile_readstr
#include "src/core/logging.h"   // for Warning
#include "xcsv.h"               // for XcsvFile, xcsv_file, xcsv_read_internal_style, xcsv_setup_internal_style


#define MYNAME "vecs"

struct vecs_t {
  ff_vecs_t* vec;
  QString name;
  QString desc;
  QString extensions; // list of possible extensions separated by '/', first is output default for GUI.
  QString parent;
};

extern ff_vecs_t an1_vecs;
extern ff_vecs_t bcr_vecs;
extern ff_vecs_t brauniger_iq_vecs;
extern ff_vecs_t cetus_vecs;
extern ff_vecs_t compegps_vecs;
extern ff_vecs_t copilot_vecs;
extern ff_vecs_t coto_vecs;
extern ff_vecs_t cst_vecs;
extern ff_vecs_t dg100_vecs;
extern ff_vecs_t dg200_vecs;
extern ff_vecs_t easygps_vecs;
extern ff_vecs_t energympro_vecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t garmin_txt_vecs;
extern ff_vecs_t gcdb_vecs;
extern ff_vecs_t gdb_vecs;
extern ff_vecs_t geoniche_vecs;
extern ff_vecs_t geo_vecs;
extern ff_vecs_t geojson_vecs;
extern ff_vecs_t globalsat_sport_vecs;
extern ff_vecs_t glogbook_vecs;
extern ff_vecs_t google_dir_vecs;
extern ff_vecs_t gpilots_vecs;
extern ff_vecs_t gpl_vecs;
extern ff_vecs_t gpssim_vecs;
extern ff_vecs_t gpspilot_vecs;
extern ff_vecs_t gpsutil_vecs;
extern ff_vecs_t gpx_vecs;
extern ff_vecs_t gtm_vecs;
extern ff_vecs_t hiketech_vecs;
extern ff_vecs_t holux_vecs;
extern ff_vecs_t HsaEndeavourNavigator_vecs;
extern ff_vecs_t html_vecs;
extern ff_vecs_t igc_vecs;
extern ff_vecs_t ignr_vecs;
extern ff_vecs_t igo8_vecs;
extern ff_vecs_t kml_vecs;
extern ff_vecs_t lowranceusr_vecs;
extern ff_vecs_t mag_fvecs;
extern ff_vecs_t maggeo_vecs;
extern ff_vecs_t magnav_vec;
extern ff_vecs_t mag_svecs;
extern ff_vecs_t magX_fvecs;
extern ff_vecs_t mapsend_vecs;
extern ff_vecs_t mps_vecs;
extern ff_vecs_t mtk_vecs;
extern ff_vecs_t mtk_fvecs;
extern ff_vecs_t mtk_m241_vecs;
extern ff_vecs_t mtk_m241_fvecs;
extern ff_vecs_t mtk_locus_vecs;
extern ff_vecs_t mynav_vecs;
extern ff_vecs_t navicache_vecs;
extern ff_vecs_t netstumbler_vecs;
extern ff_vecs_t nmea_vecs;
extern ff_vecs_t nmn4_vecs;
extern ff_vecs_t ozi_vecs;
extern ff_vecs_t palmdoc_vecs;
extern ff_vecs_t pcx_vecs;
extern ff_vecs_t psit_vecs;             /* MRCB */
extern ff_vecs_t quovadis_vecs;
extern ff_vecs_t saroute_vecs;
extern ff_vecs_t shape_vecs;
extern ff_vecs_t skytraq_vecs;
extern ff_vecs_t skytraq_fvecs;
extern ff_vecs_t miniHomer_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t stmsdf_vecs;
#endif
#if CSVFMTS_ENABLED
extern ff_vecs_t stmwpp_vecs;
#endif
extern ff_vecs_t tef_xml_vecs;
extern ff_vecs_t text_vecs;
extern ff_vecs_t tiger_vecs;
extern ff_vecs_t tmpro_vecs;
extern ff_vecs_t tomtom_vecs;
extern ff_vecs_t tpg_vecs;
extern ff_vecs_t tpo2_vecs;
extern ff_vecs_t tpo3_vecs;
extern ff_vecs_t unicsv_vecs;
extern ff_vecs_t vcf_vecs;
extern ff_vecs_t vitosmt_vecs;
extern ff_vecs_t wfff_xml_vecs;
extern ff_vecs_t xcsv_vecs;
extern ff_vecs_t yahoo_vecs;
extern ff_vecs_t wbt_svecs;
extern ff_vecs_t wbt_fvecs;
extern ff_vecs_t gtc_vecs;
extern ff_vecs_t dmtlog_vecs;
extern ff_vecs_t raymarine_vecs;
extern ff_vecs_t alanwpr_vecs;
extern ff_vecs_t alantrl_vecs;
extern ff_vecs_t vitovtt_vecs;
extern ff_vecs_t ggv_bin_vecs;
extern ff_vecs_t ggv_log_vecs;
extern ff_vecs_t g7towin_vecs;
extern ff_vecs_t garmin_gpi_vecs;
extern ff_vecs_t lmx_vecs;
extern ff_vecs_t random_vecs;
extern ff_vecs_t xol_vecs;
extern ff_vecs_t navilink_vecs;
extern ff_vecs_t ik3d_vecs;
extern ff_vecs_t osm_vecs;
extern ff_vecs_t destinator_poi_vecs;
extern ff_vecs_t destinator_trl_vecs;
extern ff_vecs_t destinator_itn_vecs;
extern ff_vecs_t exif_vecs;
extern ff_vecs_t vidaone_vecs;
extern ff_vecs_t gopal_vecs;
extern ff_vecs_t humminbird_vecs;
extern ff_vecs_t humminbird_ht_vecs;
extern ff_vecs_t mapasia_tr7_vecs;
extern ff_vecs_t gnav_trl_vecs;
extern ff_vecs_t navitel_trk_vecs;
extern ff_vecs_t ggv_ovl_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t jtr_vecs;
#endif
extern ff_vecs_t itracku_vecs;
extern ff_vecs_t itracku_fvecs;
extern ff_vecs_t sbp_vecs;
extern ff_vecs_t ng_vecs;
extern ff_vecs_t sbn_vecs;
extern ff_vecs_t mmo_vecs;
extern ff_vecs_t bushnell_vecs;
extern ff_vecs_t bushnell_trl_vecs;
extern ff_vecs_t skyforce_vecs;
extern ff_vecs_t v900_vecs;
extern ff_vecs_t pocketfms_bc_vecs;
extern ff_vecs_t pocketfms_fp_vecs;
extern ff_vecs_t pocketfms_wp_vecs;
extern ff_vecs_t enigma_vecs;
extern ff_vecs_t vpl_vecs;
extern ff_vecs_t teletype_vecs;
extern ff_vecs_t jogmap_vecs;
extern ff_vecs_t wintec_tes_vecs;
extern ff_vecs_t subrip_vecs;
extern ff_vecs_t format_garmin_xt_vecs;
extern ff_vecs_t format_fit_vecs;
extern ff_vecs_t mapbar_track_vecs;
extern ff_vecs_t f90g_track_vecs;
extern ff_vecs_t mapfactor_vecs;

static
const QVector<vecs_t> vec_list = {
#if CSVFMTS_ENABLED
  /* XCSV must be the first entry in this table. */
  {
    &xcsv_vecs,
    "xcsv",
    "? Character Separated Values",
    nullptr,
    nullptr,
  },
#endif
  {
    &geo_vecs,
    "geo",
    "Geocaching.com .loc",
    "loc",
    nullptr,
  },
  {
    &gpx_vecs,
    "gpx",
    "GPX XML",
    "gpx",
    nullptr,
  },
  {
    &mag_svecs,
    "magellan",
    "Magellan serial protocol",
    nullptr,
    nullptr,
  },
  {
    &mag_fvecs,
    "magellan",
    "Magellan SD files (as for Meridian)",
    nullptr,
    nullptr,
  },
  {
    &magX_fvecs,
    "magellanx",
    "Magellan SD files (as for eXplorist)",
    "upt",
    nullptr,
  },
  {
    &garmin_vecs,
    "garmin",
    "Garmin serial/USB protocol",
    nullptr,
    nullptr,
  },
  {
    &gdb_vecs,
    "gdb",
    "Garmin MapSource - gdb",
    "gdb",
    nullptr,
  },
  {
    &mapsend_vecs,
    "mapsend",
    "Magellan Mapsend",
    nullptr,
    nullptr,
  },
  {
    &mps_vecs,
    "mapsource",
    "Garmin MapSource - mps",
    "mps",
    nullptr,
  },
  {
    &nmea_vecs,
    "nmea",
    "NMEA 0183 sentences",
    nullptr,
    nullptr,
  },
  {
    &ozi_vecs,
    "ozi",
    "OziExplorer",
    nullptr,
    nullptr,
  },
  {
    &pcx_vecs,
    "pcx",
    "Garmin PCX5",
    "pcx",
    nullptr,
  },
  {
    &kml_vecs,
    "kml",
    "Google Earth (Keyhole) Markup Language",
    "kml",
    nullptr,
  },
#if MAXIMAL_ENABLED
  {
    &gpsutil_vecs,
    "gpsutil",
    "gpsutil",
    nullptr,
    nullptr,
  },
  {
    &lowranceusr_vecs,
    "lowranceusr",
    "Lowrance USR",
    "usr",
    nullptr,
  },
  {
    &holux_vecs,
    "holux",
    "Holux (gm-100) .wpo Format",
    "wpo",
    nullptr,
  },
  {
    &tpg_vecs,
    "tpg",
    "National Geographic Topo .tpg (waypoints)",
    "tpg",
    nullptr,
  },
  {
    &tpo2_vecs,
    "tpo2",
    "National Geographic Topo 2.x .tpo",
    "tpo",
    nullptr,
  },
  {
    &tpo3_vecs,
    "tpo3",
    "National Geographic Topo 3.x/4.x .tpo",
    "tpo",
    nullptr,
  },
  {
    &tmpro_vecs,
    "tmpro",
    "TopoMapPro Places File",
    "tmpro",
    nullptr,
  },
  {
    &tiger_vecs,
    "tiger",
    "U.S. Census Bureau Tiger Mapping Service",
    nullptr,
    nullptr,
  },
  {
    &easygps_vecs,
    "easygps",
    "EasyGPS binary format",
    "loc",
    nullptr,
  },
  {
    &saroute_vecs,
    "saroute",
    "DeLorme Street Atlas Route",
    "anr",
    nullptr,
  },
  {
    &navicache_vecs,
    "navicache",
    "Navicache.com XML",
    nullptr,
    nullptr,
  },
  {	/* MRCB */
    &psit_vecs,
    "psitrex",
    "KuDaTa PsiTrex text",
    nullptr,
    nullptr,
  },
#if SHAPELIB_ENABLED
  {
    &shape_vecs,
    "shape",
    "ESRI shapefile",
    "shp",
    nullptr,
  },
#endif
  {
    &gpl_vecs,
    "gpl",
    "DeLorme GPL",
    "gpl",
    nullptr,
  },
  {
    &text_vecs,
    "text",
    "Textual Output",
    "txt",
    nullptr,
  },
  {
    &html_vecs,
    "html",
    "HTML Output",
    "html",
    nullptr,
  },
  {
    &netstumbler_vecs,
    "netstumbler",
    "NetStumbler Summary File (text)",
    nullptr,
    nullptr,
  },
  {
    &igc_vecs,
    "igc",
    "FAI/IGC Flight Recorder Data Format",
    nullptr,
    nullptr,
  },
  {
    &brauniger_iq_vecs,
    "baroiq",
    "Brauniger IQ Series Barograph Download",
    nullptr,
    nullptr,
  },
  {
    &mtk_vecs,
    "mtk",
    "MTK Logger (iBlue 747,Qstarz BT-1000,...) download",
    nullptr,
    nullptr,
  },
  {
    &mtk_fvecs,
    "mtk-bin",
    "MTK Logger (iBlue 747,...) Binary File Format",
    "bin",
    nullptr,
  },
  {
    &mtk_m241_vecs,
    "m241",
    "Holux M-241 (MTK based) download",
    nullptr,
    nullptr,
  },
  {
    &mtk_m241_fvecs,
    "m241-bin",
    "Holux M-241 (MTK based) Binary File Format",
    "bin",
    nullptr,
  },
  {
    &mtk_locus_vecs,
    "mtk_locus",
    "MediaTek Locus",
    nullptr,
    nullptr,
  },
#endif // MAXIMAL_ENABLED
  {
    &wbt_svecs,
    "wbt",
    "Wintec WBT-100/200 GPS Download",
    nullptr,
    nullptr,
  },
#if MAXIMAL_ENABLED
  {
    &vpl_vecs,
    "vpl",
    "Honda/Acura Navigation System VP Log File Format",
    nullptr,
    nullptr,
  },
  {
    &wbt_fvecs,
    "wbt-bin",
    "Wintec WBT-100/200 Binary File Format",
    "bin",
    nullptr,
  },
  {
    &wbt_fvecs,
    "wbt-tk1",
    "Wintec WBT-201/G-Rays 2 Binary File Format",
    "tk1",
    nullptr,
  },
  {
    &hiketech_vecs,
    "hiketech",
    "HikeTech",
    "gps",
    nullptr,
  },
  {
    &glogbook_vecs,
    "glogbook",
    "Garmin Logbook XML",
    "xml",
    nullptr,
  },
  {
    &vcf_vecs,
    "vcard",
    "Vcard Output (for iPod)",
    "vcf",
    nullptr,
  },
  {
    &google_dir_vecs,
    "googledir",
    "Google Directions XML",
    "xml",
    nullptr,
  },
  {
    &maggeo_vecs,
    "maggeo",
    "Magellan Explorist Geocaching",
    "gs",
    nullptr,
  },
  {
    &an1_vecs,
    "an1",
    "DeLorme .an1 (drawing) file",
    "an1",
    nullptr,
  },
  {
    &tomtom_vecs,
    "tomtom",
    "TomTom POI file (.ov2)",
    "ov2",
    nullptr,
  },
  {
    &tef_xml_vecs,
    "tef",
    "Map&Guide 'TourExchangeFormat' XML",
    "xml",
    nullptr,
  },
  {
    &vitosmt_vecs,
    "vitosmt",
    "Vito Navigator II tracks",
    "smt",
    nullptr,
  },
  {
    &wfff_xml_vecs,
    "wfff",
    "WiFiFoFum 2.0 for PocketPC XML",
    "xml",
    nullptr,
  },
  {
    &bcr_vecs,
    "bcr",
    "Motorrad Routenplaner (Map&Guide) .bcr files",
    "bcr",
    nullptr,
  },
  {
    &ignr_vecs,
    "ignrando",
    "IGN Rando track files",
    "rdn",
    nullptr,
  },
#if CSVFMTS_ENABLED
  {
    &stmsdf_vecs,
    "stmsdf",
    "Suunto Trek Manager (STM) .sdf files",
    "sdf",
    nullptr,
  },
#endif
#if CSVFMTS_ENABLED
  {
    &stmwpp_vecs,
    "stmwpp",
    "Suunto Trek Manager (STM) WaypointPlus files",
    "txt",
    nullptr,
  },
#endif //  CSVFMTS_ENABLED
  {
    &cst_vecs,
    "cst",
    "CarteSurTable data file",
    "cst",
    nullptr,
  },
  {
    &nmn4_vecs,
    "nmn4",
    "Navigon Mobile Navigator .rte files",
    "rte",
    nullptr,
  },
#if CSVFMTS_ENABLED
  {
    &compegps_vecs,
    "compegps",
    "CompeGPS data files (.wpt/.trk/.rte)",
    nullptr,
    nullptr,
  },
#endif //CSVFMTS_ENABLED
  {
    &yahoo_vecs,
    "yahoo",
    "Yahoo Geocode API data",
    nullptr,
    nullptr,
  },
  {
    &unicsv_vecs,
    "unicsv",
    "Universal csv with field structure in first line",
    nullptr,
    nullptr,
  },
  {
    &gtm_vecs,
    "gtm",
    "GPS TrackMaker",
    "gtm",
    nullptr,
  },
  {
    &gpssim_vecs,
    "gpssim",
    "Franson GPSGate Simulation",
    "gpssim",
    nullptr,
  },
#if CSVFMTS_ENABLED
  {
    &garmin_txt_vecs,
    "garmin_txt",
    "Garmin MapSource - txt (tab delimited)",
    "txt",
    nullptr,
  },
#endif // CSVFMTS_ENABLED
  {
    &gtc_vecs,
    "gtrnctr",
    "Garmin Training Center (.tcx/.crs/.hst/.xml)",
    "tcx/crs/hst/xml",
    nullptr,
  },
  {
    &dmtlog_vecs,
    "dmtlog",
    "TrackLogs digital mapping (.trl)",
    "trl",
    nullptr,
  },
  {
    &raymarine_vecs,
    "raymarine",
    "Raymarine Waypoint File (.rwf)",
    "rwf",
    nullptr,
  },
  {
    &alanwpr_vecs,
    "alanwpr",
    "Alan Map500 waypoints and routes (.wpr)",
    "wpr",
    nullptr,
  },
  {
    &alantrl_vecs,
    "alantrl",
    "Alan Map500 tracklogs (.trl)",
    "trl",
    nullptr,
  },
  {
    &vitovtt_vecs,
    "vitovtt",
    "Vito SmartMap tracks (.vtt)",
    "vtt",
    nullptr,
  },
  {
    &ggv_log_vecs,
    "ggv_log",
    "Geogrid-Viewer tracklogs (.log)",
    "log",
    nullptr,
  },
#if CSVFMTS_ENABLED
  {
    &g7towin_vecs,
    "g7towin",
    "G7ToWin data files (.g7t)",
    "g7t",
    nullptr,
  },
#endif
  {
    &garmin_gpi_vecs,
    "garmin_gpi",
    "Garmin Points of Interest (.gpi)",
    "gpi",
    nullptr,
  },
  {
    &lmx_vecs,
    "lmx",
    "Nokia Landmark Exchange",
    nullptr,
    nullptr,
  },
  {
    &random_vecs,
    "random",
    "Internal GPS data generator",
    nullptr,
    nullptr,
  },
  {
    &xol_vecs,
    "xol",
    "Swiss Map 25/50/100 (.xol)",
    "xol",
    nullptr,
  },
  {
    &dg100_vecs,
    "dg-100",
    "GlobalSat DG-100/BT-335 Download",
    nullptr,
    nullptr,
  },
  {
    &dg200_vecs,
    "dg-200",
    "GlobalSat DG-200 Download",
    nullptr,
    nullptr,
  },
  {
    &navilink_vecs,
    "navilink",
    "NaviGPS GT-11/BGT-11 Download",
    nullptr,
    nullptr,
  },
  {
    &ik3d_vecs,
    "ik3d",
    "MagicMaps IK3D project file (.ikt)",
    "ikt",
    nullptr,
  },
  {
    &osm_vecs,
    "osm",
    "OpenStreetMap data files",
    "osm",
    nullptr,
  },
  {
    &destinator_poi_vecs,
    "destinator_poi",
    "Destinator Points of Interest (.dat)",
    "dat",
    nullptr,
  },
  {
    &destinator_itn_vecs,
    "destinator_itn",
    "Destinator Itineraries (.dat)",
    "dat",
    nullptr,
  },
  {
    &destinator_trl_vecs,
    "destinator_trl",
    "Destinator TrackLogs (.dat)",
    "dat",
    nullptr,
  },
  {
    &exif_vecs,
    "exif",
    "Embedded Exif-GPS data (.jpg)",
    "jpg",
    nullptr,
  },
  {
    &vidaone_vecs,
    "vidaone",
    "VidaOne GPS for Pocket PC (.gpb)",
    "gpb",
    nullptr,
  },
  {
    &igo8_vecs,
    "igo8",
    "IGO8 .trk",
    "trk",
    nullptr,
  },
  {
    &gopal_vecs,
    "gopal",
    "GoPal GPS track log (.trk)",
    "trk",
    nullptr,
  },
  {
    &humminbird_vecs,
    "humminbird",
    "Humminbird waypoints and routes (.hwr)",
    "hwr",
    nullptr,
  },
  {
    &humminbird_ht_vecs,
    "humminbird_ht",
    "Humminbird tracks (.ht)",
    "ht",
    nullptr,
  },
  {
    &mapasia_tr7_vecs,
    "mapasia_tr7",
    "MapAsia track file (.tr7)",
    "tr7",
    nullptr,
  },
  {
    &gnav_trl_vecs,
    "gnav_trl",
    "Google Navigator Tracklines (.trl)",
    "trl",
    nullptr,
  },
  {
    &navitel_trk_vecs,
    "navitel_trk",
    "Navitel binary track (.bin)",
    "bin",
    nullptr,
  },
  {
    &ggv_ovl_vecs,
    "ggv_ovl",
    "Geogrid-Viewer ascii overlay file (.ovl)",
    "ovl",
    nullptr,
  },
#if CSVFMTS_ENABLED
  {
    &jtr_vecs,
    "jtr",
    "Jelbert GeoTagger data file",
    "jtr",
    nullptr,
  },
#endif
  {
    &itracku_vecs,
    "itracku",
    "XAiOX iTrackU Logger",
    nullptr,
    nullptr,
  },

  {
    &itracku_fvecs,
    "itracku-bin",
    "XAiOX iTrackU Logger Binary File Format",
    "bin",
    nullptr,
  },
  {
    &sbp_vecs,
    "sbp",
    "NaviGPS GT-31/BGT-31 datalogger (.sbp)",
    "sbp",
    nullptr,
  },
  {
    &sbn_vecs,
    "sbn",
    "NaviGPS GT-31/BGT-31 SiRF binary logfile (.sbn)",
    "sbn",
    nullptr,
  },
  {
    &mmo_vecs,
    "mmo",
    "Memory-Map Navigator overlay files (.mmo)",
    "mmo",
    nullptr,
  },
  {
    &bushnell_vecs,
    "bushnell",
    "Bushnell GPS Waypoint file",
    "wpt",
    nullptr,
  },
  {
    &bushnell_trl_vecs,
    "bushnell_trl",
    "Bushnell GPS Trail file",
    "trl",
    nullptr,
  },
  {
    &skyforce_vecs,
    "skyforce",
    "Skymap / KMD150 ascii files",
    nullptr,
    nullptr,
  },
  {
    &pocketfms_bc_vecs,
    "pocketfms_bc",
    "PocketFMS breadcrumbs",
    nullptr,
    nullptr,
  },
  {
    &pocketfms_fp_vecs,
    "pocketfms_fp",
    "PocketFMS flightplan (.xml)",
    "xml",
    nullptr,
  },
  {
    &pocketfms_wp_vecs,
    "pocketfms_wp",
    "PocketFMS waypoints (.txt)",
    "txt",
    nullptr,
  },
  {
    &v900_vecs,
    "v900",
    "Columbus/Visiontac V900 files (.csv)",
    nullptr,
    nullptr,
  },
  {
    &ng_vecs,
    "naviguide",
    "Naviguide binary route file (.twl)",
    "twl",
    nullptr,
  },
  {
    &enigma_vecs,
    "enigma",
    "Enigma binary waypoint file (.ert)",
    "ert",
    nullptr,
  },
  {
    &skytraq_vecs,
    "skytraq",
    "SkyTraq Venus based loggers (download)",
    nullptr,
    nullptr,
  },
  {
    &teletype_vecs,
    "teletype",
    "Teletype [ Get Jonathon Johnson to describe",
    nullptr,
    nullptr,
  },
  {
    &skytraq_fvecs,
    "skytraq-bin",
    "SkyTraq Venus based loggers Binary File Format",
    "bin",
    nullptr,
  },
  {
    &miniHomer_vecs,
    "miniHomer",
    "MiniHomer, a skyTraq Venus 6 based logger (download tracks, waypoints and get/set POI)",
    nullptr,
    nullptr,
  },
  {
    &jogmap_vecs,
    "jogmap",
    "Jogmap.de XML format",
    "xml",
    nullptr,
  },
  {
    &wintec_tes_vecs,
    "wintec_tes",
    "Wintec TES file",
    "tes",
    nullptr,
  },
  {
    &subrip_vecs,
    "subrip",
    "SubRip subtitles for video mapping (.srt)",
    "srt",
    nullptr,
  },
  {
    &format_garmin_xt_vecs,
    "garmin_xt",
    "Mobile Garmin XT Track files",
    nullptr,
    nullptr,
  },
  {
    &format_fit_vecs,
    "garmin_fit",
    "Flexible and Interoperable Data Transfer (FIT) Activity file",
    "fit",
    nullptr,
  },
  {
    &mapbar_track_vecs,
    "mapbar",
    "Mapbar (China) navigation track for Sonim Xp3300",
    "trk",
    nullptr,
  },
  {
    &f90g_track_vecs,
    "f90g",
    "F90G Automobile DVR GPS log file",
    "map",
    nullptr,
  },
  {
    &mapfactor_vecs,
    "mapfactor",
    "Mapfactor Navigator",
    "xml",
    nullptr,
  },
  {
    &energympro_vecs,
    "energympro",
    "Energympro GPS training watch",
    "cpo",
    nullptr,
  },
  {
    &mynav_vecs,
    "mynav",
    "MyNav TRC format",
    "trc",
    nullptr,
  },
  {
    &geojson_vecs,
    "geojson",
    "GeoJson",
    "json",
    nullptr,
  },
  {
    &ggv_bin_vecs,
    "ggv_bin",
    "Geogrid-Viewer binary overlay file (.ovl)",
    "ovl",
    nullptr,
  },
  {
    &globalsat_sport_vecs,
    "globalsat",
    "GlobalSat GH625XT GPS training watch",
    nullptr,
    nullptr,
  }
#endif // MAXIMAL_ENABLED
};

/*
 * When we modify an element on the list we need to be careful
 * that we are not modifying a Qt COW copy.
 * Qt has an undocumented but public member function isDetached().
 * If the list is detached it implies it is not shared, then functions
 * then might detach, like the iterator begin which is implcitly used
 * in the range based for loop, won't cause a copy to be created.
 * We can make sure this is true for at least our regression cases
 * with assertions.
 * There is an odd situation that an empty QVector is not detached,
 * so we have to exclude this from the check.
 * The possibility of detachement is also why the type of element
 * on the list must be default constructable. This is why we have
 * to supply a default for any const members of arglist_t.  Without
 * the default the default constructor would be implicitly deleted.
 */

void
init_vecs()
{
  for (const auto& vec : vec_list) {
    if (vec.vec->args && !vec.vec->args->isEmpty()) {
      assert(vec.vec->args->isDetached());
      for (auto& arg : *vec.vec->args) {
        arg.argvalptr = nullptr;
        if (arg.argval) {
          *arg.argval = nullptr;
        }
      }
    }
  }
}

static int
is_integer(const char* c)
{
  return isdigit(c[0]) || ((c[0] == '+' || c[0] == '-') && isdigit(c[1]));
}

void
exit_vecs()
{
  for (const auto& vec : vec_list) {
    if (vec.vec->exit) {
      (*vec.vec->exit)();
    }
    if (vec.vec->args && !vec.vec->args->isEmpty()) {
      assert(vec.vec->args->isDetached());
      for (auto& arg : *vec.vec->args) {
        if (arg.argvalptr) {
          xfree(arg.argvalptr);
          *arg.argval = arg.argvalptr = nullptr;
        }
      }
    }
  }
}

void
assign_option(const QString& module, arglist_t* arg, const char* val)
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
      is_fatal(1 != sscanf(c, "%d", &test),
               "%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
    }
    break;
  case ARGTYPE_FLOAT:
    if (*c == '\0') {
      c = "0";
    } else {
      double test;
      is_fatal(1 != sscanf(c, "%lf", &test),
               "%s: Invalid parameter value %s for option %s", qPrintable(module), val, arg->argstring);
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

void
disp_vec_options(const QString& vecname, const QVector<arglist_t>* args)
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

void validate_options(const QStringList& options, const QVector<arglist_t>* args, const QString& name)
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

ff_vecs_t*
find_vec(const QString& vecname)
{
  QStringList options = vecname.split(',');
  if (options.isEmpty()) {
    fatal("A format name is required.\n");
  }
  const QString svecname = options.takeFirst();

  for (const auto& vec : vec_list) {
    if (svecname.compare(vec.name, Qt::CaseInsensitive) != 0) {
      continue;
    }

    validate_options(options, vec.vec->args, vec.name);

    if (vec.vec->args && !vec.vec->args->isEmpty()) {
      assert(vec.vec->args->isDetached());
      for (auto& arg : *vec.vec->args) {
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
      disp_vec_options(vec.name, vec.vec->args);
    }

#if CSVFMTS_ENABLED
    // xcsv_setup_internal_style( nullptr );
#endif // CSVFMTS_ENABLED
    vec.vec->name = vec.name;	/* needed for session information */
    return vec.vec;

  }

  /*
   * Didn't find it in the table of "real" file types, so plan B
   * is to search the list of xcsv styles.
   */
  for (const auto& svec : style_list) {
    if (svecname.compare(svec.name,  Qt::CaseInsensitive) != 0) {
      continue;
    }

    validate_options(options, vec_list.at(0).vec->args, svec.name);

    if (vec_list[0].vec->args && !vec_list[0].vec->args->isEmpty()) {
      assert(vec_list[0].vec->args->isDetached());
      for (auto& arg : *vec_list[0].vec->args) {
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
      disp_vec_options(svec.name, vec_list[0].vec->args);
    }
#if CSVFMTS_ENABLED
    xcsv_setup_internal_style(svec.style_buf);
#endif // CSVFMTS_ENABLED

    vec_list[0].vec->name = svec.name;	/* needed for session information */
    return vec_list[0].vec;
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
QString
get_option(const QStringList& options, const char* argname)
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

/*
 * Smoosh the vecs list and style lists together and sort them
 * alphabetically.  Returns an allocated copy of a style_vecs_array
 * that's populated and sorted.
 */
static QVector<vecs_t>
sort_and_unify_vecs()
{
  QVector<vecs_t> svp;
  svp.reserve(vec_list.size() + style_list.size());

  /* Normal vecs are easy; populate the first part of the array. */
  for (const auto& vec : vec_list) {
    vecs_t uvec = vec;
    if (uvec.parent == nullptr) {
      uvec.parent = uvec.name;
    }
    svp.append(uvec);
  }

  /* The style formats are based on the xcsv format,
   * Make sure we know which entry in the vector list that is.
   */
  assert(vec_list.at(0).name == "xcsv");
  /* The style formats use a modified xcsv argument list that doesn't include
   * the option to set the style file.  Make sure we know which entry in
   * the argument list that is.
   */
  assert(case_ignore_strcmp(vec_list.at(0).vec->args->at(0).helpstring,
                            "Full path to XCSV style file") == 0);
  /* Prepare a modified argument list for the style formats. */
  auto xcsv_args = new QVector<arglist_t>(*vec_list.at(0).vec->args); /* LEAK */
  xcsv_args->removeFirst();

  /* Walk the style list, parse the entries, dummy up a "normal" vec */
  for (const auto& svec : style_list) {
    xcsv_read_internal_style(svec.style_buf);
    vecs_t uvec;
    uvec.name = svec.name;
    uvec.vec = new ff_vecs_t; /* LEAK */
    uvec.extensions = xcsv_file.extension;
    *uvec.vec = *vec_list.at(0).vec; /* Inherits xcsv opts */
    /* Reset file type to inherit ff_type from xcsv. */
    uvec.vec->type = xcsv_file.type;
    /* Skip over the first help entry for all but the
     * actual 'xcsv' format - so we don't expose the
     * 'Full path to XCSV style file' argument to any
     * GUIs for an internal format.
     */
    uvec.vec->args = xcsv_args;
    memset(&uvec.vec->cap, 0, sizeof(uvec.vec->cap));
    switch (xcsv_file.datatype) {
    case unknown_gpsdata:
    case wptdata:
      uvec.vec->cap[ff_cap_rw_wpt] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case trkdata:
      uvec.vec->cap[ff_cap_rw_trk] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    case rtedata:
      uvec.vec->cap[ff_cap_rw_rte] = (ff_cap)(ff_cap_read | ff_cap_write);
      break;
    default:
      ;
    }
    uvec.desc = xcsv_file.description;
    uvec.parent = "xcsv";
    svp.append(uvec);
  }

  /*
   *  Display the available formats in a format that's easy for humans to
   *  parse for help on available command line options.
   */
  auto alpha = [](const vecs_t& a, const vecs_t& b)->bool {
    return QString::compare(a.desc, b.desc, Qt::CaseInsensitive) < 0;
  };

  /* Now that we have everything in an array, alphabetize them */
  std::sort(svp.begin(), svp.end(), alpha);

  return svp;
}

#define VEC_FMT "	%-20.20s  %-.50s\n"

void
disp_vecs()
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vec.vec->type == ff_type_internal)  {
      continue;
    }
    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    if (vec.vec->args) {
      for (const auto& arg : qAsConst(*vec.vec->args)) {
        if (!(arg.argtype & ARGTYPE_HIDDEN))
          printf("	  %-18.18s    %s%-.50s %s\n",
                 arg.argstring,
                 (arg.argtype & ARGTYPE_TYPEMASK) ==
                 ARGTYPE_BOOL ? "(0/1) " : "",
                 arg.helpstring,
                 (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

void
disp_vec(const QString& vecname)
{
  const auto svp = sort_and_unify_vecs();
  for (const auto& vec : svp) {
    if (vecname.compare(vec.name, Qt::CaseInsensitive) != 0)  {
      continue;
    }

    printf(VEC_FMT, qPrintable(vec.name), qPrintable(vec.desc));
    if (vec.vec->args) {
      for (const auto& arg : qAsConst(*vec.vec->args)) {
        if (!(arg.argtype & ARGTYPE_HIDDEN))
          printf("	  %-18.18s    %s%-.50s %s\n",
                 arg.argstring,
                 (arg.argtype & ARGTYPE_TYPEMASK) ==
                 ARGTYPE_BOOL ? "(0/1) " : "",
                 arg.helpstring,
                 (arg.argtype & ARGTYPE_REQUIRED) ? "(required)" : "");
      }
    }
  }
}

/*
 * Additional information for V1.
 * Output format type at front of line.
 */
static void
disp_v1(ff_type t)
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

static void
disp_v2(const ff_vecs_t* v)
{
  for (auto& i : v->cap) {
    putchar((i & ff_cap_read) ? 'r' : '-');
    putchar((i & ff_cap_write) ? 'w' : '-');
  }
  putchar('\t');
}

const char*
name_option(uint32_t type)
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

static
void disp_help_url(const vecs_t& vec, const arglist_t* arg)
{
  printf("\t" WEB_DOC_DIR "/fmt_%s.html", CSTR(vec.name));
  if (arg) {
    printf("#fmt_%s_o_%s", CSTR(vec.name), arg->argstring);
  }
  printf("\n");
}


static void
disp_v3(const vecs_t& vec)
{
  disp_help_url(vec, nullptr);
  if (vec.vec->args) {
    for (const auto& arg : qAsConst(*vec.vec->args)) {
      if (!(arg.argtype & ARGTYPE_HIDDEN)) {
        printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
               CSTR(vec.name),
               arg.argstring,
               arg.helpstring,
               name_option(arg.argtype),
               arg.defaultvalue ? arg.defaultvalue : "",
               arg.minvalue ? arg.minvalue : "",
               arg.maxvalue ? arg.maxvalue : "");
      }
      disp_help_url(vec, &arg);
      printf("\n");
    }
  }
}

/*
 *  Display the available formats in a format that's easy to machine
 *  parse.   Typically invoked by programs like graphical wrappers to
 *  determine what formats are supported.
 */
void
disp_formats(int version)
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
        disp_v1(vec.vec->type);
      } else {
        if (vec.vec->type == ff_type_internal) {
          continue;
        }
      }
      if (version >= 2) {
        disp_v2(vec.vec);
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

bool validate_args(const QString& name, const QVector<arglist_t>* args)
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
      if (arg.argtype == ARGTYPE_INT) {
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

static bool
validate_vec(const vecs_t& vec)
{
  bool ok = validate_args(vec.name, vec.vec->args);

  if (!((vec.vec->cap[0]|vec.vec->cap[1]|vec.vec->cap[2]) & ff_cap_write)) {
    if (vec.vec->wr_init != nullptr) {
      Warning() << "ERROR no write capability but non-null wr_init %s\n" << vec.name;
      ok = false;
    }
  }
  if (!((vec.vec->cap[0]|vec.vec->cap[1]|vec.vec->cap[2]) & ff_cap_read)) {
    if (vec.vec->rd_init != nullptr) {
      Warning() << "ERROR no read capability but non-null rd_init %s\n" << vec.name;
      ok = false;
    }
  }
  if ((vec.vec->cap[0]|vec.vec->cap[1]|vec.vec->cap[2]) & ff_cap_write) {
    if (vec.vec->wr_init == nullptr) {
      Warning() << "ERROR write capability but null wr_init %s\n" << vec.name;
      ok = false;
    }
  }
  if ((vec.vec->cap[0]|vec.vec->cap[1]|vec.vec->cap[2]) & ff_cap_read) {
    if (vec.vec->rd_init == nullptr) {
      Warning() << "ERROR read capability but null rd_init %s\n" << vec.name;
      ok = false;
    }
  }

  if (vec.vec->wr_init != nullptr) {
    if (vec.vec->write == nullptr) {
      Warning() << "ERROR nonnull wr_init but null write %s\n" << vec.name;
      ok = false;
    }
    if (vec.vec->wr_deinit == nullptr) {
      Warning() << "ERROR nonnull wr_init but null wr_deinit %s\n" << vec.name;
      ok = false;
    }
  }
  if (vec.vec->wr_init == nullptr) {
    if (vec.vec->write != nullptr) {
      Warning() << "ERROR null wr_init with non-null write %s\n" << vec.name;
      ok = false;
    }
    if (vec.vec->wr_deinit != nullptr) {
      Warning() << "ERROR null wr_init with non-null wr_deinit %s\n" << vec.name;
      ok = false;
    }
  }

  if (vec.vec->rd_init != nullptr) {
    if (vec.vec->read == nullptr) {
      Warning() << "ERROR nonnull rd_init but null read %s\n" << vec.name;
      ok = false;
    }
    if (vec.vec->rd_deinit == nullptr) {
      Warning() << "ERROR nonnull rd_init but null rd_deinit %s\n" << vec.name;
      ok = false;
    }
  }
  if (vec.vec->rd_init == nullptr) {
    if (vec.vec->read != nullptr) {
      Warning() << "ERROR null rd_init with non-null read %s\n" << vec.name;
      ok = false;
    }
    if (vec.vec->rd_deinit != nullptr) {
      Warning() << "ERROR null rd_init with non-null rd_deinit %s\n" << vec.name;
      ok = false;
    }
  }

  return ok;
}

bool validate_formats()
{
  bool ok = true;

  for (const auto& vec : vec_list) {
    ok = validate_vec(vec) && ok;
  }

  return ok;
}
