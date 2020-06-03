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
#ifndef VECS_H_INCLUDED_
#define VECS_H_INCLUDED_

#include <cstdint>

#include <QtCore/QString>       // for QString
#include <QtCore/QStringList>   // for QStringList
#include <QtCore/QVector>       // for QVector<>::iterator, QVector

#include "defs.h"
#include "dg-100.h"
#include "format.h"
#include "energympro.h"
#include "garmin_fit.h"
#include "geojson.h"
#include "ggv_bin.h"
#include "globalsat_sport.h"
#include "gpx.h"
#include "kml.h"
#include "legacyformat.h"
#include "lowranceusr.h"
#include "mynav.h"
#include "nmea.h"
#include "osm.h"
#include "qstarz_bl_1000.h"
#include "random.h"
#include "shape.h"
#include "subrip.h"
#include "unicsv.h"
#include "xcsv.h"
#include "yahoo.h"


extern ff_vecs_t geo_vecs;
extern ff_vecs_t mag_svecs;
extern ff_vecs_t mag_fvecs;
extern ff_vecs_t magX_fvecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t gdb_vecs;
extern ff_vecs_t mapsend_vecs;
extern ff_vecs_t ozi_vecs;
extern ff_vecs_t pcx_vecs;
#if MAXIMAL_ENABLED
extern ff_vecs_t gpsutil_vecs;
extern ff_vecs_t holux_vecs;
extern ff_vecs_t tpg_vecs;
extern ff_vecs_t tpo2_vecs;
extern ff_vecs_t tpo3_vecs;
extern ff_vecs_t tmpro_vecs;
extern ff_vecs_t tiger_vecs;
extern ff_vecs_t easygps_vecs;
extern ff_vecs_t saroute_vecs;
extern ff_vecs_t navicache_vecs;
extern ff_vecs_t gpl_vecs;
extern ff_vecs_t text_vecs;
extern ff_vecs_t html_vecs;
extern ff_vecs_t netstumbler_vecs;
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
extern ff_vecs_t vpl_vecs;
extern ff_vecs_t wbt_fvecs;
//extern ff_vecs_t wbt_fvecs;
extern ff_vecs_t hiketech_vecs;
extern ff_vecs_t glogbook_vecs;
extern ff_vecs_t vcf_vecs;
extern ff_vecs_t google_dir_vecs;
extern ff_vecs_t maggeo_vecs;
extern ff_vecs_t an1_vecs;
extern ff_vecs_t tomtom_vecs;
extern ff_vecs_t tef_xml_vecs;
extern ff_vecs_t vitosmt_vecs;
extern ff_vecs_t wfff_xml_vecs;
extern ff_vecs_t bcr_vecs;
extern ff_vecs_t ignr_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t stmsdf_vecs;
#endif // CSVFMTS_ENABLED
#if CSVFMTS_ENABLED
extern ff_vecs_t stmwpp_vecs;
#endif // CSVFMTS_ENABLED
extern ff_vecs_t cst_vecs;
extern ff_vecs_t nmn4_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t compegps_vecs;
#endif // CSVFMTS_ENABLED
// extern ff_vecs_t yahoo_vecs;
extern ff_vecs_t gtm_vecs;
extern ff_vecs_t gpssim_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t garmin_txt_vecs;
#endif // CSVFMTS_ENABLED
extern ff_vecs_t gtc_vecs;
extern ff_vecs_t dmtlog_vecs;
extern ff_vecs_t raymarine_vecs;
extern ff_vecs_t alanwpr_vecs;
extern ff_vecs_t alantrl_vecs;
extern ff_vecs_t vitovtt_vecs;
extern ff_vecs_t ggv_log_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t g7towin_vecs;
#endif // CSVFMTS_ENABLED
extern ff_vecs_t garmin_gpi_vecs;
extern ff_vecs_t lmx_vecs;
extern ff_vecs_t xol_vecs;
extern ff_vecs_t navilink_vecs;
extern ff_vecs_t ik3d_vecs;
extern ff_vecs_t destinator_poi_vecs;
extern ff_vecs_t destinator_itn_vecs;
extern ff_vecs_t destinator_trl_vecs;
extern ff_vecs_t exif_vecs;
extern ff_vecs_t vidaone_vecs;
extern ff_vecs_t igo8_vecs;
extern ff_vecs_t gopal_vecs;
extern ff_vecs_t humminbird_vecs;
extern ff_vecs_t humminbird_ht_vecs;
extern ff_vecs_t mapasia_tr7_vecs;
extern ff_vecs_t gnav_trl_vecs;
extern ff_vecs_t navitel_trk_vecs;
extern ff_vecs_t ggv_ovl_vecs;
#if CSVFMTS_ENABLED
extern ff_vecs_t jtr_vecs;
#endif // CSVFMTS_ENABLED
extern ff_vecs_t itracku_vecs;
extern ff_vecs_t itracku_fvecs;
extern ff_vecs_t sbp_vecs;
extern ff_vecs_t sbn_vecs;
extern ff_vecs_t mmo_vecs;
extern ff_vecs_t bushnell_vecs;
extern ff_vecs_t bushnell_trl_vecs;
extern ff_vecs_t skyforce_vecs;
extern ff_vecs_t pocketfms_bc_vecs;
extern ff_vecs_t pocketfms_fp_vecs;
extern ff_vecs_t pocketfms_wp_vecs;
extern ff_vecs_t v900_vecs;
extern ff_vecs_t ng_vecs;
extern ff_vecs_t enigma_vecs;
extern ff_vecs_t skytraq_vecs;
extern ff_vecs_t teletype_vecs;
extern ff_vecs_t skytraq_fvecs;
extern ff_vecs_t miniHomer_vecs;
extern ff_vecs_t jogmap_vecs;
extern ff_vecs_t wintec_tes_vecs;
extern ff_vecs_t format_garmin_xt_vecs;
extern ff_vecs_t mapbar_track_vecs;
extern ff_vecs_t f90g_track_vecs;
extern ff_vecs_t mapfactor_vecs;
#endif // MAXIMAL_ENABLED

class Vecs
{
// Meyers Singleton
public:
  static Vecs& Instance()
  {
    static Vecs instance;
    return instance;
  }
  Vecs(const Vecs&) = delete;
  Vecs& operator= (const Vecs&) = delete;
  Vecs(Vecs&&) = delete;
  Vecs& operator=(Vecs&&) = delete;

private:
  Vecs() = default;
  ~Vecs() = default;

private:
  struct vecs_t {
    Format* vec;
    QString name;
    QString desc;
    QString extensions; // list of possible extensions separated by '/', first is output default for GUI.
    QString parent;
  };

  struct arginfo_t {
    arginfo_t() = default;
    explicit arginfo_t(const arglist_t& arg) :
      argstring(arg.argstring),
      helpstring(arg.helpstring),
      defaultvalue(arg.defaultvalue),
      argtype(arg.argtype),
      minvalue(arg.minvalue),
      maxvalue(arg.maxvalue)
    {}

    QString argstring;
    QString helpstring;
    QString defaultvalue;
    uint32_t argtype{ARGTYPE_UNKNOWN};
    QString minvalue;
    QString maxvalue;
  };

  struct vecinfo_t {
    QString name;
    QString desc;
    QString extensions;
    QString parent;
    ff_type type{ff_type_file};
    QVector<ff_cap> cap;
    QVector<arginfo_t> arginfo;
  };

public:
  void init_vecs();
  void exit_vecs();
  static void assign_option(const QString& module, arglist_t* arg, const char* val);
  static void disp_vec_options(const QString& vecname, const QVector<arglist_t>* args);
  static void validate_options(const QStringList& options, const QVector<arglist_t>* args, const QString& name);
  static QString get_option(const QStringList& options, const char* argname);
  Format* find_vec(const QString& vecname);
  void disp_vecs() const;
  void disp_vec(const QString& vecname) const;
  static const char* name_option(uint32_t type);
  void disp_formats(int version) const;
  static bool validate_args(const QString& name, const QVector<arglist_t>* args);
  bool validate_formats() const;

private:
  static int is_integer(const char* c);
  QVector<vecinfo_t> sort_and_unify_vecs() const;
  static void disp_v1(ff_type t);
  static void disp_v2(const vecinfo_t& v);
  static void disp_help_url(const vecinfo_t& vec, const QString& argstring);
  static void disp_v3(const vecinfo_t& vec);
  static bool validate_vec(const vecs_t& vec);

private:
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
  LegacyFormat gdb_fmt {gdb_vecs};
  LegacyFormat mapsend_fmt {mapsend_vecs};
  NmeaFormat nmea_fmt;
  LegacyFormat ozi_fmt {ozi_vecs};
  LegacyFormat pcx_fmt {pcx_vecs};
  KmlFormat kml_fmt;
#if MAXIMAL_ENABLED
  LegacyFormat gpsutil_fmt {gpsutil_vecs};
  LowranceusrFormat lowranceusr_fmt;
  LegacyFormat holux_fmt {holux_vecs};
  LegacyFormat tpg_fmt {tpg_vecs};
  LegacyFormat tpo2_fmt {tpo2_vecs};
  LegacyFormat tpo3_fmt {tpo3_vecs};
  LegacyFormat tmpro_fmt {tmpro_vecs};
  LegacyFormat tiger_fmt {tiger_vecs};
  LegacyFormat easygps_fmt {easygps_vecs};
  LegacyFormat saroute_fmt {saroute_vecs};
  LegacyFormat navicache_fmt {navicache_vecs};
#if SHAPELIB_ENABLED
  ShapeFormat shape_fmt;
#endif
  LegacyFormat gpl_fmt {gpl_vecs};
  LegacyFormat text_fmt {text_vecs};
  LegacyFormat html_fmt {html_vecs};
  LegacyFormat netstumbler_fmt {netstumbler_vecs};
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
  LegacyFormat vpl_fmt {vpl_vecs};
  LegacyFormat wbt_ffmt {wbt_fvecs};
//LegacyFormat wbt_ffmt {wbt_fvecs};
  LegacyFormat hiketech_fmt {hiketech_vecs};
  LegacyFormat glogbook_fmt {glogbook_vecs};
  LegacyFormat vcf_fmt {vcf_vecs};
  LegacyFormat google_dir_fmt {google_dir_vecs};
  LegacyFormat maggeo_fmt {maggeo_vecs};
  LegacyFormat an1_fmt {an1_vecs};
  LegacyFormat tomtom_fmt {tomtom_vecs};
  LegacyFormat tef_xml_fmt {tef_xml_vecs};
  LegacyFormat vitosmt_fmt {vitosmt_vecs};
  LegacyFormat wfff_xml_fmt {wfff_xml_vecs};
  LegacyFormat bcr_fmt {bcr_vecs};
  LegacyFormat ignr_fmt {ignr_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat stmsdf_fmt {stmsdf_vecs};
#endif // CSVFMTS_ENABLED
#if CSVFMTS_ENABLED
  LegacyFormat stmwpp_fmt {stmwpp_vecs};
#endif // CSVFMTS_ENABLED
  LegacyFormat cst_fmt {cst_vecs};
  LegacyFormat nmn4_fmt {nmn4_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat compegps_fmt {compegps_vecs};
#endif // CSVFMTS_ENABLED
  YahooFormat yahoo_fmt;
  UnicsvFormat unicsv_fmt;
  LegacyFormat gtm_fmt {gtm_vecs};
  LegacyFormat gpssim_fmt {gpssim_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat garmin_txt_fmt {garmin_txt_vecs};
#endif // CSVFMTS_ENABLED
  LegacyFormat gtc_fmt {gtc_vecs};
  LegacyFormat dmtlog_fmt {dmtlog_vecs};
  LegacyFormat raymarine_fmt {raymarine_vecs};
  LegacyFormat alanwpr_fmt {alanwpr_vecs};
  LegacyFormat alantrl_fmt {alantrl_vecs};
  LegacyFormat vitovtt_fmt {vitovtt_vecs};
  LegacyFormat ggv_log_fmt {ggv_log_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat g7towin_fmt {g7towin_vecs};
#endif // CSVFMTS_ENABLED
  LegacyFormat garmin_gpi_fmt {garmin_gpi_vecs};
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
  LegacyFormat exif_fmt {exif_vecs};
  LegacyFormat vidaone_fmt {vidaone_vecs};
  LegacyFormat igo8_fmt {igo8_vecs};
  LegacyFormat gopal_fmt {gopal_vecs};
  LegacyFormat humminbird_fmt {humminbird_vecs};
  LegacyFormat humminbird_ht_fmt {humminbird_ht_vecs};
  LegacyFormat mapasia_tr7_fmt {mapasia_tr7_vecs};
  LegacyFormat gnav_trl_fmt {gnav_trl_vecs};
  LegacyFormat navitel_trk_fmt {navitel_trk_vecs};
  LegacyFormat ggv_ovl_fmt {ggv_ovl_vecs};
#if CSVFMTS_ENABLED
  LegacyFormat jtr_fmt {jtr_vecs};
#endif // CSVFMTS_ENABLED
  LegacyFormat itracku_fmt {itracku_vecs};
  LegacyFormat itracku_ffmt {itracku_fvecs};
  LegacyFormat sbp_fmt {sbp_vecs};
  LegacyFormat sbn_fmt {sbn_vecs};
  LegacyFormat mmo_fmt {mmo_vecs};
  LegacyFormat bushnell_fmt {bushnell_vecs};
  LegacyFormat bushnell_trl_fmt {bushnell_trl_vecs};
  LegacyFormat skyforce_fmt {skyforce_vecs};
  LegacyFormat pocketfms_bc_fmt {pocketfms_bc_vecs};
  LegacyFormat pocketfms_fp_fmt {pocketfms_fp_vecs};
  LegacyFormat pocketfms_wp_fmt {pocketfms_wp_vecs};
  LegacyFormat v900_fmt {v900_vecs};
  LegacyFormat ng_fmt {ng_vecs};
  LegacyFormat enigma_fmt {enigma_vecs};
  LegacyFormat skytraq_fmt {skytraq_vecs};
  LegacyFormat teletype_fmt {teletype_vecs};
  LegacyFormat skytraq_ffmt {skytraq_fvecs};
  LegacyFormat miniHomer_fmt {miniHomer_vecs};
  LegacyFormat jogmap_fmt {jogmap_vecs};
  LegacyFormat wintec_tes_fmt {wintec_tes_vecs};
  SubripFormat subrip_fmt;
  LegacyFormat format_garmin_xt_fmt {format_garmin_xt_vecs};
  GarminFitFormat format_fit_fmt;
  LegacyFormat mapbar_track_fmt {mapbar_track_vecs};
  LegacyFormat f90g_track_fmt {f90g_track_vecs};
  LegacyFormat mapfactor_fmt {mapfactor_vecs};
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
      &mapsend_fmt,
      "mapsend",
      "Magellan Mapsend",
      nullptr,
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
      &pcx_fmt,
      "pcx",
      "Garmin PCX5",
      "pcx",
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
      &gpsutil_fmt,
      "gpsutil",
      "gpsutil",
      nullptr,
      nullptr,
    },
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
      &tmpro_fmt,
      "tmpro",
      "TopoMapPro Places File",
      "tmpro",
      nullptr,
    },
    {
      &tiger_fmt,
      "tiger",
      "U.S. Census Bureau Tiger Mapping Service",
      nullptr,
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
    {
      &navicache_fmt,
      "navicache",
      "Navicache.com XML",
      nullptr,
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
      &gpl_fmt,
      "gpl",
      "DeLorme GPL",
      "gpl",
      nullptr,
    },
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
      &netstumbler_fmt,
      "netstumbler",
      "NetStumbler Summary File (text)",
      nullptr,
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
      &vpl_fmt,
      "vpl",
      "Honda/Acura Navigation System VP Log File Format",
      nullptr,
      nullptr,
    },
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
      &maggeo_fmt,
      "maggeo",
      "Magellan Explorist Geocaching",
      "gs",
      nullptr,
    },
    {
      &an1_fmt,
      "an1",
      "DeLorme .an1 (drawing) file",
      "an1",
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
      &vitosmt_fmt,
      "vitosmt",
      "Vito Navigator II tracks",
      "smt",
      nullptr,
    },
    {
      &wfff_xml_fmt,
      "wfff",
      "WiFiFoFum 2.0 for PocketPC XML",
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
#if CSVFMTS_ENABLED
    {
      &stmsdf_fmt,
      "stmsdf",
      "Suunto Trek Manager (STM) .sdf files",
      "sdf",
      nullptr,
    },
#endif
#if CSVFMTS_ENABLED
    {
      &stmwpp_fmt,
      "stmwpp",
      "Suunto Trek Manager (STM) WaypointPlus files",
      "txt",
      nullptr,
    },
#endif //  CSVFMTS_ENABLED
    {
      &cst_fmt,
      "cst",
      "CarteSurTable data file",
      "cst",
      nullptr,
    },
    {
      &nmn4_fmt,
      "nmn4",
      "Navigon Mobile Navigator .rte files",
      "rte",
      nullptr,
    },
#if CSVFMTS_ENABLED
    {
      &compegps_fmt,
      "compegps",
      "CompeGPS data files (.wpt/.trk/.rte)",
      nullptr,
      nullptr,
    },
#endif //CSVFMTS_ENABLED
    {
      &yahoo_fmt,
      "yahoo",
      "Yahoo Geocode API data",
      nullptr,
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
      &alanwpr_fmt,
      "alanwpr",
      "Alan Map500 waypoints and routes (.wpr)",
      "wpr",
      nullptr,
    },
    {
      &alantrl_fmt,
      "alantrl",
      "Alan Map500 tracklogs (.trl)",
      "trl",
      nullptr,
    },
    {
      &vitovtt_fmt,
      "vitovtt",
      "Vito SmartMap tracks (.vtt)",
      "vtt",
      nullptr,
    },
    {
      &ggv_log_fmt,
      "ggv_log",
      "Geogrid-Viewer tracklogs (.log)",
      "log",
      nullptr,
    },
#if CSVFMTS_ENABLED
    {
      &g7towin_fmt,
      "g7towin",
      "G7ToWin data files (.g7t)",
      "g7t",
      nullptr,
    },
#endif
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
      &vidaone_fmt,
      "vidaone",
      "VidaOne GPS for Pocket PC (.gpb)",
      "gpb",
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
      &gopal_fmt,
      "gopal",
      "GoPal GPS track log (.trk)",
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
#if CSVFMTS_ENABLED
    {
      &jtr_fmt,
      "jtr",
      "Jelbert GeoTagger data file",
      "jtr",
      nullptr,
    },
#endif
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
      &bushnell_fmt,
      "bushnell",
      "Bushnell GPS Waypoint file",
      "wpt",
      nullptr,
    },
    {
      &bushnell_trl_fmt,
      "bushnell_trl",
      "Bushnell GPS Trail file",
      "trl",
      nullptr,
    },
    {
      &skyforce_fmt,
      "skyforce",
      "Skymap / KMD150 ascii files",
      nullptr,
      nullptr,
    },
    {
      &pocketfms_bc_fmt,
      "pocketfms_bc",
      "PocketFMS breadcrumbs",
      nullptr,
      nullptr,
    },
    {
      &pocketfms_fp_fmt,
      "pocketfms_fp",
      "PocketFMS flightplan (.xml)",
      "xml",
      nullptr,
    },
    {
      &pocketfms_wp_fmt,
      "pocketfms_wp",
      "PocketFMS waypoints (.txt)",
      "txt",
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
      &ng_fmt,
      "naviguide",
      "Naviguide binary route file (.twl)",
      "twl",
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
      &jogmap_fmt,
      "jogmap",
      "Jogmap.de XML format",
      "xml",
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
#endif // VECS_H_INCLUDED_
