/*
    Describe vectors containing file operations.
 
    Copyright (C) 2002, 2004, 2005, 2006, 2007 Robert Lipe, robertlipe@usa.net

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
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA

 */

#include <stdio.h>
#include "defs.h"
#include "csv_util.h"
#include "inifile.h"
#include "gbversion.h"

#define MYNAME "vecs.c"

typedef struct {
	ff_vecs_t *vec;
	const char *name;
	const char *desc;
	const char *extension;
	const char *parent;
} vecs_t;

extern ff_vecs_t an1_vecs;
extern ff_vecs_t axim_gpb_vecs;
extern ff_vecs_t bcr_vecs;
extern ff_vecs_t brauniger_iq_vecs;
extern ff_vecs_t cetus_vecs;
extern ff_vecs_t coastexp_vecs;
extern ff_vecs_t compegps_vecs;
extern ff_vecs_t copilot_vecs;
extern ff_vecs_t coto_vecs;
extern ff_vecs_t cst_vecs;
extern ff_vecs_t delbin_vecs;
extern ff_vecs_t dg100_vecs;
extern ff_vecs_t easygps_vecs;
extern ff_vecs_t garmin_vecs;
extern ff_vecs_t garmin_txt_vecs;
extern ff_vecs_t gcdb_vecs;
extern ff_vecs_t gdb_vecs;
extern ff_vecs_t geoniche_vecs;
extern ff_vecs_t geo_vecs;
extern ff_vecs_t glogbook_vecs;
extern ff_vecs_t google_vecs;
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
extern ff_vecs_t magpdb_vecs;
extern ff_vecs_t mag_svecs;
extern ff_vecs_t magX_fvecs;
extern ff_vecs_t mapsend_vecs;
extern ff_vecs_t mps_vecs;
extern ff_vecs_t msroute_vecs;
extern ff_vecs_t mtk_vecs;
extern ff_vecs_t mtk_fvecs;
extern ff_vecs_t mtk_m241_vecs;
extern ff_vecs_t mtk_m241_fvecs;
extern ff_vecs_t navicache_vecs;
extern ff_vecs_t netstumbler_vecs;
extern ff_vecs_t nmea_vecs;
extern ff_vecs_t nmn4_vecs;
extern ff_vecs_t overlay_vecs;
extern ff_vecs_t ozi_vecs;
extern ff_vecs_t palmdoc_vecs;
extern ff_vecs_t pcx_vecs;
extern ff_vecs_t ppdb_vecs;
extern ff_vecs_t psit_vecs;             /* MRCB */
extern ff_vecs_t psp_vecs;
extern ff_vecs_t quovadis_vecs;
extern ff_vecs_t saroute_vecs;
extern ff_vecs_t shape_vecs;
extern ff_vecs_t skytraq_vecs;
extern ff_vecs_t skytraq_fvecs;
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
extern ff_vecs_t format_garmin_xt_vecs;

static
vecs_t vec_list[] = {
#if CSVFMTS_ENABLED
	/* XCSV must be the first entry in this table. */
	{
		&xcsv_vecs,
		"xcsv",
		"? Character Separated Values",
		NULL
	},
#endif
	{
		&geo_vecs, 
		"geo",
		"Geocaching.com .loc",
		"loc"
	}, 
	{
		&gpx_vecs,
		"gpx",
		"GPX XML",
		"gpx"
	},
	{
		&mag_svecs,
		"magellan",
		"Magellan serial protocol", 
		NULL
	},
	{
		&mag_fvecs,
		"magellan",
		"Magellan SD files (as for Meridian)", 
		NULL
	},
	{
		&magX_fvecs,
		"magellanx",
		"Magellan SD files (as for eXplorist)", 
		"upt"
	},
	{
		&garmin_vecs,
		"garmin",
		"Garmin serial/USB protocol", 
		NULL
	},
	{
		&gdb_vecs,
		"gdb",
		"Garmin MapSource - gdb",
		"gdb"
	},
	{
		&gtc_vecs,
		"gtrnctr",
		"Garmin Training Center",
		"xml"
	},
	{
		&mapsend_vecs,
		"mapsend",
		"Magellan Mapsend", 
		NULL
	},
	{
		&mps_vecs,
		"mapsource",
		"Garmin MapSource - mps",
		"mps"
	},
	{
		&nmea_vecs,
		"nmea",
		"NMEA 0183 sentences",
		NULL
	},
	{
		&ozi_vecs,
		"ozi",
		"OziExplorer",
		NULL
	},
	{
		&pcx_vecs,
		"pcx",
		"Garmin PCX5",
		"pcx"
	},
	{
		&kml_vecs,
		"kml",
		"Google Earth (Keyhole) Markup Language",
		"kml"
	},
#if MAXIMAL_ENABLED
	{
		&gpsutil_vecs,
		"gpsutil",
		"gpsutil", 
		NULL
	},
	{
		&psp_vecs,
		"psp",
		"MS PocketStreets 2002 Pushpin",
		"psp"
	},
	{
		&lowranceusr_vecs,
		"lowranceusr",
		"Lowrance USR",
		"usr"
	},
#if PDBFMTS_ENABLED
	{
		&cetus_vecs,
		"cetus",
		"Cetus for Palm/OS", 
		"pdb"
	},
	{
		&copilot_vecs,
		"copilot",
		"CoPilot Flight Planner for Palm/OS", 
		"pdb"
	},
	{
		&gpspilot_vecs,
		"gpspilot",
		"GPSPilot Tracker for Palm/OS", 
		"pdb"
	},
	{
		&magnav_vec,
		"magnav",
		"Magellan NAV Companion for Palm/OS", 
		"pdb"
	},
#endif /* PDBFMTS_ENABLED */
	{
		&holux_vecs,
		"holux",
		"Holux (gm-100) .wpo Format",
		"wpo"
	},
	{
		&tpg_vecs,
		"tpg",
		"National Geographic Topo .tpg (waypoints)",
		"tpg"
	},
	{
		&tpo2_vecs,
		"tpo2",
		"National Geographic Topo 2.x .tpo",
		"tpo"
	},
	{
		&tpo3_vecs,
		"tpo3",
		"National Geographic Topo 3.x/4.x .tpo",
		"tpo"
	},
	{
		&tmpro_vecs,
		"tmpro",
		"TopoMapPro Places File",
		"tmpro"
	},
#if PDBFMTS_ENABLED
	{
		&gcdb_vecs,
		"gcdb",
		"GeocachingDB for Palm/OS", 
		"pdb"
	},
#endif
	{
		&tiger_vecs,
		"tiger",
		"U.S. Census Bureau Tiger Mapping Service",
		NULL
	},
	{
		&easygps_vecs,
		"easygps",
		"EasyGPS binary format",
		"loc"
	},
#if PDBFMTS_ENABLED
	{
		&quovadis_vecs,
		"quovadis",
		"Quovadis",
		"pdb"
	},
	{
		&gpilots_vecs,
		"gpilots",
		"GpilotS",
		"pdb"
	},
#endif
	{
		&saroute_vecs,
		"saroute",
		"DeLorme Street Atlas Route",
		"anr"
	},
	{
		&navicache_vecs,
		"navicache",
		"Navicache.com XML",
		NULL
	},
	{
		&coastexp_vecs,
		"coastexp",
		"CoastalExplorer XML",
		NULL
	},
	{	/* MRCB */
		&psit_vecs,
		"psitrex",
		"KuDaTa PsiTrex text",
		NULL
	},
#if SHAPELIB_ENABLED
	{
		&shape_vecs,
		"shape",
		"ESRI shapefile",
		"shp"
	},
#endif
#if PDBFMTS_ENABLED
	{
		&geoniche_vecs,
		"geoniche",
		"GeoNiche .pdb",
		"pdb"
	},
#endif
	{
		&gpl_vecs,
		"gpl",
		"DeLorme GPL",
		"gpl"
	},
	{
		&text_vecs,
		"text",
		"Textual Output",
		"txt"
	},
	{
		&html_vecs,
		"html",
		"HTML Output",
		"html"
	},
#if PDBFMTS_ENABLED
	{
		&palmdoc_vecs,
		"palmdoc",
		"PalmDoc Output",
		"pdb"
	},
#endif
	{
		&netstumbler_vecs,
		"netstumbler",
		"NetStumbler Summary File (text)",
		NULL
	},
	{
		&HsaEndeavourNavigator_vecs,
		"hsandv",
		"HSA Endeavour Navigator export File",
		NULL
	},
        {
                &igc_vecs,
                "igc",
                "FAI/IGC Flight Recorder Data Format",
                NULL
        },
        {
                &brauniger_iq_vecs,
                "baroiq",
                "Brauniger IQ Series Barograph Download",
                NULL
        },
        {
                &mtk_vecs,
                "mtk",
                "MTK Logger (iBlue 747,Qstarz BT-1000,...) download",
                NULL
        },
        {
                &mtk_fvecs,
                "mtk-bin",
                "MTK Logger (iBlue 747,...) Binary File Format",
                "bin"
        },
        {
                &mtk_m241_vecs,
                "m241",
                "Holux M-241 (MTK based) download",
                NULL
        },
        {
                &mtk_m241_fvecs,
                "m241-bin",
                "Holux M-241 (MTK based) Binary File Format",
                "bin"
        },
#endif // MAXIMAL_ENABLED
        {
                &wbt_svecs,
                "wbt",
                "Wintec WBT-100/200 GPS Download",
                NULL
        },
		{
				&vpl_vecs,
				"vpl",
				"Honda/Acura Navigation System VP Log File Format",
				NULL
		},
#if MAXIMAL_ENABLED
        {
                &wbt_fvecs,
                "wbt-bin",
                "Wintec WBT-100/200 Binary File Format",
                "bin"
        },
        {
                &wbt_fvecs,
                "wbt-tk1",
                "Wintec WBT-201/G-Rays 2 Binary File Format",
                "tk1"
        },
        {
                &hiketech_vecs,
                "hiketech",
                "HikeTech",
                "gps"
        },
        {
                &glogbook_vecs,
                "glogbook",
                "Garmin Logbook XML",
                "xml"
        },
	{
                &vcf_vecs,
                "vcard",
                "Vcard Output (for iPod)",
                "vcf",
        },
#if 0
	{
		&overlay_vecs,
		"overlay",
		"Geogrid-Viewer",
		"ovl"
	},
#endif
	{
		&google_vecs,
		"google",
		"Google Maps XML",
		"xml"
	},
	{
		&maggeo_vecs,
		"maggeo",
		"Magellan Explorist Geocaching",
		"gs"
	},
	{
		&an1_vecs,
		"an1",
		"DeLorme .an1 (drawing) file",
		"an1"
	},
	{
		&tomtom_vecs,
		"tomtom",
		"TomTom POI file (.ov2)",
		"ov2"
	},
	{
		&tef_xml_vecs,
		"tef",
		"Map&Guide 'TourExchangeFormat' XML",
		"xml"
	},
#if PDBFMTS_ENABLED
	{
		&ppdb_vecs,
		"pathaway",
		"PathAway Database for Palm/OS",
		"pdb"
	},
#endif
	{
		&vitosmt_vecs,
		"vitosmt",
		"Vito Navigator II tracks",
		"smt"
	},
	{
		&wfff_xml_vecs,
		"wfff",
		"WiFiFoFum 2.0 for PocketPC XML",
		"xml"
	},
	{
		&bcr_vecs,
		"bcr",
		"Motorrad Routenplaner (Map&Guide) .bcr files",
		"bcr"
	},
#if PDBFMTS_ENABLED
	{
		&coto_vecs,
		"coto",
		"cotoGPS for Palm/OS", 
		"pdb"
	},
#endif
	{
		&ignr_vecs,
		"ignrando",
		"IGN Rando track files",
		"rdn"
	},
#if CSVFMTS_ENABLED
	{
		&stmsdf_vecs,
		"stmsdf",
		"Suunto Trek Manager (STM) .sdf files",
		"sdf"
	},
#endif
#if CSVFMTS_ENABLED
	{
		&stmwpp_vecs,
		"stmwpp",
		"Suunto Trek Manager (STM) WaypointPlus files",
		"txt"
	},
#endif //  CSVFMTS_ENABLED
	{
		&msroute_vecs,
		"msroute",
		"Microsoft AutoRoute 2002 (pin/route reader)",
		"axe"
	},
	{
		&msroute_vecs,
		"msroute",
		"Microsoft Streets and Trips (pin/route reader)" ,
		"est"
	},
	{
		&cst_vecs,
		"cst",
		"CarteSurTable data file",
		"cst"
	},
	{
		&nmn4_vecs,
		"nmn4",
		"Navigon Mobile Navigator .rte files",
		"rte"
	},
#if PDBFMTS_ENABLED
	{
		&magpdb_vecs,
		"mag_pdb",
		"Map&Guide to Palm/OS exported files (.pdb)",
		"pdb"
	},
#endif
#if CSVFMTS_ENABLED
	{
		&compegps_vecs,
		"compegps",
		"CompeGPS data files (.wpt/.trk/.rte)",
		NULL
	},
#endif //CSVFMTS_ENABLED
	{
		&yahoo_vecs,
		"yahoo",
		"Yahoo Geocode API data",
		NULL
	},
	{
		&unicsv_vecs,
		"unicsv",
		"Universal csv with field structure in first line",
		NULL
	},
	{
		&gtm_vecs,
		"gtm",
		"GPS TrackMaker",
		"gtm"
	},
	{
		&gpssim_vecs,
		"gpssim",
		"Franson GPSGate Simulation",
		"gpssim"
	},
#if CSVFMTS_ENABLED
        {
		&garmin_txt_vecs,
		"garmin_txt",
		"Garmin MapSource - txt (tab delimited)",
		"txt"
	},
#endif // CSVFMTS_ENABLED
        {
		&axim_gpb_vecs,
		"axim_gpb",
		"Dell Axim Navigation System (.gpb) file format",
		"gpb"
	},
	{
		&gtc_vecs,
		"gtrnctr",
		"Garmin Training Center (.tcx)",
		"xml"
	},
	{
		&dmtlog_vecs,
		"dmtlog",
		"TrackLogs digital mapping (.trl)",
		"trl"
	},
	{
		&raymarine_vecs,
		"raymarine",
		"Raymarine Waypoint File (.rwf)",
		"rwf"
	},
        {
                &alanwpr_vecs,
                "alanwpr",
                "Alan Map500 waypoints and routes (.wpr)",
                "wpr"
        },
        {
                &alantrl_vecs,
                "alantrl",
                "Alan Map500 tracklogs (.trl)",
                "trl"
        },
        {
                &vitovtt_vecs,
                "vitovtt",
		"Vito SmartMap tracks (.vtt)",
		"vtt"
        },
        {
                &ggv_log_vecs,
                "ggv_log",
                "Geogrid-Viewer tracklogs (.log)",
                "log"
        },
#if CSVFMTS_ENABLED
        {
		&g7towin_vecs,
		"g7towin",
		"G7ToWin data files (.g7t)",
		"g7t"
	},
#endif
        {
                &garmin_gpi_vecs,
                "garmin_gpi",
                "Garmin Points of Interest (.gpi)",
                "gpi"
        },
        {
                &lmx_vecs,
                "lmx",
                "Nokia Landmark Exchange",
                NULL
        },
        {
                &random_vecs,
                "random",
                "Internal GPS data generator",
                NULL
        },
        {
                &xol_vecs,
                "xol",
                "Swiss Map 25/50/100 (.xol)",
                "xol"
        },
        {
                &dg100_vecs,
                "dg-100",
                "GlobalSat DG-100/BT-335 Download",
                NULL
        },
        {
                &navilink_vecs,
                "navilink",
                "NaviGPS GT-11/BGT-11 Download",
                NULL
        },
        {
                &ik3d_vecs,
                "ik3d",
                "MagicMaps IK3D project file (.ikt)",
                "ikt"
        },
        {
                &osm_vecs,
                "osm",
                "OpenStreetMap data files",
                "xml"
        },
        {
                &destinator_poi_vecs,
                "destinator_poi",
                "Destinator Points of Interest (.dat)",
                "dat"
        },
        {
                &destinator_itn_vecs,
                "destinator_itn",
                "Destinator Itineraries (.dat)",
                "dat"
        },
        {
                &destinator_trl_vecs,
                "destinator_trl",
                "Destinator TrackLogs (.dat)",
                "dat"
        },
        {
                &exif_vecs,
                "exif",
                "Embedded Exif-GPS data (.jpg)",
		"jpg"
        },
        {
                &vidaone_vecs,
                "vidaone",
                "VidaOne GPS for Pocket PC (.gpb)",
		"gpb"
        },
	{
		&igo8_vecs,
		"igo8",
		"IGO8 .trk",
		"trk"
	},
	{
                &gopal_vecs,
                "gopal",
                "GoPal GPS track log (.trk)",
		"trk"
        },
	{
		&humminbird_vecs,
		"humminbird",
		"Humminbird waypoints and routes (.hwr)",
		"hwr"
        },
	{
		&humminbird_ht_vecs,
		"humminbird_ht",
		"Humminbird tracks (.ht)",
		"ht"
        },
	{
		&mapasia_tr7_vecs,
		"mapasia_tr7",
		"MapAsia track file (.tr7)",
		"tr7"
	},
	{
		&gnav_trl_vecs,
		"gnav_trl",
		"Google Navigator Tracklines (.trl)",
		"trl"
	},
	{
		&navitel_trk_vecs,
		"navitel_trk",
		"Navitel binary track (.bin)",
		"bin"
	},
        {
                &ggv_ovl_vecs,
                "ggv_ovl",
                "Geogrid-Viewer ascii overlay file (.ovl)",
                "ovl"
        },
#if CSVFMTS_ENABLED
        {
                &jtr_vecs,
                "jtr",
                "Jelbert GeoTagger data file",
                "jtr"
        },
#endif
        {
                &itracku_vecs,
                "itracku",
                "XAiOX iTrackU Logger",
                NULL
        },

        {
                &itracku_fvecs,
                "itracku-bin",
                "XAiOX iTrackU Logger Binary File Format",
                "bin"
        },
        {
                &sbp_vecs,
                "sbp",
                "NaviGPS GT-31/BGT-31 datalogger (.sbp)",
                "sbp"
        },
        {
                &sbn_vecs,
                "sbn",
                "NaviGPS GT-31/BGT-31 SiRF binary logfile (.sbn)",
                "sbn"
        },
        {
                &mmo_vecs,
                "mmo",
                "Memory-Map Navigator overlay files (.mmo)",
                "mmo"
        },
        {
                &bushnell_vecs,
                "bushnell",
                "Bushnell GPS Waypoint file",
                "wpt"
        },
        {
                &bushnell_trl_vecs,
                "bushnell_trl",
                "Bushnell GPS Trail file",
                "trl"
        },
        {
        	&skyforce_vecs,
        	"skyforce",
        	"Skymap / KMD150 ascii files",
        	NULL
        },
        {
        	&pocketfms_bc_vecs,
        	"pocketfms_bc",
        	"PocketFMS breadcrumbs",
        	NULL
        },
        {
        	&pocketfms_fp_vecs,
        	"pocketfms_fp",
        	"PocketFMS flightplan (.xml)",
        	"xml"
        },
        {
        	&pocketfms_wp_vecs,
        	"pocketfms_wp",
        	"PocketFMS waypoints (.txt)",
        	"txt"
        },
        {
        	&v900_vecs,
        	"v900",
        	"Columbus/Visiontac V900 files (.csv)",
        	NULL
        },
        {
        	&ng_vecs,
        	"naviguide",
        	"Naviguide binary route file (.twl)",
        	"twl"
        },
        {
        	&enigma_vecs,
        	"enigma",
        	"Enigma binary waypoint file (.ert)",
        	"ert"
        },
	{
		&delbin_vecs, 
		"delbin",
		"DeLorme PN-20/PN-30/PN-40 USB protocol",
		NULL
	}, 
        {
                &skytraq_vecs,
                "skytraq",
                "SkyTraq Venus based loggers (download)",
                NULL
        },
        {
                &teletype_vecs,
                "teletype",
                "Teletype [ Get Jonathon Johnson to describe",
                NULL
        },
        {
                &skytraq_fvecs,
                "skytraq-bin",
                "SkyTraq Venus based loggers Binary File Format",
                "bin"
        },
        {
                &jogmap_vecs,
                "jogmap",
                "Jogmap.de XML format",
                "xml"
        },
        {
                &wintec_tes_vecs,
                "wintec_tes",
                "Wintec TES file",
                "tes"
        },
        {
        	&format_garmin_xt_vecs,
        	"garmin_xt",
        	"Mobile Garmin XT Track files",
        	NULL
        },
#endif // MAXIMAL_ENABLED
	{
		NULL,
		NULL,
		NULL,
		NULL
	}
};

void
init_vecs(void)
{
	vecs_t *vec = vec_list;
	while ( vec->vec ) {
		arglist_t *ap;
		if ( vec->vec->args ) {
			for ( ap = vec->vec->args; ap->argstring; ap++ ) {
				ap->argvalptr = NULL;
				if (ap->argval) *ap->argval = NULL;
			}
		}
		vec++;
	}
}

int
is_integer(const char *c)
{
	return isdigit(c[0]) || ((c[0] == '+' || c[0] == '-') && isdigit(c[1]));
}

void 
exit_vecs( void )
{
	vecs_t *vec = vec_list;
	while ( vec->vec ) {
		arglist_t *ap;
		if ( vec->vec->exit ) {
			(*vec->vec->exit)();
		}
		if ( vec->vec->args ) {
			for ( ap = vec->vec->args; ap->argstring; ap++ ) {
				if ( ap->defaultvalue && 
					( ap->argtype == ARGTYPE_INT ) &&
					! is_integer(ap->defaultvalue)) {
					warning("%s: not an integer\n", ap->argstring);
				}
				if ( ap->argvalptr ) {
					xfree(ap->argvalptr);
					*ap->argval = ap->argvalptr = NULL;
				}
			}
		}
		vec++;
	}
}

void
assign_option(const char *module, arglist_t *ap, const char *val)
{
	char *c;

	if (ap->argval == NULL)
		fatal("%s: No local variable defined for option \"%s\"!", module, ap->argstring);

	if (ap->argvalptr != NULL) {
		xfree(ap->argvalptr);
		ap->argvalptr = NULL;
	}
	if (ap->argval) *ap->argval = NULL;

	if (val == NULL) return;

	if (case_ignore_strcmp(val, ap->argstring) == 0) c = "";
	else c = (char *)val;

	switch(ap->argtype & ARGTYPE_TYPEMASK) {
		case ARGTYPE_INT:
			if (*c == '\0') c = "0";
			else {
				int test;
				is_fatal(1 != sscanf(c, "%d", &test), 
					"%s: Invalid parameter value %s for option %s", module, val, ap->argstring);
			}
			break;
		case ARGTYPE_FLOAT:
			if (*c == '\0') c = "0";
			else {
				double test;
				is_fatal(1 != sscanf(c, "%lf", &test), 
					"%s: Invalid parameter value %s for option %s", module, val, ap->argstring);
			}
			break;
		case ARGTYPE_BOOL:
			if (*c == '\0') c = "1";
			else {
				switch(*c) {
					case 'Y':
					case 'y': c = "1"; break;
					case 'N':
					case 'n': c = "0"; break;
					default:
						if (isdigit(*c)) {
							if (*c == '0') c = "0";
							else c = "1";
						}
						else {
							warning(MYNAME ": Invalid logical value '%s' (%s)!\n", c, module);
							c = "0";
						}
						break;
				}
			}
			break;
	}

	/* for bool options without default: don't set argval if "FALSE" */

	if (((ap->argtype & ARGTYPE_TYPEMASK) == ARGTYPE_BOOL) && 
	    (*c == '0') && (ap->defaultvalue == NULL)) {
		return;
	}
	*ap->argval = ap->argvalptr = xstrdup(c);
}

void
disp_vec_options(const char *vecname, arglist_t *ap)
{
	for (ap = ap; ap && ap->argstring; ap++) {
		if (*ap->argval && ap->argval) {
			printf("options: module/option=value: %s/%s=\"%s\"", 
				vecname, ap->argstring, *ap->argval);
			if (ap->defaultvalue && (case_ignore_strcmp(ap->defaultvalue, *ap->argval) == 0)) 
				printf(" (=default)");
			printf("\n");
		}
	}
}

ff_vecs_t *
find_vec(char *const vecname, char **opts)
{
	vecs_t *vec = vec_list;
	style_vecs_t *svec = style_list;
	char *v = xstrdup(vecname);
	char *svecname = strtok(v, ",");
	int found = 0;

	if (vecname == NULL) {
		fatal("A format name is required.\n");
	}

	while (vec->vec) {
		arglist_t *ap;
		char *res;

		if (case_ignore_strcmp(svecname, vec->name)) {
			vec++;
			continue;
		}

		res = strchr(vecname, ',');
		if (res) {
			*opts = strchr(vecname, ',')+1;
		} else {
			*opts = NULL;
		}
		
		if (vec->vec->args) {
			for (ap = vec->vec->args; ap->argstring; ap++) {
				const char *opt;
				
				if ( res ) {
					opt = get_option(*opts, ap->argstring);
					if ( opt ) {
						found = 1;
						assign_option(svecname, ap, opt);
						xfree((char *)opt);
						continue;
					}
				}
				opt = inifile_readstr(global_opts.inifile, vec->name, ap->argstring);
				if (opt == NULL) opt = inifile_readstr(global_opts.inifile, "Common format settings", ap->argstring);
				if (opt == NULL) opt = ap->defaultvalue;
				assign_option(vec->name, ap, opt);
			}
		}
		if (opts && opts[0] && !found) {
			warning("'%s' is an unknown option to %s.\n", *opts, vec->name);
		}

		if (global_opts.debug_level >= 1)
			disp_vec_options(vec->name, vec->vec->args);
		
#if CSVFMTS_ENABLED		
		// xcsv_setup_internal_style( NULL );
#endif // CSVFMTS_ENABLED		
		xfree(v);
		vec->vec->name = vec->name;	/* needed for session information */
		return vec->vec;
		
	}

	/* 
	 * Didn't find it in the table of "real" file types, so plan B
	 * is to search the list of xcsv styles.
	 */
	while (svec->name) {
		arglist_t *ap;
		char *res;

		if (case_ignore_strcmp(svecname, svec->name)) {
			svec++;
			continue;
		}

		res = strchr(vecname, ',');
		if (res) {
			*opts = strchr(vecname, ',') + 1;
		} else {
			*opts = NULL;
		}
		
		if (vec_list[0].vec->args) {
			for (ap = vec_list[0].vec->args; ap->argstring; ap++) {
				const char *opt;
				
				if ( res ) {
					opt = get_option(*opts, ap->argstring);
					if ( opt ) {
						found = 1;
						assign_option(svecname, ap, opt);
						xfree((char *)opt);
						continue;
					}
				}
				opt = inifile_readstr(global_opts.inifile, svec->name, ap->argstring);
				if (opt == NULL) opt = inifile_readstr(global_opts.inifile, "Common format settings", ap->argstring);
				if (opt == NULL) opt = ap->defaultvalue;
				assign_option(svec->name, ap, opt);
			}
		}

		if (opts && opts[0] && !found) {
			warning("'%s' is an unknown option to %s.\n", *opts, svec->name);
		}

		if (global_opts.debug_level >= 1)
			disp_vec_options(svec->name, vec_list[0].vec->args);
#if CSVFMTS_ENABLED
		xcsv_setup_internal_style(svec->style_buf);
#endif // CSVFMTS_ENABLED

		xfree(v);
		vec_list[0].vec->name = svec->name;	/* needed for session information */
		return vec_list[0].vec;
	}
	
	/*
	 * Not found.
	 */
	xfree(v);
	return NULL;
}

/*
 * Find and return a specific argument in an arg list.
 * Modelled approximately after getenv.
 */
char *
#ifdef DEBUG_MEM
GET_OPTION(const char *iarglist, const char *argname, DEBUG_PARAMS)
#else
get_option(const char *iarglist, const char *argname)
#endif
{
	size_t arglen = strlen(argname);
	char *arglist;
	char *rval = NULL;
	char *arg;
	char *argp;

	if (!iarglist) {
		return NULL;
	}

	arglen = strlen(argname);
	arglist = xstrdup(iarglist);

	for (arg = arglist; argp = strtok(arg, ","), NULL != argp; arg = NULL) {
		if (0 == case_ignore_strncmp(argp, argname, arglen)) {
			/*
			 * If we have something of the form "foo=bar"
			 * return "bar".   Otherwise, we assume we have
			 * simply "foo" so we return that.
			 */
            if (argp[arglen] == '=') {
				rval = argp + arglen + 1;
                break;
            }
            else if (argp[arglen] == '\0') {
				rval = argp;
                break;
            }
		}
	}
	/*
	 * Return an offset into the allocated copy.
	 * The caller mustn't free or otherwise get froggy with 
	 * this data.
	 */
	if ( rval ) {
		rval = xxstrdup(rval,file, line);
	}
	xfree(arglist);
	return rval;
}

/*
 *  Display the available formats in a format that's easy for humans to
 *  parse for help on available command line options.
 */
static signed int 
alpha (const void *a, const void *b)
{

	const vecs_t *const *ap = (const vecs_t *const*) a;
	const vecs_t *const *bp = (const vecs_t *const*) b;
	
	return case_ignore_strcmp((*ap)->desc , (*bp)->desc);
}

/*
 * Smoosh the vecs list and style lists together and sort them
 * alphabetically.  Returns an allocated copy of a style_vecs_array
 * that's populated and sorted.
 */
vecs_t **
sort_and_unify_vecs(int *ctp)
{
	int vc;
	vecs_t **svp;
	vecs_t *vec;
#if CSVFMTS_ENABLED
	style_vecs_t *svec;
#endif
	int i = 0;

	/* Get a count from both the vec (normal) and the svec (csv) lists */

#if CSVFMTS_ENABLED
	extern size_t nstyles;
	vc = sizeof vec_list / sizeof vec_list[0] - 1 + nstyles;
#else
	vc = sizeof vec_list / sizeof vec_list[0] - 1;
#endif // CSVFMTS_ENABLED


	svp = (vecs_t **)xcalloc(vc, sizeof(style_vecs_t *));
	/* Normal vecs are easy; populate the first part of the array. */
	for (vec = vec_list; vec->vec; vec++, i++) {
		svp[i] = vec;
		if (svp[i]->parent == NULL) {
			svp[i]->parent = svp[i]->name;
		}
	}

#if CSVFMTS_ENABLED
	/* Walk the style list, parse the entries, dummy up a "normal" vec */
	for (svec = style_list; svec->name; svec++, i++)  {
		xcsv_read_internal_style(svec->style_buf);
		svp[i] = (vecs_t*) xcalloc(1, sizeof **svp);
		svp[i]->name = svec->name;
		svp[i]->vec = (ff_vecs_t*) xmalloc(sizeof(*svp[i]->vec));
		svp[i]->extension = xcsv_file.extension;
		*svp[i]->vec = *vec_list[0].vec; /* Interits xcsv opts */
		/* Reset file type to inherit ff_type from xcsv for everything
		 * except the xcsv format itself, which we leave as "internal"
		 */
		if (case_ignore_strcmp(svec->name, "xcsv")) {
			svp[i]->vec->type = xcsv_file.type;
			/* Skip over the first help entry for all but the
			 * actual 'xcsv' format - so we don't expose the
			 * 'full path to xcsv style file' argument to any
			 * GUIs for an internal format.
			 */
			svp[i]->vec->args++;
		}
		memset(&svp[i]->vec->cap, 0, sizeof(svp[i]->vec->cap));
		switch(xcsv_file.datatype) {
			case 0:
			case wptdata:
				svp[i]->vec->cap[ff_cap_rw_wpt] = (ff_cap) (ff_cap_read | ff_cap_write); break;
			case trkdata:
				svp[i]->vec->cap[ff_cap_rw_trk] = (ff_cap) (ff_cap_read | ff_cap_write); break;
			case rtedata:
				svp[i]->vec->cap[ff_cap_rw_rte] = (ff_cap)(ff_cap_read | ff_cap_write); break;
			default: ;
		}
		svp[i]->desc = xcsv_file.description;
		svp[i]->parent = "xcsv";
	}
#endif // CSVFMTS_ENABLED

	/* Now that we have everything in an array, alphabetize them */
	qsort(svp, vc, sizeof(*svp), alpha);

	*ctp = i;
	return svp;
}

#define VEC_FMT "	%-20.20s  %-.50s\n"

void
disp_vecs(void)
{
	vecs_t **svp;
	arglist_t *ap;
	int vc;
	int i = 0;

	svp = sort_and_unify_vecs(&vc);
	for (i=0;i<vc;i++) {
		if ( svp[i]->vec->type == ff_type_internal )  {
			continue;
		}
		printf(VEC_FMT, svp[i]->name, svp[i]->desc);
		for (ap = svp[i]->vec->args; ap && ap->argstring; ap++) {
			if ( !(ap->argtype & ARGTYPE_HIDDEN)) 
				printf("	  %-18.18s    %s%-.50s %s\n",
				ap->argstring, 
				(ap->argtype & ARGTYPE_TYPEMASK) == 
					ARGTYPE_BOOL ? "(0/1) " : "",
				ap->helpstring,
				(ap->argtype & ARGTYPE_REQUIRED)?"(required)":"");
		}
	}
	xfree (svp);
	return;
}

void
disp_vec( const char *vecname )
{
	vecs_t **svp;
	arglist_t *ap;
	int vc;
	int i = 0;

	svp = sort_and_unify_vecs(&vc);
	for (i=0;i<vc;i++) {
		if ( case_ignore_strcmp( svp[i]->name, vecname ))  {
			continue;
		}
		printf(VEC_FMT, svp[i]->name, svp[i]->desc);
		for (ap = svp[i]->vec->args; ap && ap->argstring; ap++) {
			if ( !(ap->argtype & ARGTYPE_HIDDEN)) 
				printf("	  %-18.18s    %s%-.50s %s\n",
				ap->argstring, 
				(ap->argtype & ARGTYPE_TYPEMASK) == 
					ARGTYPE_BOOL ? "(0/1) " : "",
				ap->helpstring,
				(ap->argtype & ARGTYPE_REQUIRED)?"(required)":"");
		}
	}
	xfree (svp);
	return;
}

/*
 * Additional information for V1.
 * Output format type at front of line.
 */
static void
disp_v1(ff_type t)
{
	char *tstring;
		
	switch (t) {
		case ff_type_file: tstring = "file"; break;
		case ff_type_serial: tstring = "serial"; break;
		case ff_type_internal: tstring = "internal"; break;
		default: tstring = "unknown"; break;
	}
	printf("%s\t", tstring);
}

static void
disp_v2(ff_vecs_t *v)
{
	int i;
	for (i = 0; i < 3; i++) {
		putchar(v->cap[i] & ff_cap_read  ? 'r' : '-');
		putchar(v->cap[i] & ff_cap_write  ? 'w' : '-');
	}
	putchar('\t');
}

const char *
name_option(long type)
{
	const char *at[] = {
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
void disp_help_url(const vecs_t *vec, arglist_t *arg)
{
	printf("\t" WEB_DOC_DIR "/fmt_%s.html", vec->name);
	if (arg) {
		printf("#fmt_%s_o_%s",vec->name, arg->argstring);
	}
	printf("\n");
}


static void 
disp_v3(const vecs_t *vec)
{
	arglist_t *ap;

	disp_help_url(vec, NULL);
	for (ap = vec->vec->args; ap && ap->argstring; ap++) {
		if ( !(ap->argtype & ARGTYPE_HIDDEN)) {
			printf("option\t%s\t%s\t%s\t%s\t%s\t%s\t%s",
			vec->name, 
			ap->argstring, 
			ap->helpstring, 
			name_option(ap->argtype),
			ap->defaultvalue? ap->defaultvalue : "",
			ap->minvalue? ap->minvalue : "",
			ap->maxvalue? ap->maxvalue : "");
		}
		disp_help_url(vec, ap);
		printf("\n");
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
	vecs_t **svp;
	vecs_t *vec;
	int i, vc = 0;

	switch(version) {
	case 0:
	case 1:
	case 2:
	case 3:
		svp = sort_and_unify_vecs(&vc);
		for (i=0;i<vc;i++,vec++) {
			vec = svp[i];

			/* Version 1 displays type at front of all types.
			 * Version 0 skips internal types.
			 */
			if (version > 0) {
				disp_v1(vec->vec->type);
			} else {
				if (vec->vec->type == ff_type_internal)
					continue;
			}
			if (version >= 2) {
				disp_v2(vec->vec);
			}
			printf("%s\t%s\t%s%s%s\n", vec->name, 
				vec->extension? vec->extension : "", 
				vec->desc,
				version >= 3 ? "\t" : "",
				version >= 3 ? vec->parent : "");
			if (version >= 3) {
				disp_v3(vec);
			}
		}
		xfree (svp);
		break;
	default:
		;
	}
}
