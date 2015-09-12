/*
    Copyright (C) 2006	Etienne Tasse	etasse@yahoo.com

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

#include "defs.h"
#include "xmlgeneric.h"
#include <QtCore/QXmlStreamAttributes>
#include <stdio.h>

/* argument storage */
static char* 	aicicon	=0;
static char* 	aioicon =0;
static char* 	ahcicon =0;
static char* 	ahoicon =0;
static char* 	snmac	=0;

static
arglist_t wfff_xml_args[] = {
  {
    "aicicon", &aicicon, "Infrastructure closed icon name",
    "Red Square", ARGTYPE_STRING
  },
  {
    "aioicon", &aioicon, "Infrastructure open icon name",
    "Green Square", ARGTYPE_STRING
  },
  {
    "ahcicon", &ahcicon, "Ad-hoc closed icon name",
    "Red Diamond", ARGTYPE_STRING
  },
  {
    "ahoicon", &ahoicon, "Ad-hoc open icon name",
    "Green Diamond", ARGTYPE_STRING
  },
  {"snmac", &snmac, "Shortname is MAC address", NULL, ARGTYPE_BOOL },
  {0, 0, 0, 0, 0}
};

#define xfreez(p) { if (p) xfree(p); p=0; }

#define MYNAME "wfff_xml"

static xg_callback	wfff_s,      wfff_e;
static xg_callback	wfff_wep,    wfff_mac,    wfff_type;
static xg_callback	wfff_ssid,   wfff_chan;
static xg_callback	wfff_mnrssi, wfff_mxrssi;
static xg_callback	wfff_first,  wfff_last;
static xg_callback	wfff_hdop,   wfff_lat,    wfff_lon;

static
xg_tag_mapping loc_map[] = {
  { wfff_s,	 	cb_start, 	"/DocumentElement/AP"			},
  { wfff_e,		cb_end, 	"/DocumentElement/AP"			},
  { wfff_wep, 	cb_cdata, 	"/DocumentElement/AP/WEP"		},
  { wfff_mac, 	cb_cdata, 	"/DocumentElement/AP/MAC"		},
  { wfff_ssid, 	cb_cdata, 	"/DocumentElement/AP/SSID"		},
  { wfff_type, 	cb_cdata, 	"/DocumentElement/AP/Type"		},
  { wfff_mnrssi, 	cb_cdata, 	"/DocumentElement/AP/MinRSSI"	},
  { wfff_mxrssi, 	cb_cdata, 	"/DocumentElement/AP/MaxRSSI"	},
  { wfff_chan, 	cb_cdata, 	"/DocumentElement/AP/Channel"	},
  { wfff_first, 	cb_cdata, 	"/DocumentElement/AP/FirstSeen" },
  { wfff_last, 	cb_cdata, 	"/DocumentElement/AP/LastSeen" 	},
  { wfff_hdop, 	cb_cdata, 	"/DocumentElement/AP/HDOP"		},
  { wfff_lat, 	cb_cdata, 	"/DocumentElement/AP/Lat"		},
  { wfff_lon, 	cb_cdata, 	"/DocumentElement/AP/Lon"		},
  { 0,(xg_cb_type)0,0 }
};

/* work variables for wfff_xxx */
static QString	ap_mac;
static QString	ap_ssid;
static QString	ap_type;
static QString	ap_wep;
static int		ap_chan		=0;
static time_t	ap_first	=0;
static QString 	ap_last;
static float	ap_mnrssi	=0.0;
static float	ap_mxrssi	=0.0;
static float	ap_hdop		=0.0;
static double	ap_lat		=0.0;
static double	ap_lon		=0.0;

/*	Start of AP block */
void wfff_s(xg_string, const QXmlStreamAttributes*)
{
  ap_mnrssi=0.0;
  ap_mxrssi=0.0;
  ap_chan=0;
  ap_hdop=0.0;
  ap_first=0;
  ap_last=QString();
  ap_lat=0.0;
  ap_lon=0.0;
}

void wfff_mac(const QString& args, const QXmlStreamAttributes*) { 
  ap_mac = args; 
}
void wfff_ssid(const QString& args, const QXmlStreamAttributes*) {
  ap_ssid = args;
}
void wfff_type(const QString& args, const QXmlStreamAttributes*) {
  ap_type = args;
}
void wfff_mnrssi(const QString& args, const QXmlStreamAttributes*) {
  ap_mnrssi = args.toDouble();
}
void wfff_mxrssi(const QString& args, const QXmlStreamAttributes*) {
  ap_mxrssi = args.toDouble();
}
void wfff_chan(const QString& args, const QXmlStreamAttributes*) {
  ap_chan = args.toInt();
}
void wfff_first(const QString& args, const QXmlStreamAttributes*) {
  ap_first = xml_parse_time(args).toTime_t();
}
void wfff_last(const QString& args, const QXmlStreamAttributes*) {
  ap_last = args;
}
void wfff_wep(const QString& args, const QXmlStreamAttributes*) {
  ap_wep = args;
}
void wfff_hdop(const QString& args, const QXmlStreamAttributes*) {
  ap_hdop = args.toDouble();
}
void wfff_lat(const QString& args, const QXmlStreamAttributes*) {
  ap_lat = args.toDouble();
}
void wfff_lon(const QString& args, const QXmlStreamAttributes*) {
  ap_lon = args.toDouble();
}

/*	End of AP Block, set waypoint and add */
static long tosscount=0;

void wfff_e(xg_string args, const QXmlStreamAttributes*)
{
  Waypoint*	wpt_tmp		=0;
  char		desc[255]	="\0";

  if ((ap_hdop>=1)&&(ap_hdop<50)) { // Discard invalid GPS fix
    wpt_tmp = new Waypoint;

    if (snmac) {
      wpt_tmp->shortname = ap_mac;
    } else {
      wpt_tmp->shortname = ap_ssid;
    }

    snprintf(desc, sizeof desc,
             "%s/%s/WEP %s/Ch %d/%2.0fdB/%2.0fdB/%s",
             snmac?CSTR(ap_ssid):CSTR(ap_mac), CSTR(ap_type), CSTR(ap_wep),
             ap_chan, ap_mnrssi, ap_mxrssi, CSTR(ap_last));
    wpt_tmp->description = desc;

    wpt_tmp->latitude = ap_lat;
    wpt_tmp->longitude = ap_lon;
    wpt_tmp->hdop = ap_hdop;
    wpt_tmp->altitude = unknown_alt;
    wpt_tmp->fix = fix_unknown;

    QString ap_wep_(ap_wep);
    QString ap_type_(ap_type);
    if (ap_wep_.startsWith("on", Qt::CaseInsensitive)) {
      if (ap_type_.startsWith("AP", Qt::CaseInsensitive)) {
        wpt_tmp->icon_descr = aicicon; /* Infra Closed */
      } else {
        wpt_tmp->icon_descr = ahcicon; /* AdHoc Closed */
      }
    } else {
      if (ap_type_.startsWith("AP", Qt::CaseInsensitive)) {
        wpt_tmp->icon_descr = aioicon; /* Infra Open */
      } else {
        wpt_tmp->icon_descr = ahoicon;	/* AdHoc Open */
      }
    }

    wpt_tmp->SetCreationTime(ap_first);

    waypt_add(wpt_tmp);

  } else {
    tosscount++;
  }
}

void
wfff_xml_rd_init(const char* fname)
{
  tosscount = 0;

  xml_init(fname, loc_map, NULL);
}

void
wfff_xml_read(void)
{
  xml_read();
}

void
wfff_xml_rd_deinit(void)
{
  xml_deinit();

  if (tosscount) {
    warning("Warning: %s reading file. Threw away %ld invalid entries.\n",
            MYNAME, tosscount);
  }

}

ff_vecs_t wfff_xml_vecs = {
  ff_type_file,
  {ff_cap_read, ff_cap_none, ff_cap_none},
  wfff_xml_rd_init,
  0,
  wfff_xml_rd_deinit,
  0,
  wfff_xml_read,
  0,
  0,
  wfff_xml_args
};
