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

/* argument storage */
static char *	aicicon	=0;
static char *	aioicon =0;
static char *	ahcicon =0;
static char *	ahoicon =0;
static char *	snmac	=0;

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
#define MY_CBUF 4096

#if ! HAVE_LIBEXPAT
void
wfff_xml_rd_init(const char *fname)
{
  fatal(MYNAME ": This build excluded WFFF_XML support because expat was not installed.\n");
}
void
wfff_xml_read(void)
{
}
void
wfff_xml_rd_deinit(void)
{
}

#else

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
static char* 	ap_mac		=0;
static char* 	ap_ssid		=0;
static char* 	ap_type		=0;
static char* 	ap_wep		=0;
static int		ap_chan		=0;
static time_t	ap_first	=0;
static char* 	ap_last		=0;
static float	ap_mnrssi	=0.0;
static float	ap_mxrssi	=0.0;
static float	ap_hdop		=0.0;
static double	ap_lat		=0.0;
static double	ap_lon		=0.0;

/*	Start of AP block */
void wfff_s(const char *args, const char **unused)
{
  xfreez(ap_mac);
  xfreez(ap_ssid);
  xfreez(ap_type);
  xfreez(ap_wep);
  xfreez(ap_last);
  ap_mnrssi=0.0;
  ap_mxrssi=0.0;
  ap_chan=0;
  ap_hdop=0.0;
  ap_first=0;
  ap_last=0;
  ap_lat=0.0;
  ap_lon=0.0;
}

void wfff_mac(const char *args, const char **unused)
{
  if (args) {
    ap_mac = xstrdup(args);
  }
}

void wfff_ssid(const char *args, const char **unused)
{
  if (args) {
    ap_ssid = xstrdup(args);
  }
}

void wfff_type(const char *args, const char **unused)
{
  if (args) {
    ap_type = xstrdup(args);
  }
}

void wfff_mnrssi(const char *args, const char **unused)
{
  if (args) {
    ap_mnrssi = atof(args);
  }
}

void wfff_mxrssi(const char *args, const char **unused)
{
  if (args) {
    ap_mxrssi = atof(args);
  }
}

void wfff_chan(const char *args, const char **unused)
{
  if (args) {
    ap_chan = atoi(args);
  }
}

void wfff_first(const char *args, const char **unused)
{
  if (args) {
    ap_first = xml_parse_time(args, NULL);
  }
}

void wfff_last(const char *args, const char **unused)
{
  if (args) {
    ap_last = xstrdup(args);
  }
}

void wfff_wep(const char *args, const char **unused)
{
  if (args) {
    ap_wep = xstrdup(args);
  }
}

void wfff_hdop(const char *args, const char **unused)
{
  if (args) {
    ap_hdop = atof(args);
  }
}

void wfff_lat(const char *args, const char **unused)
{
  if (args) {
    ap_lat = atof(args);
  }
}

void wfff_lon(const char *args, const char **unused)
{
  if (args) {
    ap_lon = atof(args);
  }
}

/*	End of AP Block, set waypoint and add */
static long tosscount=0;

void wfff_e(const char *args, const char **unused)
{
  waypoint*	wpt_tmp		=0;
  char		desc[255]	="\0";

  if ((ap_hdop>=1)&&(ap_hdop<50)) { // Discard invalid GPS fix
    wpt_tmp = waypt_new();

    if (snmac) {
      wpt_tmp->shortname = xstrdup(ap_mac);
    } else {
      wpt_tmp->shortname = xstrdup(ap_ssid);
    }

    snprintf(desc, sizeof desc,
             "%s/%s/WEP %s/Ch %d/%2.0fdB/%2.0fdB/%s",
             (snmac?ap_ssid:ap_mac), ap_type, ap_wep,
             ap_chan, ap_mnrssi, ap_mxrssi, ap_last);
    wpt_tmp->description = xstrdup(desc);

    wpt_tmp->latitude = ap_lat;
    wpt_tmp->longitude = ap_lon;
    wpt_tmp->hdop = ap_hdop;
    wpt_tmp->altitude = unknown_alt;
    wpt_tmp->fix = fix_unknown;

    wpt_tmp->wpt_flags.icon_descr_is_dynamic = 1;
    if (case_ignore_strncmp(ap_wep,"On",2)==0) {
      if (case_ignore_strncmp(ap_type,"AP",2)==0) {
        wpt_tmp->icon_descr = xstrdup(aicicon); /* Infra Closed */
      } else {
        wpt_tmp->icon_descr = xstrdup(ahcicon); /* AdHoc Closed */
      }
    } else {
      if (case_ignore_strncmp(ap_type,"AP",2)==0) {
        wpt_tmp->icon_descr = xstrdup(aioicon); /* Infra Open */
      } else {
        wpt_tmp->icon_descr = xstrdup(ahoicon);	/* AdHoc Open */
      }
    }

    wpt_tmp->creation_time = ap_first;

    waypt_add(wpt_tmp);

  } else {
    tosscount++;
  }

  /* cleanup */
  xfreez(ap_mac);
  xfreez(ap_ssid);
  xfreez(ap_type);
  xfreez(ap_wep);
  xfreez(ap_last);

}


void
wfff_xml_rd_init(const char *fname)
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

  /* cleanup */
  xfreez(ap_mac);
  xfreez(ap_ssid);
  xfreez(ap_type);
  xfreez(ap_wep);
  xfreez(ap_last);

  xml_deinit();

  if (tosscount) {
    warning("Warning: %s reading file. Threw away %ld invalid entries.\n",
            MYNAME, tosscount);
  }

}

#endif

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
