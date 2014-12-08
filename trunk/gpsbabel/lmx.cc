/*
    Access Nokia Landmark Exchange files.

    Copyright (C) 2007  Robert Lipe, robertlipe+source@gpsbabel.org

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


/*
 * Nokia's Landmark Exchange (LMX) format is a straight-forward XML
 * format.  Though they do support a compact binary representation,
 * we don't implement that at this time in GPSBabel.
 */

#include "defs.h"
#include "xmlgeneric.h"
#include <stdio.h>
#include <QtCore/QXmlStreamAttributes>

static gbfile* ofd;
static Waypoint* wpt_tmp;
QString urllink;
QString urllinkt;
static char* binary = NULL;

#define MYNAME "lmx"

static
arglist_t lmx_args[] = {
  {
    "binary", &binary,
    "Compact binary representation",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

/*
 * Writer
 */


static void
lmx_wr_init(const char* fname)
{
  ofd = gbfopen(fname, "w", MYNAME);
}

static void
lmx_wr_deinit(void)
{
  gbfclose(ofd);
}

static const char*
lmx_stag(int tag)
{
  switch (tag) {
  case 0xC5:
    return "lmx";
  case 0x46:
    return "landmarkCollection";
  case 0x47:
    return "landmark";
  case 0x48:
    return "name";
  case 0x49:
    return "description";
  case 0x4A:
    return "coordinates";
  case 0x4B:
    return "latitude";
  case 0x4C:
    return "longitude";
  case 0x4D:
    return "altitude";
  case 0x4E:
    return "horizontalAccuracy";
  case 0x4F:
    return "verticalAccuracy";
  case 0x50:
    return "timeStamp";
  case 0x51:
    return "coverageRadius";
  case 0x52:
    return "category";
  case 0x53:
    return "id";
  case 0x54:
    return "addressInfo";
  case 0x55:
    return "country";
  case 0x56:
    return "countryCode";
  case 0x57:
    return "state";
  case 0x58:
    return "county";
  case 0x59:
    return "city";
  case 0x5A:
    return "district";
  case 0x5B:
    return "postalCode";
  case 0x5C:
    return "crossing1";
  case 0x5D:
    return "crossing2";
  case 0x5E:
    return "street";
  case 0x5F:
    return "buildingName";
  case 0x60:
    return "buildingFloor";
  case 0x61:
    return "buildingZone";
  case 0x62:
    return "buildingRoom";
  case 0x63:
    return "extension";
  case 0x64:
    return "phoneNumber";
  case 0x65:
    return "mediaLink";
  case 0x66:
    return "mime";
  case 0x67:
    return "url";
  default:
    return 0;
  }
}

static void
lmx_indent(int count)
{
  int i;
  for (i=0; i<count; i++) {
    gbfputc('\t', ofd);
  }
}

static void
lmx_start_tag(int tag, int indent)
{
  if (binary) {
    gbfputc(tag, ofd);
  } else {
    lmx_indent(indent);
    gbfprintf(ofd, "<lm:%s>", lmx_stag(tag));
  }
}

static void
lmx_end_tag(int tag, int indent)
{
  if (binary) {
    gbfputc(0x01, ofd);
  } else {
    lmx_indent(indent);
    gbfprintf(ofd, "</lm:%s>\n", lmx_stag(tag));
  }
}

static void
lmx_write_xml(int tag, const QString& data, int indent)
{
  lmx_start_tag(tag, indent);

  if (binary) {
    gbfputc(0x03, ofd); // inline string follows
    gbfputcstr(data, ofd);
  } else {
    char* tmp_ent = xml_entitize(CSTR(data));
    gbfputs(tmp_ent, ofd);
    xfree(tmp_ent);
  }

  lmx_end_tag(tag, 0);
}

static void
lmx_print(const Waypoint* wpt)
{
  QString oname;
  QString odesc;

  /*
   * Desparation time, try very hard to get a good shortname
   */
  odesc = wpt->notes;
  if (odesc.isEmpty()) {
    odesc = wpt->description;
  }
  if (odesc.isEmpty()) {
    odesc = wpt->shortname;
  }

  oname = global_opts.synthesize_shortnames ? odesc : wpt->shortname;

  lmx_start_tag(0x47, 2); // landmark
  if (!binary) {
    gbfputc('\n', ofd);
  }
  if (!oname.isEmpty()) {
    lmx_write_xml(0x48, oname, 3); // name
  }
  if (!wpt->description.isEmpty()) {
    lmx_write_xml(0x49, wpt->description, 3); // description
  }
  lmx_start_tag(0x4A, 3); // coordinates
  if (!binary) {
    gbfputc('\n', ofd);
  }

  char tbuf[100];
  sprintf(tbuf, "%f", wpt->latitude);
  lmx_write_xml(0x4B, tbuf, 4); // latitude

  sprintf(tbuf, "%f", wpt->longitude);
  lmx_write_xml(0x4C, tbuf, 4); // longitude

  if (wpt->altitude && (wpt->altitude != unknown_alt)) {
    sprintf(tbuf, "%f", wpt->altitude);
    lmx_write_xml(0x4D, tbuf, 4); // altitude
  }
  lmx_end_tag(0x4A, 3); // coordinates

  if (wpt->HasUrlLink()) {
    lmx_start_tag(0x65, 3); // mediaLink
    if (!binary) {
      gbfputc('\n', ofd);
    }
    UrlLink link = wpt->GetUrlLink();
    if (!link.url_link_text_.isEmpty()) {
      lmx_write_xml(0x48, link.url_link_text_, 4);  // name
    }
    lmx_write_xml(0x67, link.url_, 4); // url
    lmx_end_tag(0x65, 3); // mediaLink
  }

  lmx_end_tag(0x47, 2); // landmark
}


static void
lmx_write(void)
{
  if (binary) {
    gbfputc(0x03, ofd); // WBXML version 1.3
    gbfputuint16(0x04A4, ofd); // "-//NOKIA//DTD LANDMARKS 1.0//EN"
    gbfputc(106, ofd); // Charset=UTF-8
    gbfputc(0x00, ofd); // empty string table
    gbfputc(0xC5, ofd); // lmx
    gbfputc(0x05, ofd); // xmlns=http://www.nokia.com/schemas/location/landmarks/
    gbfputc(0x85, ofd); // 1/0/
    gbfputc(0x06, ofd); // xmlns:xsi=
    gbfputc(0x86, ofd); // http://www.w3.org/2001/XMLSchema-instance
    gbfputc(0x07, ofd); // xsi:schemaLocation=http://www.nokia.com/schemas/location/landmarks/
    gbfputc(0x85, ofd); // 1/0/
    gbfputc(0x87, ofd); // whitespace
    gbfputc(0x88, ofd); // lmx.xsd
    gbfputc(0x01, ofd); // END lmx attributes
  } else {
    gbfprintf(ofd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    gbfprintf(ofd, "<lm:lmx xmlns:lm=\"http://www.nokia.com/schemas/location/landmarks/1/0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.nokia.com/schemas/location/landmarks/1/0/ lmx.xsd\">\n");
  }

  lmx_start_tag(0x46, 1); // landmarkCollection
  if (!binary) {
    gbfputc('\n', ofd);
  }
  waypt_disp_all(lmx_print);
  lmx_end_tag(0x46, 1); // landmarkCollection
  lmx_end_tag(0xC5, 0); // lmx
}

/*
 * Reader
 */

static xg_callback	lmx_lm_start, lmx_lm_end;
static xg_callback	lmx_lm_name,lmx_lm_desc;
static xg_callback	lmx_lm_lat, lmx_lm_lon, lmx_lm_alt;
static xg_callback	lmx_lm_mlink_s, lmx_lm_mlink_e;
static xg_callback	lmx_lm_link, lmx_lm_linkt;

static xg_tag_mapping gl_map[] = {
#define LM "/lm:lmx/lm:landmarkCollection/lm:landmark"
  { lmx_lm_start, 	cb_start, 	LM },
  { lmx_lm_end,   	cb_end, 	LM },
  { lmx_lm_name,	 	cb_cdata, 	LM "/lm:name" },
  { lmx_lm_desc,  	cb_cdata, 	LM "/lm:description" },
  { lmx_lm_lat,   	cb_cdata, 	LM "/lm:coordinates/lm:latitude" },
  { lmx_lm_lon, 		cb_cdata, 	LM "/lm:coordinates/lm:longitude" },
  { lmx_lm_alt,		cb_cdata, 	LM "/lm:coordinates/lm:altitude" },
  { lmx_lm_mlink_s,  	cb_start, 	LM "/lm:mediaLink" },
  { lmx_lm_link,  	cb_cdata, 	LM "/lm:mediaLink/lm:url" },
  { lmx_lm_linkt, 	cb_cdata, 	LM "/lm:mediaLink/lm:name" },
  { lmx_lm_mlink_e,	cb_end, 	LM "/lm:mediaLink" },
  { NULL,	(xg_cb_type)0,         NULL}
};

static void
lmx_rd_init(const char* fname)
{
  xml_init(fname, gl_map, NULL);
}

static void
lmx_read(void)
{
  xml_read();
}

static void
lmx_rd_deinit(void)
{
  xml_deinit();
}



static void
lmx_lm_start(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp = new Waypoint;
}

static void
lmx_lm_end(xg_string args, const QXmlStreamAttributes*)
{
  waypt_add(wpt_tmp);
}

static void
lmx_lm_lat(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->latitude = args.toDouble();
}

static void
lmx_lm_lon(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->longitude = args.toDouble();
}

static void
lmx_lm_alt(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->altitude = args.toDouble();
}

static void
lmx_lm_name(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->shortname = args;
}

static void
lmx_lm_desc(xg_string args, const QXmlStreamAttributes*)
{
  wpt_tmp->description = args;
}

static void
lmx_lm_mlink_s(xg_string args, const QXmlStreamAttributes*)
{
  urllink = urllinkt = QString();
}

static void
lmx_lm_link(xg_string args, const QXmlStreamAttributes*)
{
  urllink = args;
}

static void
lmx_lm_linkt(xg_string args, const QXmlStreamAttributes*)
{
  urllinkt = args;
}

static void
lmx_lm_mlink_e(xg_string args, const QXmlStreamAttributes*)
{
  waypt_add_url(wpt_tmp, urllink, urllinkt);
}


ff_vecs_t lmx_vecs = {
  ff_type_file,
  {
    (ff_cap)(ff_cap_read | ff_cap_write),	/* waypoints */
    ff_cap_none,			/* tracks */
    ff_cap_none			/* routes */
  },
  lmx_rd_init,
  lmx_wr_init,
  lmx_rd_deinit,
  lmx_wr_deinit,
  lmx_read,
  lmx_write,
  NULL,
  lmx_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
