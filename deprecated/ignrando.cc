/*

	Support for IGN Rando track files.

	Copyright (C) 2005,2006 Olaf Klein

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


#include <cstdio>                  // for sscanf
#include <cstdlib>                 // for atoi
#include <ctime>                   // for strftime, localtime, time_t, tm

#include <QByteArray>              // for QByteArray
#include <QIODevice>               // for QIODevice, QIODeviceBase::ReadOnly
#include <QList>                   // for QList
#include <QString>                 // for QString, operator==
#include <QXmlStreamAttributes>    // for QXmlStreamAttributes
#include <QtGlobal>                // for QT_VERSION, QT_VERSION_CHECK, qPrintable

#include "defs.h"
#include "gbfile.h"                // for gbfprintf, gbfclose, gbfopen, gbfile
#include "src/core/datetime.h"     // for DateTime
#include "src/core/file.h"         // for File
#include "xmlgeneric.h"            // for xg_callback, xg_string, cb_cdata, xml_deinit, xml_init, xml_readunicode, cb_start, cb_end, xg_cb_type, xg_tag_mapping


#define MYNAME "IGNRando"

#if (QT_VERSION >= QT_VERSION_CHECK(6, 0, 0))
static QString rd_fname;
#endif
static gbfile* fout;

static route_head* track;
static Waypoint* wpt;
static int track_index;		/* index of track we'll write */
static int track_num;		/* current index of track within track_disp_all */

static int xmlpoints;

/* options */
static char* index_opt = nullptr;

static QVector<arglist_t> ignr_args = {
  {"index", &index_opt, "Index of track to write (if more than one in source)", nullptr, ARGTYPE_INT, "1", nullptr , nullptr},
};


static xg_callback	ignr_start;
static xg_callback	ignr_nb_etapes, ignr_descr;
static xg_callback	ignr_etape_begin, ignr_etape_end;
static xg_callback	ignr_etape_pos, ignr_etape_alt;

static
xg_tag_mapping ignr_xml_map[] = {
  { ignr_start,		cb_start,	"/RANDONNEE" },
  { ignr_nb_etapes,   	cb_cdata,	"/RANDONNEE/INFORMATIONS/NB_ETAPES" },
  { ignr_descr,   	cb_cdata,	"/RANDONNEE/INFORMATIONS/DESCRIPTION" },
  { ignr_etape_begin, 	cb_start, 	"/RANDONNEE/ETAPE" },
  { ignr_etape_end, 	cb_end, 	"/RANDONNEE/ETAPE" },
  { ignr_etape_pos,	cb_cdata,	"/RANDONNEE/ETAPE/POSITION" },
  { ignr_etape_alt,	cb_cdata,	"/RANDONNEE/ETAPE/ALTITUDE" },
  { nullptr,	(xg_cb_type)0, 		nullptr }
};

static void
ignr_xml_error(int condition)
{
  if (condition != 0) {
    fatal(MYNAME ": Error in XML structure!\n");
  }
}

/* xmlgeneric callbacks */

static void
ignr_start(xg_string, const QXmlStreamAttributes*)
{
  ignr_xml_error((track != nullptr));

  track = new route_head;
  track_add_head(track);
}

static void
ignr_nb_etapes(xg_string args, const QXmlStreamAttributes*)
{
  xmlpoints = args.toInt();
}

static void
ignr_descr(xg_string args, const QXmlStreamAttributes*)
{
  ignr_xml_error((track == nullptr));
  track->rte_desc = args;
}

static void
ignr_etape_begin(xg_string, const QXmlStreamAttributes*)
{
  ignr_xml_error((wpt != nullptr));

  wpt = new Waypoint;
}

static void
ignr_etape_end(xg_string, const QXmlStreamAttributes*)
{
  ignr_xml_error((track == nullptr) || (wpt == nullptr));

  track_add_wpt(track, wpt);
  wpt = nullptr;
}

static void
ignr_etape_pos(xg_string args, const QXmlStreamAttributes*)
{
  ignr_xml_error((wpt == nullptr) || (args.isEmpty()));

  if (2 != sscanf(STRFROMUNICODE(args), "%lf,%lf", &wpt->latitude, &wpt->longitude)) {
    fatal(MYNAME ": Invalid coordinates \"%s\"!\n", qPrintable(args));
  }
}

static void
ignr_etape_alt(xg_string args, const QXmlStreamAttributes*)
{
  ignr_xml_error((wpt == nullptr));
  if (args == nullptr) {
    return;
  }

  if (1 != sscanf(STRFROMUNICODE(args), "%lf", &wpt->altitude)) {
    fatal(MYNAME ": Invalid altitude \"%s\"!\n", qPrintable(args));
  }
}

/* callbacks registered in ignr_vecs */

static void
ignr_rd_init(const QString& fname)
{
#if (QT_VERSION < QT_VERSION_CHECK(6, 0, 0))
  xml_init(fname, ignr_xml_map, nullptr);
#else
  rd_fname = fname;
  xml_init(nullptr, ignr_xml_map, nullptr);
#endif
  wpt = nullptr;
  track = nullptr;
}

static void
ignr_rd_deinit()
{
  xml_deinit();
#if (QT_VERSION >= QT_VERSION_CHECK(6, 0, 0))
  rd_fname.clear();
#endif
}

static void
ignr_read()
{
#if (QT_VERSION < QT_VERSION_CHECK(6, 0, 0))
  // QXmlStreamReader had access to the windows-1252 QTextCodec that we expect
  // to find in the XMLDecl.
  xml_read();
#else
  // QXmlStreamReader doesn't have access to a windows-1252 QStringDecoder,
  // and will throw an error if we pass a QIODevice when it sees windows-1252
  // in the XMLDecl.
  // Therfore we must decode the input manually and pass a QString.
  // With a QString QXmlStreamReader will ignore the XMLDecl.
  gpsbabel::File file(rd_fname);
  file.open(QIODevice::ReadOnly);
  xml_readunicode(STRTOUNICODE(file.readAll()));
  file.close();
#endif
}

/* write support */

/* callbacks registered in ignr_vecs */

static void
ignr_rw_init(const QString& fname)
{
  fout = gbfopen(fname, "w", MYNAME);
}

static void
ignr_rw_deinit()
{
  gbfclose(fout);
}

static void
ignr_write_track_hdr(const route_head* track_hdr)
{
  track_num++;

  if (track_num != track_index) {
    return;
  }

  gbfprintf(fout, "\t<INFORMATIONS>\n");
  gbfprintf(fout, "\t\t<NB_ETAPES>%d</NB_ETAPES>\n", track_hdr->rte_waypt_ct());
  if (!track_hdr->rte_desc.isEmpty()) {
    gbfprintf(fout, "\t\t<DESCRIPTION>%s</DESCRIPTION>\n", STRFROMUNICODE(track_hdr->rte_desc));
  }
  gbfprintf(fout, "\t</INFORMATIONS>\n");
}

static void
ignr_write_waypt(const Waypoint* waypoint)
{
  if (track_num != track_index) {
    return;
  }

  gbfprintf(fout, "\t<ETAPE>\n");
  gbfprintf(fout, "\t\t<POSITION>%3.6f,%3.6f</POSITION>\n", waypoint->latitude, waypoint->longitude);
  if (waypoint->altitude != unknown_alt) {
    gbfprintf(fout, "\t\t<ALTITUDE>%3.6f</ALTITUDE>\n", waypoint->altitude);
  }
  gbfprintf(fout, "\t</ETAPE>\n");
}

static void
ignr_write()
{
  time_t now;
  struct tm tm;
  char buff[32];

  if (index_opt != nullptr) {
    track_index = atoi(index_opt);
    if ((track_index < 1) || (track_index > (int) track_count()))
      fatal(MYNAME ": Invalid track index %d (we have currently %d track(s))!\n",
            track_index, track_count());
  } else {
    track_index = 1;
  }
  track_num = 0;

  now = current_time().toTime_t();
  tm = *localtime(&now);

  gbfprintf(fout, "<?xml version=\"1.0\" encoding=\"windows-1252\"?>\n");
  gbfprintf(fout, "<RANDONNEE>\n");
  gbfprintf(fout, "\t<ENTETE>\n");
  gbfprintf(fout, "\t\t<VERSION_XML>1.1</VERSION_XML>\n");
  gbfprintf(fout, "\t\t<VERSION_BASE>IHA03AA</VERSION_BASE>\n");

  strftime(buff, sizeof(buff), "%d/%m/%Y", &tm);
  gbfprintf(fout, "\t\t<DATE>%s</DATE>\n", buff);
  strftime(buff, sizeof(buff), "%H:%M:%S", &tm);
  gbfprintf(fout, "\t\t<HEURE>%s</HEURE>\n", buff);

  gbfprintf(fout, "\t</ENTETE>\n");
  track_disp_all(ignr_write_track_hdr, nullptr, ignr_write_waypt);
  gbfprintf(fout, "</RANDONNEE>\n");
}

ff_vecs_t ignr_vecs = {
  ff_type_file,
  { ff_cap_none, (ff_cap)(ff_cap_read | ff_cap_write), ff_cap_none },
  ignr_rd_init,
  ignr_rw_init,
  ignr_rd_deinit,
  ignr_rw_deinit,
  ignr_read,
  ignr_write,
  nullptr,
  &ignr_args,
  CET_CHARSET_MS_ANSI, 1
  , NULL_POS_OPS
};
