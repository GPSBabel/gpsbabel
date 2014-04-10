/*
    Copyright (C) 2003-2013 Robert Lipe, robertlipe+source@gpsbabel.org

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
#include "cet_util.h"
#include <QtCore/QXmlStreamReader>
#include "src/core/file.h"

static char* noretired = NULL;
static const char* read_fname = NULL;

static
arglist_t nav_args[] = {
  {
    "noretired", &noretired, "Suppress retired geocaches",
    NULL, ARGTYPE_BOOL, ARG_NOMINMAX
  },
  ARG_TERMINATOR
};

#define MYNAME "navicache"
#define NC_URL	"http://www.navicache.com/cgi-bin/db/displaycache2.pl?CacheID="

static struct
    nc_type_mapping {
  geocache_type type;
  const char* name;
} nc_type_map[] = {
  { gt_unknown, "unknown" },
  { gt_traditional, "normal" },
  { gt_multi, "Multi-part" },
  { gt_virtual, "Virtual" },
  { gt_event, "event" }
};

static struct
    nc_container_mapping {
  geocache_container type;
  const char* name;
} nc_container_map[] = {
  { gc_other, "Unknown" },
  { gc_micro, "Micro" },
  { gc_regular, "Normal" },
  { gc_large, "Large" },
  { gc_virtual, "Virtual" }
};

static
geocache_type
nc_mktype(const QString& t)
{
  int i;
  int sz = sizeof(nc_type_map) / sizeof(nc_type_map[0]);

  for (i = 0; i < sz; i++) {
    if (0 == t.compare(nc_type_map[i].name, Qt::CaseInsensitive)) {
      return nc_type_map[i].type;
    }
  }
  return gt_unknown;
}

static
geocache_container
nc_mkcont(const QString& t)
{
  int i;
  int sz = sizeof(nc_container_map) / sizeof(nc_container_map[0]);

  for (i = 0; i < sz; i++) {
    if (0 == t.compare(nc_container_map[i].name, Qt::CaseInsensitive)) {
      return nc_container_map[i].type;
    }
  }
  return gc_unknown;
}

static void
nav_rd_init(const char* fname)
{
  read_fname = fname;
}

static void
NaviReadCache(const QXmlStreamReader& reader)
{
  const QXmlStreamAttributes a = reader.attributes();
  Waypoint* wpt_tmp = new Waypoint;
  geocache_data* gc_data;
  gc_data = wpt_tmp->AllocGCData();
  if (a.hasAttribute("cache_id")) {
    int n = a.value("cache_id").toString().toInt();
    QString fn = QString("N%1").arg(n, 5, 16, QChar('0'));
    wpt_tmp->shortname = fn;

    UrlLink l(QString(NC_URL) + QString::number(n));
    wpt_tmp->AddUrlLink(l);
  }
  if (a.hasAttribute("name")) {
    wpt_tmp->description = a.value("name").toString();
  }
  if (a.hasAttribute("user_name")) {
    gc_data->placer = a.value("user_name").toString();
  }

  if (a.hasAttribute("latitude")) {
    wpt_tmp->latitude = a.value("latitude").toString().toDouble();
  }
  if (a.hasAttribute("longitude")) {
    wpt_tmp->longitude = a.value("longitude").toString().toDouble();
  }

  if (a.hasAttribute("difficulty")) {
    gc_data->diff = a.value("difficulty").toString().toDouble() * 10;
  }
  if (a.hasAttribute("terrain")) {
    gc_data->terr = a.value("terrain").toString().toDouble() * 10;
  }

  if (a.hasAttribute("cache_type")) {
    QString t = a.value("cache_type").toString();
    gc_data->type = nc_mktype(t);
    if (t == "normal") {
      wpt_tmp->icon_descr = "Geocache-regular";
    } else if (t == "multi-part") {
      wpt_tmp->icon_descr = "Geocache-multi";
    } else if (t == "moving_travelling") {
      wpt_tmp->icon_descr = "Geocache-moving";
    } else {
      wpt_tmp->icon_descr = QString("Geocache-%-%1").arg(t);
    }
  }

  if (a.hasAttribute("hidden_date")) {
    QString h = a.value("hidden_date").toString();
    QDateTime hd = QDateTime::fromString(h, "yyyy-MM-dd");
    wpt_tmp->SetCreationTime(hd);
  }

  if (a.hasAttribute("retired")) {
    if (a.value("terrain").toString() == "yes" && noretired) {
      delete wpt_tmp;
      return;
    }
  }

  if (a.hasAttribute("cache_size")) {
    gc_data->container = nc_mkcont(a.value("cache_size").toString());
  }

  if (a.hasAttribute("description")) {
    gc_data->desc_long.is_html = true;
    gc_data->desc_long.utfstring = a.value("description").toString();
  }

  if (a.hasAttribute("comments")) {
    gc_data->desc_short.is_html = true;
    gc_data->desc_short.utfstring = a.value("comments").toString();
  }


  waypt_add(wpt_tmp);
}

static void
nav_read(void)
{
  QXmlStreamReader reader;
  gpsbabel::File file(read_fname);
  file.open(QIODevice::ReadOnly);
  reader.setDevice(&file);

  while (!reader.atEnd()) {
    if (reader.tokenType() == QXmlStreamReader::StartElement) {
      if (reader.name() == "CacheDetails") {
        NaviReadCache(reader);
      }
    }
    reader.readNext();
  }
  if (reader.hasError())  {
    fatal(MYNAME ":Read error: %s (%s, line %ld, col %ld)\n",
          CSTR(reader.errorString()),
          CSTR(file.fileName()),
          (long) reader.lineNumber(),
          (long) reader.columnNumber());
  }
}

static void
nav_rd_deinit(void)
{
}

static void
nav_wr_init(const char* fname)
{
  fatal(MYNAME ": Does not support writing Navicache files.\n");
}

static void
nav_wr_deinit(void)
{
}

static void
nav_write(void)
{
}

ff_vecs_t navicache_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_none, ff_cap_none },
  nav_rd_init,
  nav_wr_init,
  nav_rd_deinit,
  nav_wr_deinit,
  nav_read,
  nav_write,
  NULL,
  nav_args,
  CET_CHARSET_UTF8, 0	/* CET-REVIEW */
};
