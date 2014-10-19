/*

    Support for G7ToWin data files (.g7t),
    Copyright (C) 2007 Olaf Klein, o.b.klein@gpsbabel.org

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
    History:
    		04/07/2007: start programming
    		04/15/2007: added to gpsbabel
*/

#include "defs.h"
#include "csv_util.h"
#include "garmin_fs.h"
#include "garmin_tables.h"
#include "jeeps/gpsmath.h"
#include "strptime.h"

#include <time.h>

#if CSVFMTS_ENABLED

#define MYNAME "g7towin"

#define G7T_HEADER	"Version 2:G7T"

static gbfile* fin;
static grid_type grid;
static int datum;
static gpsdata_type mode;
static double altf;
static int gardown;
static int event_ct;

static
arglist_t g7towin_args[] = {
  ARG_TERMINATOR
};

#define WAYPT__OFS	0x00000
#define TRKPT__OFS	0x01000

#define WPT_c0_OFS	0x0c000
#define WPT_c1_OFS	0x0c100
#define WPT_c2_OFS	0x0c200
#define WPT_c3_OFS	0x0c300
#define WPT_c4_OFS	0x0c400
#define WPT_c5_OFS	0x0c500
#define WPT_c6_OFS	0x0c600
#define WPT_c7_OFS	0x0c700
#define WPT_c8_OFS	0x0c800
#define WPT_cA_OFS	0x0cA00
#define WPT_cB_OFS	0x0cB00
#define WPT_cC_OFS	0x0cC00
#define WPT_cD_OFS	0x0cD00

static void
parse_line(char* buff, int index, const char* delimiter, Waypoint* wpt)
{
  char* cin;
  garmin_fs_p gmsd = GMSD_FIND(wpt);

  while ((cin = csv_lineparse(buff, delimiter, "", index++))) {

    buff = NULL;
    cin = lrtrim(cin);

    if ((*cin == '\0') ||
        (strcmp(cin, "INF") == 0) ||
        (strcmp(cin, "1e25") == 0) ||
        (strcmp(cin, "1.0e25") == 0)) {
      continue;
    }

    switch (index) {

      int categories;
      struct tm tm;
      char* cerr;

    case TRKPT__OFS + 1:
      cin += parse_coordinates(cin, datum, grid,
                               &wpt->latitude, &wpt->longitude, MYNAME);
      while (isspace(*cin)) {
        cin++;
      }

      memset(&tm, 0, sizeof(tm));
      cerr = strptime(cin, "%a %b %d %H:%M:%S %Y", &tm);
      if (cerr == NULL) {
        fatal(MYNAME ": Unable to convert date (%s)!\n", cin);
      }
      wpt->SetCreationTime(mkgmtime(&tm));
      break;

    case WAYPT__OFS + 1:
      wpt->description = (cin);
      break;

    case WAYPT__OFS + 2:
      wpt->icon_descr = gt_find_desc_from_icon_number(
                          atoi(cin), PCX);
      break;

    case WAYPT__OFS + 4:
      if (strcmp(cin, "S+C") == 0) {
        GMSD_SET(display, gt_display_mode_symbol_and_comment);
      } else if (strcmp(cin, "S") == 0) {
        GMSD_SET(display, gt_display_mode_symbol);
      } else if (strcmp(cin, "S+N") == 0) {
        GMSD_SET(display, gt_display_mode_symbol_and_name);
      }
      break;

    case WPT_cA_OFS + 1:
    case WPT_c1_OFS + 1:
      wpt->shortname = cin;
      break;

    case WPT_cA_OFS + 4:
    case WPT_c4_OFS + 2:
      GMSD_SETSTR(city, cin);
      break;

    case WPT_cA_OFS + 5:
    case WPT_c4_OFS + 3:
      GMSD_SETSTR(state, cin);
      break;

    case WPT_cA_OFS + 6:
    case WPT_c4_OFS + 4:
      GMSD_SETSTR(cc, cin);
      break;

    case WPT_cB_OFS + 1:
    case WPT_c6_OFS + 2:
      GMSD_SETSTR(facility, cin);
      break;

    case WPT_cB_OFS + 2:
    case WPT_c6_OFS + 3:
      GMSD_SETSTR(addr, cin);
      break;

    case WPT_cB_OFS + 3: /*cross road */
    case WPT_c6_OFS + 4:
      GMSD_SETSTR(cross_road, cin);
      break;

    case TRKPT__OFS + 2: /* altitude */
    case WPT_cC_OFS + 1:
    case WPT_c5_OFS + 1:
    case WPT_c8_OFS + 1:
      wpt->altitude = altf * atof(cin);
      break;

    case TRKPT__OFS + 3: /* depth */
    case WPT_cC_OFS + 2:
    case WPT_c5_OFS + 2:
    case WPT_c8_OFS + 2:
      WAYPT_SET(wpt, depth, altf * atof(cin));
      break;

    case TRKPT__OFS + 10: /* temperature */
      if (*cin == '|') {
        cin++;  /* in track points */
      }
      if (strcmp(cin, "1e25") == 0) {
        break;
      }
      if (strcmp(cin, "1.0e25") == 0) {
        break;
      }
      /* !!! NO BREAK !!! */
    case WPT_cD_OFS + 1:
    case WPT_cB_OFS + 6:
      WAYPT_SET(wpt, temperature, atof(cin));
      break;

    case WAYPT__OFS + 6: /* proximity */
    case WPT_cD_OFS + 2:
      WAYPT_SET(wpt, proximity, atof(cin));
      break;

    case WPT_cB_OFS + 5:
    case WPT_cD_OFS + 3:
      categories = atoi(cin);
      if (categories != 0) {
        GMSD_SET(category, atoi(cin));
      }
      break;

#if 0

      /* currently unused */

    case TRKPT__OFS + 5: /* distance from previous point */
    case TRKPT__OFS + 6: /* distance from segment start */
    case TRKPT__OFS + 7: /* distance from start */
    case TRKPT__OFS + 8: /* velocity from previous point */
    case TRKPT__OFS + 9: /* time (in seconds) from previous point */
      break;

    case WAYPT__OFS + 3: /* ignore color */
      break;

    case WAYPT__OFS + 5: /* always '0' */
      break;

    case TRKPT__OFS + 4:
      if (case_ignore_strcmp(cin, "FT") == 0) ;
      else if (case_ignore_strcmp(cin, "M") == 0) ;
      else if (case_ignore_strcmp(cin, "SM") == 0) ;
      else if (case_ignore_strcmp(cin, "NM") == 0) ;
      else if (case_ignore_strcmp(cin, "KM") == 0) ;
      break;

    case WPT_cB_OFS + 4: /* unknown (datatype) */
      break;

    case WPT_cC_OFS + 3: /* waypt_class (always FF) */
      break;

    case WPT_cC_OFS + 4: /* class & subclass */
    case WPT_cC_OFS + 5:
    case WPT_cC_OFS + 6:
    case WPT_cC_OFS + 7:
    case WPT_cC_OFS + 8:
    case WPT_cC_OFS + 9:
    case WPT_cC_OFS + 10:
    case WPT_cC_OFS + 11:
    case WPT_cC_OFS + 12:
    case WPT_cC_OFS + 13:
    case WPT_cC_OFS + 14:
    case WPT_cC_OFS + 15:
    case WPT_cC_OFS + 16:
    case WPT_cC_OFS + 17:
    case WPT_cC_OFS + 18:
    case WPT_cC_OFS + 19:
    case WPT_cC_OFS + 20:
    case WPT_cC_OFS + 21:
      break;

    case WPT_cC_OFS + 22:
      /* distance */
      break;
#endif
    }
  }
}

static Waypoint*
parse_waypt(char* buff)
{
  char* cin, *cerr;
  int i;
  struct tm tm;
  Waypoint* wpt;
  garmin_fs_p gmsd;

  wpt = new Waypoint;
  gmsd = garmin_fs_alloc(-1);
  fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

  if (gardown) {
    cin = buff + 6;
  } else {
    /* We've seen waypoints with length of 14 and 15 !!! */
    cin = buff + 15;
    while ((cin > buff) && (! isspace(*cin))) {
      cin--;
    }
  }

  while (isspace(*cin)) {
    cin--;
  }
  if (cin >= buff) {
    char*s = xstrndup(buff, cin - buff + 1);
    wpt->shortname = s;
    xfree(s);
  }

  if (gardown) {
    buff += 6;
  } else {
    buff += 15;
  }
  while (isspace(*buff)) {
    buff++;
  }

  buff += parse_coordinates(buff, datum, grid,
                            &wpt->latitude, &wpt->longitude, MYNAME);
  while (isspace(*buff)) {
    buff++;
  }

  memset(&tm, 0, sizeof(tm));
  cerr = strptime(buff, "%a %b %d %H:%M:%S %Y", &tm);
  if (cerr == NULL) {
    fatal(MYNAME ": Unable to convert date (%s)!\n", buff);
  }
  wpt->SetCreationTime(mkgmtime(&tm));

  /* go over time stamp */
  i = 5;
  while (buff && i) {
    i--;
    buff = strchr(buff, ' ');
    if (buff) {
      buff++;
    }
  }
  if (gardown && (buff == NULL)) {
    return wpt;
  }
  is_fatal((buff == NULL), MYNAME ": Incomplete waypoint line!");

  while (isspace(*buff)) {
    buff++;
  }

  parse_line(buff, WAYPT__OFS, "^", wpt);

  return wpt;
}

static Waypoint*
parse_trkpt(char* buff)
{
  garmin_fs_p gmsd;
  Waypoint* wpt;

  wpt = new Waypoint;
  gmsd = garmin_fs_alloc(-1);
  fs_chain_add(&wpt->fs, (format_specific_data*) gmsd);

  parse_line(buff, TRKPT__OFS, ";", wpt);

  return wpt;
}

/*
 * parse_categories is currently only a dummy procedure.
 * w'll need a central storage with binding to the module
 * which has established a list of category names.
 */

static void
parse_categories(char* buff)
{
  char* cin;
  int cat = 0;

  while ((cin = csv_lineparse(buff, ",", "", cat++))) {
    uint16_t cx;

    buff = NULL;

    cin = lrtrim(cin);
    if (*cin == 0) {
      continue;
    }

    garmin_fs_convert_category(cin, &cx);
  }
}


/* main functions */

static void
rd_init(const char* fname)
{
  fin = gbfopen(fname, "rb", MYNAME);

  gardown = 1;
  mode = wptdata;
  grid = grid_lat_lon_dmm;
  datum = DATUM_WGS84;
  altf = 1;
  event_ct = 0;
}

static void
rd_deinit(void)
{
  gbfclose(fin);
}

static void
data_read(void)
{
  char* buff;
  int line = 0;
  Waypoint* wpt = NULL;
  route_head* head = NULL;

  while ((buff = gbfgetstr(fin))) {
    char* cin = buff;
    char* cdata;

    if ((line++ == 0) && fin->unicode) {
      cet_convert_init(CET_CHARSET_UTF8, 1);
    }

    cin = lrtrim(buff);
    if (!*cin) {
      continue;
    }

    cdata = cin+1;
    while (! isspace(*cdata)) {
      cdata++;
    }
    while (isspace(*cdata)) {
      cdata++;
    }
    if (! *cdata) {
      continue;
    }

    switch (*cin) {

    case '#': /* comment */
      break;

    case 'A':
      if (case_ignore_strncmp(cdata, "Meter", 5) == 0) {
        altf = 1.0;
      } else if (case_ignore_strncmp(cdata, "Feet", 4) == 0) {
        altf = FEET_TO_METERS(1.0);
      }
      break;

    case 'C': /* categories */
      parse_categories(cdata);
      break;

    case 'D':
      datum = gt_lookup_datum_index(cdata, MYNAME);
      break;

    case 'I': /* event point */
      wpt = new Waypoint;
      cdata += parse_coordinates(cdata, datum, grid,
                                 &wpt->latitude, &wpt->longitude, MYNAME);
      wpt->shortname = QString("Event%1").arg(++event_ct);
      while (isspace(*cdata)) {
        cdata++;
      }
      if (*cdata == ';') {
        cdata++;
        wpt->icon_descr = gt_find_desc_from_icon_number(
                            atoi(cdata), PCX);
      }
      waypt_add(wpt);
      break;

    case 'M':
      grid = gt_lookup_grid_type(cdata, MYNAME);
      break;

    case 'P': /* proximity waypoint */
    case 'W': /* normal waypoint */
      wpt = parse_waypt(cin + 3);
      if (wpt) {
        if (mode == rtedata) {
          route_add_wpt(head, wpt);
        } else {
          waypt_add(wpt);
        }
      }
      break;

    case 'c': /* additional lines */
      switch (*(cin+1)) {
        int index;

      case 'A':
      case 'B':
      case 'C':
      case 'D':

        index = WPT_cA_OFS + ((*(cin+1) - 'A') * 256);
        parse_line(cdata, index, "|", wpt);
        break;

      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':

        index = WPT_c0_OFS + ((*(cin+1) - '0') * 256);
        parse_line(cdata, index, ";", wpt);
        break;

      case 'L':
        waypt_add_url(wpt, xstrdup(cdata), NULL);
        break;

      default:
        break;
      }
      break;

    case 'N':	/* track log header */
      mode = trkdata;
      head = route_head_alloc();
      cdata = strchr(cdata, '-');
      if (cdata) {
        while (isspace(*cdata)) {
          cdata++;
        }
        if (*cdata) {
          char* s;
          s = strrchr(cdata, ',');
          if (s) {
            *s = '\0';
            s = strrchr(cdata, ',');
            if (s) {
              *s = '\0';
              head->rte_name = cdata;
            }
          }
        }
      }
      track_add_head(head);
      break;

    case 'R':	/* route header */
      mode = rtedata;
      head = route_head_alloc();
      cdata += 3; /*skip route number */
      if (*cdata) {
        head->rte_name = cdata;
      }
      route_add_head(head);
      break;

    case 'T':
      wpt = parse_trkpt(cdata);
      if (wpt) {
        track_add_wpt(head, wpt);
      }
      break;

    case 'V':
      if (strcmp(cin, G7T_HEADER) != 0) {
        fatal(MYNAME ": Invalid version or invalid file!\n");
      }
      gardown = 0;
      break;

    default:
      break;
    }
  }
}

/* --------------------------------------------------------------------------- */

ff_vecs_t g7towin_vecs = {
  ff_type_file,
  { ff_cap_read, ff_cap_read, ff_cap_read },
  rd_init,
  NULL,
  rd_deinit,
  NULL,
  data_read,
  NULL,
  NULL,
  g7towin_args,
  CET_CHARSET_MS_ANSI, 0
};

#endif /* CSVFMTS_ENABLED */
