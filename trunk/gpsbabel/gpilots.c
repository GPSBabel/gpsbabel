/*
    Read and write GPilotS files.

    Copyright (C) 2003 Robert Lipe, robertlipe@usa.net

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
#if PDBFMTS_ENABLED
#include "pdbfile.h"
#include "garmin_tables.h"

#define MYNAME "GPilotS"
#define MYWPT  0x57707473  	/* Wpts */
#define MYTRK  0x54726b73  	/* Trks */
#define MYRTE  0x57707473  	/* Wpts */
#define MYCREATOR 0x4750696c 	/* GPil */


/*
 * Structures grafted from http://www.cru.fr/perso/cc/GPilotS/
 */


typedef struct {
  long lat;			/* latitude in semicircles */
  long lon;			/* longitude in semicircles */
}
Semicircle_Type;

typedef struct {
  char ident[6];			/* identifier */
  unsigned char lat[4];		/* position */
  unsigned char lon[4];		/* position */
  unsigned char unused[4];	/* should be set to zero */
  char cmnt[40];			/* comment */
  unsigned char smbl;		/* symbol id */
  unsigned char dspl;		/* display option */
} D103_Wpt_Type;

typedef union {
  float f;
  unsigned int i;
} fi_t;

typedef struct {		       /*                             size */
  unsigned char wpt_class;            /* class (see below)            1 */
  unsigned char color;                /* color (see below)            1 */
  unsigned char dspl;                 /* display options (see below)  1 */
  unsigned char attr;                 /* attributes (see below)       1 */
  unsigned char smbl[2];              /* waypoint symbol              2 */
  unsigned char subclass[18];         /* subclass                    18 */
  unsigned char lat[4];		/* position */
  unsigned char lon[4];		/* position */
  float alt;			       /* altitude in meters           4 */
  float dpth;			       /* depth in meters              4 */
  float dist;			       /* proximity distance in meters 4 */
  char state[2];		       /* state                        2 */
  char cc[2];			       /* country code                 2 */
  char varlenstrs[1];			/* start of variable length strings */
  /* G_char ident[]; variable length string 1-51       */
  /* G_char comment[]; waypoint user comment 1-51      */
  /* G_char facility[]; facility name 1-31             */
  /* G_char city[]; city name 1-25                     */
  /* G_char addr[]; address number 1-51                */
  /* G_char cross_road[]; intersecting road label 1-51 */
}
D108_Wpt_Type;

typedef struct {		       /* structure de waypoint "interne" */
  unsigned char ident[51];	       /* identifier (50 + '0') */
  Semicircle_Type posn;	       /* position (common to all Garmin types) */
  unsigned char cmnt[51];	       /* comment (50 + '0') */
  float dst;			       /* proximity distance */
  float alt;			       /* altitude */
  int smbl;			       /* symbol id */
  unsigned char dspl;		       /* display option */
  unsigned char color;	       /* color */
}
Custom_Wpt_Type;

typedef struct {		       /* internal track header */
  char name[256];		       /* nom du groupe de trackpoints */
  unsigned char dspl;		       /* display on the map ? */
  unsigned char color;	       /* color */
  unsigned char type;		       /* type of following track points */
  unsigned char unused;              /* type of following track points */
  unsigned char number[2];	       /* number of track points */
  unsigned char latmin[4];	       /* latitude min */
  unsigned char latmax[4];	       /* latitude max */
  unsigned char lonmin[4];	       /* longitude min */
  unsigned char lonmax[4];	       /* longitude max */
  unsigned char unused2[2];          /* type of following track points */
}
Custom_Trk_Hdr_Type;

typedef struct {
  unsigned char lat[4];		/* position */
  unsigned char lon[4];		/* position */
  unsigned char time[4];
  unsigned char alt[4];
  unsigned char new_trk;
  unsigned char unused;
} Custom_Trk_Point_Type;

typedef struct {		       /* custom compact track point type */
  unsigned char lat[4];		/* position */
  unsigned char lon[4];		/* position */
  unsigned char new_trk;
  unsigned char unused;
} Compact_Trk_Point_Type;                /* size : 10 bytes */

struct record {
  struct {
    unsigned char type;
    unsigned short size;
    unsigned int version;
  } header;
  union {
    D103_Wpt_Type d103;
    D108_Wpt_Type d108;
    Custom_Wpt_Type CustWpt;
    Custom_Trk_Hdr_Type CustTrkHdr;
#if LATER
    Custom_Rte_Hdr_Type CustRteHdr;
#endif
  } wpt;
};


static pdbfile* file_in, *file_out;
static const char* out_fname;
static int ct = 0;
static char* dbname = NULL;

static
arglist_t my_args[] = {
  {"dbname", &dbname, "Database name", NULL, ARGTYPE_STRING, ARG_NOMINMAX},
  ARG_TERMINATOR
};

static void
rd_init(const char* fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
  pdb_close(file_in);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
wr_init(const char* fname)
{
  file_out = pdb_create(fname, MYNAME);
  out_fname = fname;
}

static void
wr_deinit(void)
{
  pdb_close(file_out);
  if (dbname) {
    xfree(dbname);
    dbname = NULL;
  }
}

static void
data_read(void)
{
  struct record* rec;
  pdbrec_t* pdb_rec;
  route_head* track_head = NULL;

  if (file_in->creator != MYCREATOR) {
    fatal(MYNAME ": Not a %s file.\n", MYNAME);
  }

  switch (file_in->type) {
  case MYWPT:
    /* blah */
    break;
  case MYTRK:
    /* blah */
    break;
  default:
    fatal(MYNAME ": Unknown file type 0x%x\n", (int) file_in->type);
  }

  for (pdb_rec = file_in->rec_list; pdb_rec; pdb_rec=pdb_rec->next) {
    waypoint* wpt_tmp;
    Custom_Trk_Point_Type* tp_cust;
    Compact_Trk_Point_Type* tp_comp;
    int lat;
    int lon;
    int sz;
    fi_t fi;
    int trk_num = 0;
    int trk_seg_num = 1;
    char trk_seg_num_buf[10];
    char* trk_name = "";

    wpt_tmp = waypt_new();

    rec = (struct record*) pdb_rec->data;
    switch (rec->header.type) {
      /*
       * G103Type
       */
    case 4:
      wpt_tmp->shortname = xstrndupt(rec->wpt.d103.ident, sizeof(rec->wpt.d103.ident));
      wpt_tmp->description = xstrndupt(rec->wpt.d103.cmnt, sizeof(rec->wpt.d103.cmnt));
      /* This is odd.   This is a Palm DB file,
       * yet the data appears to be little endian,
       * not appropriate the the actual Palm.
       */
      lon = le_read32(&rec->wpt.d103.lon);
      lat = le_read32(&rec->wpt.d103.lat);
      wpt_tmp->longitude = lon / 2147483648.0 * 180.0;
      wpt_tmp->latitude = lat / 2147483648.0 * 180.0;
      waypt_add(wpt_tmp);
      break;
      /*
       * G108Type
       */
    case 9:
      wpt_tmp->shortname = xstrndupt(rec->wpt.d108.varlenstrs, 50);
      wpt_tmp->description = xstrndupt(rec->wpt.d108.varlenstrs + strlen(wpt_tmp->shortname) + 1, 50);
      /* This is odd.   This is a Palm DB file,
       * yet the data appears to be little endian,
       * not appropriate the the actual Palm.
       */
      lon = le_read32(&rec->wpt.d108.lon);
      lat = le_read32(&rec->wpt.d108.lat);
      wpt_tmp->longitude = lon / 2147483648.0 * 180.0;
      wpt_tmp->latitude = lat / 2147483648.0 * 180.0;
      fi.i = le_read32(&rec->wpt.d108.alt);
      wpt_tmp->altitude = fi.f;
      fi.i = le_read32(&rec->wpt.d108.dpth);
      WAYPT_SET(wpt_tmp, depth, fi.f);
      fi.i = le_read32(&rec->wpt.d108.dist);
      WAYPT_SET(wpt_tmp, proximity, fi.f);
      wpt_tmp->wpt_flags.icon_descr_is_dynamic = 0;
      wpt_tmp->icon_descr = gt_find_desc_from_icon_number((rec->wpt.d108.smbl[1] << 8) + rec->wpt.d108.smbl[0], PCX, NULL);
      waypt_add(wpt_tmp);
      break;

      /*
       * CustomTrkHdr
       */
    case 101:
      trk_name = rec->wpt.CustTrkHdr.name;
      sz = be_read16(&rec->wpt.CustTrkHdr.number);

      /* switch between custom track points and compact track points.
       * (compact points have no altitude and time info.
       */
      switch (rec->wpt.CustTrkHdr.type) {
      case 102:
        tp_cust = (Custom_Trk_Point_Type*)((char*) pdb_rec->data + sizeof(rec->header) + sizeof(rec->wpt.CustTrkHdr));
        while (sz--) {
          if ((int)(tp_cust->new_trk) == 1 || trk_seg_num == 1) {
            /*
             * Start a new track segment
             */
            track_head = route_head_alloc();
            if (trk_seg_num == 1) {
              track_head->rte_name = xstrdup(trk_name);
            } else {
              /* name in the form TRACKNAME #n */
              snprintf(trk_seg_num_buf, sizeof(trk_seg_num_buf), "%d", trk_seg_num);
              track_head->rte_name = xmalloc(strlen(trk_name)+strlen(trk_seg_num_buf)+3);
              sprintf(track_head->rte_name, "%s #%s", trk_name, trk_seg_num_buf);
            }
            trk_seg_num++;
            track_head->rte_num = trk_num;
            trk_num++;
            track_add_head(track_head);
          }

          wpt_tmp = waypt_new();

          /* This is even more odd.
           * Track data is stored as big endian while
           * waypoint data is little endian!?
           */
          lon = be_read32(&tp_cust->lon);
          lat = be_read32(&tp_cust->lat);
          wpt_tmp->longitude = lon / 2147483648.0 * 180.0;
          wpt_tmp->latitude = lat / 2147483648.0 * 180.0;
          /*
           * Convert Garmin/GPilotS time format to gpsbabel time format.
           * Garmin/GPilotS count seconds from "UTC 12:00 AM December 31 1989".
           * gpsbabel counts seconds from "UTC 12:00 AM January 1 1970".
           */
          wpt_tmp->creation_time = be_read32(&tp_cust->time) + 631065600;
          fi.i = be_read32(&tp_cust->alt);
          wpt_tmp->altitude = fi.f;
          track_add_wpt(track_head, wpt_tmp);
          tp_cust++;
        }
        break;
      case 104:
        tp_comp = (Compact_Trk_Point_Type*)((char*) pdb_rec->data + sizeof(rec->header) + sizeof(rec->wpt.CustTrkHdr));
        while (sz--) {
          if ((int)(tp_comp->new_trk) == 1 || trk_seg_num == 1) {
            /*
             * Start a new track segment
             */
            track_head = route_head_alloc();
            if (trk_seg_num == 1) {
              track_head->rte_name = xstrdup(trk_name);
            } else {
              /* name in the form TRACKNAME #n */
              snprintf(trk_seg_num_buf, sizeof(trk_seg_num_buf), "%d", trk_seg_num);
              track_head->rte_name = xmalloc(strlen(trk_name)+strlen(trk_seg_num_buf)+3);
              sprintf(track_head->rte_name, "%s #%s", trk_name, trk_seg_num_buf);
            }
            trk_seg_num++;
            track_head->rte_num = trk_num;
            trk_num++;
            track_add_head(track_head);
          }

          wpt_tmp = waypt_new();
          lon = be_read32(&tp_comp->lon);
          lat = be_read32(&tp_comp->lat);
          wpt_tmp->longitude = lon / 2147483648.0 * 180.0;
          wpt_tmp->latitude = lat / 2147483648.0 * 180.0;
          track_add_wpt(track_head, wpt_tmp);
          tp_comp++;
        }
        break;
      default:
        fatal(MYNAME ": track point type %d not supported.\n", rec->wpt.CustTrkHdr.type);
      }
      break;
    default:
      fatal(MYNAME ": input record type %d not supported.\n", rec->header.type);
    }

  }
}


struct hdr {
  char* wpt_name;
  waypoint* wpt;
};

static void
my_write_wpt(const waypoint* wpt)
{
  struct record* rec;
  char* vdata;
  int lat, lon;

  rec = xcalloc(sizeof *rec, 1);
  vdata = (char*)rec + sizeof(*rec);

  rec->header.type = 4;
  rec->header.size = 5;
  rec->header.version = 6;

  strncpy(rec->wpt.d103.ident, wpt->shortname, sizeof(rec->wpt.d103.ident));
  strncpy(rec->wpt.d103.cmnt, wpt->description, sizeof(rec->wpt.d103.cmnt));
  lat = wpt->latitude  / 180.0 * 2147483648.0;
  lon = wpt->longitude  / 180.0 * 2147483648.0;
  le_write32(&rec->wpt.d103.lat, lat);
  le_write32(&rec->wpt.d103.lon, lon);

  pdb_write_rec(file_out, 0, ct, ct+1, rec, (char*)vdata - (char*)rec);
  ct++;
  xfree(rec);
}

static void
data_write(void)
{
  if (dbname) {
    strncpy(file_out->name, dbname, PDB_DBNAMELEN);
  } else {
    strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
  }

  /*
   * Populate header.
   */
  file_out->name[PDB_DBNAMELEN-1] = 0;
  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;

  file_out->type = MYWPT;
  file_out->creator = MYCREATOR;
  file_out->version = 1;

  waypt_disp_all(my_write_wpt);
}


ff_vecs_t gpilots_vecs = {
  ff_type_file,
  { ff_cap_read | ff_cap_write, ff_cap_read | ff_cap_write, ff_cap_none},
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  my_args,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
