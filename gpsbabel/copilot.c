/*
    Read and write CoPilot files.

    Copyright (C) 2002 Paul Tomblin, ptomblin@xcski.com

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
#include "grtcirc.h"

#define MYNAME			"CoPilot Waypoint"
#define wayp_TYPE 		0x77617970  	/* wayp */
#define wayu_TYPE		0x77617975	/* wayu */
#define swpu_TYPE 		0x73777075  	/* swpu */
#define GXPU_CREATOR	0x47584255 		/* GXBU */
#define AP_P_CREATOR	0x41502d50 		/* AP-P */


struct record0 {
  pdb_double	latitude; 	/* PDB double format, */
  pdb_double	longitude; 	/* similarly, neg = east */
  pdb_double	magvar; 	/* magnetic variation in degrees, neg = east */
  gbuint32	elevation; 	/* feet */
};

struct record1 {
  pdb_double	latitude; 	/* PDB double format, */
  pdb_double	longitude; 	/* similarly, neg = east */
  pdb_double	magvar; 	/* magnetic variation in degrees, neg = east */
  pdb_double	elevation; 	/* feet */
};

struct record3 {
  pdb_double	latitude; 	/* PDB double format, */
  pdb_double	longitude; 	/* similarly, neg = east */
  pdb_double	magvar; 	/* magnetic variation in degrees, neg = east */
  pdb_double	elevation; 	/* feet */
  char		flags;		/* flags */
};

struct record4 {
  pdb_double	latitude; 	/* PDB double format, */
  pdb_double	longitude; 	/* similarly, neg = east */
  pdb_float	magvar; 	/* magnetic variation in degrees, neg = east */
  pdb_float	elevation; 	/* feet */
};

static pdbfile *file_in, *file_out;
static const char *out_fname;
static int ct;

static void
rd_init(const char *fname)
{
  file_in = pdb_open(fname, MYNAME);
}

static void
rd_deinit(void)
{
  pdb_close(file_in);
}

static void
wr_init(const char *fname)
{
  file_out = pdb_create(fname, MYNAME);
  out_fname = fname;
  ct = 0;
}

static void
wr_deinit(void)
{
  pdb_close(file_out);
}

static waypoint*
read_version0(void *data)
{
  char *vdata;
  waypoint *wpt_tmp;
  struct record0* rec = (struct record0*)data;

  wpt_tmp = waypt_new();

  wpt_tmp->longitude =
    DEG(-pdb_read_double(&rec->longitude));
  wpt_tmp->latitude =
    DEG(pdb_read_double(&rec->latitude));
  wpt_tmp->altitude = FEET_TO_METERS(be_read32(&rec->elevation));

  vdata = (char *) data + sizeof(*rec);

  wpt_tmp->shortname = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->description = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->notes = NULL;

  return wpt_tmp;
}

static waypoint*
read_version1(void *data)
{
  char *vdata;
  waypoint *wpt_tmp;
  struct record1* rec = (struct record1*)data;

  wpt_tmp = waypt_new();

  wpt_tmp->longitude =
    DEG(-pdb_read_double(&rec->longitude));
  wpt_tmp->latitude =
    DEG(pdb_read_double(&rec->latitude));
  wpt_tmp->altitude =
    FEET_TO_METERS(pdb_read_double(&rec->elevation));

  vdata = (char *) data + sizeof(*rec);

  wpt_tmp->shortname = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->description = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->notes = xstrdup(vdata);

  return wpt_tmp;
}

static waypoint*
read_version3(void *data)
{
  char *vdata;
  waypoint *wpt_tmp;
  struct record3* rec = (struct record3*)data;

  wpt_tmp = waypt_new();

  wpt_tmp->longitude =
    DEG(-pdb_read_double(&rec->longitude));
  wpt_tmp->latitude =
    DEG(pdb_read_double(&rec->latitude));
  wpt_tmp->altitude =
    FEET_TO_METERS(pdb_read_double(&rec->elevation));

  vdata = (char *) data + sizeof(*rec);

  wpt_tmp->shortname = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->description = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->notes = xstrdup(vdata);

  return wpt_tmp;
}

static waypoint*
read_version4(void *data)
{
  char *vdata;
  waypoint *wpt_tmp;
  struct record4* rec = (struct record4*)data;

  wpt_tmp = waypt_new();

  wpt_tmp->longitude =
    DEG(-pdb_read_double(&rec->longitude));
  wpt_tmp->latitude =
    DEG(pdb_read_double(&rec->latitude));
  wpt_tmp->altitude =
    FEET_TO_METERS(pdb_read_float(&rec->elevation));

  vdata = (char *) data + sizeof(*rec);

  wpt_tmp->shortname = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->description = xstrdup(vdata);
  vdata = vdata + strlen(vdata) + 1;

  wpt_tmp->notes = xstrdup(vdata);

  return wpt_tmp;
}

static void
data_read(void)
{
  pdbrec_t *pdb_rec;

  if ((file_in->creator != GXPU_CREATOR && file_in->creator != AP_P_CREATOR) ||
      (file_in->type != wayp_TYPE && file_in->type != swpu_TYPE &&
       file_in->type != wayu_TYPE)) {
    fatal(MYNAME ": Not a CoPilot file.\n");
  }
  if (file_in->version > 4) {
    fatal(MYNAME ": %d is not a known version.\n", file_in->version);
  }


  for (pdb_rec = file_in->rec_list; pdb_rec; pdb_rec = pdb_rec->next) {
    waypoint *wpt_tmp;

    switch (file_in->version) {
    case 0:
      wpt_tmp = read_version0(pdb_rec->data);
      break;
    case 1:
    case 2:
      wpt_tmp = read_version1(pdb_rec->data);
      break;
    case 3:
      wpt_tmp = read_version3(pdb_rec->data);
      break;
    case 4:
      wpt_tmp = read_version4(pdb_rec->data);
      break;
    default:
      fatal(MYNAME ": Unknown version %d.\n", file_in->version);
    }
    waypt_add(wpt_tmp);
  }
}

static void
copilot_writewpt(const waypoint *wpt)
{
  struct record4 *rec;
  char *vdata;

  rec = xcalloc(sizeof(*rec)+1141,1);

  pdb_write_double(&rec->latitude, RAD(wpt->latitude));
  pdb_write_double(&rec->longitude, RAD(-wpt->longitude));
  pdb_write_float(&rec->magvar, 0);
  pdb_write_float(&rec->elevation,
                  METERS_TO_FEET(wpt->altitude));

  vdata = (char *)rec + sizeof(*rec);
  if (wpt->shortname) {
    strncpy(vdata, wpt->shortname, 10);
    vdata[9] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;
  if (wpt->description) {
    strncpy(vdata, wpt->description, 100);
    vdata[99] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  if (wpt->notes) {
    strncpy(vdata, wpt->notes, 1000);
    vdata[999] = '\0';
  } else {
    vdata[0] ='\0';
  }
  vdata += strlen(vdata) + 1;

  pdb_write_rec(file_out, 0, 2, ct++, rec, (char *)vdata - (char *)rec);

  xfree(rec);
}

static void
data_write(void)
{
  strncpy(file_out->name, out_fname, PDB_DBNAMELEN);
  file_out->name[PDB_DBNAMELEN-1] = 0;
  file_out->attr = PDB_FLAG_BACKUP;
  file_out->ctime = file_out->mtime = current_time() + 2082844800U;
  file_out->type = wayp_TYPE;
  file_out->creator = GXPU_CREATOR;
  file_out->version = 4;

  waypt_disp_all(copilot_writewpt);
}


ff_vecs_t copilot_vecs = {
  ff_type_file,
  FF_CAP_RW_WPT,
  rd_init,
  wr_init,
  rd_deinit,
  wr_deinit,
  data_read,
  data_write,
  NULL,
  NULL,
  CET_CHARSET_ASCII, 0	/* CET-REVIEW */
};
#endif
