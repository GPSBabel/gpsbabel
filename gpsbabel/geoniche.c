/*
    Read and write GeoNiche files.

    Copyright (C) 2003 Rick Richardson <rickr@mn.rr.com>

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
#include "coldsync/palm.h"
#include "coldsync/pdb.h"

#define MYNAME		"Geoniche"
#define MYTYPE 		0x50454e44  	/* PEND */
#define MYCREATOR	0x47656f4e 	/* GeoN */

static FILE		*FileIn;
static FILE		*FileOut;
static const char	*FilenameOut;
static struct pdb	*PdbOut;

static char		Rec0Magic[] = "68000NV4Q2";

static char *Arg_dbname = NULL;
static char *Arg_category = NULL;

static
arglist_t Args[] = {
	{"dbname", &Arg_dbname,
	    "Database name (filename)", ARGTYPE_STRING },
	{"category", &Arg_category,
	    "Category name (Cache)", ARGTYPE_STRING },
	{0, 0, 0, 0 }
};

#define	ARG_FREE(X) do { if (X) { xfree(X); X = NULL; } } while (0)

/*
 * Conversions between gc.com ID's and GID's
 */
static char	GcSet[] = "0123456789ABCDEFGHJKMNPQRTVWXYZ";
static int	GcOffset = 16 * 31 * 31 * 31 - 65536;

static int
gid2id(char *gid)
{
    char	*p;
    int		i, val;

    if (strncmp(gid, "GC", 2) != 0)
	return -1;
    if (strlen(gid) != 6)
	return -1;
    gid += 2;

    if (strcmp(gid, "G000") < 0)
	return strtol(gid, NULL, 16);

    for (val = i = 0; i < 4; ++i)
    {
	val *= 31;
	p = strchr(GcSet, gid[i]);
	if (!p) return -1;
	val += p - GcSet;
    }
    return val - GcOffset;
}

static void
id2gid(char gid[6+1], int id)
{
    gid[0] = 0;
    if (id < 0)
	return;
    else if (id < 65536)
	snprintf(gid, 6+1, "GC%04X", id);
    else
    {
	int	i;

	id += GcOffset;
	gid[0] = 'G';
	gid[1] = 'C';
	for (i = 5; i >= 2; --i) {
	    gid[i] = GcSet[id%31];
	    id /= 31;
	}
	gid[6] = 0;
	if (id)
	    gid[0] = 0;
    }
    return;
}

static void
rd_init(const char *fname)
{
    FileIn = fopen(fname, "rb");
    if (FileIn == NULL)
	fatal(MYNAME ": Cannot open %s for reading\n", fname);
}

static void
rd_deinit(void)
{
    fclose(FileIn);
    ARG_FREE(Arg_dbname);
    ARG_FREE(Arg_category);
}

static void
wr_init(const char *fname)
{
    FileOut = fopen(fname, "wb");
    FilenameOut = fname;
    if (FileOut == NULL)
	fatal(MYNAME ": Cannot open %s for writing\n", fname);
}

static void
wr_deinit(void)
{
    fclose(FileOut);
    ARG_FREE(Arg_dbname);
    ARG_FREE(Arg_category);
}

static char *
field(char **pp, int *lenp)
{
    int		len = *lenp;
    char	*p = *pp;
    char	*dp, *dbuf;
    int		state = 0;

    if (len == 0 || *p == 0)
	return NULL;

    dbuf = dp = xmalloc(len);
    while (len)
    {
	char	ch;

	ch = *p++;
	--len;
	if (ch == 0 || len == 0)
	    break;
	switch (state)
	{
	case 0:
	    if (ch == '\\')
		state = 1;
	    else if (ch == ',')
		goto eof;
	    else
		*dp++ = ch;
	    break;
	default:
	    *dp++ = ch;
	    state = 0;
	    break;
	}
    }
eof:
    *dp++ = 0;
    dbuf = xrealloc(dbuf, dp - dbuf);
    /* fprintf(stderr, "<%.8s> dbuf=%x, len=%d\n", *pp, dbuf, len); */
    *pp = p;
    *lenp = len;
    return dbuf;
}

static void
data_read(void)
{
    struct pdb *pdb;
    struct pdb_record *pdb_rec;

    if (NULL == (pdb = pdb_Read(fileno(FileIn))))
	fatal(MYNAME ": pdb_Read failed\n");

    if ((pdb->creator != MYCREATOR) || (pdb->type != MYTYPE))
	fatal(MYNAME ": Not a GeoNiche file.\n");

    /* Process record 0 */
    pdb_rec = pdb->rec_index.rec;
    if (strcmp(pdb_rec->data, Rec0Magic))
	fatal(MYNAME ": Bad record 0, not a GeoNiche file.\n");
    pdb_rec = pdb_rec->next;

    /* Process the rest of the records */
    for (; pdb_rec; pdb_rec = pdb_rec->next)
    {
	waypoint	*wpt;
	char		*vdata;
	int		vlen;
	char		*p;

	int		id;
	int		route_id;
	char		*title;
	char		*category;
	double		lat, lon, alt;
	char		*datestr, *timestr;
	int		icon;
	char		*notes;
	char		gid[6+1];
	struct tm	tm;

	wpt = xcalloc(sizeof(*wpt), 1);
	if (!wpt)
	    fatal(MYNAME ": Couldn't allocate waypoint.\n");
	vdata = (char *) pdb_rec->data;
	vlen = pdb_rec->data_len;

	/* Field 1: Target */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 1.\n");
	if (strcmp(p, "Route") == 0)
	    fatal(MYNAME ": Route record type is not implemented.\n");
	if (strcmp(p, "Target"))
	    fatal(MYNAME ": Unknown record type '%s'.\n", p);
	xfree(p);

	/* Field 2: Import ID number */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 2.\n");
	id = atoi(p);
	xfree(p);

	/* Field 3: Title */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 3.\n");
	title = p;

	/* Field 4: Route ID number */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 4.\n");
	route_id = atoi(p);
	xfree(p);

	/* Field 5: Category */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 5.\n");
	category = p;

	/* Field 6: Latitude */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 6.\n");
	lat = atof(p);
	xfree(p);

	/* Field 7: Longitude */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 7.\n");
	lon = atof(p);
	xfree(p);

	/* Field 8: Altitude */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 8.\n");
	alt = atof(p);
	xfree(p);

	/* Field 9: Creation date */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 9.\n");
	datestr = p;

	/* Field 10: Creation time */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 10.\n");
	timestr = p;

	/* Field 11: Visited date */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 11.\n");
	xfree(p);

	/* Field 12: Visited time */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 12.\n");
	xfree(p);

	/* Field 13: Icon color (R G B) */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 13.\n");
	xfree(p);

	/* Field 14: icon number */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 14.\n");
	icon = atoi(p);
	xfree(p);

	/* Field 15: unused */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 15.\n");
	xfree(p);

	/* Field 16: unused */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 16.\n");
	xfree(p);

	/* Field 17: unused */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 17.\n");
	xfree(p);

	/* Field 18: Notes */
	p = field(&vdata, &vlen);
	if (!p) fatal(MYNAME ": Premature EOD processing field 18.\n");
	notes = p;

	sscanf(datestr, "%d/%d/%d", &tm.tm_mon, &tm.tm_mday, &tm.tm_year);
	tm.tm_mon -= 1;
	tm.tm_year -= 1900;
	sscanf(timestr, "%d:%d:%d", &tm.tm_hour, &tm.tm_min, &tm.tm_sec);
	wpt->creation_time = mktime(&tm);
	xfree(datestr);
	xfree(timestr);

	id2gid(gid, id);
	wpt->latitude = lat;
	wpt->longitude = lon;
	wpt->altitude = alt;
	wpt->icon_descr = category;

	if (gid[0])
	{
	    wpt->shortname = strdup(gid);
	    wpt->description = title;
	    wpt->notes = notes;
	}
	else
	{
	    wpt->shortname = strdup(title);
	    wpt->description = title;
	    wpt->notes = notes;
	}

	waypt_add(wpt);
    } 
    free_pdb(pdb);
}

static char *
enscape(char *s)
{
    char	*buf, *d;

    if (!s)
    {
	d =  xmalloc(1);
	*d = 0;
	return d;
    }
    buf = d = xmalloc(strlen(s) * 2 + 1);
    for (; *s; ++s)
    {
	if (*s == '\\' || *s == ',')
	    *d++ = '\\';
	*d++ = *s;
    }

    *d = 0;
    return buf;
}

/*
 * Attempt to map an icon description into a GeoNiche icon number
 */
static int
wpt2icon(const waypoint *wpt)
{
    const char	*desc = wpt->icon_descr;

    if (!desc) return 0;
    else if (strstr(desc, "reg")) return 43;
    else if (strstr(desc, "trad")) return 43;
    else if (strstr(desc, "multi")) return 44;
    else if (strstr(desc, "offset")) return 44;
    else if (strstr(desc, "virt")) return 45;
    else if (strstr(desc, "loca")) return 45;
    else if (strstr(desc, "event")) return 46;
    else if (strstr(desc, "lett")) return 47;
    else if (strstr(desc, "hyb")) return 47;
    else if (strstr(desc, "unk")) return 48;
    else if (strstr(desc, "cam")) return 49;
    else return 0;
}

static void
copilot_writewpt(const waypoint *wpt)
{
    static int		ct = 0;
    struct pdb_record	*opdb_rec;
    int			vlen;
    static int		vsize = 4096;
    char		*vdata;
    char		*title;
    struct tm		tm;
    char		datestr[10+1];
    char		timestr[8+1];
    char		*notes;
    int			id;

    if (ct == 0)
    {
	opdb_rec = new_Record (0, 0, ct++, sizeof(Rec0Magic), Rec0Magic);	       
	if (opdb_rec == NULL)
	    fatal(MYNAME ": libpdb couldn't create record\n");
	if (pdb_AppendRecord(PdbOut, opdb_rec))
	    fatal(MYNAME ": libpdb couldn't append record\n");
    }

    if (wpt->description[0])
	title = enscape(wpt->description);
    else
	title = enscape(wpt->shortname);

    id = gid2id(wpt->shortname);
    if (id < 0)
	id = ct;

    tm = *localtime(&wpt->creation_time);
    strftime(datestr, sizeof(datestr), "%m/%d/%Y", &tm);
    strftime(timestr, sizeof(timestr), "%H:%M:%S", &tm);

    /* Notes field MUST have soemthing in it */
    if (!wpt->notes || wpt->notes[0] == 0)
	notes = strdup(title);
    else
	notes = enscape(wpt->notes);

    vdata = (char *) xmalloc(vsize);
    if (vdata == NULL)
	fatal(MYNAME ": libpdb couldn't get record memory\n");

    for (;;)
    {
	vlen = snprintf(vdata, vsize,
	    "Target,%d,%s,,%s,%f,%f,%f,%s,%s,,,,%d,,,,%s"
	    , id
	    , title
	    /* route ID */
	    , Arg_category ? Arg_category : "Cache"
	    , wpt->latitude
	    , wpt->longitude
	    , wpt->altitude
	    , datestr
	    , timestr
	    /* visited date */
	    /* visited time */
	    /* icon color R G B */
	    , wpt2icon(wpt)
	    /* unused1 */
	    /* unused2 */
	    /* unused3 */
	    , notes
	    );

	if (vlen > -1 && vlen < vsize)
	    break;

	/* try again with more space. */
	if (vlen > -1)
	    vsize = vlen + 1;
	else
	    vsize *= 2;
	vdata = (char *) xrealloc(vdata, vsize);
	if (vdata == NULL)
	    fatal(MYNAME ": libpdb couldn't get record memory\n");
    }

    opdb_rec = new_Record (0, 0, ct++, vlen+1, vdata);	       

    if (opdb_rec == NULL)
	fatal(MYNAME ": libpdb couldn't create record\n");
    if (pdb_AppendRecord(PdbOut, opdb_rec))
	fatal(MYNAME ": libpdb couldn't append record\n");

    xfree(notes);
    xfree(title);
    xfree(vdata);
}

static void
data_write(void)
{
    if (NULL == (PdbOut = new_pdb()))
	fatal (MYNAME ": new_pdb failed\n");

    if (Arg_dbname)
	strncpy(PdbOut->name, Arg_dbname, PDB_DBNAMELEN);
    else
	strncpy(PdbOut->name, FilenameOut, PDB_DBNAMELEN);
    PdbOut->name[PDB_DBNAMELEN-1] = 0;

    PdbOut->attributes = PDB_ATTR_BACKUP;
    PdbOut->ctime = PdbOut->mtime = time(NULL) + (49*365 + 17*366) * (60*60*24);
    PdbOut->type = MYTYPE;
    PdbOut->creator = MYCREATOR; 
    PdbOut->version = 0;
    PdbOut->modnum = 1;

    waypt_disp_all(copilot_writewpt);
    
    pdb_Write(PdbOut, fileno(FileOut));

    free_pdb(PdbOut);
}


ff_vecs_t geoniche_vecs =
{
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	Args
};
