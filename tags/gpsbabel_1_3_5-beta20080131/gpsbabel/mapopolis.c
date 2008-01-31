/*
    Read and write Mapopolis files.

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

#define MYNAME "Companion Waypoints"
#define MYTYPE  0x64617461  	/* Platdata */
#define MYCREATOR 0x5061746c 	/* Plat */

#define	LONDIV	(85116.044444)
#define	LATDIV	(1000000.0/9)
#define	LONDIV2	(85116.044444/2)
#define	LATDIV2	(1000000.0/(9*2))


struct record0
{
	char	unk[6];
	pdb_32 lon1;
	pdb_32 lat1;
	pdb_32 lon2;
	pdb_32 lat2;
	pdb_32 lonD;
	pdb_32 latD;
	char	name[31+1];
	char	unk2[35];
	char	demo;
	char	unk3[4];
	unsigned char	expcode[4];
	/* ... more stuff ... */
};

double	Lat1, Lon1;
double	Lat2, Lon2;
double	LatD, LonD;

struct record {
	char unknown[10];
	pdb_16 unk1;
	pdb_16 unk2;
	pdb_16 lon1d;
	pdb_16 lat1d;
	pdb_16 lon2d;
	pdb_16 lat2d;
};

static pdbfile *file_in, *file_out;
static short_handle mkshort_handle;

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
	mkshort_handle = mkshort_new_handle();
	setshort_length(mkshort_handle, 20);
}

static void
wr_deinit(void)
{
	pdb_close(file_out);
	mkshort_del_handle(&mkshort_handle);
}

convert_rec0(struct record0 *rec0)
{
	Lon1 = be_read32(&rec0->lon1) / LONDIV;
	Lat1 = be_read32(&rec0->lat1) / LATDIV;
	Lon2 = be_read32(&rec0->lon2) / LONDIV;
	Lat2 = be_read32(&rec0->lat2) / LATDIV;
	LonD = be_read32(&rec0->lonD) / LONDIV2;
	LatD = be_read32(&rec0->latD) / LATDIV2;

// printf("%s: %.5f %.5f %.5f %.5f %.5f %.5f\n",
//	rec0->name, Lat1, Lon1, Lat2, Lon2, LatD, LonD);


}

/*
 *  * Decode the information field
 *   */
void
decode(char *buf)
{
	int	i;

	for (i = 0; buf[i]; ++i) {
		buf[i] = buf[i] ^ ((i % 96) & 0xf);
	}
}

static void
data_read(void)
{
	struct record *rec;
	pdbrec_t *pdb_rec;

	if ((file_in->creator != MYCREATOR) || (file_in->type != MYTYPE)) {
		fatal(MYNAME ": Not a Magellan Navigator file.\n");
	}

	pdb_rec = file_in->rec_list;
	convert_rec0((struct record0*) pdb_rec->data);

//	for(pdb_rec = pdb->rec_index.rec; pdb_rec; pdb_rec=pdb_rec->next) {
	for(pdb_rec = pdb_rec->next; pdb_rec; pdb_rec=pdb_rec->next) {
		waypoint *wpt_tmp;
		char *vdata = 0;
		char *edata;
		struct tm tm = {0};

		rec = (struct record *) pdb_rec->data;
		edata = (char *) rec + pdb_rec->size;

		for (; vdata < edata; rec = (struct record *) vdata) {
			wpt_tmp = waypt_new();
			wpt_tmp->latitude = Lat1 + 
				be_read16(&rec->lat1d) / LATDIV2; 
			wpt_tmp->longitude = Lon1 + 
				be_read16(&rec->lon1d) / LONDIV2; 

			vdata = (char *) rec + sizeof(*rec);
			wpt_tmp->description = xstrdup(vdata);
			vdata += strlen(wpt_tmp->description) + 1 + 6;

			while (*vdata == 0x40)
				vdata++;
			decode(vdata);
			wpt_tmp->notes = xstrdup(vdata);
			vdata += strlen(wpt_tmp->notes) + 1;

			waypt_add(wpt_tmp);
		}
	}
}


static void
my_writewpt(const waypoint *wpt)
{
#if 0
	struct record *rec;
	static int ct;
	struct tm *tm;
	char *vdata;
	time_t tm_t;
	const char *sn = global_opts.synthesize_shortnames ?
		mkshort(mkshort_handle, wpt->description) :
		wpt->shortname;

	rec = xcalloc(sizeof(*rec)+56,1);

        tm = NULL;	
	if ( wpt->creation_time ) {
		tm = gmtime( &wpt->creation_time);
	}
	if ( !tm ) {
		tm_t = current_time();
		tm = gmtime( &tm_t );
	}
	
	be_write16( &rec->crt_sec, tm->tm_sec );
	be_write16( &rec->crt_min, tm->tm_min );
	be_write16( &rec->crt_hour, tm->tm_hour );
	be_write16( &rec->crt_mday, tm->tm_mday );
	be_write16( &rec->crt_mon, tm->tm_mon + 1 );
	be_write16( &rec->crt_year, tm->tm_mon + 1900 );
	
        be_write16( &rec->unknown, 0);
	
	be_write16( &rec->xx_sec, tm->tm_sec );
	be_write16( &rec->xx_min, tm->tm_min );
	be_write16( &rec->xx_hour, tm->tm_hour );
	be_write16( &rec->xx_mday, tm->tm_mday );
	be_write16( &rec->xx_mon, tm->tm_mon + 1 );
	be_write16( &rec->xx_year, tm->tm_mon + 1900 );
	
        be_write16( &rec->unknown2, 0);
	
	be_write32(&rec->longitude, si_round(wpt->longitude * 100000.0));
	be_write32(&rec->latitude, si_round(wpt->latitude * 100000.0));
	be_write32(&rec->elevation, wpt->altitude);

	rec->plot = 0;
	rec->unknown3 = 'a';
	
	vdata = (char *)rec + sizeof(*rec);
	if ( sn ) {
		strncpy( vdata, sn, 21 );
		vdata[20] = '\0';
	}
	else { 
		vdata[0] ='\0';
	}
	vdata += strlen( vdata ) + 1;
	if ( wpt->description ) {
		strncpy( vdata, wpt->description, 33 );
		vdata[32] = '\0';
	}
	else {
		vdata[0] = '\0';
	}
	vdata += strlen( vdata ) + 1;
	vdata[0] = '\0';
	vdata[1] = '\0';
	vdata += 2;
	
	pdb_write(file_out, 0, rec, (char *)vdata - (char *)rec);

	xfree(rec);
#endif
}

static void
data_write(void)
{
	static char *appinfo = 
		"\0\x01"
		"User\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
		"\0\x01\x02\x03\x04\x05\x06\x07\x08"
		"\x09\x0a\x0b\x0c\x0d\x0e\x0f\0\0";
	
	strncpy(file_out->name, "Companion Waypoints", PDB_DBNAMELEN);
	file_out->name[PDB_DBNAMELEN-1] = 0;
	file_out->attr = PDB_FLAG_BACKUP;
	file_out->ctime = file_out->mtime = current_time() + 2082844800U;
	file_out->type = MYTYPE;  /* CWpt */
	file_out->creator = MYCREATOR; /* cGPS */
	file_out->version = 1;
	file_out->appinfo = (void *)appinfo;
	file_out->appinfo_len = 276;

	waypt_disp_all(my_writewpt);
}


ff_vecs_t mapopolis_vecs = {
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
