/*
    Access to EasyGPS files.

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
#include <ctype.h>

static FILE *file_in;
static FILE *file_out;
static void *mkshort_handle;
static char *deficon = "Waypoint";

#define MYNAME "EasyGPS"

static
arglist_t easygps_args[] = {
/*	{"deficon", &deficon, "Default icon name"}, */
	{0, 0, 0}
};

static void
rd_init(const char *fname, const char *args)
{
	char ibuf[100] = {'0'} ;
	const char *ezsig = "TerraByte Location File";

	file_in = fopen(fname, "rb");
	if (file_in == NULL) {
		fatal(MYNAME ": Cannot open %s for reading\n", fname);
	}

	fread(ibuf, 52, 1, file_in);
	
	if (strncmp(ibuf, ezsig, sizeof(ezsig)-1) ||
				ibuf[51] != 'W') {
		fatal(MYNAME ": %s is not an EasyGPS file\n", fname);
	}
}

static void
rd_deinit(void)
{
	fclose(file_in);
}

static void
wr_init(const char *fname, const char *args)
{
	file_out = fopen(fname, "wb");
	mkshort_handle = mkshort_new_handle();

	if (file_out == NULL) {
		fatal(MYNAME ": Cannot open %s for writing\n", fname);
	}
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

/*
 *  Read a pascal string from file_in and return a copy in allocated
 *  storage.
 */
static void *
pread(void)
{
	char *d;
	int ilen;

	ilen = fgetc(file_in);
	d = xmalloc(ilen + 1);
	fread(d, ilen, 1, file_in);
	d[ilen] = 0;
	return d;
}

static void
data_read(void)
{
	char p;
	char ibuf[10];
	char bbuf[4096];
	char *bbufp;
	double d;
	do {
		unsigned char tag;
		unsigned char ilen;
		waypoint *wpt_tmp;

		wpt_tmp = xcalloc(sizeof(*wpt_tmp), 1);
			
		for (tag = fgetc(file_in); tag != 0xff; tag = fgetc(file_in)) {
		switch (tag) {
			case 1:
				wpt_tmp->shortname = pread();
				break;
			case 2:
			case 3:
				wpt_tmp->description = pread();
				break;
			case 5:
				wpt_tmp->notes = pread();
				break;
			case 6:
				wpt_tmp->url_link_text = pread();
				break;
			case 7:
				wpt_tmp->icon_descr = pread();
				break;
			case 8:  /* NULL Terminated (vs. pascal) descr */
				bbufp = bbuf;
				for(;;) {
					p = fgetc(file_in);
					*bbufp++ = p;
					if ( 0 == p ) {
						break;
					}
				}
				wpt_tmp->description = xstrdup(bbuf);
				break;
			case 9: /* NULL Terminated (vs. pascal) link */
				bbufp = bbuf;
				for(;;) {
					p = fgetc(file_in);
					*bbufp++ = p;
					if ( 0 == p ) {
						break;
					}
				}
				wpt_tmp->url = xstrdup(bbuf);
				break;
			case 0x10:
				bbufp = bbuf;
				for(;;) {
					p = fgetc(file_in);
					*bbufp++ = p;
					if ( 0 == p ) {
						break;
					}
				}
				wpt_tmp->url_link_text = xstrdup(bbuf);
				break;
			case 0x63:
				fread(ibuf, 8, 1, file_in);
				le_read64(&d, ibuf);
				wpt_tmp->position.latitude.degrees = d;
				break;
			case 0x64:
				fread(ibuf, 8, 1, file_in);
				le_read64(&d, ibuf);
				wpt_tmp->position.longitude.degrees = d;
				break;
			case 0x65:
			case 0x66:
				fread(ibuf, 8, 1, file_in);
				break;
			case 0x84:
			case 0x85:
				fread(ibuf, 4, 1, file_in);
				break;
			case 0x86: /* May be proximity.  I think it's time. */
				fread(ibuf, 4, 1, file_in);
				break;
			default:
				printf("Unknown tag %x\n", tag);
				;
		}
		}
		waypt_add(wpt_tmp);
		p = fgetc(file_in);
	} while (!feof(file_in) && (p == 'W'));
}



	/*
	 * Write a Pascal string to the output stream.
	 */
static void
write_pstring(const char *p)
{
	int len = strlen(p);
	if (len > 255) {
			fatal(MYNAME ": String too long at %d bytes\n", len);
	}
	fputc(len, file_out);
	fwrite(p, len, 1, file_out);
}

static void
ez_disp(const waypoint *wpt)
{
	fprintf(file_out, "W", 0xb);
	if (wpt->shortname) {
		fputc(1, file_out);
		write_pstring(wpt->shortname);
	} 
	if (wpt->description) {
		fputc(3, file_out);
		write_pstring(wpt->description);
	}
	if (wpt->icon_descr) {
		fputc(7, file_out);
		write_pstring(wpt->icon_descr);
	}
	fputc(0x63, file_out);
	fwrite(&wpt->position.latitude.degrees, 8, 1, file_out);
	fputc(0x64, file_out);
	fwrite(&wpt->position.longitude.degrees, 8, 1, file_out);
	if (wpt->notes) {
		fputc(5, file_out);
		write_pstring(wpt->notes);
	}
	if (wpt->url_link_text) {
		fputc(6, file_out);
		write_pstring(wpt->url_link_text);
	}
	if (1 && wpt->url) {
		fputc(9, file_out);
		fputs(wpt->url, file_out);
		fputc(0, file_out);
	}
	fputc(0xff, file_out);
}

static void
data_write(void)
{
	setshort_length(mkshort_handle, 6);

	fprintf(file_out, 
		"TerraByte Location File Copyright 2001 TopoGrafix\n");
	/*
	 * I don't know what this is.
	 */
	fprintf(file_out, "%c", 0xb);

	waypt_disp_all(ez_disp);

	/*
	 * Files seem to always end in a zero.
	 */
	fputc(0x00, file_out);
}


ff_vecs_t easygps_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
	easygps_args
};
