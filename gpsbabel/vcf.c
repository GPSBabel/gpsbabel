/*
    Output only format for Vcard format, VCF

    Copyright (C) 2005 Robert Lipe, robertlipe@usa.net

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
#include "jeeps/gpsmath.h"
#include <ctype.h>

static FILE *file_out;
static void *mkshort_handle;

static char *encrypt = NULL;

#define MYNAME "VCF"

static
arglist_t vcf_args[] = {
	{ "encrypt", &encrypt,
		"Encrypt hints using ROT13", NULL, ARGTYPE_BOOL },
	{0, 0, 0, 0, 0}
};

static void
wr_init(const char *fname)
{
	file_out = xfopen(fname, "w", MYNAME);
	mkshort_handle = mkshort_new_handle();
}

static void
wr_deinit(void)
{
	fclose(file_out);
	mkshort_del_handle(mkshort_handle);
}

/*
 * Print a possibly empty input string, replacing newlines with escaped
 * newlines as we go.
 */
static void
vcf_print_utf(const utf_string *s)
{
	char *p, *p2;
	char *stripped_html;

	if (!s)
		return;

	stripped_html = strip_html(s);
 	p = gstrsub(stripped_html, "\n", "\\n");
 	p2 = gstrsub(p, "<p>", "\\n");
	fputs(p2, file_out);
	xfree(p);
	xfree(p2);
}

static void
vcf_print(const char *s)
{
	char *p;

	if (!s)
		return;

 	p = gstrsub(s, "\n", "\\n");
	fputs(p, file_out);
	xfree(p);
}

static void
vcf_disp(const waypoint *wpt)
{
	int latint, lonint;
	char tbuf[1024];
	time_t tm = wpt->creation_time;
	long utmz;
	double utme, utmn;
	char utmzc;
	
	lonint = abs(wpt->longitude);
	latint = abs(wpt->latitude);

	fprintf(file_out, "BEGIN:VCARD\nVERSION:3.0\n");
	fprintf(file_out, "N:%s;%s;;;\n", wpt->description,wpt->shortname);
	fprintf(file_out, "ORG:%c%d %06.3f %c%d %06.3f\n", wpt->latitude < 0 ? 'S' : 'N',  abs(latint), 60.0 * (fabs(wpt->latitude) - latint), wpt->longitude < 0 ? 'W' : 'E', abs(lonint), 60.0 * (fabs(wpt->longitude) - lonint));

	if (wpt->url) {
		fprintf(file_out, "URL:%s\n", wpt->url);
	}

	fprintf(file_out, "NOTE:");
	vcf_print_utf(&wpt->gc_data.desc_short);
	fprintf(file_out, "\\n");
	vcf_print_utf(&wpt->gc_data.desc_long);
	fprintf(file_out, "\\n\\nHINT:\\n");
	if (encrypt) {
		char *s = rot13(wpt->gc_data.hint);
		vcf_print(s);
		xfree(s);
	} else {
		vcf_print(wpt->gc_data.hint);
	}

	fprintf(file_out, "\nEND:VCARD\n");
}

static void
data_write(void)
{
	setshort_length(mkshort_handle, 6);
	waypt_disp_all(vcf_disp);
}


ff_vecs_t vcf_vecs = {
	ff_type_file,
	NULL,
	wr_init,
	NULL,
	wr_deinit,
	NULL,
	data_write,
	NULL, 
	vcf_args
};
