/* 

	Support for IGN Rando track files, 
	
	Copyright (C) 2005 Olaf Klein, o.b.klein@t-online.de

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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "defs.h"
#include "xmlgeneric.h"

#if !NO_EXPAT
#include <expat.h>
#endif

#define MYNAME "IGNRando"

static FILE *fout;

static route_head *track;
static waypoint *wpt;
static int track_index;		/* index of track we'll write */
static int track_num;		/* current index of track within track_disp_all */

static int xmlpoints;

/* options */
static char *index_opt = NULL;

static arglist_t ignr_args[] = 
{
	{"index", &index_opt, "Index of track to write (if more the one in source)", NULL, ARGTYPE_INT, "1", NULL },
	ARG_TERMINATOR
};

static xg_callback	ignr_start;
static xg_callback	ignr_nb_etapes, ignr_descr;
static xg_callback	ignr_etape_begin, ignr_etape_end;
static xg_callback	ignr_etape_pos, ignr_etape_alt;

static 
xg_tag_mapping ignr_xml_map[] = 
{
	{ ignr_start,		cb_start,	"/RANDONNEE" },
	{ ignr_nb_etapes,   	cb_cdata,	"/RANDONNEE/INFORMATIONS/NB_ETAPES" },
	{ ignr_descr,   	cb_cdata,	"/RANDONNEE/INFORMATIONS/DESCRIPTION" },
	{ ignr_etape_begin, 	cb_start, 	"/RANDONNEE/ETAPE" },
	{ ignr_etape_end, 	cb_end, 	"/RANDONNEE/ETAPE" },
	{ ignr_etape_pos,	cb_cdata,	"/RANDONNEE/ETAPE/POSITION" },
	{ ignr_etape_alt,	cb_cdata,	"/RANDONNEE/ETAPE/ALTITUDE" },
	{ NULL, 		0, 		NULL }
};

#if NO_EXPAT

static void
ignr_rd_init(const char *fname)
{
	fatal(MYNAME ": This build excluded \"" MYNAME "\" input support because expat was not installed.\n");
}

static void
ignr_read(void)
{
}

static void
ignr_rd_deinit(void)
{
}

#else

static void
ignr_xml_error(int condition)
{
	if (condition != 0)
		fatal(MYNAME ": Error in XML structure!\n");
}

/* xmlgeneric callbacks */

static xg_callback	ignr_start;
static xg_callback	ignr_nb_etapes, ignr_descr;
static xg_callback	ignr_etape_begin, ignr_etape_end;

static void 
ignr_start(const char *args, const char **attrv)
{
	ignr_xml_error((track != NULL));
	
	track = route_head_alloc();
	track_add_head(track);
}

static void 
ignr_nb_etapes(const char *args, const char **attrv)
{
	xmlpoints = atoi(args);
}

static void 
ignr_descr(const char *args, const char **attrv)
{
	ignr_xml_error((track == NULL));

	if ((args != NULL) && (strlen(args) > 0))
		track->rte_desc = xstrdup(args);
}

static void 
ignr_etape_begin(const char *args, const char **attrv)
{
	ignr_xml_error((wpt != NULL));
	
	wpt = waypt_new();
}

static void 
ignr_etape_end(const char *args, const char **attrv)
{
	ignr_xml_error((track == NULL) || (wpt == NULL));
	
	route_add_wpt(track, wpt);
	wpt = NULL;
}

static void
ignr_etape_pos(const char *args, const char **attrv)
{
	ignr_xml_error((wpt == NULL) || (args == NULL));

	if (2 != sscanf(args, "%lf,%lf", &wpt->latitude, &wpt->longitude))
				fatal(MYNAME ": Invalid coordinates \"%s\"!\n", args);
}

static void
ignr_etape_alt(const char *args, const char **attrv)
{
	ignr_xml_error((wpt == NULL));
	if (args == NULL) return;
	
	if (1 != sscanf(args, "%lf", &wpt->altitude))
		fatal(MYNAME ": Invalid altitude \"%s\"!\n", args);
}

/* callbacks registered in ignr_vecs */

static void 
ignr_rd_init(const char *fname)
{
	xml_init(fname, ignr_xml_map, NULL);
	wpt = NULL;
	track = NULL;
}

static void 
ignr_rd_deinit(void)
{
	xml_deinit();	
}

static void 
ignr_read(void)
{
	xml_read();
}

#endif

/* write support */

static void
ignr_fprintf(FILE *f, const char *fmt, ...)
{
	char buff[256];
	char *temp = buff;
	va_list args;
	int i;
	
	va_start(args, fmt);
	
	i = vsnprintf(buff, sizeof(buff), fmt, args);
	if (i >= sizeof(buff))
	{
		temp = xmalloc(i + 1);
		i = vsnprintf(temp, i + 1, fmt, args);
	}
	if (i < 0)
	{
		fatal(MYNAME ": error in vsnprintf.\n");	
	}
	else if (i > 0)
	{
		char eol = temp[i - 1];
		if (eol == '\n') i--;
		fwrite(temp, 1, i, f);
		if (eol == '\n') fprintf(f, "\r\n");
	}
	
	if (temp != buff) xfree(temp);
	va_end(args);
}

/* callbacks registered in ignr_vecs */

static void 
ignr_rw_init(const char *fname)
{
	fout = xfopen(fname, "wb", MYNAME);
}

static void 
ignr_rw_deinit(void)
{
	fclose(fout);
}

static void
ignr_write_track_hdr(const route_head *track)
{
	track_num++;

	if (track_num != track_index) return;
	
	ignr_fprintf(fout, "\t<INFORMATIONS>\n");
	ignr_fprintf(fout, "\t\t<NB_ETAPES>%d</NB_ETAPES>\n", track->rte_waypt_ct);
	if (track->rte_desc != NULL)
		ignr_fprintf(fout, "\t\t<DESCRIPTION>%s</DESCRIPTION>\n", track->rte_desc);
	ignr_fprintf(fout, "\t</INFORMATIONS>\n");
}

static void
ignr_write_track_trl(const route_head *track)
{
}

static void
ignr_write_waypt(const waypoint *wpt)
{
	if (track_num != track_index) return;

	ignr_fprintf(fout, "\t<ETAPE>\n");
	ignr_fprintf(fout, "\t\t<POSITION>%3.6f,%3.6f</POSITION>\n", wpt->latitude, wpt->longitude);
	if (wpt->altitude != unknown_alt)
		ignr_fprintf(fout, "\t\t<ALTITUDE>%3.6f</ALTITUDE>\n", wpt->altitude);
	ignr_fprintf(fout, "\t</ETAPE>\n");
}

static void 
ignr_write(void)
{
	time_t now;
	struct tm tm;
	char buff[32];
	
	if (index_opt != NULL)
	{
		track_index = atoi(index_opt);
		if ((track_index < 1) || (track_index > (int) track_count()))
		    fatal(MYNAME ": Invalid track index %d (we have currently %d track(s))!\n", 
			track_index, track_count());
	}	    
	else
		track_index = 1;
	track_num = 0;

        now = current_time();
	tm = *localtime(&now);
	
	ignr_fprintf(fout, "<?xml version=\"1.0\" encoding=\"windows-1252\"?>\n");
	ignr_fprintf(fout, "<RANDONNEE>\n");
	ignr_fprintf(fout, "\t<ENTETE>\n");
	ignr_fprintf(fout, "\t\t<VERSION_XML>1.1</VERSION_XML>\n");
	ignr_fprintf(fout, "\t\t<VERSION_BASE>IHA03AA</VERSION_BASE>\n");
	
	strftime(buff, sizeof(buff), "%d/%m/%Y", &tm);
	ignr_fprintf(fout, "\t\t<DATE>%s</DATE>\n", buff);
	strftime(buff, sizeof(buff), "%H:%M:%S", &tm);
	ignr_fprintf(fout, "\t\t<HEURE>%s</HEURE>\n", buff);
	
	ignr_fprintf(fout, "\t</ENTETE>\n");
	track_disp_all(ignr_write_track_hdr, ignr_write_track_trl, ignr_write_waypt);
	ignr_fprintf(fout, "</RANDONNEE>\n");
}

ff_vecs_t ignr_vecs = {
	ff_type_file,
	{ ff_cap_none, ff_cap_read | ff_cap_write, ff_cap_none },
	ignr_rd_init,	
	ignr_rw_init,	
	ignr_rd_deinit,
	ignr_rw_deinit,
	ignr_read,
	ignr_write,
	NULL, 
	ignr_args,
	CET_CHARSET_MS_ANSI, 1
};
