/*
	readgpx.c

	Extract data from MS Streets & Trips .est and Autoroute .axe files in GPX format.

    Copyright (C) 2003 James Sherring, james_sherring@yahoo.com

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


	This app depends on istorage & istorage-make from Pabs (pabs3@zip.to)
	and James Clark's Expat xml parser from http://www.libexpat.org/.

*/

#include <stdio.h>
#include <memory.h>
#include <string.h>

#include <expat.h>
#include "gpx.h"
#include "st2gpx.h"


typedef void (*gpx_elm_start_handler)(void *, const char *, const char **);
typedef void (*gpx_elm_end_handler)(void *, const char *);

int current_main_element=0;
int current_element=0;
static XML_Char * cdata;
static int cdata_length;

//FILE* gpx_in_file=NULL;

char * gpx_elem_name[] = 
{
	"unknown-element",
	"wpt",
	"rte",
	"rtept",
	"trk",
	"trkseg",
	"trkpt",
	"name",
	"desc",
	"src"
};

struct gpxpt * gpxpt_new()
{
	struct gpxpt * nw = (struct gpxpt *)xmalloc(sizeof(struct gpxpt));
	nw->name=NULL;
	nw->desc=NULL;
	nw->lat=0;
	nw->lon=0;
	nw->elevation=0;
	nw->use_elevation=0;
	return nw;
}

void gpxpt_delete(struct gpxpt * pt)
{
	if(pt==NULL)
		return;
	xfree(pt->name);
	xfree(pt->desc);
	xfree(pt);
}

struct gpxpt * gpxpt_copy(struct gpxpt * otherpt)
{
	struct gpxpt * nw = (struct gpxpt *)xmalloc(sizeof(struct gpxpt));
	nw->name=xmalloc(strlen(otherpt->name)+1);
	strcpy(nw->name, otherpt->name);
	nw->desc=xmalloc(strlen(otherpt->desc)+1);
	strcpy(nw->desc, otherpt->desc);
	nw->lat=otherpt->lat;
	nw->lon=otherpt->lon;
	nw->elevation=otherpt->elevation;
	nw->use_elevation=otherpt->use_elevation;
	return nw;
}

struct gpxpt * find_matching_wpt(struct gpx_data * dat, char* name)
{
	int i;
	struct gpxpt * match = NULL;
	for(i=0; i<dat->wpt_list_count; i++)
		if(strcmp(name, dat->wpt_list[i]->name)==0)
		{
			match = dat->wpt_list[i];
			break;
		}
	return match;
}

struct gpxrte * gpxrte_new()
{
	struct gpxrte * nw = (struct gpxrte *)xmalloc(sizeof(struct gpxrte));
	nw->name=NULL;
	nw->rtept_list=NULL;
	nw->rtept_list_count=0;
	return nw;
}

void gpxrte_delete(struct gpxrte * rte)
{
	int i;
	if(rte==NULL)
		return;
	for (i=0; i<rte->rtept_list_count; i++)
		gpxpt_delete(rte->rtept_list[i]);
	xfree(rte->rtept_list);
	xfree(rte->name);
	xfree(rte);
}

struct gpxtrk * gpxtrk_new()
{
	struct gpxtrk * nw = (struct gpxtrk *)xmalloc(sizeof(struct gpxtrk));
	nw->trkpt_list=NULL;
	nw->trkpt_list_count=0;
	return nw;
}

void gpxtrk_delete(struct gpxtrk * trk)
{
	int i;
	if(trk==NULL)
		return;
	for (i=0; i<trk->trkpt_list_count; i++)
		gpxpt_delete(trk->trkpt_list[i]);
	xfree(trk->trkpt_list);
	xfree(trk);
}

struct gpx_data * gpx_data_new()
{
	struct gpx_data * all_data = (struct gpx_data *)xmalloc(sizeof(struct gpx_data));
	all_data->data_source_name=NULL;
	all_data->wpt_list=NULL;
	all_data->wpt_list_count=0;
	all_data->rte_list=NULL;
	all_data->rte_list_count=0;
	all_data->trk_list=NULL;
	all_data->trk_list_count=0;
	return all_data;
}

void gpx_data_delete(struct gpx_data * data)
{
	int i;
	if(data==NULL)
		return;

	xfree(data->data_source_name);
	for (i=0; i<data->wpt_list_count; i++)
		gpxpt_delete(data->wpt_list[i]);
	for (i=0; i<data->rte_list_count; i++)
		gpxrte_delete(data->rte_list[i]);
	for (i=0; i<data->trk_list_count; i++)
		gpxtrk_delete(data->trk_list[i]);
	xfree(data->wpt_list);
	xfree(data->rte_list);
	xfree(data->trk_list);
	xfree(data);
}

void print_wptlist(struct gpxpt ** wpt_list, int wpt_list_count)
{
	int i;
	printf("Global waypoints:\n");
	for (i=0; i<wpt_list_count; i++)
		printf("wpt: lat=%f, lon=%f, name='%s', desc='%s'\n", 
				wpt_list[i]->lat, wpt_list[i]->lon, wpt_list[i]->name, wpt_list[i]->desc);
}

void print_route(struct gpxrte * rte)
{
	int i;
	printf("Route '%s' with %d route-points:\n", rte->name, rte->rtept_list_count);
	for (i=0; i<rte->rtept_list_count; i++)
		printf("rtept: lat=%f, lon=%f, name='%s', desc='%s'\n", 
				(rte->rtept_list)[i]->lat, (rte->rtept_list)[i]->lon,
				(rte->rtept_list)[i]->name, (rte->rtept_list)[i]->desc);
}

void print_route_list(struct gpxrte ** rte_list, int rte_list_count)
{
	int i;
	printf("Global route list with %d routes:\n", rte_list_count);
	for (i=0; i<rte_list_count; i++)
		print_route(rte_list[i]);
}

void print_track(struct gpxtrk * trk)
{
	int i;
	printf("Track with %d track-points:\n", trk->trkpt_list_count);
	for (i=0; i<trk->trkpt_list_count; i++)
		printf("trkpt: lat=%f, lon=%f\n", 
				trk->trkpt_list[i]->lat, trk->trkpt_list[i]->lon);
}

void print_track_list(struct gpxtrk ** trk_list, int trk_list_count)
{
	int i;
	printf("Global track list with %d tracks:\n", trk_list_count);
	for (i=0; i<trk_list_count; i++)
		print_track(trk_list[i]);
}

void print_gpx_data(struct gpx_data * all_data)
{
	print_wptlist(all_data->wpt_list, all_data->wpt_list_count);
	print_route_list(all_data->rte_list, all_data->rte_list_count);
	print_track_list(all_data->trk_list, all_data->trk_list_count);
}

char* get_att(char* match, const char **atts)
{		  
	const char **avp = &atts[0];
	while (*avp) { 
		if (strcmp(avp[0], match) == 0) 
			return (char *)avp[1];
		avp+=2;
	}
	return NULL;
}

void startunkn(void *userData, const char *name, const char **atts)
{
	if(opts.verbose_flag > 3)
		printf("ignoring start element name %s\n", name);
}

void endunkn(void *userData, const char *name)
{
	if(opts.verbose_flag > 3)
		printf("ignoring end element name %s\n", name);
}

void startwpt(void *userData, const char *name, const char **atts)
{
	struct gpx_data * dat = (struct gpx_data *)userData;

	if (current_main_element!=0)
	{
		printf("unexpected <wpt> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_WPT;

	dat->wpt_list=(struct gpxpt **)xrealloc(dat->wpt_list, (dat->wpt_list_count+1)*sizeof(struct gpxpt *));
	dat->wpt_list[dat->wpt_list_count] = gpxpt_new();

	sscanf(get_att("lat", atts), "%lf", &(dat->wpt_list[dat->wpt_list_count]->lat));
	sscanf(get_att("lon", atts), "%lf", &(dat->wpt_list[dat->wpt_list_count]->lon));
	dat->wpt_list_count++;
}

void endwpt(void *userData, const char *name)
{
	if (current_main_element!=GPX_ELEM_TYPE_WPT)
	{
		printf("unexpected </wpt> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=0;
}

void startrte(void *userData, const char *name, const char **atts)
{
	struct gpx_data * dat = (struct gpx_data *)userData;

	if (current_main_element!=0)
	{
		printf("unexpected <rte> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_RTE;

	dat->rte_list=(struct gpxrte **)xrealloc(dat->rte_list, (dat->rte_list_count+1)*sizeof(struct gpxrte *));
	dat->rte_list[dat->rte_list_count]=gpxrte_new();
//	dat->rte_list[dat->rte_list_count]->name="{get route name from a following element}";
	dat->rte_list_count++;
}

void endrte(void *userData, const char *name)
{
	if (current_main_element!=GPX_ELEM_TYPE_RTE)
	{
		printf("unexpected </rte> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=0;
}

void startrtept(void *userData, const char *name, const char **atts)
{
	struct gpx_data * dat = (struct gpx_data *)userData;
	struct gpxrte * thisrte = dat->rte_list[dat->rte_list_count-1];

	if (current_main_element!=GPX_ELEM_TYPE_RTE)
	{
		printf("unexpected <rtept> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_RTEPT;

	thisrte->rtept_list=(struct gpxpt **)xrealloc(thisrte->rtept_list, 
												(thisrte->rtept_list_count+1)*sizeof(struct gpxpt *));
	thisrte->rtept_list[thisrte->rtept_list_count] = gpxpt_new();
	sscanf(get_att("lat", atts), "%lf", &(thisrte->rtept_list[thisrte->rtept_list_count]->lat));
	sscanf(get_att("lon", atts), "%lf", &(thisrte->rtept_list[thisrte->rtept_list_count]->lon));
	thisrte->rtept_list_count++;
}

void endrtept(void *userData, const char *name)
{
	if (current_main_element!=GPX_ELEM_TYPE_RTEPT)
	{
		printf("unexpected </rtept> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_RTE;
}

void starttrk(void *userData, const char *name, const char **atts)
{
	struct gpx_data * dat = (struct gpx_data *)userData;

	if (current_main_element!=0)
	{
		printf("unexpected <trk> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_TRK;

	dat->trk_list=(struct gpxtrk **)xrealloc(dat->trk_list, (dat->trk_list_count+1)*sizeof(struct gpxtrk *));
	dat->trk_list[dat->trk_list_count]=gpxtrk_new();
	dat->trk_list_count++;
}

void endtrk(void *userData, const char *name)
{
//	struct gpx_data * dat = (struct gpx_data *)userData;

	if (current_main_element!=GPX_ELEM_TYPE_TRK)
	{
		printf("unexpected </trk> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=0;

//	printf("read end of track%d, with %d points\n", 
//			dat->trk_list_count,
//			dat->trk_list[dat->trk_list_count-1]->trkpt_list_count); 
}

// just eat <trkseg>: we join all track segments as a single track

void starttrkseg(void *userData, const char *name, const char **atts)
{
	if (current_main_element!=GPX_ELEM_TYPE_TRK)
	{
		printf("unexpected <trkseg> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_TRKSEG;
}

void endtrkseg(void *userData, const char *name)
{
	if (current_main_element!=GPX_ELEM_TYPE_TRKSEG)
	{
		printf("unexpected </trkseq> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_TRK;
}

void starttrkpt(void *userData, const char *name, const char **atts)
{
	struct gpx_data * dat = (struct gpx_data *)userData;
	struct gpxtrk * thistrk = dat->trk_list[dat->trk_list_count-1];

	if (current_main_element!=GPX_ELEM_TYPE_TRKSEG)
	{
		printf("unexpected <trkpt> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_TRKPT;

	thistrk->trkpt_list=(struct gpxpt **)xrealloc(thistrk->trkpt_list, 
												(thistrk->trkpt_list_count+1)*sizeof(struct gpxpt *));
	thistrk->trkpt_list[thistrk->trkpt_list_count]=gpxpt_new();
	sscanf(get_att("lat", atts), "%lf", &(thistrk->trkpt_list[thistrk->trkpt_list_count]->lat));
	sscanf(get_att("lon", atts), "%lf", &(thistrk->trkpt_list[thistrk->trkpt_list_count]->lon));
	thistrk->trkpt_list_count++;
}

void endtrkpt(void *userData, const char *name)
{
	if (current_main_element!=GPX_ELEM_TYPE_TRKPT)
	{
		printf("unexpected </trkpt> element\n");
		debug_pause();
		exit(1);
	}
	current_main_element=GPX_ELEM_TYPE_TRKSEG;
}

void startname(void *userData, const char *name, const char **atts)
{
}

void endname(void *userData, const char *name)
{
	struct gpx_data * dat = (struct gpx_data *)userData;
	struct gpxrte* thisrte;
//	struct gpxtrk* thistrk;
	char* nameval = xrealloc(cdata, (cdata_length+1)*sizeof(XML_Char));
	nameval[cdata_length]=0;
	cdata=NULL;
	cdata_length=0;
	str2ascii(nameval);
	switch (current_main_element)
	{
	case GPX_ELEM_TYPE_WPT:
		dat->wpt_list[dat->wpt_list_count-1]->name=nameval;
		break;
	case GPX_ELEM_TYPE_RTE:
		dat->rte_list[dat->rte_list_count-1]->name=nameval;
		break;
	case GPX_ELEM_TYPE_RTEPT:
		thisrte = dat->rte_list[dat->rte_list_count-1];
		thisrte->rtept_list[thisrte->rtept_list_count-1]->name=nameval;
		break;
	case GPX_ELEM_TYPE_TRK:
//		thistrk = dat->trk_list[dat->trk_list_count-1];
//		thistrk->trkpt_list[thistrk->trkpt_list_count-1]->name=nameval;
		//break;
	case GPX_ELEM_TYPE_TRKSEG:
		//break;
	case GPX_ELEM_TYPE_TRKPT:
	default:
		xfree(nameval);
		break;
	}
}

void startdesc(void *userData, const char *name, const char **atts)
{
}

void enddesc(void *userData, const char *name)
{
	struct gpx_data * dat = (struct gpx_data *)userData;
//	struct gpxrte* thisrte;
//	struct gpxtrk* thistrk;
	char* desc = xrealloc(cdata, cdata_length+1);
	desc[cdata_length]=0;
	cdata=NULL;
	cdata_length=0;
	str2ascii(desc);
	switch (current_main_element)
	{
	case GPX_ELEM_TYPE_WPT:
		str2ascii(desc);
		dat->wpt_list[dat->wpt_list_count-1]->desc=desc;
		break;
	case GPX_ELEM_TYPE_RTE:
//		dat->rte_list[dat->rte_list_count-1]->desc=desc;
//		break;
	case GPX_ELEM_TYPE_RTEPT:
//		thisrte = dat->rte_list[dat->rte_list_count-1];
//		thisrte->rtept_list[thisrte->rtept_list_count-1]->desc=desc;
//		break;
	case GPX_ELEM_TYPE_TRK:
//		thistrk = dat->trk_list[dat->trk_list_count-1];
//		thistrk->trkpt_list[thistrk->trkpt_list_count-1]->desc=desc;
//		break;
	case GPX_ELEM_TYPE_TRKSEG:
//		break;
	case GPX_ELEM_TYPE_TRKPT:
	default:
		xfree(desc);
		break;
	}
}

void startsrc(void *userData, const char *name, const char **atts)
{
}

void endsrc(void *userData, const char *name)
{
}

#define GPX_NUM_ELEM_HANDLERS 10

gpx_elm_start_handler gpx_start_elm_handler[] = 
{
	&startunkn,
	&startwpt,
	&startrte,
	&startrtept,
	&starttrk,
	&starttrkseg,
	&starttrkpt,
	&startname,
	&startdesc,
	&startsrc
};

gpx_elm_end_handler gpx_end_elm_handler[] = 
{
	&endunkn,
	&endwpt,
	&endrte,
	&endrtept,
	&endtrk,
	&endtrkseg,
	&endtrkpt,
	&endname,
	&enddesc,
	&endsrc
};


int get_gpx_type_ndx(const char* type_name)
{
	int i;
	for (i=1; i<GPX_NUM_ELEM_HANDLERS; i++)
		if(strcmp(type_name, gpx_elem_name[i])==0)
			return i;
	return 0;
}

static void XMLCALL
startElement(void *userData, const char *name, const char **atts)
{
	int i = get_gpx_type_ndx(name);
	gpx_start_elm_handler[i](userData, name, atts);

	xfree(cdata);
	cdata=NULL;
	cdata_length=0;

}

static void XMLCALL
endElement(void *userData, const char *name)
{
	int i;
  i = get_gpx_type_ndx(name);
  gpx_end_elm_handler[i](userData, name);
}


static void XMLCALL
CharacterData(void *userData, const XML_Char *s, int len)
{
	// Remember s is not null-terminated!
	// cdata is reset to NULL at the start of every element,
	// which means we lose any cdata split by elements.
	// Could be more clever and only copy cdata for types we want

	cdata = xrealloc(cdata, (cdata_length+len)*sizeof(XML_Char));
	memcpy(cdata+cdata_length, s, len);
	cdata_length += len;
}

struct gpx_data * process_gpx_in_file(char* gpx_in_file_name)
{

	char buf[BUFSIZ];
	XML_Parser parser = XML_ParserCreate(NULL);
	int done;
	FILE* gpx_in_file;

	struct gpx_data * all_data = gpx_data_new();

	all_data->data_source_name=(char*)xmalloc(strlen(gpx_in_file_name)+1);
	strcpy(all_data->data_source_name, gpx_in_file_name);

	XML_SetUserData(parser, all_data);
	XML_SetElementHandler(parser, startElement, endElement);
	XML_SetCharacterDataHandler(parser, CharacterData);

 	if(gpx_in_file_name==NULL)
	{
		fprintf(stderr, "Unexpected null filename for gpx_in_file_name\n");
		debug_pause();
		exit(1);
	}
	else
	{
		printf("Importing data from %s\n", gpx_in_file_name);
		gpx_in_file = fopen(gpx_in_file_name, "r");
	}
 
 	if(gpx_in_file==NULL)
	{
		fprintf(stderr, "Unable to open GPX file %s\n", gpx_in_file_name);
		debug_pause();
		exit(1);
	}

	do 
	{
		size_t len = fread(buf, 1, sizeof(buf), gpx_in_file);
		done = len < sizeof(buf);
		if (XML_Parse(parser, buf, len, done) == XML_STATUS_ERROR) 
		{
			fprintf(stderr,
				"%s at line %d\n",
				XML_ErrorString(XML_GetErrorCode(parser)),
				XML_GetCurrentLineNumber(parser));
			return all_data;
		}
	} while (!done);
	XML_ParserFree(parser);

	if (opts.verbose_flag > 2)
	{
	}

	xfree(cdata);

	return all_data;
}
