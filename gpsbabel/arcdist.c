/*
    Distance from point to arc filter

    Copyright (C) 2002 Robert Lipe, robertlipe@usa.net

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
#include <stdio.h>
#include "defs.h"
#include "grtcirc.h"

#define MYNAME "Arc filter"

extern queue waypt_head;

static double pos_dist;
static char *distopt = NULL;
static char *arcfileopt = NULL;
static char *exclopt = NULL;
static char *ptsopt = NULL;

typedef struct {
	double distance;
} extra_data;

static
arglist_t arcdist_args[] = {
	{"file", &arcfileopt,  "File containing vertices of arc", 
		ARGTYPE_FILE | ARGTYPE_REQUIRED},
	{"distance", &distopt, "Maximum distance from arc", 
		ARGTYPE_FLOAT | ARGTYPE_REQUIRED},
	{"exclude", &exclopt, "Exclude points close to the arc", ARGTYPE_BOOL},
	{"points", &ptsopt, "Use distance from vertices not lines", 
		ARGTYPE_BOOL},
	{0, 0, 0, 0}
};

#define BADVAL 999999

void 
arcdist_process(void)
{
	queue * elem, * tmp;
	waypoint * waypointp;
	double dist;
	extra_data *ed;
        double lat1, lon1, lat2, lon2;
	int fileline = 0;

	FILE *arcfile = xfopen( arcfileopt, "r", MYNAME );
	
        lat1 = lon1 = lat2 = lon2 = BADVAL;
	while ( !feof(arcfile)) {
	    char line[200];
	    char *pound = NULL;
	    int argsfound = 0;
	    
	    fgets( line, sizeof(line), arcfile );
	   
	    fileline++;
	    
	    pound = strchr( line, '#' );
	    if ( pound ) *pound = '\0';
	    
	    lat2 = lon2 = BADVAL;
	    argsfound = sscanf( line, "%lf %lf", &lat2, &lon2 );
	   
	    if ( argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
                warning(MYNAME ": Warning: Arc file contains unusable vertex on line %d.\n", fileline );
	    } 
	    else if ( lat2 != BADVAL && lon2 != BADVAL &&
	         (ptsopt || (lat1 != BADVAL && lon1 != BADVAL ))) {
  	      QUEUE_FOR_EACH(&waypt_head, elem, tmp) {

		waypointp = (waypoint *)elem;
		if ( ptsopt ) {
		   dist = gcdist( lat2*M_PI/180.0, lon2*M_PI/180.0, 
				   waypointp->latitude*M_PI/180.0, 
				   waypointp->longitude*M_PI/180.0 );
		}
		else {
		   dist = linedist(lat1, lon1, lat2, lon2, 
				waypointp->latitude,
				waypointp->longitude );
		}

		/* convert radians to float point statute miles */
		dist = tomiles(dist);

		if ( waypointp->extra_data ) {
			ed = (extra_data *) waypointp->extra_data;
		}
		else {
			ed = (extra_data *) xcalloc(1, sizeof(*ed));
			ed->distance = BADVAL;
		}
		if ( ed->distance > dist ) {
			ed->distance = dist;
		}
		waypointp->extra_data = ed;
	      }
	    }
	    lat1 = lat2;
	    lon1 = lon2;
	}
	    
	fclose(arcfile);


	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wp = (waypoint *) elem;
		ed = (extra_data *) wp->extra_data;
		wp->extra_data = NULL;
	        if ( ed ) {
		    if ((ed->distance >= pos_dist) == (exclopt == NULL)) {
			waypt_del(wp);
			waypt_free(wp);
		    }
		    xfree( ed );
		}
	}
}

void
arcdist_init(const char *args) {
	char *fm;

	pos_dist = 0;

	if (distopt) {
		pos_dist = strtod(distopt, &fm);

		if ((*fm == 'k') || (*fm == 'K')) {
			 /* distance is kilometers, convert to feet */
			pos_dist *= .6214;
		}
	}
}

void
arcdist_deinit(void) {
	/* do nothing */
}

filter_vecs_t arcdist_vecs = {
	arcdist_init,
	arcdist_process,
	arcdist_deinit,
	NULL,
	arcdist_args
};
