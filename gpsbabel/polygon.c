/*
    Inside/Outside polygon filter
   
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
#include "defs.h"

#define MYNAME "Polygon filter"

extern queue waypt_head;

static char *polyfileopt = NULL;
static char *exclopt = NULL;

#define OUTSIDE     0
#define INSIDE      1
#define LIMBO       2
#define LIMBO_UP    4
#define LIMBO_BEGIN 8
#define BEGIN_UP    16
#define BEGIN_HOR   32
#define UP          64

typedef struct {
	unsigned short state;
} extra_data;

static
arglist_t polygon_args[] = {
	{"file", &polyfileopt,  "File containing vertices of polygon",
		ARGTYPE_FILE | ARGTYPE_REQUIRED },
	{"exclude", &exclopt, "Exclude points inside the polygon",
		ARGTYPE_BOOL },
	{0, 0, 0, 0}
};

static void polytest ( double lat1, double lon1,
		double lat2, double lon2,
		double lat3, double lon3,
		unsigned short *state, int first, int last ) {
	
	if ( lat1 == lat3 ) {
	    if ( lat2 < lat3 ) {
		/* going down */
		if (*state & LIMBO) {
		    if ( *state & LIMBO_UP ) {
			*state = *state ^ INSIDE;
		    }
		    *state = *state & ~LIMBO &~LIMBO_UP;
		}
		else if (*state & LIMBO_BEGIN) {
		    if ( *state & BEGIN_HOR ) {
			*state = *state & ~BEGIN_HOR;
		    }
		    else if ( last ) {
			if ( *state & BEGIN_UP ) {
			    *state = *state ^ INSIDE;
			}
			*state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
		    }
		}
	        else if ( first && (lon1 > lon3)) {
		    *state |= LIMBO_BEGIN;
		}
	    }
	    else if ( lat2 == lat3 ) {
		if ( first & (lon1 > lon3 || lon2 > lon3)) {
		    *state |= LIMBO_BEGIN | BEGIN_HOR;
		}
		else if (last && (*state & LIMBO_BEGIN) && (*state & LIMBO)) {
		    if ( (!!(*state & LIMBO_UP)) != (!!(*state & BEGIN_UP))) {
		        *state = *state ^ INSIDE;
		    }
                    *state = *state & ~LIMBO & ~LIMBO_UP & 
			     ~LIMBO_BEGIN & ~BEGIN_UP;    
		}
		else if ( *state & LIMBO ) {
		    /* do nothing */
		}
		else {
	            if ( lon1 <= lon3 && lon2 > lon3 ) {
		        if ( *state & UP ) {
			    *state = *state | LIMBO_UP & ~UP;
			}
			*state = *state | LIMBO;
	            }
		}
	    }
	    else {
		/* going up */
		if (*state & LIMBO) {
		    if ( !(*state & LIMBO_UP) ) {
			*state = *state ^ INSIDE;
		    }
		    *state = *state & ~LIMBO & ~LIMBO_UP;
		}
		else if (*state & LIMBO_BEGIN) {
		    if ( *state & BEGIN_HOR ) {
			*state = *state & ~BEGIN_HOR | BEGIN_UP;
		    }
		    else if ( last ) {
			if ( !(*state & BEGIN_UP) ) {
			    *state = *state ^ INSIDE;
			}
			*state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
		    }
		}
		else if ( first && (lon1 > lon3)) {
		    *state |= LIMBO_BEGIN | BEGIN_UP;
        	}
	    }
	    *state = *state & ~UP;
	}
	else if ( lat2 == lat3 ) {
	    if ( lat1 < lat3 ) {
		if ( last ) {
		    if ( *state & BEGIN_UP ) {
			*state = *state ^ INSIDE;
		    }
		    *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
		}
		else if ( lon2 > lon3 ) {
		    *state |= LIMBO;
		}
	    }
	    /* no case for lat1==lat3; that's above */
	    else {
		if ( last ) {
		    if ( !(*state & BEGIN_UP) ) {
			*state = *state ^ INSIDE;
		    }
		    *state = *state & ~LIMBO_BEGIN & ~BEGIN_UP;
		}
		else if ( lon2 > lon3 ) {
		    *state |= LIMBO | LIMBO_UP;
		}
		else {
		    *state |= UP;
		}
	    }
	}
	else {
	    if ( (lat1 > lat3 && lat2 < lat3) ||
	         (lat1 < lat3 && lat2 > lat3)) {
		/* we only care if the lines might intersect */
		if ( lon1 > lon3 && lon2 > lon3 ) {
		    *state = *state ^ INSIDE;
		}
		else if (!(lon1 <= lon3 && lon2 <= lon3)) {
		    /* we're inside the bbox of a diagonal line.  math time. */
		    double loni = (lon2-lon1)/(lat2-lat1)*(lat3-lat1);
		    if ( loni > lon3 ) {
		        *state = *state ^ INSIDE;
		    }
		}
	    }
	}

}

#define BADVAL 999999

void 
polygon_process(void)
{
	queue * elem, * tmp;
	waypoint * waypointp;
	extra_data *ed;
        double lat1, lon1, lat2, lon2;
	double olat, olon;
	int fileline = 0;
	int first = 1;
	int last = 0;

	FILE *polyfile = xfopen( polyfileopt, "r", MYNAME );
	
        olat = olon = lat1 = lon1 = lat2 = lon2 = BADVAL;
	while ( !feof(polyfile)) {
	    char line[200];
	    char *pound = NULL;
	    int argsfound = 0;
	    
	    fgets( line, sizeof(line), polyfile );
	   
	    fileline++;
	    
	    pound = strchr( line, '#' );
	    if ( pound ) *pound = '\0';
	    
	    lat2 = lon2 = BADVAL;
	    argsfound = sscanf( line, "%lf %lf", &lat2, &lon2 );
	   
	    if ( argsfound != 2 && strspn(line, " \t\n") < strlen(line)) {
                warning(MYNAME 
		    ": Warning: Polygon file contains unusable vertex on line %d.\n", 
		    fileline );
	    } 
	    else if ( lat1 != BADVAL && lon1 != BADVAL &&
	         lat2 != BADVAL && lon2 != BADVAL ) {
  	      QUEUE_FOR_EACH(&waypt_head, elem, tmp) {

		waypointp = (waypoint *)elem;
		if ( waypointp->extra_data ) {
		    ed = waypointp->extra_data;
		}
		else {
		    ed = xcalloc(1, sizeof(*ed));
		    ed->state = OUTSIDE;
		    waypointp->extra_data = ed;
		}
	        if ( olat != BADVAL && olon != BADVAL &&
	            olat == lat2 && olon == lon2 ) {
		    last = 1;
		}
		polytest( lat1, lon1, lat2, lon2, 
			waypointp->latitude,
			waypointp->longitude,
		       	&ed->state, first, last );
		first = 0;
		last = 0;
	      }
	    }
	    if ( olat != BADVAL && olon != BADVAL &&
	         olat == lat2 && olon == lon2 ) {
		olat = BADVAL;
		olon = BADVAL;
		lat1 = BADVAL;
		lon1 = BADVAL;
		first = 1;
	    }
	    else if ( lat1 == BADVAL || lon1 == BADVAL ) {
		olat = lat2;
		olon = lon2;
		lat1 = lat2;
		lon1 = lon2;
	    }
	    else {
	        lat1 = lat2;
	        lon1 = lon2;
	    }
	}
	    
	fclose(polyfile);


	QUEUE_FOR_EACH(&waypt_head, elem, tmp) {
		waypoint *wp = (waypoint *) elem;
		ed = wp->extra_data;
		wp->extra_data = NULL;
	        if ( ed ) {
		    if (((ed->state & INSIDE) == OUTSIDE ) == (exclopt == NULL)) {
			waypt_del(wp);
			waypt_free(wp);
		    }
		    xfree( ed );
		}
	}
}

void
polygon_init(const char *args) {
	/* do nothing */
}

void
polygon_deinit(void) {
	/* do nothing */
}

filter_vecs_t polygon_vecs = {
	polygon_init,
	polygon_process,
	polygon_deinit,
	polygon_args
};
