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
#include <math.h>
#include "defs.h"

#ifndef M_PI
#  define M_PI 3.14159265358979323846
#endif

#define MYNAME "Arc filter"

extern queue waypt_head;

static double pos_dist;
static char *distopt = NULL;
static char *arcfileopt = NULL;
static char *exclopt = NULL;

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
	{0, 0, 0, 0}
};

static double gcdist( double lat1, double lon1, double lat2, double lon2 ) {
  return acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon1-lon2));
}

 
static double linedist(double lat1, double lon1,
		double lat2, double lon2,
		double lat3, double lon3 ) {
  double lat4, lon4;

  double x1,y1,z1;
  double x2,y2,z2;
  double x3,y3,z3;

  double xa,ya,za,la;

  double xp,yp,zp,lp;

  double dot;

  double d12, d1p, dp2;

  double dist;

  /* degrees to radians */
  lat1 *= M_PI/180.0;  lon1 *= M_PI/180.0;
  lat2 *= M_PI/180.0;  lon2 *= M_PI/180.0;
  lat3 *= M_PI/180.0;  lon3 *= M_PI/180.0;

  /* polar to ECEF rectangular */
  x1 = cos(lon1)*cos(lat1); y1 = sin(lat1); z1 = sin(lon1)*cos(lat1);
  x2 = cos(lon2)*cos(lat2); y2 = sin(lat2); z2 = sin(lon2)*cos(lat2);
  x3 = cos(lon3)*cos(lat3); y3 = sin(lat3); z3 = sin(lon3)*cos(lat3);

  /* 'a' is the axis; the line that passes through the center of the earth
   * and is perpendicular to the great circle through point 1 and point 2 
   * It is computed by taking the cross product of the '1' and '2' vectors.*/
  xa = y1*z2-y2*z1;
  ya = z1*x2-z2*x1;
  za = x1*y2-y1*x2;
  la = sqrt(xa*xa+ya*ya+za*za);

  if ( la ) {
    xa /= la;
    ya /= la;
    za /= la;

    /* dot is the component of the length of '3' that is along the axis.
     * What's left is a non-normalized vector that lies in the plane of 
     * 1 and 2. */
    dot = x3*xa+y3*ya+z3*za;

    xp = x3-dot*xa;
    yp = y3-dot*ya;
    zp = z3-dot*za;

    lp = sqrt(xp*xp+yp*yp+zp*zp);

    if ( lp ) {
      xp /= lp;
      yp /= lp;
      zp /= lp;

      /* convert the point 'p' (the closest point to '3' on the great circle
       * that passest through '1' and '2') back to lat/lon */
      lat4 = asin( yp );
      lon4 = atan2( zp/cos(lat4), xp/cos(lat4));

      /* compute a few distances */
      d12 = gcdist(lat1,lon1,lat2,lon2);
      d1p = gcdist(lat1,lon1,lat4,lon4);
      dp2 = gcdist(lat4,lon4,lat2,lon2);
      
      if ( d12 >= d1p && d12 >= dp2 ) {
	/* if 'p' is closer to both 1 and 2 than 1 is to 2, p is between. */
        dist = gcdist(lat3,lon3,lat4,lon4);
      }
      else {
	/* otherwise, get the distance from the closest endpoint */
	if ( d1p < dp2 ) {
          dist = gcdist(lat1,lon1,lat3,lon3);
	}
	else {
          dist = gcdist(lat2,lon2,lat3,lon3);
	}
      }
    }
    else {
      /* lp is 0 when 3 is 90 degrees from the great circle */
      dist = M_PI/2;
    }    
  }
  else {
    /* la is 0 when 1 and 2 are either the same point or 180 degrees apart */
    d12 = gcdist(lat1,lon1,lat2,lon2);
    if ( d12 < 1 ) { 
      /* less than a radian : same point */
      dist = gcdist(lat1,lon1,lat3,lon3);
    }
    else {
      /* more than a radian : 180 degrees apart; there's a great circle that
       * goes through all 3 points, so the distance is 0 */
      dist = 0;
    }
  }
  return dist;
}

#define BADVAL 999999

void 
arcdist_process(void)
{
	queue * elem, * tmp;
	waypoint * waypointp;
	double dist;
	waypoint ** comp;
	int i, wc;
	queue temp_head;
	extra_data *ed;
        double lat1, lon1, lat2, lon2;
	int fileline = 0;

	FILE *arcfile = fopen( arcfileopt, "r" );
	if ( arcfile == NULL ) {
		fatal(MYNAME ": Can't open %s for reading.\n",arcfileopt); 
	}
	
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
                fprintf( stderr, "%s: Warning: Arc file contains unusable vertex on line %d.\n", MYNAME, fileline );
	    } 
	    else if ( lat1 != BADVAL && lon1 != BADVAL &&
	         lat2 != BADVAL && lon2 != BADVAL ) {
  	      QUEUE_FOR_EACH(&waypt_head, elem, tmp) {

		waypointp = (waypoint *)elem;
		dist = linedist(lat1, lon1, lat2, lon2, 
				waypointp->position.latitude.degrees,
				waypointp->position.longitude.degrees );

		/* convert radians to float point statute miles */
		dist = (((dist * 180.0 * 60.0) / M_PI) * 1.1516);

		if ( waypointp->extra_data ) {
			ed = waypointp->extra_data;
		}
		else {
			ed = xcalloc(1, sizeof(*ed));
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
		ed = wp->extra_data;
		wp->extra_data = NULL;
	        if ( ed ) {
		    if ((ed->distance >= pos_dist) == (exclopt == NULL)) {
			waypt_del(wp);
			waypt_free(wp);
			continue;
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
	arcdist_args
};
