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


static void crossproduct( double x1, double y1, double z1, 
		   double x2, double y2, double z2,
		   double *xa, double *ya, double *za ) {
    *xa = y1*z2-y2*z1;
    *ya = z1*x2-z2*x1;
    *za = x1*y2-y1*x2;
}

static double dotproduct( double x1, double y1, double z1,
		   double x2, double y2, double z2 ) {
  return (x1*x2+y1*y2+z1*z2);
}
 
static double linedist(double lat1, double lon1,
		double lat2, double lon2,
		double lat3, double lon3 ) {

  static double _lat1 = -9999;
  static double _lat2 = -9999;
  static double _lon1 = -9999;
  static double _lon2 = -9999;
  
  static double x1,y1,z1;
  static double x2,y2,z2;
  static double xa,ya,za,la;
  
  double x3,y3,z3;
  double xp,yp,zp,lp;
 
  double xa1,ya1,za1;
  double xa2,ya2,za2;
  
  double d1, d2;
  double c1, c2;

  double dot;

  int newpoints;
  
  /* degrees to radians */
  lat1 *= M_PI/180.0;  lon1 *= M_PI/180.0;
  lat2 *= M_PI/180.0;  lon2 *= M_PI/180.0;
  lat3 *= M_PI/180.0;  lon3 *= M_PI/180.0;

  newpoints = 1;
  if ( lat1 == _lat1 && lat2 == _lat2 && lon1 == _lon1 && lon2 == _lon2) {
    newpoints = 0;
  }
  else {
    _lat1 = lat1;
    _lat2 = lat2;
    _lon1 = lon1;
    _lon2 = lon2;
  }
  
  /* polar to ECEF rectangular */
  if ( newpoints ) {
    x1 = cos(lon1)*cos(lat1); y1 = sin(lat1); z1 = sin(lon1)*cos(lat1);
    x2 = cos(lon2)*cos(lat2); y2 = sin(lat2); z2 = sin(lon2)*cos(lat2);
  }
  x3 = cos(lon3)*cos(lat3); y3 = sin(lat3); z3 = sin(lon3)*cos(lat3);

  if ( newpoints ) {
  /* 'a' is the axis; the line that passes through the center of the earth
   * and is perpendicular to the great circle through point 1 and point 2 
   * It is computed by taking the cross product of the '1' and '2' vectors.*/
    crossproduct( x1, y1, z1, x2, y2, z2, &xa, &ya, &za );
    la = sqrt(xa*xa+ya*ya+za*za);

    if ( la ) {
      xa /= la;
      ya /= la;
      za /= la;
    }
  }
  if ( la ) {

    /* dot is the component of the length of '3' that is along the axis.
     * What's left is a non-normalized vector that lies in the plane of 
     * 1 and 2. */
	  
    dot = dotproduct(x3,y3,z3,xa,ya,za);

    xp = x3-dot*xa;
    yp = y3-dot*ya;
    zp = z3-dot*za;

    lp = sqrt(xp*xp+yp*yp+zp*zp);

    if ( lp ) {
      xp /= lp;
      yp /= lp;
      zp /= lp;
     
      crossproduct(x1,y1,z1,xp,yp,zp,&xa1,&ya1,&za1); 
      d1 = dotproduct( xa1, ya1, za1, xa, ya, za );

      crossproduct(xp,yp,zp,x2,y2,z2,&xa2,&ya2,&za2);
      d2 = dotproduct( xa2, ya2, za2, xa, ya, za );
     
      if ( d1 >= 0 && d2 >= 0 ) {
          /* rather than call gcdist and all its sines and cosines and
           * worse, we can get the angle directly.  It's the arctangent
           * of the length of the component of vector 3 along the axis 
           * divided by the length of the component of vector 3 in the 
           * plane.  We already have both of those numbers. 
           * 
           * atan2 would be overkill because lp and fabs(dot) are both
           * known to be positive. */
	      
	   return atan( fabs(dot)/lp ); 
      }
      
      /* otherwise, get the distance from the closest endpoint */
      c1 = dotproduct( x1,y1,z1,xp,yp,zp );
      c2 = dotproduct( x2,y2,z2,xp,yp,zp );
      d1 = labs(d1);
      d2 = labs(d2);
      
      /* This is a hack.  d$n$ is proportional to the sine of the angle
       * between point $n$ and point p.  That preserves orderedness up
       * to an angle of 90 degrees.  c$n$ is proportional to the cosine
       * of the same angle; if the angle is over 90 degrees, c$n$ is
       * negative.  In that case, we flop the sine across the y=1 axis
       * so that the resulting value increases as the angle increases. 
       * 
       * This only works because all of the points are on a unit sphere. */

      if ( c1 < 0 ) {
	d1 = 2 - d1;
      }
      if ( c2 < 0 ) {
	d2 = 2 - d2;
      }

      if ( labs(d1) < labs(d2)) {
        return gcdist(lat1,lon1,lat3,lon3);  
      }
      else {
        return gcdist(lat2,lon2,lat3,lon3);
      }
    }
    else {
      /* lp is 0 when 3 is 90 degrees from the great circle */
      return M_PI/2;
    }    
  }
  else {
    /* la is 0 when 1 and 2 are either the same point or 180 degrees apart */
    dot = dotproduct(x1,y1,z1,x2,y2,z2);
    if ( dot >= 0 ) { 
      return gcdist(lat1,lon1,lat3,lon3);
    }
    else {
      return 0;
    }
  }
  return 0;
}

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
                warning(MYNAME ": Warning: Arc file contains unusable vertex on line %d.\n", fileline );
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
