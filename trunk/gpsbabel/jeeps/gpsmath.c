/********************************************************************
** @source JEEPS arithmetic/conversion functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0 
** @modified Dec 28 1999 Alan Bleasby. First version
** @@
** 
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
** 
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
********************************************************************/
#include "gps.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "gpsdatum.h"



static int32 GPS_Math_LatLon_To_UTM_Param(double lat, double lon, int32 *zone,
					  char *zc, double *Mc, double *E0,
					  double *N0, double *F0);
static int32 GPS_Math_UTM_Param_To_Mc(int32 zone, char zc, double *Mc,
				      double *E0, double *N0, double *F0);



/* @func GPS_Math_Deg_To_Rad *******************************************
**
** Convert degrees to radians
**
** @param [r] v [double] degrees
**
** @return [double] radians
************************************************************************/

double GPS_Math_Deg_To_Rad(double v)
{
    return v*(double)((double)GPS_PI/(double)180.);
}



/* @func GPS_Math_Rad_To_Deg *******************************************
**
** Convert radians to degrees
**
** @param [r] v [double] radians
**
** @return [double] degrees
************************************************************************/

double GPS_Math_Rad_To_Deg(double v)
{
    return v*(double)((double)180./(double)GPS_PI);
}



/* @func GPS_Math_Deg_To_DegMin *****************************************
**
** Convert degrees to degrees and minutes
**
** @param [r] v [double] degrees
** @param [w] d [int32 *]  whole degrees
** @param [w] m [double *] minutes
**
** @return [void]
************************************************************************/

void GPS_Math_Deg_To_DegMin(double v, int32 *d, double *m)
{
    int32 sign;
    
    if(v<(double)0.)
    {
	v *= (double)-1.;
	sign = 1;
    }
    else
	sign = 0;
    
    *d = (int32)v;
    *m = (v-(double)*d) * (double)60.0;
    if(*m>(double)59.999)
    {
	++*d;
	*m = (double)0.0;
    }

    if(sign)
	*d = -*d;

    return;
}



/* @func GPS_Math_DegMin_To_Deg *****************************************
**
** Convert degrees and minutes to degrees
**
** @param [r] d [int32] whole degrees
** @param [r] m [double] minutes
** @param [w] deg [double *] degrees
**
** @return [void]
************************************************************************/

void GPS_Math_DegMin_To_Deg(int32 d, double m, double *deg)
{

    *deg = ((double)abs(d)) + m / (double)60.0;
    if(d<0)
	*deg = -*deg;

    return;
}



/* @func GPS_Math_Deg_To_DegMinSec *************************************
**
** Convert degrees to degrees, minutes and seconds
**
** @param [r] v [double] degrees
** @param [w] d [int32 *]  whole degrees
** @param [w] m [int32 *]  whole minutes
** @param [w] s [double *] seconds
**
** @return [void]
************************************************************************/

void GPS_Math_Deg_To_DegMinSec(double v, int32 *d, int32 *m, double *s)
{
    int32 sign;
    double t;
    
    if(v<(double)0.)
    {
	v *= (double)-1.;
	sign = 1;
    }
    else
	sign = 0;
    
    *d = (int32)v;
    t  = (v -(double)*d) * (double)60.0;
    *s = (t-(double)*m) * (double)60.0;

    if(*s>(double)59.999)
    {
	++t;
	*s = (double)0.0;
    }

    
    if(t>(double)59.999)
    {
	++*d;
	t = 0;
    }

    *m = (int32)t;

    if(sign)
	*d = -*d;

    return;
}



/* @func GPS_Math_DegMinSec_To_Deg *****************************************
**
** Convert degrees, minutes and seconds to degrees
**
** @param [r] d [int32] whole degrees
** @param [r] m [int32] whole minutes
** @param [r] s [double] seconds
** @param [w] deg [double *] degrees
**
** @return [void]
************************************************************************/

void GPS_Math_DegMinSec_To_Deg(int32 d, int32 m, double s, double *deg)
{

    *deg = ((double)abs(d)) + ((double)m + s / (double)60.0) / (double)60.0;
    if(d<0)
	*deg = -*deg;

    return;
}



/* @func GPS_Math_Metres_To_Feet *******************************************
**
** Convert metres to feet 
**
** @param [r] v [double] metres
**
** @return [double] feet
************************************************************************/

double GPS_Math_Metres_To_Feet(double v)
{
    return v/0.3048;
}



/* @func GPS_Math_Feet_To_Metres *******************************************
**
** Convert feet to metres
**
** @param [r] v [double] feet
**
** @return [double] metres
************************************************************************/

double GPS_Math_Feet_To_Metres(double v)
{
    return v * 0.3048;
}



/* @func GPS_Math_Deg_To_Semi *******************************************
**
** Convert degrees to semi-circles
**
** @param [r] v [double] degrees
**
** @return [int32] semicircles
************************************************************************/

int32 GPS_Math_Deg_To_Semi(double v)
{
    return ((1U<<31) / 180) * v;
}



/* @func GPS_Math_Semi_To_Deg *******************************************
**
** Convert semi-circles to degrees
**
** @param [r] v [int32] semi-circles
**
** @return [double] degrees
************************************************************************/

double GPS_Math_Semi_To_Deg(int32 v)
{
    return (double) (((double)v/(double)(1U<<31)) * (double)180);
}



/* @func GPS_Math_Utime_To_Gtime *******************************************
**
** Convert Unix time (1970) to GPS time (1990)
**
** @param [r] v [time_t] Unix time
**
** @return [time_t] GPS time
************************************************************************/

time_t GPS_Math_Utime_To_Gtime(time_t v)
{
    return v-631065600;
}



/* @func GPS_Math_Gtime_To_Utime *******************************************
**
** Convert GPS time (1990) to Unix time (1970)
**
** @param [r] v [time_t] GPS time
**
** @return [time_t] Unix time
************************************************************************/

time_t GPS_Math_Gtime_To_Utime(time_t v)
{
    return v+631065600;
}




/* @func GPS_Math_LatLonH_To_XYZ **********************************
**
** Convert latitude and longitude and ellipsoidal height to
** X, Y & Z coordinates
**
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] H [double] ellipsoidal height (metres)
** @param [w] x [double *] X
** @param [w] y [double *] Y
** @param [w] z [double *] Z
** @param [r] a [double] semi-major axis (metres)
** @param [r] b [double] semi-minor axis (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_LatLonH_To_XYZ(double phi, double lambda, double H,
			     double *x, double *y, double *z,
			     double a, double b)
{
    double esq;
    double nu;

    phi    = GPS_Math_Deg_To_Rad(phi);
    lambda = GPS_Math_Deg_To_Rad(lambda);
    

    esq   = ((a*a)-(b*b)) / (a*a);
    
    nu    = a / pow(((double)1.0-esq*sin(phi)*sin(phi)),(double)0.5);
    *x    = (nu+H) * cos(phi) * cos(lambda);
    *y    = (nu+H) * cos(phi) * sin(lambda);
    *z    = (((double)1.0-esq)*nu+H) * sin(phi);

    return;
}




/* @func GPS_Math_XYX_To_LatLonH ***************************************
**
** Convert XYZ coordinates to latitude and longitude and ellipsoidal height
**
** @param [w] phi [double] latitude (deg)
** @param [w] lambda [double] longitude (deg)
** @param [w] H [double] ellipsoidal height (metres)
** @param [r] x [double *] X
** @param [r] y [double *] Y
** @param [r] z [double *] Z
** @param [r] a [double] semi-major axis (metres)
** @param [r] b [double] semi-minor axis (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_XYZ_To_LatLonH(double *phi, double *lambda, double *H,
			     double x, double y, double z,
			     double a, double b)
{
    double esq;
    double nu=0.0;
    double phix;
    double nphi;
    double rho;
    int32    t1=0;
    int32    t2=0;

    if(x<(double)0 && y>=(double)0) t1=1;
    if(x<(double)0 && y<(double)0)  t2=1;

    rho  = pow(((x*x)+(y*y)),(double)0.5);
    esq  = ((a*a)-(b*b)) / (a*a);
    phix = atan(z/(((double)1.0 - esq) * rho));
    nphi = (double)1e20;
    
    while(fabs(phix-nphi)>(double)0.00000000001)
    {
	nphi  = phix;
	nu    = a / pow(((double)1.0-esq*sin(nphi)*sin(nphi)),(double)0.5);
	phix  = atan((z+esq*nu*sin(nphi))/rho);
    }
    
    *phi    = GPS_Math_Rad_To_Deg(phix);
    *H      = (rho / cos(phix)) - nu;
    *lambda = GPS_Math_Rad_To_Deg(atan(y/x));

    if(t1) *lambda += (double)180.0;
    if(t2) *lambda -= (double)180.0;

    return;
}



/* @func GPS_Math_Airy1830LatLonH_To_XYZ **********************************
**
** Convert Airy 1830 latitude and longitude and ellipsoidal height to
** X, Y & Z coordinates
**
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] H [double] ellipsoidal height (metres)
** @param [w] x [double *] X
** @param [w] y [double *] Y
** @param [w] z [double *] Z
**
** @return [void]
************************************************************************/
void GPS_Math_Airy1830LatLonH_To_XYZ(double phi, double lambda, double H,
				     double *x, double *y, double *z)
{
    double a = 6377563.396;
    double b = 6356256.910;

    GPS_Math_LatLonH_To_XYZ(phi,lambda,H,x,y,z,a,b);

    return;
}



/* @func GPS_Math_WGS84LatLonH_To_XYZ **********************************
**
** Convert WGS84 latitude and longitude and ellipsoidal height to
** X, Y & Z coordinates
**
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] H [double] ellipsoidal height (metres)
** @param [w] x [double *] X
** @param [w] y [double *] Y
** @param [w] z [double *] Z
**
** @return [void]
************************************************************************/
void GPS_Math_WGS84LatLonH_To_XYZ(double phi, double lambda, double H,
				  double *x, double *y, double *z)
{
    double a = 6378137.000;
    double b = 6356752.3141;

    GPS_Math_LatLonH_To_XYZ(phi,lambda,H,x,y,z,a,b);

    return;
}




/* @func GPS_Math_XYZ_To_Airy1830LatLonH **********************************
**
** Convert XYZ to Airy 1830 latitude and longitude and ellipsoidal height
**
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] H [double] ellipsoidal height (metres)
** @param [w] x [double *] X
** @param [w] y [double *] Y
** @param [w] z [double *] Z
**
** @return [void]
************************************************************************/
void GPS_Math_XYZ_To_Airy1830LatLonH(double *phi, double *lambda, double *H,
				     double x, double y, double z)
{
    double a = 6377563.396;
    double b = 6356256.910;

    GPS_Math_XYZ_To_LatLonH(phi,lambda,H,x,y,z,a,b);

    return;
}



/* @func GPS_Math_XYZ_To_WGS84LatLonH **********************************
**
** Convert XYZ to WGS84 latitude and longitude and ellipsoidal height
**
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] H [double] ellipsoidal height (metres)
** @param [w] x [double *] X
** @param [w] y [double *] Y
** @param [w] z [double *] Z
**
** @return [void]
************************************************************************/
void GPS_Math_XYZ_To_WGS84LatLonH(double *phi, double *lambda, double *H,
				  double x, double y, double z)
{
    double a = 6378137.000;
    double b = 66356752.3141;

    GPS_Math_XYZ_To_LatLonH(phi,lambda,H,x,y,z,a,b);

    return;
}






    
/* @func  GPS_Math_LatLon_To_EN **********************************
**
** Convert latitude and longitude to eastings and northings
** Standard Gauss-Kruger Transverse Mercator
**
** @param [w] E [double *] easting (metres)
** @param [w] N [double *] northing (metres)
** @param [r] phi [double] latitude (deg)
** @param [r] lambda [double] longitude (deg)
** @param [r] N0 [double] true northing origin (metres)
** @param [r] E0 [double] true easting  origin (metres)
** @param [r] phi0 [double] true latitude origin (deg)
** @param [r] lambda0 [double] true longitude origin (deg)
** @param [r] F0 [double] scale factor on central meridian
** @param [r] a [double] semi-major axis (metres)
** @param [r] b [double] semi-minor axis (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_LatLon_To_EN(double *E, double *N, double phi,
			   double lambda, double N0, double E0,
			   double phi0, double lambda0,
			   double F0, double a, double b)
{
    double esq;
    double n;
    double etasq;
    double nu;
    double rho;
    double M;
    double I;
    double II;
    double III;
    double IIIA;
    double IV;
    double V;
    double VI;
    
    double tmp;
    double tmp2;
    double fdf;
    double fde;
    
    phi0    = GPS_Math_Deg_To_Rad(phi0);
    lambda0 = GPS_Math_Deg_To_Rad(lambda0);
    phi     = GPS_Math_Deg_To_Rad(phi);
    lambda  = GPS_Math_Deg_To_Rad(lambda);
    
    esq = ((a*a)-(b*b)) / (a*a);
    n   = (a-b) / (a+b);
    
    tmp  = (double)1.0 - (esq * sin(phi) * sin(phi));
    nu   = a * F0 * pow(tmp,(double)-0.5);
    rho  = a * F0 * ((double)1.0 - esq) * pow(tmp,(double)-1.5);
    etasq = (nu / rho) - (double)1.0;

    fdf   = (double)5.0 / (double)4.0;
    tmp   = (double)1.0 + n + (fdf * n * n) + (fdf * n * n * n);
    tmp  *= (phi - phi0);
    tmp2  = (double)3.0*n + (double)3.0*n*n + ((double)21./(double)8.)*n*n*n;
    tmp2 *= (sin(phi-phi0) * cos(phi+phi0));
    tmp  -= tmp2;

    fde   = ((double)15.0 / (double)8.0);
    tmp2  = ((fde*n*n) + (fde*n*n*n)) * sin((double)2.0 * (phi-phi0));
    tmp2 *= cos((double)2.0 * (phi+phi0));
    tmp  += tmp2;
    
    tmp2  = ((double)35.0/(double)24.0) * n * n * n;
    tmp2 *= sin((double)3.0 * (phi-phi0));
    tmp2 *= cos((double)3.0 * (phi+phi0));
    tmp  -= tmp2;

    M     = b * F0 * tmp;
    I     = M + N0;
    II    = (nu / (double)2.0) * sin(phi) * cos(phi);
    III   = (nu / (double)24.0) * sin(phi) * cos(phi) * cos(phi) * cos(phi);
    III  *= ((double)5.0 - (tan(phi) * tan(phi)) + ((double)9.0 * etasq));
    IIIA  = (nu / (double)720.0) * sin(phi) * pow(cos(phi),(double)5.0);
    IIIA *= ((double)61.0 - ((double)58.0*tan(phi)*tan(phi)) +
	     pow(tan(phi),(double)4.0));
    IV    = nu * cos(phi);

    tmp   = pow(cos(phi),(double)3.0);
    tmp  *= ((nu/rho) - tan(phi) * tan(phi));
    V     = (nu/(double)6.0) * tmp;

    tmp   = (double)5.0 - ((double)18.0 * tan(phi) * tan(phi));
    tmp  += tan(phi)*tan(phi)*tan(phi)*tan(phi) + ((double)14.0 * etasq);
    tmp  -= ((double)58.0 * tan(phi) * tan(phi) * etasq);
    tmp2  = cos(phi)*cos(phi)*cos(phi)*cos(phi)*cos(phi) * tmp;
    VI    = (nu / (double)120.0) * tmp2;
    
    *N = I + II*(lambda-lambda0)*(lambda-lambda0) +
	     III*pow((lambda-lambda0),(double)4.0) +
	     IIIA*pow((lambda-lambda0),(double)6.0);

    *E = E0 + IV*(lambda-lambda0) + V*pow((lambda-lambda0),(double)3.0) +
	 VI * pow((lambda-lambda0),(double)5.0);

    return;
}



/* @func GPS_Math_Airy1830MLatLonToINGEN ************************************
**
** Convert Modified Airy 1830  datum latitude and longitude to Irish
** National Grid Eastings and Northings
**
** @param [r] phi [double] modified Airy latitude     (deg)
** @param [r] lambda [double] modified Airy longitude (deg)
** @param [w] E [double *] NG easting (metres)
** @param [w] N [double *] NG northing (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_Airy1830M_LatLonToINGEN(double phi, double lambda, double *E,
				      double *N)
{
    double N0      =  250000;
    double E0      =  200000;
    double F0      = 1.000035;
    double phi0    = 53.5;
    double lambda0 = -8.;
    double a       = 6377340.189;
    double b       = 6356034.447;

    GPS_Math_LatLon_To_EN(E,N,phi,lambda,N0,E0,phi0,lambda0,F0,a,b);

    return;
}




/* @func GPS_Math_Airy1830LatLonToNGEN **************************************
**
** Convert Airy 1830 datum latitude and longitude to UK Ordnance Survey
** National Grid Eastings and Northings
**
** @param [r] phi [double] WGS84 latitude     (deg)
** @param [r] lambda [double] WGS84 longitude (deg)
** @param [w] E [double *] NG easting (metres)
** @param [w] N [double *] NG northing (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_Airy1830LatLonToNGEN(double phi, double lambda, double *E,
				   double *N)
{
    double N0      = -100000;
    double E0      =  400000;
    double F0      = 0.9996012717;
    double phi0    = 49.;
    double lambda0 = -2.;
    double a       = 6377563.396;
    double b       = 6356256.910;

    GPS_Math_LatLon_To_EN(E,N,phi,lambda,N0,E0,phi0,lambda0,F0,a,b);

    return;
}




/* @func  GPS_Math_EN_To_LatLon **************************************
**
** Convert Eastings and Northings to latitude and longitude
**
** @param [w] E [double] NG easting (metres)
** @param [w] N [double] NG northing (metres)
** @param [r] phi [double *] Airy latitude     (deg)
** @param [r] lambda [double *] Airy longitude (deg)
** @param [r] N0 [double] true northing origin (metres)
** @param [r] E0 [double] true easting  origin (metres)
** @param [r] phi0 [double] true latitude origin (deg)
** @param [r] lambda0 [double] true longitude origin (deg)
** @param [r] F0 [double] scale factor on central meridian
** @param [r] a [double] semi-major axis (metres)
** @param [r] b [double] semi-minor axis (metres)
**
** @return [void]
************************************************************************/
void GPS_Math_EN_To_LatLon(double E, double N, double *phi,
			   double *lambda, double N0, double E0,
			   double phi0, double lambda0,
			   double F0, double a, double b)
{
    double esq;
    double n;
    double etasq;
    double nu;
    double rho;
    double M;
    double VII;
    double VIII;
    double IX;
    double X;
    double XI;
    double XII;
    double XIIA;
    double phix;
    double nphi=0.0;
    
    double tmp;
    double tmp2;
    double fdf;
    double fde;

    phi0    = GPS_Math_Deg_To_Rad(phi0);
    lambda0 = GPS_Math_Deg_To_Rad(lambda0);

    n     = (a-b) / (a+b);
    fdf   = (double)5.0 / (double)4.0;
    fde   = ((double)15.0 / (double)8.0);

    esq = ((a*a)-(b*b)) / (a*a);


    phix = ((N-N0)/(a*F0)) + phi0;
    
    tmp  = (double)1.0 - (esq * sin(phix) * sin(phix));
    nu   = a * F0 * pow(tmp,(double)-0.5);
    rho  = a * F0 * ((double)1.0 - esq) * pow(tmp,(double)-1.5);
    etasq = (nu / rho) - (double)1.0;

    M = (double)-1e20;

    while(N-N0-M > (double)0.000001)
    {
	nphi = phix;
	
	tmp   = (double)1.0 + n + (fdf * n * n) + (fdf * n * n * n);
	tmp  *= (nphi - phi0);
	tmp2  = (double)3.0*n + (double)3.0*n*n +
	        ((double)21./(double)8.)*n*n*n;
	tmp2 *= (sin(nphi-phi0) * cos(nphi+phi0));
	tmp  -= tmp2;


	tmp2  = ((fde*n*n) + (fde*n*n*n)) * sin((double)2.0 * (nphi-phi0));
	tmp2 *= cos((double)2.0 * (nphi+phi0));
	tmp  += tmp2;
    
	tmp2  = ((double)35.0/(double)24.0) * n * n * n;
	tmp2 *= sin((double)3.0 * (nphi-phi0));
	tmp2 *= cos((double)3.0 * (nphi+phi0));
	tmp  -= tmp2;

	M     = b * F0 * tmp;

	if(N-N0-M > (double)0.000001)
	    phix = ((N-N0-M)/(a*F0)) + nphi;
    }
    

    VII  = tan(nphi) / ((double)2.0 * rho * nu);

    tmp  = (double)5.0 + (double)3.0 * tan(nphi) * tan(nphi) + etasq;
    tmp -= (double)9.0 * tan(nphi) * tan(nphi) * etasq;
    VIII = (tan(nphi)*tmp) / ((double)24.0 * rho * nu*nu*nu);

    tmp  = (double)61.0 + (double)90.0 * tan(nphi) * tan(nphi);
    tmp += (double)45.0 * pow(tan(nphi),(double)4.0);
    IX   = tan(nphi) / ((double)720.0 * rho * pow(nu,(double)5.0)) * tmp;

    X    = (double)1.0 / (cos(nphi) * nu);

    tmp  = (nu / rho) + (double)2.0 * tan(nphi) * tan(nphi);
    XI   = ((double)1.0 / (cos(nphi) * (double)6.0 * nu*nu*nu)) * tmp;

    tmp  = (double)5.0 + (double)28.0 * tan(nphi)*tan(nphi);
    tmp += (double)24.0 * pow(tan(nphi),(double)4.0);
    XII  = ((double)1.0 / ((double)120.0 * pow(nu,(double)5.0) * cos(nphi)))
	   * tmp;

    tmp  = (double)61.0 + (double)662.0 * tan(nphi) * tan(nphi);
    tmp += (double)1320.0 * pow(tan(nphi),(double)4.0);
    tmp += (double)720.0  * pow(tan(nphi),(double)6.0);
    XIIA = ((double)1.0 / (cos(nphi) * (double)5040.0 * pow(nu,(double)7.0)))
	   * tmp;

    *phi = nphi - VII*pow((E-E0),(double)2.0) + VIII*pow((E-E0),(double)4.0) -
	   IX*pow((E-E0),(double)6.0);
    
    *lambda = lambda0 + X*(E-E0) - XI*pow((E-E0),(double)3.0) +
	      XII*pow((E-E0),(double)5.0) - XIIA*pow((E-E0),(double)7.0);

    *phi    = GPS_Math_Rad_To_Deg(*phi);
    *lambda = GPS_Math_Rad_To_Deg(*lambda);

    return;
}




/* @func GPS_Math_NGENToAiry1830LatLon **************************************
**
** Convert  to UK Ordnance Survey National Grid Eastings and Northings to
** Airy 1830 datum latitude and longitude
**
** @param [r] E [double] NG easting (metres)
** @param [r] N [double] NG northing (metres)
** @param [w] phi [double *] Airy latitude     (deg)
** @param [w] lambda [double *] Airy longitude (deg)
**
** @return [void]
************************************************************************/
void GPS_Math_NGENToAiry1830LatLon(double E, double N, double *phi,
				   double *lambda)
{
    double N0      = -100000;
    double E0      =  400000;
    double F0      = 0.9996012717;
    double phi0    = 49.;
    double lambda0 = -2.;
    double a       = 6377563.396;
    double b       = 6356256.910;

    GPS_Math_EN_To_LatLon(E,N,phi,lambda,N0,E0,phi0,lambda0,F0,a,b);
    
    return;
}



/* @func GPS_Math_INGENToAiry1830MLatLon **************************************
**
** Convert Irish National Grid Eastings and Northings to modified
** Airy 1830 datum latitude and longitude
**
** @param [r] E [double] ING easting (metres)
** @param [r] N [double] ING northing (metres)
** @param [w] phi [double *] modified Airy latitude     (deg)
** @param [w] lambda [double *] modified Airy longitude (deg)
**
** @return [void]
************************************************************************/
void GPS_Math_INGENToAiry1830MLatLon(double E, double N, double *phi,
				     double *lambda)
{
    double N0      =  250000;
    double E0      =  200000;
    double F0      = 1.000035;
    double phi0    = 53.5;
    double lambda0 = -8.;
    double a       = 6377340.189;
    double b       = 6356034.447;

    GPS_Math_EN_To_LatLon(E,N,phi,lambda,N0,E0,phi0,lambda0,F0,a,b);
    
    return;
}



/* @func GPS_Math_EN_To_UKOSNG_Map *************************************
**
** Convert Airy 1830 eastings and northings to Ordnance Survey map
** two letter code plus modified eastings and northings
**
** @param [r] E [double] NG easting (metres)
** @param [r] N [double] NG northing (metres)
** @param [w] mE [double *] modified easting (metres)
** @param [w] mN [double *] modified northing (metres)
** @param [w] map [char *] map code
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_EN_To_UKOSNG_Map(double E, double N, double *mE,
				double *mN, char *map)
{
    int32  t;
    int32  idx;
    
    if(E>=(double)700000. || E<(double)0.0 || N<(double)0.0 ||
       N>=(double)1300000.0)
	return 0;

    idx = ((int32)N/100000)*7 + (int32)E/100000;
    (void) strcpy(map,UKNG[idx]);
    
    t = ((int32)E / 100000) * 100000;
    *mE = E - (double)t;

    t = ((int32)N / 100000) * 100000;
    *mN = N - (double)t;
    
    return 1;
}



/* @func GPS_Math_UKOSNG_Map_To_EN *************************************
**
** Convert Ordnance Survey map eastings and northings plus
** two letter code to Airy 1830 eastings and northings
**
** @param [w] map [char *] map code
** @param [r] mapE [double] easting (metres)
** @param [r] mapN [double] northing (metres)
** @param [w] E [double *] full Airy easting (metres)
** @param [w] N [double *] full Airy northing (metres)

**
** @return [int32] success
************************************************************************/
int32 GPS_Math_UKOSNG_Map_To_EN(char *map, double mapE, double mapN, double *E,
				double *N)
{
    int32  t;
    int32  idx;
    
    if(mapE>=(double)100000.0 || mapE<(double)0.0 || mapN<(double)0.0 ||
       mapN>(double)100000.0)
	return 0;

    idx=0;
    while(*UKNG[idx])
    {
	if(!strcmp(UKNG[idx],map)) break;
	++idx;
    }
    if(!*UKNG[idx])
	return 0;
    

    t = (idx / 7) * 100000;
    *N = mapN + (double)t;

    t = (idx % 7) * 100000;
    *E = mapE + (double)t;

    return 1;
}



/* @func GPS_Math_Molodensky *******************************************
**
** Transform one datum to another
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [r] Sa   [double] source semi-major axis (metres)
** @param [r] Sif  [double] source inverse flattening
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] Da   [double]   dest semi-major axis (metres)
** @param [r] Dif  [double]   dest inverse flattening
** @param [r] dx  [double]   dx
** @param [r] dy  [double]   dy
** @param [r] dz  [double]   dz
**
** @return [void]
************************************************************************/
void GPS_Math_Molodensky(double Sphi, double Slam, double SH, double Sa,
			 double Sif, double *Dphi, double *Dlam,
			 double *DH, double Da, double Dif, double dx,
			 double dy, double dz)
{
    double Sf;
    double Df;
    double esq;
    double bda;
    double da;
    double df;
    double N;
    double M;
    double tmp;
    double tmp2;
    double dphi;
    double dlambda;
    double dheight;
    double phis;
    double phic;
    double lams;
    double lamc;
    
    Sf = (double)1.0 / Sif;
    Df = (double)1.0 / Dif;
    
    esq = (double)2.0*Sf - pow(Sf,(double)2.0);
    bda = (double)1.0 - Sf;
    Sphi = GPS_Math_Deg_To_Rad(Sphi);
    Slam = GPS_Math_Deg_To_Rad(Slam);
    
    da = Da - Sa;
    df = Df - Sf;

    phis = sin(Sphi);
    phic = cos(Sphi);
    lams = sin(Slam);
    lamc = cos(Slam);
    
    N = Sa /  sqrt((double)1.0 - esq*pow(phis,(double)2.0));
    
    tmp = ((double)1.0-esq) /pow(((double)1.0-esq*pow(phis,(double)2.0)),1.5);
    M   = Sa * tmp;

    tmp  = df * ((M/bda)+N*bda) * phis * phic;
    tmp2 = da * N * esq * phis * phic / Sa;
    tmp2 += ((-dx*phis*lamc-dy*phis*lams) + dz*phic);
    dphi = (tmp2 + tmp) / (M + SH);
    
    dlambda = (-dx*lams+dy*lamc) / ((N+SH)*phic);

    dheight = dx*phic*lamc + dy*phic*lams + dz*phis - da*(Sa/N) +
	df*bda*N*phis*phis;
    
    *Dphi = Sphi + dphi;
    *Dlam = Slam + dlambda;
    *DH   = SH   + dheight;
    
    *Dphi = GPS_Math_Rad_To_Deg(*Dphi);
    *Dlam = GPS_Math_Rad_To_Deg(*Dlam);

    return;
}



/* @func GPS_Math_Known_Datum_To_WGS84_M **********************************
**
** Transform datum to WGS84 using Molodensky
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n    [int32] datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_Known_Datum_To_WGS84_M(double Sphi, double Slam, double SH,
				     double *Dphi, double *Dlam, double *DH,
				     int32 n)
{
    double Sa;
    double Sif;
    double Da;
    double Dif;
    double x;
    double y;
    double z;
    int32    idx;
    
    Da  = (double) 6378137.0;
    Dif = (double) 298.257223563;
    
    idx  = GPS_Datum[n].ellipse;
    Sa   = GPS_Ellipse[idx].a;
    Sif  = GPS_Ellipse[idx].invf;
    x    = GPS_Datum[n].dx;
    y    = GPS_Datum[n].dy;
    z    = GPS_Datum[n].dz;

    GPS_Math_Molodensky(Sphi,Slam,SH,Sa,Sif,Dphi,Dlam,DH,Da,Dif,x,y,z);

    return;
}



/* @func GPS_Math_WGS84_To_Known_Datum_M ********************************
**
** Transform WGS84 to other datum using Molodensky
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n    [int32] datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_WGS84_To_Known_Datum_M(double Sphi, double Slam, double SH,
				     double *Dphi, double *Dlam, double *DH,
				     int32 n)
{
    double Sa;
    double Sif;
    double Da;
    double Dif;
    double x;
    double y;
    double z;
    int32    idx;
    
    Sa  = (double) 6378137.0;
    Sif = (double) 298.257223563;
    
    idx  = GPS_Datum[n].ellipse;
    Da   = GPS_Ellipse[idx].a;
    Dif  = GPS_Ellipse[idx].invf;
    x    = -GPS_Datum[n].dx;
    y    = -GPS_Datum[n].dy;
    z    = -GPS_Datum[n].dz;

    GPS_Math_Molodensky(Sphi,Slam,SH,Sa,Sif,Dphi,Dlam,DH,Da,Dif,x,y,z);

    return;
}



/* @func GPS_Math_Known_Datum_To_WGS84_C **********************************
**
** Transform datum to WGS84 using Cartesian coordinates
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n    [int32] datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_Known_Datum_To_WGS84_C(double Sphi, double Slam, double SH,
				     double *Dphi, double *Dlam, double *DH,
				     int32 n)
{
    double Sa;
    double Sif;
    double Sb;
    double Da;
    double Dif;
    double Db;
    double x;
    double y;
    double z;
    int32    idx;
    double sx;
    double sy;
    double sz;
    
    Da  = (double) 6378137.0;
    Dif = (double) 298.257223563;
    Db  = Da - (Da / Dif);
    
    idx  = GPS_Datum[n].ellipse;
    Sa   = GPS_Ellipse[idx].a;
    Sif  = GPS_Ellipse[idx].invf;
    Sb   = Sa - (Sa / Sif);
    
    x    = GPS_Datum[n].dx;
    y    = GPS_Datum[n].dy;
    z    = GPS_Datum[n].dz;

    GPS_Math_LatLonH_To_XYZ(Sphi,Slam,SH,&sx,&sy,&sz,Sa,Sb);
    sx += x;
    sy += y;
    sz += z;
    
    GPS_Math_XYZ_To_LatLonH(Dphi,Dlam,DH,sx,sy,sz,Da,Db);

    return;
}



/* @func GPS_Math_WGS84_To_Known_Datum_C ********************************
**
** Transform WGS84 to other datum using Cartesian coordinates
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n    [int32] datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_WGS84_To_Known_Datum_C(double Sphi, double Slam, double SH,
				     double *Dphi, double *Dlam, double *DH,
				     int32 n)
{
    double Sa;
    double Sif;
    double Da;
    double Dif;
    double x;
    double y;
    double z;
    int32    idx;
    double Sb;
    double Db;
    double dx;
    double dy;
    double dz;
    
    Sa  = (double) 6378137.0;
    Sif = (double) 298.257223563;
    Sb   = Sa - (Sa / Sif);
    
    idx  = GPS_Datum[n].ellipse;
    Da   = GPS_Ellipse[idx].a;
    Dif  = GPS_Ellipse[idx].invf;
    Db  = Da - (Da / Dif);

    x    = -GPS_Datum[n].dx;
    y    = -GPS_Datum[n].dy;
    z    = -GPS_Datum[n].dz;

    GPS_Math_LatLonH_To_XYZ(Sphi,Slam,SH,&dx,&dy,&dz,Sa,Sb);
    dx += x;
    dy += y;
    dz += z;

    GPS_Math_XYZ_To_LatLonH(Dphi,Dlam,DH,dx,dy,dz,Da,Db);

    return;
}



/* @func GPS_Math_Known_Datum_To_Known_Datum_M *************************
**
** Transform WGS84 to other datum using Molodensky
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n1   [int32] source datum number from GPS_Datum structure
** @param [r] n2   [int32] dest   datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_Known_Datum_To_Known_Datum_M(double Sphi, double Slam, double SH,
					   double *Dphi, double *Dlam,
					   double *DH, int32 n1, int32 n2)
{
    double Sa;
    double Sif;
    double Da;
    double Dif;
    double x1;
    double y1;
    double z1;
    double x2;
    double y2;
    double z2;
    double x;
    double y;
    double z;
    
    int32    idx1;
    int32    idx2;
    
    
    idx1 = GPS_Datum[n1].ellipse;
    Sa   = GPS_Ellipse[idx1].a;
    Sif  = GPS_Ellipse[idx1].invf;
    x1   = GPS_Datum[n1].dx;
    y1   = GPS_Datum[n1].dy;
    z1   = GPS_Datum[n1].dz;

    idx2 = GPS_Datum[n2].ellipse;
    Da   = GPS_Ellipse[idx2].a;
    Dif  = GPS_Ellipse[idx2].invf;
    x2   = GPS_Datum[n2].dx;
    y2   = GPS_Datum[n2].dy;
    z2   = GPS_Datum[n2].dz;

    x = -(x2-x1);
    y = -(y2-y1);
    z = -(z2-z1);

    GPS_Math_Molodensky(Sphi,Slam,SH,Sa,Sif,Dphi,Dlam,DH,Da,Dif,x,y,z);

    return;
}



/* @func GPS_Math_Known_Datum_To_Known_Datum_C *************************
**
** Transform known datum to other datum using Cartesian coordinates
**
** @param [r] Sphi [double] source latitude (deg)
** @param [r] Slam [double] source longitude (deg)
** @param [r] SH   [double] source height  (metres)
** @param [w] Dphi [double *] dest latitude (deg)
** @param [w] Dlam [double *] dest longitude (deg)
** @param [w] DH   [double *] dest height  (metres)
** @param [r] n1   [int32] source datum number from GPS_Datum structure
** @param [r] n2   [int32] dest   datum number from GPS_Datum structure
**
** @return [void]
************************************************************************/
void GPS_Math_Known_Datum_To_Known_Datum_C(double Sphi, double Slam, double SH,
					   double *Dphi, double *Dlam,
					   double *DH, int32 n1, int32 n2)
{
    double Sa;
    double Sif;
    double Da;
    double Dif;
    double x1;
    double y1;
    double z1;
    double x2;
    double y2;
    double z2;
    
    int32    idx1;
    int32    idx2;
    
    double Sb;
    double Db;
    double dx;
    double dy;
    double dz;
    
    idx1  = GPS_Datum[n1].ellipse;
    Sa    = GPS_Ellipse[idx1].a;
    Sif   = GPS_Ellipse[idx1].invf;
    Sb    = Sa - (Sa / Sif);

    x1    = GPS_Datum[n1].dx;
    y1    = GPS_Datum[n1].dy;
    z1    = GPS_Datum[n1].dz;

    idx2  = GPS_Datum[n2].ellipse;
    Da    = GPS_Ellipse[idx2].a;
    Dif   = GPS_Ellipse[idx2].invf;
    Db    = Da - (Da / Dif);

    x2    = GPS_Datum[n2].dx;
    y2    = GPS_Datum[n2].dy;
    z2    = GPS_Datum[n2].dz;

    GPS_Math_LatLonH_To_XYZ(Sphi,Slam,SH,&dx,&dy,&dz,Sa,Sb);
    dx += -(x2-x1);
    dy += -(y2-y1);
    dz += -(z2-z1);

    GPS_Math_XYZ_To_LatLonH(Dphi,Dlam,DH,dx,dy,dz,Da,Db);

    return;
}



/* @func GPS_Math_WGS84_To_UKOSMap_M ***********************************
**
** Convert WGS84 lat/lon to Ordnance survey map code and easting and
** northing. Uses Molodensky
**
** @param [r] lat  [double] WGS84 latitude (deg)
** @param [r] lon  [double] WGS84 longitude (deg)
** @param [w] mE   [double *] map easting (metres)
** @param [w] mN   [double *] map northing (metres)
** @param [w] map  [char *] map two letter code
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_WGS84_To_UKOSMap_M(double lat, double lon, double *mE,
				  double *mN, char *map)
{
    double alat;
    double alon;
    double aht;
    double aE;
    double aN;


    GPS_Math_WGS84_To_Known_Datum_M(lat,lon,30,&alat,&alon,&aht,86);

    GPS_Math_Airy1830LatLonToNGEN(alat,alon,&aE,&aN);

    if(!GPS_Math_EN_To_UKOSNG_Map(aE,aN,mE,mN,map))
	return 0;

    return 1;
}



/* @func GPS_Math_UKOSMap_To_WGS84_M ***********************************
**
** Transform UK Ordnance survey map position to WGS84 lat/lon
** Uses Molodensky transformation
**
** @param [r] map  [char *] map two letter code
** @param [r] mE   [double] map easting (metres)
** @param [r] mN   [double] map northing (metres)
** @param [w] lat  [double *] WGS84 latitude (deg)
** @param [w] lon  [double *] WGS84 longitude (deg)
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_UKOSMap_To_WGS84_M(char *map, double mE, double mN,
				  double *lat, double *lon)
{
    double E;
    double N;
    double alat;
    double alon;
    double ht;
    
    if(!GPS_Math_UKOSNG_Map_To_EN(map,mE,mN,&E,&N))
	return 0;

    GPS_Math_NGENToAiry1830LatLon(E,N,&alat,&alon);

    GPS_Math_Known_Datum_To_WGS84_M(alat,alon,0,lat,lon,&ht,78);

    return 1;
}



/* @func GPS_Math_WGS84_To_UKOSMap_C ***********************************
**
** Convert WGS84 lat/lon to Ordnance survey map code and easting and
** northing. Uses cartesian transformation
**
** @param [r] lat  [double] WGS84 latitude (deg)
** @param [r] lon  [double] WGS84 longitude (deg)
** @param [w] mE   [double *] map easting (metres)
** @param [w] mN   [double *] map northing (metres)
** @param [w] map  [char *] map two letter code
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_WGS84_To_UKOSMap_C(double lat, double lon, double *mE,
				  double *mN, char *map)
{
    double alat;
    double alon;
    double aht;
    double aE;
    double aN;


    GPS_Math_WGS84_To_Known_Datum_C(lat,lon,30,&alat,&alon,&aht,86);

    GPS_Math_Airy1830LatLonToNGEN(alat,alon,&aE,&aN);

    if(!GPS_Math_EN_To_UKOSNG_Map(aE,aN,mE,mN,map))
	return 0;

    return 1;
}



/* @func GPS_Math_UKOSMap_To_WGS84_C ***********************************
**
** Transform UK Ordnance survey map position to WGS84 lat/lon
** Uses cartesian transformation
**
** @param [r] map  [char *] map two letter code
** @param [r] mE   [double] map easting (metres)
** @param [r] mN   [double] map northing (metres)
** @param [w] lat  [double *] WGS84 latitude (deg)
** @param [w] lon  [double *] WGS84 longitude (deg)
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_UKOSMap_To_WGS84_C(char *map, double mE, double mN,
				  double *lat, double *lon)
{
    double E;
    double N;
    double alat;
    double alon;
    double ht;
    
    if(!GPS_Math_UKOSNG_Map_To_EN(map,mE,mN,&E,&N))
	return 0;

    GPS_Math_NGENToAiry1830LatLon(E,N,&alat,&alon);

    GPS_Math_Known_Datum_To_WGS84_C(alat,alon,0,lat,lon,&ht,78);

    return 1;
}


/* @funcstatic GPS_Math_LatLon_To_UTM_Param *****************************
**
** Transform NAD33
**
** @param [r] lat  [double] NAD latitude (deg)
** @param [r] lon  [double] NAD longitude (deg)
** @param [w] zone [int32 *]  zone number
** @param [w] zc   [char *] zone character
** @param [w] Mc   [double *] central meridian
** @param [w] E0   [double *] false easting
** @param [w] N0   [double *] false northing
** @param [w] F0   [double *] scale factor
**
** @return [int32] success
************************************************************************/
static int32 GPS_Math_LatLon_To_UTM_Param(double lat, double lon, int32 *zone,
					  char *zc, double *Mc, double *E0,
					  double *N0, double *F0)
{
    int32 ilon;
    int32 ilat;
    int32 psign;
    int32 lsign;
    
    if(lat >= (double)84.0 || lat < (double)-80.0)
	return 0;
    
    psign = lsign = 0;
    if(lon < (double)0.0)
	lsign=1;
    if(lat < (double)0.0)
	psign=1;

    ilon = abs((int32)lon);
    ilat = abs((int32)lat);
    
    if(!lsign)
    {
	*zone = 31 + (ilon / 6);
	*Mc   = (double)((ilon / 6) * 6 + 3);
    }
    else
    {
	*zone = 30 - (ilon / 6);
	*Mc   = -(double)((ilon / 6) * 6 + 3);
    }

    if(!psign)
    {
	*zc = 'N' + ilat / 8;
	if(*zc > 'N') ++*zc;
    }
    else
    {
	*zc = 'M' - (ilat / 8);
	if(*zc <= 'I') --*zc;
    }


    if(lat>=(double)56.0 && lat<(double)64.0 && lon>=(double)3.0 &&
       lon<(double)12.0)
    {
	*zone = 32;
	*zc   = 'V';
	*Mc   = (double)9.0;
    }
    
    if(*zc=='X' && lon>=(double)0.0 && lon<(double)42.0)
    {
	if(lon<(double)9.0)
	{
	    *zone = 31;
	    *Mc   = (double)3.0;
	}
	else if(lon<(double)21.0)
	{
	    *zone = 33;
	    *Mc   = (double)15.0;
	}
	else if(lon<(double)33.0)
	{
	    *zone = 35;
	    *Mc   = (double)27.0;
	}
	else
	{
	    *zone = 37;
	    *Mc   = (double)39.0;
	}
    }
    
    if(!psign)
	*N0 = (double)0.0;
    else
	*N0 = (double)10000000;

    *E0 = (double)500000;
    *F0 = (double)0.9996;
    
    return 1;
}



/* @func GPS_Math_NAD83_To_UTM_EN **************************************
**
** Transform NAD33 lat/lon to UTM zone, easting and northing
**
** @param [r] lat  [double] NAD latitude (deg)
** @param [r] lon  [double] NAD longitude (deg)
** @param [w] E    [double *] easting (metres)
** @param [w] N    [double *] northing (metres)
** @param [w] zone [int32 *]  zone number
** @param [w] zc   [char *] zone character
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_NAD83_To_UTM_EN(double lat, double lon, double *E,
			       double *N, int32 *zone, char *zc)
{
    double phi0;
    double lambda0;
    double N0;
    double E0;
    double F0;
    double a;
    double b;

    if(!GPS_Math_LatLon_To_UTM_Param(lat,lon,zone,zc,&lambda0,&E0,
				     &N0,&F0))
	return 0;

    phi0 = (double)0.0;

    a = (double) GPS_Ellipse[21].a;
    b = a - (a/GPS_Ellipse[21].invf);

    GPS_Math_LatLon_To_EN(E,N,lat,lon,N0,E0,phi0,lambda0,F0,a,b);

    return 1;
}



/* @func GPS_Math_WGS84_To_UTM_EN **************************************
**
** Transform WGS84 lat/lon to UTM zone, easting and northing
**
** @param [r] lat  [double] WGS84 latitude (deg)
** @param [r] lon  [double] WGS84 longitude (deg)
** @param [w] E    [double *] easting (metres)
** @param [w] N    [double *] northing (metres)
** @param [w] zone [int32 *]  zone number
** @param [w] zc   [char *] zone character
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_WGS84_To_UTM_EN(double lat, double lon, double *E,
			       double *N, int32 *zone, char *zc)
{
    double phi;
    double lambda;
    double H;
    
    GPS_Math_WGS84_To_Known_Datum_M(lat,lon,0,&phi,&lambda,&H,77);
    if(!GPS_Math_NAD83_To_UTM_EN(phi,lambda,E,N,zone,zc))
	return 0;

    return 1;
}



/* @funcstatic GPS_Math_UTM_Param_To_Mc ********************************
**
** Convert UTM zone and zone character to central meridian value.
** Also return false eastings, northings and scale factor
**
** @param [w] zone [int32]  zone number
** @param [w] zc   [char] zone character
** @param [w] Mc   [double *] central meridian
** @param [w] E0   [double *] false easting
** @param [w] N0   [double *] false northing
** @param [w] F0   [double *] scale factor
**
** @return [int32] success
************************************************************************/
static int32 GPS_Math_UTM_Param_To_Mc(int32 zone, char zc, double *Mc,
				      double *E0, double *N0, double *F0)
{

    if(zone>60 || zone<0 || zc<'C' || zc>'X')
	return 0;

    if(zone > 30)
	*Mc = (double)((zone-31)*6) + (double)3.0;
    else
	*Mc = (double) -(((30-zone)*6)+3);
    
    if(zone==32 && zc=='V')
	*Mc = (double)9.0;

    if(zone==31 && zc=='X')
	*Mc = (double)3.0;
    if(zone==33 && zc=='X')
	*Mc = (double)15.0;
    if(zone==35 && zc=='X')
	*Mc = (double)27.0;
    if(zone==37 && zc=='X')
	*Mc = (double)39.0;
    
    if(zc>'M')
	*N0 = (double)0.0;
    else
	*N0 = (double)10000000;

    *E0 = (double)500000;
    *F0 = (double)0.9996;
    
    return 1;
}



/* @func GPS_Math_UTM_EN_To_NAD83 **************************************
**
** Transform UTM zone, easting and northing to NAD83 lat/lon
**
** @param [r] lat  [double *] NAD latitude (deg)
** @param [r] lon  [double *] NAD longitude (deg)
** @param [w] E    [double] easting (metres)
** @param [w] N    [double] northing (metres)
** @param [w] zone [int32]    zone number
** @param [w] zc   [char]   zone character
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_UTM_EN_To_NAD83(double *lat, double *lon, double E,
			       double N, int32 zone, char zc)
{
    double phi0;
    double lambda0;
    double N0;
    double E0;
    double F0;
    double a;
    double b;

    if(!GPS_Math_UTM_Param_To_Mc(zone,zc,&lambda0,&E0,&N0,&F0))
	return 0;

    phi0 = (double)0.0;

    a = (double) GPS_Ellipse[21].a;
    b = a - (a/GPS_Ellipse[21].invf);

    GPS_Math_EN_To_LatLon(E,N,lat,lon,N0,E0,phi0,lambda0,F0,a,b);

    return 1;
}



/* @func GPS_Math_UTM_EN_To_WGS84 **************************************
**
** Transform UTM zone, easting and northing to WGS84 lat/lon
**
** @param [w] lat  [double *] WGS84 latitude (deg)
** @param [r] lon  [double *] WGS84 longitude (deg)
** @param [w] E    [double]   easting (metres)
** @param [w] N    [double]   northing (metres)
** @param [w] zone [int32]      zone number
** @param [w] zc   [char]     zone character
**
** @return [int32] success
************************************************************************/
int32 GPS_Math_UTM_EN_To_WGS84(double *lat, double *lon, double E,
			       double N, int32 zone, char zc)
{
    double phi;
    double lambda;
    double H;

    if(!GPS_Math_UTM_EN_To_NAD83(&phi,&lambda,E,N,zone,zc))
	return 0;

    
    GPS_Math_Known_Datum_To_WGS84_M(phi,lambda,0,lat,lon,&H,77);

    return 1;
}
