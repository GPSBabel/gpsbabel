#ifndef JEEPS_GPSMATH_H_INCLUDED_
#define JEEPS_GPSMATH_H_INCLUDED_

#include "jeeps/gpsport.h"

#define GPS_PI 3.141592653589
#define GPS_FLTMIN 1.75494351E-38
#define GPS_FLTMAX 3.402823466E+38


  double GPS_Math_Deg_To_Rad(double v);
  double GPS_Math_Rad_To_Deg(double v);

  double GPS_Math_Metres_To_Feet(double v);
  double GPS_Math_Feet_To_Metres(double v);

int32_t GPS_Math_Deg_To_Semi(double v);
  double GPS_Math_Semi_To_Deg(int32_t v);

  time_t GPS_Math_Utime_To_Gtime(time_t v);
  time_t GPS_Math_Gtime_To_Utime(time_t v);

  void   GPS_Math_Deg_To_DegMin(double v, int32_t* d, double* m);
  void   GPS_Math_DegMin_To_Deg(int32_t d, double m, double* deg);
  void   GPS_Math_Deg_To_DegMinSec(double v, int32_t* d, int32_t* m, double* s);
  void   GPS_Math_DegMinSec_To_Deg(int32_t d, int32_t m, double s, double* deg);


  void GPS_Math_Airy1830LatLonToNGEN(double phi, double lambda, double* E,
                                     double* N);
  void GPS_Math_Airy1830M_LatLonToINGEN(double phi, double lambda, double* E,
                                        double* N);
int32_t GPS_Math_EN_To_UKOSNG_Map(double E, double N, double* mE,
                                  double* mN, char* map);
int32_t GPS_Math_UKOSNG_Map_To_EN(const char* map, double mapE, double mapN,
                                  double* E, double* N);

  void GPS_Math_LatLonH_To_XYZ(double phi, double lambda, double H,
                               double* x, double* y, double* z,
                               double a, double b);
  void GPS_Math_XYZ_To_LatLonH(double* phi, double* lambda, double* H,
                               double x, double y, double z,
                               double a, double b);

  void GPS_Math_EN_To_LatLon(double E, double N, double* phi,
                             double* lambda, double N0, double E0,
                             double phi0, double lambda0,
                             double F0, double a, double b);
  void GPS_Math_LatLon_To_EN(double* E, double* N, double phi,
                             double lambda, double N0, double E0,
                             double phi0, double lambda0,
                             double F0, double a, double b);

  void GPS_Math_NGENToAiry1830LatLon(double E, double N, double* phi,
                                     double* lambda);
  void GPS_Math_INGENToAiry1830MLatLon(double E, double N, double* phi,
                                       double* lambda);


  void GPS_Math_Airy1830LatLonH_To_XYZ(double phi, double lambda, double H,
                                       double* x, double* y, double* z);
  void GPS_Math_WGS84LatLonH_To_XYZ(double phi, double lambda, double H,
                                    double* x, double* y, double* z);
  void GPS_Math_XYZ_To_Airy1830LatLonH(double* phi, double* lambda, double* H,
                                       double x, double y, double z);
  void GPS_Math_XYZ_To_WGS84LatLonH(double* phi, double* lambda, double* H,
                                    double x, double y, double z);

  void GPS_Math_Molodensky(double Sphi, double Slam, double SH, double Sa,
                           double Sif, double* Dphi, double* Dlam,
                           double* DH, double Da, double Dif, double dx,
                           double dy, double dz);
  void GPS_Math_Known_Datum_To_WGS84_M(double Sphi, double Slam, double SH,
                                       double* Dphi, double* Dlam, double* DH,
                                       int32_t n);
  void GPS_Math_WGS84_To_Known_Datum_M(double Sphi, double Slam, double SH,
                                       double* Dphi, double* Dlam, double* DH,
                                       int32_t n);
  void GPS_Math_Known_Datum_To_WGS84_C(double Sphi, double Slam, double SH,
                                       double* Dphi, double* Dlam, double* DH,
                                       int32_t n);
  void GPS_Math_WGS84_To_Known_Datum_C(double Sphi, double Slam, double SH,
                                       double* Dphi, double* Dlam, double* DH,
                                       int32_t n);

  void GPS_Math_Known_Datum_To_Known_Datum_M(double Sphi, double Slam, double SH,
      double* Dphi, double* Dlam,
      double* DH, int32_t n1, int32_t n2);
  void GPS_Math_Known_Datum_To_Known_Datum_C(double Sphi, double Slam, double SH,
      double* Dphi, double* Dlam,
      double* DH, int32_t n1, int32_t n2);

int32_t GPS_Math_WGS84_To_UKOSMap_M(double lat, double lon, double* mE,
                                    double* mN, char* map);
int32_t GPS_Math_UKOSMap_To_WGS84_M(const char* map, double mE, double mN,
                                    double* lat, double* lon);
int32_t GPS_Math_WGS84_To_UKOSMap_C(double lat, double lon, double* mE,
                                    double* mN, char* map);
int32_t GPS_Math_UKOSMap_To_WGS84_C(const char* map, double mE, double mN,
                                    double* lat, double* lon);


int32_t GPS_Math_NAD83_To_UTM_EN(double lat, double lon, double* E,
                                 double* N, int32_t* zone, char* zc);
int32_t GPS_Math_WGS84_To_UTM_EN(double lat, double lon, double* E,
                                 double* N, int32_t* zone, char* zc);

int32_t GPS_Math_UTM_EN_To_WGS84(double* lat, double* lon, double E,
                                 double N, int32_t zone, char zc);
int32_t GPS_Math_UTM_EN_To_NAD83(double* lat, double* lon, double E,
                                 double N, int32_t zone, char zc);

int32_t GPS_Math_Known_Datum_To_UTM_EN(double lat, double lon, double* E,
                                       double* N, int32_t* zone, char* zc, int n);
int32_t GPS_Math_UTM_EN_To_Known_Datum(double* lat, double* lon, double E,
                                       double N, int32_t zone, char zc, int n);

  void GPS_Math_Swiss_LatLon_To_EN(double phi, double lambda, double* E,
                                   double* N,double phi0,double lambda0,
                                   double E0, double N0, double a, double b);
  void GPS_Math_Swiss_EN_To_LatLon(double E, double N, double* phi,
                                   double* lambda, double phi0, double lambda0,
                                   double E0, double N0, double a, double b);

  void GPS_Math_Cassini_LatLon_To_EN(double phi, double lambda, double* E,
                                     double* N, double phi0, double M0,
                                     double E0, double N0, double a, double b);
  void GPS_Math_Cassini_EN_To_LatLon(double E, double N, double* phi,
                                     double* lambda, double phi0, double M0,
                                     double E0, double N0, double a, double b);

int32_t GPS_Math_WGS84_To_ICS_EN(double lat, double lon, double* E,
                                 double* N);
  void GPS_Math_ICS_EN_To_WGS84(double E, double N, double* lat, double* lon);

int32_t GPS_Math_WGS84_To_Swiss_EN(double phi, double lambda, double* E, double* N);
  void GPS_Math_Swiss_EN_To_WGS84(double E, double N, double* lat, double* lon);

  void GPS_Math_UTM_EN_to_LatLon(int ReferenceEllipsoid,
                                 double UTMNorthing, double UTMEasting,
                                 double* Lat, double* Lon,
                                 double lambda0, double E0, double N0);

int32_t GPS_Lookup_Datum_Index(const char* n);
int32_t GPS_Lookup_Datum_Index(const QString& n);
  const char* GPS_Math_Get_Datum_Name(int datum_index);

#endif // JEEPS_GPSMATH_H_INCLUDED_
