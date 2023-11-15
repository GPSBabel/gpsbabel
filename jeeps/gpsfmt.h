#ifndef JEEPS_GPSFMT_H_INCLUDED_
#define JEEPS_GPSFMT_H_INCLUDED_


#include "jeeps/gps.h"
#include <cstdio>
#include <ctime>

void   GPS_Fmt_Print_Time(time_t Time, FILE* outf);
void   GPS_Fmt_Print_Position(double lat, double lon, FILE* outf);
void   GPS_Fmt_Print_Pvt(GPS_PPvt_Data pvt, FILE* outf);
void   GPS_Fmt_Print_Almanac(GPS_PAlmanac* alm, int32_t n, FILE* outf);
void   GPS_Fmt_Print_Track(GPS_PTrack* trk, int32_t n, FILE* outf);
int32_t GPS_Fmt_Print_Waypoint(GPS_PWay* way, int32_t n, FILE* outf);
int32_t GPS_Fmt_Print_Proximity(GPS_PWay* way, int32_t n, FILE* outf);
int32_t GPS_Fmt_Print_Route(GPS_PWay* way, int32_t n, FILE* outf);

#endif // JEEPS_GPSFMT_H_INCLUDED_
