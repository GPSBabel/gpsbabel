#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpscom_h
#define gpscom_h


#include "gps.h"
#include <time.h>

int32  GPS_Command_Off(const char *port);

time_t GPS_Command_Get_Time(const char *port);
int32  GPS_Command_Send_Time(const char *port, time_t Time);

int32  GPS_Command_Get_Position(const char *port, double *lat, double *lon);
int32  GPS_Command_Send_Position(const char *port, double lat, double lon);

int32  GPS_Command_Pvt_On(const char *port, int32 *fd);
int32  GPS_Command_Pvt_Off(const char *port, int32 *fd);
int32  GPS_Command_Pvt_Get(int32 *fd, GPS_PPvt_Data *pvt);

int32  GPS_Command_Get_Almanac(const char *port, GPS_PAlmanac **alm);
int32  GPS_Command_Send_Almanac(const char *port, GPS_PAlmanac *alm, int32 n);

int32  GPS_Command_Get_Track(const char *port, GPS_PTrack **trk);
int32  GPS_Command_Send_Track(const char *port, GPS_PTrack *trk, int32 n);

int32  GPS_Command_Get_Waypoint(const char *port, GPS_PWay **way,int (*cb)(int, struct GPS_SWay **));
int32  GPS_Command_Send_Waypoint(const char *port, GPS_PWay *way, int32 n, int (*cb)(struct GPS_SWay **));

int32  GPS_Command_Get_Proximity(const char *port, GPS_PWay **way);
int32  GPS_Command_Send_Proximity(const char *port, GPS_PWay *way, int32 n);

int32  GPS_Command_Get_Route(const char *port, GPS_PWay **way);
int32  GPS_Command_Send_Route(const char *port, GPS_PWay *way, int32 n);


#endif

#ifdef __cplusplus
}
#endif
