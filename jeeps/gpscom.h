#ifndef JEEPS_GPSCOM_H_INCLUDED_
#define JEEPS_GPSCOM_H_INCLUDED_


#include "jeeps/gps.h"
#include <ctime>

int32_t GPS_Command_Off(const char* port);

time_t GPS_Command_Get_Time(const char* port);
int32_t GPS_Command_Send_Time(const char* port, time_t Time);

int32_t GPS_Command_Get_Position(const char* port, double* lat, double* lon);
int32_t GPS_Command_Send_Position(const char* port, double lat, double lon);

int32_t GPS_Command_Pvt_On(const char* port, gpsdevh** fd);
int32_t GPS_Command_Pvt_Off(gpsdevh* fd);
int32_t GPS_Command_Pvt_Get(gpsdevh* fd, GPS_PPvt_Data* pvt);

int32_t GPS_Command_Get_Almanac(const char* port, GPS_PAlmanac** alm);
int32_t GPS_Command_Send_Almanac(const char* port, GPS_PAlmanac* alm, int32_t n);

int32_t GPS_Command_Get_Track(const char* port, GPS_PTrack** trk, int (*cb)(int, GPS_SWay**));
int32_t GPS_Command_Send_Track(const char* port, GPS_PTrack* trk, int32_t n, int eraset);

int32_t GPS_Command_Get_Waypoint(const char* port, GPS_PWay** way, int (*cb)(int, GPS_SWay**));
int32_t GPS_Command_Send_Waypoint(const char* port, GPS_PWay* way, int32_t n, int (*cb)(GPS_SWay**));

int32_t GPS_Command_Get_Proximity(const char* port, GPS_PWay** way);
int32_t GPS_Command_Send_Proximity(const char* port, GPS_PWay* way, int32_t n);

int32_t GPS_Command_Get_Route(const char* port, GPS_PWay** way);
int32_t GPS_Command_Send_Route(const char* port, GPS_PWay* way, int32_t n);

int32_t GPS_Command_Get_Lap(const char* port, GPS_PLap** lap, int (*cb)(int, GPS_SWay**));

int32_t GPS_Command_Send_Course(const char* port, GPS_PCourse* crs, GPS_PCourse_Lap* clp,
                                GPS_PTrack* trk, GPS_PCourse_Point* cpt,
                                int32_t n_crs, int32_t n_clp, int32_t n_trk, int32_t n_cpt);
int32_t GPS_Command_Send_Track_As_Course(const char* port, GPS_PTrack* trk, int32_t n_trk,
    GPS_PWay* wpt, int32_t n_wpt, int eraset);

int32_t GPS_Command_Get_Workout(const char* port, void** lap, int (*cb)(int, GPS_SWay**));
int32_t GPS_Command_Get_Fitness_User_Profile(const char* port, void** lap, int (*cb)(int, GPS_SWay**));
int32_t GPS_Command_Get_Workout_Limits(const char* port, void** lap, int (*cb)(int, GPS_SWay**));
int32_t GPS_Command_Get_Course_Limits(const char* port, void** lap, int (*cb)(int, GPS_SWay**));
#endif // JEEPS_GPSCOM_H_INCLUDED_
