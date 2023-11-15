#ifndef JEEPS_GPSRQST_H_INCLUDED_
#define JEEPS_GPSRQST_H_INCLUDED_


#include "jeeps/gps.h"

int32_t GPS_Rqst_Send_Time(gpsdevh* fd, time_t Time);
int32_t GPS_Rqst_Send_Position(gpsdevh* fd, double lat, double lon);


#endif // JEEPS_GPSRQST_H_INCLUDED_
