#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsrqst_h
#define gpsrqst_h


#include "gps.h"

int32 GPS_Rqst_Send_Time(int32 fd, time_t Time);
int32 GPS_Rqst_Send_Position(int32 fd, double lat, double lon);


#endif

#ifdef __cplusplus
}
#endif
