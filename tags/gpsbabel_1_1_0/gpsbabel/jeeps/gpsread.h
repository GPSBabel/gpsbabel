#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsread_h
#define gpsread_h


#include "gps.h"
#include <time.h>

time_t GPS_Time_Now(void);
int32  GPS_Packet_Read(int32 fd, GPS_PPacket *packet);
int32  GPS_Get_Ack(int32 fd, GPS_PPacket *tra, GPS_PPacket *rec);
int32  ajb(int32 fd);


#endif

#ifdef __cplusplus
}
#endif
