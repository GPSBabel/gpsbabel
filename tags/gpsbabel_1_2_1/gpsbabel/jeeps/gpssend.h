#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpssend_h
#define gpssend_h


#include "gps.h"

#define GPS_ARB_LEN 1024

UC gps_sendbuf[GPS_ARB_LEN];

void   GPS_Make_Packet(GPS_PPacket *packet, UC type, UC *data, int16 n);    
int32  GPS_Write_Packet(int32 fd, GPS_PPacket packet);
int32  GPS_Send_Ack(int32 fd, GPS_PPacket *tra, GPS_PPacket *rec);



#endif

#ifdef __cplusplus
}
#endif
