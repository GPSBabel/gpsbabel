#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpssend_h
#define gpssend_h


#include "gps.h"

#define GPS_ARB_LEN 1024

void   GPS_Serial_Make_Packet(GPS_PPacket *packet, UC type, UC *data, int16 n);    
int32  GPS_Serial_Write_Packet(gpsdevh *fd, GPS_PPacket packet);
int32  GPS_Serial_Send_Ack(gpsdevh *fd, GPS_PPacket *tra, GPS_PPacket *rec);

void   GPS_Make_Packet(GPS_PPacket *packet, UC type, UC *data, int16 n);    


#endif

#ifdef __cplusplus
}
#endif
