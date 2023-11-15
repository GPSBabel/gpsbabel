#ifndef JEEPS_GPSREAD_H_INCLUDED_
#define JEEPS_GPSREAD_H_INCLUDED_


#include "jeeps/gps.h"

  time_t GPS_Time_Now();
int32_t GPS_Serial_Packet_Read(gpsdevh* fd, GPS_Packet* packet);
  bool GPS_Serial_Get_Ack(gpsdevh *fd, GPS_Packet *tra, GPS_Packet *rec);

#endif // JEEPS_GPSREAD_H_INCLUDED_
