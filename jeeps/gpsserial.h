#ifndef JEEPS_GPSSERIAL_H_INCLUDED_
#define JEEPS_GPSSERIAL_H_INCLUDED_


#include "jeeps/gps.h"

constexpr int msecDELAY = 180;	/* Milliseconds before GPS sends A001 */
constexpr int DEFAULT_BAUD = 9600;

int32_t GPS_Serial_Chars_Ready(gpsdevh* dh);
int32_t GPS_Serial_On(const char* port, gpsdevh** dh);
int32_t GPS_Serial_Off(gpsdevh* dh);
int32_t GPS_Serial_Wait(gpsdevh* dh);
int32_t GPS_Serial_Flush(gpsdevh* dh);
int32_t GPS_Serial_Read(gpsdevh* dh, void* ibuf, int size);
int32_t GPS_Serial_Write(gpsdevh* dh, const void* obuf, int size);
int32_t GPS_Serial_Set_Baud_Rate(gpsdevh* dh, int br);

#endif // JEEPS_GPSSERIAL_H_INCLUDED_
