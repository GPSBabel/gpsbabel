#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsserial_h
#define gpsserial_h


#include "gps.h"

#define usecDELAY 180000	/* Microseconds before GPS sends A001 */

int32  GPS_Serial_Chars_Ready(int32 fd);
int32  GPS_Serial_Close(int32 fd, const char *port);
int32  GPS_Serial_Open(int32 *fd, const char *port);
int32  GPS_Serial_Open_NMEA(int32 *fd, const char *port);
int32  GPS_Serial_Restoretty(const char *port);
int32  GPS_Serial_Savetty(const char *port);
int32  GPS_Serial_On(const char *port, int32 *fd);
int32  GPS_Serial_Off(const char *port, int32 fd);
int32  GPS_Serial_Wait(int32 fd);
int32  GPS_Serial_Flush(int32 fd);
int32  GPS_Serial_On_NMEA(const char *port, int32 *fd);
int32  GPS_Serial_Read(int32 ignored, void *ibuf, int size);
int32  GPS_Serial_Write(int32 ignored, const void *obuf, int size);
void   GPS_Serial_Error(char *hdr, ...);


#endif

#ifdef __cplusplus
}
#endif
