#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsapp_h
#define gpsapp_h


#include "gps.h"

int32  GPS_Init(const char *port);

int32  GPS_A100_Get(const char *port, GPS_PWay **way, int (*cb)(int ct, GPS_PWay *));
int32  GPS_A100_Send(const char *port, GPS_PWay *way, int32 n, int (*cb)(GPS_PWay *));

int32  GPS_A200_Get(const char *port, GPS_PWay **way);
int32  GPS_A201_Get(const char *port, GPS_PWay **way);
int32  GPS_A200_Send(const char *port, GPS_PWay *way, int32 n);
int32  GPS_A201_Send(const char *port, GPS_PWay *way, int32 n);

int32  GPS_A300_Get(const char *port, GPS_PTrack **trk);
int32  GPS_A301_Get(const char *port, GPS_PTrack **trk);
int32  GPS_A300_Send(const char *port, GPS_PTrack *trk, int32 n);
int32  GPS_A301_Send(const char *port, GPS_PTrack *trk, int32 n);

int32  GPS_D300_Get(GPS_PTrack *trk, int32 entries, int32 fd);
void   GPS_D300b_Get(GPS_PTrack *trk, UC *data);
void   GPS_D301b_Get(GPS_PTrack *trk, UC *data);
void   GPS_D310_Get(GPS_PTrack *trk, UC *s);
void   GPS_D300_Send(UC *data, GPS_PTrack trk);
void   GPS_D301_Send(UC *data, GPS_PTrack trk);
void   GPS_D310_Send(UC *data, GPS_PTrack trk, int32 *len);

int32  GPS_A400_Get(const char *port, GPS_PWay **way);
int32  GPS_A400_Send(const char *port, GPS_PWay *way, int32 n);

int32  GPS_A500_Get(const char *port, GPS_PAlmanac **alm);
int32  GPS_A500_Send(const char *port, GPS_PAlmanac *alm, int32 n);

time_t GPS_A600_Get(const char *port);
time_t GPS_D600_Get(GPS_PPacket packet);
int32  GPS_A600_Send(const char *port, time_t Time);
void   GPS_D600_Send(GPS_PPacket *packet, time_t Time);

int32  GPS_A700_Get(const char *port, double *lat, double *lon);
int32  GPS_A700_Send(const char *port, double lat, double lon);
void   GPS_D700_Get(GPS_PPacket packet, double *lat, double *lon);
void   GPS_D700_Send(GPS_PPacket *packet, double lat, double lon);

int32  GPS_A800_On(const char *port, int32 *fd);
int32  GPS_A800_Off(const char *port, int32 *fd);
int32  GPS_A800_Get(int32 *fd, GPS_PPvt_Data *packet);
void   GPS_D800_Get(GPS_PPacket packet, GPS_PPvt_Data *pvt);

const char * Get_Pkt_Type(unsigned char p, unsigned char d0, const char **xinfo);


#endif

#ifdef __cplusplus
}
#endif
