#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsnmea_h
#define gpsnmea_h


#include "gps.h"


typedef struct GPS_SGsv
{
    int32 inview;
    int32 prn[32];
    int32 elevation[32];
    int32 azimuth[32];
    int32 strength[32];
    int32 valid;
} GPS_OGsv,*GPS_PGsv;

typedef struct GPS_SRme
{
    double hpe;
    double vpe;
    double spe;
    int32 valid;
} GPS_ORme,*GPS_PRme;

typedef struct GPS_SGll
{
    double lat;
    double lon;
    time_t time;
    char   dv;
    int32  valid;
} GPS_OGll,*GPS_PGll;

typedef struct GPS_SRmz
{
    int32 height;
    int32 dim;
    int32 valid;
} GPS_ORmz,*GPS_PRmz;

typedef struct GPS_SRmm
{
    char datum[83];
    int32 valid;
} GPS_ORmm,*GPS_PRmm;

typedef struct GPS_SBod
{
    double true;
    double mag;
    char   dest[83];
    char   start[83];
    int32  valid;
} GPS_OBod,*GPS_PBod;

typedef struct GPS_SRte
{
    char type;
    int32 rte;
    char *wpts;
    int32 valid;
} GPS_ORte,*GPS_PRte;

typedef struct GPS_SWpl
{
    double lat;
    double lon;
    char wpt[83];
    int32 valid;
} GPS_OWpl,*GPS_PWpl;

typedef struct GPS_SRmc
{
    time_t time;
    char   warn;
    double lat;
    double lon;
    double speed;
    double cmg;
    char   date[83];
    double magvar;
    int32  valid;
} GPS_ORmc,*GPS_PRmc;

typedef struct GPS_SRmb
{
    char warn;
    double cross;
    char correct;
    char owpt[83];
    char dwpt[83];
    double lat;
    double lon;
    double range;
    double true;
    double velocity;
    char   alarm;
    int32  valid;
} GPS_ORmb,*GPS_PRmb;

typedef struct GPS_SGga
{
    time_t time;
    double lat;
    double lon;
    int32  qual;
    int32  nsat;
    double hdil;
    double alt;
    double galt;
    int32  last;
    int32  dgpsid;
    int32  valid;
} GPS_OGga,*GPS_PGga;

typedef struct GPS_SGsa
{
    char type;
    int32 fix;
    int32 nsat;
    int32 prn[12];
    double pdop;
    double hdop;
    double vdop;
    int32  valid;
} GPS_OGsa,*GPS_PGsa;

typedef struct GPS_SApb
{
    char blink;
    char warn;
    double edist;
    char steer;
    char unit;
    char alarmc;
    char alarmp;
    double od;
    char wpt[83];
    double pd;
    double hdg;
    int32  valid;
} GPS_OApb,*GPS_PApb;

typedef struct GPS_SBwc
{
    time_t time;
    double lat;
    double lon;
    double true;
    double mag;
    double dist;
    char wpt[83];
    int32 valid;
} GPS_OBwc,*GPS_PBwc;

typedef struct GPS_SBwr
{
    time_t time;
    double lat;
    double lon;
    double true;
    double mag;
    double dist;
    char wpt[83];
    int32 valid;
} GPS_OBwr,*GPS_PBwr;

typedef struct GPS_SDbt
{
    double f;
    double m;
    int32  valid;
} GPS_ODbt,*GPS_PDbt;

typedef struct GPS_SHdm
{
    double hdg;
    int32 valid;
} GPS_OHdm,*GPS_PHdm;

typedef struct GPS_SHsc
{
    double true;
    double mag;
    int32  valid;
} GPS_OHsc,*GPS_PHsc;

typedef struct GPS_SMtw
{
    double T;
    int32  valid;
} GPS_OMtw,*GPS_PMtw;

typedef struct GPS_SR00
{
    char wpts[83];
    int32 valid;
} GPS_OR00,*GPS_PR00;

typedef struct GPS_SVhw
{
    double true;
    double mag;
    double wspeed;
    double speed;
    int32  valid;
} GPS_OVhw,*GPS_PVhw;

typedef struct GPS_SVwr
{
    double wind;
    char   wdir;
    double knots;
    double ms;
    double khr;
    int32  valid;
} GPS_OVwr,*GPS_PVwr;

typedef struct GPS_SVtg
{
    double true;
    double mag;
    double knots;
    double khr;
    int32  valid;
} GPS_OVtg,*GPS_PVtg;

typedef struct GPS_SXte
{
    char warn;
    char cycle;
    double dist;
    char steer;
    char unit;
    int32 valid;
} GPS_OXte,*GPS_PXte;

typedef struct GPS_SXtr
{
    double dist;
    char steer;
    char unit;
    int32 valid;
} GPS_OXtr,*GPS_PXtr;

typedef struct GPS_SLib
{
    double freq;
    double baud;
    char rqst;
    int32 valid;
} GPS_OLib,*GPS_PLib;


typedef struct GPS_SNmea
{
    GPS_PGsv gsv;
    GPS_PRme rme;
    GPS_PGll gll;
    GPS_PRmz rmz;
    GPS_PRmm rmm;
    GPS_PBod bod;
    GPS_PRte rte;
    GPS_PWpl wpl;
    GPS_PRmc rmc;
    GPS_PRmb rmb;
    GPS_PGga gga;
    GPS_PGsa gsa;
    GPS_PApb apb;
    GPS_PBwc bwc;
    GPS_PBwr bwr;
    GPS_PDbt dbt;
    GPS_PHdm hdm;
    GPS_PHsc hsc;
    GPS_PMtw mtw;
    GPS_PR00 r00;
    GPS_PVhw vhw;
    GPS_PVwr vwr;
    GPS_PVtg vtg;
    GPS_PXte xte;
    GPS_PXtr xtr;
    GPS_PLib lib;
} GPS_ONmea,*GPS_PNmea;






extern int32 gps_fd; 		/* FD for serial port access [NMEA] */
extern GPS_PNmea gps_nmea;      /* Internal nmea data repository    */



void  GPS_NMEA_Add_Checksum(char *s);
int32 GPS_NMEA_Line_Check(const char *s);
int32 GPS_NMEA_Load(int32 fd);
int32 GPS_NMEA_Init(const char *s);
void  GPS_NMEA_Exit(void);
int32 GPS_NMEA_Send(const char *s, int32 flag);

#endif

#ifdef __cplusplus
}
#endif
