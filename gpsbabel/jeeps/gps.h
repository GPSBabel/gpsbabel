#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gps_h
#define gps_h

#include "../defs.h"
#include "gpsport.h"
#include <time.h>

#define FRAMING_ERROR  -1
#define PROTOCOL_ERROR -2
#define HARDWARE_ERROR -3
#define SERIAL_ERROR   -4
#define MEMORY_ERROR   -5
#define GPS_UNSUPPORTED -6
#define INPUT_ERROR -7

#define MAX_GPS_PACKET_SIZE	1024
#define GPS_TIME_OUT		5

#define gpsTrue  1
#define gpsFalse 0

#define DLE 0x10
#define ETX 0x03


extern int32 gps_errno;
extern int32 gps_warning;
extern int32 gps_error;
extern int32 gps_user;
extern int32 gps_show_bytes;


typedef struct GPS_SPacket
{
    UC dle;
    UC type;
    UC n;
    UC *data;
    UC chk;
    UC edle;
    UC etx;
    UC bytes;		/* Actual number of bytes (for sending) */    
} GPS_OPacket, *GPS_PPacket;



typedef struct GPS_SProduct_Data_Type
{
    int16 id;
    int16 version;
    char  desc[MAX_GPS_PACKET_SIZE];
} GPS_OProduct_Data_Type, *GPS_PProduct_Data_Type;




typedef struct GPS_SPvt_Data_Type
{
    float alt;
    float epe;
    float eph;
    float epv;
    int16 fix;
    double tow;
    double lat;
    double lon;
    float east;
    float north;
    float up;
    float msl_hght;
    int16 leap_scnds;
    int32 wn_days;
} GPS_OPvt_Data, *GPS_PPvt_Data;



typedef struct GPS_STrack
{
    double   lat;		/* Degrees */
    double   lon;		/* Degrees */
    time_t   Time;		/* Unix time */
    float    alt;		/* Altitude */
    float    dpth;		/* Depth    */
    int32    tnew;		/* New track? */
    int32    ishdr;		/* Track header? */
    int32    dspl;		/* Display on map? */
    int32    colour;		/* Colour */
    char     trk_ident[256];	/* Track identifier */
}
GPS_OTrack, *GPS_PTrack;



typedef struct GPS_SAlmanac
{
    UC    svid;
    int16 wn;
    float toa;
    float af0;
    float af1;
    float e;
    float sqrta;
    float m0;
    float w;
    float omg0;
    float odot;
    float i;
    UC    hlth;
} GPS_OAlmanac, *GPS_PAlmanac;


typedef struct GPS_SWay
{
    char   ident[256];
    double lat;
    double lon;
    char   cmnt[256];
    float  dst;
    int32  smbl;
    int32  dspl;
    char   wpt_ident[256];
    char   lnk_ident[256];
    UC     subclass[18];
    int32  colour;
    char   cc[2];
    UC     wpt_class;
    float  alt;
    char   city[24];
    char   state[2];
    char   name[30];
    char   facility[32];
    char   addr[52];
    char   cross_road[52];
    int32  attr;
    float  dpth;
    int32  idx;
    int32  prot;
    int32  isrte;
    int32  rte_prot;
    UC     rte_num;
    char   rte_cmnt[20];
    char   rte_ident[256];
    int32  islink;
    int32  rte_link_class;
    char   rte_link_subclass[18];
    char   rte_link_ident[256];

    char     Time_populated;	/* 1 if true */
    time_t   Time;		/* Unix time */
} GPS_OWay, *GPS_PWay;




#include "gpsserial.h"
#include "gpssend.h"
#include "gpsread.h"
#include "gpsutil.h"
#include "gpsapp.h"
#include "gpsprot.h"
#include "gpscom.h"
#include "gpsfmt.h"
#include "gpsmath.h"
#include "gpsnmea.h"
#include "gpsmem.h"
#include "gpsrqst.h"
#include "gpsinput.h"
#include "gpsproj.h"
#include "gpsnmeafmt.h"
#include "gpsnmeaget.h"

time_t gps_save_time;
double gps_save_lat;
double gps_save_lon;
extern int32  gps_save_id;
extern double gps_save_version;
extern char   gps_save_string[GPS_ARB_LEN];
extern int gps_is_usb;

extern struct COMMANDDATA COMMAND_ID[2];
extern struct LINKDATA LINK_ID[3];
extern struct GPS_MODEL_PROTOCOL GPS_MP[];

extern char *gps_marine_sym[];
extern char *gps_land_sym[];
extern char *gps_aviation_sym[];
extern char *gps_16_sym[];


#endif

#ifdef __cplusplus
}
#endif
