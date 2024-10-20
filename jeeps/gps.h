#ifndef JEEPS_GPS_H_INCLUDED_
#define JEEPS_GPS_H_INCLUDED_

#include "defs.h"
#include "jeeps/gpsport.h"
#include <ctime>

#define FRAMING_ERROR  -1
#define PROTOCOL_ERROR -2
#define HARDWARE_ERROR -3
#define SERIAL_ERROR   -4
#define MEMORY_ERROR   -5
#define GPS_UNSUPPORTED -6
#define INPUT_ERROR -7

#define MAX_GPS_PACKET_SIZE	1024
#define GPS_TIME_OUT		5

#define DLE 0x10
#define ETX 0x03


extern int32_t gps_errno;
extern int32_t gps_warning;
extern int32_t gps_error;
extern int32_t gps_user;
extern int32_t gps_show_bytes;
extern char gps_categories[16][17];


struct GPS_Packet {
  US type{0};
  uint32_t n{0};
  UC data[MAX_GPS_PACKET_SIZE] {};
};


struct GPS_Serial_Packet {
  UC dle;
  UC type;
  UC n;
  UC* data;
  UC chk;
  UC edle;
  UC etx;
};


typedef struct GPS_SPvt_Data_Type {
  float alt;
  float epe;
  float eph;
  float epv;
  int16_t fix;
  double tow;
  double lat;
  double lon;
  float east;
  float north;
  float up;
  float msl_hght;
  int16_t leap_scnds;
  int32_t wn_days;
} GPS_OPvt_Data, *GPS_PPvt_Data;



typedef struct GPS_STrack {
  double   lat;		/* Degrees */
  double   lon;		/* Degrees */
  time_t   Time;		/* Unix time */
  float    alt;		/* Altitude */
  float    dpth;		/* Depth    */
  float    temperature;	/* Temperature.  Degrees Celsius. */
  int      temperature_populated; /* True if above is valid. */
  unsigned char  heartrate;		/* Heartrate as in Garmin 301 */
  unsigned char  cadence;		/* Crank cadence as in Edge 305 */
  unsigned int   wsensor_pres:1; /* Wheel sensor present */
  unsigned int   tnew:1;	/* New track? */
  unsigned int   ishdr:1;	/* Track header? */
  unsigned int   no_latlon:1;	/* True if no valid lat/lon found. */
  int32_t dspl;		/* Display on map? */
  int32_t colour;		/* Colour */
  float    distance; /* distance traveled in meters.*/
  int      distance_populated; /* True if above is valid. */
  char     trk_ident[256];	/* Track identifier */
}
GPS_OTrack, *GPS_PTrack;



typedef struct GPS_SAlmanac {
  UC    svid;
  int16_t wn;
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


typedef struct GPS_SWay {
  char   ident[256];
  double lat;
  double lon;
  char   cmnt[256];
  float  dst;
  int32_t smbl;
  int32_t dspl;
  char   wpt_ident[256];
  char   lnk_ident[256];
  UC     subclass[18];
  int32_t colour;
  char   cc[2];
  UC     wpt_class;
  UC     alt_is_unknown;
  float  alt;
  char   city[24];
  char   state[2];
  char   name[30];
  char   facility[32];
  char   addr[52];
  char   cross_road[52];
  int32_t attr;
  float  dpth;
  int32_t idx;
  int32_t prot;
  int32_t isrte;
  int32_t rte_prot;
  UC     rte_num;
  char   rte_cmnt[20];
  char   rte_ident[256];
  int32_t islink;
  int32_t rte_link_class;
  char   rte_link_subclass[18];
  char   rte_link_ident[256];

  char     time_populated;	/* 1 if true */
  time_t   time;		/* Unix time */
  char     temperature_populated;
  float    temperature;		/* Degrees celsius. */
  uint16_t category;

} GPS_OWay, *GPS_PWay;

/*
 * Forerunner/Edge Lap data.
 */
typedef struct GPS_SLap {
  uint32_t index; /* unique index in device or -1 */
  time_t	start_time;
  uint32_t total_time;	/* Hundredths of a second */
  float	total_distance;	/* In meters */
  double	begin_lat;
  double	begin_lon;
  double	end_lat;
  double	end_lon;
  int16_t calories;
  uint32_t track_index; /* ref to track or -1 */
  float max_speed; /* In meters per second */
  unsigned char avg_heart_rate; /* In beats-per-minute, 0 if invalid */
  unsigned char max_heart_rate; /* In beats-per-minute, 0 if invalid */
  unsigned char intensity; /* Same as D1001 */
  unsigned char avg_cadence; /* In revolutions-per-minute, 0xFF if invalid */
  unsigned char trigger_method;
  /*Some D1015 unknown */
  /*    unsigned char unk1015_1;
  int16 unk1015_2;
  int16 unk1015_3;
  */
} GPS_OLap, *GPS_PLap;


typedef struct GPS_SCourse {
  uint32_t index;                    /* Unique among courses on device */
  char      course_name[16];          /* Null-terminated unique course name */
  uint32_t track_index;              /* Index of the associated track
                                         * Must be 0xFFFFFFFF if there is none*/
} GPS_OCourse, *GPS_PCourse;


typedef struct GPS_SCourse_Lap {
  uint32_t course_index;         /* Index of associated course */
  uint32_t lap_index;            /* This lap's index in the course */
  uint32_t total_time;           /* In hundredths of a second */
  float         total_dist;           /* [m] */
  double        begin_lat;            /* Starting position of the lap */
  double        begin_lon;            /* Invalid if lat,lon are 0x7FFFFFFF.*/
  double        end_lat;              /* Final position of the lap */
  double        end_lon;              /* Invalid if lat,lon are 0x7FFFFFFF.*/
  UC            avg_heart_rate;       /* In beats-per-minute, >0 */
  UC            max_heart_rate;       /* In beats-per-minute, >0 */
  UC            intensity;            /* 0=standard, active lap.
                                           1=rest lap in a workout */
  UC            avg_cadence;          /* In revolutions-per-minute */
} GPS_OCourse_Lap, *GPS_PCourse_Lap;

typedef struct GPS_SCourse_Point {
  char        name[11];               /* Null-terminated name */
  uint32_t course_index;           /* Index of associated course */
  time_t      track_point_time;       /* Time */
  UC          point_type;             /* generic = 0,
                                         * summit = 1,
                                         * valley = 2,
                                         * water = 3,
                                         * food = 4,
                                         * danger = 5,
                                         * left = 6,
                                         * right = 7,
                                         * straight = 8,
                                         * first_aid = 9,
                                         * fourth_category = 10,
                                         * third_category = 11,
                                         * second_category = 12,
                                         * first_category = 13,
                                         * hors_category = 14,
                                         * sprint = 15 */
} GPS_OCourse_Point, *GPS_PCourse_Point;

typedef struct GPS_SCourse_Limits {
  int32_t max_courses;
  int32_t max_course_laps;
  int32_t max_course_pnt;
  int32_t max_course_trk_pnt;
} GPS_OCourse_Limits, *GPS_PCourse_Limits;


using pcb_fn = int (*)(int, GPS_SWay**);

#include "jeeps/gpsdevice.h"
#include "jeeps/gpssend.h"
#include "jeeps/gpsread.h"
#include "jeeps/gpsutil.h"
#include "jeeps/gpsapp.h"
#include "jeeps/gpsprot.h"
#include "jeeps/gpscom.h"
#include "jeeps/gpsmath.h"
#include "jeeps/gpsmem.h"
#include "jeeps/gpsrqst.h"

extern time_t gps_save_time;
extern double gps_save_lat;
extern double gps_save_lon;
extern int32_t gps_save_id;
extern double gps_save_version;
extern char   gps_save_string[GPS_ARB_LEN];
extern int gps_is_usb;
extern int gps_baud_rate;

extern COMMANDDATA COMMAND_ID[2];
extern LINKDATA LINK_ID[3];
extern GPS_MODEL_PROTOCOL GPS_MP[];

extern const char* gps_marine_sym[];
extern const char* gps_land_sym[];
extern const char* gps_aviation_sym[];
extern const char* gps_16_sym[];


#endif // JEEPS_GPS_H_INCLUDED_
