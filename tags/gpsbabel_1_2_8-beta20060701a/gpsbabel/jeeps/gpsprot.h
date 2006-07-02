#ifdef __cplusplus
extern "C"
{
#endif

#ifndef gpsprotocols_h
#define gpsprotocols_h

#include "gps.h"

/* 
 *  Link protocols
 */

struct LINKDATA
{
    UC Pid_Ack_Byte;
    UC Pid_Command_Data;
    UC Pid_Xfer_Cmplt;
    UC Pid_Date_Time_Data;
    UC Pid_Position_Data;
    UC Pid_Prx_Wpt_Data;
    UC Pid_Nak_Byte;
    UC Pid_Records;
    UC Pid_Rte_Hdr;
    UC Pid_Rte_Wpt_Data;
    UC Pid_Almanac_Data;
    UC Pid_Trk_Data;
    UC Pid_Wpt_Data;
    UC Pid_Pvt_Data;
    UC Pid_Rte_Link_Data;
    UC Pid_Trk_Hdr;
    UC Pid_Protocol_Array;
    UC Pid_Product_Rqst;
    UC Pid_Product_Data;
    UC Pid_Wpt_Cat_Data;
}
;





/*
 * Command types
 */

#define pA010 10
#define pA011 11

int32 gps_device_command;


struct COMMANDDATA
{
    US Cmnd_Abort_Transfer;
    US Cmnd_Transfer_Alm;
    US Cmnd_Transfer_Posn;
    US Cmnd_Transfer_Prx;
    US Cmnd_Transfer_Rte;
    US Cmnd_Transfer_Time;
    US Cmnd_Transfer_Trk;
    US Cmnd_Transfer_Wpt;
    US Cmnd_Turn_Off_Pwr;
    US Cmnd_Start_Pvt_Data;
    US Cmnd_Stop_Pvt_Data;
    US Cmnd_FlightBook_Transfer;
    US Cmnd_Transfer_Lap;
    US Cmnd_Transfer_Wpt_Cats;
    US Cmnd_Transfer_Runs;
}
;




/*
 * Waypoint Transfer Protocol
 */
#define pA100 100
int32 gps_waypt_transfer;

/*
 * Waypoint category transfer protocol
 */
#define pA101 101
int32 gps_category_transfer;

/*
 * Route Transfer Protocol
 */
#define pA200 200
#define pA201 201
int32 gps_route_transfer;

/*
 * Track Log Transfer Protocol
 */
#define pA300 300
#define pA301 301
#define pA302 302
#define pA304 304
int32 gps_trk_transfer;

/*
 *  Proximity Waypoint Transfer Protocol
 */
#define pA400 400
int32 gps_prx_waypt_transfer;

/*
 *  Almanac Transfer Protocol
 */
#define pA500 500
int32 gps_almanac_transfer;


/*
 *  Date Time Transfer
 */
#define pA600 600
int32 gps_date_time_transfer;


/*
 *  Position
 */
#define pA700 700
int32 gps_position_transfer;


/*
 *  Pvt
 */
#define pA800 800
int32 gps_pvt_transfer;

/*
 * Lap Data Transfer
 */
#define pA906 906
int32 gps_lap_transfer;

#define pA1000 1000
int32 gps_run_transfer;



/*
 * Waypoint D Type
 */
#define pD100 100
#define pD101 101
#define pD102 102
#define pD103 103
#define pD104 104
#define pD105 105
#define pD106 106
#define pD107 107
#define pD108 108
#define pD109 109
#define pD110 110
#define pD150 150
#define pD151 151
#define pD152 152
#define pD154 154
#define pD155 155

int32 gps_rte_type;
int32 gps_waypt_type;

/*
 * Waypoint category types
 */
#define pD120 120
int32 gps_category_type;

/*
 * Rte Header Type
 */
#define pD200 200
#define pD201 201
#define pD202 202
int32 gps_rte_hdr_type;


/*
 * Rte Link Type
 */
#define pD210 210
int32 gps_rte_link_type;


/*
 *  Trk Point Type
 */
#define pD300 300
#define pD301 301
#define pD302 302
#define pD303 303
#define pD304 304
int32 gps_trk_type;


/*
 *  Trk Header Type
 */
#define pD310 310
#define pD311 311
#define pD312 312
int32 gps_trk_hdr_type;



/*
 * Prx Wpt Type
 */
#define pD400 400
#define pD403 403
#define pD450 450

int32 gps_prx_waypt_type;


/*
 * Almanac Type
 */
#define pD500 500
#define pD501 501
#define pD550 550
#define pD551 551

int32 gps_almanac_type;


/*
 * Date Time Type
 */
#define pD600 600

int32 gps_date_time_type;



/*
 * Position Type
 */
#define pD700 700

int32 gps_position_type;



/*
 * Pvt Data Type
 */
#define pD800 800

int32 gps_pvt_type;

/*
 * Lap Data Type
 */
#define pD906 906

int32 gps_lap_type;

/*
 * Link protocol type
 */
#define pL000 0
#define pL001 1
#define pL002 2

int32 gps_link_type;



struct GPS_MODEL_PROTOCOL
{
    US    id;
    int32 link;
    int32 command;
    int32 wayptt;
    int32 wayptd;
    int32 rtea;
    int32 rted0;
    int32 rted1;
    int32 trka;
    int32 trkd;
    int32 prxa;
    int32 prxd;
    int32 alma;
    int32 almd;
}
;

US     GPS_Protocol_Version_Change(US id, US version);
int32  GPS_Protocol_Table_Set(US id);
void   GPS_Protocol_Error(US tag, US data);
void   GPS_Unknown_Protocol_Print(void);

    
#endif

#ifdef __cplusplus
}
#endif
