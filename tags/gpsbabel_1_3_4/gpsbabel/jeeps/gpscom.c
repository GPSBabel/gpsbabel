/********************************************************************
** @source JEEPS command functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0 
** @modified Dec 28 1999 Alan Bleasby. First version
** @modified Copyright (C) 2005, 2006 Robert Lipe
** @@
** 
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Library General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
** 
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Library General Public License for more details.
** 
** You should have received a copy of the GNU Library General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
********************************************************************/
#include "gps.h"
#include <stdio.h>


/* @func GPS_Command_Off ***********************************************
**
** Turn off power on GPS
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Off(const char *port)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;

    GPS_Util_Little();

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Turn_Off_Pwr);
   
    /* robertl - LINK_ID isn't set yet.  Hardcode it to Garmin spec value */ 
    GPS_Make_Packet(&tra, 10, /* LINK_ID[gps_link_type].Pid_Command_Data, */
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;

    if(!GPS_Device_Chars_Ready(fd))
    {
	if(!GPS_Get_Ack(fd, &tra, &rec))
	    return gps_errno;
	GPS_User("Power off command acknowledged");
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}


/* @func GPS_Command_Get_Waypoint ***************************************
**
** Get waypoint from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] pointer to waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/

int32 GPS_Command_Get_Waypoint(const char *port, GPS_PWay **way, pcb_fn cb)
{
    int32 ret=0;

    /* 
     * It's a bit tacky to do this up front without ticking the
     * progress meter, but this come in pretty quickly...
     */
    if (gps_category_transfer) {
	ret = GPS_A101_Get(port);
	if (!ret) {
fatal("blah");
	   return PROTOCOL_ERROR;
	}
	
    }

    switch(gps_waypt_transfer)
    {
    case pA100:
	ret = GPS_A100_Get(port,way, cb);
	break;
    default:
	GPS_Error("Get_Waypoint: Unknown waypoint protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Waypoint ******************************************
**
** Send waypoints to GPS
**
** @param [r] port [const char *] serial port
** @param [r] way [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Waypoint(const char *port, GPS_PWay *way, int32 n, int (*cb)(struct GPS_SWay **))
{
    int32 ret=0;

    switch(gps_waypt_transfer)
    {
    case pA100:
	ret = GPS_A100_Send(port, way, n, cb);
	break;
    default:
	GPS_Error("Send_Waypoint: Unknown waypoint protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    


/* @func GPS_Command_Get_Route **************************************
**
** Get Route(s) from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] pointer to waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/

int32 GPS_Command_Get_Route(const char *port, GPS_PWay **way)
{
    int32 ret=0;

    switch(gps_route_transfer)
    {
    case pA200:
	ret = GPS_A200_Get(port,way);
	break;
    case pA201:
	ret = GPS_A201_Get(port,way);
	break;
    default:
	GPS_Error("Get_Route: Unknown route protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Route ****************************************
**
** Send route(s) to GPS
**
** @param [r] port [const char *] serial port
** @param [r] way [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Route(const char *port, GPS_PWay *way, int32 n)
{
    int32 ret=0;


    switch(gps_route_transfer)
    {
    case pA200:
	ret = GPS_A200_Send(port, way, n);
	break;
    case pA201:
	ret = GPS_A201_Send(port, way, n);
	break;
    default:
	GPS_Error("Send_Route: Unknown route protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    


/* @func GPS_Command_Get_Track ***************************************
**
** Get track log from GPS
**
** @param [r] port [const char *] serial port
** @param [w] trk [GPS_PTrack **] pointer to track array
**
** @return [int32] number of track entries
************************************************************************/

int32 GPS_Command_Get_Track(const char *port, GPS_PTrack **trk, pcb_fn cb)
{
    int32 ret=0;

    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_trk_transfer)
    {
    case pA300:
	ret = GPS_A300_Get(port,trk,cb);
	break;
    case pA301:
    case pA302:
	ret = GPS_A301_Get(port,trk,cb);
	break;
    default:
	GPS_Error("Get_Track: Unknown track protocol %d\n", gps_trk_transfer);
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Track ******************************************
**
** Send track log to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PTrack *] track array
** @param [r] n [int32] number of track entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Track(const char *port, GPS_PTrack *trk, int32 n)
{
    int32 ret=0;

    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_trk_transfer)
    {
    case pA300:
	ret = GPS_A300_Send(port, trk, n);
	break;
    case pA301:
	ret = GPS_A301_Send(port, trk, n);
	break;
    default:
	GPS_Error("Send_Track: Unknown track protocol");
	break;
    }

    return ret;
}    


/* @func GPS_Command_Get_Proximity **************************************
**
** Get proximitywaypoint from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] pointer to waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/

int32 GPS_Command_Get_Proximity(const char *port, GPS_PWay **way)
{
    int32 ret=0;

    if(gps_prx_waypt_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_prx_waypt_transfer)
    {
    case pA400:
	ret = GPS_A400_Get(port,way);
	break;
    default:
	GPS_Error("Get_Proximity: Unknown proximity protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Proximity ******************************************
**
** Send proximity waypoints to GPS
**
** @param [r] port [const char *] serial port
** @param [r] way [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Proximity(const char *port, GPS_PWay *way, int32 n)
{
    int32 ret=0;


    if(gps_prx_waypt_transfer == -1)
	return GPS_UNSUPPORTED;


    switch(gps_prx_waypt_transfer)
    {
    case pA400:
	ret = GPS_A400_Send(port, way, n);
	break;
    default:
	GPS_Error("Send_Proximity: Unknown proximity protocol");
	break;
    }

    return ret;
}    



/* @func GPS_Command_Get_Almanac ***************************************
**
** Get almanac from GPS
**
** @param [r] port [const char *] serial port
** @param [w] alm [GPS_PAlmanac **] pointer to almanac array
**
** @return [int32] number of almanac entries
************************************************************************/

int32 GPS_Command_Get_Almanac(const char *port, GPS_PAlmanac **alm)
{
    int32 ret=0;

    if(gps_almanac_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_almanac_transfer)
    {
    case pA500:
	ret = GPS_A500_Get(port,alm);
	break;
    default:
	GPS_Error("Get_Almanac: Unknown almanac protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Almanac ******************************************
**
** Send almanac to GPS
**
** @param [r] port [const char *] serial port
** @param [r] alm [GPS_PAlmanac *] almanac array
** @param [r] n [int32] number of almanac entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Almanac(const char *port, GPS_PAlmanac *alm, int32 n)
{
    int32 ret=0;

	if(gps_almanac_transfer == -1)
		return GPS_UNSUPPORTED;

    switch(gps_almanac_transfer)
    {
    case pA500:
	ret = GPS_A500_Send(port, alm, n);
	break;
    default:
	GPS_Error("Send_Almanac: Unknown almanac protocol");
	break;
    }

    return ret;
}    



/* @func GPS_Command_Get_Time ******************************************
**
** Get time from GPS
**
** @param [r] port [const char *] serial port
**
** @return [time_t] unix-style time
************************************************************************/

time_t GPS_Command_Get_Time(const char *port)
{
    time_t ret=0;

    switch(gps_date_time_transfer)
    {
    case pA600:
	ret = GPS_A600_Get(port);
	break;
    /* 
     * If the unit doesn't support it (i.e. a C320 in charging mode), 
     * but don't treat as error; return as zero.
     */
    case -1:
	return 0;
    default:
	GPS_Error("Get_Time: Unknown date/time protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Time ******************************************
**
** Set GPS time
**
** @param [r] port [const char *] serial port
** @param [r] Time [time_t] unix-style time
**
** @return [int32] true if OK
************************************************************************/

int32 GPS_Command_Send_Time(const char *port, time_t Time)
{
    time_t ret=0;

    switch(gps_date_time_transfer)
    {
    case pA600:
	ret = GPS_A600_Send(port, Time);
	break;
    default:
	GPS_Error("Send_Time: Unknown date/time protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    




/* @func GPS_Command_Get_Position ***************************************
**
** Get position from GPS
**
** @param [r] port [const char *] serial port
** @param [w] lat [double *] latitude  (deg)
** @param [w] lon [double *] longitude (deg)
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Get_Position(const char *port, double *lat, double *lon)
{
    int32 ret=0;

    switch(gps_position_transfer)
    {
    case pA700:
	ret = GPS_A700_Get(port,lat,lon);
	break;
    /* 
     * If the unit doesn't support it (i.e. a C320 in charging mode), 
     *  zero lat/lon, but don't treat as error. 
     */
    case -1:
	*lat = *lon = 0.0;
	break;
    default:
	GPS_Error("Get_Position: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Send_Position ******************************************
**
** Set GPS position
**
** @param [r] port [const char *] serial port
** @param [r] lat [double] latitude  (deg)
** @param [r] lon [double] longitude (deg)
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Position(const char *port, double lat, double lon)
{
    int32 ret=0;

    switch(gps_position_transfer)
    {
    case pA700:
	ret = GPS_A700_Send(port, lat, lon);
	break;
    default:
	GPS_Error("Send_Position: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    
    

/* @func GPS_Command_Pvt_On ********************************************
**
** Instruct GPS to start sending Pvt data every second
**
** @param [r] port [const char *] serial port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success if supported and GPS starts sending
************************************************************************/

int32 GPS_Command_Pvt_On(const char *port, gpsdevh **fd)
{
    int32 ret=0;


    if(gps_pvt_transfer == -1)
	return GPS_UNSUPPORTED;


    switch(gps_pvt_transfer)
    {
    case pA800:
	ret = GPS_A800_On(port,fd);
	break;
    default:
	GPS_Error("Pvt_On: Unknown position protocol");
	return PROTOCOL_ERROR;
    }


    return ret;
}    



/* @func GPS_Command_Pvt_Off ********************************************
**
** Instruct GPS to stop sending Pvt data every second
**
** @param [r] port [const char *] serial port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Pvt_Off(const char *port, gpsdevh **fd)
{
    int32 ret=0;

    
    if(gps_pvt_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_pvt_transfer)
    {
    case pA800:
	ret = GPS_A800_Off(port,fd);
	break;
    default:
	GPS_Error("Pvt_Off: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    



/* @func GPS_Command_Pvt_Get ********************************************
**
** Get a single PVT info entry
**
** @param [w] fd [int32 *] file descriptor
** @param [w] pvt [GPS_PPvt_Data *] pvt data structure to fill
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Pvt_Get(gpsdevh **fd, GPS_PPvt_Data *pvt)
{
    int32 ret=0;

    if(gps_pvt_transfer == -1)
	return GPS_UNSUPPORTED;

    (*pvt)->fix = 0;

    switch(gps_pvt_transfer)
    {
    case pA800:
	ret = GPS_A800_Get(fd,pvt);
	break;
    default:
	GPS_Error("Pvt_Get: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    return ret;
}    

/* @func GPS_Command_Get_Lap ***************************************
**
** Get lap from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PLap **] pointer to lap array
**
** @return [int32] number of lap entries
************************************************************************/

int32 GPS_Command_Get_Lap(const char *port, GPS_PLap **lap, pcb_fn cb)
{
    int32 ret=0;

    if(gps_lap_transfer == -1)
	return GPS_UNSUPPORTED;

    switch(gps_lap_transfer)
    {
	case pA906:
	    ret = GPS_A906_Get(port,lap, cb);
	    break;
	default:
	    GPS_Error("Get_Lap: Unknown lap protocol");
	    return PROTOCOL_ERROR;
    }

    return ret;
}    
 /*Stubs for unimplemented stuff*/
int32  GPS_Command_Get_Workout(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  
int32  GPS_Command_Get_Fitness_User_Profile(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  
int32  GPS_Command_Get_Workout_Limits(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  
int32  GPS_Command_Get_Course(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  
int32  GPS_Command_Get_Course_Limits(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  

