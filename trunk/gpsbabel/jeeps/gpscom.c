/********************************************************************
** @source JEEPS command functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0 
** @modified Dec 28 1999 Alan Bleasby. First version
** @modified Copyright (C) 2005, 2006 Robert Lipe
** @modified Copyright (C) 2007 Achim Schumacher
** @modified Copyright (C) 2010 Martin Buck
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
    case pA302:
       /* Units with A302 don't support track upload, so we convert the
        * track to a course on the fly and send that instead
        */
       ret = GPS_Command_Send_Track_As_Course(port, trk, n);
       break;
    default:
	GPS_Error("Send_Track: Unknown track protocol %d.", gps_trk_transfer);
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
** @param [w] lap [GPS_PLap **] pointer to lap array
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

/* @func GPS_Command_Get_Course ***************************************
**
** Get Courses from GPS. According to Garmin protocol specification, this
** includes getting all course laps, course tracks and course points
** from the device.
**
** @param [r] port [const char *] serial port
** @param [w] crs [GPS_PCourse **] pointer to course array
** @param [w] clp [GPS_PCourse_Lap **] pointer to course lap array
** @param [w] trk [GPS_PTrack **] pointer to track array
** @param [w] cpt [GPS_PCourse_Point **] pointer to course point array
** @param [w] n_clp [int32 **] pointer to number of lap entries
** @param [w] n_trk [int32 **] pointer to number of track entries
** @param [w] n_cpt [int32 **] pointer to number of course point entries
**
** @return [int32] number of course entries
************************************************************************/
int32  GPS_Command_Get_Course
                (const char *port,
                 GPS_PCourse **crs,
                 GPS_PCourse_Lap **clp,
                 GPS_PTrack **trk,
                 GPS_PCourse_Point **cpt,
                 int32 *n_clp,
                 int32 *n_trk,
                 int32 *n_cpt,
                 pcb_fn cb)
{
    int32 ret=0;

    if(gps_course_transfer == -1)
       return GPS_UNSUPPORTED;

    switch(gps_course_transfer)
    {
       case pA1006:
           ret = GPS_A1006_Get(port,crs,cb);
           break;
       default:
           GPS_Error("Get_Course: Unknown course protocol");
           return PROTOCOL_ERROR;
    }

    switch(gps_course_lap_transfer)
    {
       case pA1007:
           *n_clp = GPS_A1007_Get(port,clp, 0);
           break;
       default:
           GPS_Error("Get_Course: Unknown course lap protocol");
           return PROTOCOL_ERROR;
    }

    switch(gps_course_trk_transfer)
    {
        case pA1012:
           GPS_Error("Get_Course: Not implemented track protocol %d\n",
                            gps_trk_transfer);
           break;
        case pA302:
           *n_trk = GPS_A302_Get(port,trk,cb);
           break;
        default:
           GPS_Error("Get_Course: Unknown course track protocol %d\n",
                            gps_trk_transfer);
           return PROTOCOL_ERROR;
    }

    switch(gps_course_point_transfer)
    {
       case pA1008:
           *n_cpt = GPS_A1008_Get(port,cpt, 0);
           break;
       default:
           GPS_Error("Get_Course: Unknown course point protocol");
           return PROTOCOL_ERROR;
    }

    return ret;
}


/* @func GPS_Command_Send_Course ***************************************
**
** Send Courses to GPS. According to Garmin protocol specification, this
** includes sending all course laps, course tracks and course points
** to the device.
**
** Data integrity is being checked: only those course laps, tracks and
** points are sent to the device which belong to a course in the course
** array. Otherwise, those data will be silently dropped.
**
** @param [r] port [const char *] serial port
** @param [w] crs [GPS_PCourse **] course array
** @param [w] clp [GPS_PCourse_Lap *] course lap array
** @param [w] trk [GPS_PTrack *] track array
** @param [w] cpt [GPS_PCourse_Point *] course point array
** @param [w] n_crs [int32] number of course entries
** @param [w] n_clp [int32] number of lap entries
** @param [w] n_trk [int32] number of track entries
** @param [w] n_cpt [int32] number of course point entries
**
** @return [int32] Success
************************************************************************/
int32  GPS_Command_Send_Course
                (const char *port,
                 GPS_PCourse *crs,
                 GPS_PCourse_Lap *clp,
                 GPS_PTrack *trk,
                 GPS_PCourse_Point *cpt,
                 int32 n_crs,
                 int32 n_clp,
                 int32 n_trk,
                 int32 n_cpt)
{
    gpsdevh *fd;
    int32 ret_crs=0;
    int32 ret_clp=0;
    int32 ret_trk=0;
    int32 ret_cpt=0;

    if(gps_course_transfer == -1)
       return GPS_UNSUPPORTED;

    /* Initialize device communication:
     * In contrast to other transfer protocols, this has to be handled here;
     * shutting off communication in between the different parts
     * could lead to data corruption on the device because all the courses
     * and their associated lap and track data have to be sent in one
     * transaction.
     */
    if(!GPS_Device_On(port,&fd))
        return gps_errno;

    switch(gps_course_transfer)
    {
       case pA1006:
           ret_crs = GPS_A1006_Send(port,crs,n_crs,fd);
           break;
       default:
           GPS_Error("Send_Course: Unknown course protocol");
           return PROTOCOL_ERROR;
    }

    switch(gps_course_lap_transfer)
    {
       case pA1007:
           ret_clp = GPS_A1007_Send(port,clp,n_clp,fd);
           break;
       default:
           GPS_Error("Send_Course: Unknown course lap protocol");
           return PROTOCOL_ERROR;
    }

    switch(gps_course_trk_transfer)
    {
        case pA1012:
           GPS_Error("Send_Course: Not implemented track protocol %d\n",
                            gps_trk_transfer);
           break;
        case pA302:
           ret_trk = GPS_A302_Send(port,trk,n_trk,fd);
           break;
        default:
           GPS_Error("Send_Course: Unknown course track protocol %d\n",
                            gps_trk_transfer);
           return PROTOCOL_ERROR;
    }

    switch(gps_course_point_transfer)
    {
       case pA1008:
           ret_cpt = GPS_A1008_Send(port,cpt,n_cpt,fd);
           break;
       default:
           GPS_Error("Send_Course: Unknown course point protocol");
           return PROTOCOL_ERROR;
    }

    if(!GPS_Device_Off(fd))
        return gps_errno;


    return ret_crs * ret_clp * ret_trk * ret_cpt;
}


static uint32 Unique_Course_Index(GPS_PCourse *crs, int n_crs)
{
    uint32 idx;
    int i;

    for (idx=0; ; idx++)
    {
       for (i=0; i<n_crs; i++)
           if (crs[i]->index==idx)
               break; /* Already have this index */
       if (i>=n_crs)
           return idx; /* Found unused index */
    }
}


static uint32 Unique_Track_Index(GPS_PCourse *crs, int n_crs)
{
    uint32 idx;
    int i;

    for (idx=0; ; idx++)
    {
       for (i=0; i<n_crs; i++)
           if (crs[i]->track_index==idx)
               break; /* Already have this index */
       if (i>=n_crs)
           return idx; /* Found unused index */
    }
}


static void
Course_Garbage_Collect(GPS_PCourse *crs, int *n_crs,
                       GPS_PCourse_Lap *clp, int *n_clp,
                       GPS_PTrack *ctk, int *n_ctk,
                       GPS_PCourse_Point *cpt, int *n_cpt) {
    int i, j;

    /* Remove courses with duplicate names, keeping newest */
restart_courses:
    for (i=*n_crs-1; i>0; i--)
    {
       for (j=i-1; j>=0; j--)
       {
           if (!strcmp(crs[i]->course_name, crs[j]->course_name))
           {
               /* Remove course */
               GPS_Course_Del(&crs[j]);
               memmove(&crs[j], &crs[j+1], (*n_crs-j-1)*sizeof(*crs));
               (*n_crs)--;
               goto restart_courses;
           }
       }
    }

  /* Remove unreferenced laps */
restart_laps:
  for (i=0; i<*n_clp; i++)
  {
    for (j=0; j<*n_crs; j++)
      if (crs[j]->index == clp[i]->course_index)
        break;
    if (j>=*n_crs)
    {
      /* Remove lap */
      GPS_Course_Lap_Del(&clp[i]);
      memmove(&clp[i], &clp[i+1], (*n_clp-i-1)*sizeof(*clp));
      (*n_clp)--;
      goto restart_laps;
    }
  }

  /* Remove unreferenced tracks */
restart_tracks:
  for (i=0; i<*n_ctk; i++)
  {
    uint32 trk_idx;

    if (!ctk[i]->ishdr)
      continue;
    trk_idx = strtoul(ctk[i]->trk_ident, NULL, 0);
    for (j=0; j<*n_crs; j++)
      if (crs[j]->index == trk_idx)
        break;
    if (j>=*n_crs)
    {
      /* Remove track */
      for (j=i; j<*n_ctk; j++)
      {
        if (j!=i && ctk[j]->ishdr)
          break;
        GPS_Track_Del(&ctk[j]);
      }
      memmove(&ctk[i], &ctk[j], (*n_ctk-j)*sizeof(*ctk));
      *(n_ctk) -= j-i;
      goto restart_tracks;
    }
  }

  /* Remove unreferenced course points */
restart_course_points:
  for (i=0; i<*n_cpt; i++)
  {
    for (j=0; j<*n_crs; j++)
      if (crs[j]->index == cpt[i]->course_index)
        break;
    if (j>=*n_crs)
    {
      /* Remove course point */
      GPS_Course_Point_Del(&cpt[i]);
      memmove(&cpt[i], &cpt[i+1], (*n_cpt-i-1)*sizeof(*cpt));
      (*n_cpt)--;
      goto restart_course_points;
    }
  }
}


/* @func GPS_Command_Send_Track_As_Course ******************************
**
** Convert track log to course, then send to GPS. Since sending a course
** to the device will erase all existing courses regardless of their
** name or index, we first have to download all courses, merge the new
** one and then send all courses at once.
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PTrack *] track array
** @param [r] n [int32] number of track entries
**
** @return [int32] success
************************************************************************/

int32 GPS_Command_Send_Track_As_Course(const char *port, GPS_PTrack *trk, int32 n)
{
    GPS_PCourse *crs = NULL;
    GPS_PCourse_Lap *clp = NULL;
    GPS_PTrack *ctk = NULL;
    GPS_PCourse_Point *cpt = NULL;
    int n_crs, n_clp=0, n_ctk=0, n_cpt=0;
    int i, trk_end, trk_crs;
    int32 ret;

    /* Read existing courses from device */
    n_crs = GPS_Command_Get_Course(port, &crs, &clp, &ctk, &cpt, &n_clp, &n_ctk, &n_cpt, NULL);
    if (n_crs < 0) return n_crs;

    /* Create new course+lap+track points for each track */
    trk_crs = n_crs;
    for (i=0;i<n;i++) {
       if (!trk[i]->ishdr)
           continue;

       /* Find end of track */
       for (trk_end=i; trk_end<n-1; trk_end++)
           if (trk[trk_end+1]->ishdr)
               break;
       if (trk_end==i)
           trk_end=0; /* Empty track */

       /* Create & append course */
       crs = realloc(crs, (n_crs+1) * sizeof(GPS_PCourse));
       if (!crs) return MEMORY_ERROR;
       crs[n_crs] = GPS_Course_New();
       if (!crs[n_crs]) return MEMORY_ERROR;

       crs[n_crs]->index = Unique_Course_Index(crs, n_crs);
       strncpy(crs[n_crs]->course_name, trk[i]->trk_ident,
               sizeof(crs[n_crs]->course_name)-1);

       crs[n_crs]->track_index = Unique_Track_Index(crs, n_crs);

       /* Create & append new lap */
       clp = realloc(clp, (n_clp+1) * sizeof(GPS_PCourse_Lap));
       if (!clp) return MEMORY_ERROR;
       clp[n_clp] = GPS_Course_Lap_New();
       if (!clp[n_clp]) return MEMORY_ERROR;

       clp[n_clp]->course_index = crs[n_crs]->index; /* Index of associated course */
       clp[n_clp]->lap_index = 0; /* Lap index, unique per course */
       clp[n_clp]->total_time = 0; /* FIXME: Calculate */
       clp[n_clp]->total_dist = 0; /* FIXME: Calculate */
       if (trk_end)
       {
           clp[n_clp]->begin_lat = trk[i+1]->lat;
           clp[n_clp]->begin_lon = trk[i+1]->lon;
           clp[n_clp]->end_lat = trk[trk_end]->lat;
           clp[n_clp]->end_lon = trk[trk_end]->lon;
       }
       else
       {
           clp[n_clp]->begin_lat = 0x7fffffff;
           clp[n_clp]->begin_lon = 0x7fffffff;
           clp[n_clp]->end_lat = 0x7fffffff;
           clp[n_clp]->end_lon = 0x7fffffff;
       }
       clp[n_clp]->avg_heart_rate = 0; /* FIXME: Calculate */
       clp[n_clp]->max_heart_rate = 0; /* FIXME: Calculate */
       clp[n_clp]->intensity = 0;
       clp[n_clp]->avg_cadence = 0xff; /* FIXME: Calculate */
       n_crs++;
       n_clp++;
    }

    /* Append new track points */
    ctk = realloc(ctk, (n_ctk+n) * sizeof(GPS_PTrack));
    if (!ctk) return MEMORY_ERROR;

    for (i=0;i<n;i++) {
       ctk[n_ctk] = GPS_Track_New();
       if (!ctk[n_ctk]) return MEMORY_ERROR;
       *ctk[n_ctk] = *trk[i];

       if (ctk[n_ctk]->ishdr)
       {
           /* Index of new track, must match the track index in associated course */
           memset(ctk[n_ctk]->trk_ident, 0, sizeof(ctk[n_ctk]->trk_ident));
           sprintf(ctk[n_ctk]->trk_ident, "%d", crs[trk_crs]->track_index);
           trk_crs++;
       }
       n_ctk++;
    }

    /* Remove course data that's no longer needed */
    Course_Garbage_Collect(crs, &n_crs, clp, &n_clp, ctk, &n_ctk, cpt, &n_cpt);

    /* Finally send courses including new ones to device */
    ret = GPS_Command_Send_Course(port, crs, clp, ctk, cpt,
                                  n_crs, n_clp, n_ctk, n_cpt);

    for (i=0;i<n_crs;i++)
    {
       GPS_Course_Del(&crs[i]);
    }
    free(crs);

    for (i=0;i<n_clp;i++)
    {
       GPS_Course_Lap_Del(&clp[i]);
    }
    free(clp);

    for (i=0;i<n_ctk;i++)
    {
       GPS_Track_Del(&ctk[i]);
    }
    free(ctk);

    for (i=0;i<n_cpt;i++)
    {
       GPS_Course_Point_Del(&cpt[i]);
    }
    free(cpt);

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

int32  GPS_Command_Get_Course_Limits(const char *port, void **lap, int (*cb)(int, struct GPS_SWay **)){
  return 0;
}  
