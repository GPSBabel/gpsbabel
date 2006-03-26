/********************************************************************
** @source JEEPS constructor and deconstructor functions
**
** @author Copyright (C) 1999,2000 Alan Bleasby
** @version 1.0 
** @modified December 28th 1999 Alan Bleasby. First version
** @modified June 29th 2000 Alan Bleasby. NMEA additions
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
#include "garminusb.h"
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <limits.h>

/* @func GPS_Packet_New ***********************************************
**
** Packet constructor
**
** @return [GPS_PPacket] virgin packet
**********************************************************************/

GPS_PPacket GPS_Packet_New(void)
{
    GPS_PPacket ret;
    int hdr_size = sizeof(GPS_OPacket) ;
    if(!(ret=(GPS_PPacket )malloc(hdr_size)))
    
    {
	perror("malloc");
	fprintf(stderr,"GPS_Packet_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }
    if(!(ret->data = (UC *)malloc(MAX_GPS_PACKET_SIZE*sizeof(UC))))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Packet_New: Insufficient data memory");
	fflush(stderr);
	return NULL;
    }

    ret->dle = ret->edle = DLE;
    ret->etx = ETX;

    return ret;
}



/* @func GPS_Packet_Del ***********************************************
**
** Packet destructor
**
** @param [w] thys [GPS_PPacket *] packet to delete
**
** @return [void]
**********************************************************************/

void GPS_Packet_Del(GPS_PPacket *thys)
{
    free((void *)(*thys)->data);
    free((void *)*thys);

    return;
}



/* @func GPS_Pvt_New ***********************************************
**
** Pvt constructor
**
** @return [GPS_PPvt_Data] virgin pvt
**********************************************************************/

GPS_PPvt_Data GPS_Pvt_New(void)
{
    GPS_PPvt_Data ret;
    
    if(!(ret=(GPS_PPvt_Data)malloc(sizeof(GPS_OPvt_Data))))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Pvt_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }

    return ret;
}



/* @func GPS_Pvt_Del ***********************************************
**
** Pvt destructor
**
** @param [w] thys [GPS_PPvt_Data *] pvt to delete
**
** @return [void]
**********************************************************************/

void GPS_Pvt_Del(GPS_PPvt_Data *thys)
{
    free((void *)*thys);

    return;
}



/* @func GPS_Almanac_New ***********************************************
**
** Almanac constructor
**
** @return [GPS_PAlmanac] virgin almanac
**********************************************************************/

GPS_PAlmanac GPS_Almanac_New(void)
{
    GPS_PAlmanac ret;
    
    if(!(ret=(GPS_PAlmanac)malloc(sizeof(GPS_OAlmanac))))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Almanac_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }

    ret->svid=0xff;
    ret->wn  = -1;
    ret->hlth=0xff;

    return ret;
}



/* @func GPS_Almanac_Del ***********************************************
**
** Almanac destructor
**
** @param [w] thys [GPS_PAlmanac *] almanac to delete
**
** @return [void]
**********************************************************************/

void GPS_Almanac_Del(GPS_PAlmanac *thys)
{
    free((void *)*thys);

    return;
}



/* @func GPS_Track_New ***********************************************
**
** Track constructor
**
** @return [GPS_PTrack] virgin track
**********************************************************************/

GPS_PTrack GPS_Track_New(void)
{
    GPS_PTrack ret;
    
    if(!(ret=(GPS_PTrack)calloc(1,sizeof(GPS_OTrack))))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Track_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }

    return ret;
}



/* @func GPS_Track_Del ***********************************************
**
** Track destructor
**
** @param [w] thys [GPS_PTrack *] track to delete
**
** @return [void]
**********************************************************************/

void GPS_Track_Del(GPS_PTrack *thys)
{
    free((void *)*thys);

    return;
}



/* @func GPS_Way_New ***********************************************
**
** Waypoint constructor
**
** @return [GPS_PWay] virgin waypoint
**********************************************************************/

GPS_PWay GPS_Way_New(void)
{
    GPS_PWay ret;
    int32 i;
    
    if(!(ret=(GPS_PWay)xcalloc(sizeof(GPS_OWay),1)))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Way_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }

    /*
     * It turns out that the Way struct, initialized with zeros (not the
     * random stuff that we got with malloc, but REALLY initialized with
     * zeros from the calloc above actually does use C strings and it's
     * up to the various way_blah_send functions to zero/string pad things
     * as it goes.   So neutralize this.
     */
#if 0

    /* 
     * Mark all as "unused".  These appear in the same order as in the struct.
     */
#define BLANK(x)   memset(x, ' ',sizeof(x))
    BLANK(ret->ident);
    BLANK(ret->cmnt);
    BLANK(ret->wpt_ident);
    BLANK(ret->lnk_ident);
    BLANK(ret->subclass);
    BLANK(ret->name);
    BLANK(ret->facility);
    BLANK(ret->addr);
    BLANK(ret->cross_road);
    BLANK(ret->city);
    BLANK(ret->rte_cmnt);
    BLANK(ret->rte_ident);
    BLANK(ret->rte_link_subclass);
    BLANK(ret->rte_link_ident);
    BLANK(ret->state);
    BLANK(ret->cc);

    ret->facility[0] = 0;
    ret->addr[0] = 0;
    ret->wpt_ident[0] = 0;
#endif
    
    ret->lat = ret->lon = GPS_FLTMAX;
    ret->dst = 0;
    ret->smbl = ret->dspl = ret->colour = ret->alt = ret->prot = INT_MAX;

    if(gps_waypt_type==pD108)
    {
	ret->dst  = 0;
	ret->attr = 0x60;
	for(i=0;i<7;++i) ret->subclass[i] = 0;
	for(i=6;i<18;++i) ret->subclass[i] = 0xff;
    }
        
    return ret;
}



/* @func GPS_Way_Del ***********************************************
**
** Waypoint destructor
**
** @param [w] thys [GPS_Pway *] waypoint to delete
**
** @return [void]
**********************************************************************/

void GPS_Way_Del(GPS_PWay *thys)
{
    xfree((void *)*thys);

    return;
}

