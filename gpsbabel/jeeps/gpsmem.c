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
    
    if(!(ret=(GPS_PPacket )malloc(sizeof(GPS_OPacket))))
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
    
    if(!(ret=(GPS_PWay)malloc(sizeof(GPS_OWay))))
    {
	perror("malloc");
	fprintf(stderr,"GPS_Way_New: Insufficient memory");
	fflush(stderr);
	return NULL;
    }

    /* 
     * Mark all as "unused".  These appear in the same order as in the struct.
     */
#define BLANK(x)  memset(x, ' ',sizeof(x))
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
    free((void *)*thys);

    return;
}




/* @func GPS_Gsv_New ***********************************************
**
** Satellites in view constructor
**
** @return [GPS_PGsv] virgin siv
**********************************************************************/

GPS_PGsv GPS_Gsv_New(void)
{
    GPS_PGsv ret;
    
    if(!(ret=(GPS_PGsv)malloc(sizeof(GPS_OGsv))))
	return NULL;

    ret->valid = ret->inview = 0;
    *ret->elevation = *ret->azimuth = *ret->strength = '\0';

    return ret;
}



/* @func GPS_Gsv_Del ***********************************************
**
** Satellites in view destructor
**
** @param [w] thys [GPS_PGsv *] siv to delete
**
** @return [void]
**********************************************************************/

void GPS_Gsv_Del(GPS_PGsv *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Rme_New ***********************************************
**
** Position error constructor
**
** @return [GPS_PRme] virgin rme
**********************************************************************/

GPS_PRme GPS_Rme_New(void)
{
    GPS_PRme ret;
    
    if(!(ret=(GPS_PRme)malloc(sizeof(GPS_ORme))))
	return NULL;

    ret->valid = 0;
    ret->hpe = ret->vpe = ret->spe = (double)0.;
    
    return ret;
}



/* @func GPS_Rme_Del ***********************************************
**
** Position error destructor
**
** @param [w] thys [GPS_PRme *] posn error to delete
**
** @return [void]
**********************************************************************/

void GPS_Rme_Del(GPS_PRme *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Wpl_New ***********************************************
**
** Waypoint constructor
**
** @return [GPS_PWpl] virgin rme
**********************************************************************/

GPS_PWpl GPS_Wpl_New(void)
{
    GPS_PWpl ret;
    
    if(!(ret=(GPS_PWpl)malloc(sizeof(GPS_OWpl))))
	return NULL;

    ret->valid = 0;
    *ret->wpt = '\0';
    ret->lat = ret->lon = (double)0.;
    
    return ret;
}



/* @func GPS_Wpl_Del ***********************************************
**
** Waypoint destructor
**
** @param [w] thys [GPS_PWpl *] waypoint to delete
**
** @return [void]
**********************************************************************/

void GPS_Wpl_Del(GPS_PWpl *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Gll_New ***********************************************
**
** Position (geographic lat/lon) constructor
**
** @return [GPS_PGll] virgin gll
**********************************************************************/

GPS_PGll GPS_Gll_New(void)
{
    GPS_PGll ret;
    
    if(!(ret=(GPS_PGll)malloc(sizeof(GPS_OGll))))
	return NULL;

    ret->valid = 0;
    ret->time = (time_t)0;
    ret->lat = ret->lon = (double)0.;
    ret->dv='\0';
    
    return ret;
}



/* @func GPS_Gll_Del ***********************************************
**
** Position destructor
**
** @param [w] thys [GPS_PGll *] posn to delete
**
** @return [void]
**********************************************************************/

void GPS_Gll_Del(GPS_PGll *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Rmz_New ***********************************************
**
** Altitude constructor
**
** @return [GPS_PRmz] virgin altitude
**********************************************************************/

GPS_PRmz GPS_Rmz_New(void)
{
    GPS_PRmz ret;
    
    if(!(ret=(GPS_PRmz)malloc(sizeof(GPS_ORmz))))
	return NULL;

    ret->valid = ret->dim = ret->height = 0;
    
    return ret;
}



/* @func GPS_Rmz_Del ***********************************************
**
** Altitude destructor
**
** @param [w] thys [GPS_PRmz *] altitude to delete
**
** @return [void]
**********************************************************************/

void GPS_Rmz_Del(GPS_PRmz *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Rmm_New ***********************************************
**
** Datum constructor
**
** @return [GPS_PRmm] virgin datum
**********************************************************************/

GPS_PRmm GPS_Rmm_New(void)
{
    GPS_PRmm ret;
    
    if(!(ret=(GPS_PRmm)malloc(sizeof(GPS_ORmm))))
	return NULL;

    ret->valid = 0;
    *ret->datum = '\0';
    
    return ret;
}



/* @func GPS_Rmm_Del ***********************************************
**
** Datum destructor
**
** @param [w] thys [GPS_PRmm *] datum to delete
**
** @return [void]
**********************************************************************/

void GPS_Rmm_Del(GPS_PRmm *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Bod_New ***********************************************
**
** Bearing constructor
**
** @return [GPS_PBod] virgin bearing
**********************************************************************/

GPS_PBod GPS_Bod_New(void)
{
    GPS_PBod ret;
    
    if(!(ret=(GPS_PBod)malloc(sizeof(GPS_OBod))))
	return NULL;

    ret->valid = 0;
    *ret->dest = *ret->start = '\0';
    ret->true = ret->mag = (double)0.;
    
    return ret;
}



/* @func GPS_Bod_Del ***********************************************
**
** Bearing destructor
**
** @param [w] thys [GPS_PBod *] bearing to delete
**
** @return [void]
**********************************************************************/

void GPS_Bod_Del(GPS_PBod *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Rte_New ***********************************************
**
** Route (NMEA) constructor
**
** @return [GPS_PRte] virgin bearing
**********************************************************************/

GPS_PRte GPS_Rte_New(void)
{
    GPS_PRte ret;
    
    if(!(ret=(GPS_PRte)malloc(sizeof(GPS_ORte))))
	return NULL;

    ret->valid = ret->rte = 0;
    ret->type = '\0';
    ret->wpts = NULL;
    
    return ret;
}



/* @func GPS_Rte_Del ***********************************************
**
** Route (NMEA) destructor
**
** @param [w] thys [GPS_PRte *] route to delete
**
** @return [void]
**********************************************************************/

void GPS_Rte_Del(GPS_PRte *thys)
{
    if((*thys)->wpts)
	free((void *)(*thys)->wpts);
    free((void *)*thys);

    return;
}




/* @func GPS_Rmc_New ***********************************************
**
** Minimum recommended specific constructor
**
** @return [GPS_PRmc] virgin minimum
**********************************************************************/

GPS_PRmc GPS_Rmc_New(void)
{
    GPS_PRmc ret;
    
    if(!(ret=(GPS_PRmc)malloc(sizeof(GPS_ORmc))))
	return NULL;

    ret->valid = 0;
    ret->time = (time_t)0;
    *ret->date = ret->warn = '\0';
    ret->lat = ret->lon = ret->speed = ret->cmg = ret->magvar =
	(double)0.;
    
    return ret;
}



/* @func GPS_Rmc_Del ***********************************************
**
** Minimum recommended specific destructor
**
** @param [w] thys [GPS_PRmc *] rec min to delete
**
** @return [void]
**********************************************************************/

void GPS_Rmc_Del(GPS_PRmc *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Rmb_New ***********************************************
**
** Minimum recommended nav constructor
**
** @return [GPS_PRmb] virgin minimum nav
**********************************************************************/

GPS_PRmb GPS_Rmb_New(void)
{
    GPS_PRmb ret;
    
    if(!(ret=(GPS_PRmb)malloc(sizeof(GPS_ORmb))))
	return NULL;

    ret->valid = 0;
    *ret->owpt = *ret->dwpt = ret->warn = ret->correct = ret->alarm = '\0';
    ret->cross = ret->lat = ret->lon = ret->range = ret->true = ret->velocity =
	(double)0.;
    
    return ret;
}



/* @func GPS_Rmb_Del ***********************************************
**
** Minimum recommended nav destructor
**
** @param [w] thys [GPS_PRmb *] rec min nav to delete
**
** @return [void]
**********************************************************************/

void GPS_Rmb_Del(GPS_PRmb *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Gga_New ***********************************************
**
** Fix constructor
**
** @return [GPS_PGga] virgin fix
**********************************************************************/

GPS_PGga GPS_Gga_New(void)
{
    GPS_PGga ret;
    
    if(!(ret=(GPS_PGga)malloc(sizeof(GPS_OGga))))
	return NULL;

    ret->time = (time_t)0.;
    ret->valid = ret->qual = ret->nsat = ret->last = ret->dgpsid = 0;
    ret->hdil = ret->lat = ret->lon = ret->alt = ret->galt = (double)0.;
    
    return ret;
}



/* @func GPS_Gga_Del ***********************************************
**
** Fix destructor
**
** @param [w] thys [GPS_PGga *] fix to delete
**
** @return [void]
**********************************************************************/

void GPS_Gga_Del(GPS_PGga *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Gsa_New ***********************************************
**
** DOP constructor
**
** @return [GPS_PGsa] virgin DOP
**********************************************************************/

GPS_PGsa GPS_Gsa_New(void)
{
    GPS_PGsa ret;
    
    if(!(ret=(GPS_PGsa)malloc(sizeof(GPS_OGsa))))
	return NULL;

    ret->type = '\0';
    ret->valid = ret->nsat = ret->fix = 0;
    ret->pdop = ret->hdop = ret->vdop = (double)0.;
    
    return ret;
}



/* @func GPS_Gsa_Del ***********************************************
**
** DOP destructor
**
** @param [w] thys [GPS_PGsa *] DOP to delete
**
** @return [void]
**********************************************************************/

void GPS_Gsa_Del(GPS_PGsa *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Apb_New ***********************************************
**
** Autopilot B constructor
**
** @return [GPS_PApb] virgin autopilot
**********************************************************************/

GPS_PApb GPS_Apb_New(void)
{
    GPS_PApb ret;
    
    if(!(ret=(GPS_PApb)malloc(sizeof(GPS_OApb))))
	return NULL;

    ret->blink = ret->warn = ret->steer = ret->unit = ret->alarmc =
	ret->alarmp = *ret->wpt = '\0';
    ret->valid = 0;
    ret->edist = ret->od = ret->pd = ret->hdg = (double)0.;
    
    return ret;
}



/* @func GPS_Apb_Del ***********************************************
**
** Autopilot destructor
**
** @param [w] thys [GPS_PApb *] autopilot to delete
**
** @return [void]
**********************************************************************/

void GPS_Apb_Del(GPS_PApb *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Bwc_New ***********************************************
**
** Waypoint bng constructor
**
** @return [GPS_PBwc] virgin waypoint bng
**********************************************************************/

GPS_PBwc GPS_Bwc_New(void)
{
    GPS_PBwc ret;
    
    if(!(ret=(GPS_PBwc)malloc(sizeof(GPS_OBwc))))
	return NULL;

    *ret->wpt = '\0';
    ret->time = (time_t)0;
    ret->valid = 0;
    ret->lat = ret->lon = ret->true = ret->mag = ret->dist = (double)0.;
    
    return ret;
}



/* @func GPS_Bwc_Del ***********************************************
**
** Waypoint bearing destructor
**
** @param [w] thys [GPS_PBwc *] waypoint bearing to delete
**
** @return [void]
**********************************************************************/

void GPS_Bwc_Del(GPS_PBwc *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Bwr_New ***********************************************
**
** Waypoint bng rhumb constructor
**
** @return [GPS_PBwr] virgin waypoint bng
**********************************************************************/

GPS_PBwr GPS_Bwr_New(void)
{
    GPS_PBwr ret;
    
    if(!(ret=(GPS_PBwr)malloc(sizeof(GPS_OBwr))))
	return NULL;

    *ret->wpt = '\0';
    ret->time = (time_t)0;
    ret->valid = 0;
    ret->lat = ret->lon = ret->true = ret->mag = ret->dist = (double)0.;
    
    return ret;
}



/* @func GPS_Bwr_Del ***********************************************
**
** Waypoint bearing rhumb destructor
**
** @param [w] thys [GPS_PBwr *] waypoint bearing rhumb to delete
**
** @return [void]
**********************************************************************/

void GPS_Bwr_Del(GPS_PBwr *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Dbt_New ***********************************************
**
** Depth constructor
**
** @return [GPS_PDbt] virgin depth
**********************************************************************/

GPS_PDbt GPS_Dbt_New(void)
{
    GPS_PDbt ret;
    
    if(!(ret=(GPS_PDbt)malloc(sizeof(GPS_ODbt))))
	return NULL;

    ret->valid = 0;
    ret->f = ret->m = (double)0.;
    
    return ret;
}



/* @func GPS_Dbt_Del ***********************************************
**
** Depth destructor
**
** @param [w] thys [GPS_PDbt *] depth to delete
**
** @return [void]
**********************************************************************/

void GPS_Dbt_Del(GPS_PDbt *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Hdm_New ***********************************************
**
** Magnetic heading constructor
**
** @return [GPS_PHdm] virgin hdg
**********************************************************************/

GPS_PHdm GPS_Hdm_New(void)
{
    GPS_PHdm ret;
    
    if(!(ret=(GPS_PHdm)malloc(sizeof(GPS_OHdm))))
	return NULL;

    ret->valid = 0;
    ret->hdg = (double)0.;
    
    return ret;
}



/* @func GPS_Hdm_Del ***********************************************
**
** Magnetic heading destructor
**
** @param [w] thys [GPS_PHdm *] mag hdg to delete
**
** @return [void]
**********************************************************************/

void GPS_Hdm_Del(GPS_PHdm *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Hsc_New ***********************************************
**
** Heading to steer constructor
**
** @return [GPS_PHsc] virgin hdg
**********************************************************************/

GPS_PHsc GPS_Hsc_New(void)
{
    GPS_PHsc ret;
    
    if(!(ret=(GPS_PHsc)malloc(sizeof(GPS_OHsc))))
	return NULL;

    ret->valid = 0;
    ret->true = ret->mag = (double)0.;
    
    return ret;
}



/* @func GPS_Hsc_Del ***********************************************
**
** Heading to steer destructor
**
** @param [w] thys [GPS_PHsc *] hdg to delete
**
** @return [void]
**********************************************************************/

void GPS_Hsc_Del(GPS_PHsc *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Mtw_New ***********************************************
**
** Water temp constructor
**
** @return [GPS_PMtw] virgin temp
**********************************************************************/

GPS_PMtw GPS_Mtw_New(void)
{
    GPS_PMtw ret;
    
    if(!(ret=(GPS_PMtw)malloc(sizeof(GPS_OMtw))))
	return NULL;

    ret->valid = 0;
    ret->T = (double)0.;
    
    return ret;
}



/* @func GPS_Mtw_Del ***********************************************
**
** Water temperature destructor
**
** @param [w] thys [GPS_PMtw *] water temp to delete
**
** @return [void]
**********************************************************************/

void GPS_Mtw_Del(GPS_PMtw *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_R00_New ***********************************************
**
** Waypoint list constructor
**
** @return [GPS_PR00] virgin wpt list
**********************************************************************/

GPS_PR00 GPS_R00_New(void)
{
    GPS_PR00 ret;
    
    if(!(ret=(GPS_PR00)malloc(sizeof(GPS_OR00))))
	return NULL;

    ret->valid = 0;
    *ret->wpts='\0';
    
    return ret;
}



/* @func GPS_R00_Del ***********************************************
**
** Waypoint list destructor
**
** @param [w] thys [GPS_PR00 *] waypoint list to delete
**
** @return [void]
**********************************************************************/

void GPS_R00_Del(GPS_PR00 *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Vhw_New ***********************************************
**
** Water speed constructor
**
** @return [GPS_PVhw] virgin water speed
**********************************************************************/

GPS_PVhw GPS_Vhw_New(void)
{
    GPS_PVhw ret;
    
    if(!(ret=(GPS_PVhw)malloc(sizeof(GPS_OVhw))))
	return NULL;

    ret->valid = 0;
    ret->true = ret->mag = ret->wspeed = ret->speed = (double)0.;
    
    return ret;
}



/* @func GPS_Vhw_Del ***********************************************
**
** Water speed destructor
**
** @param [w] thys [GPS_PVhw *] waypoint list to delete
**
** @return [void]
**********************************************************************/

void GPS_Vhw_Del(GPS_PVhw *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Vwr_New ***********************************************
**
** Wind constructor
**
** @return [GPS_PVwr] virgin wind
**********************************************************************/

GPS_PVwr GPS_Vwr_New(void)
{
    GPS_PVwr ret;
    
    if(!(ret=(GPS_PVwr)malloc(sizeof(GPS_OVwr))))
	return NULL;

    ret->wdir = '\0';
    ret->valid = 0;
    ret->wind = ret->knots = ret->ms = ret->khr = (double)0.;
    
    return ret;
}



/* @func GPS_Vwr_Del ***********************************************
**
** Wind destructor
**
** @param [w] thys [GPS_PVwr *] wind to delete
**
** @return [void]
**********************************************************************/

void GPS_Vwr_Del(GPS_PVwr *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Vtg_New ***********************************************
**
** Track made good constructor
**
** @return [GPS_PVtg] virgin tmg
**********************************************************************/

GPS_PVtg GPS_Vtg_New(void)
{
    GPS_PVtg ret;
    
    if(!(ret=(GPS_PVtg)malloc(sizeof(GPS_OVtg))))
	return NULL;

    ret->valid = 0;
    ret->true = ret->mag = ret->knots = ret->khr = (double)0.;
    
    return ret;
}



/* @func GPS_Vtg_Del ***********************************************
**
** Track made good destructor
**
** @param [w] thys [GPS_PVtg *] tmg to delete
**
** @return [void]
**********************************************************************/

void GPS_Vtg_Del(GPS_PVtg *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Xte_New ***********************************************
**
** Cross track error constructor
**
** @return [GPS_Xte] virgin xte
**********************************************************************/

GPS_PXte GPS_Xte_New(void)
{
    GPS_PXte ret;
    
    if(!(ret=(GPS_PXte)malloc(sizeof(GPS_OXte))))
	return NULL;

    ret->valid = 0;
    ret->warn = ret->cycle = ret->steer = ret->unit = '\0';
    ret->dist = (double)0.;
    
    return ret;
}



/* @func GPS_Xte_Del ***********************************************
**
** Cross track error destructor
**
** @param [w] thys [GPS_PXte *] xte to delete
**
** @return [void]
**********************************************************************/

void GPS_Xte_Del(GPS_PXte *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Xtr_New ***********************************************
**
** Cross track error dead constructor
**
** @return [GPS_Xtr] virgin xtr
**********************************************************************/

GPS_PXtr GPS_Xtr_New(void)
{
    GPS_PXtr ret;
    
    if(!(ret=(GPS_PXtr)malloc(sizeof(GPS_OXtr))))
	return NULL;

    ret->valid = 0;
    ret->steer = ret->unit = '\0';
    ret->dist = (double)0.;
    
    return ret;
}



/* @func GPS_Xtr_Del ***********************************************
**
** Cross track error dead destructor
**
** @param [w] thys [GPS_PXtr *] xtr to delete
**
** @return [void]
**********************************************************************/

void GPS_Xtr_Del(GPS_PXtr *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Lib_New ***********************************************
**
** Link constructor
**
** @return [GPS_Lib] virgin link
**********************************************************************/

GPS_PLib GPS_Lib_New(void)
{
    GPS_PLib ret;
    
    if(!(ret=(GPS_PLib)malloc(sizeof(GPS_OLib))))
	return NULL;

    ret->valid = 0;
    ret->rqst = '\0';
    ret->freq = ret->baud = (double)0.;
    
    return ret;
}



/* @func GPS_Lib_Del ***********************************************
**
** Link destructor
**
** @param [w] thys [GPS_PLib *] link to delete
**
** @return [void]
**********************************************************************/

void GPS_Lib_Del(GPS_PLib *thys)
{
    free((void *)*thys);

    return;
}




/* @func GPS_Nmea_New ***********************************************
**
** Nmea data constructor
**
** @return [GPS_PNmea] virgin nmea data
**********************************************************************/

GPS_PNmea GPS_Nmea_New(void)
{
    GPS_PNmea ret;
    
    if(!(ret=(GPS_PNmea)malloc(sizeof(GPS_ONmea))))
	return NULL;

    ret->gsv = GPS_Gsv_New();
    ret->rme = GPS_Rme_New();
    ret->gll = GPS_Gll_New();
    ret->rmz = GPS_Rmz_New();
    ret->rmm = GPS_Rmm_New();
    ret->bod = GPS_Bod_New();
    ret->rte = GPS_Rte_New();
    ret->wpl = GPS_Wpl_New();
    ret->rmc = GPS_Rmc_New();
    ret->rmb = GPS_Rmb_New();
    ret->gga = GPS_Gga_New();
    ret->gsa = GPS_Gsa_New();
    ret->apb = GPS_Apb_New();
    ret->bwc = GPS_Bwc_New();
    ret->bwr = GPS_Bwr_New();
    ret->dbt = GPS_Dbt_New();
    ret->hdm = GPS_Hdm_New();
    ret->hsc = GPS_Hsc_New();
    ret->mtw = GPS_Mtw_New();
    ret->r00 = GPS_R00_New();
    ret->vhw = GPS_Vhw_New();
    ret->vwr = GPS_Vwr_New();
    ret->vtg = GPS_Vtg_New();
    ret->xte = GPS_Xte_New();
    ret->xtr = GPS_Xtr_New();
    ret->lib = GPS_Lib_New();
    
    return ret;
}



/* @func GPS_Nmea_Del ***********************************************
**
** NMEA data destructor
**
** @param [w] thys [GPS_PNmea *] nmea data to delete
**
** @return [void]
**********************************************************************/

void GPS_Nmea_Del(GPS_PNmea *thys)
{

    GPS_Gsv_Del(&(*thys)->gsv);
    GPS_Rme_Del(&(*thys)->rme);
    GPS_Gll_Del(&(*thys)->gll);
    GPS_Rmz_Del(&(*thys)->rmz);
    GPS_Rmm_Del(&(*thys)->rmm);
    GPS_Bod_Del(&(*thys)->bod);
    GPS_Rte_Del(&(*thys)->rte);
    GPS_Wpl_Del(&(*thys)->wpl);
    GPS_Rmc_Del(&(*thys)->rmc);
    GPS_Rmb_Del(&(*thys)->rmb);
    GPS_Gga_Del(&(*thys)->gga);
    GPS_Gsa_Del(&(*thys)->gsa);
    GPS_Apb_Del(&(*thys)->apb);
    GPS_Bwc_Del(&(*thys)->bwc);
    GPS_Bwr_Del(&(*thys)->bwr);
    GPS_Dbt_Del(&(*thys)->dbt);
    GPS_Hdm_Del(&(*thys)->hdm);
    GPS_Hsc_Del(&(*thys)->hsc);
    GPS_Mtw_Del(&(*thys)->mtw);
    GPS_R00_Del(&(*thys)->r00);
    GPS_Vhw_Del(&(*thys)->vhw);
    GPS_Vwr_Del(&(*thys)->vwr);
    GPS_Vtg_Del(&(*thys)->vtg);
    GPS_Xte_Del(&(*thys)->xte);
    GPS_Xtr_Del(&(*thys)->xtr);
    GPS_Lib_Del(&(*thys)->lib);

    free((void *)*thys);

    return;
}
