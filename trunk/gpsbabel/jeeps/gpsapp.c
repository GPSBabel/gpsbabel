/********************************************************************
** @source JEEPS application and data functions
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0
** @modified Dec 28 1999 Alan Bleasby. First version
** @modified Copyright (C) 2004, 2005, 2006 Robert Lipe
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
#include <string.h>
#include <time.h>
#include <stdlib.h>
#include <ctype.h>

/*
 * This violates the layering design, but is needed for device discovery.
 * See the use of gps_is_usb and GPS_Packet_Read_usb below.
 */
#include "garminusb.h"
#include "gpsusbint.h"

#define XMIN(a,b) (a < b? a : b)

static int32    GPS_A000(const char *port);
static void   GPS_A001(GPS_PPacket packet);


static void   GPS_A500_Translate(UC *s, GPS_PAlmanac *alm);
static void   GPS_A500_Encode(UC *s, GPS_PAlmanac alm);
static void   GPS_A300_Translate(UC *s, GPS_PTrack *trk);
static void   GPS_A300_Encode(UC *s, GPS_PTrack trk);


static void   GPS_D100_Get(GPS_PWay *way, UC *s);
static void   GPS_D101_Get(GPS_PWay *way, UC *s);
static void   GPS_D102_Get(GPS_PWay *way, UC *s);
static void   GPS_D103_Get(GPS_PWay *way, UC *s);
static void   GPS_D104_Get(GPS_PWay *way, UC *s);
static void   GPS_D105_Get(GPS_PWay *way, UC *s);
static void   GPS_D106_Get(GPS_PWay *way, UC *s);
static void   GPS_D107_Get(GPS_PWay *way, UC *s);
static void   GPS_D108_Get(GPS_PWay *way, UC *s);
static void   GPS_D109_Get(GPS_PWay *way, UC *s, int proto);
static void   GPS_D150_Get(GPS_PWay *way, UC *s);
static void   GPS_D151_Get(GPS_PWay *way, UC *s);
static void   GPS_D152_Get(GPS_PWay *way, UC *s);
static void   GPS_D154_Get(GPS_PWay *way, UC *s);
static void   GPS_D155_Get(GPS_PWay *way, UC *s);

static void   GPS_D100_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D101_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D102_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D103_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D104_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D105_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D106_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D107_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D108_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D109_Send(UC *data, GPS_PWay way, int32 *len, int proto);
static void   GPS_D150_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D151_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D152_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D154_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D155_Send(UC *data, GPS_PWay way, int32 *len);

static void   GPS_D120_Get(int n, char *data);

static void   GPS_D200_Get(GPS_PWay *way, UC *s);
static void   GPS_D201_Get(GPS_PWay *way, UC *s);
static void   GPS_D202_Get(GPS_PWay *way, UC *s);
static void   GPS_D210_Get(GPS_PWay *way, UC *s);
static void   GPS_D200_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D201_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D202_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D210_Send(UC *data, GPS_PWay way, int32 *len);

static void   GPS_D400_Get(GPS_PWay *way, UC *s);
static void   GPS_D403_Get(GPS_PWay *way, UC *s);
static void   GPS_D450_Get(GPS_PWay *way, UC *s);
static void   GPS_D400_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D403_Send(UC *data, GPS_PWay way, int32 *len);
static void   GPS_D450_Send(UC *data, GPS_PWay way, int32 *len);

static void   GPS_D500_Send(UC *data, GPS_PAlmanac alm);
static void   GPS_D501_Send(UC *data, GPS_PAlmanac alm);
static void   GPS_D550_Send(UC *data, GPS_PAlmanac alm);
static void   GPS_D551_Send(UC *data, GPS_PAlmanac alm);


int32	gps_save_id;
int	gps_is_usb;
double	gps_save_version;
char	gps_save_string[GPS_ARB_LEN];

/*
 * Internal function to copy what Garmin describes as a "Character Array".
 * Dest buffer is padded with spaces and must not contain nulls.  Optionally
 * we uppercase the string because some models (III's and 12's) react
 * violently to lower case data.
 */
typedef enum { UpperNo = 0, UpperYes = 1 } copycase;

static
void copy_char_array(UC **dst, char* src, int count, copycase mustupper)
{
	UC *d = *dst;
	int ocount =  count;
	do {
		UC sc = *src++;
		if (sc == 0) {
			while (count--)
				*d++ = ' ';
			break;
		}
		if (!isalnum(sc)) continue;
		else *d++ = mustupper == UpperYes ? toupper(sc) : sc;
	} while (--count) ;
	*dst += ocount;
}


/* @func GPS_Init ******************************************************
**
** Initialise GPS communication
** Get capabilities and store time lat/lon in case GPS requests
** it later.
** Find endian nature of hardware and store
**
** @param [r] port [const char *] serial port
**
** @return [int32] 1 if success -ve if error
************************************************************************/
int32 GPS_Init(const char *port)
{
    int32 ret;

    (void) GPS_Util_Little();

    ret = GPS_A000(port);
    if(ret<0) return ret;
    gps_save_time = GPS_Command_Get_Time(port);

    /*
     * Some units may be unable to return time, such as a C320 when in
     * charging mode.  Only consider it fatal if the unit returns an error,
     * not just absence of returning a time.
     */
    if(gps_save_time < 0) {
	return FRAMING_ERROR;
    }

    if (0 == strncmp(gps_save_string, "GPilotS", 7)) {
	    return 1;
    }

    return GPS_Command_Get_Position(port,&gps_save_lat,&gps_save_lon);
}


/* @funcstatic GPS_A000 ************************************************
**
** Return product ID, version and description. Turn off PVT transfer
**
** @param [r] port [const char *] serial port
**
** @return [int32] 1 if success -ve if error
************************************************************************/
static int32 GPS_A000(const char *port)
{
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int16 version;
    int16 id;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!GPS_Device_Flush(fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Make_Packet(&tra, LINK_ID[0].Pid_Product_Rqst,NULL,0);
    if(!GPS_Write_Packet(fd,tra))
	return SERIAL_ERROR;

    if(!GPS_Get_Ack(fd, &tra, &rec))
	return SERIAL_ERROR;

    GPS_Packet_Read(fd, &rec);
    GPS_Send_Ack(fd, &tra, &rec);

    id = GPS_Util_Get_Short(rec->data);
    version = GPS_Util_Get_Short((rec->data)+2);

    (void) strcpy(gps_save_string,(char *)rec->data+4);
    gps_save_id = id;
    gps_save_version = (double)((double)version/(double)100.);

    GPS_User("Unit:\t%s\nID:\t%d\nVersion:\t%.2f",
	gps_save_string, gps_save_id, gps_save_version);

#if 0
    gps_date_time_transfer = pA600;
    gps_date_time_type     = pD600;  /* All models so far */
    gps_position_transfer  = pA700;
    gps_position_type      = pD700;  /* All models so far */
#else
    gps_date_time_transfer = -1;
    gps_date_time_type     = -1;
    gps_position_transfer  = -1;
    gps_position_type      = -1;
#endif
    gps_pvt_transfer       = -1;
    gps_pvt_type           = -1;
    gps_trk_transfer       = -1;
    gps_trk_type           = -1;
    gps_trk_hdr_type       = -1;
    gps_rte_link_type      = -1;

    gps_prx_waypt_transfer = -1;
    gps_prx_waypt_type     = -1;
    gps_almanac_transfer   = -1;
    gps_almanac_type       = -1;
    gps_lap_transfer       = -1;
    gps_lap_type           = -1;

    if(!GPS_Device_Wait(fd))
    {
	GPS_Warning("A001 protocol not supported");
	id = GPS_Protocol_Version_Change(id,version);
	if(GPS_Protocol_Table_Set(id)<0)
	    return GPS_UNSUPPORTED;
    }
    else
    {
        int i;
	/*
	 * The unit may return more than one packet, so read and
	 * discard all but the product inquiry response.  We have
	 * no way of knowing how many we'll get, so we have to keep
	 * reading until we incur a timeout.
	 * Worse still, the serial layer assumes a read timeout is a
	 * fatal error, while the USB layer (correctly) returns that error
	 * to the caller.  So we call GPS_Device_Wait which spins into
	 * a delay/select for the serial system and a NOP for USB.
	 *
	 * Worse _yet_, this is the one place in all of Garmin Protocolsville
	 * where we don't know a priori how many packets will be sent in
	 * response.   Since we want the lower levels of the USB handler
	 * to handle the ugliness of the "return to interrupt" packets, we
	 * reach behind that automation here and hand that ourselves.
	 */
	for (i = 0; i < 25; i++) {
	    rec->type = 0;

	    if (gps_is_usb) {
		GPS_Packet_Read_usb(fd, &rec, 0);
	    } else {
		if(!GPS_Device_Wait(fd))
			goto carry_on;

		if (GPS_Packet_Read(fd, &rec) <= 0) {
			goto carry_on;
		}

		GPS_Send_Ack(fd, &tra, &rec);
	    }

	    if (rec->type == 0xfd) {
		GPS_A001(rec);
		goto carry_on;
	    }

	   /*
 	    * If a 296 has previously been interrupted, it's going to
	    * ignore the session request (grrrr) and continue to send
	    * us left over packets.   So if we see anything that isn't
 	    * part of our A000 discovery  cycle, reset the counter and
	    * continue to loop.
	    *
	    * Garmin acknowledges this is a firmware defect.
	    */
	    if (rec->type < 0xf8) {
		i = 0;
	    }
	}
	fatal("Failed to find a product inquiry response.\n");
    }

carry_on:
    /* Make sure PVT is off as some GPS' have it on by default */
    if(gps_pvt_transfer != -1)
	GPS_A800_Off(port,&fd);

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}




/* @funcstatic  GPS_A001 ************************************************
**
** Extract protocol capabilities
** This routine could do with re-writing. It's too long and obtuse.
**
** @param [r] packet [GPS_PPacket] A001 protocol packet
**
** @return [void]
************************************************************************/
static void GPS_A001(GPS_PPacket packet)
{
    int32 entries;
    int32 i;
    UC *p;
    US tag;
    US data;
    US lasta=0;

    gps_link_type          = -1;
    gps_device_command     = -1;
    gps_waypt_transfer     = -1;
    gps_waypt_type         = -1;
    gps_route_transfer     = -1;
    gps_rte_hdr_type       = -1;
    gps_rte_type           = -1;
    gps_trk_transfer       = -1;
    gps_trk_type           = -1;
    gps_prx_waypt_transfer = -1;
    gps_prx_waypt_type     = -1;
    gps_almanac_transfer   = -1;
    gps_almanac_type       = -1;
    gps_lap_transfer       = -1;
    gps_lap_type           = -1;

    entries = packet->n / 3;
    p = packet->data;

    for(i=0;i<entries;++i,p+=3)
    {
	tag = *p;
	data = GPS_Util_Get_Short(p+1);

	switch (tag) {
	/* Only one type of P[hysical] so far */
	case 'P':
	    if(data!=0)
		GPS_Protocol_Error(tag,data);
	    break;
	case 'L':
	    gps_link_type = data;
	    break;
	case 'A':
	    GPS_User("\nCapability %c%d:", tag, data);
	    lasta = data;
	    switch (data) {
		case 10:
		    gps_device_command = pA010-10;
		    break;
		case 11:
		    gps_device_command = pA010-10;
		    break;
		case 100:
		    gps_waypt_transfer = pA100;
		    break;
		case 101:
		    gps_category_transfer = pA101;
		    break;
		case 200:
		    gps_route_transfer = pA200;
		    break;
		case 201:
		    gps_route_transfer = pA201;
		    break;
		case 300:
		    gps_trk_transfer = pA300;
		    break;
		case 301:
		    gps_trk_transfer = pA301;
		    break;
		case 302:
		    gps_trk_transfer = pA302;
		    break;
		case 400:
		    gps_prx_waypt_transfer = pA400;
		    break;
		case 500:
		    gps_almanac_transfer = pA500;
		    break;
		case 600:
		    gps_date_time_transfer = pA600;
		    break;
		case 650:
		    /*  FlightBook Transfer Protocol */
		    break;
		case 700:
		    gps_position_transfer = pA700;
		    break;
		case 800:
		    gps_pvt_transfer = pA800;
		    break;
		case 906:
		    gps_lap_transfer = pA906;
		    break;
		case 1000:
		    gps_run_transfer = pA1000;
		    break;
		case 1002:
		    gps_workout_transfer = pA1002;
		    break;
		case 1004:
		    gps_user_profile_transfer = pA1004;
		    break;
		case 1005:
		    gps_workout_limits_transfer = pA1005;
		    break;
		case 1006:
		    gps_course_transfer = pA1006;
		    break;
		case 1009:
		    gps_course_limits_transfer = pA1009;
		    break;
	    }
	    break;

	case 'D':
	    GPS_User(" %c%d", tag, data);
	    if(lasta<200)
	    {
		switch (data) {
			case 100:
			case 101:
			case 102:
			case 103:
			case 104:
			case 105:
			case 106:
			case 107:
			case 108:
			case 109:
			case 110:
			/* 15x is panel-mount aviation */
			case 150:
			case 151:
			case 152:
			/* 153 not documented */
			case 154:
			case 155:
				gps_waypt_type = data;
				break;

			/*
			 * Observered on Quest 3.0, 27xx, 27x, 29x.
			 */
			case 120:
				gps_category_type = data;
				break;

			case 200:
			case 201:
			case 202:
				gps_rte_hdr_type = data;
				break;
			
			/* 210 Link packets appear in newer models, but the
			 * doc isn't sufficiently clear on what they really
			 * mean.
			 */
			case 210:
				gps_rte_link_type = data;
				break;
			
		}
	    }


	    else if(lasta<300)
	    {
		if(data>=200 && data <=202)
		{
		    gps_rte_hdr_type = data;
		    continue;
		}
		if(data==210)
		{
		    gps_rte_link_type = data;
		    continue;
		}

		if(data<=110 && data>=100)
		{
		    gps_rte_type = data;
		    continue;
		}
		if(data<153 && data>=150)
		{
		    gps_rte_type = data;
		    continue;
		}
		if(data<156 && data>=154)
		{
		    gps_rte_type = data;
		    continue;
		}
		if(data<451)
		{
		    if(data==400)
			gps_rte_type = pD400;
		    else if(data==403)
			gps_rte_type = pD403;
		    else if(data==450)
			gps_rte_type = pD450;
		    else
			GPS_Protocol_Error(tag,data);
		    continue;
		}
	    }
		
	    else if(lasta<400)
	    {
		    switch (data) {
			    case 300: gps_trk_type = pD300; break;
			    case 301: gps_trk_type = pD301; break;
			    case 302: gps_trk_type = pD302; break;
			    case 303: gps_trk_type = pD303; break;
			    case 304: gps_trk_type = pD304; break;
			    case 310: gps_trk_hdr_type = pD310; break;
			    case 311: gps_trk_hdr_type = pD311; break;
			    case 312: gps_trk_hdr_type = pD312; break;
			    default:  GPS_Protocol_Error(tag,data); break;
		    }
		    continue;
	    }

	    else if(lasta<500)
	    {
		    if((data<=110 && data>=100) ||
				(data<153 && data>=150) ||
				(data<156 && data>=154)) {
		    	gps_prx_waypt_type = data;
		    }
			else if(data==400)
			gps_prx_waypt_type = pD400;
		    else if(data==403)
			gps_prx_waypt_type = pD403;
		    else if(data==450)
			gps_prx_waypt_type = pD450;
		    else
			GPS_Protocol_Error(tag,data);
		    continue;
		}

	    else if(lasta<600)
	    {
		if(data==500)
		    gps_almanac_type = pD500;
		else if(data==501)
		    gps_almanac_type = pD501;
		else if(data==550)
		    gps_almanac_type = pD550;
		else if(data==551)
		    gps_almanac_type = pD551;
		else
		    GPS_Protocol_Error(tag,data);
		continue;
	    }

	    else if(lasta<650)
	    {
		if (data == 600) {
		    gps_date_time_type = pD600;
		} else {
		    /* Stupid undocumented 60 D601 packets */
		    /* GPS_Protocol_Error(tag,data); */
			continue;
		}
		continue;
	    }
		
	    else if(lasta<651)
	    {
			/*  FlightBook Transfer Protocol, not handled */
			continue;
	    }
		
	    else if(lasta<800)
	    {
		if(data!=700)
		    GPS_Protocol_Error(tag,data);
		else
		    gps_position_type = pD700;
		continue;
	    }
		
	    else if(lasta<900)
	    {
		if (data == 800)
		    gps_pvt_type = pD800;
		/*
		 *  Stupid, undocumented Vista 3.60 D802 packets
		 else
		    GPS_Protocol_Error(tag,data);
	         */
		continue;
	    }
		
	    else if (lasta < 1000)
	    {
		if (data == 906)
		    gps_lap_type = pD906;
			else if (data == 1001)
				gps_lap_type = pD1001;
			else if (data == 1011)
				gps_lap_type = pD1011;
			else if (data == 1015)
				gps_lap_type = pD1015;
			continue;
	    }
		
	    else if (lasta < 1002)
	    {
		if (data == 1000)
			gps_run_type = pD1000;
		else if (data == 1009)
			gps_run_type = pD1009;
		else if (data == 1010)
			gps_run_type = pD1010;
		continue;
	    }
		
	    else if (lasta < 1003)
	    {
		if (data == 1002)
			gps_workout_type = pD1002;
		else if (data == 1008)
			gps_workout_type = pD1008;
		continue;
	    }
		
	    else if (lasta < 1004)
	    {
		if (data == 1003)
		    gps_workout_occurrence_type = pD1003;
		continue;
	    }
		
	    else if (lasta < 1005)
	    {
		if (data == 1004)
		    gps_user_profile_type = pD1004;
		continue;
	    }
		
	    else if (lasta < 1006)
	    {
		if (data == 1005)
		    gps_workout_limits_type = pD1005;
		continue;
	    }
		
	    else if (lasta < 1007)
	    {
		if (data == 1006)
		    gps_course_type = pD1006;
		continue;
	    }
		
	    else if (lasta < 1008)
	    {
		if (data == 1007)
		    gps_course_lap_type = pD1007;
		continue;
	    }
		
	    else if (lasta < 1009)
	    {
		if (data == 1012)
		    gps_course_point_type = pD1012;
		continue;
	    }
		
	    else if (lasta < 1010)
	    {
		if (data == 1013)
		    gps_course_limits_type = pD1013;
		continue;
	    }
	}
    }

    GPS_User("\nLink_type %d  Device_command %d\n",
	gps_link_type, gps_device_command);
    GPS_User("Waypoint: Transfer %d Type %d\n",
	gps_waypt_transfer, gps_waypt_type);
    GPS_User("Route:    Transfer %d Header %d Type %d\n",
	gps_route_transfer, gps_rte_hdr_type, gps_rte_type);
    GPS_User("Track:    Transfer %d Type %d\n",
	gps_trk_transfer, gps_trk_type);

    return;
}




/* @func GPS_A100_Get ******************************************************
**
** Get waypoint data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/
int32 GPS_A100_Get(const char *port, GPS_PWay **way, int (*cb)(int, GPS_PWay *))
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;


    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Wpt);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);

    if(!GPS_Write_Packet(fd,tra))
    {
	GPS_Error("A100_Get: Cannot write packet");
	return FRAMING_ERROR;
    }

    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A100_Get: No acknowledge");	
	return FRAMING_ERROR;
    }

    GPS_Packet_Read(fd, &rec);
    GPS_Send_Ack(fd, &tra, &rec);

    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*way)=(GPS_PWay *)malloc(n*sizeof(GPS_PWay))))
	{
	    GPS_Error("A100_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}

    for(i=0;i<n;++i)
    {
	if(!((*way)[i]=GPS_Way_New()))
	    return MEMORY_ERROR;

	if(!GPS_Packet_Read(fd, &rec)) {
	    return gps_errno;
	}

	if(!GPS_Send_Ack(fd, &tra, &rec)) {
	    return gps_errno;
	}

	switch(gps_waypt_type)
	{
	case pD100:
	    GPS_D100_Get(&((*way)[i]),rec->data);
	    break;
	case pD101:
	    GPS_D101_Get(&((*way)[i]),rec->data);
	    break;
	case pD102:
	    GPS_D102_Get(&((*way)[i]),rec->data);
	    break;
	case pD103:
	    GPS_D103_Get(&((*way)[i]),rec->data);
	    break;
	case pD104:
	    GPS_D104_Get(&((*way)[i]),rec->data);
	    break;
	case pD105:
	    GPS_D105_Get(&((*way)[i]),rec->data);
	    break;
	case pD106:
	    GPS_D106_Get(&((*way)[i]),rec->data);
	    break;
	case pD107:
	    GPS_D107_Get(&((*way)[i]),rec->data);
	    break;
	case pD108:
	    GPS_D108_Get(&((*way)[i]),rec->data);
	    break;
	case pD109:
	    GPS_D109_Get(&((*way)[i]),rec->data, 109);
	    break;
	case pD110:
	    GPS_D109_Get(&((*way)[i]),rec->data, 110);
	    break;
	case pD150:
	    GPS_D150_Get(&((*way)[i]),rec->data);
	    break;
	case pD151:
	    GPS_D151_Get(&((*way)[i]),rec->data);
	    break;
	case pD152:
	    GPS_D152_Get(&((*way)[i]),rec->data);
	    break;
	case pD154:
	    GPS_D154_Get(&((*way)[i]),rec->data);
	    break;
	case pD155:
	    GPS_D155_Get(&((*way)[i]),rec->data);
	    break;
	default:
	    GPS_Error("A100_GET: Unknown waypoint protocol: %d", gps_waypt_type);
	    return PROTOCOL_ERROR;
	}
	/* Issue callback for status updates. */
	if (cb) {
		cb(n, &((*way)[i]));
	}
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("A100_GET: Error transferring waypoints.  Expected %d completion code.  Got %d.  %d of %d received", LINK_ID[gps_link_type].Pid_Xfer_Cmplt, rec->type, i, n);
	return FRAMING_ERROR;
    }

    if(i != n)
    {
	GPS_Error("A100_GET: Waypoint entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}





/* @func GPS_A100_Send **************************************************
**
** Send waypoints to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A100_Send(const char *port, GPS_PWay *way, int32 n, int (*cb)(GPS_PWay *))
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;

    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data, (short) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("Waypoint start data not acknowledged");
	return gps_errno;
    }


    for(i=0;i<n;++i)
    {
        if (cb) {
		if (cb((GPS_PWay *) way[i]))  /* BUGBUG Wrong level of indirection */
			break;
	}

	switch(gps_waypt_type)
	{
	case pD100:
	    GPS_D100_Send(data,way[i],&len);
	    break;
	case pD101:
	    GPS_D101_Send(data,way[i],&len);
	    break;
	case pD102:
	    GPS_D102_Send(data,way[i],&len);
	    break;
	case pD103:
	    GPS_D103_Send(data,way[i],&len);
	    break;
	case pD104:
	    GPS_D104_Send(data,way[i],&len);
	    break;
	case pD105:
	    GPS_D105_Send(data,way[i],&len);
	    break;
	case pD106:
	    GPS_D106_Send(data,way[i],&len);
	    break;
	case pD107:
	    GPS_D107_Send(data,way[i],&len);
	    break;
	case pD108:
	    GPS_D108_Send(data,way[i],&len);
	    break;
	case pD109:
	    GPS_D109_Send(data,way[i],&len, 109);
	    break;
	case pD110:
	    GPS_D109_Send(data,way[i],&len, 110);
	    break;
	case pD150:
	    GPS_D150_Send(data,way[i],&len);
	    break;
	case pD151:
	    GPS_D151_Send(data,way[i],&len);
	    break;
	case pD152:
	    GPS_D152_Send(data,way[i],&len);
	    break;
	case pD154:
	    GPS_D154_Send(data,way[i],&len);
	    break;
	case pD155:
	    GPS_D155_Send(data,way[i],&len);
	    break;
	default:
	    GPS_Error("Unknown waypoint protocol");
	    return PROTOCOL_ERROR;
	}

	GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Wpt_Data,
                  data, (US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A100_Send: Pid_Wpt_Data not acknowledged");
	    return gps_errno;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Wpt);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("Waypoint complete data not acknowledged");
	return gps_errno;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}


/*
 * Get the list of waypoint categories from the receiver.
 */
int32 GPS_A101_Get(const char *port)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;


    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Wpt_Cats);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);

    if(!GPS_Write_Packet(fd,tra))
    {
	GPS_Error("A101_Get: Cannot write packet");
	return FRAMING_ERROR;
    }

    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A101_Get: No acknowledge");	
	return FRAMING_ERROR;
    }

    GPS_Packet_Read(fd, &rec);
    GPS_Send_Ack(fd, &tra, &rec);

    n = GPS_Util_Get_Short(rec->data);
    for (i = 0; i < n; ++i) {
        if(!GPS_Packet_Read(fd, &rec)) {
            return gps_errno;
        }
        if(!GPS_Send_Ack(fd, &tra, &rec)) {
            return gps_errno;
        }
	switch(gps_category_type) {
	case pD120:
	    GPS_D120_Get(i,(char *) rec->data);
	    break;
	}
    }
    if(!GPS_Packet_Read(fd, &rec))
        return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
        return gps_errno;

    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
        GPS_Error("A101_Get: Error transferring waypoints.  Expected %d completion code.  Got %d.  %d of %d received", LINK_ID[gps_link_type].Pid_Xfer_Cmplt, rec->type, i, n);
        return FRAMING_ERROR;
    }


    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;

}

/* @funcstatic GPS_D100_Get *********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D100_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 100;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    return;
}



/* @funcstatic GPS_D101_Get *********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D101_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 101;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*way)->smbl = *p;

    return;
}



/* @funcstatic GPS_D102_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D102_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 102;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*way)->smbl = GPS_Util_Get_Short(p);


    return;
}



/* @funcstatic GPS_D103_Get *********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D103_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 103;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->smbl = *p++;
    (*way)->dspl = *p;


    return;
}



/* @funcstatic GPS_D104_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D104_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 104;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    (*way)->dspl = *p;

    return;
}



/* @funcstatic GPS_D105_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D105_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    UC *q;

    p=s;

    (*way)->prot = 105;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    q = (UC *) (*way)->wpt_ident;
    while((*q++ = *p++));

    return;
}



/* @funcstatic GPS_D106_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D106_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    UC *q;
    int32 i;

    p=s;

    (*way)->prot = 106;

    (*way)->wpt_class = *p++;

    for(i=0;i<13;++i) (*way)->subclass[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    q = (UC *) (*way)->wpt_ident;
    while((*q++ = *p++));
    q = (UC *) (*way)->lnk_ident;
    while((*q++ = *p++));

    return;
}



/* @funcstatic GPS_D107_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D107_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 107;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->smbl = *p++;
    (*way)->dspl = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*way)->colour = *p++;

    return;
}



/* @funcstatic GPS_D108_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D108_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    UC *q;

    int32 i;

    p=s;

    (*way)->prot = 108;

    (*way)->wpt_class = *p++;
    (*way)->colour    = *p++;
    (*way)->dspl      = *p++;
    (*way)->attr      = *p++;
    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);
    for(i=0;i<18;++i) (*way)->subclass[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->alt = GPS_Util_Get_Float(p);
    p+=sizeof(float);
    (*way)->dpth = GPS_Util_Get_Float(p);
    p+=sizeof(float);
    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<2;++i) (*way)->state[i] = *p++;
    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    q = (UC *) (*way)->ident;
    while((*q++ = *p++));

    q = (UC *) (*way)->cmnt;
    while((*q++ = *p++));

    q = (UC *) (*way)->facility;
    while((*q++ = *p++));

    q = (UC *) (*way)->city;
    while((*q++ = *p++));

    q = (UC *) (*way)->addr;
    while((*q++ = *p++));

    q = (UC *) (*way)->cross_road;
    while((*q++ = *p++));

    return;
}

/* @funcstatic GPS_D109_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
** Quest uses D110's which are just like D109's but with the addition
** of temp, time, and wpt_cat stuck between ete and ident.   Rather than
** duplicating the function, we just handle this at runtime.
************************************************************************/
static void GPS_D109_Get(GPS_PWay *way, UC *s, int protoid)
{
    UC *p;
    UC *q;

    int32 i;

    p=s;

    (*way)->prot = protoid;

    p++;				/* data packet type */
    (*way)->wpt_class = *p++;
    (*way)->colour    = *p & 0x1f;
    (*way)->dspl      = (*p++ >> 5) & 3;
    (*way)->attr      = *p++;
    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);
    for(i=0;i<18;++i) (*way)->subclass[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->alt = GPS_Util_Get_Float(p);
    p+=sizeof(float);
    (*way)->dpth = GPS_Util_Get_Float(p);
    p+=sizeof(float);
    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<2;++i) (*way)->state[i] = *p++;
    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    p += 4; /* Skip over "outbound link ete in seconds */
    if (protoid == 110) {
	float gps_temp;
	int gps_time;
	gps_temp = GPS_Util_Get_Float(p);
	p+=4;
	if (gps_temp <= 1.0e24) {
		(*way)->temperature_populated = 1;
		(*way)->temperature = gps_temp;
	}

	gps_time = GPS_Util_Get_Uint(p);
	p+=4;
	/* The spec says that 0xffffffff is unknown, but the 60CSX with
	 * firmware 2.5.0 writes zero.
	 */
	if (gps_time != 0xffffffff && gps_time != 0) {
		(*way)->time_populated = 1;
		(*way)->time = GPS_Math_Gtime_To_Utime(gps_time);
	}
	(*way)->category = GPS_Util_Get_Short(p);
	p += 2;
    }

    q = (UC *) (*way)->ident;
    while((*q++ = *p++));

    q = (UC *) (*way)->cmnt;
    while((*q++ = *p++));

    q = (UC *) (*way)->facility;
    while((*q++ = *p++));

    q = (UC *) (*way)->city;
    while((*q++ = *p++));

    q = (UC *) (*way)->addr;
    while((*q++ = *p++));

    q = (UC *) (*way)->cross_road;
    while((*q++ = *p++));

    return;
}


/* @funcstatic GPS_D150_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D150_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 150;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;
    for(i=0;i<2;++i) (*way)->cc[i] = *p++;
    (*way)->wpt_class = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;
    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    return;
}



/* @funcstatic GPS_D151_Get *********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D151_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 151;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    ++p;

    (*way)->wpt_class = *p;

    return;
}



/* @funcstatic GPS_D152_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D152_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 152;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    ++p;

    (*way)->wpt_class = *p;

    return;
}


/* @funcstatic GPS_D154_Get ********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D154_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 154;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    ++p;

    (*way)->wpt_class = *p++;

    (*way)->smbl = GPS_Util_Get_Short(p);

    return;
}


/* @funcstatic GPS_D155_Get *********************************************
**
** Get waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D155_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 155;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<2;++i) (*way)->cc[i] = *p++;

    ++p;

    (*way)->wpt_class = *p++;

    (*way)->smbl = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    (*way)->dspl = *p;

    return;
}

/*
 * We'll cheat for now.  We know there are no more than 16 categories
 * as of this writing for no data type exposes more than 16 bits in the
 * bitmask of categories.
 */
char gps_categories[16][17];
/*
 * Read descriptor s into category number N;
 */
static
void GPS_D120_Get(int cat_num, char *s)
{
	/* we're guaranteed to have no more than 16 chars plus a
	 * null terminator.
	 *
	 * If the unit returned no string, the user has not configured one,
	 * so mimic the behaviour of the 276/296.
	 */

	if (*s) {
		strncpy(gps_categories[cat_num], s, sizeof (gps_categories[0]));
	} else {
		snprintf(gps_categories[cat_num], sizeof (gps_categories[0]),
			"Category %d", cat_num+1);
	}
}


/* @funcstatic GPS_D100_Send *******************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D100_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    *len = 58;

    return;
}


/* @funcstatic GPS_D101_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D101_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);


    GPS_Util_Put_Float(p,way->dst);
    p+= sizeof(float);

    *p = way->smbl;

    *len = 63;

    return;
}


/* @funcstatic GPS_D102_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D102_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    GPS_Util_Put_Float(p,way->dst);
    p+= sizeof(float);

    GPS_Util_Put_Short(p,(US) way->smbl);

    *len = 64;

    return;
}


/* @funcstatic GPS_D103_Send *******************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D103_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    *p++ = (UC) way->smbl;
    *p   = (UC) way->dspl;

    *len = 60;

    return;
}


/* @funcstatic GPS_D104_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D104_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    /* byonke confirms that sending lower case comment data to a III+
     * results in the comment being truncated there.   So we uppercase
     * the entire comment.
     */
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    GPS_Util_Put_Float(p,way->dst);
    p+= sizeof(float);

    GPS_Util_Put_Short(p, (int16) way->smbl);
    p+=sizeof(int16);

    *p = 3; /* display symbol with waypoint name */

    *len = 65;

    return;
}


/* @funcstatic GPS_D105_Send *******************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D105_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    UC *q;

    p = data;

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);

    GPS_Util_Put_Short(p, (int16) way->smbl);
    p+=sizeof(int16);

    q = (UC *) way->wpt_ident;
    while((*p++ = *q++));


    *len = p-data;

    return;
}


/* @funcstatic GPS_D106_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D106_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    UC *q;
    int32 i;

    p = data;

    *p++ = way->wpt_class;
    for(i=0;i<13;++i) *p++ = way->subclass[i];
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);

    GPS_Util_Put_Short(p, (int16) way->smbl);
    p+=sizeof(int16);

    q = (UC *) way->wpt_ident;
    while((*p++ = *q++));
    q = (UC *) way->lnk_ident;
    while((*p++ = *q++));

    *len = p-data;

    return;
}


/* @funcstatic GPS_D107_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D107_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    *p++ = way->smbl;
    *p++ = way->dspl;

    GPS_Util_Put_Float(p,way->dst);
    p+= sizeof(float);

    *p = way->colour;

    *len = 65;

    return;
}



/* @funcstatic GPS_D108_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D108_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    UC *q;

    int32 i;

    p = data;

    *p++ = way->wpt_class;
    *p++ = way->colour;
    *p++ = way->dspl;
    *p++ = 0x60;
    GPS_Util_Put_Short(p,(US) way->smbl);
    p+=sizeof(int16);
    for(i=0;i<18;++i) *p++ = way->subclass[i];
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);

    if (way->alt_is_unknown) {
	GPS_Util_Put_Float(p,(const float) 1.0e25);
    } else {
	GPS_Util_Put_Float(p,way->alt);
    }
    p+=sizeof(float);
    GPS_Util_Put_Float(p,way->dpth);
    p+=sizeof(float);
    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    for(i=0;i<2;++i) *p++ = way->state[i];
    for(i=0;i<2;++i) *p++ = way->cc[i];


    q = (UC *) way->ident;
    i = XMIN(51, sizeof(way->ident));
    while((*p++ = *q++) && i--);
    q = (UC *) way->cmnt;
    i = XMIN(51, sizeof(way->cmnt));
    while((*p++ = *q++) && i--);
    q = (UC *) way->facility;
    i = XMIN(31, sizeof(way->facility));
    while((*p++ = *q++) && i--);
    q = (UC *) way->city;
    i = XMIN(25, sizeof(way->city));
    while((*p++ = *q++) && i--);
    q = (UC *) way->addr;
    i = XMIN(51, sizeof(way->addr));
    while((*p++ = *q++) && i--);
    q = (UC *) way->cross_road;
    i = XMIN(51, sizeof(way->cross_road));
    while((*p++ = *q++) && i--);

    *len = p-data;

    return;
}


/* @funcstatic GPS_D109_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
** D109's and D110's are so simlar, we handle themw with the same code.
************************************************************************/
static void GPS_D109_Send(UC *data, GPS_PWay way, int32 *len, int protoid)
{
    UC *p;
    UC *q;

    int32 i;

    p = data;

    *p++ = 1; /* data packet type; must be 1 for D109 and D110 */
    *p++ = 0; // way->wpt_class;

    *p++ = ((way->dspl & 3) << 5) | 0x1f;	/* colour & display */

    if (protoid == 109) {	/* attr */
	*p++ = 0x70;
    } else if (protoid == 110) {
	*p++  = 0x80;
    } else {
	GPS_Warning("Unknown protoid in GPS_D109_Send.");
    }
    GPS_Util_Put_Short(p,(US) way->smbl);
    p+=sizeof(int16);
    for(i=0;i<18;++i) *p++ = way->subclass[i];
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    if (way->alt_is_unknown) {
	GPS_Util_Put_Float(p,(const float) 1.0e25);
    } else {
	GPS_Util_Put_Float(p,way->alt);
    }
    p+=sizeof(float);
    GPS_Util_Put_Float(p,way->dpth);
    p+=sizeof(float);
    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    for(i=0;i<2;++i) *p++ = way->state[i];
    for(i=0;i<2;++i) *p++ = way->cc[i];
    for(i=0;i<4;++i) *p++ = 0xff; /* D109 silliness for ETE */
    if (protoid == 110) {
	float temp = 1.0e25f;

	GPS_Util_Put_Float(p, temp);
	p += 4;

	if (way->time_populated) {
		GPS_Util_Put_Uint(p,GPS_Math_Utime_To_Gtime(way->time));
		p+=sizeof(uint32);
	} else {
		for(i=0;i<4;++i) *p++ = 0xff; /* unknown time*/
	}

	GPS_Util_Put_Short(p, (US) way->category);; /* D110 category */
	p += 2;
    }

    q = (UC *) way->ident;
    i = XMIN(51, sizeof(way->ident));
    while((*p++ = *q++) && i--);
    q = (UC *) way->cmnt;
    i = XMIN(51, sizeof(way->cmnt));
    while((*p++ = *q++) && i--);
    q = (UC *) way->facility;
    i = XMIN(31, sizeof(way->facility));
    while((*p++ = *q++) && i--);
    q = (UC *) way->city;
    i = XMIN(25, sizeof(way->city));
    while((*p++ = *q++) && i--);
    q = (UC *) way->addr;
    i = XMIN(51, sizeof(way->addr));
    while((*p++ = *q++) && i--);
    q = (UC *) way->cross_road;
    i = XMIN(51, sizeof(way->cross_road));
    while((*p++ = *q++) && i--);
    *len = p-data;
    return;
}


/* @funcstatic GPS_D150_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D150_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);
    for(i=0;i<2;++i) *p++ = way->cc[i];

    if(way->wpt_class == 7) way->wpt_class = 0;
    *p++ = way->wpt_class;

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    copy_char_array(&p, way->city, 24, UpperYes);
    copy_char_array(&p, way->state, 2, UpperYes);
    copy_char_array(&p, way->name, 30, UpperYes);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    *len = 115;

    return;
}


/* @funcstatic GPS_D151_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D151_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);
    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    copy_char_array(&p, way->name, 30, UpperYes);
    copy_char_array(&p, way->city, 24, UpperYes);
    copy_char_array(&p, way->state, 2, UpperYes);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    for(i=0;i<2;++i) *p++ = way->cc[i];
    *p++ = 0;

    if(way->wpt_class == 3) way->wpt_class = 0;
    *p   = way->wpt_class;

    *len = 124;

    return;
}



/* @funcstatic GPS_D152_Send ********************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D152_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);
    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    copy_char_array(&p, way->name, 30, UpperYes);
    copy_char_array(&p, way->city, 24, UpperYes);
    copy_char_array(&p, way->state, 2, UpperYes);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    for(i=0;i<2;++i) *p++ = way->cc[i];
    *p++ = 0;

    if(way->wpt_class == 5) way->wpt_class = 0;
    *p   = way->wpt_class;

    *len = 124;

    return;
}


/* @funcstatic GPS_D154_Send *******************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D154_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);

    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    copy_char_array(&p, way->name, 30, UpperYes);
    copy_char_array(&p, way->city, 24, UpperYes);
    copy_char_array(&p, way->state, 2, UpperYes);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    for(i=0;i<2;++i) *p++ = way->cc[i];
    *p++ = 0;

    if(way->wpt_class == 9) way->wpt_class = 0;
    *p++   = way->wpt_class;

    GPS_Util_Put_Short(p,(int16)way->smbl);

    *len = 126;

    return;
}



/* @funcstatic GPS_D155_Send *******************************************
**
** Form waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D155_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    copy_char_array(&p, way->ident, 6, UpperYes);

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    copy_char_array(&p, way->cmnt, 40, UpperYes);
    GPS_Util_Put_Float(p,way->dst);
    p+=sizeof(float);

    copy_char_array(&p, way->name, 30, UpperYes);
    copy_char_array(&p, way->city, 24, UpperYes);
    copy_char_array(&p, way->state, 2, UpperYes);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    copy_char_array(&p, way->cc, 2, UpperYes);
    *p++ = 0;

    /* Ignore wpt_class; our D155 points are always user type which is "4". */
    *p++ = 4;

    GPS_Util_Put_Short(p,(int16)way->smbl);
    p+=sizeof(int16);

    *p = way->dspl;

    *len = 127;

    return;
}



/* @func GPS_A200_Get ******************************************************
**
** Get route data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/
int32 GPS_A200_Get(const char *port, GPS_PWay **way)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;


    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Rte);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*way)=(GPS_PWay *)malloc(n*sizeof(GPS_PWay))))
	{
	    GPS_Error("A200_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}


    for(i=0;i<n;++i)
    {
	if(!((*way)[i]=GPS_Way_New()))
	    return MEMORY_ERROR;
	
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;

	if(rec->type == LINK_ID[gps_link_type].Pid_Rte_Hdr)
	{
	    switch(gps_rte_hdr_type)
	    {
	    case pD200:
		GPS_D200_Get(&((*way)[i]),rec->data);
		break;
	    case pD201:
		GPS_D201_Get(&((*way)[i]),rec->data);
		break;
	    case pD202:
		GPS_D202_Get(&((*way)[i]),rec->data);
		break;
	    default:
		GPS_Error("A200_GET: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	    continue;
	}

	if(rec->type != LINK_ID[gps_link_type].Pid_Rte_Wpt_Data)
	{
	    GPS_Error("A200_GET: Non Pid_rte_Wpt_Data");
	    return FRAMING_ERROR;
	}
	
	(*way)[i]->isrte  = 0;
	(*way)[i]->islink = 0;

	switch(gps_rte_type)
	{
	case pD100:
	    GPS_D100_Get(&((*way)[i]),rec->data);
	    break;
	case pD101:
	    GPS_D101_Get(&((*way)[i]),rec->data);
	    break;
	case pD102:
	    GPS_D102_Get(&((*way)[i]),rec->data);
	    break;
	case pD103:
	    GPS_D103_Get(&((*way)[i]),rec->data);
	    break;
	case pD104:
	    GPS_D104_Get(&((*way)[i]),rec->data);
	    break;
	case pD105:
	    GPS_D105_Get(&((*way)[i]),rec->data);
	    break;
	case pD106:
	    GPS_D106_Get(&((*way)[i]),rec->data);
	    break;
	case pD107:
	    GPS_D107_Get(&((*way)[i]),rec->data);
	    break;
	case pD108:
	    GPS_D108_Get(&((*way)[i]),rec->data);
	    break;
	case pD109:
	    GPS_D109_Get(&((*way)[i]),rec->data,109);
	    break;
	case pD110:
	    GPS_D109_Get(&((*way)[i]),rec->data,110);
	    break;
	case pD150:
	    GPS_D150_Get(&((*way)[i]),rec->data);
	    break;
	case pD151:
	    GPS_D151_Get(&((*way)[i]),rec->data);
	    break;
	case pD152:
	    GPS_D152_Get(&((*way)[i]),rec->data);
	    break;
	case pD154:
	    GPS_D154_Get(&((*way)[i]),rec->data);
	    break;
	case pD155:
	    GPS_D155_Get(&((*way)[i]),rec->data);
	    break;
	default:
	    GPS_Error("A200_GET: Unknown route protocol");
	    return PROTOCOL_ERROR;
	}
	(*way)[i-1]->prot = (*way)[i]->prot;
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;

    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("A200_GET: Error transferring routes");
	return FRAMING_ERROR;
    }

    if(i != n)
    {
	GPS_Error("A200_GET: Route entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);


    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}



/* @func GPS_A201_Get ******************************************************
**
** Get route data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/
int32 GPS_A201_Get(const char *port, GPS_PWay **way)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;


    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Rte);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*way)=(GPS_PWay *)malloc(n*sizeof(GPS_PWay))))
	{
	    GPS_Error("A201_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}


    for(i=0;i<n;++i)
    {
	if(!((*way)[i]=GPS_Way_New()))
	    return MEMORY_ERROR;
	
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;

	if(rec->type == LINK_ID[gps_link_type].Pid_Rte_Hdr)
	{
	    switch(gps_rte_hdr_type)
	    {
	    case pD200:
		GPS_D200_Get(&((*way)[i]),rec->data);
		break;
	    case pD201:
		GPS_D201_Get(&((*way)[i]),rec->data);
		break;
	    case pD202:
		GPS_D202_Get(&((*way)[i]),rec->data);
		break;
	    default:
		GPS_Error("A201_GET: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	    (*way)[i]->islink = 0;
	    continue;
	}


	if(rec->type == LINK_ID[gps_link_type].Pid_Rte_Link_Data)
	{
	    switch(gps_rte_link_type)
	    {
	    case pD210:
		GPS_D210_Get(&((*way)[i]),rec->data);
		break;
	    default:
		GPS_Error("A201_GET: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	    (*way)[i]->isrte  = 0;
	    (*way)[i]->islink = 1;
	    continue;
	}
	
	if(rec->type != LINK_ID[gps_link_type].Pid_Rte_Wpt_Data)
	{
	    GPS_Error("A200_GET: Non Pid_rte_Wpt_Data");
	    return FRAMING_ERROR;
	}
	
	(*way)[i]->isrte  = 0;
	(*way)[i]->islink = 0;

	switch(gps_rte_type)
	{
	case pD100:
	    GPS_D100_Get(&((*way)[i]),rec->data);
	    break;
	case pD101:
	    GPS_D101_Get(&((*way)[i]),rec->data);
	    break;
	case pD102:
	    GPS_D102_Get(&((*way)[i]),rec->data);
	    break;
	case pD103:
	    GPS_D103_Get(&((*way)[i]),rec->data);
	    break;
	case pD104:
	    GPS_D104_Get(&((*way)[i]),rec->data);
	    break;
	case pD105:
	    GPS_D105_Get(&((*way)[i]),rec->data);
	    break;
	case pD106:
	    GPS_D106_Get(&((*way)[i]),rec->data);
	    break;
	case pD107:
	    GPS_D107_Get(&((*way)[i]),rec->data);
	    break;
	case pD108:
	    GPS_D108_Get(&((*way)[i]),rec->data);
	    break;
	case pD109:
	    GPS_D109_Get(&((*way)[i]),rec->data,109);
	    break;
	case pD110:
	    GPS_D109_Get(&((*way)[i]),rec->data,110);
	    break;
	case pD150:
	    GPS_D150_Get(&((*way)[i]),rec->data);
	    break;
	case pD151:
	    GPS_D151_Get(&((*way)[i]),rec->data);
	    break;
	case pD152:
	    GPS_D152_Get(&((*way)[i]),rec->data);
	    break;
	case pD154:
	    GPS_D154_Get(&((*way)[i]),rec->data);
	    break;
	case pD155:
	    GPS_D155_Get(&((*way)[i]),rec->data);
	    break;
	default:
	    GPS_Error("A200_GET: Unknown route protocol");
	    return PROTOCOL_ERROR;
	}
	(*way)[i-1]->prot = (*way)[i]->prot;
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;

    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("A200_GET: Error transferring routes");
	return FRAMING_ERROR;
    }

    if(i != n)
    {
	GPS_Error("A200_GET: Route entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);


    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}



/* @func GPS_A200_Send **************************************************
**
** Send routes to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A200_Send(const char *port, GPS_PWay *way, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;
    UC  method;

    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A200_Send: Route start data not acknowledged");
	return FRAMING_ERROR;
    }


    for(i=0;i<n;++i)
    {
	if(way[i]->isrte)
	{
	    method = LINK_ID[gps_link_type].Pid_Rte_Hdr;

	    switch(gps_rte_hdr_type)
	    {
	    case pD200:
		GPS_D200_Send(data,way[i],&len);
		break;
	    case pD201:
		GPS_D201_Send(data,way[i],&len);
		break;
	    case pD202:
		GPS_D202_Send(data,way[i],&len);
		break;
	    default:
		GPS_Error("A200_Send: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	}
	else
	{
	    method = LINK_ID[gps_link_type].Pid_Rte_Wpt_Data;

	    switch(gps_rte_type)
	    {
	    case pD100:
		GPS_D100_Send(data,way[i],&len);
		break;
	    case pD101:
		GPS_D101_Send(data,way[i],&len);
		break;
	    case pD102:
		GPS_D102_Send(data,way[i],&len);
		break;
	    case pD103:
		GPS_D103_Send(data,way[i],&len);
		break;
	    case pD104:
		GPS_D104_Send(data,way[i],&len);
		break;
	    case pD105:
		GPS_D105_Send(data,way[i],&len);
		break;
	    case pD106:
		GPS_D106_Send(data,way[i],&len);
		break;
	    case pD107:
		GPS_D107_Send(data,way[i],&len);
		break;
	    case pD108:
		GPS_D108_Send(data,way[i],&len);
		break;
	    case pD150:
		GPS_D150_Send(data,way[i],&len);
		break;
	    case pD151:
		GPS_D151_Send(data,way[i],&len);
		break;
	    case pD152:
		GPS_D152_Send(data,way[i],&len);
		break;
	    case pD154:
		GPS_D154_Send(data,way[i],&len);
		break;
	    case pD155:
		GPS_D155_Send(data,way[i],&len);
		break;
	    default:
		GPS_Error("A200_Send: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	}
	

	GPS_Make_Packet(&tra, method, data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A200_Send: Route packet not acknowledged");
	    return FRAMING_ERROR;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Wpt);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A200_Send: Route complete data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @func GPS_A201_Send **************************************************
**
** Send routes to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A201_Send(const char *port, GPS_PWay *way, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;
    UC  method;

    if(!GPS_Device_On(port,&fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A200_Send: Route start data not acknowledged");
	return FRAMING_ERROR;
    }


    for(i=0;i<n;++i)
    {
	if(way[i]->isrte)
	{
	    method = LINK_ID[gps_link_type].Pid_Rte_Hdr;

	    switch(gps_rte_hdr_type)
	    {
	    case pD200:
		GPS_D200_Send(data,way[i],&len);
		break;
	    case pD201:
		GPS_D201_Send(data,way[i],&len);
		break;
	    case pD202:
		GPS_D202_Send(data,way[i],&len);
		break;
	    default:
		GPS_Error("A200_Send: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	}
	else if(way[i]->islink)
	{
	    method = LINK_ID[gps_link_type].Pid_Rte_Link_Data;

	    switch(gps_rte_link_type)
	    {
	    case pD210:
		GPS_D210_Send(data,way[i],&len);
		break;
	    default:
		GPS_Error("A201_Send: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	}
	else
	{
	    method = LINK_ID[gps_link_type].Pid_Rte_Wpt_Data;

	    switch(gps_rte_type)
	    {
	    case pD100:
		GPS_D100_Send(data,way[i],&len);
		break;
	    case pD101:
		GPS_D101_Send(data,way[i],&len);
		break;
	    case pD102:
		GPS_D102_Send(data,way[i],&len);
		break;
	    case pD103:
		GPS_D103_Send(data,way[i],&len);
		break;
	    case pD104:
		GPS_D104_Send(data,way[i],&len);
		break;
	    case pD105:
		GPS_D105_Send(data,way[i],&len);
		break;
	    case pD106:
		GPS_D106_Send(data,way[i],&len);
		break;
	    case pD107:
		GPS_D107_Send(data,way[i],&len);
		break;
	    case pD108:
		GPS_D108_Send(data,way[i],&len);
		break;
	    case pD109:
		GPS_D109_Send(data,way[i],&len, 109);
		break;
	    case pD110:
		GPS_D109_Send(data,way[i],&len, 110);
		break;
	    case pD150:
		GPS_D150_Send(data,way[i],&len);
		break;
	    case pD151:
		GPS_D151_Send(data,way[i],&len);
		break;
	    case pD152:
		GPS_D152_Send(data,way[i],&len);
		break;
	    case pD154:
		GPS_D154_Send(data,way[i],&len);
		break;
	    case pD155:
		GPS_D155_Send(data,way[i],&len);
		break;
	    default:
		GPS_Error("A200_Send: Unknown route protocol");
		return PROTOCOL_ERROR;
	    }
	}
	

	GPS_Make_Packet(&tra, method, data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A200_Send: Route packet not acknowledged");
	    return FRAMING_ERROR;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Wpt);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A200_Send: Route complete data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}





/* @funcstatic GPS_D200_Get ********************************************
**
** Get route header data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D200_Get(GPS_PWay *way, UC *s)
{
    (*way)->rte_prot = 200;
    (*way)->rte_num  = *s;
    (*way)->isrte    = 1;

    return;
}



/* @funcstatic GPS_D201_Get *******************************************
**
** Get route header data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D201_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->rte_prot = 201;
    (*way)->rte_num  = *p++;
    (*way)->isrte    = 1;
    for(i=0;i<20;++i) (*way)->rte_cmnt[i] = *p++;

    return;
}



/* @funcstatic GPS_D202_Get ********************************************
**
** Get route header data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D202_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    UC *q;

    p=s;

    (*way)->rte_prot = 202;
#if 0
    /* D202 has only a null terminated string for rte_ident */
    (*way)->rte_num  = *p++;
#endif
    (*way)->isrte    = 1;
    q = (UC *) (*way)->rte_ident;
    while((*q++=*p++));

    return;
}



/* @funcstatic GPS_D210_Get ********************************************
**
** Get route link data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D210_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    UC *q;
    int32 i;

    p=s;

    (*way)->rte_link_class = GPS_Util_Get_Short(p);
    p+=sizeof(int16);
    for(i=0;i<18;++i) (*way)->rte_link_subclass[i] = *p++;
    q = (UC *) (*way)->rte_link_ident;
    while((*q++=*p++));

    return;
}



/* @funcstatic GPS_D200_Send *******************************************
**
** Form route header data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D200_Send(UC *data, GPS_PWay way, int32 *len)
{

    *data = way->rte_num;
    *len = 1;

    return;
}



/* @funcstatic GPS_D201_Send *******************************************
**
** Form route header data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D201_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;

    p = data;

    *p++ = way->rte_num;
    copy_char_array(&p, way->rte_cmnt, 20, UpperYes);
    *len = 21;

    return;
}



/* @funcstatic GPS_D202_Send ********************************************
**
** Form route header data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D202_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    UC *q;

    p = data;
    q = (UC *) way->rte_ident;

    while((*p++ = *q++));

    *len = p-data;

    return;
}



/* @funcstatic GPS_D210_Send ********************************************
**
** Form route link data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D210_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    UC *q;
    int32 i;

    p = data;

    GPS_Util_Put_Short(p,(US) way->rte_link_class);
    p+=sizeof(int16);
    for(i=0;i<18;++i) *p++ = way->rte_link_subclass[i];

    q = (UC *) way->rte_link_ident;
    while((*p++ = *q++));

    *len = p-data;

    return;
}



/* @func GPS_A300_Get ******************************************************
**
** Get track data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] trk [GPS_PTrack **] track array
**
** @return [int32] number of track entries
************************************************************************/
int32 GPS_A300_Get(const char *port, GPS_PTrack **trk, pcb_fn cb)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;
    int32 ret;


    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    /* Only those GPS' with L001 can send track data */
    if(!LINK_ID[gps_link_type].Pid_Trk_Data)
    {
	GPS_Warning("A300 protocol unsupported");
	return GPS_UNSUPPORTED;
    }

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Trk);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;
    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;


    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*trk)=(GPS_PTrack *)malloc(n*sizeof(GPS_PTrack))))
	{
	    GPS_Error("A300_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}
    for(i=0;i<n;++i)
	if(!((*trk)[i]=GPS_Track_New()))
	    return MEMORY_ERROR;

    switch(gps_trk_type)
    {
    case pD300:
	ret = GPS_D300_Get(*trk,n,fd);
	if(ret<0) return ret;
	break;
    default:
	GPS_Error("A300_GET: Unknown track protocol");
	return PROTOCOL_ERROR;
    }

    if(ret != n)
    {
	GPS_Error("A300_GET: Track entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return ret;
}

/*
 * This is to get around a problem with the x305 sporting units.
 * The unit will not "finalize" a track unless the operator manually
 * does it from the pushbutton panel OR until the device has gone through
 * a 'get runs' cycle.  Garmin's Training Center, of course, does this
 * because it actually uses that data.   Here we just go through the
 * mechanics of building and sending the requests and then throwing away
 * all the data in order to finalize that.
 *
 * Hopefully, this won't be needed forever.
 */
int
drain_run_cmd(gpsdevh *fd)
{
    GPS_PPacket tra;
    GPS_PPacket rec;
    static UC data[2];

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Runs);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);

    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    for(;;) {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
    	if(rec->type == LINK_ID[gps_link_type].Pid_Xfer_Cmplt) {
	    break;
	}
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);
  return 0;
}

/* @func GPS_A301_Get ******************************************************
**
** Get track data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] trk [GPS_PTrack **] track array
**
** @return [int32] number of track entries
************************************************************************/
int32 GPS_A301_Get(const char *port, GPS_PTrack **trk, pcb_fn cb)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;

    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    /* Only those GPS' with L001 can send track data */
    if(!LINK_ID[gps_link_type].Pid_Trk_Data)
    {
	GPS_Warning("A301 protocol unsupported");
	return GPS_UNSUPPORTED;
    }

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if ((gps_trk_type == pD304) && gps_run_transfer) {
	drain_run_cmd(fd);
    }

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Trk);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;
    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;


    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*trk)=(GPS_PTrack *)malloc(n*sizeof(GPS_PTrack))))
	{
	    GPS_Error("A301_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}
    for(i=0;i<n;++i)
	if(!((*trk)[i]=GPS_Track_New()))
	    return MEMORY_ERROR;

    for(i=0;i<n;++i)
    {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
	if(rec->type == LINK_ID[gps_link_type].Pid_Trk_Hdr)
	{
	    switch(gps_trk_hdr_type)
	    {
	    case pD310:
	    case pD312:
		GPS_D310_Get(&((*trk)[i]),rec->data);
		break;
	    case pD311:
		GPS_D311_Get(&((*trk)[i]),rec->data);
		break;
	    default:
		GPS_Error("A301_Get: Unknown track protocol");
		return PROTOCOL_ERROR;
	    }
	    (*trk)[i]->ishdr = 1;
	    continue;
	}
	
	if(rec->type != LINK_ID[gps_link_type].Pid_Trk_Data)
	{
	    GPS_Error("A301_Get: Non-Pid_Trk_Data");
	    return FRAMING_ERROR;
	}

	(*trk)[i]->ishdr = 0;
	
	switch(gps_trk_type)
	{
	case pD300:
	    GPS_D300b_Get(&((*trk)[i]),rec->data);
	    break;
	case pD301:
	    GPS_D301b_Get(&((*trk)[i]),rec->data);
	    break;
	case pD302:
	    GPS_D302b_Get(&((*trk)[i]),rec->data);
	    break;
	case pD303:
	case pD304:
	    GPS_D303b_Get(&((*trk)[i]),rec->data);
	    break;
	default:
	    GPS_Error("A301_GET: Unknown track protocol");
	    return PROTOCOL_ERROR;
	}
	/* Cheat and don't _really_ pass the trkpt back */
	cb(n, NULL);
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;
    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("A301_Get: Error transferring tracks");
	return FRAMING_ERROR;
    }

    if(i != n)
    {
	GPS_Error("A301_GET: Track entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}





/* @func GPS_A300_Send **************************************************
**
** Send track log to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PTrack *] track array
** @param [r] n [int32] number of track entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A300_Send(const char *port, GPS_PTrack *trk, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;

    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    /* Only those GPS' with L001 can send track data */
    if(!LINK_ID[gps_link_type].Pid_Trk_Data)
    {
	GPS_Warning("A300 protocol unsupported");
	return GPS_UNSUPPORTED;
    }

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A300_Send: Track start data not acknowledged");
	return FRAMING_ERROR;
    }

    for(i=0;i<n;++i)
    {
	switch(gps_trk_type)
	{
	case pD300:
	    GPS_D300_Send(data,trk[i]);
	    len = 13;
	    break;
	default:
	    GPS_Error("A300_Send: Unknown track protocol");
	    return PROTOCOL_ERROR;
	}

	GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Trk_Data,
			data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A300_Send: Pid_Trk_Data not acknowledgedn");
	    return FRAMING_ERROR;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Trk);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A300_Send: Track complete data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @func GPS_A301_Send **************************************************
**
** Send track log to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PTrack *] track array
** @param [r] n [int32] number of track entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A301_Send(const char *port, GPS_PTrack *trk, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;
    UC  method;

    if(gps_trk_transfer == -1)
	return GPS_UNSUPPORTED;

    if(gps_trk_transfer == 302) {
	GPS_Warning("A302: Device does not support sending tracks to it. ");
	return GPS_UNSUPPORTED;
    }

    /* Only those GPS' with L001 can send track data */
    if(!LINK_ID[gps_link_type].Pid_Trk_Data)
    {
	GPS_Warning("A301 protocol unsupported");
	return GPS_UNSUPPORTED;
    }

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A301_Send: Track start data not acknowledged");
	return FRAMING_ERROR;
    }


    for(i=0;i<n;++i)
    {
	if(trk[i]->ishdr)
	{
	    method = LINK_ID[gps_link_type].Pid_Trk_Hdr;

	    switch(gps_trk_hdr_type)
	    {
	    case pD310:
	    case pD312:
		GPS_D310_Send(data,trk[i],&len);
		break;
	    case pD311:
		GPS_D311_Send(data,trk[i],&len);
		break;
	    default:
		GPS_Error("A301_Send: Unknown track protocol %d", gps_trk_hdr_type);
		return PROTOCOL_ERROR;
	    }
	}
	else
	{
	    method = LINK_ID[gps_link_type].Pid_Trk_Data;

	    switch(gps_trk_type)
	    {
	    case pD300:
		GPS_D300_Send(data,trk[i]);
		len = 13;
		break;
	    case pD301:
		GPS_D301_Send(data,trk[i], 301);
		len = 21;
		break;
	    case pD302:
		GPS_D301_Send(data,trk[i], 302);
		len = 25;
		break;
	    case pD304:
		GPS_D304_Send(data,trk[i]);
		len = 23;
		break;
	    default:
		GPS_Error("A301_Send: Unknown track protocol");
		return PROTOCOL_ERROR;
	    }
	}
	

	GPS_Make_Packet(&tra, method, data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A301_Send: Track packet not acknowledgedn");
	    return FRAMING_ERROR;
	}
    }
	

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Trk);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A301_Send: Track complete data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @func GPS_D300_Get ******************************************************
**
** Get track data
**
** @param [w] trk [GPS_PTrack *] track array
** @param [r] entries [int32] number of packets to receive
** @param [r] fd [int32] file descriptor
**
** @return [int32] number of entries read
************************************************************************/
int32 GPS_D300_Get(GPS_PTrack *trk, int32 entries, gpsdevh *fd)
{
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    for(i=0;i<entries;++i)
    {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
	
	GPS_A300_Translate(rec->data, &trk[i]);
    }


    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;


    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("D300_GET: Error transferring track log");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);
	
    return i;
}



/* @func GPS_D300b_Get ******************************************************
**
** Get track data (A301 protocol)
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] data [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D300b_Get(GPS_PTrack *trk, UC *data)
{

    GPS_A300_Translate(data, trk);
    return;
}



/* @func GPS_D301b_Get ******************************************************
**
** Get track data (A301 protocol)
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] data [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D301b_Get(GPS_PTrack *trk, UC *data)
{
    UC *p;
    uint32 t;

    p=data;

    (*trk)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*trk)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    t = GPS_Util_Get_Uint(p);
    if(!t || t==0x7fffffff || t==0xffffffff)
	(*trk)->Time=0;
    else
	(*trk)->Time = GPS_Math_Gtime_To_Utime((time_t)t);
    p+=sizeof(uint32);

    (*trk)->alt = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*trk)->dpth = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*trk)->tnew = *p;

    return;
}

/* @func GPS_D302b_Get ******************************************************
**
** Get track data (A301 protocol)
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] data [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D302b_Get(GPS_PTrack *trk, UC *data)
{
    UC *p;
    uint32 t;
    double gps_temp;

    p=data;

    (*trk)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*trk)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    t = GPS_Util_Get_Uint(p);
    if(!t || t==0x7fffffff || t==0xffffffff)
	(*trk)->Time=0;
    else
	(*trk)->Time = GPS_Math_Gtime_To_Utime((time_t)t);
    p+=sizeof(uint32);

    (*trk)->alt = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*trk)->dpth = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    /* The only difference between 302 and 301 is the presence of temp
     * in the middle.   Nice planning, eh?
     */
    gps_temp = GPS_Util_Get_Float(p);
    if (gps_temp <= 1.0e24) {
	    (*trk)->temperature_populated = 1;
	    (*trk)->temperature = gps_temp;
    }

    p+=sizeof(float);

    (*trk)->tnew = *p;

    return;
}


/* @func GPS_D303b_Get ******************************************************
**
** Get track data (A302 protocol) -- used in Forerunner 301
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] data [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D303b_Get(GPS_PTrack *trk, UC *data)
{
    UC *p;
    uint32 t;
    uint32 raw_lat, raw_lon;
    int lat_undefined, lon_undefined;
    p=data;

    /* Latitude and longitude are sometimes invalid (0x7fffffff or
     * maybe 0xffffffff?) I guess this makes sense if the device is
     * reporting heart rate and time anyway.  I presume that latitude
     * and longitude are defined or left undefined together?
     */
    raw_lat = GPS_Util_Get_Int(p);
    lat_undefined = !raw_lat || raw_lat==0x7fffffff || raw_lat==0xffffffff;
    if (lat_undefined)
	(*trk)->lat=0;
    else
	(*trk)->lat = GPS_Math_Semi_To_Deg(raw_lat);
    p+=sizeof(int32);

    raw_lon = GPS_Util_Get_Int(p);
    lon_undefined = !raw_lon || raw_lon==0x7fffffff || raw_lon==0xffffffff;
    if (lon_undefined)
	(*trk)->lon=0;
    else
	(*trk)->lon = GPS_Math_Semi_To_Deg(raw_lon);
    p+=sizeof(int32);

    /*
     * Let the caller decide if it wants to toss trackpionts with only
     * hear and/or time data.
     */
    if (lat_undefined || lon_undefined) {
	(*trk)->no_latlon = 1;
    }

    if (lat_undefined != lon_undefined)
	GPS_Warning("GPS_D303b_Get: assumption (lat_undefined == lon_undefined) violated");

    t = GPS_Util_Get_Uint(p);

    if(!t || t==0x7fffffff || t==0xffffffff)
	(*trk)->Time=0;
    else
	(*trk)->Time = GPS_Math_Gtime_To_Utime((time_t)t);
    p+=sizeof(uint32);

    /* When latitude and longitude are undefined, this field seems to be
     * a constant on my receiver (51 59 04 69) */
    (*trk)->alt = GPS_Util_Get_Float(p);
    if (lat_undefined || lon_undefined) (*trk)->alt = 0.0f;
    p+=sizeof(float);

    /* Heartrate is reported as 0 if there is no signal from
     * a heartrate monitor.
     *  305 and 304 are identical until now.
     */
    switch (gps_trk_type) {
    case pD304:
	(*trk)->distance = GPS_Util_Get_Float(p);
	p+=sizeof(float); /* A float indicating number of meters travelled. */
	
	(*trk)->heartrate = (*p++);
	/* crank cadence, RPM, 0xff if invalid.  */
	if (*p != 0xff) {
		(*trk)->cadence = (*p);
	}

	/* sensor present.  Boolean */
	p++;
	
	break;
    case pD303:
	(*trk)->heartrate = *p++;
	break;
    }
	
    /* There doesn't seem to be a trk_seg bool, or at least I've not
     * observed it yet.  One possibility is to start a new segment
     * each time latitude and longitude are undefined? (Ie data from
     * the heartrate monitor but none from the GPS. */
    (*trk)->tnew = 0;	

    return;
}


/* @func GPS_D310_Get ******************************************************
**
** Get track header data (A301 protocol)
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D310_Get(GPS_PTrack *trk, UC *s)
{
    UC *p;
    UC *q;

    p=s;

    (*trk)->dspl = *p++;
    (*trk)->colour = *p++;

    q = (UC *) (*trk)->trk_ident;

    while((*q++ = *p++));

    return;
}

/* @func GPS_D311_Get ******************************************************
**
** Get track header data (A301 protocol)
**
** @param [w] trk [GPS_PTrack *] track
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
void GPS_D311_Get(GPS_PTrack *trk, UC *s)
{
    UC *p;
    short identifier;

    p=s;

    /* Forerunner */
    identifier = GPS_Util_Get_Short(s);
    sprintf((*trk)->trk_ident, "%d", identifier);
    (*trk)->tnew = 1;

    return;
}


/* @func GPS_D300_Send **************************************************
**
** Form track data string
**
** @param [w] data [UC *] string to write to
** @param [r] trk [GPS_PTrack] track data
**
** @return [void]
************************************************************************/
void GPS_D300_Send(UC *data, GPS_PTrack trk)
{
    UC *p;

    p = data;
    GPS_A300_Encode(p,trk);

    return;
}



/* @func GPS_D301_Send **************************************************
**
** Form track data string
**
** @param [w] data [UC *] string to write to
** @param [r] trk [GPS_PTrack] track data
**
** @return [void]
************************************************************************/
void GPS_D301_Send(UC *data, GPS_PTrack trk, int type)
{
    UC *p;

    p = data;
    GPS_A300_Encode(p,trk);
    p = data+12;

    GPS_Util_Put_Float(p,trk->alt);
    p+=sizeof(float);
    GPS_Util_Put_Float(p,trk->dpth);
    p+=sizeof(float);

    if (type == 302) {
      GPS_Util_Put_Float(p,1.0e25);
      p+=sizeof(float);
    }

    *p = trk->tnew;

    return;
}

void GPS_D304_Send(UC *data, GPS_PTrack trk)
{
    UC *p;

    p = data;
    GPS_A300_Encode(p,trk);
    p = data+12;

    GPS_Util_Put_Float(p,trk->alt);
    p+=sizeof(float);

    GPS_Util_Put_Float(p, (const float) 1.0e25);
    p+=sizeof(float);

    /* Not really clear if the members below makes sense to write or not */

    *p = trk->heartrate;
    p+=sizeof(char);

    *p = trk->cadence;
    p+=sizeof(char);

    *p = trk->cadence > 0 ? trk->cadence : 0xff;
    p+=sizeof(char);

    return;
}

/* @func GPS_D311_Send **************************************************
**
** Form track header data string
**
** @param [w] data [UC *] string to write to
** @param [r] trk [GPS_PTrack] track data
** @param [w] len [int32 *] length of data
**
** @return [void]
************************************************************************/
void GPS_D311_Send(UC *data, GPS_PTrack trk, int32 *len)
{
    UC *p;
    int identifier;

    p = data;
//    sscanf(trk->trk_ident, "%d", &identifier);
    identifier = 0x1234;
    GPS_Util_Put_Short(p,identifier);
    p += 2;
    *len = p-data;

    return;
}

/* @func GPS_D310_Send **************************************************
**
** Form track header data string
**
** @param [w] data [UC *] string to write to
** @param [r] trk [GPS_PTrack] track data
** @param [w] len [int32 *] length of data
**
** @return [void]
************************************************************************/
void GPS_D310_Send(UC *data, GPS_PTrack trk, int32 *len)
{
    UC *p;
    UC *q;

    p = data;

    *p++ = trk->dspl;
    *p++ = trk->colour;

    q = (UC *) trk->trk_ident;
    while((*p++ = *q++));

    *len = p-data;

    return;
}


/* @funcstatic  GPS_A300_Translate ***************************************
**
** Translate track packet to track structure
**
** @param [r] s [const UC *] track packet data
** @param [w] trk [GPS_PTrack *] track entry pointer
**
** @return [void]
************************************************************************/
static void GPS_A300_Translate(UC *s, GPS_PTrack *trk)
{
    UC *p;
    uint32 t;

    p=s;

    (*trk)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*trk)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    t = GPS_Util_Get_Uint(p);
    if(!t || t==0x7fffffff || t==0xffffffff)
	(*trk)->Time=0;
    else
	(*trk)->Time = GPS_Math_Gtime_To_Utime((time_t)t);
    p+=sizeof(uint32);

    (*trk)->tnew = *p;

    return;
}



/* @funcstatic  GPS_A300_Encode ***************************************
**
** Encode track structure to track packet
**
** @param [w] s [UC *] string to write to
** @param [r] trk [GPS_PTrack] track entry
**
** @return [void]
************************************************************************/
static void GPS_A300_Encode(UC *s, GPS_PTrack trk)
{
    UC *p;

    p=s;

    GPS_Util_Put_Int(p,GPS_Math_Deg_To_Semi(trk->lat));
    p+=sizeof(int32);

    GPS_Util_Put_Int(p,GPS_Math_Deg_To_Semi(trk->lon));
    p+=sizeof(int32);

    GPS_Util_Put_Uint(p,GPS_Math_Utime_To_Gtime(trk->Time));
    p+=sizeof(uint32);

    *p = (UC) trk->tnew;

    return;
}



/* @func GPS_A400_Get **************************************************
**
** Get proximity waypoint data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] way [GPS_PWay **] waypoint array
**
** @return [int32] number of waypoint entries
************************************************************************/
int32 GPS_A400_Get(const char *port, GPS_PWay **way)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 n;
    int32 i;

    if(gps_prx_waypt_transfer == -1)
	return GPS_UNSUPPORTED;


    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Prx);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    if(!GPS_Device_Chars_Ready(fd))
    {
	GPS_Warning("A400 (ppx) protocol not supported");
	GPS_Packet_Del(&rec);
	GPS_Packet_Del(&tra);

	if(!GPS_Device_Off(fd))
	    return gps_errno;

	return GPS_UNSUPPORTED;
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;

    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    n = GPS_Util_Get_Short(rec->data);

    if(n)
	if(!((*way)=(GPS_PWay *)malloc(n*sizeof(GPS_PWay))))
	{
	    GPS_Error("A400_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}


    for(i=0;i<n;++i)
    {
	if(!((*way)[i]=GPS_Way_New()))
	    return MEMORY_ERROR;
	
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;


	switch(gps_prx_waypt_type)
	{
	case pD400:
	    GPS_D400_Get(&((*way)[i]),rec->data);
	    break;
	case pD101:
	    GPS_D101_Get(&((*way)[i]),rec->data);
	    break;
	case pD102:
	    GPS_D102_Get(&((*way)[i]),rec->data);
	    break;
	case pD403:
	    GPS_D403_Get(&((*way)[i]),rec->data);
	    break;
	case pD104:
	    GPS_D104_Get(&((*way)[i]),rec->data);
	    break;
	case pD105:
	    GPS_D105_Get(&((*way)[i]),rec->data);
	    break;
	case pD106:
	    GPS_D106_Get(&((*way)[i]),rec->data);
	    break;
	case pD107:
	    GPS_D107_Get(&((*way)[i]),rec->data);
	    break;
	case pD108:
	    GPS_D108_Get(&((*way)[i]),rec->data);
	    break;
	case pD109:
	    GPS_D109_Get(&((*way)[i]),rec->data,109);
	    break;
	case pD110:
	    GPS_D109_Get(&((*way)[i]),rec->data,110);
	    break;
	case pD450:
	    GPS_D450_Get(&((*way)[i]),rec->data);
	    break;
	case pD151:
	    GPS_D151_Get(&((*way)[i]),rec->data);
	    break;
	case pD152:
	    GPS_D152_Get(&((*way)[i]),rec->data);
	    break;
	case pD154:
	    GPS_D154_Get(&((*way)[i]),rec->data);
	    break;
	case pD155:
	    GPS_D155_Get(&((*way)[i]),rec->data);
	    break;
	default:
	    GPS_Error("A400_GET: Unknown prx waypoint protocol");
	    return PROTOCOL_ERROR;
	}
    }

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    if(rec->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt)
    {
	GPS_Error("A400_GET: Error transferring prx waypoints");
	return FRAMING_ERROR;
    }

    if(i != n)
    {
	GPS_Error("A400_GET: Prx waypoint entry number mismatch");
	return FRAMING_ERROR;
    }


    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}



/* @func GPS_A400_Send **************************************************
**
** Send proximity waypoints to GPS
**
** @param [r] port [const char *] serial port
** @param [r] trk [GPS_PWay *] waypoint array
** @param [r] n [int32] number of waypoint entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A400_Send(const char *port, GPS_PWay *way, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;

    if(gps_prx_waypt_transfer == -1)
	return GPS_UNSUPPORTED;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A400_Send: Prx start data not acknowledgedn");
	return FRAMING_ERROR;
    }


    for(i=0;i<n;++i)
    {
	switch(gps_prx_waypt_type)
	{
	case pD400:
	    GPS_D400_Send(data,way[i],&len);
	    break;
	case pD101:
	    GPS_D101_Send(data,way[i],&len);
	    break;
	case pD102:
	    GPS_D102_Send(data,way[i],&len);
	    break;
	case pD403:
	    GPS_D403_Send(data,way[i],&len);
	    break;
	case pD104:
	    GPS_D104_Send(data,way[i],&len);
	    break;
	case pD105:
	    GPS_D105_Send(data,way[i],&len);
	    break;
	case pD106:
	    GPS_D106_Send(data,way[i],&len);
	    break;
	case pD107:
	    GPS_D107_Send(data,way[i],&len);
	    break;
	case pD108:
	    GPS_D108_Send(data,way[i],&len);
	    break;
	case pD450:
	    GPS_D450_Send(data,way[i],&len);
	    break;
	case pD151:
	    GPS_D151_Send(data,way[i],&len);
	    break;
	case pD152:
	    GPS_D152_Send(data,way[i],&len);
	    break;
	case pD154:
	    GPS_D154_Send(data,way[i],&len);
	    break;
	case pD155:
	    GPS_D155_Send(data,way[i],&len);
	    break;
	default:
	    GPS_Error("A400_Send: Unknown prx waypoint protocol");
	    return PROTOCOL_ERROR;
	}

	GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Prx_Wpt_Data,
			data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A400_Send: Pid_Prx_Wpt_Data not acknowledged");
	    return FRAMING_ERROR;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Prx);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A400_Send: Prx waypoint complete data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @funcstatic GPS_D400_Get ********************************************
**
** Get proximity waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D400_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 400;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst=GPS_Util_Get_Float(p);


    return;
}


/* @funcstatic GPS_D403_Get ********************************************
**
** Get proximity waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D403_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 403;
    for(i=0;i<6;++i) (*way)->ident[i] = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    p+=sizeof(int32);

    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->smbl = *p++;
    (*way)->dspl = *p++;

    (*way)->dst=GPS_Util_Get_Float(p);

    return;
}


/* @funcstatic GPS_D450_Get ********************************************
**
** Get proximity waypoint data
**
** @param [w] way [GPS_PWay *] waypoint array
** @param [r] s [UC *] packet data
**
** @return [void]
************************************************************************/
static void GPS_D450_Get(GPS_PWay *way, UC *s)
{
    UC *p;
    int32 i;

    p=s;

    (*way)->prot = 450;

    (*way)->idx = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<6;++i) (*way)->ident[i] = *p++;
    for(i=0;i<2;++i) (*way)->cc[i] = *p++;
    (*way)->wpt_class = *p++;

    (*way)->lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*way)->alt = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    for(i=0;i<24;++i) (*way)->city[i] = *p++;
    for(i=0;i<2;++i) (*way)->state[i] = *p++;
    for(i=0;i<30;++i) (*way)->name[i] = *p++;
    for(i=0;i<40;++i) (*way)->cmnt[i] = *p++;

    (*way)->dst=GPS_Util_Get_Float(p);

    return;
}


/* @funcstatic GPS_D400_Send ********************************************
**
** Form proximity waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D400_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    for(i=0;i<6;++i) *p++ = way->ident[i];
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    for(i=0;i<40;++i) *p++ = way->cmnt[i];

    GPS_Util_Put_Float(p,way->dst);

    *len = 62;

    return;
}


/* @funcstatic GPS_D403_Send *******************************************
**
** Form proximity waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D403_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    for(i=0;i<6;++i) *p++ = way->ident[i];
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);
    GPS_Util_Put_Uint(p,0);
    p+=sizeof(int32);
    for(i=0;i<40;++i) *p++ = way->cmnt[i];

    *p++ = way->smbl;
    *p   = way->dspl;

    GPS_Util_Put_Float(p,way->dst);

    *len = 64;

    return;
}


/* @funcstatic GPS_D450_Send *******************************************
**
** Form proximity waypoint data string
**
** @param [w] data [UC *] string to write to
** @param [r] way [GPS_PWay] waypoint data
** @param [w] len [int32 *] packet length
**
** @return [void]
************************************************************************/
static void GPS_D450_Send(UC *data, GPS_PWay way, int32 *len)
{
    UC *p;
    int32 i;

    p = data;

    GPS_Util_Put_Short(p,(US) way->idx);
    p+=sizeof(int16);

    for(i=0;i<6;++i) *p++ = way->ident[i];
    for(i=0;i<2;++i) *p++ = way->cc[i];
    *p++ = way->wpt_class;

    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lat));
    p+=sizeof(int32);
    GPS_Util_Put_Int(p,(int32)GPS_Math_Deg_To_Semi(way->lon));
    p+=sizeof(int32);

    GPS_Util_Put_Short(p,(US) way->alt);
    p+=sizeof(int16);

    for(i=0;i<24;++i) *p++ = way->city[i];
    for(i=0;i<2;++i)  *p++ = way->state[i];
    for(i=0;i<30;++i) *p++ = way->name[i];
    for(i=0;i<40;++i) *p++ = way->cmnt[i];

    GPS_Util_Put_Float(p,way->dst);


    *len = 121;

    return;
}



/* @func GPS_A500_Get ******************************************************
**
** Get almanac from GPS
**
** @param [r] port [const char *] serial port
** @param [w] alm [GPS_PAlmanac **] almanac array
**
** @return [int32] number of almanac entries
************************************************************************/
int32 GPS_A500_Get(const char *port, GPS_PAlmanac **alm)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket trapkt;
    GPS_PPacket recpkt;
    int32 i, n;

    if (gps_almanac_transfer == -1)
	return GPS_UNSUPPORTED;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if (!(trapkt = GPS_Packet_New() ) || !(recpkt = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Alm);
    GPS_Make_Packet(&trapkt, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,trapkt))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &trapkt, &recpkt))
	return gps_errno;
    if(!GPS_Packet_Read(fd, &recpkt))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &trapkt, &recpkt))
	return gps_errno;

    n = GPS_Util_Get_Short(recpkt->data);

    if(n)
	if(!((*alm)=(GPS_PAlmanac *)malloc(n*sizeof(GPS_PAlmanac))))
	{
	    GPS_Error("A500_Get: Insufficient memory");
	    return MEMORY_ERROR;
	}

    for(i=0;i<n;++i) {
	if(!((*alm)[i]=GPS_Almanac_New()))
	    return MEMORY_ERROR;
		if(!GPS_Packet_Read(fd, &recpkt)) {
			return gps_errno;
		}

		if(!GPS_Send_Ack(fd, &trapkt, &recpkt)) {
			return gps_errno;
		}

		switch(gps_almanac_type) {
    case pD500:
	GPS_A500_Translate(recpkt->data, &((*alm)[i]));
	break;
    case pD501:
	GPS_A500_Translate(recpkt->data, &((*alm)[i]));
	(*alm)[i]->hlth=recpkt->data[42];
	break;
    case pD550:
	(*alm)[i]->svid = recpkt->data[0];
	GPS_A500_Translate(recpkt->data+1, &((*alm)[i]));
	break;
    case pD551:
	(*alm)[i]->svid = recpkt->data[0];
	GPS_A500_Translate(recpkt->data+1, &((*alm)[i]));
	(*alm)[i]->hlth = recpkt->data[43];
	break;
    default:
	GPS_Error("A500_GET: Unknown almanac protocol");
	return PROTOCOL_ERROR;
    }
		/* Cheat and don't _really_ pass the trkpt back */
/*		cb(n, NULL);*/
	}

    if(!GPS_Packet_Read(fd, &recpkt))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &trapkt, &recpkt))
	return gps_errno;
    if(recpkt->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt) {
	GPS_Error("A500_Get: Error transferring almanac");
	return FRAMING_ERROR;
    }

    if(i != n) {
	GPS_Error("A500_GET: Almanac entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&trapkt);
    GPS_Packet_Del(&recpkt);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return n;
}


/* @func GPS_A500_Send **************************************************
**
** Send almanac to GPS
**
** @param [r] port [const char *] serial port
** @param [r] alm [GPS_PAlmanac *] almanac array
** @param [r] n [int32] number of almanac entries
**
** @return [int32] success
************************************************************************/
int32 GPS_A500_Send(const char *port, GPS_PAlmanac *alm, int32 n)
{
    UC data[GPS_ARB_LEN];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 i;
    int32 len;
    int32 timesent;
    int32 posnsent;
    int32 ret;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,(US) n);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Records,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A500_Send: Almanac start data not acknowledged");
	return FRAMING_ERROR;
    }


    for(i=0;i<n;++i)
    {
	switch(gps_almanac_type)
	{
	case pD500:
	    if(n!=32)
	    {
		GPS_Error("A500_Send: SATELLITES: n!=32 specified");
		GPS_Packet_Del(&tra);
		GPS_Packet_Del(&rec);
		return PROTOCOL_ERROR;
	    }
	    GPS_D500_Send(data,alm[i]);
	    len = 42;
	    break;
	case pD501:
	    if(n!=32)
	    {
		GPS_Error("A500_Send: SATELLITES: n!=32 specified");
		GPS_Packet_Del(&tra);
		GPS_Packet_Del(&rec);
		return PROTOCOL_ERROR;
	    }
	    GPS_D501_Send(data,alm[i]);
	    len = 43;
	    break;
	case pD550:
	    GPS_D550_Send(data,alm[i]);
	    len = 43;
	    break;
	case pD551:
	    GPS_D551_Send(data,alm[i]);
	    len = 44;
	    break;
	default:
	    GPS_Error("A500_Send: Unknown almanac protocol");
	    return 0;
	}

	GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Almanac_Data,
			data,(US) len);

	if(!GPS_Write_Packet(fd,tra))
	    return gps_errno;

	if(!GPS_Get_Ack(fd, &tra, &rec))
	{
	    GPS_Error("A500_Send: Almanac Pid_Almanac_Data not acknowledged");
	    return FRAMING_ERROR;
	}
    }

    GPS_Util_Put_Short(data,COMMAND_ID[gps_device_command].Cmnd_Transfer_Alm);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Xfer_Cmplt,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
    {
	GPS_Error("A500_Send: Almanac complete data not acknowledged");
	return FRAMING_ERROR;
    }

    timesent=posnsent=0;

    /*
     *  Allow GPS a little while to decide whether it wants to ask for
     *  the time. Note that the time sent is held in gps_save_time
     *  global
     */
    if(GPS_Device_Wait(fd))
    {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
	
	if(rec->type == LINK_ID[gps_link_type].Pid_Command_Data &&
	   GPS_Util_Get_Short(rec->data) == COMMAND_ID[gps_device_command].
	   Cmnd_Transfer_Time)
	{
	    GPS_User("INFO: GPS time request. Sending....");
	    ret = GPS_Rqst_Send_Time(fd,gps_save_time);
	    if(ret < 0) return ret;
	    timesent=1;
	}
    }



    /*
     *  Allow GPS a little while to decide whether it wants to ask for
     *  the position. Note that the posn sent is held in gps_save_lat
     *  and gps_save_lon global!
     */
    if(GPS_Device_Wait(fd))
    {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
	
	if(rec->type == LINK_ID[gps_link_type].Pid_Command_Data &&
	   GPS_Util_Get_Short(rec->data) == COMMAND_ID[gps_device_command].
	   Cmnd_Transfer_Posn)
	{
	    GPS_User("INFO: GPS position request. Sending....");
	    ret = GPS_Rqst_Send_Position(fd,gps_save_lat,gps_save_lon);
	    if(ret < 0) return ret;
	    posnsent=1;
	}
    }

    if(!timesent)
    {
	ret = GPS_Rqst_Send_Time(fd,gps_save_time);
	if(ret < 0) return ret;
    }


    if(!posnsent)
    {
	ret = GPS_Rqst_Send_Position(fd,gps_save_lat,gps_save_lon);
	if(ret < 0) return ret;
    }


    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}

/* @funcstatic  GPS_A500_Translate ***************************************
**
** Translate almanac packet to almanac structure
**
** @param [r] s [const UC *] almanac packet data
** @param [w] alm [GPS_PAlmanac *] almanac entry pointer
**
** @return [void]
************************************************************************/
static void GPS_A500_Translate(UC *s, GPS_PAlmanac *alm)
{
    UC *p;

    p=s;

    (*alm)->wn = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    (*alm)->toa = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->af0 = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->af1 = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->e = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->sqrta = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->m0 = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->w = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->omg0 = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->odot = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*alm)->i = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    return;
}


/* @funcstatic GPS_D500_Send *******************************************
**
** Form almanac data string
**
** @param [w] data [UC *] string to write to
** @param [r] alm [GPS_PAlmanac] almanac data
**
** @return [void]
************************************************************************/
static void GPS_D500_Send(UC *data, GPS_PAlmanac alm)
{
    UC *p;

    p = data;
    GPS_A500_Encode(p,alm);

    return;
}



/* @funcstatic GPS_D501_Send ********************************************
**
** Form almanac data string
**
** @param [w] data [UC *] string to write to
** @param [r] alm [GPS_PAlmanac] almanac data
**
** @return [void]
************************************************************************/
static void GPS_D501_Send(UC *data, GPS_PAlmanac alm)
{
    UC *p;

    p=data;
    p[42] = alm->hlth;
    GPS_A500_Encode(p,alm);

    return;
}



/* @funcstatic GPS_D550_Send ********************************************
**
** Form almanac data string
**
** @param [w] data [UC *] string to write to
** @param [r] alm [GPS_PAlmanac] almanac data
**
** @return [void]
************************************************************************/
static void GPS_D550_Send(UC *data, GPS_PAlmanac alm)
{
    UC *p;

    p = data;
    *p = alm->svid;
    GPS_A500_Encode(p+1,alm);

    return;
}



/* @funcstatic GPS_D551_Send ********************************************
**
** Form almanac data string
**
** @param [w] data [UC *] string to write to
** @param [r] alm [GPS_PAlmanac] almanac data
**
** @return [void]
************************************************************************/
static void GPS_D551_Send(UC *data, GPS_PAlmanac alm)
{
    UC *p;

    p = data;
    *p = alm->svid;
    GPS_A500_Encode(p+1,alm);
    p[43] = alm->hlth;

    return;
}



/* @funcstatic  GPS_A500_Encode ***************************************
**
** Encode almanac structure to almanac packet
**
** @param [w] s [UC *] string to write to
** @param [r] alm [GPS_PAlmanac] almanac entry
**
** @return [void]
************************************************************************/
static void GPS_A500_Encode(UC *s, GPS_PAlmanac alm)
{
    UC *p;

    p=s;

    GPS_Util_Put_Short(p,alm->wn);
    p+=sizeof(int16);

    GPS_Util_Put_Float(p,alm->toa);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->af0);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->af1);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->e);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->sqrta);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->m0);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->w);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->omg0);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->odot);
    p+=sizeof(float);

    GPS_Util_Put_Float(p,alm->i);

    return;
}


/* @func GPS_A600_Get ******************************************************
**
** Get time from GPS
**
** @param [r] port [const char *] serial port
**
** @return [time_t] GPS time as unix system time, -ve if error
************************************************************************/
time_t GPS_A600_Get(const char *port)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    time_t ret;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Time);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    switch(gps_date_time_type)
    {
    case pD600:
	ret = GPS_D600_Get(rec);
	break;
    default:
	GPS_Error("A600_Get: Unknown data/time protocol");
	return PROTOCOL_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return ret;
}





/* @func GPS_A600_Send **************************************************
**
** Send time to GPS
**
** @param [r] port [const char *] serial port
** @param [r] Time [time_t] unix-style time
**
** @return [int32] success
************************************************************************/
int32 GPS_A600_Send(const char *port, time_t Time)
{
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;
    int32 posnsent=0;
    int32 ret=0;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;

    switch(gps_date_time_type)
    {
    case pD600:
	GPS_D600_Send(&tra,Time);
	break;
    default:
	GPS_Error("A600_Send: Unknown data/time protocol");
	return PROTOCOL_ERROR;
    }

    if(!GPS_Write_Packet(fd,tra))
	return gps_error;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_error;


    /*
     *  Allow GPS a little while to decide whether it wants to ask for
     *  the position. Note that the posn sent is held in gps_save_lat
     *  and gps_save_lon globals!
     */
    if(GPS_Device_Wait(fd))
    {
	if(!GPS_Packet_Read(fd, &rec))
	    return gps_errno;
	
	if(!GPS_Send_Ack(fd, &tra, &rec))
	    return gps_errno;
	
	if(rec->type == LINK_ID[gps_link_type].Pid_Command_Data &&
	   GPS_Util_Get_Short(rec->data) == COMMAND_ID[gps_device_command].
	   Cmnd_Transfer_Posn)
	{
	    GPS_User("INFO: GPS position request. Sending....");
	    ret = GPS_Rqst_Send_Position(fd,gps_save_lat,gps_save_lon);
	    if(ret < 0) return ret;
	    posnsent=1;
	}
    }


    if(!posnsent)
    {
	ret = GPS_Rqst_Send_Position(fd,gps_save_lat,gps_save_lon);
	if(ret < 0) return ret;
    }


    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}





/* @func GPS_D600_Get ******************************************************
**
** Convert date/time packet to ints
**
** @param [r] packet [GPS_PPacket] packet
**
** @return [time_t] gps time as unix system time
************************************************************************/
time_t GPS_D600_Get(GPS_PPacket packet)
{
    UC *p;
    static struct tm ts;

    p = packet->data;

    ts.tm_mon  = *p++ - 1;
    ts.tm_mday = *p++;
    ts.tm_year = (int32) GPS_Util_Get_Short(p) - 1900;
    p+=2;
    ts.tm_hour = (int32) GPS_Util_Get_Short(p);
    p+=2;
    ts.tm_min  = *p++;
    ts.tm_sec  = *p++;

    return mktime(&ts);
}


/* @func GPS_D600_Send ******************************************************
**
** make a time packet for sending to the GPS
**
** @param [w] packet [GPS_PPacket *] packet
** @param [r] Time [time_t] unix-style time
**
** @return [void]
************************************************************************/
void GPS_D600_Send(GPS_PPacket *packet, time_t Time)
{
    UC data[10];
    UC *p;
    struct tm *ts;

    p = data;

    ts = localtime(&Time);
    *p++ = ts->tm_mon+1;
    *p++ = ts->tm_mday;

    GPS_Util_Put_Short(p,(US) (ts->tm_year+1900));
    p+=2;
    GPS_Util_Put_Short(p,(US) ts->tm_hour);
    p+=2;

    *p++ = ts->tm_min;
    *p   = ts->tm_sec;

    GPS_Make_Packet(packet, LINK_ID[gps_link_type].Pid_Date_Time_Data,
		    data,8);

    return;
}




/* @func GPS_A700_Get ******************************************************
**
** Get position from GPS
**
** @param [r] port [const char *] serial port
** @param [w] lat [double *] latitude  (deg)
** @param [w] lon [double *] longitude (deg)
**
** @return [int32] success
************************************************************************/
int32 GPS_A700_Get(const char *port, double *lat, double *lon)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Transfer_Posn);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return gps_errno;

    if(!GPS_Packet_Read(fd, &rec))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &tra, &rec))
	return gps_errno;

    switch(gps_position_type)
    {
    case pD700:
	GPS_D700_Get(rec, lat, lon);
	break;
    default:
	GPS_Error("A700_Get: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @func GPS_A700_Send ******************************************************
**
** Send position to GPS
**
** @param [r] port [const char *] serial port
** @param [r] lat [double] latitude  (deg)
** @param [r] lon [double] longitute (deg)
**
** @return [int32] success
************************************************************************/
int32 GPS_A700_Send(const char *port, double lat, double lon)
{
    gpsdevh *fd;
    GPS_PPacket tra;
    GPS_PPacket rec;

    if(!GPS_Device_On(port, &fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    switch(gps_position_type)
    {
    case pD700:
	GPS_D700_Send(&tra,lat,lon);
	break;
    default:
	GPS_Error("A700_Send: Unknown position protocol");
	return PROTOCOL_ERROR;
    }

    if(!GPS_Write_Packet(fd,tra))
	return 0;
    if(!GPS_Get_Ack(fd, &tra, &rec))
	return 0;


    GPS_Packet_Del(&tra);
    GPS_Packet_Del(&rec);

    if(!GPS_Device_Off(fd))
	return gps_errno;

    return 1;
}



/* @func GPS_D700_Get ******************************************************
**
** Convert position packet to lat/long in degrees
**
** @param [r] packet [GPS_PPacket] packet
** @param [w] lat [double *] latitude  (deg)
** @param [w] lon [double *] longitude (deg)
**
** @return [void]
************************************************************************/
void GPS_D700_Get(GPS_PPacket packet, double *lat, double *lon)
{
    UC *p;
    double t;

    p = packet->data;

    t    = GPS_Util_Get_Double(p);
    *lat = GPS_Math_Rad_To_Deg(t);

    p += sizeof(double);

    t    = GPS_Util_Get_Double(p);
    *lon = GPS_Math_Rad_To_Deg(t);


    return;
}


/* @func GPS_D700_Send ******************************************************
**
** make a position packet for sending to the GPS
**
** @param [w] packet [GPS_PPacket *] packet
** @param [r] lat [double] latitude  (deg)
** @param [r] lon [double] longitude (deg)
**
** @return [void]
************************************************************************/
void GPS_D700_Send(GPS_PPacket *packet, double lat, double lon)
{
    UC data[16];
    UC *p;

    lat = GPS_Math_Deg_To_Rad(lat);
    lon = GPS_Math_Deg_To_Rad(lon);

    p = data;

    GPS_Util_Put_Double(p,lat);
    p+=sizeof(double);
    GPS_Util_Put_Double(p,lon);

    GPS_Make_Packet(packet, LINK_ID[gps_link_type].Pid_Position_Data,
		    data,16);

    return;
}



/* @func GPS_A800_On ******************************************************
**
** Turn on GPS PVT
**
** @param [r] port [const char *] serial port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success
************************************************************************/
int32 GPS_A800_On(const char *port, gpsdevh **fd)
{
    static UC data[2];
    GPS_PPacket tra;
    GPS_PPacket rec;

    if(!GPS_Device_On(port, fd))
	return gps_errno;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Start_Pvt_Data);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(*fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(*fd, &tra, &rec))
    {
	GPS_Error("A800_on: Pvt start data not acknowledged");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&rec);
    GPS_Packet_Del(&tra);

    return 1;
}



/* @func GPS_A800_Off ******************************************************
**
** Turn off GPS PVT
**
** @param [r] port [const char *] port
** @param [w] fd [int32 *] file descriptor
**
** @return [int32] success
************************************************************************/
int32 GPS_A800_Off(const char *port, gpsdevh **fd)
{
    static UC data[2];
    GPS_PPacket tra;
    GPS_PPacket rec;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    GPS_Util_Put_Short(data,
		       COMMAND_ID[gps_device_command].Cmnd_Stop_Pvt_Data);
    GPS_Make_Packet(&tra, LINK_ID[gps_link_type].Pid_Command_Data,
		    data,2);
    if(!GPS_Write_Packet(*fd,tra))
	return gps_errno;
    if(!GPS_Get_Ack(*fd, &tra, &rec))
    {
	GPS_Error("A800_Off: Not acknowledged");
	return FRAMING_ERROR;
    }


    GPS_Packet_Del(&rec);
    GPS_Packet_Del(&tra);

//    if(!GPS_Device_Off(*fd))
//	return gps_errno;

    return 1;
}


/* @func GPS_A800_Get **************************************************
**
** make a position packet for sending to the GPS
**
** @param [r] fd [int32 *] file descriptor
** @param [w] packet [GPS_PPvt_Data *] packet
**
** @return [int32] success
************************************************************************/
int32 GPS_A800_Get(gpsdevh **fd, GPS_PPvt_Data *packet)
{
    GPS_PPacket tra;
    GPS_PPacket rec;

    if(!(tra = GPS_Packet_New()) || !(rec = GPS_Packet_New()))
	return MEMORY_ERROR;


    if(!GPS_Packet_Read(*fd, &rec)) {
	GPS_Packet_Del(&rec);
	GPS_Packet_Del(&tra);
	return gps_errno;
    }

    if(!GPS_Send_Ack(*fd, &tra, &rec)) {
	GPS_Packet_Del(&rec);
	GPS_Packet_Del(&tra);
	return gps_errno;
    }

    if (rec->type != LINK_ID[gps_link_type].Pid_Pvt_Data) {
	GPS_Packet_Del(&rec);
	GPS_Packet_Del(&tra);
	return 0;
    }

    switch(gps_pvt_type)
    {
    case pD800:
	GPS_D800_Get(rec,packet);
	break;
    default:
	GPS_Error("A800_GET: Unknown pvt protocol");
	GPS_Packet_Del(&rec);
	GPS_Packet_Del(&tra);
	return PROTOCOL_ERROR;
    }

    GPS_Packet_Del(&rec);
    GPS_Packet_Del(&tra);

    return 1;
}



/* @func GPS_D800_Get ******************************************************
**
** Convert packet to pvt structure
**
** @param [r] packet [GPS_PPacket] packet
** @param [w] pvt [GPS_PPvt_Data *] pvt structure
**
** @return [void]
************************************************************************/
void GPS_D800_Get(GPS_PPacket packet, GPS_PPvt_Data *pvt)
{
    UC *p;

    p = packet->data;

    (*pvt)->alt = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->epe = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->eph = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->epv = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->fix = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    (*pvt)->tow = GPS_Util_Get_Double(p);
    p+=sizeof(double);

    (*pvt)->lat = GPS_Math_Rad_To_Deg(GPS_Util_Get_Double(p));
    p+=sizeof(double);

    (*pvt)->lon = GPS_Math_Rad_To_Deg(GPS_Util_Get_Double(p));
    p+=sizeof(double);

    (*pvt)->east = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->north = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->up = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->msl_hght = GPS_Util_Get_Float(p);
    p+=sizeof(float);

    (*pvt)->leap_scnds = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

    (*pvt)->wn_days = GPS_Util_Get_Int(p);

    return;
}

/* @func GPS_A906_Get ******************************************************
**
** Get lap data from GPS
**
** @param [r] port [const char *] serial port
** @param [w] trk [GPS_PLap **] lap array
**
** @return [int32] number of lap entries
************************************************************************/

int32 GPS_A906_Get(const char *port, GPS_PLap **lap, pcb_fn cb)
{
    static UC data[2];
    gpsdevh *fd;
    GPS_PPacket trapkt;
    GPS_PPacket recpkt;
    int32 i, n;

    if (gps_lap_transfer == -1)
	return GPS_UNSUPPORTED;

    if (!GPS_Device_On(port, &fd))
	return gps_errno;

    if (!(trapkt = GPS_Packet_New() ) || !(recpkt = GPS_Packet_New()))
       return MEMORY_ERROR;

    GPS_Util_Put_Short(data,
                       COMMAND_ID[gps_device_command].Cmnd_Transfer_Lap);
    GPS_Make_Packet(&trapkt, LINK_ID[gps_link_type].Pid_Command_Data,
                    data,2);
    if(!GPS_Write_Packet(fd,trapkt))
        return gps_errno;
    if(!GPS_Get_Ack(fd, &trapkt, &recpkt))
        return gps_errno;
    if(!GPS_Packet_Read(fd, &recpkt))
        return gps_errno;
    if(!GPS_Send_Ack(fd, &trapkt, &recpkt))
        return gps_errno;

    n = GPS_Util_Get_Short(recpkt->data);

    if(n)
        if(!((*lap)=(GPS_PLap *)malloc(n*sizeof(GPS_PLap))))
        {
            GPS_Error("A906_Get: Insufficient memory");
            return MEMORY_ERROR;
        }

    for(i=0;i<n;++i) {
        if(!((*lap)[i]=GPS_Lap_New()))
            return MEMORY_ERROR;
	if(!GPS_Packet_Read(fd, &recpkt)) {
		return gps_errno;
	}

	if(!GPS_Send_Ack(fd, &trapkt, &recpkt)) {
		return gps_errno;
	}

	switch(gps_lap_type) {
	    case pD906:
		    case pD1001:
		    case pD1011:
		    case pD1015:
			    GPS_D1011b_Get(&((*lap)[i]),recpkt->data);
		break;
	    default:
		GPS_Error("A906_Get: Unknown Lap protocol %d\n", gps_lap_type);
		return PROTOCOL_ERROR;
	}

	/* Cheat and don't _really_ pass the trkpt back */
	cb(n, NULL);
    }

    if(!GPS_Packet_Read(fd, &recpkt))
	return gps_errno;
    if(!GPS_Send_Ack(fd, &trapkt, &recpkt))
	return gps_errno;
    if(recpkt->type != LINK_ID[gps_link_type].Pid_Xfer_Cmplt) {
	GPS_Error("A906_Get: Error transferring laps");
	return FRAMING_ERROR;
    }

    if(i != n) {
	GPS_Error("A906_GET: Lap entry number mismatch");
	return FRAMING_ERROR;
    }

    GPS_Packet_Del(&trapkt);
    GPS_Packet_Del(&recpkt);

    if (!GPS_Device_Off(fd))
	return gps_errno;
    return n;
}

/* @func GPS_D1011b_Get ******************************************************
**
** Convert packet D906, D1001, D1011, D1015 to lap structure
**
** @param [r] packet [GPS_PPacket] packet
** @param [w] pvt [GPS_PLap *] lap structure
**
** @return [void]
************************************************************************/
void GPS_D1011b_Get(GPS_PLap *Lap, UC *p)
{
    uint32 t;

	/* Lap index (not in D906) */
	switch(gps_lap_type) {
		case pD906:
			(*Lap)->index = -1;
			break;
		case pD1001:
			(*Lap)->index = GPS_Util_Get_Uint(p);
			p+=sizeof(uint32);
			break;
		case pD1011:
		case pD1015:
			(*Lap)->index = GPS_Util_Get_Short(p);
			p+=sizeof(uint16);
			p+=sizeof(uint16); /*unused*/
			break;
		default:
			break;
	}

    t = GPS_Util_Get_Uint(p);
    (*Lap)->start_time = GPS_Math_Gtime_To_Utime((time_t)t);
    p+=sizeof(uint32);

    (*Lap)->total_time = GPS_Util_Get_Int(p);
    p+=sizeof(int32);

    (*Lap)->total_distance = GPS_Util_Get_Float(p);
    p+=sizeof(float);
	if(gps_lap_type != pD906){
		(*Lap)->max_speed = GPS_Util_Get_Float(p);
		p+=sizeof(float);
	}

    (*Lap)->begin_lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);
    (*Lap)->begin_lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);
    (*Lap)->end_lat = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);
    (*Lap)->end_lon = GPS_Math_Semi_To_Deg(GPS_Util_Get_Int(p));
    p+=sizeof(int32);

    (*Lap)->calories = GPS_Util_Get_Short(p);
    p+=sizeof(int16);

	/* Track index, only in D906*/
    if(gps_lap_type == pD906){
	(*Lap)->track_index = *p++;
	p++; /*Unused*/
		
	/*Last field, no more to do */
	return;
    } else {
	(*Lap)->track_index = -1;
    }

    (*Lap)->avg_heart_rate = *p++;
    (*Lap)->max_heart_rate = *p++;
    (*Lap)->intensity = *p++;

    switch(gps_lap_type) {
	case pD1001:
			/*No more fields */
	    return;
	case pD1011:
	case pD1015:
	    (*Lap)->avg_cadence = *p++;
	    (*Lap)->trigger_method = *p++;
	    break;
	default:
	    /*pD906 already returned*/
	    break;
    }

    if (gps_lap_type==pD1015) {
	/*some unknown fields like 04 dc 44 ff ff */
	/*		(*Lap)->unk1015_1 = *p++; normally 4?
	(*Lap)->unk1015_2 = GPS_Util_Get_Short(p);wkt related , ffff otherwise
	p+=sizeof(int16);
	(*Lap)->unk1015_3 = GPS_Util_Get_Short(p);ffff ?
	p+=sizeof(int16);
	*/
    }

    return;
}


/*
 *  It's unfortunate that these aren't constant and therefore switchable,
 *  but they really are runtime variable.  Sigh.
 */
const char *
Get_Pkt_Type(unsigned char p, unsigned short d0, const char **xinfo)
{
	*xinfo = NULL;
#define LT LINK_ID[gps_link_type]
	if (p == LT.Pid_Ack_Byte)
		return "ACK";
	if (p == LT.Pid_Command_Data) {
		switch (d0) {
			case 0: *xinfo = "Abort"; break;
			case 1: *xinfo = "Xfer Alm"; break;
			case 2: *xinfo = "Xfer Posn"; break;
			case 3: *xinfo = "Xfer Prx"; break;
			case 4: *xinfo = "Xfer Rte"; break;
			case 5: *xinfo = "Xfer Time"; break;
			case 6: *xinfo = "Xfer Trk"; break;
			case 7: *xinfo = "Xfer Wpt"; break;
			case 8: *xinfo = "Power Down"; break;
			case 49: *xinfo = "Xfer PVT Start"; break;
			case 50: *xinfo = "Xfer PVT Stop"; break;
			case 92: *xinfo = "Flight Records"; break;
			case 117: *xinfo = "Xfer Laps"; break;
			case 121: *xinfo = "Xfer Categories"; break;
			case 450: *xinfo = "Xfer Runs"; break;
			case 451: *xinfo = "Xfer Workouts"; break;
			case 452: *xinfo = "Xfer Wkt Occurrences"; break;
			case 453: *xinfo = "Xfer User Profile "; break;
			case 454: *xinfo = "Xfer Wkt Limits"; break;
			case 561: *xinfo = "Xfer Courses"; break;
			case 562: *xinfo = "Xfer Course Laps"; break;
			case 563: *xinfo = "Xfer Course Point"; break;
			case 564: *xinfo = "Xfer Course Tracks"; break;
			case 565: *xinfo = "Xfer Course Limits"; break;

			default: *xinfo = "Unknown";
		}
		return "CMDDAT";
	}
	if (p == LT.Pid_Protocol_Array)
		return "PRTARR";
	if (p == LT.Pid_Product_Rqst)
		return "PRDREQ";
	if (p == LT.Pid_Product_Data)
		return "PRDDAT";
	if (p == LT.Pid_Ext_Product_Data)
		return "PRDEDA";

	if (p == LT.Pid_Xfer_Cmplt)
		return "XFRCMP";
	if (p == LT.Pid_Date_Time_Data)
		return "DATTIM";
	if (p == LT.Pid_Position_Data)
		return "POS";
	if (p == LT.Pid_Prx_Wpt_Data)
		return "WPT";
	if (p == LT.Pid_Nak_Byte)
		return "NAK";
	if (p == LT.Pid_Records)
		return "RECORD";
	if (p == LT.Pid_Rte_Hdr)
		return "RTEHDR";
	if (p == LT.Pid_Rte_Wpt_Data)
		return "RTEWPT";
	if (p == LT.Pid_Almanac_Data)
		return "RALMAN";
	if (p == LT.Pid_Trk_Data)
		return "TRKDAT";
	if (p == LT.Pid_Wpt_Data)
		return "WPTDAT";
	if (p == LT.Pid_Pvt_Data)
		return "PVTDAT";
	if (p == LT.Pid_Rte_Link_Data)
		return "LNKDAT";
	if (p == LT.Pid_Trk_Hdr)
		return "TRKHDR";

	if (p == LT.Pid_FlightBook_Record)
		return "FLIBOO";
	if (p == LT.Pid_Lap)
		return "LAPDAT";
	if (p == LT.Pid_Wpt_Cat_Data)
		return "WPTCAT";
	if (p == LT.Pid_Run)
		return "RUNDAT";
	if (p == LT.Pid_Workout)
		return "WKTDAT";
	if (p == LT.Pid_Workout_Occurrence)
		return "WKTOCC";
	if (p == LT.Pid_Fitness_User_Profile)
		return "UPROFI";
	if (p == LT.Pid_Workout_Limits)
		return "WKTLIM";
	if (p == LT.Pid_Course)
		return "CRSDAT";
	if (p == LT.Pid_Course_Lap)
		return "CRSLAP";
	if (p == LT.Pid_Course_Point)
		return "CRSPOI";
	if (p == LT.Pid_Course_Trk_Hdr)
		return "CRSTHD";
	if (p == LT.Pid_Course_Trk_Data)
		return "CRSTDA";
	if (p == LT.Pid_Course_Limits)
		return "CRSLIM";
	if (p == LT.Pid_Trk2_Hdr)
		return "TRKHD2";
		
	if (p == GUSB_REQUEST_BULK)
		return "REQBLK";
	if (p == GUSB_SESSION_START)
		return "SESREQ";
	if (p == GUSB_SESSION_ACK)
		return "SESACK";
		
	return "UNKNOWN";
}
