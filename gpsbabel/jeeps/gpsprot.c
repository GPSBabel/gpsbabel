/********************************************************************
** @source JEEPS protocol table lookup functions (GPS' without A001)
**
** @author Copyright (C) 1999 Alan Bleasby
** @version 1.0 
** @modified Dec 28 1999 Alan Bleasby. First version
** @modified Copyright (C) 2006 Robert Lipe
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

#define GPS_TAGUNK  20

/* Storage for any unknown tags */
static int32 gps_tag_unknown[GPS_TAGUNK];
static int32 gps_tag_data_unknown[GPS_TAGUNK];
static int32 gps_n_tag_unknown = 0;



struct COMMANDDATA COMMAND_ID[2]=
{
    {
	0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x31,0x32,92,117,121,450,451,452,453,454,561,562,563,564,564
    }
    ,
    {
	0x00,0x04,0x00,0x00,0x08,0x14,0x00,0x15,0x1a,0x00,0x00
    }
};

struct LINKDATA LINK_ID[3]=
{
    {
	0xfd,0xfe,0xff,248,
	0x06,0,0,0,0,0,0x15,0,0,0,0,
	0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
	0xfd,0xfe,0xff,248,
	0x06,0x0a,0x0c,0x0e,0x11,0x13,0x15,0x1b,0x1d,0x1e,0x1f,
	0x22,0x23,0x33,0x62,0x63,
	134,149,152,990,991,992,993,994,1061,1062,1063,1064,1065,1066,222
    }
    ,
    {
	0xfd,0xfe,0xff,248,
	0x06,0x0b,0x0c,0x14,0x18,0,0x15,0x23,0x25,0x27,0x04,
	0,0x2b,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    }
};

struct GPS_MODEL_PROTOCOL GPS_MP[]=
{
    {   7,pL001,pA010,pA100,pD100,pA200,pD200,-1,-1,-1,-1,-1,
	   pA500,pD500
    },
    {  13,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  14,pL001,pA010,pA100,pD100,pA200,pD200,pD100,-1,-1,pA400,pD400,
	   pA500,pD500
    },
    {  15,pL001,pA010,pA100,pD151,pA200,pD200,pD151,-1,-1,pA400,pD151,
	   pA500,pD500
    },
    {  18,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  20,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD550
    },
    {  22,pL001,pA010,pA100,pD152,pA200,pD200,pD152,pA300,pD300,pA400,pD152,
	   pA500,pD500
    },
    {  23,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  24,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  25,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  29,pL001,pA010,pA100,pD101,pA200,pD201,pD101,pA300,pD300,pA400,pD101,
	   pA500,pD500
    },
    {  929,pL001,pA010,pA100,pD102,pA200,pD201,pD102,pA300,pD300,pA400,pD102,
	   pA500,pD500
    },
    {  31,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  33,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD550
    },
    {  34,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD550
    },
    {  35,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  36,pL001,pA010,pA100,pD152,pA200,pD200,pD152,pA300,pD300,pA400,pD152,
	   pA500,pD500
    },
    {  936,pL001,pA010,pA100,pD152,pA200,pD200,pD152,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  39,pL001,pA010,pA100,pD151,pA200,pD201,pD151,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  41,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  42,pL001,pA010,pA100,pD100,pA200,pD200,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD500
    },
    {  44,pL001,pA010,pA100,pD101,pA200,pD201,pD101,pA300,pD300,pA400,pD101,
	   pA500,pD500
    },
    {  45,pL001,pA010,pA100,pD152,pA200,pD201,pD152,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  47,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  48,pL001,pA010,pA100,pD154,pA200,pD201,pD154,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  49,pL001,pA010,pA100,pD102,pA200,pD201,pD102,pA300,pD300,pA400,pD102,
	   pA500,pD501
    },
    {  50,pL001,pA010,pA100,pD152,pA200,pD201,pD152,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  52,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD550
    },
    {  53,pL001,pA010,pA100,pD152,pA200,pD201,pD152,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  55,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  56,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  59,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  61,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  62,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  64,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD551
    },
    {  71,pL001,pA010,pA100,pD155,pA200,pD201,pD155,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  72,pL001,pA010,pA100,pD104,pA200,pD201,pD104,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  73,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  74,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,-1,-1,
	   pA500,pD500
    },
    {  76,pL001,pA010,pA100,pD102,pA200,pD201,pD102,pA300,pD300,pA400,pD102,
	   pA500,pD501
    },
    {  77,pL001,pA010,pA100,pD100,pA200,pD201,pD100,pA300,pD300,pA400,pD400,
	   pA500,pD501
    },
    {  777,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  877,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  977,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  87,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  88,pL001,pA010,pA100,pD102,pA200,pD201,pD102,pA300,pD300,pA400,pD102,
	   pA500,pD501
    },
    {  95,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  96,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  97,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  98,pL002,pA011,pA100,pD150,pA200,pD201,pD150,-1,-1,pA400,pD450,
	   pA500,pD551
    },
    {  100,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  105,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  106,pL001,pA010,pA100,pD103,pA200,pD201,pD103,pA300,pD300,pA400,pD403,
	   pA500,pD501
    },
    {  112,pL001,pA010,pA100,pD152,pA200,pD201,pD152,pA300,pD300,-1,-1,
	   pA500,pD501
    },
    {  0,0,0,0,0,0,0,0,0,0,0,0,0,0
    }
};


/* @func GPS_Protocol_Version_Change ************************************
**
** Alters/recalculates ID, if necessary, for indexing the GPS_MP
** structure in order to find available protocols.
**
** @param [r] id [US] Garmin id
** @param [r] version [UD] Garmin version
**
** @return [void]
************************************************************************/
    
US GPS_Protocol_Version_Change(US id, US version)
{
    if(id==29)
	if(version>=400)
	    id = 929;

    if(id==36)
	if(version>=300)
	    id = 936;

    if(id==77)
    {
	if(version>=301 && version<350)
	    id = 777;
	else if(version>=350 && version<361)
	    id = 877;
	else if(version>=361)
	    id = 977;
    }

    return id;
}



/* @func GPS_Protocol_Table_Set ************************************
**
** Set protocol capabilities based on table look-up
** For those units without the A001 protocol
**
** @param [r] id [const US] id
**
** @return [int32] Success
************************************************************************/

int32 GPS_Protocol_Table_Set(US id)
{
    int32 i;
    US  v;
    char s[GPS_ARB_LEN];

    i=0;
    while((v=GPS_MP[i].id))
    {
	if(v==id)
	{
	    gps_link_type          = GPS_MP[i].link;
	    gps_device_command     = GPS_MP[i].command-10;
	    gps_waypt_transfer     = GPS_MP[i].wayptt;
	    gps_waypt_type         = GPS_MP[i].wayptd;
	    gps_route_transfer     = GPS_MP[i].rtea;
	    gps_rte_hdr_type       = GPS_MP[i].rted0;
	    gps_rte_type           = GPS_MP[i].rted1;
	    gps_trk_transfer       = GPS_MP[i].trka;
	    gps_trk_type           = GPS_MP[i].trkd;
	    gps_prx_waypt_transfer = GPS_MP[i].prxa;
	    gps_prx_waypt_type     = GPS_MP[i].prxd;
	    gps_almanac_transfer   = GPS_MP[i].alma;
	    gps_almanac_type       = GPS_MP[i].almd;
	    return 1;
	}
	++i;
    }


    (void)sprintf(s,"INIT: No table entry for ID %d\n",id);
    GPS_Error(s);

    return GPS_UNSUPPORTED;
}


/* @func GPS_Protocol_Error *******************************************
**
** Called if an unrecognised/illegal protocol is met
** For those units with the A001 protocol
**
** @param [r] tag [const US] tag
** @param [r] data [const US] data
**
** @return [void]
************************************************************************/
	    
void GPS_Protocol_Error(US tag, US data)
{
    char s[GPS_ARB_LEN];

    (void) sprintf(s,"PROTOCOL ERROR: Unknown tag/data [%c/%d]\n",tag,data);
    GPS_Error(s);

    if(gps_n_tag_unknown < GPS_TAGUNK)
    {
	gps_tag_unknown[gps_n_tag_unknown] = tag;
        gps_tag_data_unknown[gps_n_tag_unknown++] = data;
    }
    
    return;
}



/* @func GPS_Unknown_Protocol_Print *******************************************
**
** Diagnostic routine for printing out any unknown protocols
** For those units with the A001 protocol
**
** @return [void]
************************************************************************/
	    
void GPS_Unknown_Protocol_Print(void)
{
    int32 i;

    (void) fprintf(stdout,"\nUnknown protocols: ");
    if(!gps_n_tag_unknown)
	(void) fprintf(stdout,"None");
    (void) fprintf(stdout,"\n");

    for(i=0; i<gps_n_tag_unknown; ++i)
	(void) fprintf(stdout,"[%c %d]\n",(char)gps_tag_unknown[i],
		       (int)gps_tag_data_unknown[i]);
    return;
}
