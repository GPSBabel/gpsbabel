/*
    Access to holux wpo files.

    Copyright (C) 2002 Jochen Becker, jb@bepo.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111 USA


History:
    2002-09-15      J. Becker       start programming   


*/
/* This module is for the holux (gm-100) .wpo format */



#include <ctype.h>
#include "defs.h"
#include "holux.h"


static FILE *file_in;
static 	unsigned char *HxWFile;
static  char fOutname[256];


static void rd_init(const char *fname)
{
	file_in = fopen(fname, "r");
	if (file_in == NULL) {
		fatal("GPSBABEL: Cannot open %s for reading\n", fname);
	}
}


static void rd_deinit(void)
{
	fclose(file_in);
}



static void
wr_init(const char *fname)
{
    FILE *file_in;
	
    file_in = fopen(EMPTY_WPO, "rb");
	if (file_in == NULL) {
		fatal("GPSBABEL: Cannot open %s for reading\n", EMPTY_WPO);
    }


	HxWFile = calloc(GM100_WPO_FILE_SIZE, 1);
	if (HxWFile == NULL) 
    {
		fatal("GPSBABEL: Cannot alloc memory\n");
    }

    /* read the empty wpo file to the data-array */
    fread( HxWFile, 1, GM100_WPO_FILE_SIZE, file_in );
	fclose(file_in);
    strcpy (fOutname,fname);
    
}


/* write the collected data to the output file */
static void wr_deinit(void)
{   
    /* this function is never called */

}




static void data_read(void)
{
	char name[9], desc[90];
	double lat,lon;
	unsigned char *HxWpt;
	waypoint *wpt_tmp;
    int iCount;
    int iDataRead;
    int iWptNum;
    int iWptIndex;
    WPT *pWptHxTmp;
    int iWptLen;
    DWORD dwIndex;

	HxWpt = calloc(GM100_WPO_FILE_SIZE, 1);
	if (HxWpt == NULL) 
    {
		fatal("GPSBABEL: Cannot alloc memory\n");
    }

    /* read the wpo file to the data-array */
    iDataRead = fread( HxWpt, 1, GM100_WPO_FILE_SIZE, file_in );

    if (iDataRead == 0)
    {
		fatal("GPSBABEL: Error reading data from .wpo file\n");
    }

    iWptLen = sizeof(WPT);
    iWptNum = ((WPTHDR *)HxWpt)->num;

    /* Get the waypoints */
    for (iCount = 0; iCount < iWptNum ; iCount ++)
    {
        wpt_tmp = calloc(sizeof(*wpt_tmp), 1);
    
        iWptIndex = ((WPTHDR *)HxWpt)->idx[iCount];         /* get the waypoint index  */
        dwIndex= OFFS_WPT + (sizeof(WPT) * iWptIndex);
        pWptHxTmp =  (WPT *)&HxWpt[OFFS_WPT + (sizeof(WPT) * iWptIndex)];
        
        wpt_tmp->position.altitude.altitude_meters = 0;
        strncpy(name,pWptHxTmp->name,sizeof(pWptHxTmp->name));
        name[sizeof(pWptHxTmp->name)]=0;
        
        strncpy(desc,pWptHxTmp->comment,sizeof(pWptHxTmp->comment));
        desc[sizeof(pWptHxTmp->comment)]=0;
        
		wpt_tmp->shortname = strdup(name);
		wpt_tmp->description = strdup(desc);

        lon = (double)pWptHxTmp->pt.iLongitude / 36000; 
        lat = ((double)pWptHxTmp->pt.iLatitude  / 36000) * -1;
		wpt_tmp->position.longitude.degrees = lon;
		wpt_tmp->position.latitude.degrees = lat;
		waypt_add(wpt_tmp);
	}
}



static void gpsutil_disp(waypoint *wpt)
{
	double lon,lat;
    int iIndex;
    WPT *pWptHxTmp;

	lon =wpt->position.longitude.degrees * 36000;
	lat =wpt->position.latitude.degrees * -36000;

    iIndex =  ((WPTHDR *)HxWFile)->num;
    ((WPTHDR *)HxWFile)->idx[iIndex] = iIndex;         /* set the waypoint index  */
 
    /* set Waypoint */
    pWptHxTmp =  (WPT *)&HxWFile[OFFS_WPT + (sizeof(WPT) * iIndex)];

    memset (&(pWptHxTmp->name),0x20,sizeof(pWptHxTmp->name));  
    if (wpt->shortname != NULL)
        strncpy((char *)&(pWptHxTmp->name), wpt->shortname, sizeof(pWptHxTmp->name));
    else
        sprintf((char *)&(pWptHxTmp->name),"W%d",iIndex);

    memset (&(pWptHxTmp->comment),0x20,sizeof(pWptHxTmp->comment));  
    if (wpt->description != NULL)
        strncpy((char *)&(pWptHxTmp->comment), wpt->description, sizeof(pWptHxTmp->comment));
     

    pWptHxTmp->pt.iLatitude = (int)lat;
    pWptHxTmp->pt.iLongitude = (int)lon;
    pWptHxTmp->checked = 01;

    ((WPTHDR *)HxWFile)->num = ++iIndex;
    ((WPTHDR *)HxWFile)->next= iIndex;



}


static void data_write(void)
{
    int iWritten;
    FILE *file_out;

    waypt_disp_all(gpsutil_disp);
   

	file_out = fopen(fOutname, "wb");
	if (file_out == NULL) {
		fatal("GPSUTIL: Cannot open %s for writing\n", fOutname);
	}
  
    iWritten = fwrite (HxWFile, 1, GM100_WPO_FILE_SIZE,file_out);  
    if (iWritten == 0)
    {
		fatal("GPSBABEL: Error writing .%s\n", fOutname);
    }

	fclose(file_out);
    free(HxWFile);
}


ff_vecs_t holux_vecs = {
	rd_init,
	wr_init,
	rd_deinit,
	wr_deinit,
	data_read,
	data_write,
};
