/*
    holux.h
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

 */

 /* header file for the holux gm-100 wpo format */
   
#ifndef BYTE
#define BYTE unsigned char
#endif

#ifndef WORD
#define WORD unsigned short
#endif

#ifndef DWORD
#define DWORD unsigned int
#endif


/* #define GM100_WPO_FILE_SIZE 25512 */      /* size of a holux gm-100 wpo file used by mapShow 1.4*/ 
#define GM100_WPO_FILE_SIZE 25600       /* size of a holux gm-100 wpo file used by the GM-100*/ 

#define ROUTESTART	    23600           /* Offset for start of route */
#define MAXWPT		    500             /* max number of waypoint */
#define MAXRTE		    20              /* max number of routes */
#define MAXWPTINRTE 	30

#define WPT_HDR_ID		0x5C38A631      /* waypoint header */
#define RTE_HDR_ID		0xD87F59F0      /* route header */


 /* Offsets */
#define OFFS_WPT        0x05E4          /* offet for waypoint table */                


typedef  struct tagWPTHDR
{
	DWORD	    id;			            /* WPT_HDR_ID */
	short		num;			        /* Current wpt number */
	short		next;			        /* next wpt number */
	short		idx[MAXWPT];	        /* saving wpt index here for each wpt, default was -1*/
	BYTE		used[MAXWPT];	        /* Have the match wpt been used (0xFF), Default was 0 */
}WPTHDR;




typedef  struct tagPOINT
{
    signed int  iLongitude;
    signed int  iLatitude;
}POINT;




typedef  struct tagDATE
{
    BYTE    day;
    BYTE    month;
    short   year;
}HX_DATE;


typedef struct tagWPT
{
	char name[8];				        /* wpt name  */
	char comment[12];			        /* comment string */	
	POINT	 pt;				        /* waypoint location  */
	short    vocidx;				    /* voice index, not used */
	short    usecount;			        /* counter: times used by routes */
	HX_DATE     date;			        /* date */
	unsigned time;			            /* time	 */
	char     checked;				    /* Active or not */
    BYTE     dummy[3];                  /* fill bytes */
}WPT;



typedef  struct tagRTEHDR
{
	DWORD	        id;			             /* RTE_HDR_ID */
	short		    num;			         /* Current route number */
	short		    next;			         /* next route number */
	signed short	idx[MAXRTE];	         /* saving route index here for each route, default was -1  */
	BYTE		    used[MAXRTE];	         /* Have the wpt been used (0xFF), Default was 0 */
	signed short    rteno;			         /* Saving navigationroute number here */
}RTEHDR;


typedef  struct tagRTE
{
	char name[8];				         /* route name */
	char comment[12];			         /* comment string */	
	short wptnum;				         /* the total waypoint number */
	short wptidx[MAXWPTINRTE];	         /* the waypoint index in this route */
	short reserved;			
	int date;				             /* date */
	int time;				             /* time	 */
}RTE;

