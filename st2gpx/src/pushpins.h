/*
	pushpins.h

	Extract data from MS Streets & Trips .est and Autoroute .axe files in GPX format.

    Copyright (C) 2003 James Sherring, james_sherring@yahoo.com

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

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_PUSHPINS 5000

// max size to be read from ppin memo fields
#define MAX_PPIN_MEMO	1000

typedef struct pushpin
{
	long UdId;
	int SetId;
	long Grid;
	long Precision;
//	dword RenderData;
//	byte MatchId;
//	long MOBBId;
//	long SourceUdId;
//	bool IsTerritory;
//
	char* UdName;	// max 128
//	bool NoNameSearch;
//	byte NoteTypeId;
	char* NoteShort; // max 255
//	char* NoteLong; // memo
//	char* GeocodeShort; // max 255
//	char* GeocodeLong; // memo
//	byte GeocodeHierarchy; 
//	byte GeocodeContext; 
//	OLE Object EntityArray ????
//
// shouldn't include these two here because they are not part of the native pushpin definition
	double lat;
	double lon;
} tag_pushpin;


typedef struct pushpinset
{
	int SetId;
	char* SetName; // max 128
/*	byte RenderMethod;
	byte GeocodeMethod;
	byte CreateMethod;
	byte GeometryType;
	char[128] RenderData;
	long UdCount;
	long MatchedCount;
	long SkippedCount;
	long UnmatchedCount;
	long CounterUpdateMask;
	int ParentSetId;
	bool IsRendered;
	long Z_Order;
	long GeocodeCtxt;
	OLE Object ThemeRenderData
	date/time LastGeocoded
	char* DataSrcDescr; // memo
	char[30] CurrencyData;
*/
} tag_pushpinset;

//struct pushpin * ppin_list[MAX_PUSHPINS];
struct pushpin * ppin_list[5000];

//EXTERN_C
 void read_ppin_list(char* ppin_file_name);


#ifdef __cplusplus
} /* closing brace for extern "C" */
#endif
