/*
	ppinutil.h

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


	This app depends on istorage & istorage-make from Pabs (pabs3@zip.to)
	and James Clark's Expat xml parser from http://www.libexpat.org/.

*/
#ifdef	__cplusplus
extern "C" {
#endif

typedef struct pushpin_safelist
{
	struct pushpin ** pushpin_list;
	int num_pushpins;
	char * UDM_Data[3];
	long UDM_Data_length[3];
} tag_pushpin_safelist;

typedef struct grid_point {
	long grid;
	long precision;
} tag_ms_point;

typedef struct point {
	double lon;
	double lat;
} tag_point;

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
// shouldn't include these 3 here because they are not part of the native pushpin definition
	double lat;
	double lon;
	// 1 = by hand
	// 2 = from file?
	// 4 = not matched?
	short int MatchId;
	long MOBBId;
	char garmin_ident[7];
} tag_pushpin;


// not used yet

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

struct pushpin * pushpin_new();
void pushpin_delete(struct pushpin * pp);
struct pushpin_safelist * pushpin_safelist_new();
void pushpin_safelist_delete(struct pushpin_safelist * ppl);

struct pushpin * ppin_by_UdId(int UdId, struct pushpin_safelist * ppplist);
struct pushpin_safelist * process_pushpin_file(char* ppin_in_file_name);

//struct point ms2latlong(struct ms_point msp);
struct point grid2latlon(long grid, long precision);
struct grid_point latlon2grid(double lat, double lon);

#ifdef	__cplusplus
}
#endif
