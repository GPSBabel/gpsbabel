/*
	properties.h

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


typedef unsigned long DWORD;

typedef struct dictionary 
{
    DWORD    cEntries;        // Count of entries in the list.
	// array of propids
    DWORD *  ent_propid;  // Property ID
	// This next is an array of pointers into properties value,
	// with the string length preceeding (ie X->ent_cb == X->ent_sz -4)
    char **  ent_sz;  // Zero-terminated string. Code page as indicated. 
} tag_dictionary;

typedef struct ole_property
{
	DWORD	propid;
    DWORD   dwType;      // type tag
	int		buflen;
	char*	buf;
} tag_ole_property;

typedef struct ole_property_set
{
//	FMTID  fmtid ;       // semantic name of a section
	char   fmtid[16];       // semantic name of a section
	char   clsid[16];		// clsid for the creating program
	int    OSKind;			// The OS that created the prop-set
	int    OSVer;	
	unsigned int	   cProps;
	struct dictionary * dict;
	struct ole_property * pPropList;
} tag_ole_property_set;

struct ole_property_set * read_ole_properties(char* source_file_name, char* properties_file_name);
void ole_property_set_delete(struct ole_property_set * props);
struct ole_property * get_propterty(struct ole_property_set * props, DWORD propid);



#ifdef	__cplusplus
}
#endif
