/*
	properties.c

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

// see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dnolegen/html/msdn_propset.asp
// http://www.dwam.net/docs/oleref/storage_9.htm


#include <windows.h>
#include <stdio.h>
#include <time.h>

#include "st2gpx.h"
#include "properties.h"

#define OLE_PROP_STREAM ".Olhud5yvVwudb10uAaq5ezn4Ac"

// some types from MS, as published at
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dnolegen/html/msdn_propset.asp

typedef struct PROPERTYSETHEADER
{
     // Header
     WORD    wByteOrder;      // Always 0xFFFE
     WORD    wFormat;        // Always 0
     WORD    dwOSVer;        // System version
	 // I needed to add this to get code to work with 1-byte structs alignment;
	 // It is normally effectively added by VC++ default compiler setting
	 WORD	alignment_fill;
     CLSID   clsid;          // Application CLSID
     DWORD   dwReserved;     // Should be 1
} PROPERTYSETHEADER;

typedef struct FORMATIDOFFSET
{
   FMTID     fmtid ;       // semantic name of a section
   DWORD     dwOffset ;    // offset from start of whole property set 
                           // stream to the section
} FORMATIDOFFSET;

typedef struct PROPERTYIDOFFSET 
{
    DWORD        propid;     // name of a property
    DWORD        dwOffset;   // offset from the start of the section to that 
                             // property type/value pair
} PROPERTYIDOFFSET;

typedef struct tagPROPERTYSECTIONHEADER
{
    DWORD              cbSection ;         // Size of section
    DWORD              cProperties ;      // Count of properties in section
    PROPERTYIDOFFSET   rgPropIDOffset[];    // Array of property locations
} PROPERTYSECTIONHEADER;

typedef struct SERIALIZEDPROPERTYVALUE 
{
    DWORD        dwType;      // type tag
    BYTE        rgb[];        // the actual property value
} SERIALIZEDPROPERTYVALUE;

/*
typedef struct tagENTRY {
    DWORD    propid;  // Property ID
    DWORD    cb;      // Count of bytes in the string, including the null 
                      // at the end.
    char     sz[cb];  // Zero-terminated string. Code page as indicated 
                      // by property ID one.
    } ENTRY;

typedef struct tagDICTIONARY {
    DWORD    cEntries;        // Count of entries in the list.
    ENTRY    rgEntry[cEntries]; 
    } DICTIONARY;
*/

// my defs

char * fmtid2str(char * str, char * fmtid)
// normal fmtids are a 16B struct, I use a 16B char buf
// fmtid should be 16B long
// str should be 39B long (incl null)
// We need to be careful here to get the byte order correct.
// This seems to match MS's interpretation.
{
	sprintf(str, "{%8.8x-%4.4x-%4.4x-%2.2x%2.2x-%2.2x%2.2x%2.2x%2.2x%2.2x%2.2x}", 
					*(unsigned long*)(fmtid), 
					*(unsigned short*)(fmtid+4), 
					*(unsigned short*)(fmtid+6),
					*(unsigned char*)(fmtid+8),
					*(unsigned char*)(fmtid+9),
					*(unsigned char*)(fmtid+10),
					*(unsigned char*)(fmtid+11),
					*(unsigned char*)(fmtid+12),
					*(unsigned char*)(fmtid+13),
					*(unsigned char*)(fmtid+14),
					*(unsigned char*)(fmtid+15)	);
	str[39]=0;
	return str;
}

struct ole_property_set * ole_property_set_new(int cProps)
{
	struct ole_property_set * props 
		=(struct ole_property_set*)xmalloc(sizeof(struct ole_property_set)); 
	props->cProps = cProps;
	props->dict=NULL; 
	props->pPropList = (struct ole_property*)xmalloc(cProps*sizeof(struct ole_property)); 
	return props;
}

void ole_property_set_delete(struct ole_property_set * props)
{
	unsigned int i;

	if ( (props==NULL) || (props->pPropList == NULL) )
		return;

	for(i=0; i<props->cProps; i++)
	{
		xfree(props->pPropList[i].buf);
		// FIXME who owns this memory? Cant free it if it is a pointer into another buff
		//xfree(props->pPropList[i]);
	}

	if (props->dict != NULL)
	{
		xfree(props->dict->ent_propid);
		xfree(props->dict->ent_sz);
		xfree(props->dict);
	}
	xfree(props->pPropList);
	xfree(props);
}


struct dictionary * read_dictionary(int ents, char* buf, int bufsize)
{
	int ent_read=0;
	int bytes_read=0;
	int this_ent_cb=0;
	struct dictionary * dict = (struct dictionary *)xmalloc(sizeof(struct dictionary));
	dict->cEntries = ents;
	dict->ent_propid=(DWORD*)xmalloc(ents*sizeof(DWORD));
	dict->ent_sz=(char**)xmalloc(ents*sizeof(char*));

	printf("reading %d entries from dictionary\n", ents);

	while(ent_read<ents)
	{
		if (bytes_read>bufsize)
		{
			printf("read past end of buffer while reading properties dictionary\n");
			xfree(dict->ent_propid);
			xfree(dict->ent_sz);
			xfree(dict);
			return NULL;
		}

		dict->ent_propid[ent_read]=*(DWORD*)(buf+bytes_read);
		bytes_read += 4;

		//dict->ent_cb[ent_read]=
		this_ent_cb = *(DWORD*)(buf+bytes_read);
//		dict->ent_cb[ent_read]=(DWORD*)(buf+bytes_read);
		bytes_read += 4;

		if(this_ent_cb > 128)
		{
			printf("reading dictionary, read entry for propid %#x with name length %d > max allowed length of 128\n",
				dict->ent_propid[ent_read], this_ent_cb);
//			xfree(dict->ent_cb);
			xfree(dict->ent_propid);
			xfree(dict->ent_sz);
			xfree(dict);
			return NULL;
		}

		// Dont allocate memory for dictionary strings,
		dict->ent_sz[ent_read]=buf+bytes_read;

		// MSDN documentation says cb is bytes, but it is wchars.
		bytes_read += 2*this_ent_cb;

		ent_read++;
	}
	return dict;
}

unsigned int get_dict_entry(struct dictionary* dict, DWORD propid)
// Find a dictionary entry for a given propid.
// This is then used to get the name for a propid, if available.
{
	unsigned int i;
	if (dict==NULL)
		return -1;

	for(i=0; i<dict->cEntries; i++)
		if (dict->ent_propid[i] == propid)
			return i;

	return 1;
}

struct ole_property * get_propterty(struct ole_property_set * props, DWORD propid)
{
	unsigned int i;
	
	if (props==NULL)
		return NULL;

	for(i=0; i< props->cProps; i++)
		if( props->pPropList[i].propid == propid)
			return props->pPropList+i;

	return NULL;
}

void print_ole_properties(struct ole_property_set * props)
{
	unsigned int i;
	unsigned int j;
	char* name;
	int dict_ent;
	char fmtidstr[39];
	char clsidstr[39];

	if(props==NULL)
		return;

	fmtid2str(fmtidstr, props->fmtid);
	fmtid2str(clsidstr, props->clsid);
	printf("Printing OLE Property Set, with format id %s,\n", fmtidstr);
	printf("clsid %s, os %#x osver %#x\n", clsidstr, props->OSKind, props->OSVer);

	for (i=0; i< (props->cProps); i++)
	{
		name = NULL;
		dict_ent = get_dict_entry(props->dict, props->pPropList[i].propid);
		if (dict_ent != -1)
			name = props->dict->ent_sz[dict_ent];

		wprintf(L"\nproperty %d: has name %s propid %#x, type %#x, and length %d bytes\n", 
				i, name, props->pPropList[i].propid, 
				props->pPropList[i].dwType, props->pPropList[i].buflen );

		switch(props->pPropList[i].propid)
		{
		case 0:
			printf("This is the property-set dictionary, with entries:\n");
			printf("PropId    Property Name\n");
			printf("-----------------------\n");
			for (j=0; j<props->dict->cEntries; j++)
				wprintf(L"%#x   %s\n", props->dict->ent_propid[j], (props->dict->ent_sz[j]));
			break;
		case 1:
			printf("This specifies codepage %d (1200=Unicode, 1252=Ansi).\n", 
					*(USHORT*)(props->pPropList[i].buf) );
			break;
		case 0x80000000:
			printf("This specifies locale %d .\n", 
					*(unsigned short*)(props->pPropList[i].buf) );
			break;
		default:
			switch(props->pPropList[i].dwType)
			{
			case VT_BSTR: //=8
			//  the prefix bytecount is still there 
				wprintf(L"This has value '%ls'\n", props->pPropList[i].buf+4);
				break;
			case VT_UNKNOWN: //=13
				break;
			case VT_BOOL: // =11
				if (*(short*)(props->pPropList[i].buf))
					printf("This has value 'True'\n");
				else
					printf("This has value 'False'\n");
				break;
			case VT_I4: // = 3
				printf("This has value %d\n", *(int *)(props->pPropList[i].buf) );
				break;
			case VT_LPWSTR: //=31
				wprintf(L"This has value '%ls'\n", (props->pPropList[i].buf+4));
				break;
			case VT_FILETIME: //=31
				printf("This has value '%s'\n", ctime((const time_t *)props->pPropList[i].buf+4));
				break;
			default:
				printf("I dont understand this type yet\n");
				printbuf(props->pPropList[i].buf, props->pPropList[i].buflen);
				break;
			}
		}
	}
}

struct ole_property_set * read_ole_properties2(char* prop_file_name)
{
	FILE* prop_file=NULL;
	PROPERTYSETHEADER * prop_header = (PROPERTYSETHEADER *)xmalloc(sizeof(PROPERTYSETHEADER));
	FORMATIDOFFSET * fmt_id_os = (FORMATIDOFFSET *)xmalloc(sizeof(FORMATIDOFFSET));
	PROPERTYSECTIONHEADER * psect_header = (PROPERTYSECTIONHEADER *)xmalloc(sizeof(PROPERTYSECTIONHEADER));
	char* section_buff=NULL;
	struct ole_property_set * property_set;

	unsigned int i=0;
	unsigned int j=0;
	unsigned int next_start=0;
	int status=0;

	if ((prop_file = fopen(prop_file_name, "rb")) == NULL)
	{
		fprintf(stderr, "Cannot open property file %s\n", prop_file_name);
		return NULL;
	}

	status=readbytes(prop_file, (char*)prop_header, sizeof(PROPERTYSETHEADER));
	status=readbytes(prop_file, (char*)fmt_id_os, sizeof(FORMATIDOFFSET));

	if ( (prop_header->wByteOrder != 0xFFFE) || (prop_header->wFormat != 0) 
		 || (prop_header->dwReserved != 1)  || (fmt_id_os->dwOffset !=0x30) )
	{
		printf("Not a valid properties set header in file %s\n", prop_file_name);
		fclose(prop_file);
		return NULL;
	}

	status=readbytes(prop_file, (char*)psect_header, sizeof(PROPERTYSECTIONHEADER));

	psect_header = (PROPERTYSECTIONHEADER *)xrealloc(psect_header, psect_header->cbSection);
	section_buff = (char*)psect_header;

	status=readbytes(prop_file, (char*)(psect_header->rgPropIDOffset), 
						psect_header->cbSection - sizeof(PROPERTYSECTIONHEADER));

	property_set = ole_property_set_new(psect_header->cProperties);
	memcpy(property_set->fmtid, &(fmt_id_os->fmtid), 16);
	memcpy(property_set->clsid, &(prop_header->clsid), 16);
	property_set->OSKind = *((short int*)( &(prop_header->dwOSVer)   ) +1);
	property_set->OSVer =  *((short int*)( &(prop_header->dwOSVer)   ));


	// We cant rely on pointer order matching data order,
	// so we need to do a little extra work to find the start of the next data
	for (i=0; i< psect_header->cProperties ; i++)
	{
		next_start=psect_header->cbSection; //ie end of the buffer
		for(j=0; j<psect_header->cProperties; j++)
			if(  ( psect_header->rgPropIDOffset[i].dwOffset < psect_header->rgPropIDOffset[j].dwOffset )
				 && ( psect_header->rgPropIDOffset[j].dwOffset < next_start) )
				next_start = psect_header->rgPropIDOffset[j].dwOffset;

		property_set->pPropList[i].buflen
			= next_start
			- psect_header->rgPropIDOffset[i].dwOffset
			-4;
	}


	for (i=0; i< psect_header->cProperties; i++)
	{
		property_set->pPropList[i].propid = psect_header->rgPropIDOffset[i].propid;
		
//		property_set->pPropList[i].buf 
//			= section_buff + psect_header->rgPropIDOffset[i].dwOffset  + 4;

		property_set->pPropList[i].buf = (char*)xmalloc(property_set->pPropList[i].buflen);
		memcpy(property_set->pPropList[i].buf,
				section_buff + psect_header->rgPropIDOffset[i].dwOffset  + 4,
				property_set->pPropList[i].buflen);

		property_set->pPropList[i].dwType =  *((DWORD*)(section_buff + psect_header->rgPropIDOffset[i].dwOffset));
	}

	for (i=0; i< property_set->cProps; i++)
		if (property_set->pPropList[i].propid==0)
			property_set->dict=read_dictionary(property_set->pPropList[i].dwType, 
								property_set->pPropList[i].buf, 
								property_set->pPropList[i].buflen);

	fclose(prop_file);
	xfree(prop_header);
	xfree(psect_header);
	xfree(fmt_id_os);

	return property_set;	
}

struct ole_property_set * read_ole_properties(char* source_file_name, char* properties_file_name)
{
	char * prop_file_name;
	struct ole_property_set * strips_property_set=NULL;
	struct ole_property_set * summary_property_set=NULL;
	
	if ( (source_file_name==NULL) && (properties_file_name==NULL) )
		return NULL;

	if (source_file_name!=NULL)
		prop_file_name = (char*)xmalloc(strlen(source_file_name)+40);
	else 
		prop_file_name = properties_file_name;

	strcpy(prop_file_name, source_file_name);
	strcpy(prop_file_name + strlen(prop_file_name), ".Contents\\");
	strcpy(prop_file_name + strlen(prop_file_name), OLE_PROP_STREAM);
	strips_property_set = read_ole_properties2(prop_file_name);
	if (opts.verbose_flag>3)
	{
		printf("printing properties from %s\n",OLE_PROP_STREAM);
		print_ole_properties(strips_property_set);
	}

	// We dont really need this next property set,
	// but it may be interesting for debuging
	if ( (source_file_name != NULL) && (opts.verbose_flag>3) )
	{
		strcpy(prop_file_name, source_file_name);
		strcpy(prop_file_name + strlen(prop_file_name), ".Contents\\.SummaryInformation");
		summary_property_set = read_ole_properties2(prop_file_name);
		printf("printing properties from .SummaryInformation\n");
		print_ole_properties(summary_property_set);
	}

	debug_pause();

	xfree(prop_file_name);
	ole_property_set_delete(summary_property_set);

	return strips_property_set;
}
