/*
	pushpins.cpp

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

#import "c:\program files\common files\system\ado\msado15.dll" rename ( "EOF", "adoEOF" )
//#import <msado15.dll> rename ( "EOF", "adoEOF" )

#include <windows.h>
#include <initguid.h>  // Include only once in your application
//#include "adoid.h"     // ADO GUID's
//#include "adoint.h"    // ADO Classes, enums, etc.
#include <oleauto.h>   // for sysstringlength
#include <stdio.h>
//#include <stdlib.h>
//#include <malloc.h>
//#include <memory.h>
#include <string.h>

#include "pushpins.h"

struct InitOle {
InitOle()  { ::CoInitialize(NULL); }
~InitOle() { ::CoUninitialize();   }
} _init_InitOle_;

int verbose_flag;

void * xmalloc(size_t size)
{
	void *obj = malloc(size);
	if (!obj)
	{
		fprintf(stderr, "Unable to allocate %d bytes of memory.\n", size);
	}
	return obj;
}

LPWSTR wstr2cpy(BSTR wstr)
{
	int len=wcslen(wstr);
	LPWSTR newstr=(LPWSTR)xmalloc((len+1)*sizeof(WCHAR));
	wcscpy(newstr, wstr);
	return newstr;
}

char* wstr2str(BSTR wstr)
// there must be a better way to do this
{
	int len;
	int i;
	char* newstr=NULL;
	// call to get length
	len = WideCharToMultiByte(
				CP_THREAD_ACP,
				0,
				wstr,
				-1,
				newstr,
				0, 
				NULL,
				0);
	if(len==0)
	{
		//printf("wide2normal got length %d\n", len);
		len=wcslen(wstr);
		//printf("wide2normal cheated and got length %d\n", len);
	};
	newstr = (char*)xmalloc((len+1)*sizeof(char));
	// now call to set string
	len = WideCharToMultiByte(
				CP_THREAD_ACP,
				0,
				wstr,
				-1,
				newstr,
				len, 
				NULL,
				0);
	//printf("wide2normal copied %d chars and got string %s\n", len, newstr);
	if (len==0)
	{
		len=wcslen(wstr);
		for(i=0; i<len; i++)
			newstr[i]=wstr[i];
		newstr[len]=0;
		//printf("wide2normal cheated to get string %s\n", newstr);
	}
	return newstr;
}

void readfield(VARIANT vt_val, unsigned short fieldtype, void* stor_var)
{
//	int len=0;
	LPWSTR tempwstr=NULL;
	char* shortstr=NULL;
//	int status;
//	int i;

	if ((vt_val.vt == fieldtype) || ( (fieldtype==VT_BSTR) && (vt_val.vt==VT_NULL)))
	{
		switch (vt_val.vt)
		{
		case VT_I4:
			*(long*)stor_var = vt_val.lVal;
			break;

		case VT_I2:
			*(int*)stor_var = vt_val.iVal;
			break;

		case VT_BSTR:
			// FIXME handle bstr properly
			tempwstr = wstr2cpy(vt_val.bstrVal);
			//wprintf(L"got temp str%s\n",tempwstr);

			shortstr = wstr2str(vt_val.bstrVal);
			//printf("got short temp str%s\n",shortstr);
			
			//*(void**)stor_var = (void*)tempwstr;
			*(char**)stor_var = shortstr;
			//printf("verified short temp str%s\n", *(char**)stor_var);
			break;

		case VT_NULL:
			// expected string, this is null 
			// so return empty string

			shortstr = (char*)xmalloc(sizeof(char));
			shortstr[0]=0;
			//printf("got short temp str%s\n",shortstr);
			
			*(char**)stor_var = shortstr;
			break;
		default:
			fprintf(stderr,"Unsupported type, unable to read from DB\n");
		}

	}
	else
	{
		fprintf(stderr,"Type mismatch when reading from DB, expected type %d read type %d\n", 
			fieldtype, vt_val.vt);
	}

}

// EXTERN_C 
EXTERN_C void read_ppin_list(char* ppin_file_name)
{

	struct pushpin * ppin;
	int ppinnum=0;
	
	try 
	{
		HRESULT hr = S_OK;
		ADODB::_RecordsetPtr rs; 
		char cnstr[500];
		char sqlstr[] = "SELECT UD_Secondary.UdId, UD_Secondary.UdName, UD_Secondary.NoteShort, UD_Secondary.NoteLong, UD_Main.Grid, UD_Main.Precision FROM UD_Main INNER JOIN UD_Secondary ON UD_Main.UdId = UD_Secondary.UdId";
		sprintf(cnstr, "PROVIDER=Microsoft.Jet.OLEDB.4.0;data source=%s; Jet OLEDB:Database Password=Geo80;", ppin_file_name);

		hr = rs.CreateInstance( __uuidof( ADODB::Recordset ) );
		hr = rs->Open(sqlstr, 
			          cnstr, 
					  ADODB::adOpenForwardOnly, 
					  ADODB::adLockReadOnly, 
					  -1 );
		hr = rs->MoveFirst();
		while ((rs->adoEOF == FALSE))
		{
			ppin = (struct pushpin *)xmalloc(sizeof(struct pushpin));

			readfield(rs->Fields->GetItem("UdId")->Value,		VT_I4,	&(ppin->UdId));
			readfield(rs->Fields->GetItem("UdName")->Value,		VT_BSTR,&(ppin->UdName));
			readfield(rs->Fields->GetItem("Grid")->Value,		VT_I4,	&(ppin->Grid));
			readfield(rs->Fields->GetItem("Precision")->Value,	VT_I4,	&(ppin->Precision));
			readfield(rs->Fields->GetItem("NoteShort")->Value,	VT_BSTR,&(ppin->NoteShort));

			if (verbose_flag > 3)
				printf("Read pushpin UdId=%d, UdName=%s, Grid=%d, Precision=%d, NoteShort=%s\n", 
					ppin->UdId, ppin->UdName, 
					ppin->Grid, ppin->Precision, ppin->NoteShort);

			ppin_list[ppinnum]=ppin;
			ppinnum++;
			if (ppinnum>MAX_PUSHPINS-1)
			{
				printf("Exceeded maximun number of pushpins (I should fix this)\n");
				break;
			}

			hr = rs->MoveNext();
		}
		hr = rs->Close();
		rs = NULL;
	}
	catch( _com_error &e)
	{
        _bstr_t bstrSource(e.Source());
        _bstr_t bs =  _bstr_t(" Error: ") + _bstr_t(e.Error()) + _bstr_t(" Msg: ") 
            + _bstr_t(e.ErrorMessage()) + _bstr_t(" Description: ") 
            + _bstr_t(e.Description());
        
        MessageBox(0,bs,bstrSource, MB_OK);
    }  
}
