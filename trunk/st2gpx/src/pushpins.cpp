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

// FIXME (re)use a single db conection for all recordsets for improved performance

// for vt type values see http://www.canaimasoft.com/f90VB/OnlineManuals/UserManual/TH_99.htm

//#import "c:\program files\common files\system\ado\msado15.dll" rename ( "EOF", "adoEOF" )
//#import <msado15.dll> rename ( "EOF", "adoEOF" )
#include "msado15.tlh"
#include "msado15.tli"

#include <windows.h>
#include <initguid.h>  // Include only once in your application
#include <oleauto.h>   // for sysstringlength
#include <stdio.h>
#include <string.h>
#include "st2gpx.h"
#include "gpx.h"
#include "pushpins.h"
#include "ppinutil.h"
#include "contents.h"


struct InitOle {
InitOle()  { ::CoInitialize(NULL); }
~InitOle() { ::CoUninitialize();   }
} _init_InitOle_;


VARIANT val2variant(unsigned short fieldtype, void* stor_var)
{
	VARIANT vt_val;
	vt_val.vt=fieldtype;

	// not sure if this is neccessary
	VariantInit(&vt_val);

	switch (fieldtype)
	{
	case VT_I4:
		vt_val.lVal = *(int*)stor_var;
		vt_val.vt=fieldtype;
		break;

	case VT_I2:
		vt_val.iVal = *(short int*)stor_var ;
		vt_val.vt=fieldtype;
		break;

	case VT_BSTR:
		// Note that stor_var must be a pointer to a null-terminated char* string (not unicode)
		if ( (stor_var==NULL) || (strlen((char*)stor_var)==0) )
			vt_val.vt=VT_NULL;
		else
		{
			vt_val.vt=fieldtype;
			vt_val.bstrVal = _com_util::ConvertStringToBSTR((char*)stor_var);
		}
		break;

/*	case (VT_ARRAY | VT_UI1): // 8192 | 17 = 8209
		SafeArrayGetLBound(vt_val.parray, 1, &lbound);
		SafeArrayGetUBound(vt_val.parray, 1, &ubound);
		if( (SafeArrayGetDim(vt_val.parray)!=1) || (lbound !=0) )
		{
			printf("got safe array of short ints, with %d dimensions and bounds %d:%d\n", 
				SafeArrayGetDim(vt_val.parray), lbound, ubound);
			printf("Sadly, I was expecting 1 dimention and lower bound of zero, so I will ignore this array.\n");
			break;
		}
		for(elmt=lbound; elmt<ubound+1; elmt++)
		{
			SafeArrayGetElement(vt_val.parray, &elmt, &array_val);
			//printf("array(%d)=%d\n", elmt, array_val);
			((char*)stor_var)[elmt-lbound]=array_val;
		}
		//debug_pause();
		break;
*/
	default:
		fprintf(stderr,"Unsupported type %d, setting DB value to NULL\n", vt_val.vt);
		vt_val.vt=VT_NULL;
	}

	return vt_val;
};

void variant2val(VARIANT vt_val, unsigned short fieldtype, void* stor_var, int expected_buf_length)
{
	char* shortstr=NULL;
	long lbound;
	long ubound;
	long elmt;
	// should be a short?
	char array_val=0;

	if ((vt_val.vt == fieldtype) || ( (fieldtype==VT_BSTR) && (vt_val.vt==VT_NULL))
		|| ( (fieldtype==(VT_ARRAY | VT_UI1)) && (vt_val.vt==VT_NULL)) )
	{
		switch (vt_val.vt)
		{
		case VT_I4:
			*(int*)stor_var = vt_val.lVal;
			break;

		case VT_I2:
			// FIXME should I do this as short?
			*(short int*)stor_var = vt_val.iVal;
			break;

		case VT_BSTR:
			//shortstr = wstr2str(vt_val.bstrVal);
			shortstr = _com_util::ConvertBSTRToString(vt_val.bstrVal);
			*(char**)stor_var = shortstr;
			break;

		case VT_NULL:
			// FIXME should return NULL here and hande the nulls elsewhere
			// expected string, this is null 
			// so return empty string
			shortstr = (char*)xmalloc(1);
			shortstr[0]=0;
			*(char**)stor_var = shortstr;
			break;
		case (VT_ARRAY | VT_UI1): // 8192 | 17 = 8209
			SafeArrayGetLBound(vt_val.parray, 1, &lbound);
			SafeArrayGetUBound(vt_val.parray, 1, &ubound);
			if( (SafeArrayGetDim(vt_val.parray)!=1) || (lbound !=0) )
			{
				printf("got safe array of short ints, with %d dimensions and bounds %d:%d\n", 
					SafeArrayGetDim(vt_val.parray), lbound, ubound);
				printf("Sadly, I was expecting 1 dimension and lower bound of zero, so I will ignore this array.\n");
				break;
			}

			if (ubound+1 != expected_buf_length)
			{
				printf("Error reading VT_ARRAY: expected length %d but array has length %d\n",
						expected_buf_length, ubound);
				//return;
			}
			for(elmt=lbound; elmt<ubound+1; elmt++)
			{
				SafeArrayGetElement(vt_val.parray, &elmt, &array_val);
				if (ubound+1 == expected_buf_length)
					((char*)stor_var)[elmt-lbound]=array_val;
				else
					printf("array(%d)=%d\n", elmt, array_val);
			}
			//debug_pause();
			break;
		default:
			fprintf(stderr,"Unsupported type %d, unable to read from DB\n", vt_val.vt);
		}
	}
	else
	{
		fprintf(stderr,"Type mismatch when reading from DB, expected type %d read type %d\n", 
			fieldtype, vt_val.vt);
		debug_pause();
	}

}

EXTERN_C struct pushpin_safelist * read_pushpins(char* ppin_file_name)
{

	struct pushpin * ppin;
	int ppinnum=0;
	int UdmDataId=0;
	struct pushpin_safelist * ppplist = pushpin_safelist_new();
	// number of pushpins to allocate pointer-array storage for, this is incremented as needed
	int ppin_list_alloc_size=100;
	int val_type=0;
	char * ppin_sql = NULL;
	char * UDM_sql = NULL;
	char RenderData_buf[4]="";
	int RenderData_len=0;

	ppplist->pushpin_list = (struct pushpin **)xmalloc(ppin_list_alloc_size*sizeof(struct pushpin *));

	if (opts.st_version_num<9)
		ppin_sql = "SELECT UD_Secondary.UdId, UD_Secondary.UdName, UD_Secondary.NoteShort, UD_Secondary.NoteLong, UD_Main.Grid, UD_Main.Precision, UD_Main.RenderData, UD_Main.MatchId, UD_Main.MOBBId FROM UD_Main INNER JOIN UD_Secondary ON UD_Main.UdId = UD_Secondary.UdId";
	else
		ppin_sql = "SELECT UdId, UdName, NoteShort, NoteLong, Grid, Precision, RenderData, MatchId, MOBBId FROM UD_Main";

	try 
	{
		HRESULT hr = S_OK;
		ADODB::_RecordsetPtr rs; 
		char cnstr[500];
		sprintf(cnstr, "PROVIDER=Microsoft.Jet.OLEDB.4.0;data source=%s; Jet OLEDB:Database Password=Geo80;", ppin_file_name);

		hr = rs.CreateInstance( __uuidof( ADODB::Recordset ) );

		// *****************
		// Read GEODB_LastId
		// *****************

		// FIXME read dbversion, use that instead of file version for setting sql

		// *************
		// read pushpins
		// *************

		hr = rs->Open(ppin_sql, 
			          cnstr, 
					  ADODB::adOpenForwardOnly, 
					  ADODB::adLockReadOnly, 
					  -1 );

		while ((rs->adoEOF == FALSE))
		{
			ppin = pushpin_new();

			variant2val(rs->Fields->GetItem("UdId")->Value,		VT_I4,	&(ppin->UdId), 0);
			variant2val(rs->Fields->GetItem("UdName")->Value,	VT_BSTR,&(ppin->UdName), 0);
			variant2val(rs->Fields->GetItem("Grid")->Value,		VT_I4,	&(ppin->Grid), 0);
			variant2val(rs->Fields->GetItem("Precision")->Value,VT_I4,	&(ppin->Precision), 0);
			variant2val(rs->Fields->GetItem("NoteShort")->Value,VT_BSTR,&(ppin->NoteShort), 0);

			RenderData_len = rs->Fields->GetItem("RenderData")->ActualSize;
			val_type = rs->Fields->GetItem("RenderData")->Value.vt;
			if(val_type!=VT_NULL)
			{
				if (RenderData_len==4)
				{
					variant2val(rs->Fields->GetItem("RenderData")->Value,(VT_ARRAY | VT_UI1),  RenderData_buf, RenderData_len);
					ppin->RenderData = *(int*)RenderData_buf;
					//printf("got pushpin symbol %#x=%d\n", ppin->RenderData, ppin->RenderData);
				}
				else if (RenderData_len==8)
				{
					variant2val(rs->Fields->GetItem("RenderData")->Value,(VT_ARRAY | VT_UI1),  RenderData_buf, RenderData_len);
					ppin->RenderData = *(int*)RenderData_buf;
					//printf("got pushpin symbol %#x=%d\n", ppin->RenderData, ppin->RenderData);
					ppin->RenderData2 = *(int*)(RenderData_buf+4);
					if(ppin->RenderData2 != 0)
					{
						printf("got pushpin symbol %#x=%d\n", ppin->RenderData, ppin->RenderData);
						printf("Got pushpin symbol part2 %#x=%d\n", ppin->RenderData2, ppin->RenderData2);
					}
				}
				else
				{
					printf("Unexpected RenderData_len=%d\n", RenderData_len);
					variant2val(rs->Fields->GetItem("RenderData")->Value,(VT_ARRAY | VT_UI1),  RenderData_buf, RenderData_len);
					printbuf(RenderData_buf, RenderData_len);
				}
			}

			variant2val(rs->Fields->GetItem("MatchId")->Value,	VT_I2,  &(ppin->MatchId), 0);
			variant2val(rs->Fields->GetItem("MOBBId")->Value,	VT_I4,  &(ppin->MOBBId), 0);

			str2ascii(ppin->UdName);
			str2ascii(ppin->NoteShort);

			if (opts.verbose_flag > 3)
				printf("Read pushpin UdId=%d, UdName=%s, Grid=%d, Precision=%d, NoteShort=%s\n", 
					ppin->UdId, ppin->UdName, 
					ppin->Grid, ppin->Precision, ppin->NoteShort);
			// what are the valid matchIds?
			if (ppin->MatchId==4)
			{
				printf("Read unmatched pushpin UdId=%d, UdName=%s, Grid=%d, Precision=%d, NoteShort=%s\n", 
					ppin->UdId, ppin->UdName, 
					ppin->Grid, ppin->Precision, ppin->NoteShort);
				printf("Discarding this pushpin because it is not geocoded (i.e. not matched to map).\n");
				pushpin_delete(ppin);
			}
			else
			{
				ppplist->pushpin_list[ppinnum]=ppin;
				ppinnum++;
				if (ppinnum>ppin_list_alloc_size-1)
				{
					ppin_list_alloc_size += 100;
					ppplist->pushpin_list = (struct pushpin **)xrealloc(ppplist->pushpin_list, ppin_list_alloc_size*sizeof(struct pushpin *));
				}
			}
			hr = rs->MoveNext();
		}
		hr = rs->Close();
		ppplist->num_pushpins=ppinnum;

		// *************
		// read UDM_Data
		// *************

		// always read udm data now
		//if (opts.explore_flag)
		{
			UDM_sql = "Select UdmDataId, UdmData from UDM_Data";

			hr = rs->Open(UDM_sql, 
						  cnstr, 
						  ADODB::adOpenForwardOnly, 
						  ADODB::adLockReadOnly, 
						  -1 );

			while ((rs->adoEOF == FALSE))
			{
				variant2val(rs->Fields->GetItem("UdmDataId")->Value, VT_I4, &UdmDataId, 0);
				if ( (UdmDataId<0) || (UdmDataId>3) )
				{
					printf("*** Unexpected UdmDataId=%d\n", UdmDataId);
					break;
				}
				
				ppplist->UDM_Data_length[UdmDataId] = rs->Fields->GetItem("UdmData")->ActualSize; 
				ppplist->UDM_Data[UdmDataId]=(char*)xmalloc(ppplist->UDM_Data_length[UdmDataId]);
				variant2val(rs->Fields->GetItem("UdmData")->Value, VT_ARRAY | VT_UI1, ppplist->UDM_Data[UdmDataId], ppplist->UDM_Data_length[UdmDataId] );

				printf("In UDM_Data table, for UdId=%d got %d bytes of data\n",
					UdmDataId, 	ppplist->UDM_Data_length[UdmDataId]);
				
				if (opts.explore_flag)
				{
					printbuf(ppplist->UDM_Data[UdmDataId], ppplist->UDM_Data_length[UdmDataId]);
				}

				hr = rs->MoveNext();
			}
		hr = rs->Close();

		if (opts.explore_flag)
			explore_udm_data(ppplist);

		}

	rs = NULL;

	}
	catch( _com_error &e)
	{
        _bstr_t bstrSource(e.Source());
        _bstr_t bs =  _bstr_t("*** Exception: ") + _bstr_t(e.Error()) + _bstr_t(" Msg: ") 
            + _bstr_t(e.ErrorMessage()) + _bstr_t(" Description: ") 
            + _bstr_t(e.Description());
		
		wprintf(bs);
		_flushall();
        
        if (opts.verbose_flag>4)
			MessageBox(0,bs,bstrSource, MB_OK);
    }  

	if (opts.verbose_flag > 1)
		printf("Read %d pushpins from %s.\n", ppplist->num_pushpins, ppin_file_name);

	return ppplist;
}

EXTERN_C void write_pushpins_from_gpx(char* ppin_file_name, 
									  struct gpx_data * all_gpx, 
									  struct contents * conts,
									  char* conts_file_name)
{
	short int DbVersion=0;
	int LastSetId=0;
	long LastUserDataId=0;
	long thisfirstUserDataId=0;
	long thisUserDataId=0;
	char* sql;
	char* sql2;

	// nothing to write
	if (all_gpx->wpt_list_count==0)
		return;

	try 
	{
		HRESULT hr = S_OK;
		ADODB::_RecordsetPtr rs; 
		ADODB::_RecordsetPtr rs2; 
		char cnstr[500];
		sprintf(cnstr, "PROVIDER=Microsoft.Jet.OLEDB.4.0;data source=%s; Jet OLEDB:Database Password=Geo80;", ppin_file_name);

		hr = rs.CreateInstance( __uuidof( ADODB::Recordset ) );

		// *****************
		// Read GEODB_LastId
		// *****************

		sql = "select DbVersion, LastSetId, LastUserDataId from GEODB_LastId";
		hr = rs->Open(sql, 
			          cnstr, 
					  ADODB::adOpenKeyset, 
					  ADODB::adLockOptimistic, 
					  -1 );

		variant2val(rs->Fields->GetItem("DbVersion")->Value,		VT_I2,	&DbVersion, 0);
		variant2val(rs->Fields->GetItem("LastSetId")->Value,		VT_I4,	&LastSetId, 0);
		variant2val(rs->Fields->GetItem("LastUserDataId")->Value,	VT_I4,	&LastUserDataId, 0);

		printf("Got DbVersion=%d, LastSetId=%d and LastUserDataId=%d\n", DbVersion, LastSetId, LastUserDataId);

		// *****************
		// update LastSetId, LastUserDataId in GEODB_LastId
		// *****************

		// Later we might import pushpins as different sets, 
		// but for now we just import as one set

		LastSetId += 1;
		thisfirstUserDataId = LastUserDataId +1;
		LastUserDataId += all_gpx->wpt_list_count;

		rs->Fields->GetItem("LastSetId"     )->Value =	val2variant(VT_I4,	&LastSetId);
		rs->Fields->GetItem("LastUserDataId")->Value =	val2variant(VT_I4,	&LastUserDataId);

		rs->Update();
		hr = rs->Close();
		//printf("Updated LastSetId, LastUserDataId in GEODB_LastId\n");


		// ******************
		// Write all pushpins
		// ******************

		if (opts.st_version_num<9)
		{
			sql = "Select UdId, SetId, Grid, Precision, RenderData, MatchId, MOBBId, SourceUdId from UD_Main";
			sql2 = "Select UdId, NoteTypeId, GeocodeHierarchy, GeocodeContext, UdName, NoteShort, NoteLong from UD_Secondary";
			hr = rs->Open(sql, 
			          cnstr, 
					  ADODB::adOpenKeyset, 
					  ADODB::adLockOptimistic, 
					  -1 );
			hr = rs2.CreateInstance( __uuidof( ADODB::Recordset ) );
			hr = rs2->Open(sql2, 
						  cnstr, 
						  ADODB::adOpenKeyset, 
						  ADODB::adLockOptimistic, 
						  -1 );
		}
		else
		{
			sql = "Select UdId, SetId, Grid, Precision, RenderData, MatchId, MOBBId, SourceUdId, NoteTypeId, GeocodeHierarchy, GeocodeContext, UdName, NoteShort, NoteLong from UD_Main";
			// maybe we have to do this later, when rs is set?
			hr = rs->Open(sql, 
			          cnstr, 
					  ADODB::adOpenKeyset, 
					  ADODB::adLockOptimistic, 
					  -1 );
		}

 
		int w;
		struct gpxpt * gpt;
		struct grid_point gridpt;
		long lzero=0;
		short int sizero=0;
		short int sione=1;
		short int sitwo=2;

		// do for each wpt
		for(w=0; w<all_gpx->wpt_list_count; w++)
		{
			if (opts.st_version_num<9)
			{
				rs->AddNew();
				rs2->AddNew();
			}
			else
			{
				rs->AddNew();
				rs2 = rs;
			}

			thisUserDataId=thisfirstUserDataId+w;
			gpt = all_gpx->wpt_list[w];
			gridpt=latlon2grid(gpt->lat, gpt->lon);

			//printf("writing ppin with thisUserDataId=%d, gpt->name=%s\n", thisUserDataId, gpt->name);

			rs->Fields->GetItem("UdId"		)->Value = val2variant(VT_I4,	&thisUserDataId);
			rs->Fields->GetItem("SetId"		)->Value = val2variant(VT_I2,	&LastSetId);
			rs->Fields->GetItem("Grid"		)->Value = val2variant(VT_I4,	&(gridpt.grid));
			rs->Fields->GetItem("Precision"	)->Value = val2variant(VT_I4,	&(gridpt.precision));
			rs->Fields->GetItem("MatchId"	)->Value = val2variant(VT_I2,	&sitwo);
			// This is stored in the DB as binary type, 
			// but hopefully it should be ok to write as int.
			// FIXME - Potential problem: 
			// This field has longer maximum width in newer MAP versions.
			// Width 32 (AR2001), 64 (MP2002), 128 (AR2003)

			if(gpt->symbol != 0)
				rs->Fields->GetItem("RenderData"	)->Value = val2variant(VT_I4,	&(gpt->symbol));
			rs->Fields->GetItem("MOBBId"	)->Value = val2variant(VT_I4,	&lzero);
			rs->Fields->GetItem("SourceUdId")->Value = val2variant(VT_I4,	&lzero);

			if (opts.st_version_num<9)
				rs->Update();

			rs2->Fields->GetItem("UdId"		)->Value = val2variant(VT_I4,  &thisUserDataId);
			rs2->Fields->GetItem("UdName"   )->Value = val2variant(VT_BSTR,(gpt->name));
			rs2->Fields->GetItem("NoteTypeId")->Value = val2variant(VT_I2,  &sione);
			rs2->Fields->GetItem("NoteShort")->Value = val2variant(VT_BSTR,(gpt->desc));
			rs2->Fields->GetItem("GeocodeHierarchy")->Value = val2variant(VT_I2,  &sizero);
			rs2->Fields->GetItem("GeocodeContext"  )->Value = val2variant(VT_I4,  &lzero);
			// sometime I should support this
			// rs2->Fields->GetItem("NoteLong"   )->Value = val2variant(?,  NULL);
			rs2->Update();
		}

		if (opts.st_version_num<9)
		{	
			rs->Close();
			rs2->Close();
			rs2 = NULL;
		} 
		else
		{
			rs->Close();
		}

		//printf("Inserted all pushpin records\n");

		// **********************
		// insert row in SET_Main, 
		// **********************

		// SetId, SetName, UdCount, MatchedCount 

		// FIXME cant I use a compiler var for size of largest short?
		if (LastSetId>(1<<15))
			printf("too many set ids!");
		short int SetId=(short int)LastSetId;

		char* SetName = all_gpx->data_source_name;
		short int RenderMethod = 2;
		short int GeocodeMethod = 5;
		short int CreateMethod = 2;
		short int GeometryType = 1;
		long UdCount=all_gpx->wpt_list_count;
		long MatchedCount=all_gpx->wpt_list_count;
		long SkippedCount = 0;
		long UnmatchedCount = 0;
		long CounterUpdateMask=0;
		short int ParentSetId=0;
		short int IsRendered=-1;
		long Z_Order=0; //5000;
		long GeocodeCtxt=39070; // ???
		int HLnkSrc=3;
		long HLnkColId=0;

		if(opts.st_version_num<9)
			sql = "SELECT SetId, SetName, RenderMethod, GeocodeMethod, CreateMethod, GeometryType, UdCount, MatchedCount, SkippedCount, UnmatchedCount, CounterUpdateMask, ParentSetId, IsRendered, Z_Order, GeocodeCtxt FROM SET_Main";
		else
			sql = "SELECT SetId, SetName, RenderMethod, GeocodeMethod, CreateMethod, GeometryType, UdCount, MatchedCount, SkippedCount, UnmatchedCount, CounterUpdateMask, ParentSetId, IsRendered, Z_Order, GeocodeCtxt, HLnkSrc, HLnkColId FROM SET_Main";
 
		hr = rs->Open(sql, 
			      cnstr, 
				  ADODB::adOpenKeyset, 
				  ADODB::adLockOptimistic, 
				  -1 );

		rs->AddNew();

		rs->Fields->GetItem("SetId"				)->Value = val2variant(VT_I2,	&SetId);
		rs->Fields->GetItem("SetName"			)->Value = val2variant(VT_BSTR,	SetName);
		rs->Fields->GetItem("RenderMethod"		)->Value = val2variant(VT_I2,	&RenderMethod);
		rs->Fields->GetItem("GeocodeMethod"		)->Value = val2variant(VT_I2,	&GeocodeMethod);
		rs->Fields->GetItem("CreateMethod"		)->Value = val2variant(VT_I2,	&CreateMethod);
		rs->Fields->GetItem("GeometryType"		)->Value = val2variant(VT_I2,	&GeometryType);
		rs->Fields->GetItem("UdCount"			)->Value = val2variant(VT_I4,	&UdCount);
		rs->Fields->GetItem("MatchedCount"		)->Value = val2variant(VT_I4,	&MatchedCount);
		rs->Fields->GetItem("SkippedCount"		)->Value = val2variant(VT_I4,	&SkippedCount);
		rs->Fields->GetItem("UnmatchedCount"	)->Value = val2variant(VT_I4,	&UnmatchedCount);
		rs->Fields->GetItem("CounterUpdateMask"	)->Value = val2variant(VT_I4,	&CounterUpdateMask);
		rs->Fields->GetItem("ParentSetId"		)->Value = val2variant(VT_I2,	&ParentSetId);
		rs->Fields->GetItem("IsRendered"		)->Value = val2variant(VT_I2,	&IsRendered);
		rs->Fields->GetItem("Z_Order"			)->Value = val2variant(VT_I4,	&Z_Order);
		rs->Fields->GetItem("GeocodeCtxt"		)->Value = val2variant(VT_I4,	&GeocodeCtxt);
		
		if(opts.st_version_num>8)
		{
			rs->Fields->GetItem("HLnkSrc"		)->Value = val2variant(VT_I2,	&HLnkSrc);
			rs->Fields->GetItem("HLnkColId"		)->Value = val2variant(VT_I4,	&HLnkColId);
		}
		
		rs->Update();
		rs->Close();


		// ***************************
		// construct & update UDM_data
		// ***************************

		// we need to do this to show ppin name, info, symbol etc

		// *******************
		// COM / ADO  clean up 
		// *******************

		rs = NULL;
		if (rs2)
			rs2 = NULL;

		// *****************
		// update contents
		// *****************

		struct contents * mod_conts = contents_insert_ppinset(conts, (unsigned short)LastSetId);
		if (mod_conts!=NULL)
		{
			write_contents(mod_conts, conts_file_name);
			contents_delete(mod_conts);

			printf("Wrote %d waypoints as PushPins\n", all_gpx->wpt_list_count);
			debug_pause();
		}
		else
			printf("failed to successfully write pushpins, output is now corrupt.\n");
	 }
	catch( _com_error &e)
	{
        _bstr_t bstrSource(e.Source());
        _bstr_t bs =  _bstr_t("*** Exception: ") + _bstr_t(e.Error()) + _bstr_t(" Msg: ") 
            + _bstr_t(e.ErrorMessage()) + _bstr_t(" Description: ") 
            + _bstr_t(e.Description());
		
		wprintf(bs);
		_flushall();
        
//        if (opts.verbose_flag>4)
			MessageBox(0,bs,bstrSource, MB_OK);
    }  

}
