/*Copyright by Pabs (pabs3@zip.to, http://zip.to/pabs3/) Dec 2002*/

/*Licenced under the BSD sloppyright - nasty little daemons better not use this source & not attribute me*/

/*Compile notes:
	M$VC: just open this file & press the build button or select build from the
	      build menu since this will create a default project that does nicely
	GCC/Cygwin: gcc -mno-cygwin -fvtable-thunks -o istorage-make.exe istorage-make.cpp -lole32 should do the trick
	            if w32api & mingw are set up correctly & the headers are correct
	GCC/Wine: You'll have to figure it out for yourself since I don't yet have
	          access to Linux (Debian) cause I need a new HD + a DVD drive ;-)
*/

/* Usage:
	-ole
		convert to an OLE compound file (default)
	-its
		convert to an ITS file
	-its -uc
		don't compress (default)
	-its -cd
		compress the data after creation
	-its -cdp
		compress the data and path after creation
*/

/*TODO:
	think up a proper name
	Add popt/getopt processing
	use libOLE on non-win32 - http://chicago.sf.net
	Implement options for the following
		rename specific files to something the normal filesystem can't handle, like 0x1 characters
		various tar/unzip options
		--help --version
*/


#include <windows.h>
#include <ole2.h>
#include <stdio.h>
#include <string.h>


//These next few bits are from a sample available from http://www.keyworks.net/code.htm (see http://helpware.net/delphi/index.html for a Delphi sample - need to join a yahoo group first - example 10 is the one to look at)
//This stuff is supposed to be documented in the MSDN, but I only found 1 page that references ITStorage & that is an error codes list
DEFINE_GUID(CLSID_ITStorage, 0x5d02926a, 0x212e, 0x11d0, 0x9d, 0xf9, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xec);
DEFINE_GUID(IID_ITStorage, 0x88cc31de, 0x27ab, 0x11d0, 0x9d, 0xf9, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xec);

const GUID CLSID_ITStorage = { 0x5d02926a, 0x212e, 0x11d0, { 0x9d, 0xf9, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xec } };
const GUID IID_ITStorage = { 0x88cc31de, 0x27ab, 0x11d0, { 0x9d, 0xf9, 0x0, 0xa0, 0xc9, 0x22, 0xe6, 0xec} };

typedef struct _ITS_Control_Data
{
	UINT cdwControlData; // Number of DWords to follow.
	UINT adwControlData[1]; // Actually this will be adwControlData[cdwControlData]
} ITS_Control_Data, *PITS_Control_Data;

typedef enum ECompactionLev {
	COMPACT_DATA = 0,
	COMPACT_DATA_AND_PATH
};

DECLARE_INTERFACE_(IITStorage, IUnknown)
{
	STDMETHOD(StgCreateDocfile) (const WCHAR* pwcsName, DWORD grfMode, DWORD reserved, IStorage** ppstgOpen) PURE;
	STDMETHOD(StgCreateDocfileOnILockBytes) (ILockBytes * plkbyt, DWORD grfMode, DWORD reserved, IStorage** ppstgOpen) PURE;
	STDMETHOD(StgIsStorageFile) (const WCHAR * pwcsName) PURE;
	STDMETHOD(StgIsStorageILockBytes) (ILockBytes * plkbyt) PURE;
	STDMETHOD(StgOpenStorage)(const WCHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage ** ppstgOpen) PURE;
	STDMETHOD(StgOpenStorageOnILockBytes)(ILockBytes * plkbyt, IStorage * pStgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage ** ppstgOpen) PURE;
	STDMETHOD(StgSetTimes)(WCHAR const* lpszName, FILETIME const* pctime, FILETIME const* patime, FILETIME const* pmtime) PURE;
	STDMETHOD(SetControlData)(PITS_Control_Data pControlData) PURE;
	STDMETHOD(DefaultControlData)(PITS_Control_Data *ppControlData) PURE;
	STDMETHOD(Compact)(const WCHAR* pwcsName, ECompactionLev iLev) PURE;
};

IITStorage* ITStorage = NULL;

struct OPTIONS{
	bool its;
	bool compress;
	ECompactionLev compression;
	char indentchar;
	char* ole_appendstring;
	char* its_appendstring;
} options = {false,false,COMPACT_DATA,'\t',".ole",".its"};

HRESULT r;

void EnumFolder(char*f,IStorage* is);

int main(int argc, char *argv[]){
	//Get an instance of the thing to call for creating chms
	CoInitialize(NULL);
 	CoCreateInstance(CLSID_ITStorage, NULL, CLSCTX_INPROC_SERVER, IID_ITStorage, (void**)&ITStorage);
	char t[2];DWORD cdl = GetCurrentDirectory(2,t);
	char* current_dir = new char[cdl];
	if(current_dir){
		if(GetCurrentDirectory(cdl,current_dir)){
			for(int argi=1;argi<argc;argi++){
				if( strcmp(argv[argi],"-ole") == 0 ){ options.its = false; continue; }
				else if( strcmp(argv[argi],"-its") == 0 ){ options.its = true; continue; }
				else if( strcmp(argv[argi],"-uc") == 0 ){ options.compress = false; continue; }
				else if( strcmp(argv[argi],"-cd") == 0 ){
					options.compress = true;
					options.compression = COMPACT_DATA;
					continue;
				} else if( strcmp(argv[argi],"-cdp") == 0 ){
					options.compress = true;
					options.compression = COMPACT_DATA_AND_PATH;
					continue;
				}
		
				if(SetCurrentDirectory(current_dir)){
					if(SetCurrentDirectory(argv[argi])){
						if(SetCurrentDirectory("..")){
							unsigned int len = strlen(argv[argi]);
							char* appendstring = options.its ? options.its_appendstring : options.ole_appendstring;
							unsigned int appendstring_len = strlen(appendstring);
							char* file = new char[len+appendstring_len+1];
							if(file){
								strcpy(file,argv[argi]);
								strcpy(&file[len],appendstring);
			
								DWORD wlen = MultiByteToWideChar(CP_ACP,0,file,len+appendstring_len+1,NULL,0);
								WCHAR* wfile = new WCHAR[wlen+1];
								if(wfile){
									if(MultiByteToWideChar(CP_ACP,0,file,len+appendstring_len+1,wfile,wlen)){
										IStorage* is = NULL;
										if( options.its && ITStorage )
											r = ITStorage->StgCreateDocfile( wfile, STGM_CREATE|STGM_WRITE|STGM_SHARE_EXCLUSIVE, 0, &is );
										else
											r = StgCreateDocfile( wfile, STGM_CREATE|STGM_WRITE|STGM_SHARE_EXCLUSIVE, 0, &is );
			
										if( r == S_OK || is ){
											EnumFolder( argv[argi], is );
											is->Release(); is = NULL;
											if( options.its && ITStorage && options.compress ){
												r = ITStorage->Compact( wfile, options.compression );
												if(r!=S_OK) fputs("Could not compress the specified file - Ignoring & continuing\n",stderr);
											}
										} else fputs("Could not create the specified file - Ignoring & continuing\n",stderr);
									} else fputs("Could not convert the path to Unicode for the Unicode only StgOpenStorage - Ignoring & continuing\n",stderr);
									delete[]wfile; wfile = NULL;
								} else fputs("Could not alloc memory for the Unicode filename for the Unicode only StgCreateDocfile - Ignoring & continuing",stderr);
								delete[]file; file = NULL;
							} else fputs("Could not alloc memory for the filename - Ignoring & continuing",stderr);
						} else fputs("Could not enter (directory) - Ignoring & continuing\n",stderr);
					} else fputs("Could not enter (directory) - Ignoring & continuing\n",stderr);
				} else fputs("Could not reset current directory - Ignoring & continuing\n",stderr);
			}
		} else fputs("Could not get current directory - Failed\n",stderr);
	} else fputs("Could not allocate memory for current directory - Failed\n",stderr);
	if( ITStorage ) ITStorage->Release();
	CoUninitialize();
	fflush(stderr);
	return 0;
}

void EnumFolder(char*fo, IStorage* is){
	if(SetCurrentDirectory(fo)){
		HANDLE ff;WIN32_FIND_DATA wfd;
		ff = FindFirstFile("*",&wfd);
		if(ff!=INVALID_HANDLE_VALUE&&ff!=0){
			do{
				if( strcmp(wfd.cFileName,".") == 0 || strcmp(wfd.cFileName,"..") == 0 ) continue;
				DWORD len = strlen(wfd.cFileName)+1;
				DWORD wlen = MultiByteToWideChar(CP_ACP,0,wfd.cFileName,len,NULL,0);
				WCHAR* wpath = new WCHAR[len];
				if(wpath){
					if(MultiByteToWideChar(CP_ACP,0,wfd.cFileName,len,wpath,wlen)){
						if(wfd.dwFileAttributes&FILE_ATTRIBUTE_DIRECTORY){
							IStorage* is2 = NULL;
							r = is->CreateStorage(wpath,STGM_WRITE|STGM_SHARE_EXCLUSIVE,0,0,&is2);
							if( r == S_OK && is2 ){
								EnumFolder(wfd.cFileName,is2);
								//if(S_OK!=is2->SetStateBits(wfd.dwFileAttributes)) fputs("Could not set state bits (directory) - Ignoring & continuing\n",stderr);
								is2->Release(); is2 = NULL;
							} else fputs("Could not create (directory) - Ignoring & continuing\n",stderr);
						} else {
							IStream* is2 = NULL;
							r = is->CreateStream(wpath,STGM_WRITE|STGM_SHARE_EXCLUSIVE,0,0,&is2);
							if( r == S_OK && is2 ){
								HANDLE f = CreateFile(wfd.cFileName,GENERIC_READ,0,NULL,OPEN_EXISTING,FILE_FLAG_SEQUENTIAL_SCAN,NULL);
								if(f!=INVALID_HANDLE_VALUE&&f!=0/*Fucking lying MSDN*/){
									ULARGE_INTEGER fs={wfd.nFileSizeLow,wfd.nFileSizeHigh},i={0};
									is2->SetSize(fs);
									BYTE* p;
									DWORD nobm = (DWORD)fs.QuadPart, nobr, nobw;
									for(;;){
										p = new BYTE[nobm]; if(p)break;
										nobm/=2; if(!nobm)break;
									}
									if(p){
										bool err = false;
										do{
											if(!ReadFile(f,p,nobm,&nobr,NULL)||nobm!=nobr){fputs("Read error (file) - terminating this file\n",stderr);err=true;}//read less than asked and there was an error other than EOF - MSDN says that the compound file implementation of Read returns S_OK if the end of the stream was reached during the read
											nobw=0;
											r=is2->Write(p,nobr,&nobw);
											i.QuadPart+=nobw;
											if(r!=S_OK||nobr!=nobw){fputs("Write error (file) - terminating this file\n",stderr);err=true;}
										} while (i.QuadPart<fs.QuadPart&&!err);
										delete[]p; p = NULL;
									} else fputs("Could not allocate even 1 byte of mem for a buffer for transferring files - please free some memory & try again - Ignoring & continuing\n",stderr);
									CloseHandle(f); f = NULL;
								} else fputs("Could not open (file) - Ignoring & continuing\n",stderr);
								is2->Release(); is2 = NULL;
							} else fputs("Could not create (file) - Ignoring & continuing\n",stderr);
						}
						if(S_OK!=is->SetElementTimes(wpath,&wfd.ftCreationTime,&wfd.ftLastAccessTime,&wfd.ftLastWriteTime)) fputs("Could not set the creation, last modification or last access time - Ignoring & continuing\n",stderr);
					}
				}
			} while(FindNextFile(ff,&wfd));
			FindClose(ff);
		}
		SetCurrentDirectory("..");
	} else fputs("Could not enter (directory) - Ignoring & continuing\n",stderr);
}
