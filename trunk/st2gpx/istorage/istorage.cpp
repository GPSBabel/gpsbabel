/*by Pabs (pabs3@zip.to, http://zip.to/pabs3) Nov 2001, April 2002*/

/*Licenced under the BSD sloppyright - nasty little daemons better not use this source & not attribute me*/
/*Why BSD licence? HTFSIK but seriously I wanted to get to know more open/free licences*/

/*Compile notes:
	M$VC: just open this file & press the build button or select build from the
	      build menu since this will create a default project that does nicely
	GCC/Cygwin: gcc -mno-cygwin -fvtable-thunks -lole32 -o istorage.exe istorage.cpp should do the trick
	            if w32api & mingw are set up correctly & the headers are correct
	GCC/Wine: You'll have to figure it out for yourself since I don't yet have
	          access to Linux (Debian) cause I need a new HD + a DVD drive ;-)
*/

/*Usage:
	istorage some.doc some.xls some.pub some.fla some.chm
		will extract each of these files to some.???.Contents directories
		these are M$ Word, Excel and Publisher and Macromedia Flash source files (ooh and a compiled HTML Help file)
*/

/*TODO:
	think up a proper name - unist??? or what??
	Add popt/getopt processing
	reverse engineer the compound file format (& chm format - Matthew T. Russotto has already done some)
	or just use libOLE - http://chicago.sf.net
	so we don't depend on M$ for the implementation
	Implement options for the following
		-i string  invalid chars
		-f char    replace invalid chars with
		-n char    indent with this char
		-a string  append this to each file opened
		various tar/unzip options
		--help --version
*/


#include <windows.h>
#include <ole2.h>
#include <stdio.h>
#include <string.h>


struct OPTIONS{
	char fixchar;
	char* invalidchars;
	char indentchar;
	char* appendstring;
} options = {'.',"<>|/\\\"",'\t',".Contents"};

HRESULT r;

DWORD level = 0;

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

void EnumStorage(IStorage* is);
void fixpath(char* s);

int main(int argc, char *argv[]){
	//Get an instance of the thing to call for opening chms
	CoInitialize(NULL);
 	CoCreateInstance(CLSID_ITStorage, NULL, CLSCTX_INPROC_SERVER, IID_ITStorage, (void**)&ITStorage);
	for(int i=1;i<argc;i++){
		for(DWORD ii=0;ii<level;ii++)fputc(options.indentchar,stderr);
		fputs(argv[i],stderr);
		fputs(": ",stderr);
		DWORD len = strlen(argv[i]);
		DWORD wlen = MultiByteToWideChar(CP_ACP,0,argv[i],len+1,NULL,0);
		WCHAR* wpath = new WCHAR[len+1];
		if(wpath){
			if(MultiByteToWideChar(CP_ACP,0,argv[i],len+1,wpath,wlen)){
				IStorage* is = NULL;
				r = StgOpenStorage(wpath,NULL,STGM_READ|STGM_SHARE_EXCLUSIVE,NULL,0,&is);
				if( ITStorage && r != S_OK && is == NULL ){
					r = ITStorage->StgOpenStorage(wpath,NULL,STGM_READ|STGM_SHARE_DENY_WRITE,NULL,0,&is);
					fputs("StgOpenStorage failed, trying chm version...",stderr);
				}
				delete[]wpath; wpath = NULL;
				if( r == S_OK || is ){
					STATSTG ss;
					if(S_OK==is->Stat(&ss,STATFLAG_NONAME))fprintf(stderr,"CLSID={%8.8X-%4.4X-%4.4X-%4.4X-%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X} State=0x%8.8X reserved=0x%8.8X\n", ss.clsid.Data1, ss.clsid.Data2, ss.clsid.Data3, ss.clsid.Data4[0]<<8/*&0xff00*/| ss.clsid.Data4[1], ss.clsid.Data4[2], ss.clsid.Data4[3], ss.clsid.Data4[4], ss.clsid.Data4[5], ss.clsid.Data4[6], ss.clsid.Data4[7], ss.grfStateBits, ss.reserved);
					else fputs("Stat failed\n",stderr);
					char* path = new char[len+strlen(options.appendstring)+1];
					if(path){
						strcpy(path,argv[i]);
						strcpy(&path[len],options.appendstring);
						RemoveDirectory(path);
						if(CreateDirectory(path,NULL)||GetLastError()==ERROR_ALREADY_EXISTS){
							if(SetCurrentDirectory(path)){
								delete[]path; path = NULL;
								level++;
								EnumStorage(is);
								level--;
								SetCurrentDirectory("..");
							}
							else fputs("Could not enter - Ignoring & continuing\n",stderr);
						}
						else fputs("Could not create (directory) - Ignoring & continuing\n",stderr);
						delete[]path; path = NULL;
					}
					else fputs("Could not alloc memory for the ANSI pathname for the wimpy Win9$ - Ignoring & continuing\n",stderr);
					is->Release();
				}
				else fputs("Could not open the specified file - Ignoring & continuing\n",stderr);
			}
			else fputs("Could not convert the path to Unicode for the Unicode only StgOpenStorage - Ignoring & continuing\n",stderr);
			delete[]wpath; wpath = NULL;
		}
		else fputs("Could not alloc memory for the Unicode filename for the Unicode only StgOpenStorage - Ignoring & continuing",stderr);
	}
	if( ITStorage ) ITStorage->Release();
	CoUninitialize();
	fflush(stderr);//Helps with debugging
	return 0;
}

void EnumStorage(IStorage* is){
	IEnumSTATSTG* iess;
	r = is->EnumElements(0,NULL,0,&iess);
	if( r!= S_OK ) { fputs("Could not enumerate elements - Ignoring & continuing\n",stderr); return;}
	STATSTG ss;
	iess->Reset();
	for(;;){
		r=iess->Next(1,&ss,NULL);
		if( r!= S_OK ) break;
		for(DWORD i=0;i<level;i++)fputc(options.indentchar,stderr);
		fputws(ss.pwcsName,stderr);
		fputs(": ",stderr);
		fprintf(stderr,"CLSID={%8.8X-%4.4X-%4.4X-%4.4X-%2.2X%2.2X%2.2X%2.2X%2.2X%2.2X} State=0x%8.8X reserved=0x%8.8X\n", ss.clsid.Data1, ss.clsid.Data2, ss.clsid.Data3, ss.clsid.Data4[0]<<8/*&0xff00*/| ss.clsid.Data4[1], ss.clsid.Data4[2], ss.clsid.Data4[3], ss.clsid.Data4[4], ss.clsid.Data4[5], ss.clsid.Data4[6], ss.clsid.Data4[7], ss.grfStateBits, ss.reserved);
		DWORD len = wcslen(ss.pwcsName);
		char* path = new char[len+1];
		if(path){
			if(WideCharToMultiByte(CP_ACP,0,ss.pwcsName,len+1,path,len+1,NULL,NULL)){
				fixpath(path);
				switch(ss.type){
					case STGTY_STORAGE:{
						IStorage* is2;
						r = is->OpenStorage(ss.pwcsName,NULL,STGM_READ|STGM_SHARE_EXCLUSIVE,NULL,0,&is2);
						CoTaskMemFree(ss.pwcsName); ss.pwcsName = NULL;
						if( r == S_OK || is2 ){
							if(CreateDirectory(path,NULL)||(r=GetLastError())==ERROR_ALREADY_EXISTS){
								if(r==ERROR_ALREADY_EXISTS)fputs("Already exists (directory) - using anyway\n",stderr);
								else fputs("Created (directory)\n",stderr);
								HANDLE f = CreateFile(path,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,NULL);
								if(f!=INVALID_HANDLE_VALUE&&f!=0/*Fucking lying MSDN*/){
									if(!SetFileTime(f,&ss.ctime,&ss.atime,&ss.mtime))fputs("Could not set the creation, last modification or last access time\n",stderr);
									CloseHandle(f);
								}
								if(SetCurrentDirectory(path)){
									delete[]path; path = NULL;
									level++;
									EnumStorage(is2);
									level--;
									SetCurrentDirectory("..");
								}
								else fputs("Could not enter (directory) - Ignoring & continuing\n",stderr);
							}
							else fputs("Could not create (directory) - Ignoring & continuing\n",stderr);
							is2->Release();
						}
						else fputs("Could not open (directory) - Ignoring & continuing\n",stderr);
					}break;
					case STGTY_STREAM:{
						IStream* is2;
						r = is->OpenStream(ss.pwcsName,NULL,STGM_READ|STGM_SHARE_EXCLUSIVE,0,&is2);
						CoTaskMemFree(ss.pwcsName); ss.pwcsName = NULL;
						if( r == S_OK ){
							HANDLE f = CreateFile(path,GENERIC_WRITE,0,NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,NULL);
							delete[]path; path = NULL;
							if(f!=INVALID_HANDLE_VALUE&&f!=0/*Fucking lying MSDN*/){
								if(ERROR_ALREADY_EXISTS==GetLastError())fputs("Already exists (file) - overwriting\n",stderr);
								else fputs("Created (file)\n",stderr);
								//This bit, although a bit more complex than 1 byte at a time, is way faster
								BYTE* p;
								ULARGE_INTEGER i={0};
								DWORD nobm = (DWORD)ss.cbSize.QuadPart, nobr, nobw;
								bool err;
								for(;;){
									p = (BYTE*)malloc( nobm ); if(p)break;
									nobm/=2; if(!nobm)break;
								}
								if(p){
									do{
										r=is2->Read(p,nobm,&nobr);
										if(r!=S_OK&&nobm!=nobr){fputs("Read error (file) - terminating this file\n",stderr);err=true;}//read less than asked and there was an error other than EOF - MSDN says that the compound file implementation of Read returns S_OK if the end of the stream was reached during the read
										nobw=0;
										r=WriteFile(f,p,nobr,&nobw,NULL);
										i.QuadPart+=nobw;
										if(!r||nobr!=nobw){fputs("Write error (file) - terminating this file\n",stderr);err=true;}
									} while (i.QuadPart<ss.cbSize.QuadPart&&!err);
									free(p); p = NULL;
								}
								//Note that for(uint i=randint();!i;i/=2); will always go through i=1 to i=0
								else{ fputs("Could not alloc even 1 byte of mem for a buffer for transferring files - please free some memory & try again - Ignoring & continuing\n",stderr); err=true;}
								if(!SetFileTime(f,&ss.ctime,&ss.atime,&ss.mtime))fputs("Could not set the creation, last modification or last access time - Ignoring & continuing\n",stderr);
								CloseHandle(f);
							}
							else fputs("Could not create (file) - Ignoring & continuing\n",stderr);
							is2->Release();
						}
						else fputs("Could not open (file) - Ignoring & continuing\n",stderr);
					}break;
					case STGTY_LOCKBYTES:
						fputs("unsupported ILockBytes object - Ignoring & continuing\n",stderr);
					break;
					case STGTY_PROPERTY:
						fputs("unsupported property storage object - Ignoring & continuing\n",stderr);
					break;
					default:
						fputs("unknown object - Ignoring & continuing\n",stderr);
					break;
				}
			}
			else fputs("Could not convert the path back to ANSI for the wimpy Win9$ - Ignoring & continuing\n",stderr);
			delete[]path; path = NULL;
		}
		else fputs("Could not alloc memory for the ANSI pathname for the wimpy Win9$ - Ignoring & continuing\n",stderr);
		CoTaskMemFree(ss.pwcsName); ss.pwcsName = NULL;
	}
	iess->Release();
}

void fixpath(char* s){
	while(*s){//*s<32 || should probably be removed on non-FAT file systems
		if( *s<32 || strchr(options.invalidchars,*s) )*s=options.fixchar;
		s++;
	}
}