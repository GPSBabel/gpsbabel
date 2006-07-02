unit VersionInfo;

interface

uses
	SysUtils, Windows;

const
	vsComments          = 'Comments';
	vsCompanyName       = 'CompanyName';
	vsFileDescription   = 'FileDescription';
	vsFileVersion       = 'FileVersion';
	vsInternalName      = 'InternalName';
	vsLegalCopyright    = 'LegalCopyright';
	vsLegalTrademarks   = 'LegalTrademarks';
	vsOriginalFilename  = 'OriginalFilename';
	vsPrivateBuild      = 'PrivateBuild';
	vsProductName       = 'ProductName';
	vsProductVersion    = 'ProductVersion';
	vsSpecialBuild      = 'SpecialBuild';

type
	TFileInfo = (TFileVersion, TFileVersionLong, TProductVersion, TProductVersionLong);

function GetVersionString (const vsKey: string): string;
function GetFileVersion (const fInfo: TFileInfo): string;

implementation

var
	VerInfoPresent: Boolean; // True if "pVersionBuffer" contains valid information
	pVersionBuffer: Pointer; // The file information is stored in this location

function SwapLong (L: LongInt): LongInt; assembler;
asm
	rol eax, 16;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// ex: GetVersionString (vsCompanyName); returns "Down East Engineering"
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function GetVersionString (const vsKey: string): string;
var
	KeyPath: array [0..255] of char;
	p: pointer;
	Len: cardinal;
begin
	Result := '';
	if VerInfoPresent then
		if VerQueryValue (pVersionBuffer, '\VarFileInfo\Translation', p, Len) then begin
			StrLFmt (KeyPath, 255, '\StringFileInfo\%.8x\%s', [SwapLong (LongInt (p^)), vsKey]);
			if VerQueryValue (pVersionBuffer, KeyPath, p, Len) then
				Result := strPas (PChar (p));
		end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// GetFileVersion (TFileVersionLong); returns "1.0.0.0"
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function GetFileVersion (const fInfo: TFileInfo): string;
var
	Len: cardinal;
	pVerInfo: PVSFixedFileInfo;
begin
	Result := '';
	if VerInfoPresent then
		if VerQueryValue (pVersionBuffer, '\', Pointer (pVerInfo), Len) then
			case fInfo of
				TFileVersion:
					with pVerInfo^ do
						Result := Format('%d.%d',
							[dwFileVersionMS shr 16, dwFileVersionMS and $FFFF]);
				TFileVersionLong:
					with pVerInfo^ do
						Result := Format('%d.%d.%d.%d',
							[dwFileVersionMS shr 16, dwFileVersionMS and $FFFF,
							 dwFileVersionLS shr 16, dwFileVersionLS and $FFFF]);
			end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// Here is where the information is actually read from the EXE file.
// Data is read once, when the unit is initialized, and stored in 'pVersionBuffer'
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
var
	fname: array [0..MAX_PATH] of char;
	bufsize: integer;
	pVersionHandle: cardinal;

initialization
  StrPLCopy (fname, ParamStr(0), MAX_PATH); // fully qualified Application.ExeName
  bufsize := GetFileVersionInfoSize (fname, pVersionHandle);
  GetMem (pVersionBuffer, bufsize);
  VerInfoPresent := (bufsize > 0) and GetFileVersionInfo (fname, 0, bufsize, pVersionBuffer);

finalization
  FreeMem (pVersionBuffer, bufsize);
end.

