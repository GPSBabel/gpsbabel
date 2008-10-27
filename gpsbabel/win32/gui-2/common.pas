unit common;

{
    Copyright (C) 2005-2008 Olaf Klein, o.b.klein@gpsbabel.org

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
}

interface

uses
  Windows, SysUtils, Classes, Messages, Controls, StdCtrls, ExtCtrls;

const
  OTypes: array[0..6] of PChar =
    ('unknown', 'integer', 'float', 'string', 'boolean', 'file', 'outfile');

  gpsbabel_knows_inifile: Boolean = False;
//gpsbabel_ini: TInifile = nil;
  gpsbabel_knows_swap_filter: Boolean = False;
  
resourcestring
  SGPSBabelURL =         'http://www.gpsbabel.org';
  SGPSBabelTitle =       'GPSBabelGUI-2';

const
  SGPSBabelIniFilename = 'gpsbabel.ini';
  SGPSBabelExeFilename = 'gpsbabel.exe';

var
  gpsbabel_exe: string;
  gpsbabel_version: string;        // 1.101.010-beta...
  gpsbabel_vfmt: string;           // 001.101.010
  gpsbabel_minor, gpsbabel_major, gpsbabel_release: Integer;
  SGPSBabelGUIVersion: string;
  CFixedFileinfo: TVSFixedFileInfo;

const
  WM_STARTUP = WM_USER + 1;
  WM_OPTIONS_CHANGED = WM_USER + 2;

const
  MAX_NO_OF_SERIAL_PORTS = 8;

const
  SREG_TARGET_DIR = 'Target:Directory';
  SREG_SOURCE_DIR = 'Source:Directory';

  SREG_TARGET_DEV = 'Target:Device';
  SREG_SOURCE_DEV = 'Source:Device';

  SREG_SOURCE_FMT = 'Source:FileFormat';
  SREG_TARGET_FMT = 'Target:FileFormat';

  SREG_SOURCE_SER = 'Source:DeviceFormat';
  SREG_TARGET_SER = 'Target:DeviceFormat';

  SREG_TARGET_FILE = 'Target:File';
  SREG_SOURCE_FILE = 'Source:File';

  SREG_GLOBAL_LANG = 'Global:Language';

  SREG_MAIN_LAYOUT = 'Main:Layout';
  SREG_MAIN_WAYPOINTS = 'Main:Waypoints';
  SREG_MAIN_ROUTES    = 'Main:Routes';
  SREG_MAIN_TRACKS    = 'Main:Tracks';

const
  Profile: array[0..15] of string =
  ('?',
   SREG_SOURCE_DIR,
   SREG_SOURCE_FMT,
   SREG_SOURCE_DEV,
   SREG_SOURCE_SER,
   SREG_TARGET_DIR,
   SREG_TARGET_FMT,
   SREG_TARGET_DEV,
   SREG_TARGET_SER,
   SREG_TARGET_FILE,
   SREG_SOURCE_FILE,
   SREG_GLOBAL_LANG,
   SREG_MAIN_LAYOUT,
   SREG_MAIN_WAYPOINTS,
   SREG_MAIN_ROUTES,
   SREG_MAIN_TRACKS
   );

const
  GPSBabel_Domain = 'gpsbabel';

type
  TCapability = class
  protected
    FName : String;
    FDescription : string;
    FExt:   string;
    internal: string;
    Furl: PChar;
  protected
    function CanReadAny : Boolean;
    function CanWriteAny : Boolean;
    function FIsDevice : Boolean;
    function FIsFile : Boolean;
  public
    Capas: Integer;
    property Name : String read FName;
    property Description : String read FDescription write FDescription;
    property Ext : String read FExt;
    property ReadAny : Boolean read CanReadAny;
    property WriteAny : Boolean read CanWriteAny;
    property IsDevice : Boolean read FIsDevice;
    property IsFile : Boolean read FIsFile;
  end;

type
  TOptionEdit = class;

  TOption = record
    format: string;
    name:   string;
    hint:   string;
    defname: string;
    otype:  Byte;
    def:    PChar;       // default value from gpsbabel or ini-file
    gbdef:  PChar;       // default value from gpsbabel
    min:    PChar;
    max:    PChar;
    chb:    TCheckBox;
    edit:   TOptionEdit;
    dir:    Byte;        // 1 = only in; 2 = only out
  end;
  POption = ^TOption;

  TOptionEdit = class(TObject)
  protected
    FOption : POption;
    function GetValue : String; virtual; abstract;
    procedure SetValue(Value : String); virtual; abstract;
    function GetEnabled : Boolean; virtual; abstract;
    procedure SetEnabled(Value : Boolean); virtual; abstract;
  public
    property Value : String read GetValue write SetValue;
    property Option : POption read FOption;
    property Enabled : Boolean read GetEnabled write SetEnabled;
  end;

  TCapabilities = class;
  
  TOptions = class(TStringList)
  private
    FCaps: TCapabilities;
    function GetList: TStrings;
    procedure SetList(const Value: TStrings);
  protected
  public
    constructor Create(ACapabilities: TCapabilities);
    procedure AddOptionLine(const ALine: string);
    procedure DebugGetHints(List: TStringList);
    function FormatOpts(const Descr: string): TStringList; overload;
    function FormatOpts(cap : TCapability): TStringList; overload;
    function HasFormatOpts(const Format: string): Boolean; overload;
    function HasFormatOpts(cap : TCapability): Boolean; overload;
  property
    List: TStrings read GetList write SetList;
  end;

  TCapabilities = class(TStringList)
  private
    FList: TStrings;
    procedure AddFormat(const Line: string);
    function GetList: TStrings;
    procedure SetList(const Value: TStrings);
  public
    function GetCapabilityByName(const Descr: string): TCapability;
    function GetCapability(Index: Integer) : TCapability;

    property List: TStrings read GetList write SetList;
    property Capability[Index : Integer] : TCapability read GetCapability;
  end;

type
  eGPSBabelError = class(Exception);

function atoi(str: PChar): Integer;

implementation

function atoi(str: PChar): Integer;
begin
  Result := 0;
  while (str^ <> #0) do
  begin
    if ((str^ < '0') or (str^ > '9')) then Break;
    Result := (Result * 10) + (Ord(str^) - Ord('0'));
    str := str + 1;
  end;
end;

function GetFileVersion(const Filename: string): string;
var
  buff: PChar;
  hdl: DWORD;
  len: DWORD;
  sub: PChar;
  sublen: UINT;
  fix: PVSFixedFileInfo;
  i:   Integer;
begin
  Result := '?.?';

  FillChar(CFixedFileinfo, SizeOf(CFixedFileinfo), #0);

  len := GetFileVersionInfoSize(PChar(Filename), hdl);
  if not(len > 0) then exit;

  GetMem(buff, len);
  try

    if not GetFileVersionInfo(PChar(FileName), 0, len, buff) then Exit;

    fix := Pointer(buff);
    i := len - SizeOf(fix^);
    while (i > 0) do
    begin
      Dec(i);
      if (fix.dwSignature = $feef04bd) then
      begin
        CFixedFileinfo := fix^;
        Break;
      end;
      PChar(fix) := PChar(fix) + 1; 
    end;

    if not VerQueryValue(buff, PChar('\\StringFileInfo\\040904E4\\FileVersion'),
      Pointer(sub), sublen) then Exit;
    if not(sublen > 0) then Exit;
    Result := string(sub);
  finally
    FreeMem(buff);
  end;
end;

{ TOptions }

constructor TOptions.Create(ACapabilities: TCapabilities);
begin
  inherited Create;
  FCaps := ACapabilities;
  Sorted := False;
end;

procedure TOptions.AddOptionLine(const ALine: string);
var
  buff: array[0..1023] of Char;
  cin, cend: PChar;
  index: Integer;
  opt, opt2: POption;
  list: TStringList;
  i: Integer;
  s: string;
begin
  StrPCopy(buff, ALine);
  StrCat(buff, #9);

  cin := @buff;
  index := 0;
  while (true) do
  begin
    cend := StrScan(cin, #9);
    if (cend = nil) then break;
    cend^ := #0;

    case index of
      0:
        if (StrIComp(cin, 'option') <> 0) then
          Exit else
        begin
          New(opt);
          FillChar(opt^, SizeOf(opt^), #0);
        end;
      1:
        opt.format := string(cin);
      2:
        opt.name := string(cin);
      3:
        opt.hint := string(cin);
      4:
        for i := 0 to high(OTypes) do
          if (StrIComp(cin, OTypes[i]) = 0) then
          begin
            opt.otype := i;
            Break;
          end;
      5:
        if (cin^ <> #0) then
        begin
          opt.gbdef := StrNew(cin);
          if (opt.def = nil) then
            opt.def := opt.gbdef;
        end;
      6:
        if (cin^ <> #0) then
          opt.min := StrNew(cin);
      7:
        if (cin^ <> #0) then
          opt.max := StrNew(cin);
    end;

    index := index + 1;
    cin := cend + 1;
  end;

  if (opt.name = 'snlen') and (opt.gbdef = nil) then
  begin
    opt.gbdef := StrNew('10');
    opt.def := opt.gbdef;
  end;

  opt.dir := 3;  // in and out
  opt.defname := opt.name;

  index := Self.IndexOf(opt.format);
  if (index >= 0) then
    list := TStringList(Self.Objects[index])
  else begin
    list := TStringList.Create;
    list.Sorted := True;
    Self.AddObject(opt.format, list);
  end;
  list.AddObject(opt.name, Pointer(opt));
  if (opt.format = 'xcsv') then
  begin
    if (opt.name = 'style') then
    begin
      opt.dir := 1;
      New(opt2);
      opt2^ := opt^;
      opt2.name := 'style_out';
      opt2.dir := 2;
      list.AddObject(opt2.name, Pointer(opt2));
    end;
  end;
end;

procedure TOptions.DebugGetHints(List: TStringList);
var
  i, j, k: Integer;
  l: TStrings;
  o: POption;
begin
  List.Clear;
  List.Sorted := True;
  for i := 0 to Count - 1 do
  begin
    l := Pointer(Objects[i]);
    for j := 0 to l.Count - 1 do
    begin
      o := Pointer(l.Objects[j]);
      k := List.IndexOf(o.hint);
      if (k < 0) then
        List.Add(o.hint);
    end;
  end;
end;

function TOptions.FormatOpts(const Descr: string): TStringList;
var
  i: Integer;
  s: string;
  cap : TCapability;
begin
  cap:=FCaps.GetCapabilityByName(Descr);
  result:=FormatOpts(cap);
end;

function TOptions.FormatOpts(cap : TCapability): TStringList;
var
  i: Integer;
  s: string;
begin
  if (Assigned(Cap)) and Self.Find(Cap.Name, i) then
    Result := TStringList(Self.Objects[i])
  else
    Result := nil;
end;

function TOptions.GetList: TStrings;
begin
  Result := Self;
end;

function TOptions.HasFormatOpts(const Format: string): Boolean;
begin
  Result := (FormatOpts(Format) <> nil);
end;

function TOptions.HasFormatOpts(cap : TCapability): Boolean;
begin
  Result := (FormatOpts(cap) <> nil);
end;

procedure TOptions.SetList(const Value: TStrings);
var
  i: Integer;
begin
  Clear;
  for i := 0 to Value.Count - 1 do
    AddOptionLine(Value[i]);
  Sorted := True;
end;

{ TCapabilities }

procedure TCapabilities.AddFormat(const Line: string);
var
  index: Integer;
  buff: array[0..1023] of Char;
  cin, cend: PChar;
  i: Integer;
  scaps: string;
  ext: string;
  comment: string;
  name: string;
  internal: string;
  caps: Integer;
  info: TCapability;

begin
  StrPCopy(buff, Line);
  StrCat(buff, #9);
  //OutputDebugString(buff);

  cin := @buff;
  index := 0;

  while (true) do
  begin
    cend := StrScan(cin, #9);
    if (cend = nil) then break;
    cend^ := #0;

    case index of
      0:
        if (StrIComp(cin, 'option') = 0) then
          Exit
        else
          internal := StrPas(cin);
      1:
        scaps := StrPas(cin);
      2:
        name := StrPas(cin);
      3:
        begin
          ext := StrPas(cin);
          while (Pos('.', ext) <> 0) do
            System.Delete(ext, Pos('.', ext), 1);
        end
    else begin
      comment := StrPas(cin);
      if (Length(comment) = 0) or (Length(name) = 0) then break;

//    if (comment[1] = '?') then break;

      caps := 0;
      for i := 1 to Length(scaps) do
        if (scaps[i] <> '-') then caps := caps or (1 shl (i - 1));

        info:=TCapability.Create;
        info.FName:=name;
        info.FDescription := comment;
        info.FExt := ext;
        info.internal := internal;
        info.Capas := caps;

        SELF.AddObject(name,info);

        if (name = 'garmin_txt') then
        begin
          gpsbabel_knows_inifile := True;
          // add -p "" to command-line
        end
        else if (name = 'xcsv') then
          info.internal := 'file';
        break;
      end;
    end;

    index := index + 1;
    cin := cend + 1;
  end;
end;

function TCapability.FIsDevice : Boolean;
begin
  Result := (AnsiCompareText(Internal, 'serial') = 0);
end;

function TCapability.FIsFile : Boolean;
begin
  Result := (AnsiCompareText(Internal, 'file') = 0);
end;

function TCapability.CanReadAny : Boolean;
begin
  Result := capas and (1 or 4 or 16) <> 0;
end;

function TCapability.CanWriteAny : Boolean;
begin
  Result := capas and (2 or 8 or 32) <> 0;
end;

function TCapabilities.GetList: TStrings;
begin
  Result := TStringList.Create;
end;

function TCapabilities.GetCapabilityByName(const Descr: string): TCapability;
var
  i: Integer;
  info: TCapability;
begin
  for i := 0 to Count - 1 do
  begin
    info := TCapability(Objects[i]);
    if (AnsiCompareText(info.Description, Descr) = 0) then
    begin
      Result := info;
      Exit;
    end;
  end;
  Result := nil;
end;

function TCapabilities.GetCapability(Index: Integer) : TCapability;
begin
  Result := TCapability(Objects[Index]);
end;


procedure TCapabilities.SetList(const Value: TStrings);
var
  i: Integer;
  s: string;
begin
  Clear;
  for i := 0 to Value.Count - 1 do
  begin
    s := Value.Strings[i];
    AddFormat(s);
  end;
end;

(*
function Open_gpsbabel_ini(): TInifile;
var
  s: string;
begin
  s := SysUtils.ExpandFileName(SGPSBabelIniFilename);
  if not(SysUtils.FileExists(s)) then
    s := SysUtils.ExtractFilePath(ParamStr(0)) + SGPSBabelIniFilename;
  if not(SysUtils.FileExists(s)) then
    Result := TIniFile.Create(SGPSBabelIniFilename)
  else
    Result := TIniFile.Create(s)
end;
*)

initialization

  gpsbabel_exe := SysUtils.ExtractFilePath(ParamStr(0)) + SGPSBabelExeFilename;
  SGPSBabelGUIVersion := GetFileVersion(ParamStr(0));
//gpsbabel_ini := Open_gpsbabel_ini();
  DecimalSeparator := '.';

end.
