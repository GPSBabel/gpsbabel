unit common;

{
    Copyright (C) 2005-2007 Olaf Klein, o.b.klein@gpsbabel.org

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
  Windows, SysUtils, Classes, Messages, Controls, StdCtrls;

const
  OTypes: array[0..6] of PChar =
    ('unknown', 'integer', 'float', 'string', 'boolean', 'file', 'outfile');

  gpsbabel_knows_inifile: Boolean = False;
//gpsbabel_ini: TInifile = nil;
  
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
  
const
  Profile: array[0..12] of string =
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
   'Main:Layout');

const
  GPSBabel_Domain = 'gpsbabel';

type
  PFileInfo = ^TFileInfo;
  TFileInfo = record
    Descr: string;
    Ext:   string;
    internal: string;
    Capas: Integer;
    url: PChar;
  end;

type
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
    edit:   TControl;
    dir:    Byte;        // 1 = only in; 2 = only out
  end;
  POption = ^TOption;

type
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
    function FormatOpts(const Descr: string): TStringList;
    function HasFormatOpts(const Format: string): Boolean;
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
    function CanReadAny(Index: Integer): Boolean;
    function CanWriteAny(Index: Integer): Boolean;
    function GetDescr(Index: Integer): string;
    function GetExt(const Descr: string): string;
    function GetCaps(const Descr: string): Integer;
    function GetName(const Descr: string): string;
    function IsDevice(Index: Integer): Boolean;
    function IsFile(Index: Integer): Boolean;
  property
    List: TStrings read GetList write SetList;
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
begin
  s := FCaps.GetName(Descr);
  if (s <> '') and Self.Find(s, i) then
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
  info: PFileInfo;

begin
  StrPCopy(buff, Line);
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
        if (StrIComp(cin, 'option') = 0) then
          Exit
        else
          internal := StrPas(cin);
      1:
        scaps := StrPas(cin);
      2:
          name := StrPas(cin);
      3:
        ext := StrPas(cin);
    else
      begin
        comment := StrPas(cin);
        if (Length(comment) = 0) or (Length(name) = 0) then break;
        
//      if (comment[1] = '?') then break;
        
        caps := 0;
        for i := 1 to Length(scaps) do
          if (scaps[i] <> '-') then caps := caps or (1 shl (i - 1));

         New(info);
         info.Descr := comment;
         info.Ext := ext;
         info.internal := internal;
         info.Capas := caps;

         i := SELF.Add(name);
         SELF.PutObject(i, Pointer(info));

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

function TCapabilities.CanReadAny(Index: Integer): Boolean;
var
  caps: Integer;
begin
  caps := PFileInfo(SELF.Objects[Index]).Capas;
  Result := caps and (1 or 4 or 16) <> 0;
end;

function TCapabilities.CanWriteAny(Index: Integer): Boolean;
var
  caps: Integer;
begin
  caps := PFileInfo(SELF.Objects[Index]).Capas;
  Result := caps and (2 or 8 or 32) <> 0;
end;

function TCapabilities.GetCaps(const Descr: string): Integer;
var
  info: PFileInfo;
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    info := PFileInfo(Objects[i]);
    if (AnsiCompareText(info.Descr, Descr) = 0) then
    begin
      Result := info.Capas;
      Exit;
    end;
  end;
  Result := 0;
end;

function TCapabilities.GetDescr(Index: Integer): string;
var
  info: PFileInfo;
begin
  info := PFileInfo(Objects[Index]);
  Result := info.Descr;
end;

function TCapabilities.GetExt(const Descr: string): string;
var
  i: Integer;
  info: PFileInfo;
begin
  for i := 0 to Count - 1 do
  begin
    info := PFileInfo(Objects[i]);
    if (AnsiCompareText(info.Descr, Descr) = 0) then
    begin
      Result := info.Ext;
      Exit;
    end;
  end;
  Result := '.*';
end;

function TCapabilities.GetList: TStrings;
begin
  Result := TStringList.Create;
end;

function TCapabilities.GetName(const Descr: string): string;
var
  i: Integer;
  info: PFileInfo;
begin
  for i := 0 to Count - 1 do
  begin
    info := PFileInfo(Objects[i]);
    if (AnsiCompareText(info.Descr, Descr) = 0) then
    begin
      Result := SELF[i];
      Exit;
    end;
  end;
  Result := 'unknown';
end;

function TCapabilities.IsDevice(Index: Integer): Boolean;
var
  info: PFileInfo;
begin
  info := PFileInfo(Objects[Index]);
  Result := (AnsiCompareText(info.Internal, 'serial') = 0);
end;

function TCapabilities.IsFile(Index: Integer): Boolean;
var
  info: PFileInfo;
  name: string;
begin
  info := PFileInfo(Objects[Index]);
  Result := (AnsiCompareText(info.Internal, 'file') = 0);
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

end.
