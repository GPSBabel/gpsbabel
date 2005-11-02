unit common;

{
    Copyright (C) 2005 Olaf Klein, o.k.klein@t-online.de

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
  Windows, SysUtils, Classes, Messages;

resourcestring
  SGPSBabelURL = 'http://www.gpsbabel.org';
  SGPSBabelTitle = 'GPSBabelGUI-2';

var
  SGPSBabelGUIVersion: string;
  CFixedFileinfo: TVSFixedFileInfo;

const
  WM_STARTUP = WM_USER + 1;

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

const
  Profile: array[0..10] of string =
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
   SREG_SOURCE_FILE);

type
  PFileInfo = ^TFileInfo;
  TFileInfo = record
    Descr: string;
    Ext:   string;
    internal: string;
    Capas: Integer;
  end;

type
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

var
  gpsbabel_exe: string;

implementation

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
      0: internal := StrPas(cin);
      1: scaps := StrPas(cin);
      2: name := StrPas(cin);
      3: ext := StrPas(cin);
    else
      begin
        comment := StrPas(cin);
        if (Length(comment) = 0) or (Length(name) = 0) then break;
        
        if (comment[1] = '?') then break;
        
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
  
initialization

  gpsbabel_exe := SysUtils.ExtractFilePath(ParamStr(0)) + 'gpsbabel.exe';
  SGPSBabelGUIVersion := GetFileVersion(ParamStr(0));

end.
