unit utils;

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

{
    function gpsbabel created from old gui GPSBabelGUIDialogU.pas
}

interface

uses
  gnugettext,
  Windows, SysUtils, Classes, StdCtrls, ComCtrls,
  Registry, ShellAPI, Forms;

type
  PBoolean = ^Boolean;

function gpsbabel(const CommandLine: string; Output: TStrings;
  Fatal: PBoolean = nil; OEMStrings: Boolean = True): Boolean;

function GetShortName(const PathName: string): string;

procedure StoreProfile(const Tag: Integer; const Value: string); overload;
procedure StoreProfile(const Tag, Value: string); overload;

function ReadProfile(const Tag: Integer; const Default: string = ''): string; overload;
function ReadProfile(const Name: string; const Default: string = ''): string; overload;

function BackupProperties(Instance: TObject; Properties: TStrings; Backup: TStringList): Boolean;
procedure RestoreProperties(Instance: TObject; Backup: TStringList);

procedure FixStaticText(AComponent: TComponent);

function WinOpenFile(const AFile, AParams: string): Boolean;
procedure WinOpenURL(const AURL: string);

procedure UniWrite(Target: TStream; const Str: WideString);
procedure UniWriteLn(Target: TStream; const Str: WideString);

procedure MakeFirstTranslation(AComponent: TComponent);

function readme_html_path: string;

function HasUpDown(E: TEdit; var UpDown: TUpdown): Boolean;

procedure StoreBounds(const Name: string; AForm: TForm);
procedure RestoreBounds(const Name: string; AForm: TForm);

function CharCount(const Str: string; const Ch: Char): Integer;

implementation

uses
  common;

function GetShortName(const PathName: string): string;
var
  buffer: array[0..4095] of Char;
  len: DWORD;
begin
  len := Windows.GetShortPathName(PChar(PathName), @buffer, sizeof(buffer));
  SetString(Result, buffer, len);
end;

function gpsbabel(const CommandLine: string; Output: TStrings;
  Fatal: PBoolean; OEMStrings: Boolean): Boolean;

// bigger buffer_size speeds up conversion to screen

const
  BUFFER_SIZE = $20000;

var
  hRead, hWrite: THandle;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  sCmd: string;

  BytesRead, BytesDone: DWORD;
  buffer_string: string;
  buffer: PChar;
  Error: DWORD;
  Wait_Result: DWORD;
  s: string;
  i: Integer;

begin
  Result := False;

  // strings are released automatical
  // so we don't need a try/finally construct for our read buffer

  SetLength(buffer_string, BUFFER_SIZE);
  buffer := PChar(buffer_string);

  if (Fatal <> nil) then Fatal^ := False;

  if (Copy(CommandLine, 1, 1) = '~') then
    sCmd := System.Copy(CommandLine, 2, Length(CommandLine) - 1)
  else
    sCmd := SysUtils.Format('"%s" %s ', [gpsbabel_exe, CommandLine]);

  SecurityAttr.nLength := sizeof(TSECURITYATTRIBUTES);
  SecurityAttr.bInheritHandle := true;
  SecurityAttr.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite, @SecurityAttr, $8000) then
    raise eGPSBabelError.Create(_('Error WINAPI: Could not create "NamedPipe"!'));

  try

    if not FileExists(gpsbabel_exe) then
      raise eGPSBabelError.Create(_('"gpsbabel.exe" not found!!!'));

    FillChar (StartupInfo, Sizeof (StartupInfo), #0);

    StartupInfo.cb := Sizeof (StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := {SW_HIDE or} SW_SHOWMINNOACTIVE;
    StartupInfo.hStdInput := GetStdHandle (STD_INPUT_HANDLE);
    StartupInfo.hStdOutput:= hWrite;
    StartupInfo.hStdError := hWrite;

    FillChar(ProcessInfo, SizeOf(ProcessInfo), #0);

    if not CreateProcess(nil,
      pchar(sCmd), nil, nil, true, CREATE_NEW_CONSOLE, // dwCreationFlags,     // creation flags
      nil, nil, StartupInfo, ProcessInfo) then
    begin
      Error := GetLastError;
      raise eGPSBabelError.CreateFmt(
        _('Could not run "gpsbabel.exe" (Error %d)!'), [Error]);
    end;

    s := '';
    Error := 0;

    repeat
      Wait_Result := WaitforSingleObject(ProcessInfo.hProcess, 10);
      if PeekNamedPipe(hRead, nil, 0, nil, @BytesRead, nil) then
      begin
        if (BytesRead > 0) then
          Application.ProcessMessages;
        while (BytesRead > 0) do
        begin
          BytesDone := BytesRead;
          if (BytesDone > (BUFFER_SIZE - 1)) then BytesDone := BUFFER_SIZE - 1;
          ReadFile(hRead, buffer^, BytesDone, BytesDone, nil);
          if (BytesDone > 0) then
          begin
            buffer[BytesDone] := #0;
            if OEMStrings then
              OemToCharBuff(buffer, buffer, BytesDone);
            s := s + string(buffer);
            Dec(BytesRead, BytesDone);
          end;
        end
      end;
    until (Wait_Result = WAIT_OBJECT_0);

    if (Error = 0) then
      if not GetExitCodeProcess(ProcessInfo.hProcess, Error) then Error := 0;

    if (Error <> 0) and (Error <> 1) then
      raise eGPSBabelError.CreateFmt(_('"gpsbabel.exe" returned error 0x%x (%d)'), [Error, Error]);

    Output.Clear;
    while True do
    begin
      i := Pos(#13#13, s);
      if (i <> 0) then System.Delete(s, i, 1)
      else break;
    end;
    Output.SetText(PChar(s));

    Result := True;
    if (Fatal <> nil) then
      Fatal^ := (Error = 1);

  finally
    CloseHandle (hRead);
    CloseHandle (hWrite);
  end;
end;

procedure StoreProfile(const Tag: Integer; const Value: string);
var
  reg: TRegistry;
  str: string;
begin
  if (Tag <= 0) or (Tag > High(Profile)) then Exit;

  str := Profile[Tag];
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\GPSBabel', True) then
    begin
      reg.WriteString(str, Value);
    end;
  finally
    reg.Free;
  end;
end;

procedure StoreProfile(const Tag, Value: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\GPSBabel', True) then
    begin
      reg.WriteString(Tag, Value);
    end;
  finally
    reg.Free;
  end;
end;

function ReadProfile(const Tag: Integer; const Default: string): string; // overload;
var
  str: string;
begin
  if (Tag <= 0) or (Tag > High(Profile)) then Exit;
  str := Profile[Tag];
  Result := ReadProfile(str, Default);
end;

function ReadProfile(const Name: string; const Default: string = ''): string; // overload;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey('\SOFTWARE\GPSBabel', True) then
    begin
      try
        Result := reg.ReadString(Name);
      except
        Result := Default;
      end;
    end;
  finally
    reg.Free;
  end;
end;

function BackupProperties(Instance: TObject; Properties: TStrings; Backup: TStringList): Boolean;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    Backup.Assign(List);
  finally
    List.Free;
  end;
end;

procedure RestoreProperties(Instance: TObject; Backup: TStringList);
begin
end;

procedure FixStaticText(AComponent: TComponent);
var
  i, j: Integer;
  c: TComponent;
  s: TStaticText;
begin
  j := AComponent.ComponentCount;
  for i := 0 to j - 1 do
  begin
    c := AComponent.Components[i];
    if (c.ComponentCount > 0) then FixStaticText(c);

    if not c.InheritsFrom(TStaticText) then Continue;

    s := c as TStaticText;
    if (s.BorderStyle = sbsNone) then Continue;

    if (s.Alignment = taLeftJustify) then
      s.Caption := '   ' + s.Caption
    else if (s.Alignment = taRightJustify) then
      s.Caption := s.Caption + '  ';
  end;
end;

function WinOpenFile(const AFile, AParams: string): Boolean;
var
  p: PChar;
begin
  if (AParams = '') then
    p := nil else
    p := PChar(AParams);
  Result := (ShellExecute(0, 'open', PChar(AFile), p, nil, SW_SHOW) > 32);
end;

procedure WinOpenURL(const AURL: string);
var
  i: Integer;
  reg: TRegistry;
  cmd: string;
  prg: string;
  url: string;
begin
  url := AURL;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKeyReadOnly('Software\Classes\HTTP\Shell\Open\Command') then
    begin
      prg := reg.ReadString('');
      if (prg <> '') then
      begin
        i := Pos('%1', prg);
        if (i <> 0) then
        begin
          System.Delete(prg, i, 2);
          System.Insert(url, prg, i);
          url := '';
        end;

        if (prg[1] = '"') then
        begin
          i := Pos('"', Copy(prg, 2, Length(prg)));
          if (i = 0) then Exit;
          cmd := Copy(prg, 2, i - 1);
          Delete(prg, 1, i + 1);
          prg := Trim(prg);
          if (url <> '') then
          begin
            if (prg = '') then
              prg := URL else
              prg := prg + ' ' + URL;
          end;
          if WinOpenFile(cmd, PChar(prg)) then Exit
        end
          else
        if (Pos(' ', prg) <> 0) then
        begin
          i := Pos(' ', prg);
          cmd := Trim(Copy(prg, 1, i - 1));
          prg := Trim(Copy(prg, i + 1, Length(prg)));
          if (url <> '') then
          begin
            if (prg = '') then
              prg := URL
            else
              prg := Trim(prg) + ' ' + URL;
          end;
          if WinOpenFile(cmd, PChar(prg)) then Exit;
        end
        else
          if WinOpenFile(prg, PChar(URL)) then Exit;
      end;
    end;
  finally
    reg.Free;
  end;
  WinOpenFile(AURL, '');
end;

procedure UniWrite(Target: TStream; const Str: WideString);
const
  UniHeader: array[0..1] of Byte = ($FF, $FE);
var
  len: Integer;
begin
  if (Target.Size = 0) then Target.Write(UniHeader, SizeOf(UniHeader));
  len := Length(Str);
  if (len > 0) then
    Target.Write(PWideChar(Str)^, len * 2);
end;

procedure UniWriteLn(Target: TStream; const Str: WideString);
begin
  UniWrite(Target, Str);
  UniWrite(Target, #13#10);
end;

procedure MakeFirstTranslation(AComponent: TComponent);
var
  lang: string;
begin
// !!! TRICK !!!
  lang := GetCurrentLanguage;
  UseLanguage('en');
  TranslateComponent(AComponent);
  if (Copy(lang, 1, 2) <> 'en') then
  begin
    UseLanguage(lang);
    ReTranslateComponent(AComponent);
  end;
// !!! TRICK !!!
end;

function readme_html_path: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'gpsbabel.html';
  if FileExists(Result) then
  begin
    while (Pos('\', Result) <> 0) do
      Result[Pos('\', Result)] := '/';
    Result := 'file:///' + Result;
  end
  else
    Result := SGPSBabelURL + '/gpsbabel.html';
end;

function HasUpDown(E: TEdit; var UpDown: TUpdown): Boolean;
var
  i: Integer;
  c: TComponent;
  o: TComponent;
begin
  Result := False;
  o := E.Owner;
  for i := 0 to o.ComponentCount - 1 do
  begin
    c := o.Components[i];
    if (c is TUpDown) and (TUpDown(c).Associate = E) then
    begin
      UpDown := TUpDown(c);
      Result := True;
      Exit;
    end;
  end;
end;

procedure StoreBounds(const Name: string; AForm: TForm);
var
  str: string;
begin
  if (AForm = nil) then Exit;

  if (AForm.WindowState = wsMaximized) then str := 'Y' else str := 'N';
  str := Format('%s,%d,%d,%d,%d', [str,
    AForm.Left, AForm.Top, AForm.Width, AForm.Height]);
  StoreProfile(Name, str);
end;

procedure RestoreBounds(const Name: string; AForm: TForm);
var
  str: string;
  idx: Integer;
  lst: TStringList;
  bds: TRect;
begin
  if (AForm = nil) then Exit;

  str := ReadProfile(Name);
  if (str = '') then Exit;

  lst := TStringList.Create;
  try
    lst.Sorted := False;
    lst.Duplicates := dupAccept;
    lst.CommaText := str;
    try
      AForm.Position := poDesigned;

      if (StrUpper(PChar(lst[0])) = 'Y') then AForm.WindowState := wsMaximized
      else AForm.WindowState := wsNormal;

      bds.Left := StrToInt(lst[1]);
      bds.Top := StrToInt(lst[2]);
      bds.Right := bds.Left + StrToInt(lst[3]);
      bds.Bottom := bds.Top + StrToInt(lst[4]);

      AForm.BoundsRect := bds;
      
    except
      on E: Exception do;
    end;
  finally
    lst.Free;
  end;
end;

function CharCount(const Str: string; const Ch: Char): Integer;
var
  i, len: Integer;
begin
  Result := 0;
  len := Length(Str);
  for i := 1 to len do
    if (Str[i] = Ch) then
      Inc(Result);
end;

var
  hMutex: THandle;

initialization

  // Flag for InnoSetup
  hMutex := CreateMutex(nil, True, 'GPSBabelGUI_mutex');

finalization

  if (hMutex <> 0) then
    CloseHandle(hMutex);

end.
