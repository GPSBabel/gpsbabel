unit utils;

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

{
    function gpsbabel created from old gui GPSBabelGUIDialogU.pas
}

interface

uses
  gnugettext,
  Windows, SysUtils, Classes, Registry, ShellAPI;

type
  PBoolean = ^Boolean;

function gpsbabel(const CommandLine: string; Output: TStrings;
  Fatal: PBoolean = nil; OEMStrings: Boolean = True): Boolean;

function GetShortName(const PathName: string): string;
procedure StoreProfile(const Tag: Integer; const Value: string);
function ReadProfile(const Tag: Integer; const Default: string = ''): string; overload;
function ReadProfile(const Name: string; const Default: string = ''): string; overload;

function BackupProperties(Instance: TObject; Properties: TStrings; Backup: TStringList): Boolean;
procedure RestoreProperties(Instance: TObject; Backup: TStringList);

procedure FixStaticText(AComponent: TComponent);

procedure WinOpenFile(const Name: string);

implementation

uses
  Forms,
  StdCtrls,
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
    sCmd := SysUtils.Format('%s %s ', [gpsbabel_exe, CommandLine]);

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
    StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
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

    repeat
      Wait_Result := WaitforSingleObject(ProcessInfo.hProcess, 10);
      if PeekNamedPipe(hRead, nil, 0, nil, @BytesRead, nil) then
      begin
        if (BytesRead > 0) then Application.ProcessMessages;
        while (BytesRead > 0) do
        begin
          ReadFile(hRead, buffer^, BUFFER_SIZE - 1, BytesDone, nil);
          if (BytesDone > 0) then
          begin
            buffer[BytesDone] := #0;
            if OEMStrings then
              OemToCharBuff(buffer, buffer, BytesDone);
            s := s + string(buffer);
            Dec(BytesRead, BytesDone);
          end;
        end;
      end;
    until (Wait_Result = WAIT_OBJECT_0);

    if not GetExitCodeProcess(ProcessInfo.hProcess, Error) then Error := 0;

    if (Error <> 0) and (Error <> 1) then
      raise eGPSBabelError.CreateFmt(_('"gpsbabel.exe" returned error 0x%x (%d)'), [Error, Error]);

    Output.Clear;
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

procedure WinOpenFile(const Name: string);
begin
  ShellExecute(0, 'open', PChar(Name), nil, '', 0);
end;

end.
