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

interface

uses
  gnugettextD4,
  Windows, SysUtils, Classes, Registry;

function gpsbabel(const CommandLine: string; Output: TStrings): Boolean;

function GetShortName(const PathName: string): string;
procedure StoreProfile(const Tag: Integer; const Value: string);
function ReadProfile(const Tag: Integer): string;

function BackupProperties(Instance: TObject; Properties: TStrings; Backup: TStringList): Boolean;
procedure RestoreProperties(Instance: TObject; Backup: TStringList);

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

function gpsbabel(const CommandLine: string; Output: TStrings): Boolean;
var
  hRead, hWrite: THandle;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  StartupInfo: TStartupInfo;
  sCmd: string;

  BytesRead, BytesDone: DWORD;
  buffer: packed array[0..512] of Char;
  Error: DWORD;
  s: string;

begin
  Result := False;
   
  sCmd := SysUtils.Format('%s %s ', [gpsbabel_exe, CommandLine]);

{  i := WinExec(PChar(sCmd), SW_SHOWNORMAL);
  if (i <> 33) then
  begin
    MessageBox(0, 'There was an error.', 'Uhps', MB_OK);
    Exit;
  end;
}
  SecurityAttr.nLength := sizeof (TSECURITYATTRIBUTES);
  SecurityAttr.bInheritHandle := true;
  SecurityAttr.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite, @SecurityAttr, 0) then
    raise eGPSBabelError.Create('Konnte "NamedPipe" nicht anlegen!');

  try

    if not FileExists(gpsbabel_exe) then
      raise eGPSBabelError.Create('gpsbabel.exe wurde nicht gefunden!');

    FillChar (StartupInfo, Sizeof (StartupInfo), #0);

    StartupInfo.cb := Sizeof (StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
    StartupInfo.hStdInput := GetStdHandle (STD_INPUT_HANDLE);
    StartupInfo.hStdOutput:= hWrite;
    StartupInfo.hStdError := hWrite;

    FillChar(ProcessInfo, SizeOf(ProcessInfo), #0);

    if not CreateProcess (
      nil,                // lpApplicationName    // pointer to name of executable module
      // sCmd includes both the exec name and the command line parms in this call
      pchar (sCmd),       // lpCommandLine,       // pointer to command line string
      nil,                // lpProcessAttributes, // pointer to process security attributes
      nil,                // lpThreadAttributes,  // pointer to thread security attributes
      true,               // bInheritHandles,     // handle inheritance flag
      CREATE_NEW_CONSOLE, // dwCreationFlags,     // creation flags
      nil,                // lpEnvironment,       // pointer to new environment block
      nil,                // lpCurrentDirectory,  // pointer to current directory name
      StartupInfo,        // lpStartupInfo,       // pointer to STARTUPINFO
      ProcessInfo)        // lpProcessInformation // pointer to PROCESS_INFORMATION
      then
    begin
      Error := GetLastError;
      raise eGPSBabelError.CreateFmt(
        'gpsbabel.exe konnte nicht gestartet werden (Fehler %d).', [Error]);
    end;

    while (WaitforSingleObject (ProcessInfo.hProcess, 0)) <> WAIT_OBJECT_0 do sleep(100);

    if not GetExitCodeProcess(ProcessInfo.hProcess, Error) then Error := 0;

    if ((Error <> 0) and (Error <> 1)) then
      raise eGPSBabelError.CreateFmt('Schade, "gpsbabel.exe" verlies uns mit Fehler 0x%x (%d)', [Error, Error]);

    s := '';

    PeekNamedPipe(hRead, nil, 0, nil, @BytesRead, nil);

    while (BytesRead > 0) do
    begin
      ReadFile(hRead, Buffer, SizeOf(buffer)-1, BytesDone, nil);
      buffer[BytesDone] := #0;
      s := s + string(buffer);

      Dec(BytesRead, BytesDone);
    end;

    Output.Clear;
    Output.SetText(PChar(s));

    Result := True;

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

function ReadProfile(const Tag: Integer): string;
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
      try
        Result := reg.ReadString(str);
      except
        Result := '';
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

end.
