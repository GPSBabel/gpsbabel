{

    Copyright (C) 2002 Josh M. McKee, mrsnazz@users.sourceforge.net

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
    1.0.0   JMc   First release
    1.0.1   JMc   - Switched to using AddFormat for populating the formats table
                  - Updated formats table to include currently supported formats
                  - Switched to using CreateProcess rather than WinExec, so that
                    we can display data from stderr to the user.
    1.0.2   JMc   - Added LoadFormats to call the new -^ switch, to dynamically
                    load the supported formats from gpsbabel.exe.
}

unit gpsbabelfront_mainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TFormat = record
    sType:string; // type to be passed to GPSBabel
    sExt:string;  // default file extension
    sDesc:string; // description of format
  end;

  TFormGPSBabelFront = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    comboInput: TComboBox;
    editInput: TEdit;
    editOutput: TEdit;
    comboOutput: TComboBox;
    btnProcess: TButton;
    cbIgnoreShort: TCheckBox;
    Bevel1: TBevel;
    btnInput: TButton;
    btnOutput: TButton;
    OpenDialogInput: TOpenDialog;
    SaveDialogOutput: TSaveDialog;
    Label4: TLabel;
    Label5: TLabel;
    memoStdErr: TMemo;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnInputClick(Sender: TObject);
    procedure btnOutputClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
  public
    formats:array of TFormat;
    nFormatCount:integer;

    procedure LoadFormats;
    procedure AddFormat(sType,sExt,sDesc:string);

    procedure PopulateCombos;
    procedure PopulateDialogs;
  end;

var
  FormGPSBabelFront: TFormGPSBabelFront;

implementation

{$R *.dfm}

procedure TFormGPSBabelFront.AddFormat(sType,sExt,sDesc:string);
begin
  SetLength(formats,nFormatCount+1);

  formats[nFormatCount].sType := sType;
  formats[nFormatCount].sExt := sExt;
  formats[nFormatCount].sDesc := sDesc;

  inc(nFormatCount);
end;

procedure TFormGPSBabelFront.PopulateCombos;
var
  i:integer;
begin
  for i:=0 to nFormatCount-1 do begin
    comboInput.items.add(formats[i].sDesc);
    comboOutput.items.add(formats[i].sDesc);
  end;
end;

procedure TFormGPSBabelFront.PopulateDialogs;
var
  i:integer;
begin
  OpenDialogInput.Filter := '';
  SaveDialogOutput.Filter := '';
  for i:=0 to nFormatCount-1 do begin
    if (formats[i].sExt<>'') then begin
      OpenDialogInput.Filter := OpenDialogInput.Filter + formats[i].sDesc + ' (*.' +
        formats[i].sExt + ')|*.' + uppercase(formats[i].sExt) + '|';

      SaveDialogOutput.Filter := SaveDialogOutput.Filter + formats[i].sDesc + ' (*.' +
        formats[i].sExt + ')|*.' + uppercase(formats[i].sExt) + '|';
    end;
  end;

  OpenDialogInput.Filter := OpenDialogInput.Filter + 'All files (*.*)|*.*';
  SaveDialogOutput.Filter := SaveDialogOutput.Filter + 'All files (*.*)|*.*';
end;

procedure TFormGPSBabelFront.FormCreate(Sender: TObject);
begin
  nFormatCount := 0;

  // load formats from GPSBabel.exe
  LoadFormats;

  if nFormatCount = 0 then begin
    ShowMessage('Unable to load format list from GPSBabel.exe. Default format list is being used instead.');

    // add the default formats
    AddFormat('geo','loc','Geocaching.com .loc');
    AddFormat('gpsman','','GPSman');
    AddFormat('gpx','gpx','GPX XML');
    AddFormat('magellan','','Magellan protocol');
    AddFormat('mapsend','','Magellan Mapsend');
    AddFormat('pcx','pcx','Garmin PCX5');
    AddFormat('mapsource','','Garmin Mapsource');
    AddFormat('gpsutil','','gpsutil');
    AddFormat('tiger','','U.S. Census Bureau Tiger Mapping Service');
    AddFormat('csv','csv','Comma seperated values');
    AddFormat('xmap','','Delorme Topo USA4/XMap Conduit');
    AddFormat('dna','dna','Navitrak DNA marker format');
    AddFormat('psp','psp','MS PocketStreets 2002 Pushpin');
    AddFormat('cetus','','Cetus for Palm/OS');
    AddFormat('gpspilot','','GPSPilot Tracker for Palm/OS');
    AddFormat('magnav','','Magellan NAV Companion for PalmOS');
    AddFormat('garmin','','Garmin serial protocol');
    AddFormat('mxf','mxf','MapTech Exchange Format');
    AddFormat('holux','wpo','Holux (gm-100) .wpo Format');
    AddFormat('ozi','ozi','OziExplorer Waypoint');
    AddFormat('tpg','tpg','National Geographic Topo .tpg');
    AddFormat('tmpro','tmpro','TopoMapPro Places File');
  end;

  // Set up the dropdown lists and open/save dialog filters using the formats
  PopulateCombos;
  PopulateDialogs;
end;

procedure TFormGPSBabelFront.btnInputClick(Sender: TObject);
var
  sExt:string;
  i:integer;
begin
  if opendialoginput.Execute then begin
    editInput.Text := opendialoginput.filename;
    sExt := uppercase(ExtractFileExt(editInput.text));
    for i := 0 to nFormatCount-1 do begin
      if '.' + uppercase(formats[i].sExt) = sExt then comboInput.ItemIndex := i;
    end;
  end;
end;

procedure TFormGPSBabelFront.btnOutputClick(Sender: TObject);
var
  sExt:string;
  i:integer;
begin
  if savedialogoutput.Execute then begin
    editOutput.Text := savedialogoutput.filename;
    sExt := uppercase(ExtractFileExt(editOutput.text));
    for i := 0 to nFormatCount-1 do begin
      if '.' + uppercase(formats[i].sExt) = sExt then comboOutput.ItemIndex := i;
    end;
  end;
end;

procedure TFormGPSBabelFront.LoadFormats;
var
  sIgnoreShort:string;
  sCmd:string;
  f:file;
  Buffer:array[0..255] of char;
  hRead,hWrite:THandle;
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  saAttr:TSecurityAttributes;
  OutSt:TMemoryStream;
  dwRead:DWord;
  dwExitCode:cardinal;
  overlapped:TOverlapped;
  slstFormats:TStringList;
  i:integer;

  procedure ExtractFormat(sFormat:string);
  var
    toks:array[0..2] of string;
    i,nTok,nLen:integer;
  begin
    i := 1;
    nTok := 0;
    toks[0] := '';
    toks[1] := '';
    toks[2] := '';
    nLen := length(sFormat);
    while ((i<=nLen) and (nTok<3)) do begin
      if sFormat[i]=#9 then begin
        inc(nTok);
      end else begin
        toks[nTok] := toks[nTok] + sFormat[i];
      end;
      inc(i);
    end;
    {showmessage(toks[0]);
    showmessage(toks[1]);
    showmessage(toks[2]);}
    
    AddFormat(toks[0],toks[1],toks[2]);
  end;

begin
  slstFormats := TStringList.Create;

  sCmd := 'GPSBabel -^';

  memoStdErr.lines.clear;

  saAttr.nLength := sizeof(TSECURITYATTRIBUTES);
  saAttr.bInheritHandle := true;
  saAttr.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite,@saAttr,0) then begin
    ShowMessage('Unable to create pipe!');
    Exit;
  end;

  AllocConsole;

  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput:= hWrite;
  StartupInfo.hStdError := hWrite;

  if not CreateProcess(nil,pchar(sCmd),nil,nil,true,CREATE_NEW_CONSOLE,nil,nil,StartupInfo,ProcessInfo) then begin
    ShowMessage('Unable to execute GPSBabel.exe.')
  end else begin
    while (WaitforSingleObject(ProcessInfo.hProcess,0)) <> WAIT_OBJECT_0 do;

    PeekNamedPipe(hRead,nil,0,nil,@dwRead,nil);

    if dwRead>0 then begin
      OutSt := TMemoryStream.Create;

      repeat
        if ReadFile(hRead, Buffer, 80, dwRead, nil) then begin
          OutSt.WriteBuffer(Buffer, dwRead)
      end;
      until dwRead<>80;

      OutSt.Seek(0,0);
      slstFormats.LoadFromStream(OutSt);
      for i:=0 to slstFormats.count-1 do begin
        ExtractFormat(slstFormats[i]);
      end;
      OutSt.Free;
    end else memoStdErr.lines.add('Command executed successfully.');
  end;

  CloseHandle(hRead); CloseHandle(hWrite);
  FreeConsole;
end;

procedure TFormGPSBabelFront.btnProcessClick(Sender: TObject);
var
  sIgnoreShort:string;
  sCmd:string;
  f:file;
  Buffer:array[0..255] of char;
  hRead,hWrite:THandle;
  StartupInfo:TStartupInfo;
  ProcessInfo:TProcessInformation;
  saAttr:TSecurityAttributes;
  OutSt:TMemoryStream;
  dwRead:DWord;
  dwExitCode:cardinal;
  overlapped:TOverlapped;
begin
  if (comboInput.ItemIndex)<0 then begin
    ShowMessage('You must select the input file format.');
    exit;
  end;

  if (comboOutput.ItemIndex)<0 then begin
    ShowMessage('You must select the output file format.');
    exit;
  end;

  if cbIgnoreShort.checked then sIgnoreShort := '-s' else sIgnoreShort := '';

  // The output file must exist, or else ExtractShortPathName will not function
  if not fileexists(editoutput.text) then begin
    system.assign(f,editoutput.text);
    system.rewrite(f);
    system.close(f);
  end;

  // Construct the command line to execute gpsbabel.exe. ExtractShortPathName
  // is used to reduce any "long" file/directory names in the paths down to
  // 8.3 dos format names (this removes spaces, etc).
  sCmd := 'GPSBabel '+sIgnoreShort+' -i '+formats[comboInput.itemindex].sType+' -f '+
    ExtractShortPathName(editInput.text)+' -o '+formats[comboOutput.itemindex].sType+' -F '+
    ExtractShortPathName(editOutput.text);

  memoStdErr.lines.clear;

  saAttr.nLength := sizeof(TSECURITYATTRIBUTES);
  saAttr.bInheritHandle := true;
  saAttr.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite,@saAttr,0) then begin
    ShowMessage('Unable to create pipe!');
    Exit;
  end;

  AllocConsole;

  FillChar(StartupInfo,Sizeof(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput:= hWrite;
  StartupInfo.hStdError := hWrite;

  if not CreateProcess(nil,pchar(sCmd),nil,nil,true,CREATE_NEW_CONSOLE,nil,nil,StartupInfo,ProcessInfo) then begin
    ShowMessage('Unable to execute GPSBabel.exe.')
  end else begin
    while (WaitforSingleObject(ProcessInfo.hProcess,0)) <> WAIT_OBJECT_0 do;

    PeekNamedPipe(hRead,nil,0,nil,@dwRead,nil);

    if dwRead>0 then begin
      OutSt := TMemoryStream.Create;

      repeat
        if ReadFile(hRead, Buffer, 80, dwRead, nil) then begin
          OutSt.WriteBuffer(Buffer, dwRead)
      end;
      until dwRead<>80;

      OutSt.Seek(0,0);
      memoStdErr.lines.LoadFromStream(OutSt);
      OutSt.Free;
    end else memoStdErr.lines.add('Command executed successfully.');
  end;

  CloseHandle(hRead); CloseHandle(hWrite);
  FreeConsole;
end;

end.


