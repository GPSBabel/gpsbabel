{

		Copyright (C) 2002 Josh M. McKee, geo@mrsnazz.com

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

unit gpsbabelfront_mainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

const
	FormatCount = 18;

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
    procedure FormCreate(Sender: TObject);
    procedure btnInputClick(Sender: TObject);
    procedure btnOutputClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    formats:array[0..FormatCount-1] of TFormat;

    procedure PopulateCombos;
    procedure PopulateDialogs;
  end;

var
  FormGPSBabelFront: TFormGPSBabelFront;

implementation

{$R *.dfm}

procedure TFormGPSBabelFront.PopulateCombos;
var
	i:integer;
begin
	for i:=0 to FormatCount-1 do begin
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
	for i:=0 to FormatCount-1 do begin
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
	formats[0].sType := 'geo';
  formats[0].sExt := 'loc';
  formats[0].sDesc := 'Geocaching.com .loc';
	formats[1].sType := 'gpsman';
  formats[1].sExt := '';
  formats[1].sDesc := 'GPSman';
	formats[2].sType := 'gpx';
	formats[2].sExt := 'gpx';
  formats[2].sDesc := 'GPX XML';
  formats[3].sType := 'magellan';
  formats[3].sExt := '';
  formats[3].sDesc := 'Magellan protocol';
	formats[4].sType := 'mapsend';
  formats[4].sExt := '';
  formats[4].sDesc := 'Magellan Mapsend';
  formats[5].sType := 'pcx';
  formats[5].sExt := 'pcx';
  formats[5].sDesc := 'Garmin PCX5';
  formats[6].sType := 'mapsource';
  formats[6].sExt := '';
  formats[6].sDesc := 'Garmin Mapsource';
  formats[7].sType := 'gpsutil';
  formats[7].sExt := '';
  formats[7].sDesc := 'gpsutil';
  formats[8].sType := 'tiger';
  formats[8].sExt := '';
  formats[8].sDesc := 'U.S. Census Bureau Tiger Mapping Service';
  formats[9].sType := 'csv';
  formats[9].sExt := 'csv';
  formats[9].sDesc := 'Comma separated values';
  formats[10].sType := 'dna';
  formats[10].sExt := 'dna';
  formats[10].sDesc := 'Navitrak DNA marker format';
  formats[11].sType := 'psp';
  formats[11].sExt := 'psp';
  formats[11].sDesc := 'MS PocketStreets 2002 Pushpin';
	formats[12].sType := 'cetus';
  formats[12].sExt := 'pdb';
  formats[12].sDesc := 'Cetus for Palm/OS';
  formats[13].sType := 'gpspilot';
  formats[13].sExt := '';
  formats[13].sDesc := 'GPSPilot Tracker for Palm/OS';
  formats[14].sType := 'garmin';
  formats[14].sExt := '';
  formats[14].sDesc := 'Garmin serial protocol';
	formats[15].sType := 'mxf';
  formats[15].sExt := 'mxf';
  formats[15].sDesc := 'MapTech Exchange Format';
	formats[16].sType := 'holux';
  formats[16].sExt := 'wpo';
  formats[16].sDesc := 'Holux (gm-100) .wpo Format';
	formats[17].sType := 'ozi';
  formats[17].sExt := 'ozi';
  formats[17].sDesc := 'OziExplorer Waypoint';

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
		for i := 0 to FormatCount-1 do begin
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
		for i := 0 to FormatCount-1 do begin
    	if '.' + uppercase(formats[i].sExt) = sExt then comboOutput.ItemIndex := i;
    end;
  end;
end;

procedure TFormGPSBabelFront.btnProcessClick(Sender: TObject);
var
	sIgnoreShort:string;
  sCmd:string;
  f:file;
begin
	if cbIgnoreShort.checked then sIgnoreShort := '-s' else sIgnoreShort := '';

  if not fileexists(editoutput.text) then begin
		system.assign(f,editoutput.text);
		system.rewrite(f);
    system.close(f);
  end;

  sCmd := 'GPSBabel '+sIgnoreShort+' -i '+formats[comboInput.itemindex].sType+' -f '+
  	ExtractShortPathName(editInput.text)+' -o '+formats[comboOutput.itemindex].sType+' -F '+
    ExtractShortPathName(editOutput.text) + ' > result.txt';

	WinExec(pchar(sCmd),SW_SHOW);
end;

end.


