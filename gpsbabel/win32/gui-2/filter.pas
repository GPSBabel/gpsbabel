unit filter;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Mask, ExtCtrls;

type
  TfrmFilter = class(TForm)
    gbTracks: TGroupBox;
    cbTrackTitle: TCheckBox;
    edTrackTitle: TEdit;
    cbTrackSplit: TCheckBox;
    cbTrackTime: TCheckBox;
    udTimeHours: TUpDown;
    edTrackTimeHours: TEdit;
    udTimeMinutes: TUpDown;
    edTrackTimeMinutes: TEdit;
    edTrackTimeDays: TEdit;
    udTimeDays: TUpDown;
    edTrackTimeSeconds: TEdit;
    udTimeSeconds: TUpDown;
    lbTimePlusMinus: TLabel;
    lbTimeDays: TLabel;
    lbTimeHours: TLabel;
    lbTimeMinutes: TLabel;
    lbTimeSeconds: TLabel;
    cbTrackStart: TCheckBox;
    dtpTrackStartDate: TDateTimePicker;
    dtpTrackStartTime: TDateTimePicker;
    cbTrackStop: TCheckBox;
    dtpTrackStopDate: TDateTimePicker;
    dtpTrackStopTime: TDateTimePicker;
    gbRoutes: TGroupBox;
    cbRouteSimplify: TCheckBox;
    lbRouteSimplifyCount: TLabel;
    edRoutesSimplifyMaxPoints: TMaskEdit;
    udRouteSompifyMaxPoints: TUpDown;
    lbRouteSimplifyText: TLabel;
    pnBottom: TPanel;
    btnOK: TBitBtn;
    gbWaypoints: TGroupBox;
    cbWayptMergeDupLoc: TCheckBox;
    cbReverse: TCheckBox;
    cbWayptMergeDupNames: TCheckBox;
    cbWayptMergeDistance: TCheckBox;
    cobWayptMergeDist: TComboBox;
    edWayptMergeDist: TEdit;
    cbWayptSort: TCheckBox;
    cbWayptMergeDups: TCheckBox;
    btnCancel: TBitBtn;
    cbTrackPack: TCheckBox;
    cbTrackMerge: TCheckBox;
    BitBtn1: TBitBtn;
    Image1: TImage;
    procedure cbTrackTimeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbTrackTitleClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbTrackStartClick(Sender: TObject);
    procedure cbTrackStopClick(Sender: TObject);
    procedure cbRouteSimplifyClick(Sender: TObject);
    procedure cbTrackPackClick(Sender: TObject);
    procedure cbTrackMergeClick(Sender: TObject);
    procedure cbWayptMergeDistanceClick(Sender: TObject);
    procedure cbWayptMergeDupsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    lTrackTimeList: TList;
    FTracksEnabled: Boolean;
    function AnyChecked(Control: TWinControl): Boolean;
    procedure EnableList(List: TList; Enable: Boolean = True);
    procedure SetTracksEnabled(const Value: Boolean);
    function AllValid: Boolean;
  public
    { Public-Deklarationen }
    function CmdLine: string;
    property TracksEnabled: Boolean read FTracksEnabled write SetTracksEnabled;
  end;

var
  frmFilter: TfrmFilter = nil;

implementation

{$R *.DFM}

procedure FixPosition(AControl, LeftFromMe: TControl; IsText: Boolean);
begin
  AControl.Left := LeftFromMe.Left + LeftFromMe.Width;
  if (IsText) then
    AControl.Left := AControl.Left + 4;
end;

procedure TfrmFilter.cbTrackTimeClick(Sender: TObject);
begin
  EnableList(lTrackTimeList, cbTrackTime.Checked);
end;

procedure TfrmFilter.EnableList(List: TList; Enable: Boolean);
var
  i: Integer;
  o: TObject;
begin
  for i := 0 to List.Count - 1 do
  begin
    o := Pointer(List.Items[i]);
    if (o is TControl) then
      with o as TControl do
        Enabled := Enable;
  end;
end;

procedure TfrmFilter.FormCreate(Sender: TObject);
begin
  gnugettextD4.TranslateComponent(SELF);
  
  lTrackTimeList := TList.Create;
  
  lTrackTimeList.Add(edTrackTimeDays);
  lTrackTimeList.Add(edTrackTimeHours);
  lTrackTimeList.Add(edTrackTimeMinutes);
  lTrackTimeList.Add(edTrackTimeSeconds);

  EnableList(lTrackTimeList, False);

  FixPosition(edTrackTimeDays, lbTimePlusMinus, True);
  FixPosition(udTimeDays, edTrackTimeDays, False);
  FixPosition(lbTimeDays, udTimeDays, True);

  FixPosition(edTrackTimeHours, lbTimeDays, True);
  FixPosition(udTimeHours, edTrackTimeHours, False);
  FixPosition(lbTimeHours, udTimeHours, True);

  FixPosition(edTrackTimeMinutes, lbTimeHours, True);
  FixPosition(udTimeMinutes, edTrackTimeMinutes, False);
  FixPosition(lbTimeMinutes, udTimeMinutes, True);

  FixPosition(edTrackTimeSeconds, lbTimeMinutes, True);
  FixPosition(udTimeSeconds, edTrackTimeSeconds, False);
  FixPosition(lbTimeSeconds, udTimeSeconds, True);

  cobWayptMergeDist.Text := _('Miles');
end;

procedure TfrmFilter.cbTrackTitleClick(Sender: TObject);
begin
  edTrackTitle.Enabled := cbTrackTitle.Checked;
end;

function TfrmFilter.CmdLine: string;

  procedure SimpleOption(var CmdLine: string; CheckBox: TCheckBox; const Option: string);
  begin
    if (CheckBox.Checked) then
      CmdLine := Format('%s -x %s', [CmdLine, Option]);
  end;

var
  s: string;
begin
  Result := '';
  if not AnyChecked(Self) then Exit;

  Result := '';

  if AnyChecked(gbWaypoints) then
  begin
    if cbWayptMergeDups.Checked and
       (cbWayptMergeDupNames.Checked or cbWayptMergeDupLoc.Checked) then
    begin
      Result := Format('%s -x %s', [Result, 'duplicate']);
      if cbWayptMergeDupNames.Checked then
        Result := Format('%s,%s', [Result, 'shortname']);
      if cbWayptMergeDupLoc.Checked then
        Result := Format('%s,%s', [Result, 'location']);

      SimpleOption(Result, cbWayptSort, 'sort');
    end;
  end;

  if AnyChecked(gbTracks) then
  begin
    Result := Format('%s -x %s', [Result, 'track']);
    if cbTrackTitle.Checked then
      Result := Format('%s,title="%s"', [Result, edTrackTitle.Text]);

    if cbTrackTime.Checked then
    begin
      s := Format('%sd%sh%sm%ss',
        [edTrackTimeDays.Text, edTrackTimeHours.Text,
         edTrackTimeMinutes.Text, edTrackTimeSeconds.Text]);
      if (s <> '0d0h0m0s') then
        Result := Format('%s,move=%s', [Result, s]);
    end;

    if cbTrackPack.Checked then
      Result := Format('%s,pack', [Result])
    else if cbTrackMerge.Checked then
      Result := Format('%s,merge', [Result]);

    if cbTrackSplit.Checked then
      Result := Format('%s,split', [Result]);

    if cbTrackStart.Checked then
      Result := Format('%s,start=%s', [
        Result,
        FormatDateTime('yyyymmddhhnnss',
          Int(dtpTrackStartDate.DateTime) + Frac(dtpTrackStartTime.DateTime))]);
    if cbTrackStop.Checked then
      Result := Format('%s,stop=%s', [
        Result,
        FormatDateTime('yyyymmddhhnnss',
          Int(dtpTrackStopDate.DateTime) + Frac(dtpTrackStopTime.DateTime))]);
  end;

  if AnyChecked(gbRoutes) then
  begin
    if cbRouteSimplify.Checked then
      Result := Format('%s -x simplify,count=%s',
        [Result, Trim(edRoutesSimplifyMaxPoints.Text)]);

    SimpleOption(Result, cbReverse, 'reverse');
  end;
end;

function TfrmFilter.AnyChecked(Control: TWinControl): Boolean;
var
  i: Integer;
  c: TWinControl;
begin
  Result := False;
  for i := 0 to Self.ComponentCount - 1 do
  begin
    c := Pointer(Self.Components[i]);
    if not(c.InheritsFrom(TWinControl)) then Continue;
    if (c.parent <> Control) then Continue;

    if ((c is TCheckBox) and TCheckBox(c).Enabled) then
      Result := TCheckBox(c).Checked else
    if ((c is TGroupBox) and c.Enabled) then
      Result := AnyChecked(c);
    if (Result) then Exit;
  end;
end;

procedure TfrmFilter.SetTracksEnabled(const Value: Boolean);
begin
  FTracksEnabled := Value;
  gbTracks.Enabled := Value;
end;

function TfrmFilter.AllValid: Boolean;
begin
  Result := True;
end;

procedure TfrmFilter.btnOKClick(Sender: TObject);
begin
  if AllValid then ModalResult := mrOK;
end;

procedure TfrmFilter.cbTrackStartClick(Sender: TObject);
begin
  dtpTrackStartDate.Enabled := cbTrackStart.Checked;
  dtpTrackStartTime.Enabled := cbTrackStart.Checked;
end;

procedure TfrmFilter.cbTrackStopClick(Sender: TObject);
begin
  dtpTrackStopDate.Enabled := cbTrackStop.Checked;
  dtpTrackStopTime.Enabled := cbTrackStop.Checked;
end;

procedure TfrmFilter.cbRouteSimplifyClick(Sender: TObject);
begin
  edRoutesSimplifyMaxPoints.Enabled := cbRouteSimplify.Checked;
end;

procedure TfrmFilter.cbTrackPackClick(Sender: TObject);
begin
  if cbTrackPack.Checked then
    cbTrackMerge.Checked := False;
end;

procedure TfrmFilter.cbTrackMergeClick(Sender: TObject);
begin
  if cbTrackMerge.Checked then cbTrackPack.Checked := False;
end;

procedure TfrmFilter.cbWayptMergeDistanceClick(Sender: TObject);
begin
  edWayptMergeDist.Enabled := cbWayptMergeDistance.Checked;
  cobWayptMergeDist.Enabled := cbWayptMergeDistance.Checked;
end;

procedure TfrmFilter.cbWayptMergeDupsClick(Sender: TObject);
begin
  cbWayptMergeDupLoc.Enabled := cbWayptMergeDups.Checked;
  cbWayptMergeDupNames.Enabled := cbWayptMergeDups.Checked;
end;

end.
