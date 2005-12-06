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
  gnugettext, gnugettextDx,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Mask, ExtCtrls,
  common;

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
    cbWayptRadius: TCheckBox;
    edWayptRadius: TEdit;
    cobWayptRadius: TComboBox;
    lbWayptRadiusLat: TLabel;
    lbWayptRadiusLon: TLabel;
    edWayptRadiusLat: TEdit;
    edWayptRadiusLon: TEdit;
    cbTrackRangeTimeZone: TCheckBox;
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
    procedure cbWayptRadiusClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
    lTrackTimeList: TList;
    FTracksEnabled: Boolean;
    FInitialValues: string;
    function AnyChecked(Control: TWinControl): Boolean;
    procedure EnableList(List: TList; Enable: Boolean = True);
    procedure SetTracksEnabled(const Value: Boolean);
    function AllValid: Boolean;
    function ValidateNumerical(AEdit: TCustomEdit; AMin, AMax: Extended): Boolean;
    procedure ChangeCheckBoxesChecked(AComponent: TComponent; Restore: Boolean = False);
  public
    { Public-Deklarationen }
    function CmdLine: string;
    property TracksEnabled: Boolean read FTracksEnabled write SetTracksEnabled;
  end;

type
  eOutOfRange = class(Exception);

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

procedure EnableAll(Parent: TWinControl; Enable: Boolean);
var
  i: Integer;
  c: TComponent;
  master: TComponent;
  ctrl: TControl;
begin
  if (Parent = nil) then Exit;
  master := Parent.Owner;
  if (master = nil) then Exit;
  for i := 0 to master.ComponentCount - 1 do
  begin
    c := master.Components[i];
    if not(c.InheritsFrom(TControl)) then Continue;
    ctrl := Pointer(c);
    if not(ctrl.Parent = Parent) then Continue;
    ctrl.Enabled := Enable;
  end;
end;

{ TfrmFilter }

procedure TfrmFilter.FormCreate(Sender: TObject);
var
  CurrentTime: TDateTime;

begin
  TranslateComponent(SELF);

  CurrentTime := SysUtils.Now;
  dtpTrackStartDate.DateTime := Int(CurrentTime);
  dtpTrackStopDate.DateTime := Int(CurrentTime);

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

  FixPosition(lbWayptRadiusLat, cobWayptRadius, True);
  FixPosition(edWayptRadiusLat, lbWayptRadiusLat, True);
  FixPosition(lbWayptRadiusLon, edWayptRadiusLat, True);
  FixPosition(edWayptRadiusLon, lbWayptRadiusLon, True);

  // will not be translated, fill by hand

  cobWayptMergeDist.Items.Add(_('Feet'));
  cobWayptMergeDist.Items.Add(_('Meter'));
  cobWayptMergeDist.ItemIndex := 0;

  cobWayptRadius.Items.Add(_('Miles'));
  cobWayptRadius.Items.Add(_('Kilometer'));
  cobWayptRadius.ItemIndex := 0;

  dtpTrackStopTime.Time := 1 - (1.0 / (24*60*60));

  // Enable/Disable depending on gpsbabel.exe version

  if (common.gpsbabel_vfmt < '001.002.007') then
  begin
    EnableAll(gbTracks, False);
    gbTracks.Hint := Format(_('Not supported by gpsbabel.exe, release %s!'), [
      gpsbabel_version]);
    gbTracks.ShowHint := True;
  end;
end;

function TfrmFilter.ValidateNumerical(AEdit: TCustomEdit; AMin, AMax: Extended): Boolean;
var
  s: string;
  v: Extended;
begin
  Result := True;
  if not(AEdit.Enabled) then Exit;
  if (ModalResult <> mrOK)  then Exit;

  Result := False;
  s := Trim(AEdit.Text);
  if (s = '') then s := '0';
  while (Pos(',', s) <> 0) do
    s[Pos(',', s)] := '.';

  AEdit.Text := s;

  try
    v := SysUtils.StrToFloat(s);
  except
    on E: EConvertError do
    begin
      AEdit.SetFocus;
      raise;
    end;
  end;

  if (v < AMin) or (v > AMax) then
  begin
    AEdit.SetFocus;
    raise eOutOfRange.CreateFmt(_('Value (%s) out of range (%g to %g)!'),
      [s, AMin, AMax]);
  end;
  Result := True;
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
  tz_Info: TTimeZoneInformation;
  dt: TDateTime;
  dt_bias: TDateTime;
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
    end;
    if cbWayptMergeDistance.Checked then
    begin
      Result := Format('%s -x position,distance=%s', [Result, edWayptMergeDist.Text]);
      if (cobWayptMergeDist.ItemIndex = 0) then
        Result := Result + 'f' else
        Result := Result + 'm';
    end;
    if cbWayptRadius.Checked then
    begin
      Result := Format('%s -x radius,distance=%s', [Result, edWayptRadius.Text]);
      if (cobWayptRadius.ItemIndex = 0) then
        Result := Result + 'M' else
        Result := Result + 'K';
      Result := Format('%s,lat=%s,lon=%s', [Result, edWayptRadiusLat.Text, edWayptRadiusLon.Text]);
    end;
    SimpleOption(Result, cbWayptSort, 'sort');
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

    if (cbTrackRangeTimeZone.Enabled and cbTrackRangeTimeZone.Checked) then
    begin
      Windows.GetTimeZoneInformation(tz_Info);
      tz_Info.Bias := tz_Info.Bias + tz_Info.DaylightBias;
      dt_bias := tz_Info.Bias / (24*60);
    end
    else
      dt_bias := 0.0;


    if cbTrackStart.Checked then
    begin
      dt := Int(dtpTrackStartDate.DateTime) + Frac(dtpTrackStartTime.DateTime) + dt_bias;
      Result := Format('%s,start=%s', [
        Result,
        FormatDateTime('yyyymmddhhnnss', dt)]);
    end;
    if cbTrackStop.Checked then
    begin
      dt := Int(dtpTrackStopDate.DateTime) + Frac(dtpTrackStopTime.DateTime) + dt_bias;
      Result := Format('%s,stop=%s', [
        Result,
        FormatDateTime('yyyymmddhhnnss', dt)]);
    end;
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
  cbTrackRangeTimeZone.Enabled :=
    cbTrackStart.Checked or cbTrackStop.Checked;
end;

procedure TfrmFilter.cbTrackStopClick(Sender: TObject);
begin
  dtpTrackStopDate.Enabled := cbTrackStop.Checked;
  dtpTrackStopTime.Enabled := cbTrackStop.Checked;
  cbTrackRangeTimeZone.Enabled :=
    cbTrackStart.Checked or cbTrackStop.Checked;
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

procedure TfrmFilter.cbWayptRadiusClick(Sender: TObject);
begin
  edWayptRadius.Enabled := cbWayptRadius.Checked;
  cobWayptRadius.Enabled := cbWayptRadius.Checked;
  edWayptRadiusLat.Enabled := cbWayptRadius.Checked;
  edWayptRadiusLon.Enabled := cbWayptRadius.Checked;
end;

procedure TfrmFilter.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if (ModalResult <> mrOK) then
  begin
    ChangeCheckBoxesChecked(Self, True);
    CanClose := True;
    Exit;
  end;
  CanClose :=
    ValidateNumerical(edWayptRadius, 0, 99999) and
    ValidateNumerical(edWayptRadiusLat, -180, 180) and
    ValidateNumerical(edWayptRadiusLon, -90, 90) and
    ValidateNumerical(edWayptMergeDist, 0, 99999999) and
    ValidateNumerical(edRoutesSimplifyMaxPoints, 1, 9999);
  ChangeCheckBoxesChecked(Self, False);
end;

procedure TfrmFilter.FormShow(Sender: TObject);
begin
  ChangeCheckBoxesChecked(Self);
  FInitialValues := CmdLine;
end;

procedure TfrmFilter.ChangeCheckBoxesChecked(AComponent: TComponent; Restore: Boolean = False);
var
  i, j: Integer;
  c: TComponent;
begin
  j := AComponent.ComponentCount;
  for i := 0 to j - 1 do
  begin
    c := AComponent.Components[i];
    if (c is TCheckBox) then
    begin
      if (Restore) then
        TCheckBox(c).Checked := (c.Tag <> 0) else
        c.Tag := Integer(TCheckBox(c).Checked);
    end
    else if (c.ComponentCount > 0) then
      ChangeCheckBoxesChecked(c);
  end;
end;

procedure TfrmFilter.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  str: string;
begin
  if (Key <> 27) then Exit;


  str := Self.CmdLine;
  if (str <> FInitialValues) then
  begin
    if not(MessageDlg(_('Discard changes?'), mtWarning, mbOKCancel, 0) = mrOK) then
      Exit;
  end;
  ModalResult := mrCancel;
end;

end.
