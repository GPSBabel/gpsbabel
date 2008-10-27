unit filter;

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
  gnugettext, gnugettextDx,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Mask, ExtCtrls, Registry,
  common, utils;

type
  TfrmFilter = class(TForm)
    gbTracks: TGroupBox;
    cbTrackTitle: TCheckBox;
    edTrackTitleValue: TEdit;
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
    cobWayptMergeDistUnit: TComboBox;
    edWayptMergeDist: TEdit;
    cbWayptSort: TCheckBox;
    cbWayptMergeDups: TCheckBox;
    btnCancel: TBitBtn;
    cbTrackPack: TCheckBox;
    cbTrackMerge: TCheckBox;
    BitBtn1: TBitBtn;
    cbWayptRadius: TCheckBox;
    edWayptRadius: TEdit;
    cobWayptRadiusUnit: TComboBox;
    lbWayptRadiusLat: TLabel;
    lbWayptRadiusLon: TLabel;
    edWayptRadiusLat: TEdit;
    edWayptRadiusLon: TEdit;
    cbTrackRangeTimeZone: TCheckBox;
    btnHelp: TBitBtn;
    cbGPSfix: TCheckBox;
    cbTrackCourse: TCheckBox;
    cbTrackSpeed: TCheckBox;
    gbTransform: TGroupBox;
    cobTransformType: TComboBox;
    cbTransform: TCheckBox;
    cbTransformDelete: TCheckBox;
    cobGPSfixes: TComboBox;
    Panel1: TPanel;
    gbMisc: TGroupBox;
    cbSwapData: TCheckBox;
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
    procedure btnHelpClick(Sender: TObject);
    procedure cbTransformClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbGPSfixClick(Sender: TObject);
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
    procedure LoadSettingsFromInifile();
    procedure LoadSettingsFromRegistry();
    procedure StoreSettingsToInifile();
    procedure StoreSettingsToRegistry();
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
  RestoreBounds('filter_form', Self);

  cobTransformType.Items.Clear;
  cobTransformType.Items.Add(_('Waypoints') + ' -> ' + _('Routes'));
  cobTransformType.Items.Add(_('Routes') + ' -> ' + _('Waypoints'));
  cobTransformType.Items.Add(_('Routes') + ' -> ' + _('Tracks'));
  cobTransformType.Items.Add(_('Tracks') + ' -> ' + _('Routes'));
  cobTransformType.Items.Add(_('Waypoints') + ' -> ' + _('Tracks'));
  cobTransformType.Items.Add(_('Tracks') + ' -> ' + _('Waypoints'));
  cobTransformType.ItemIndex := 0;

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

  FixPosition(lbWayptRadiusLat, cobWayptRadiusUnit, True);
  FixPosition(edWayptRadiusLat, lbWayptRadiusLat, True);
  FixPosition(lbWayptRadiusLon, edWayptRadiusLat, True);
  FixPosition(edWayptRadiusLon, lbWayptRadiusLon, True);

  // will not be translated, fill by hand

  cobWayptMergeDistUnit.Items.Add(_('Feet'));
  cobWayptMergeDistUnit.Items.Add(_('Meter'));
  cobWayptMergeDistUnit.ItemIndex := 0;

  cobWayptRadiusUnit.Items.Add(_('Miles'));
  cobWayptRadiusUnit.Items.Add(_('Kilometer'));
  cobWayptRadiusUnit.ItemIndex := 0;

  dtpTrackStopTime.Time := 1 - (1.0 / (24*60*60));

  // Enable/Disable depending on gpsbabel.exe version

  if (common.gpsbabel_vfmt < '001.002.007') then
  begin
    EnableAll(gbTracks, False);
    gbTracks.Hint := Format(_('Not supported by gpsbabel.exe, release %s!'), [
      gpsbabel_version]);
    gbTracks.ShowHint := True;
  end;

  if not(gpsbabel_knows_inifile) then
  begin
    cbGPSfix.Enabled := False;
    cbTrackCourse.Enabled := False;
    cbTrackSpeed.Enabled := False;
    cobGPSfixes.Enabled := False;
  end;

  LoadSettingsFromRegistry();

  gbTransform.Enabled := (common.gpsbabel_vfmt >= '001.003.002');
  EnableAll(gbTransform, gbTransform.Enabled);

  cobTransformType.Enabled := cbTransform.Checked;
  cbTransformDelete.Enabled := cbTransform.Checked;

  cbSwapData.Enabled := gpsbabel_knows_swap_filter;
  gbMisc.Enabled := (cbSwapData.Enabled { or ... });
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
  edTrackTitleValue.Enabled := cbTrackTitle.Checked;
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

  if cbSwapData.Checked then
  begin
    Result := Format('%s -x %s', [Result, 'swap']);
  end;

  if gbTransform.Enabled and cbTransform.Checked then
  begin
    Result := Format('%s -x %s', [Result, 'transform,']);
    case cobTransformType.ItemIndex of
      0: Result := Result + 'rte=wpt';
      1: Result := Result + 'wpt=rte';
      2: Result := Result + 'trk=rte';
      3: Result := Result + 'rte=trk';
      4: Result := Result + 'trk=wpt';
      5: Result := Result + 'wpt=trk';
    end;
    if cbTransformDelete.Checked then
      Result := Result + ',del=y' else
      Result := Result + ',del=n';
  end;
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
      if (cobWayptMergeDistUnit.ItemIndex = 0) then
        Result := Result + 'f' else
        Result := Result + 'm';
    end;
    if cbWayptRadius.Checked then
    begin
      Result := Format('%s -x radius,distance=%s', [Result, edWayptRadius.Text]);
      if (cobWayptRadiusUnit.ItemIndex = 0) then
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
      Result := Format('%s,title="%s"', [Result, edTrackTitleValue.Text]);

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
    if cbGPSfix.Checked then
      Result := Format('%s,fix=%s', [Result, cobGPSfixes.Text]);
    if cbTrackCourse.Checked then
      Result := Format('%s,course', [Result]);
    if cbTrackSpeed.Checked then
      Result := Format('%s,speed', [Result]);
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
      Result := AnyChecked(c) else
    if (c is TPanel) then
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
  if AllValid then
  begin
//  StoreSettingsToInifile();
    StoreSettingsToRegistry();
    ModalResult := mrOK;
  end;
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
  if cbTrackMerge.Checked then
    cbTrackPack.Checked := False;
end;

procedure TfrmFilter.cbWayptMergeDistanceClick(Sender: TObject);
begin
  edWayptMergeDist.Enabled := cbWayptMergeDistance.Checked;
  cobWayptMergeDistUnit.Enabled := cbWayptMergeDistance.Checked;
end;

procedure TfrmFilter.cbWayptMergeDupsClick(Sender: TObject);
begin
  cbWayptMergeDupLoc.Enabled := cbWayptMergeDups.Checked;
  cbWayptMergeDupNames.Enabled := cbWayptMergeDups.Checked;
end;

procedure TfrmFilter.cbWayptRadiusClick(Sender: TObject);
begin
  edWayptRadius.Enabled := cbWayptRadius.Checked;
  cobWayptRadiusUnit.Enabled := cbWayptRadius.Checked;
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
    ValidateNumerical(edWayptRadiusLat, -90, 90) and
    ValidateNumerical(edWayptRadiusLon, -180, 180) and
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

procedure TfrmFilter.btnHelpClick(Sender: TObject);
begin
  WinOpenURL(readme_html_path + '#Data_Filters');
end;

procedure TfrmFilter.LoadSettingsFromInifile();
var
  c: TComponent;
  i: Integer;
  l: TStrings;
  s: string;
begin
(*
  l := TStringList.Create;
  try
    gpsbabel_ini.ReadSection('GPSBabelGUI', l);
    for i := 0 to l.Count - 1 do
    begin
      s := l.Strings[i];
      c := SELF.FindComponent('cb' + s);
      if (c <> nil) and (c is TCheckbox) then
        TCheckbox(c).Checked := (gpsbabel_ini.ReadString('GPSBabelGUI', s, '0') <> '0');
    end;
    edTrackTitleValue.Text := gpsbabel_ini.ReadString('track', 'title',
      edTrackTitleValue.Text);
    edRoutesSimplifyMaxPoints.Text := gpsbabel_ini.ReadString('simplify', 'count',
      edRoutesSimplifyMaxPoints.Text);
  finally
    l.Free;
  end;
*)
end;

procedure TfrmFilter.StoreSettingsToInifile();
var
  i: Integer;
  c: TComponent;
begin
(*
  for i := 0 to SELF.ComponentCount - 1 do
  begin
    c := SELF.Components[i];
    if (c is TCheckBox) then
    begin
      if TCheckBox(c).Checked then
        gpsbabel_ini.WriteString('GPSBabelGUI', Copy(TCheckbox(c).Name, 3, 256), '1')
      else
        gpsbabel_ini.DeleteKey('GPSBabelGUI', Copy(TCheckbox(c).Name, 3, 256));
    end;
  end;

  if cbTrackTitle.Checked then
    gpsbabel_ini.WriteString('track', 'title', edTrackTitleValue.Text)
  else
    gpsbabel_ini.DeleteKey('track', 'title');

  if cbRouteSimplify.Checked then
    gpsbabel_ini.WriteString('simplify', 'count', edRoutesSimplifyMaxPoints.Text)
  else
    gpsbabel_ini.DeleteKey('simplify', 'count');
*)
end;

procedure TfrmFilter.StoreSettingsToRegistry();
var
  i: Integer;
  c: TComponent;
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CURRENT_USER;
    if not(r.OpenKey('\SOFTWARE\GPSBabel', True)) then Exit;

    for i := 0 to SELF.ComponentCount - 1 do
    begin
      c := SELF.Components[i];
      if (c is TCheckbox) then
        r.WriteBool('filter:' + Copy(c.Name, 3, 256), TCheckBox(c).Checked)
      else if (c is TEdit) then
        r.WriteString('filter:' + Copy(c.Name, 3, 256), TEdit(c).Text)
      else if (c is TCombobox) then
        r.WriteString('filter:' + Copy(c.Name, 4, 256), IntToStr(TCombobox(c).ItemIndex));
    end;
  finally
    r.Free;
  end;
end;

procedure TfrmFilter.LoadSettingsFromRegistry();
var
  i: Integer;
  c: TComponent;
  r: TRegistry;
  s: string;
  u: TUpDown;

  function ReadString(R: TRegistry; const Key, Def: string): string;
  begin
    if R.ValueExists(Key) then
      Result := R.ReadString(Key)
    else
      Result := Def;
  end;

begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CURRENT_USER;
    if not(r.OpenKey('\SOFTWARE\GPSBabel', True)) then Exit;

    for i := 0 to SELF.ComponentCount - 1 do
    begin
      c := SELF.Components[i];
      try
        if (c is TCheckbox) then
          TCheckBox(c).Checked := r.ReadBool('filter:' + Copy(c.Name, 3, 256))
        else if (c is TEdit) then
        begin
          s := ReadString(r, 'filter:' + Copy(c.Name, 3, 256), TEdit(c).Text);
          if HasUpDown(TEdit(c), u) then
            u.Position := StrToInt(s)
          else
            TEdit(c).Text := s;
        end
        else if (c is TCombobox) then
        begin
          s := ReadString(r, 'filter:' + Copy(c.Name, 4, 256), '0');
          TCombobox(c).ItemIndex := StrToIntDef(s, 0);
        end;
      except
        on E: Exception do ;
      end;
    end;
  finally
    r.Free;
  end;
end;

procedure TfrmFilter.cbTransformClick(Sender: TObject);
begin
  cobTransformType.Enabled := cbTransform.Checked;
  cbTransformDelete.Enabled := cbTransform.Checked;
end;

procedure TfrmFilter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StoreBounds('filter_form', Self);
end;

procedure TfrmFilter.cbGPSfixClick(Sender: TObject);
begin
  cobGPSfixes.Enabled := TCheckBox(Sender).Checked;
end;

end.
