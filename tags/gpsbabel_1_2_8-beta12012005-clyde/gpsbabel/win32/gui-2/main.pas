unit main;

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
  TypInfo, gnugettext, gnugettextDx,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  common, utils, ImgList, ActnList, Menus, ComCtrls, ToolWin;

type
  TfrmMain = class(TForm)
    pnTop: TPanel;
    pnBottom: TPanel;
    cbWaypoints: TCheckBox;
    cbRoutes: TCheckBox;
    cbTracks: TCheckBox;
    lbWhat: TLabel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    wptInputOK: TSpeedButton;
    ImageList1: TImageList;
    wptOutputOK: TSpeedButton;
    rteInputOK: TSpeedButton;
    rteOutputOK: TSpeedButton;
    trkInputOK: TSpeedButton;
    trkOutputOK: TSpeedButton;
    ActionList1: TActionList;
    acConvert: TAction;
    btnFilter: TBitBtn;
    acFilterSelect: TAction;
    btnProcess: TBitBtn;
    memoOutput: TMemo;
    stbMain: TStatusBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuExit: TMenuItem;
    acFileExit: TAction;
    mnuHelp: TMenuItem;
    acHelpAbout: TAction;
    acHelpIntro: TAction;
    Intro1: TMenuItem;
    About1: TMenuItem;
    mnuReadme: TMenuItem;
    acHelpReadme: TAction;
    N1: TMenuItem;
    mnuOptions: TMenuItem;
    mnuSynthesizeShortNames: TMenuItem;
    Filter1: TMenuItem;
    N2: TMenuItem;
    acOptionsSourceFormat: TAction;
    acOptionsTargetFormat: TAction;
    forsourceformat1: TMenuItem;
    fortargetformat1: TMenuItem;
    acFileClearMemo: TAction;
    N3: TMenuItem;
    Clearoutput1: TMenuItem;
    acFinalizeDropDowns: TAction;
    N4: TMenuItem;
    mnuOptionsForceDataType: TMenuItem;
    acOptionsEnableCharactersetTransformation: TAction;
    Enablecharactersettransformation1: TMenuItem;
    gbInput: TGroupBox;
    sbOpenFile: TSpeedButton;
    lbInputOpts: TLabel;
    lbInputFormat: TLabel;
    lbInputFile: TLabel;
    edInputOpts: TComboBox;
    edInputFile: TComboBox;
    chbInputDevice: TCheckBox;
    cbInputLang: TComboBox;
    cbInputFormatDevice: TComboBox;
    cbInputFormat: TComboBox;
    cbInputDevice: TComboBox;
    btnInputOpts: TSpeedButton;
    gbOutput: TGroupBox;
    cbOutputFormatDevice: TComboBox;
    chbOutputDevice: TCheckBox;
    lbOutputOpts: TLabel;
    edOutputOpts: TComboBox;
    btnOutputOpts: TSpeedButton;
    lbOutputFormat: TLabel;
    cbOutputFormat: TComboBox;
    lbOutputFile: TLabel;
    edOutputFile: TComboBox;
    sbSaveFile: TSpeedButton;
    cbOutputLang: TComboBox;
    cbOutputDevice: TComboBox;
    acFileOutputToScreen: TAction;
    Outputtoscreen1: TMenuItem;
    acDebugCreatePo: TAction;
    mnuDebug: TMenuItem;
    Createoptionspo1: TMenuItem;
    acFileChangeLanguage: TAction;
    Changelanguage1: TMenuItem;
    N5: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure edInputFileChange(Sender: TObject);
    procedure CheckInput;
    procedure edOutputFileChange(Sender: TObject);
    procedure cbWaypointsClick(Sender: TObject);
    procedure cbRoutesClick(Sender: TObject);
    procedure cbTracksClick(Sender: TObject);
    procedure sbSaveFileClick(Sender: TObject);
    procedure acConvertExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure acFilterSelectExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure acHelpAboutExecute(Sender: TObject);
    procedure chbInputDeviceClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chbOutputDeviceClick(Sender: TObject);
    procedure acHelpReadmeExecute(Sender: TObject);
    procedure mnuSynthesizeShortNamesClick(Sender: TObject);
    procedure edOutputFileKeyPress(Sender: TObject; var Key: Char);
    procedure cbInputFormatDeviceChange(Sender: TObject);
    procedure cbOutputFormatDeviceChange(Sender: TObject);
    procedure cbInputFormatChange(Sender: TObject);
    procedure cbOutputFormatChange(Sender: TObject);
    procedure acOptionsSourceFormatExecute(Sender: TObject);
    procedure acOptionsTargetFormatExecute(Sender: TObject);
    procedure btnInputOptsClick(Sender: TObject);
    procedure acFileClearMemoExecute(Sender: TObject);
    procedure acFinalizeDropDownsExecute(Sender: TObject);
    procedure mnuOptionsForceDataTypeClick(Sender: TObject);
    procedure acOptionsEnableCharactersetTransformationExecute(
      Sender: TObject);
    procedure acFileOutputToScreenExecute(Sender: TObject);
    procedure acDebugCreatePoExecute(Sender: TObject);
    procedure acFileChangeLanguageExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    FCaps: TCapabilities;
    FOpts: TOptions;
    FLang: TStringList;
    FFirstShow: Boolean;
    FOutHandmade: Boolean;
    procedure AddToOutput(const Str: string);
    procedure AddToOutputFmt(const Format: string; const Args: array of const);
    procedure ComboBoxChanged(const Format: string; IsInput, IsFile: Boolean);
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
    procedure EnableOptions(const Version: string);
    function HandleOptions(const Format: string; AObject: TObject; IsInput: Boolean): Boolean;
    function HandleOptionsDlg(const Format: string; var str: string; IsInput: Boolean): Boolean;
    procedure HistoryChanged(Box: TComboBox; Swap: Boolean = False);
    procedure InitCombo(Target: TComboBox; IsInput, ForDevice: Boolean);
    procedure LoadLanguages;
    procedure LoadFileFormats;
    procedure LoadVersion;
    procedure WMOPTIONSCHANGED(var Msg: TMessage); message WM_OPTIONS_CHANGED;
    procedure WMSTARTUP(var Msg: TMessage); message WM_STARTUP;
    procedure StoreProfiles;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  filter, about, readme, options;

{$R *.DFM}

procedure FixAlign(Control: TControl; ShiftLeft: Integer;
  RightOfMe: TControl = nil);
var
  Right: Integer;
begin
  Right := Control.Parent.Left + Control.Parent.Width;

  if Assigned(RightOfMe) then
    ShiftLeft := ShiftLeft + RightOfMe.Width;

  if (akLeft in Control.Anchors) then
    Control.Width := Right - Control.Left - ShiftLeft
  else if (akRight in Control.Anchors) then
    Control.Left := Right - Control.Width - ShiftLeft;
end;

function ComboBoxSelect(AComboBox: TComboBox; const Item: string): Boolean;
var
  i: Integer;
begin
  i := AComboBox.Items.IndexOf(Item);
  AComboBox.ItemIndex := i;
  Result := (i >= 0);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TP_Ignore(mnuDebug, 'mnuDebug');
{$IFOPT D-}
  mnuDebug.Visible := False;
{$ENDIF}
  TranslateComponent(SELF);
  LoadLanguages;

  FFirstShow := True;

// VS_FF_DEBUG	The file contains debugging information or is compiled with debugging features enabled.
// VS_FF_INFOINFERRED	The file's version structure was created dynamically;
//                      therefore, some of the members in this structure may be empty or incorrect.
//                      This flag should never be set in a file's VS_VERSION_INFO data.
// VS_FF_PATCHED	The file has been modified and is not identical to the original shipping file of the same version number.
// VS_FF_PRERELEASE	The file is a development version, not a commercially released product.
// VS_FF_PRIVATEBUILD	The file was not built using standard release procedures. If this flag is set,
//                      the StringFileInfo structure should contain a PrivateBuild entry.
// VS_FF_SPECIALBUILD	The file was built by the original company using standard release procedures
//                      but is a variation of the normal file of the same version number.
//                      If this flag is set, the StringFileInfo structure should contain a SpecialBuild

  if (CFixedFileinfo.dwFileFlags and VS_FF_DEBUG <> 0) then
    Caption := Format('%s (%s)', [Caption, _('Internal development release')])
  else if (CFixedFileinfo.dwFileFlags and VS_FF_PRERELEASE <> 0) then
    Caption := Format('%s (%s)', [Caption, _('BETA')])
  else if (CFixedFileinfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0) then
    Caption := Format('%s (%s)', [Caption, _('Private release')])
  else if (CFixedFileinfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0) then
    Caption := Format('%s (%s)', [Caption, _('Special release')]);

  memoOutput.Lines.Clear;

  FCaps := TCapabilities.Create;
  FOpts := TOptions.Create(FCaps);

  OpenDialog.InitialDir := ReadProfile(OpenDialog.Tag);
  SaveDialog.InitialDir := ReadProfile(SaveDialog.Tag);

  if not ComboBoxSelect(cbInputDevice, ReadProfile(cbInputDevice.Tag)) then
    cbInputDevice.ItemIndex := 0;

  if not ComboBoxSelect(cbOutputDevice, ReadProfile(cbOutputDevice.Tag)) then
    cbOutputDevice.ItemIndex := 0;

  FixAlign(sbOpenFile, 8);
  FixAlign(sbSaveFile, 8);
  FixAlign(edInputFile, 8, sbOpenFile);
  FixAlign(edOutputFile, 8, sbSaveFile);

  FixAlign(cbInputLang, 8);
  btnInputOpts.Left := lbInputOpts.Left + lbInputOpts.Width + 8;
  edInputOpts.Left := btnInputOpts.Left + btnInputOpts.Width + 4;
  edInputOpts.Width := cbInputLang.Left - edInputOpts.Left - 4;

  FixAlign(cbOutputLang, 8);
  btnOutputOpts.Left := lbOutputOpts.Left + lbOutputOpts.Width + 8;
  edOutputOpts.Left := btnOutputOpts.Left + btnOutputOpts.Width + 4;
  edOutputOpts.Width := cbOutputLang.Left - edOutputOpts.Left - 4;

  FixAlign(btnProcess, 8);
  FixAlign(btnFilter, 16, btnProcess);

  edInputFile.Text := ReadProfile(edInputFile.Tag);

  cbInputLang.ItemIndex := 0;
  cbOutputLang.ItemIndex := 0;

  Application.OnIdle := Self.DoOnIdle;

  gbInput.Caption := '>>> ' + _('Input');
  gbOutput.Caption := '<<< ' + _('Output');

  chbInputDevice.Caption := '[' + chbInputDevice.Caption + ']';
  chbOutputDevice.Caption := '[' + chbOutputDevice.Caption + ']';
end;

procedure TfrmMain.LoadLanguages;
begin
  FLang := TStringList.Create;

  DefaultInstance.GetListOfLanguages('default', FLang);
  if (FLang.IndexOf('en') < 0) then FLang.Add('en');
  acFileChangeLanguage.Visible := (FLang.Count > 1);
end;

procedure TfrmMain.LoadFileFormats;
var
  l: TStrings;
begin
  l := TStringList.Create;
  try

    if (gpsbabel_vfmt >= '001.002.008') then
      gpsbabel('-^3', l)
    else if (gpsbabel_vfmt >= '001.002.005') then
      gpsbabel('-^2', l)
    else begin
      MessageDlg(_('The file "gpsbabel.exe" found in current directory is too old!'),
        mtError, [mbOK], 0);
      Halt(1);
    end;

    FCaps.List := l;
    FOpts.List := l;
    InitCombo(cbInputFormatDevice, False, True);
    InitCombo(cbOutputFormatDevice, True, True);
    InitCombo(cbInputFormat, True, False);
    InitCombo(cbOutputFormat, False, False);
  finally
    l.Free;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not(FFirstShow) then Exit;
  
  FFirstShow := False;
  PostMessage(SELF.Handle, WM_STARTUP, 0, 0); // keep sure our window is visible
end;

procedure TfrmMain.WMSTARTUP(var Msg: TMessage);
begin
  LoadVersion;
  EnableOptions(gpsbabel_vfmt);
  LoadFileFormats;

  // ? valid README form

  acHelpReadme.Enabled := (frmReadme.Memo.Lines.Count > 0);
end;

procedure TfrmMain.InitCombo(Target: TComboBox; IsInput, ForDevice: Boolean);
var
  i: Integer;
  OK: Boolean;
  s: string;
begin
  for i := 0 to FCaps.Count - 1 do
  begin
    if (ForDevice and not(FCaps.IsDevice(i))) then Continue;
    if not(ForDevice) and not FCaps.IsFile(i) then Continue;

    if (IsInput) then
      OK := FCaps.CanReadAny(i)
    else
      OK := FCaps.CanWriteAny(i);
    if OK then
      Target.Items.Add(FCaps.GetDescr(i));
  end;

  s := ReadProfile(Target.Tag);
  ComboBoxSelect(Target, s);

  ComboBoxChanged(Target.Text, IsInput, not(ForDevice));
end;

procedure TfrmMain.OpenButtonClick(Sender: TObject);
var
  s: string;
begin
  OpenDialog.Filter := '';
  OpenDialog.DefaultExt := '*.*';

  if (cbInputFormat.Text <> '') then
    s := cbInputFormat.Text + '|*.' + FCaps.GetExt(cbInputFormat.Text) + '|';
  s := s + _('All files|*.*');

  OpenDialog.Filter := s;
  if not SELF.OpenDialog.Execute then Exit;

  edInputFile.Text := OpenDialog.FileName;
  CheckInput;
end;

procedure TfrmMain.ComboBoxChanged(const Format: string; IsInput, IsFile: Boolean);
var
  caps: Integer;
  ext: string;
  ac: TAction;
begin
  caps := FCaps.GetCaps(Format);
  ext := FCaps.GetExt(Format);
  if IsFile and FOutHandmade and (ext = '') then
  begin
    ext := SysUtils.ExtractFileExt(edOutputFile.Text);
    if (ext <> '') and (ext[1] = '.') then Delete(ext, 1, 1);
  end;

  if IsInput then
  begin
    wptInputOK.Enabled := (caps and 1 <> 0);
    trkInputOK.Enabled := (caps and 4 <> 0);
    rteInputOK.Enabled := (caps and 16 <> 0);
  end
    else
  begin
    wptOutputOK.Enabled := (caps and 2 <> 0);
    trkOutputOK.Enabled := (caps and 8 <> 0);
    rteOutputOK.Enabled := (caps and 32 <> 0);
    if IsFile and (edOutputFile.Text <> '') and (edOutputFile.Text <> '-') then
    begin
      if (ext <> '') then FOutHandmade := False;
      edOutputFile.Text := SysUtils.ChangeFileExt(edOutputFile.Text, '.' + ext);
    end;
  end;

  CheckInput;

  if IsInput then
  begin
    edInputOpts.Text := '';
    edInputOpts.Items.Clear;
    
    ac := acOptionsSourceFormat;
    acOptionsSourceFormat.Caption := _('Input') + ': ' + Format;
    btnInputOpts.Caption := '';
//  ImageList1.GetBitmap(11, btnInputOpts.Glyph);
  end
  else begin
    edOutputOpts.Text := '';
    edOutputOpts.Items.Clear;

    ac := acOptionsTargetFormat;
    acOptionsTargetFormat.Caption := _('Output') + ': ' + Format;
    btnOutputOpts.Caption := '';
//  ImageList1.GetBitmap(11, btnOutputOpts.Glyph);
  end;

  ac.Enabled := FOpts.HasFormatOpts(Format);
  if ac.Enabled then
  begin
    ac.Hint := SysUtils.Format(_('Select and edit options for "%s"'), [Format]);
  end
    else
  begin
    ac.Hint := SysUtils.Format(_('No options available for "%s"'), [Format]);
  end;
end;

procedure TfrmMain.edInputFileChange(Sender: TObject);
begin
  CheckInput;
end;

procedure TfrmMain.CheckInput;
begin
  acConvert.Enabled :=
    ((chbInputDevice.Checked and
    (cbInputDevice.Text <> '') and
    (cbInputFormatDevice.Text <> ''))
  or
    (not(chbInputDevice.Checked) and
    (edInputFile.Text <> '') and
    (cbInputFormat.Text <> '')))
  and
    ((chbOutputDevice.Checked and
    (cbOutputDevice.Text <> '') and
    (cbOutputFormatDevice.Text <> ''))
  or
    (not(chbOutputDevice.Checked) and
    (edOutputFile.Text <> '') and
    (cbOutputFormat.Text <> '')));
end;

procedure TfrmMain.edOutputFileChange(Sender: TObject);
begin
  CheckInput;
end;

procedure TfrmMain.cbWaypointsClick(Sender: TObject);
begin
  CheckInput;
end;

procedure TfrmMain.cbRoutesClick(Sender: TObject);
begin
  CheckInput;
end;

procedure TfrmMain.cbTracksClick(Sender: TObject);
begin
  CheckInput;
end;

procedure TfrmMain.sbSaveFileClick(Sender: TObject);
var
  s: string;
begin
  SaveDialog.Filter := '';
  SaveDialog.DefaultExt := '*.*';

  if (cbOutputFormat.Text <> '') then
    s := cbOutputFormat.Text + '|*.' + FCaps.GetExt(cbOutputFormat.Text) + '|';
  s := s + _('All files|*.*');

  SaveDialog.Filter := s;
  if not SELF.SaveDialog.Execute then Exit;

  edOutputFile.Text := SaveDialog.FileName;
  CheckInput;
end;

procedure TfrmMain.acConvertExecute(Sender: TObject);
var
  cmdline: string;
  list: TStrings;
  CSave: TCursor;
  str: TStream;
  s: string;
  i: Integer;
  IFormat, OFormat: string;
  Fatal: Boolean;

begin
  btnProcess.Enabled := False;
  try
    acFinalizeDropDownsExecute(nil);

    cmdline := '';

    if chbInputDevice.Checked then
      IFormat := FCaps.GetName(cbInputFormatDevice.Text)
    else
      IFormat := FCaps.GetName(cbInputFormat.Text);
    if chbOutputDevice.Checked then
      OFormat := FCaps.GetName(cbOutputFormatDevice.Text)
    else
      OFormat := FCaps.GetName(cbOutputFormat.Text);

    if cbWaypoints.Checked then cmdline := cmdline + ' -w';
    if cbRoutes.Checked then cmdline := cmdline + ' -r';
    if cbTracks.Checked then cmdline := cmdline + ' -t';

    if mnuSynthesizeShortNames.Checked then cmdline := cmdline + ' -s';

    if chbInputDevice.Checked then
      s := SysUtils.AnsiLowerCase(cbInputDevice.Text) + ':'
    else begin
      s := edInputFile.Text;
      if not(FileExists(s)) then
      raise eGPSBabelError.CreateFmt(_('File %s not found.'), [s]);
      s := '"' + s + '"';
    end;

    // Input character set

    if acOptionsEnableCharactersetTransformation.Checked and
       (cbInputLang.ItemIndex > 0) then
      cmdline := Format('%s -c %s',
        [cmdline, cbInputLang.Text]);

    if (Trim(edInputOpts.Text) <> '') then
      cmdline := Format('%s -i %s,%s -f %s',
        [cmdline, IFormat, Trim(edInputOpts.Text), s])
    else
      cmdline := Format('%s -i %s -f %s',
        [cmdline, IFormat, s]);

    if mnuOptionsForceDataType.Checked then
    begin
      s := '';
      if not(cbWaypoints.Checked) then
        s := s + ',waypoints'; 
      if not(cbRoutes.Checked) then
        s := s + ',routes';
      if not(cbTracks.Checked) then
        s := s + ',tracks';
      if (s <> '') then
        cmdline := Format('%s -x nuketypes%s', [cmdline, s]);
    end;

    // Add filter options

    if (frmFilter <> nil) then
      cmdline := cmdline + frmFilter.CmdLine;

    // Output character set

    if acOptionsEnableCharactersetTransformation.Checked and
       (cbOutputLang.ItemIndex > 0) then
      cmdline := Format('%s -c %s',
        [cmdline, cbOutputLang.Text]);

    if (chbOutputDevice.Checked) then
    begin
      if (cbOutputDevice.Text = 'SCREEN') then
        s := '-'
      else
        s := cbOutputDevice.Text + ':'
    end
    else begin
      s := edOutputFile.Text;

      if (s <> '-') then
      begin
        if FileExists(s) then
        begin
          if (Windows.MessageBox(SELF.Handle,
            PChar(Format(_('File "%s" exists ! Overwrite ?'), [s])),
            PChar(_('Warning')), MB_YESNO) <> IDYES) then Exit;
        end
          else
        begin
          str := TFileStream.Create(s, fmCreate);
          str.Free;
        end;
        s := '"' + s + '"';
      end
      else
        s := '-';
    end;

    if (Trim(edOutputOpts.Text) <> '') then
      cmdline := Format('%s -o %s,%s -F %s',
        [cmdline, OFormat, Trim(edOutputOpts.Text), s])
    else
      cmdline := Format('%s -o %s -F %s',
        [cmdline, OFormat, s]);

    while (cmdline[1] = ' ') do System.Delete(cmdline, 1, 1);

    s := 'gpsbabel.exe ' + cmdline;
    AddToOutput(s);

    list := TStringList.Create;
    try
      CSave := Cursor;
      Cursor := crHourGlass;
      Application.ProcessMessages;
      Sleep(50);

      if not gpsbabel(cmdline, list, @Fatal, False) then
        raise eGPSBabelError.Create(_('Could not run "gpsbabel.exe"!'));

      if (list.Count > 0) then
      begin
        AddToOutput('');
        AddToOutput(string(list.GetText));
      end;

      if (Fatal) then
        MessageDlg(_('Sorry, gpsbabel.exe reported problems!'), mtError, [mbOK], 0)
      else
        MessageDlg(Format(_('Converted successfully from "%s" to "%s".'), [IFormat, OFormat]),
          mtInformation, [mbOK], 0);

    finally

      Cursor := CSave;
      list.Free;

    end;

  finally
    btnProcess.Enabled := True;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TfrmMain.acFilterSelectExecute(Sender: TObject);
begin
  if (frmFilter = nil) then
    Application.CreateForm(TfrmFilter, frmFilter);
  if not(frmFilter.ShowModal = mrOk) then Exit;
end;

procedure TfrmMain.AddToOutput(const Str: string);
begin
  memoOutput.Lines.Add(Str);
end;

procedure TfrmMain.AddToOutputFmt(const Format: string;
  const Args: array of const);
begin
  AddToOutput(SysUtils.Format(Format, Args));
end;

procedure TfrmMain.acFileExitExecute(Sender: TObject);
begin
  PostMessage(SELF.Handle, WM_CLOSE, 0, 0);
end;

procedure TfrmMain.LoadVersion;
var
  l, l2: TStringList;
  i: Integer;
  s: string;
  cx: PChar;
  
  procedure SpaceDelimited(const str: string; list: TStrings);
  var
    s: string;
    cin, cend, cx: PChar;
  begin
    s := Trim(str);
    cin := PChar(s);
    cend := cin + StrLen(cin);
    while (cin < cend) do
    begin
      while (cin^ = ' ') do cin := cin + 1;
      cx := StrScan(cin, ' ');
      if (cx = nil) then cx := cend;
      cx^ := #0;
      list.Add(string(cin));
      cin := cx + 1;
    end;
  end;

begin
  gpsbabel_major := 0;
  gpsbabel_minor := 0;
  gpsbabel_release := 0;
  
  l := TStringList.Create;
  try
    if not gpsbabel('-V', l) then Exit;

    for i := 0 to l.Count - 1 do
    begin
      s := Trim(l.Strings[i]);
      if (Copy(AnsiUpperCase(s), 1, 8) = 'GPSBABEL') then
      begin
        l2 := TStringList.Create;
        try
          SpaceDelimited(s, l2);
          gpsbabel_version := l2[2];
          cx := PChar(gpsbabel_version);
          gpsbabel_major := atoi(cx);
          cx := StrScan(cx, '.');
          if (cx <> nil) then
          begin
            gpsbabel_minor := atoi(cx + 1);
            cx := StrScan(cx + 1, '.');
            if (cx <> nil) then
              gpsbabel_release := atoi(cx + 1);
          end;

          gpsbabel_vfmt := Format('%3.3d.%3.3d.%3.3d', [
            gpsbabel_major, gpsbabel_minor, gpsbabel_release]);

          s := Format(_('GPSBabel, version %s'), [gpsbabel_version]);
          stbMain.Panels[0].Text := s;
          stbMain.Panels[0].Width := stbMain.Canvas.TextWidth(s) +
            Trunc((Length(s)* 1.5));
          Break;
        finally
          l2.Free;
        end;
      end;
    end;

  finally
    l.Free;
  end;
end;

procedure TfrmMain.acHelpAboutExecute(Sender: TObject);
begin
  if (frmAbout = nil) then
    Application.CreateForm(TfrmAbout, frmAbout);
  frmAbout.ShowModal;
end;

procedure TfrmMain.chbInputDeviceClick(Sender: TObject);
begin
  if not(Sender is TCheckBox) then Exit;

  if TCheckBox(Sender).Checked then
  begin
    edInputFile.Visible := False;
    sbOpenFile.Visible := False;
    cbInputFormat.Visible := False;
    cbInputDevice.Visible := True;
    cbInputFormatDevice.Visible := True;
    lbInputFile.Caption := _('Port');
  end
    else
  begin
    cbInputFormat.Visible := True;
    cbInputFormatDevice.Visible := False;
    cbInputDevice.Visible := False;
    edInputFile.Visible := True;
    cbInputDevice.Visible := False;
    sbOpenFile.Visible := True;
    lbInputFile.Caption := _('File');
  end;
  CheckInput;
end;

procedure TfrmMain.StoreProfiles;
var
  s: string;
begin
  s := SysUtils.ExtractFilePath(edInputFile.Text);
  if (s <> '') then
    StoreProfile(OpenDialog.Tag, s);
  s := SysUtils.ExtractFilePath(edOutputFile.Text);
  if (s <> '') then
    StoreProfile(SaveDialog.Tag, s);
  StoreProfile(cbInputFormat.Tag, cbInputFormat.Text);
  StoreProfile(cbOutputFormat.Tag, cbOutputFormat.Text);
  StoreProfile(cbInputDevice.Tag, cbInputDevice.Text);
  StoreProfile(cbInputFormatDevice.Tag, cbInputFormatDevice.Text); 
  StoreProfile(cbOutputDevice.Tag, cbOutputDevice.Text);
  StoreProfile(cbOutputFormatDevice.Tag, cbOutputFormatDevice.Text);
  StoreProfile(edInputFile.Tag, edInputFile.Text);
  StoreProfile(edOutputFile.Tag, edOutputFile.Text);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StoreProfiles;
end;

procedure TfrmMain.chbOutputDeviceClick(Sender: TObject);
begin
  if not(Sender is TCheckBox) then Exit;

  if TCheckBox(Sender).Checked then
  begin
    cbOutputFormatDevice.Visible := True;
    cbOutputDevice.Visible := True;
    edOutputFile.Visible := False;
    sbSaveFile.Visible := False;
    cbOutputFormat.Visible := False;
    lbOutputFile.Caption := _('Port');
  end
    else
  begin
    cbOutputFormat.Visible := True;
    sbSaveFile.Visible := True;
    edOutputFile.Visible := True;
   cbOutputDevice.Visible := False;
    cbOutputFormatDevice.Visible := False;
    lbOutputFile.Caption := _('File');
  end;
  CheckInput;
end;

procedure TfrmMain.acHelpReadmeExecute(Sender: TObject);
begin
  frmReadme.ShowModal;
end;

procedure TfrmMain.mnuSynthesizeShortNamesClick(Sender: TObject);
begin
  mnuSynthesizeShortNames.Checked := not(mnuSynthesizeShortNames.Checked);
end;

procedure TfrmMain.edOutputFileKeyPress(Sender: TObject; var Key: Char);
begin
  FOutHandmade := True;
end;

procedure TfrmMain.cbInputFormatDeviceChange(Sender: TObject);
begin
  ComboBoxChanged(cbInputFormatDevice.Text, True, False);
end;

procedure TfrmMain.cbOutputFormatDeviceChange(Sender: TObject);
begin
  ComboBoxChanged(cbOutputFormatDevice.Text, False, False);
end;

procedure TfrmMain.cbInputFormatChange(Sender: TObject);
begin
  ComboBoxChanged(cbInputFormat.Text, True, True);
end;

procedure TfrmMain.cbOutputFormatChange(Sender: TObject);
begin
  ComboBoxChanged(cbOutputFormat.Text, False, True);
end;

procedure TfrmMain.acOptionsSourceFormatExecute(Sender: TObject);
begin
  if chbInputDevice.Checked then
    HandleOptions(cbInputFormatDevice.Text, edInputOpts, True)
  else
    HandleOptions(cbInputFormat.Text, edInputOpts, True)
end;

procedure TfrmMain.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
  inherited;
  acFileClearMemo.Enabled := (memoOutput.Lines.Count > 0);
  Done := True;
end;

procedure TfrmMain.EnableOptions(const Version: string);
begin
    if (Version >= '001.002.008') then
      mnuOptionsForceDataType.Enabled := True;
    if (version >= '001.002.007') then
      acOptionsEnableCharactersetTransformation.Enabled := True;
end;

function TfrmMain.HandleOptions(const Format: string; AObject: TObject; IsInput: Boolean): Boolean;
var
  s: string;
  ok: Boolean;
begin
  s := GetStrProp(AObject, 'Text');
  if HandleOptionsDlg(Format, s, IsInput) then
  begin
    SetStrProp(AObject, 'Text', s);
    Result := True;
  end
  else
    Result := False;
end;

function TfrmMain.HandleOptionsDlg(const Format: string; var str: string; IsInput: Boolean): Boolean;
begin
  Application.CreateForm(TfrmOptions, frmOptions);
  try
    frmOptions.Caption := SysUtils.Format(_('Options for "%s"'), [Format]);
    frmOptions.FIsInput := IsInput;
    frmOptions.Opts := FOpts.FormatOpts(Format);
    frmOptions.OptsStr := str;
    Result := (frmOptions.ShowModal = mrOk);
    if (Result) then
    begin
      str := frmOptions.OptsStr;
      PostMessage(Self.Handle, WM_OPTIONS_CHANGED, 0, 0);
    end;
  finally
    frmOptions.Release;
  end;
end;

procedure TfrmMain.acOptionsTargetFormatExecute(Sender: TObject);
begin
  if chbOutputDevice.Checked then
    HandleOptions(cbOutputFormatDevice.Text, edOutputOpts, False)
  else
    HandleOptions(cbOutputFormat.Text, edOutputOpts, False);
end;

procedure TfrmMain.btnInputOptsClick(Sender: TObject);
begin
  acOptionsSourceFormat.Execute;
end;

procedure TfrmMain.acFileClearMemoExecute(Sender: TObject);
begin
  memoOutput.Clear;
end;

procedure TfrmMain.acFinalizeDropDownsExecute(Sender: TObject);
var
 i, j: Integer;
 c: TComponent;
 cb: TComboBox;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];
    if not(c is TComboBox) then Continue;
    cb := Pointer(c);
    if (cb.Style <> csDropDown) or (cb.Text = '') then Continue;
    j := cb.Items.IndexOf(cb.Text);
    if (j < 0) then
      cb.Items.Insert(0, cb.Text);
  end;
end;

procedure TfrmMain.WMOPTIONSCHANGED(var Msg: TMessage);
begin
  acFinalizeDropDowns.Execute;
end;

procedure TfrmMain.mnuOptionsForceDataTypeClick(Sender: TObject);
begin
  mnuOptionsForceDataType.Checked := not(mnuOptionsForceDataType.Checked);
end;

procedure TfrmMain.acOptionsEnableCharactersetTransformationExecute(
  Sender: TObject);
begin
  acOptionsEnableCharactersetTransformation.Checked := not (
    acOptionsEnableCharactersetTransformation.Checked);

  cbInputLang.Enabled := acOptionsEnableCharactersetTransformation.Checked;
  cbOutputLang.Enabled := acOptionsEnableCharactersetTransformation.Checked;
end;

procedure TfrmMain.acFileOutputToScreenExecute(Sender: TObject);
begin
  if (chbOutputDevice.Checked) then
  begin
    chbOutputDevice.Checked := False;
    chbOutputDeviceClick(chbOutputDevice);
    Application.ProcessMessages;
  end;

  acFileOutputToScreen.Checked := not (acFileOutputToScreen.Checked);
  if acFileOutputToScreen.Checked then
  begin
    chbOutputDevice.Enabled := False;
    HistoryChanged(edOutputFile);
    edOutputFile.Text := '-';
    edOutputFile.Enabled := False;
    sbSaveFile.Enabled := False;
  end
    else
  begin
    chbOutputDevice.Enabled := True;
    edOutputFile.Enabled := True;
    HistoryChanged(edOutputFile, True);
    sbSaveFile.Enabled := True;
  end;
  CheckInput;
end;

procedure TfrmMain.HistoryChanged(Box: TComboBox; Swap: Boolean);
var
  index: Integer;
  item: string;
begin
  item := Trim(Box.Text);
  if (item <> '') then
  begin
    index := Box.Items.IndexOf(item);
    if (index < 0) then
      Box.Items.Insert(0, item)
    else
      Box.Items.Move(index, 0);
  end;
  if (swap and (Box.Items.Count > 1)) then
    Box.ItemIndex := 1;
end;

procedure TfrmMain.acDebugCreatePoExecute(Sender: TObject);
var
  l: TStringList;
  i, j, len: Integer;
  s: string;
  f: TFileStream;
begin
  l := TStringList.Create;
  try
    FOpts.DebugGetHints(l);
    f := TFileStream.Create('options\options.inc', fmCreate);
    try
      s := '{gnugetext: scan-all text-domain=''options''}'#13#10#13#10;
      f.Write(PChar(s)^, Length(s));
      s := 'const'#13#10;
      f.Write(PChar(s)^, Length(s));

      for i := 0 to l.Count - 1 do
      begin
        s := l.Strings[i];
        len := Length(s);
        for j := len downto 1 do
        begin
          if (s[j] = '''') then
          begin
            Insert('''', s, j);
          end;
        end;
        s := Format('resourcestring option_%d=''%s'';'#13#10, [i, s]);
        f.Write(PChar(s)^, Length(s));
      end;

      s := #13#10'{gnugettext: reset }'#13#10;
      f.Write(PChar(s)^, Length(s));
    finally
      f.Free;
    end;
  finally
    l.Free;
  end;
end;

procedure TfrmMain.acFileChangeLanguageExecute(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  for i := 0 to FLang.Count - 1 do
  begin
    s := FLang.Strings[i];
    if (s = '') then
      Halt(1);
  end;
end;

end.
