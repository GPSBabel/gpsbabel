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
  gnugettextD4,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  common, utils, ImgList, ActnList, Menus, ComCtrls;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    sbOpenFile: TSpeedButton;
    cbInputFormat: TComboBox;
    cbOutputFormat: TComboBox;
    edOutputFile: TEdit;
    sbSaveFile: TSpeedButton;
    lbInputFile: TLabel;
    lbOutputFile: TLabel;
    lbInputFormat: TLabel;
    lbOutputFormat: TLabel;
    edInputFile: TEdit;
    Panel2: TPanel;
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
    chbInputDevice: TCheckBox;
    cbInputDevice: TComboBox;
    cbInputFormatDevice: TComboBox;
    cbOutputFormatDevice: TComboBox;
    chbOutputDevice: TCheckBox;
    cbOutputDevice: TComboBox;
    mnuReadme: TMenuItem;
    acHelpReadme: TAction;
    N1: TMenuItem;
    mnuOptions: TMenuItem;
    mnuSynthesizeShortNames: TMenuItem;
    edInputOpts: TEdit;
    lbInputOpts: TLabel;
    lbOutputOpts: TLabel;
    edOutputOpts: TEdit;
    Filter1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure ComboChange(Sender: TObject);
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
  private
    { Private-Deklarationen }
    FCaps: TCapabilities;
    FFirstShow: Boolean;
    FOutHandmade: Boolean;
    procedure AddToOutput(const Str: string);
    procedure AddToOutputFmt(const Format: string; const Args: array of const);
    procedure InitCombo(Target: TComboBox; ForRead, ForDevice: Boolean);
    procedure LoadFileFormats;
    procedure LoadVersion;
    procedure WMSTARTUP(var Msg: TMessage); message WM_STARTUP;
    procedure StoreProfiles;
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses filter, about, readme;

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
var
  s: string;
begin
  gnugettextD4.TranslateComponent(SELF);

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

  if (CFixedFileinfo.dwFileFlags and VS_FF_PRERELEASE <> 0) then
    Caption := Format('%s (%s)', [Caption, _('BETA')])
  else if (CFixedFileinfo.dwFileFlags and VS_FF_PRIVATEBUILD <> 0) then
    Caption := Format('%s (%s)', [Caption, _('Private release')])
  else if (CFixedFileinfo.dwFileFlags and VS_FF_SPECIALBUILD <> 0) then
    Caption := Format('%s (%s)', [Caption, _('Special release')]);


  FCaps := TCapabilities.Create;

  OpenDialog.InitialDir := ReadProfile(OpenDialog.Tag);
  SaveDialog.InitialDir := ReadProfile(SaveDialog.Tag);

  if not ComboBoxSelect(cbInputDevice, ReadProfile(cbInputDevice.Tag)) then
    cbInputDevice.ItemIndex := 0;

  if not ComboBoxSelect(cbOutputDevice, ReadProfile(cbOutputDevice.Tag)) then
    cbOutputDevice.ItemIndex := 0;

  FFirstShow := True;

  FixAlign(sbOpenFile, 8);
  FixAlign(sbSaveFile, 8);
  edInputOpts.Left := lbInputOpts.Left + lbInputOpts.Width + 8;
  edOutputOpts.Left := lbOutputOpts.Left + lbOutputOpts.Width + 8;
  FixAlign(edInputOpts, 8);
  FixAlign(edOutputOpts, 8);
  FixAlign(btnProcess, 8);
  FixAlign(btnFilter, 16, btnProcess);
  FixAlign(edInputFile, 8, sbOpenFile);
  FixAlign(edOutputFile, 8, sbSaveFile);
end;

procedure TfrmMain.LoadFileFormats;
var
  l: TStrings;
begin
  l := TStringList.Create;
  try
    gpsbabel('-^2', l);
    FCaps.List := l;
    InitCombo(cbInputFormat, True, False);
    InitCombo(cbOutputFormat, False, False);
    InitCombo(cbInputFormatDevice, False, True);
    InitCombo(cbOutputFormatDevice, True, True);
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
  LoadFileFormats;

  // ? valid README form

  acHelpReadme.Enabled := (frmReadme.Memo.Lines.Count > 0);
end;

procedure TfrmMain.InitCombo(Target: TComboBox; ForRead, ForDevice: Boolean);
var
  i: Integer;
  OK: Boolean;
  s: string;
begin
  for i := 0 to FCaps.Count - 1 do
  begin
    if (ForDevice and not(FCaps.IsDevice(i))) then Continue;
    if not(ForDevice) and not FCaps.IsFile(i) then Continue;

    if (ForRead) then
      OK := FCaps.CanReadAny(i)
    else
      OK := FCaps.CanWriteAny(i);
    if OK then
      Target.Items.Add(FCaps.GetDescr(i));
  end;

  s := ReadProfile(Target.Tag);
  ComboBoxSelect(Target, s);

  ComboChange(Target);
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
end;

procedure TfrmMain.ComboChange(Sender: TObject);
var
  caps: Integer;
  ext: string;
begin
  caps := FCaps.GetCaps(TComboBox(Sender).Text);
  ext := FCaps.GetExt(TComboBox(Sender).Text);
  if FOutHandmade and (ext = '') then
  begin
    ext := SysUtils.ExtractFileExt(edOutputFile.Text);
    if (ext <> '') and (ext[1] = '.') then Delete(ext, 1, 1);
  end;

  if (Sender = cbInputFormat) then
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
    if (edOutputFile.Text <> '') then
    begin
      if (ext <> '') then FOutHandmade := False;
      edOutputFile.Text := SysUtils.ChangeFileExt(edOutputFile.Text, '.' + ext);
    end;
  end;
  CheckInput;
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

begin
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

  if (Trim(edInputOpts.Text) <> '') then
    cmdline := Format('%s -i %s,%s -f %s',
      [cmdline, IFormat, Trim(edInputOpts.Text), s])
  else
    cmdline := Format('%s -i %s -f %s',
      [cmdline, IFormat, s]);

  cmdline := cmdline + frmFilter.CmdLine;

  if (chbOutputDevice.Checked) then
    s := cbOutputDevice.Text + ':'
  else begin
    s := edOutputFile.Text;

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
  end;

  if (Trim(edOutputOpts.Text) <> '') then
    cmdline := Format('%s -o %s,%s -F %s',
      [cmdline, OFormat, Trim(edOutputOpts.Text), s])
  else
    cmdline := Format('%s -o %s -F %s',
      [cmdline, OFormat, s]);

  while (cmdline[1] = ' ') do System.Delete(cmdline, 1, 1);

  AddToOutput('gpsbabel.exe ' + cmdline);

  list := TStringList.Create;
  try
    CSave := Cursor;
    Cursor := crHourGlass;
    Application.ProcessMessages;
    Sleep(50);

    if not gpsbabel(cmdline, list) then
      raise eGPSBabelError.Create(_('Could not run "gpsbabel.exe"!'));

    if (list.Count > 0) then
    begin
      AddToOutput('');
      AddToOutput(string(list.GetText));
    end;

    MessageBox(SELF.Handle,
      PChar(Format(_('Converted successfully from "%s" to "%s".'), [IFormat, OFormat])),
      PChar(_('Success')), MB_OK);

  finally

    Cursor := CSave;
    list.Free;

  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
end;

procedure TfrmMain.acFilterSelectExecute(Sender: TObject);
begin
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
  l: TStrings;
  i: Integer;
  s: string;
begin
  l := TStringList.Create;
  try
    if not gpsbabel('-V', l) then Exit;

    for i := 0 to l.Count - 1 do
    begin
      s := Trim(l.Strings[i]);
      if (Copy(AnsiUpperCase(s), 1, 8) = 'GPSBABEL') then
      begin
        stbMain.Panels[0].Text := s;
        stbMain.Panels[0].Width := stbMain.Canvas.TextWidth(s) + 32;
      end;
    end;

  finally
    l.Free;
  end;
end;

procedure TfrmMain.acHelpAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.chbInputDeviceClick(Sender: TObject);
begin
  if not(Sender is TCheckBox) then Exit;

  if TCheckBox(Sender).Checked then
  begin
    edInputFile.Visible := False;
    sbOpenFile.Visible := False;
    cbInputDevice.Visible := True;
    cbInputFormatDevice.Visible := True;
  end
    else
  begin
    cbInputFormatDevice.Visible := False;
    cbInputDevice.Visible := False;
    edInputFile.Visible := True;
    sbOpenFile.Visible := True;
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
    edOutputFile.Visible := False;
    sbSaveFile.Visible := False;
    cbOutputFormatDevice.Visible := True;
    cbOutputDevice.Visible := True;
  end
    else
  begin
    cbOutputDevice.Visible := False;
    cbOutputFormatDevice.Visible := False;
    sbSaveFile.Visible := True;
    edOutputFile.Visible := True;
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

end.
 