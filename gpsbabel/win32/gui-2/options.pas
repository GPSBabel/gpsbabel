unit options;

{
    Copyright (C) 2005-2007 Olaf Klein, o.b.klein@gpsbabel.org

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
  StdCtrls, ExtCtrls, ComCtrls, Buttons, Registry,
  Common, delphi;

type
  TfrmOptions = class(TForm)
    pnBottom: TPanel;
    pnOptions: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    mmWarning: TMemo;
    procedure CheckBoxClicked(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FOpts: TStringList;
    FCanvas: TCanvas;
    FInitialOpts: string;
    procedure CreateStringOption(const x, y, tag: Integer; o: POption; xmax: Integer = -1);
    procedure CreateIntegerOption(const x, y, tag: Integer; o: POption; xmax: Integer = -1);
    procedure CreateFileOption(const x, y, tag: Integer; o: POption; IsInput: Boolean; xmax: Integer = -1);
    function FindUpDown(AControl: TControl): TUpDown;
    function GetOptsStr: string;
    procedure OptionOpenFile(Sender: TObject);
    procedure OptionSaveFile(Sender: TObject);
    function ParseOptsLine(const Line: string; List: TStrings): Integer;
    procedure SetOpts(AList: TStringList);
    procedure SetOptsStr(const AValue: string);
    procedure StoreOptionsToInifile();
    procedure StoreOptionsToRegistry();
    procedure LoadSettingsFromRegistry();
  public
    { Public declarations }
    FFormat: string;
    FIsInput: Boolean;
    constructor Create(AOwner: TComponent); override;
  property
    Opts: TStringList read FOpts write SetOpts;
    property OptsStr: string read GetOptsStr write SetOptsStr;
  end;

type
  eUnknownOption = class(Exception);
  eParserError = class(Exception);
  
var
  frmOptions: TfrmOptions;

implementation

uses
  utils, main;

{$R *.DFM}

// returns "BottomRight" of Controls rect

function SetCaption(Control: TControl; const ACaption: string): TPoint;
var
  s: TStaticText;
  auto, info: PPropInfo;
  font: TFont;
  parentf: Boolean;
begin
  Result := Control.BoundsRect.BottomRight;

  info := GetPropInfo(Control, 'Caption');
  if (info = nil) then Exit;

  SetStrProp(Control, 'Caption', ACaption);

  auto := GetPropInfo(Control, 'AutoSize');
  if (auto <> nil) then
  begin
    SetOrdProp(Control, auto, Integer(True));
    Result := Control.BoundsRect.BottomRight;
    Exit;
  end;

  info := GetPropInfo(Control, 'Font');
  if (info = nil) then Exit;

  font := Pointer(GetObjectProp(Control, info));

  info := GetPropInfo(Control, 'ParentFont');
  if (info <> nil) then
    parentf := Boolean(GetOrdProp(Control, info)) else
    parentf := False;

// Controls with Caption but without AutoSize
// TCheckBox, TRadioButton

  s := TStaticText.Create(Control.Owner);
  try
    s.Font := font;
    s.ParentFont := parentf;
    s.Visible := False;
    s.Parent := Control.Parent;
    s.BoundsRect := Control.BoundsRect;
    s.AutoSize := True;
    s.Caption := ACaption;
    Control.Width := 18 + s.Width;
    if (Control.Height < s.Height) then
      Control.Height := s.Height;
    Result := Control.BoundsRect.BottomRight;
  finally
    s.Free;
  end;
end;

{ TfrmOptions }

constructor TfrmOptions.Create(AOwner: TComponent); // override;
begin
  inherited Create(AOwner);
  TranslateComponent(Self);
  FCanvas := Main.frmMain.stbMain.Canvas;
  mmWarning.Lines.Add(_('Be aware, that most options are made for the output side. '));
  mmWarning.Lines.Add(_('Currently we don''t have a flag which tells us which direction is used by the options.'));
end;

procedure TfrmOptions.SetOpts(AList: TStringList);
var
  i, j: Integer;
  c: TComponent;
  wc: TControl;
  o: POption;
  chb: TCheckBox;
  xy, _xy: TPoint;
  xmax: Integer;
  lb: TLabel;
  us: string;

begin
  if (AList = nil) then Exit;

  FOpts := AList;

  xy.x := 0;
  xy.y := 8;
  xmax := 0;

  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);

    o.chb := nil;
    o.edit := nil;

    if (o.dir <> 3) then
    begin
      if (FIsInput and (o.dir and 1 = 0)) then Continue
      else if (not(FIsInput) and (o.dir and 2 = 0)) then Continue;
    end;

    if (FFormat = '') then
    begin
      FFormat := o.format;
      btnHelp.Hint := readme_html_path + '#' + FFormat;
      btnHelp.ShowHint := True;
    end;

    us := AnsiLowerCase(o.hint);
    if FIsInput and (AnsiPos('read', us) = 0) and
      (
       (AnsiPos('generate ', us) <> 0) or
       (AnsiPos(' generate', us) <> 0) or
       (AnsiPos('output ', us) <> 0) or
       (AnsiPos(' output', us) <> 0) or
       (AnsiPos('write', us) <> 0) or
       (AnsiPos(' write', us) <> 0)
      ) then Continue;

    chb := TCheckBox.Create(nil);
    o.chb := chb;
    chb.Name := '___' + o.name;
    chb.OnClick := CheckBoxClicked;
    chb.Tag := i + 1;

    InsertComponent(chb);

    chb.ParentFont := False;
    chb.Font := pnOptions.Font;
    chb.Left := 8;
    chb.Top := xy.y;
    _xy := SetCaption(chb, dgettext(GPSBabel_Domain, o.Hint));
    chb.Alignment := taRightJustify;
//  chb.Checked := (gpsbabel_ini.ReadString(o.format, o.name, #1) <> #1);
    chb.Parent := pnOptions;

    chb.Hint := SysUtils.Format(_('Short "%s"'), [o.defname]);
    chb.ShowHint := True;

    if (o.format = 'xcsv') and (o.defname = 'style') then
    begin
      chb.Checked := True;
    end;

    xy.y := xy.y + chb.Height + 8;
    if (o.otype <> 4) then
      if (chb.Width > xy.x) then xy.x := chb.Width;
    if (chb.Width > xy.x) then
      xmax := chb.Width;

    if (o.otype = 4) and (o.def <> nil) and (atoi(o.def) <> 0) then
    begin
      chb.AllowGrayed := True;
      chb.State := cbGrayed;
    end;
  end;

  xy.y := 8;
  xy.X := xy.X + 8;
  if (xy.X < 42) then xy.X := 42;

  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);
    o.edit := nil;
    
    if (o.chb = nil) then Continue;

    // ('unknown', 'integer', 'float', 'string', 'boolean', 'file', 'outfile');
    case o.otype of
      1: CreateIntegerOption(xy.X, xy.Y - 2, i + 1, o, xmax);
      2, 3: CreateStringOption(xy.X, xy.Y - 2, i + 1, o, xmax);
      4: ;
      5: CreateFileOption(xy.X, xy.Y - 2, i + 1, o, True, xmax);
      6: if not FIsInput then CreateFileOption(xy.X, xy.Y - 2, i + 1, o, False, xmax);
    end;
    if (o.edit <> nil) then
      o.edit.Enabled := False;
    xy.y := xy.y + o.chb.Height + 8;
  end;

  xy.X := 0;
  xy.Y := 0;

  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];
    if not c.InheritsFrom(TControl) then Continue;
    if (c is TPanel) then Continue;
    wc := Pointer(c);
    if (wc.Parent <> pnOptions) then Continue;

    j := wc.Left + wc.Width;
    if (j > xy.X) then xy.X := j;
    j := wc.Top + wc.Height;
    if (j > xy.Y) then xy.Y := j;
    if ( wc.Name = '' ) then Continue;
  end;

  Self.Width := xy.X + 8 + (Self.Width - Self.ClientWidth);
  Self.Height := xy.Y + 8 +
    mmWarning.Height + + pnBottom.Height +
    (Self.Height - Self.ClientHeight);

  i := btnCancel.Left - btnOK.Left;
  btnCancel.Left := pnBottom.Width - btnCancel.Width - btnHelp.Left;
  btnOK.Left := btnCancel.Left - i;

  LoadSettingsFromRegistry();
end;

function TfrmOptions.GetOptsStr: string;
var
  i: Integer;
  o: POption;
  c: TComponent;
  s: string;
begin
  Result := '';
  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);
    if (o.chb = nil) then Continue;

    if o.chb.AllowGrayed then
    begin
      if (o.chb.State = cbGrayed) then Continue
    end
    else if not(o.chb.Checked) then Continue;

    if (Result <> '') then
      Result := Result + ',';
    Result := Result + o.defname;

    if (o.edit = nil) then
    begin
      if o.chb.Checked then
        Result := Result + '=1'
      else
        Result := Result + '=0';
      Continue;
    end;
    s := GetStrProp(o.edit, 'Text');
    if (Pos(' ', s) <> 0) or (Pos('"', s) <> 0) or (Pos(',', s) <> 0) then
      s := SysUtils.AnsiQuotedStr(s, '"');
    Result := SysUtils.Format('%s=%s', [Result, s]);
  end;
end;

procedure TfrmOptions.SetOptsStr(const AValue: string);
var
  l: TStrings;
  i, j, k: Integer;
  s, name, value, name_out: string;
  o: POption;
  ud: TUpDown;
begin
  l := TStringList.Create;
  try

    try
      ParseOptsLine(AValue, l);
    except
      on E: exception do
        raise eParserError.Create(_('Invalid line format!'));
    end;

    for i := 0 to l.Count - 1 do
    begin
      s := l.Strings[i];
      j := Pos('=', s);
      if (j > 0) then
      begin
        name := Copy(s, 1, j - 1);
        value := Copy(s, j + 1, Length(s) - j);
      end
        else
      begin
        Name := s;
        Value := '';
      end;
      if (name = '') then Continue;

      j := FOpts.IndexOf(name);
      if (j < 0) then
        raise eUnknownOption.CreateFmt(_('Unknown option "%s"!'), [name])
      else if not(FIsInput) then
      begin
        name_out := name + '_out';
        k := FOpts.IndexOf(name);
        if (k >= 0) then
        begin
          name := name_out;
          j := k;
        end;
      end;

      o := Pointer(FOpts.Objects[j]);
      if (o.edit <> nil) then
      begin
        o.chb.Checked := True;
        ud := FindUpDown(o.Edit);
        if (ud <> nil) then
          ud.Position := StrToInt(Value)
        else
          SetStrProp(o.edit, 'Text', Value);
      end
      else if (o.otype = 4) then
        o.chb.Checked := (value = '') or (value <> '0');
    end;
  finally
    l.Free;
  end;
  FInitialOpts := GetOptsStr;
end;

procedure TfrmOptions.CheckBoxClicked(Sender: TObject);
var
  i: Integer;
  c: TComponent;
  chb: TCheckBox;
  ctrl: TWinControl;
begin
  if (Sender = nil) or not (Sender is TCheckBox) then Exit;
  chb := Pointer(Sender);

  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];
    if (c = chb) or not(c.InheritsFrom(TWinControl)) then Continue;
    if (c.Tag <> chb.Tag) then Continue;
    ctrl := Pointer(c);
    ctrl.Enabled := chb.Checked;
  end;
end;

procedure TfrmOptions.CreateStringOption(const x, y, tag: Integer; o: POption; xmax: Integer);
var
  ed: TEdit;
begin
  ed := TEdit.Create(Self);
  o.edit := ed;

  ed.Left := x;
  ed.Top := y;
  ed.Tag := tag;
  ed.Parent := pnOptions;

  if (o.def <> nil) then
    ed.Text := string(o.def);
end;

procedure TfrmOptions.CreateIntegerOption(const x, y, tag: Integer; o: POption; xmax: Integer);
var
  ed: TEdit;
  cb: TComboBox;
  up: TUpDown;
  i:  Integer;
begin
  if (o.min <> nil) and (o.max <> nil) and
     ((StrToInt(o.max) - StrToInt(o.min)) < 32) then
  begin
    cb := TComboBox.Create(Self);
    o.edit := cb;
    cb.Left := x;
    cb.Top := y;
    cb.Tag := tag;
    if (cb.Left + cb.Width < xmax) then
      cb.Left := xmax - cb.Width;
    cb.Parent := pnOptions;

    for i := StrToInt(o.min) to StrToInt(o.max) do
      cb.Items.Add(IntToStr(i));
    if (o.def <> nil) then
      cb.Text := o.def
    else
      cb.ItemIndex := 0;
    Exit;
  end;

  ed := TEdit.Create(Self);
  o.edit := ed;

  ed.Left := x;
  ed.Top := y;
  ed.Tag := tag;
  ed.Parent := pnOptions;

  up := TUpDown.Create(Self);
  up.Parent := pnOptions;

  ed.Width := ed.Width - up.Width;
  up.Left := ed.Left + ed.Width;
  up.Top := ed.Top;
  if (o.min <> nil) then
    up.Min := StrToInt(o.min)
  else
    up.Min := -(MAXSHORT-1);
  if (o.max <> nil) then
    up.Max := StrToInt(o.max)
  else
    up.Max := MAXSHORT;
  if (o.def <> nil) then
    up.Position := StrToInt(o.def);
  up.Associate := ed;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  RestoreBounds('options_form', Self);
end;

procedure TfrmOptions.btnHelpClick(Sender: TObject);
begin
  WinOpenURL(readme_html_path + '#fmt_' + FFormat);
end;

procedure TfrmOptions.CreateFileOption(const x, y, tag: Integer;
  o: POption; IsInput: Boolean; xmax: Integer = -1);
var
  ed: TEdit;
  btn: TSpeedButton;
begin
  ed := TEdit.Create(Self);
  o.edit := ed;

  ed.Left := x;
  ed.Top := y;
  ed.Tag := tag;
  ed.Parent := pnOptions;

  btn := TSpeedButton.Create(Self);
  btn.Parent := pnOptions;
  btn.Tag := tag;
  ed.Width := ed.Width - btn.Width;
  btn.Left := ed.Left + ed.Width;
  btn.Top := ed.top;

  if IsInput then
  begin
    btn.OnClick := Self.OptionOpenFile;
    frmMain.ImageList1.GetBitmap(15, btn.Glyph);
  end
    else
  begin
    btn.OnClick := Self.OptionSaveFile;
    frmMain.ImageList1.GetBitmap(17, btn.Glyph);
  end;
end;

procedure TfrmOptions.OptionOpenFile(Sender: TObject);
var
  c: TControl;
  i: Integer;
  o: POption;
  d: TOpenDialog;
begin
  if (Sender = nil) or not(Sender is TControl) then Exit;

  c := Pointer(Sender);

  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);
    if (o.chb = nil) or (o.chb.Tag <> c.Tag) then Continue;

    d := TOpenDialog.Create(Self);
    try
      d.FileName := GetStrProp(o.edit, 'Text');
      if d.Execute then
        SetStrProp(o.edit, 'Text', d.FileName);
    finally
      d.Free;
    end;
  end;
end;

procedure TfrmOptions.OptionSaveFile(Sender: TObject);
var
  c: TControl;
  i: Integer;
  o: POption;
  d: TSaveDialog;
begin
  if (Sender = nil) or not(Sender is TControl) then Exit;
  
  c := Pointer(Sender);

  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);
    if (o.chb = nil) or (o.chb.Tag <> c.Tag) then Continue;

    d := TSaveDialog.Create(Self);
    try
      d.FileName := GetStrProp(o.edit, 'Text');
      if d.Execute then
        SetStrProp(o.edit, 'Text', d.FileName);
    finally
      d.Free;
    end;
  end;
end;

function TfrmOptions.ParseOptsLine(const Line: string; List: TStrings): Integer;
var
  s, name, val: string;
  cin, cend: PChar;
  c1, c2: PChar;
  ins: Boolean;
begin
  List.Clear;
  s := Trim(line);
  while ((s <> '') and (s[Length(s)] = ',')) do SetLength(s, Length(s) - 1);
  s := s + ',';

  cin := PChar(s);
  cend := cin + StrLen(cin);

  while (cin < cend) do
  begin
    c1 := StrScan(cin, '=');
    c2 := StrScan(cin, ',');
    if (c1 > c2) then c1 := nil;

    if (c1 <> nil) then
    begin
      c1^ := #0;
      name := string(cin);
      val := '';

      c1 := c1 + 1;
      while (c1^ > #0) and (c1^ <= ' ') do c1 := c1 + 1;

      if (c1^ = '"') then // dequote
      begin
        while (c1 < cend) do
        begin
          c1 := c1 + 1;
          if (c1^ = '"') then
          begin
            if ((c1+1)^ = '"') then
              c1 := c1 + 1
            else
              Break;
          end;
          val := val + c1^;
        end;
        c2 := StrScan(c1 + 1, ',');
      end
        else
      begin
        c2^ := #0;
        val := string(c1);
      end;
    end
      else
    begin
      c2^ := #0;
      name := string(cin);
    end;

    if (name <> '') then
    begin
      if (val <> '') then
        list.Add(Format('%s=%s', [name, val]))
      else
        list.Add(name);
    end;

    if (c2 = nil) then
      Break
    else
      cin := c2 + 1;
  end;

  Result := List.Count;
end;

function TfrmOptions.FindUpDown(AControl: TControl): TUpDown;
var
  i: Integer;
  c: TComponent;
begin
  Result := nil;
  for i := 0 to ComponentCount - 1 do
  begin
    c := Components[i];
    if c.InheritsFrom(TUpDown) and (TUpDown(c).Associate = AControl) then
    begin
      Result := Pointer(c);
      Exit;
    end;
  end;
end;

procedure TfrmOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  str: string;
begin
  if (Key <> 27) then Exit;

  str := GetOptsStr;
  if (str <> FInitialOpts) then
  begin
    if not(MessageDlg(_('Discard changes?'), mtWarning, mbOKCancel, 0) = mrOK) then
      Exit; 
  end;
  ModalResult := mrCancel;
end;

procedure TfrmOptions.StoreOptionsToInifile();
var
  i: Integer;
  o: POption;
  c: TComponent;
  s, value: string;
begin
(*
  for i := 0 to FOpts.Count - 1 do
  begin
    o := Pointer(FOpts.Objects[i]);
    if (o.chb = nil) then Continue;

    if o.chb.AllowGrayed then
    begin
      if (o.chb.State = cbGrayed) then
      begin
        gpsbabel_ini.DeleteKey(o.format, o.name);
        Continue;
      end;
    end
    else if not(o.chb.Checked) then
    begin
      gpsbabel_ini.DeleteKey(o.format, o.name);
      Continue;
    end;

    if (o.edit = nil) then
    begin
      if o.chb.Checked then
        value := '1'
      else
        value := '0';
    end
      else value := GetStrProp(o.edit, 'Text');
    if (o.gbdef <> nil) and (StrPas(o.gbdef) = value) then
      gpsbabel_ini.DeleteKey(o.format, o.name)
    else
      gpsbabel_ini.WriteString(o.format, o.name, value);
  end;
*)
end;

procedure TfrmOptions.StoreOptionsToRegistry();
var
  i: Integer;
  o: POption;
  c: TComponent;
  s, key, value: string;
  r: TRegistry;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CURRENT_USER;
    if not(r.OpenKey('\Software\GPSBabel', True)) then Exit;

    for i := 0 to FOpts.Count - 1 do
    begin
      o := Pointer(FOpts.Objects[i]);
      if (o.chb = nil) then Continue;

      key := o.format + ':' + o.name;

      if o.chb.AllowGrayed then
      begin
        if (o.chb.State = cbGrayed) then
        begin
          r.DeleteValue(key);
          Continue;
        end;
      end
      else if not(o.chb.Checked) then
      begin
        r.DeleteValue(key);
        Continue;
      end;

      if (o.edit = nil) then
      begin
        if o.chb.Checked then
          value := '1'
        else
          value := '0';
      end
        else value := GetStrProp(o.edit, 'Text');
      if (o.gbdef <> nil) and (StrPas(o.gbdef) = value) then
        r.WriteString(key, '(default)')
      else
        r.WriteString(key, value);
    end;
  finally
    r.Free;
  end;
end;

procedure TfrmOptions.LoadSettingsFromRegistry();
var
  i: Integer;
  o: POption;
  c: TComponent;
  s, key, value: string;
  r: TRegistry;
  u: TUpDown;
  v: Integer;
begin
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_CURRENT_USER;
    if not(r.OpenKeyReadOnly('\Software\GPSBabel')) then Exit;

    for i := 0 to FOpts.Count - 1 do
    begin
      o := Pointer(FOpts.Objects[i]);
      if (o.chb = nil) then Continue;

      key := o.format + ':' + o.name;
      if not r.ValueExists(key) then Continue;

      Value := r.ReadString(key);
      if (o.edit = nil) then
      begin
        if o.chb.AllowGrayed then
        begin
          if (value = '1') then
            o.chb.State := cbChecked
          else
            o.chb.State := cbUnChecked;
        end
          else o.chb.Checked := True;
      end
        else
      begin
        o.chb.Checked := True;
        if (value <> '(default)') then
        begin
          if HasUpDown(TEdit(o.edit), u) then
          begin
            if (o.def <> nil) then
              v := StrToIntDef(o.def, 0) else
              v := 0;
            u.Position := StrToIntDef(value, v);
          end
          else
            SetStrProp(o.edit, 'Text', value);
        end;
        o.edit.Enabled := True;
      end;
    end;
  finally
    r.Free;
  end;
end;

procedure TfrmOptions.btnOKClick(Sender: TObject);
begin
//StoreOptionsToInifile();
  StoreOptionsToRegistry();
end;

procedure TfrmOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StoreBounds('options_form', Self);
end;

end.
