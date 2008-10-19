unit options;

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
    procedure CreateIntegerOption(const x, y, tag: Integer; o: POption; xmax: Integer = -1);
    function FindUpDown(AControl: TControl): TUpDown;
    function GetOptsStr: string;
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
    destructor Destroy; override;
  property
    Opts: TStringList read FOpts write SetOpts;
    property OptsStr: string read GetOptsStr write SetOptsStr;
  end;

  TIntegerEdit = class(TOptionEdit)
  protected
    ed: TEdit;
    up: TUpDown;
    max, min : Integer;
    function GetValue : String; override;
    procedure SetValue(Value : String); override;
    function GetEnabled : Boolean; override;
    procedure SetEnabled(value : Boolean); override;
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    function GetIntValue : Integer;
    procedure SetIntValue(Value : Integer);
  public
    constructor Create(Form : TForm; Parent : TWinControl; AOption : POption; const x,y : Integer);
  end;

  TIntegerSelectEdit = class(TOptionEdit)
  protected
    combo: TComboBox;
    min : Integer;
    function GetValue : String; override;
    procedure SetValue(Value : String); override;
    function GetEnabled : Boolean; override;
    procedure SetEnabled(value : Boolean); override;
  public
    constructor Create(Form : TForm; Parent : TWinControl; AOption : POption; const x,y,xmax : Integer);
  end;

  TStringEdit = class(TOptionEdit)
  protected
    ed: TEdit;
    function GetValue : String; override;
    procedure SetValue(Value : String); override;
    function GetEnabled : Boolean; override;
    procedure SetEnabled(value : Boolean); override;
  public
    constructor Create(Form : TForm; Parent : TWinControl; AOption : POption; const x,y : Integer);
  end;

  TFileEdit = class(TOptionEdit)
  protected
    ed: TEdit;
    btn: TSpeedButton;
    FForm : TfrmOptions;
    FIsInput : Boolean;
    function GetValue : String; override;
    procedure SetValue(Value : String); override;
    function GetEnabled : Boolean; override;
    procedure SetEnabled(value : Boolean); override;
    procedure OpenFile(Sender: TObject);
    procedure SaveFile(Sender: TObject);
  public
    constructor Create(AForm : TfrmOptions; Parent : TWinControl; AOption : POption;
      const x,y : Integer; AIsInput: Boolean; xmax: Integer = -1);
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

constructor TIntegerEdit.Create;
begin
  FOption:=AOption;
  ed:=TEdit.Create(form);
  ed.Parent:=Parent;
  up:=TUpDown.Create(form);
  up.Parent:=Parent;

  // The following initialization will allow users
  // to decrease or increase max 16383 times before
  // TUpDown blocks - if a user will every reach that limit?
  up.Min := 0;
  up.Max := MAXSHORT;
  up.Position:=MAXSHORT DIV 2;

  ed.Left := x;
  ed.Top := y;

  ed.Width := ed.Width - up.Width;
  up.Left := ed.Left + ed.Width;
  up.Top := ed.Top;

  if (Option.min <> nil) then
    min := StrToInt(Option.min)
  else
    min := -(MAXLONG-1);
  if (Option.max <> nil) then
    max := StrToInt(Option.max)
  else
    max := MAXLONG;
  if (Option.def <> nil) then
    Value:=Option.def;
  up.OnClick:=UpDownClick;
end;

procedure TIntegerEdit.UpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  if (Button = btNext) then
    SetIntValue(GetIntValue+1)
  else
    SetIntValue(GetIntValue-1);
end;

procedure TIntegerEdit.SetIntValue(Value : Integer);
begin
  if (Value <min) or (value >max) then exit;
  ed.Text:=InttoStr(Value);
end;

function TIntegerEdit.GetIntValue : Integer;
begin
  Result:=StrToIntDef(ed.Text,0);
end;

function TIntegerEdit.GetValue : String;
begin
  Result:=InttoStr(GetIntValue);
end;

procedure TIntegerEdit.SetValue(Value : String);
begin
  SetIntValue(StrToIntDef(value,Min));
end;

function TIntegerEdit.GetEnabled : Boolean;
begin
  result:=ed.Enabled;
end;

procedure TIntegerEdit.SetEnabled(value : Boolean);
begin
  ed.Enabled:=Value;
  up.Enabled:=Value;
end;

//*******************************************++

constructor TIntegerSelectEdit.Create;
Var
  I : Integer;
begin
  FOption:=AOption;
  Combo := TComboBox.Create(Form);
  Combo.Left := x;
  Combo.Top := y;
  if (Combo.Left + Combo.Width < xmax) then
    Combo.Left := xmax - Combo.Width;
  Combo.Parent := Parent;
  min:=StrToInt(Option.min);
  for i := min to StrToInt(Option.max) do
    Combo.Items.Add(IntToStr(i));
  if (Option.def <> nil) then
    Combo.Text := Option.def
  else
    Combo.ItemIndex := 0;
end;

function TIntegerSelectEdit.GetValue : String;
begin
  Result:=InttoStr(Min + Combo.ItemIndex);
end;

procedure TIntegerSelectEdit.SetValue(Value : String);
Var
  I : Integer;
begin
  I:=StrToIntDef(Value,Min);
  I:=I-Min;
  Combo.ItemIndex:=I;
end;

function TIntegerSelectEdit.GetEnabled : Boolean;
begin
  Result:=Combo.Enabled;
end;

procedure TIntegerSelectEdit.SetEnabled(value : Boolean);
begin
  Combo.Enabled:=Value;
end;

constructor TStringEdit.Create;
begin
  FOption:=AOption;
  ed := TEdit.Create(form);
  ed.Left := x;
  ed.Top := y;
  if (Option.def <> nil) then
    Value := string(Option.def);
  ed.Parent := Parent;
end;

function TStringEdit.GetValue : String;
begin
  Result:=ed.Text;
end;

procedure TStringEdit.SetValue(Value : String);
begin
  ed.Text:=Value;
end;

function TStringEdit.GetEnabled : Boolean;
begin
  Result:=ed.Enabled;
end;

procedure TStringEdit.SetEnabled(value : Boolean);
begin
  ed.Enabled:=Value;
end;

constructor TFileEdit.Create;
begin
  FOption:=AOption;
  FForm:=AForm;
  FIsInput:=AIsInput;
  ed := TEdit.Create(FForm);

  ed.Left := x;
  ed.Top := y;
  ed.Parent := Parent;

  btn := TSpeedButton.Create(FForm);
  btn.Parent := Parent;
  ed.Width := ed.Width - btn.Width;
  btn.Left := ed.Left + ed.Width;
  btn.Top := ed.top;

  if FIsInput then begin
    frmMain.ImageList1.GetBitmap(15, btn.Glyph);
    btn.OnClick := OpenFile;
  end else begin
    frmMain.ImageList1.GetBitmap(17, btn.Glyph);
    btn.OnClick := SaveFile;
  end;
end;

procedure TFileEdit.OpenFile(Sender: TObject);
var
  d: TOpenDialog;
begin
  d := TOpenDialog.Create(FForm);
  try
    d.FileName := Value;
    if d.Execute then
      Value:=d.FileName;
  finally
    d.Free;
  end;
end;

procedure TFileEdit.SaveFile(Sender: TObject);
var
  d: TSaveDialog;
begin
  d := TSaveDialog.Create(FForm);
  try
    d.FileName := Value;
    if d.Execute then
      Value:=d.FileName;
  finally
    d.Free;
  end;
end;

function TFileEdit.GetEnabled : Boolean;
begin
  Result:=ed.Enabled;
end;

procedure TFileEdit.SetEnabled(value : Boolean);
begin
  ed.Enabled:=Value;
  btn.Enabled:=Value;
end;

function TFileEdit.GetValue : String;
begin
  Result:=ed.Text;
end;

procedure TFileEdit.SetValue(Value : String);
begin
  ed.Text:=value;
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
    chb.Tag := i;

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
    if (Assigned(o.edit)) and (chb.checked) then
      o.edit.Enabled:=True;
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
      2, 3:   o.edit:=TStringEdit.Create(self,pnOptions,o,xy.X, xy.Y - 2);
      4: ;
      5: o.edit:=TFileEdit.Create(self,pnOptions,o,xy.X, xy.Y - 2, True, xmax);
      6: if not FIsInput then o.edit:=TFileEdit.Create(self,pnOptions,o,xy.X, xy.Y - 2, False, xmax);
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
    s := o.edit.value;
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
        o.Edit.Enabled:=True;
        o.Edit.Value:=Value;
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
  chb: TCheckBox;
  o : POption; 
begin
  if (Sender = nil) or not (Sender is TCheckBox) then Exit;
  chb := Pointer(Sender);
  o:=POption(FOpts.Objects[chb.tag]);
  if not Assigned(o) Then exit;
  if not Assigned(o^.edit) Then exit;
  o^.edit.Enabled:=chb.Checked;
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
     o.edit:=TIntegerSelectEdit.Create(self,pnOptions,o,x,y,xmax)
  else
    o.edit:=TIntegerEdit.Create(self,pnOptions,o,x,y);
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
        else value := o.edit.Value;
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
           o.edit.Value := value;
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

destructor TfrmOptions.Destroy;
Var
  I : Integer;
  o: POption;
begin
  try
    for i := 0 to FOpts.Count - 1 do
    begin
      o := Pointer(FOpts.Objects[i]);
      if (Assigned(o.edit)) then begin
        o.edit.Free;
        o.edit := nil;
      end;
    end;
  finally
    inherited;
  end;
end;

end.
