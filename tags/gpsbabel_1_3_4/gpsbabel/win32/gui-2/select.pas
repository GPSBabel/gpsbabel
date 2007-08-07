unit select;

interface

uses
  gnugettext, gnugettextDx,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfrmSelect = class(TForm)
    pnTop: TPanel;
    pnBottom: TPanel;
    lbSelect: TListBox;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSelect: TfrmSelect;

function SelectFromStringList(const Title: string; List: TStrings; var Str: string): Boolean;
function SelectLanguage(const Title: string; const Builtin: TStrings; var Lang: string; const Default: string = ''): Boolean;

implementation

{$R *.DFM}

function SelectFromStringList(const Title: string; List: TStrings; var Str: string): Boolean;
var
  i, res: Integer;

begin
  Application.CreateForm(TfrmSelect, frmSelect);
  try
    frmSelect.Caption := Title;
    frmSelect.lbSelect.Items.Assign(List);
    frmSelect.ActiveControl := frmSelect.lbSelect;
    if (str <> '') then
    begin
      i := frmSelect.lbSelect.Items.IndexOf(str);
      if (i >= 0) then
        frmSelect.lbSelect.ItemIndex := i;
    end;
    res := frmSelect.ShowModal;
    Result := (res = mrOk);
    i := frmSelect.lbSelect.ItemIndex;
    if Result and (i >= 0) then
      Str := frmSelect.lbSelect.Items[i];
  finally
    frmSelect.Release;
  end;
end;

function SelectLanguage(const Title: string; const Builtin: TStrings; var Lang: string; const Default: string = ''): Boolean;
var
  i: Integer;
  s, sx, sy: string;
  l: TStringList;

begin
  Result := False;

  if (Default = '') then
    Lang := Copy(gnugettext.GetCurrentLanguage, 1, 2);

  l := TStringList.Create;
  try
    l.Sorted := True;
    
    sy := '';
    for i := 0 to Builtin.Count - 1 do
    begin
      s := Builtin.Strings[i];
      if (s = '') then Continue;

      if (CompareText(s, 'de') = 0) then sx := _('German') else
      if (CompareText(s, 'es') = 0) then sx := _('Spanish') else
      if (CompareText(s, 'fr') = 0) then sx := _('French') else
      if (CompareText(s, 'en') = 0) then sx := _('English') else
      if (CompareText(s, 'hu') = 0) then sx := _('Hungarian') else
        sx := '???';

      sx := Format('%s - %s', [s, sx]);
      if (CompareText(s, Lang) = 0) then sy := sx;

      l.Add(sx);
    end;

    if SelectFromStringList(Title, l, sy) then
    begin
      Lang := Copy(sy, 1, 2);
      Result := True;
    end;

  finally
    l.Free;
  end;
end;

{ TfrmSelect }

procedure TfrmSelect.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);

// !!! work-arround !!!
  btnOK.Caption := dgettext('delphi', 'OK');
  btnCancel.Caption := dgettext('delphi', 'Abort');
// !!! work-arround !!!
end;

procedure TfrmSelect.FormShow(Sender: TObject);
var
  i: Integer;
  s: string;
  t: TLabel;
begin
  t := TLabel.Create(Self);
  try

     t.Caption := '';
     t.Font := lbSelect.Font;
     t.ParentFont := lbSelect.ParentFont;
     t.Parent := lbSelect.Parent;

     for i := 0 to lbSelect.Items.Count - 1 do
     begin
       s := Copy(lbSelect.Items[i], 1, 4);
       while (t.Canvas.TextWidth(s) < 32) do
         s := s + ' ';
       s := s + Copy(lbSelect.Items[i], 5, 256);
       lbSelect.Items[i] := s;
     end;
     
  finally
    t.Free;
  end;
end;

end.
