unit readme;

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
  gnugettextDx,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TfrmReadme = class(TForm)
    Memo: TMemo;
    pnBottom: TPanel;
    btnOK: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FStr: TStream;
  public
    { Public declarations }
  end;

var
  frmReadme: TfrmReadme;

implementation

{$R *.DFM}

procedure TfrmReadme.FormDestroy(Sender: TObject);
begin
  FStr.Free;
end;

procedure TfrmReadme.FormCreate(Sender: TObject);
begin
  btnOK.Left := pnBottom.Width - btnOK.Width - 8;
  try
    FStr := TFileStream.Create('README', fmOpenRead);
    Memo.Lines.LoadFromStream(FStr);
  except
    FStr := nil;
  end;
end;

procedure TfrmReadme.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 27) then
    ModalResult := mrOK;
end;

end.
