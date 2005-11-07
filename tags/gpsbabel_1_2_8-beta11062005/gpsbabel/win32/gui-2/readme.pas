unit readme;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TfrmReadme = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  try
    FStr := TFileStream.Create('README', fmOpenRead);
    Memo.Lines.LoadFromStream(FStr);
  except
    FStr := nil;
  end;
end;

end.
