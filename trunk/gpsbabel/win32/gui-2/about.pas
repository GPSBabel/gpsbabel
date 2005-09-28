unit about;

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
  common;

type
  TfrmAbout = class(TForm)
    pnClient: TPanel;
    Image1: TImage;
    pnBottom: TPanel;
    BitBtn1: TBitBtn;
    StaticText3: TStaticText;
    Panel1: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FTitle: string;
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses
  utils;
  
{$R *.DFM}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  gnugettextD4.TranslateComponent(SELF);
  FTitle := Caption;
  Caption := FTitle + ' GPSBabelGUI-2';

  FixStaticText(Self);
end;

end.
