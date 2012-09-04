unit about;

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
  StdCtrls, Buttons, ExtCtrls, 
  common;

type
  TfrmAbout = class(TForm)
    pnClient: TPanel;
    pnBottom: TPanel;
    btnOK: TBitBtn;
    pnCenter: TPanel;
    stDescription: TStaticText;
    imgBabelIcon: TImage;
    stLicense: TStaticText;
    lbHint1: TLabel;
    lbVersion: TLabel;
    lbxTranslators: TListBox;
    lbTranslators: TLabel;
    lbURL: TLabel;
    lbCopyRight: TLabel;
    lbMoreInfo: TLabel;
    lbSFURL: TLabel;
    btnNewLanguage: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lbURLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnCenterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbURLClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnNewLanguageClick(Sender: TObject);
  private
    { Private declarations }
    FTitle: string;
    FFlag: Integer;
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
  TranslateComponent(SELF);
  FTitle := Caption;
  Caption := FTitle + ' ' + SGPSBabelTitle;

  lbVersion.Caption := _('Version') + ' ' + SGPSBabelGUIVersion;
  FixStaticText(Self);

  lbURL.Left := lbMoreInfo.Left + lbMoreInfo.Width + 4;
  lbSFURL.Left := lbHint1.Left + lbHint1.Width + 4;
  lbTranslators.Caption := lbTranslators.Caption + ':';

  lbxTranslators.Items.Add(_('German') + ' ' +  _('by') + ' Olaf Klein');
  lbxTranslators.Items.Add(_('French') + ' ' +  _('by') + ' Lilian Morinon');
  lbxTranslators.Items.Add(_('Spanish') + ' ' + _('by') + ' Daniel Diaz');
  lbxTranslators.Items.Add(_('Hungarian') + ' ' + _('by') + ' Sprok Bence');
  lbxTranslators.Items.Add(_('Italian') + ' ' + _('by') + ' Michele Locati');
end;

procedure TfrmAbout.lbURLMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (FFlag = 0) then
  begin
    FFlag := 1;
    TLabel(Sender).Font.Color := clRed;
  end;
end;

procedure TfrmAbout.pnCenterMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (FFlag = 1) then
  begin
    FFlag := 0;
    lbURL.Font.Color := clBlue;
    lbSFURL.Font.Color := clBlue;
  end;
end;

procedure TfrmAbout.lbURLClick(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clBlue;
  WinOpenFile('http://' + TLabel(Sender).Caption, '');
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 27) then
    ModalResult := mrOK;
end;

procedure TfrmAbout.btnNewLanguageClick(Sender: TObject);
begin
  ShowMessage(
  _('Please have a look at the file README.GUI.'#13#10#10 +
  'There you will find all information you need to'#13#10 +
  'get GPSBabelGUI working in your own language.')
 );
end;

end.
 