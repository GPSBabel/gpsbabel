unit AboutDialogU;

interface

uses
	Buttons, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics, Messages, StdCtrls, SysUtils, Windows,
	VersionInfo;

type
	TAboutBox = class (TForm)
		private // invisible outside of the unit
		protected // visible in the unit and in descended classes
		public // visible wherever the class can be referenced
		published // like public, but generates RTTI info
			lblCopyright: TLabel;
			lblLicense: TLabel;
			lblProductName: TLabel;
			lblVersion: TLabel;

			pnlInner: TPanel;
			pnlOuter: TPanel;
			ProgramIcon: TImage;

			tmrAboutBox: TTimer;

			procedure FormCreate (Sender: TObject);
			procedure FormShow (Sender: TObject);
			procedure tmrAboutBoxTimer (Sender: TObject);
	end;

var
	AboutBox: TAboutBox;

implementation

{$R *.DFM}

procedure TAboutBox.FormCreate (Sender: TObject);
var s: string;
begin
	lblCopyright.Caption   := GetVersionString ('LegalCopyright');
	lblLicense.Caption     := GetVersionString ('License');
	lblProductName.Caption := GetVersionString ('ProductName');
	Caption                := lblProductName.Caption;
	lblVersion.Caption     := lblVersion.Caption + GetFileVersion (TFileVersionLong);

	ShortDateFormat := 'yyyy';
	s := DateToStr (Date);
	if s <> Copy (lblCopyright.Caption, Length(lblCopyright.Caption)-3,4) then // year of original copyright
		lblCopyright.Caption := lblCopyright.Caption + ', ' + s;
end;

procedure TAboutBox.FormShow (Sender: TObject);
begin
	tmrAboutBox.Enabled := true;
end;

procedure TAboutBox.tmrAboutBoxTimer (Sender: TObject);
begin
	tmrAboutBox.Enabled := false;
	AboutBox.Visible := false;
end;

initialization
end.


