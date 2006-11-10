program GPSBabelGUI;

{%ToDo 'GPSBabelGUI.todo'}

uses
  Forms,
  GPSBabelGUIDialogU in 'GPSBabelGUIDialogU.pas' {GPSBabelGUIDialog},
  AboutDialogU in 'AboutDialogU.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GPSBabelGUI';
  Application.CreateForm(TGPSBabelGUIDialog, GPSBabelGUIDialog);
  Application.Run;
end.
