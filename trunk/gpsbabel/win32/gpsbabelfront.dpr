program gpsbabelfront;

uses
  Forms,
  gpsbabelfront_mainform in 'gpsbabelfront_mainform.pas' {FormGPSBabelFront};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'GPSBabel Front End';
  Application.CreateForm(TFormGPSBabelFront, FormGPSBabelFront);
  Application.Run;
end.
