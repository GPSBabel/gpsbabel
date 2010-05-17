program GPSBabelGUI;
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

{%TogetherDiagram 'ModelSupport_GPSBabelGUI\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\filter\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\delphi\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\options\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\common\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\about\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\utils\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\main\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\gnugettext\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\gnugettextDx\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\GPSBabelGUI\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\select\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\default.txvpck'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\main\default.txvpck'}
{%TogetherDiagram 'ModelSupport_GPSBabelGUI\GPSBabelGUI\default.txvpck'}

uses
  gnugettext in 'gnugettext.pas',
  gnugettextDx in 'gnugettextDx.pas',
  delphi in 'delphi.pas',
  Windows,
  SysUtils,
  classes,
  Forms,
  main in 'main.pas' {frmMain},
  utils in 'utils.pas',
  common in 'common.pas',
  filter in 'filter.pas' {frmFilter},
  about in 'about.pas' {frmAbout},
  options in 'options.pas' {frmOptions},
  select in 'select.pas' {frmSelect};

{$R *.RES}

var
  lang: string;

begin
  AddDomainForResourceString('delphi');
  lang := ReadProfile('Global:Language', '');
  if (lang <> '') then
    UseLanguage(lang);
{$IFOPT D+}
//  UseLanguage('fr');                 // for testing
{$ENDIF}

  Application.Initialize;
  Application.Title := 'GPSBabelGUI-2';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
