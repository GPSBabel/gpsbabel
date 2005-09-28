program GPSBabelGUI2;
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

uses
  gnugettext in 'gnugettext.pas',
  gnugettextD4 in 'gnugettextD4.pas',
  Windows,
  SysUtils,
  classes,
  Forms,
  main in 'main.pas' {frmMain},
  utils in 'utils.pas',
  common in 'common.pas',
  filter in 'filter.pas' {frmFilter},
  about in 'about.pas' {frmAbout},
  readme in 'readme.pas' {frmReadme};

{$R *.RES}

(*
!!!
gpsbabel.exe -w -i gdb -f "C:\TEMP\2005 Pfunds total.gdb" -x duplicate,shortname -o pathaway -F "C:\TEMP\2005 Pfunds total.pdb"
gpsbabel: Unable to allocate -16056 bytes of memory.
!!!
*)

begin
  AddDomainForResourceString('delphi');

  UseLanguage('en');                 // for testing

  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmReadme, frmReadme);
  Application.CreateForm(TfrmFilter, frmFilter);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
