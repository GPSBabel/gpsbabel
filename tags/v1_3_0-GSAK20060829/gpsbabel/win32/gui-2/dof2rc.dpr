program DOF2RC;
uses
  Windows, SysUtils, Inifiles;

var
  Ini: TInifile;
  IFName: string;
  OFName: string;
  OFile: Text;
  s: string;
  i: Integer;

  procedure WriteValue(const Key: string);
  var
    s: string;
  begin
    s := Ini.ReadString('Version Info Keys', Key, '');
    WriteLn(OFile, #9#9'VALUE "', Key, '", "', s, '\0"');
  end;

begin
  IFName := ChangeFileExt(ExpandFileName(ParamStr(1)), '.dof');
  Ini := TIniFile.Create(IFName);
  try
    OFName := SysUtils.ChangeFileExt(Ini.FileName, '.rc');
    if not Ini.SectionExists('Compiler') then
    begin
      WriteLn('Invalid DOF!');
      Halt(1);
    end;

    System.Assign(OFile, OFName);
{$I-}
    System.Rewrite(OFile);
{$I+}
    if (IOResult <> 0) then
    begin
      Halt(1);
    end;
    try
      s := SysUtils.ExtractFileName(Ini.FileName);
      s := SysUtils.ChangeFileExt(s, '.ico');

      System.WriteLn(OFile, 'MAINICON ICON "', s, '"');
      System.WriteLn(OFile);

      if not Ini.SectionExists('Version Info') then Exit;

      WriteLn(OFile, '1 VERSIONINFO');
      s := Ini.ReadString('Version Info', 'MajorVer', '0') + ',' +
           Ini.ReadString('Version Info', 'MinorVer', '0') + ',' +
           Ini.ReadString('Version Info', 'Release', '0') + ',' +
           Ini.ReadString('Version Info', 'Build', '0');
      WriteLn(OFile, 'FILEVERSION ', s);
      WriteLn(OFile, 'PRODUCTVERSION ', s);
      WriteLn(OFile, 'FILEOS 0x4');
      WriteLn(OFile, 'FILETYPE 0x1');
      WriteLn(OFile, 'FILESUBTYPE 0x0L');
      WriteLn(OFile, 'FILEFLAGSMASK 0x3fL');

      i := 0;
      if (Ini.ReadInteger('Version Info', 'Debug', 0) <> 0) then i := 1;
      if (Ini.ReadInteger('Version Info', 'PreRelease', 0) <> 0) then i := (i or 2);
      if (Ini.ReadInteger('Version Info', 'Special', 0) <> 0) then i := (i or $20);
      if (Ini.ReadInteger('Version Info', 'Private', 0) <> 0) then i := (i or 8);
      if (i <> 0) then
        WriteLn(OFile, 'FILEFLAGS ', SysUtils.Format('0x%2.2x', [i]));

      WriteLn(OFile, '{');
      WriteLn(OFile, 'BLOCK "StringFileInfo"');
      WriteLn(OFile, '{');

      if ini.SectionExists('Version Info Keys') then
      begin
        WriteLn(OFile, #9'BLOCK "040904E4"');
	WriteLn(OFile, #9'{');
        WriteValue('CompanyName');
        WriteValue('FileDescription');
        WriteValue('FileVersion');
        WriteValue('InternalName');
        WriteValue('LegalCopyright');
        WriteValue('LegalTrademarks');
        WriteValue('OriginalFilename');
        WriteValue('ProductName');
        WriteValue('ProductVersion');
        WriteValue('Comments');
	WriteLn(OFile, #9'}');
      end;
      WriteLn(OFile, '}');

      WriteLn(OFile, 'BLOCK "VarFileInfo"');
      WriteLn(OFile, '{');
      WriteLn(OFile, #9'VALUE "Translation", ',
        SysUtils.Format('0x%4.4x 0x%4.4x', [
          Ini.ReadInteger('Version Info', 'Locale', 1033),
          Ini.ReadInteger('Version Info', 'CodePage', 1252)]));
      WriteLn(OFile, '}');
      WriteLn(OFile, '}');


    finally
      System.Close(OFile);
    end;
  finally
    ini.Free;
  end;
end.

