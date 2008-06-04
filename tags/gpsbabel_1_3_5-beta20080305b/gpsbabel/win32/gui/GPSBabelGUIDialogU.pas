{
  Copyright © Richard L Messeder, Down East Engineering, www.DownEastEngineering.com and others listed below
  Delphi 5, 6 & 7 Version
  v02.00.00 Add About.
  v02.00.00 Add Exit button.
  v02.00.00 Add Help.
  v02.00.00 Add hotkeys for most common buttons.
  v02.00.00 Add timer to load formats after form is created.
  v02.00.00 Align controls on dialog.
  v02.00.00 Don't permit file processing unless GPSBabel is OK.
  v02.00.00 Evaluate need for memoStdErr vs. Message dialog.
  v02.00.00 Include all project management and Delphi project files in upload.
  v02.00.00 LoadFormatsFromGPSBabelExe: Delete unused AllocConsole.
  v02.00.00 LoadFormatsFromGPSBabelExe: don't continue if GPSBabel not found.
  v02.00.00 Look for GPSBabel.exe before attempting to open it.
  v02.00.00 Modify to use input filename and output type to generate default output filename.
  v02.00.00 Output file dialog: fill in filename and filetype, if possible.
  v02.00.00 Port to Delphi 7.
  v02.00.00 Process: Delete unused AllocConsole.
  v02.00.00 Process: handle cases where input/output file is a dir.
  v02.00.00 Process: don't allow processing null, or undefined, files.
  v02.00.00 Remove unused console invocations.
  v02.00.00 Remove unused variable declarations.
  v02.00.00 Rewrite to eliminate 'exit;'.
  v02.00.00 Sort default file types by alpha; add sort routine: CANCELLED: placed in GPSBabel.
}
{
    Copyright (C) 2002 Josh M. McKee, mrsnazz@users.sourceforge.net

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

{
    1.0.2   JMc   - Added LoadFormats to call the new -^ switch, to dynamically
                    load the supported GPSDataFormats from gpsbabel.exe.
    1.0.1   JMc   - Switched to using AddFormat for populating the GPSDataFormats table
                  - Updated GPSDataFormats table to include currently supported GPSDataFormats
                  - Switched to using CreateProcess rather than WinExec, so that
                    we can display data from stderr to the user.
    1.0.0   JMc   First release
}

unit GPSBabelGUIDialogU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, StrUtils;

const
  CR  = chr ($0D);
  LF  = chr ($0A);
  Tab = chr ($09);

type
  TGPSDataFormat = record
    sType: string; // type to be passed to GPSBabel
    sExt: string;  // default file extension
    sDesc: string; // description of format
  end;

  TGPSBabelGUIDialog = class (TForm)
    private // invisible outside of the unit
      OKtoRun: boolean;
      BytesRead: DWord;
      Buffer: array [0..4095] of char;
      BufferIndex, TokenIndex, nFormats: integer;
      sCmd, sGPSBabelMsg, sIgnoreShort: string;
      Tokens: array [0..2] of string;
      GPSDataFormats: array of TGPSDataFormat;
      hRead, hWrite: THandle;
      ProcessInfo: TProcessInformation;
      SecurityAttr: TSecurityAttributes;
      StartupInfo: TStartupInfo;
      procedure LoadFormatsFromGPSBabelExe;
      procedure PopulateCombos;
      procedure PopulateDialogs;
    protected // visible in the unit and in descended classes
    public // visible wherever the class can be referenced
    published // like public, but generates RTTI info
      btnAbout: TButton;
      btnExit: TButton;
      btnHowTo: TButton;
      btnInputFileDialog: TSpeedButton;
      btnIntro: TButton;
      btnOutputFileDialog: TSpeedButton;
      btnProcess: TButton;
      btnUseDefaultOutput: TButton;
      bvlGPSBabelURI: TBevel;
      cbIgnoreShort: TCheckBox;
      comboInput: TComboBox;
      comboOutput: TComboBox;
      dlgOpenInput: TOpenDialog;
      dlgSaveOutput: TSaveDialog;
      eInput: TEdit;
      eOutput: TEdit;
      lblGPSBabelURI: TLabel;
      lblInputFileName: TLabel;
      lblInputFormat: TLabel;
      lblOutputFileName: TLabel;
      lblOutputFormat: TLabel;
      TimerLoadFormats: TTimer;

      procedure TimerLoadFormatsTimer(Sender: TObject);
      procedure btnAboutClick(Sender: TObject);
      procedure btnExitClick(Sender: TObject);
      procedure btnHowToClick(Sender: TObject);
      procedure btnInputFileDialogClick (Sender: TObject);
      procedure btnIntroClick(Sender: TObject);
      procedure btnOutputFileDialogClick (Sender: TObject);
      procedure btnProcessClick (Sender: TObject);
      procedure btnUseDefaultOutputClick(Sender: TObject);
    end;

var
  GPSBabelGUIDialog: TGPSBabelGUIDialog;

implementation

uses AboutDialogU;

{$R *.dfm}

procedure TGPSBabelGUIDialog.TimerLoadFormatsTimer (Sender: TObject);
// This could have been in FormCreate, but this way the form is shown
// before the 'formats loaded' message.
begin
  TimerLoadFormats.Enabled := false;
  nFormats := 0;
  LoadFormatsFromGPSBabelExe;
end;

procedure TGPSBabelGUIDialog.LoadFormatsFromGPSBabelExe;
  procedure AddFormat (sType, sExt, sDesc: string);
  begin
    inc (nFormats);
    // changes the size of the GPSDataFormats array
    // initialized at nil, so the first pass sets it to 1 row, addressed as [0]
    SetLength (GPSDataFormats, nFormats);

    GPSDataFormats [nFormats-1].sType := sType; // [n-1] because array is zero-based
    GPSDataFormats [nFormats-1].sExt := sExt;
    GPSDataFormats [nFormats-1].sDesc := sDesc;
  end;

begin
  sCmd := 'GPSBabel -^'; // internal gpsbabel command -^

  SecurityAttr.nLength := sizeof (TSECURITYATTRIBUTES);
  SecurityAttr.bInheritHandle := true;
  SecurityAttr.lpSecurityDescriptor := nil;

  OKtoRun := false;
  if CreatePipe (hRead, hWrite, @SecurityAttr, 0) then begin
    FillChar (StartupInfo, Sizeof (StartupInfo), #0);
    StartupInfo.cb := Sizeof (StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
    StartupInfo.hStdInput := GetStdHandle (STD_INPUT_HANDLE);
    StartupInfo.hStdOutput:= hWrite;
    StartupInfo.hStdError := hWrite;

    if FileExists ('GPSBabel.exe') then begin
      if CreateProcess (
        nil,                // lpApplicationName    // pointer to name of executable module
        // sCmd includes both the exec name and the command line parms in this call
        pchar (sCmd),       // lpCommandLine,       // pointer to command line string
        nil,                // lpProcessAttributes, // pointer to process security attributes
        nil,                // lpThreadAttributes,  // pointer to thread security attributes
        true,               // bInheritHandles,     // handle inheritance flag
        CREATE_NEW_CONSOLE, // dwCreationFlags,     // creation flags
        nil,                // lpEnvironment,       // pointer to new environment block
        nil,                // lpCurrentDirectory,  // pointer to current directory name
        StartupInfo,        // lpStartupInfo,       // pointer to STARTUPINFO
        ProcessInfo)        // lpProcessInformation // pointer to PROCESS_INFORMATION
        then begin

        while (WaitforSingleObject (ProcessInfo.hProcess, 0)) <> WAIT_OBJECT_0 do;

        PeekNamedPipe (hRead, nil, 0, nil, @BytesRead, nil);
        ReadFile (hRead, Buffer, 4096, BytesRead, nil);
        // The data passed by GPSBabel.exe should exceed 1500 bytes, but this provides some slack.
        // (v2.0.0) We're discussing the possibility of having some sort of byte count passed by gpsbabel,
        // e.g., 'byte count=1234'; we could then check for 'byte count=', and if we don't get it
        // then we know that there is an error.
        if BytesRead > 1000 then begin
          BufferIndex := 0;
          TokenIndex := 0;
          FillChar (Tokens, SizeOf(Tokens), 0);

          // Process the buffer into Types, Extensions, and Descriptions
          while BufferIndex < BytesRead do begin
            if Buffer [BufferIndex] in [Tab,CR,LF] then // Tab between fields, CRLF between rows
              inc (TokenIndex)
            else
              Tokens [TokenIndex] := Tokens [TokenIndex] + Buffer [BufferIndex];
            if TokenIndex = 3 then begin
              TokenIndex := 0;
              inc (BufferIndex); // Because we point to CR and must skip the LF
              AddFormat (Tokens[0], Tokens[1], Tokens[2]);
              FillChar(Tokens, SizeOf (Tokens), 0);
            end;
            inc (BufferIndex);
          end;

          PopulateCombos;
          PopulateDialogs;
          OKtoRun := true;
          MessageDlg ('GPS file formats loaded from GPSBabel.exe.', mtInformation, [mbOk], 0);
        end // if BytesRead > 0
        else begin
          sGPSBabelMsg :=  copy (Buffer, 1, BytesRead);
          MessageBox (0, pchar (sGPSBabelMsg),'GPSBabel Error', MB_OK);
        end
      end
      else
        MessageDlg ('Unable to execute GPSBabel.exe.', mtError, [mbOk], 0);
    end
    else
      MessageDlg ('Can''t find GPSBabel.exe.', mtError, [mbOk], 0);

    CloseHandle (hRead);
    CloseHandle (hWrite);
  end
  else
    MessageDlg ('Unable to create pipe!', mtError, [mbOk], 0);
end;

procedure TGPSBabelGUIDialog.PopulateCombos;
var
  i: integer;
begin
  for i:=0 to nFormats-1 do begin
    comboInput.items.add (GPSDataFormats[i].sDesc);
    comboOutput.items.add (GPSDataFormats[i].sDesc);
  end;
end;

procedure TGPSBabelGUIDialog.PopulateDialogs;
var
  i: integer;
begin
  dlgOpenInput.Filter := '';
  dlgSaveOutput.Filter := '';
  for i:=0 to nFormats-1 do begin
    if (GPSDataFormats[i].sExt<>'') then begin
      dlgOpenInput.Filter := dlgOpenInput.Filter +
        GPSDataFormats[i].sDesc + ' (*.' +
        GPSDataFormats[i].sExt + ')|*.' +
        uppercase (GPSDataFormats[i].sExt) + '|';

      dlgSaveOutput.Filter := dlgSaveOutput.Filter +
        GPSDataFormats[i].sDesc + ' (*.' +
        GPSDataFormats[i].sExt + ')|*.' +
        uppercase (GPSDataFormats[i].sExt) + '|';
    end;
  end;

  dlgOpenInput.Filter := dlgOpenInput.Filter + 'All files (*.*)|*.*';
  dlgSaveOutput.Filter := dlgSaveOutput.Filter + 'All files (*.*)|*.*';
end;

procedure TGPSBabelGUIDialog.btnAboutClick(Sender: TObject);
begin
  if AboutBox = nil then begin
    Application.CreateForm (TAboutBox, AboutBox);
    AboutBox.Left := (Screen.Width - AboutBox.Width) div 2;
    AboutBox.Top := (Screen.Height - AboutBox.Height) div 2;
  end;
  AboutBox.Show;
end;

procedure TGPSBabelGUIDialog.btnExitClick (Sender: TObject);
begin
  Application.Terminate;
end;

procedure TGPSBabelGUIDialog.btnHowToClick(Sender: TObject);
begin
  MessageBox (0, PChar (
    'Begin using GPSBabelGUI by:' +CR+
    '    Defining an input file. If the file has an extension that' +CR+
    '    GPSBabelGUI recognizes, the format will be automatically' +CR+
    '    selected.' +CR+CR+
    '    If the selected input file format is not correct, select the correct' +CR+
    '    format from the Format dropdown list.' +CR+CR+
    '    Select an output format.' +CR+CR+
    '    Select Use Default Filename.' +CR+CR+
    '        GPSBabelGUI will fill in the default output directory/filename' +CR+
    '        using the input directory and filename. To change it, edit the' +CR+
    '        name directly or use the Save As dialog.' +CR+CR+
    '    Select any options for GPSBabel to use during processing.' +CR+CR+
    '    Select Process.'
  ), 'How To...', MB_OK);
end;

procedure TGPSBabelGUIDialog.btnInputFileDialogClick (Sender: TObject);
var
  sExt: string;
  i: integer;
begin
  if OKtoRun then
    if dlgOpenInput.Execute then begin
      eInput.Text := dlgOpenInput.filename;
      sExt := uppercase (ExtractFileExt (eInput.text));
      for i := 0 to nFormats-1 do
        if '.' + uppercase (GPSDataFormats[i].sExt) = sExt then
          comboInput.ItemIndex := i;
    end;
end;

procedure TGPSBabelGUIDialog.btnIntroClick(Sender: TObject);
begin
  MessageBox (0, PChar (
    'GPSBabelGUI is simply a GUI front end for GPSBabel.exe.' +CR+CR+
    'GPSBabelGUI''s only purpose is to make it easier to call' +CR+
    'GPSBabel.exe, so it must be able to find it. The easiest' +CR+
    'way for GPSBabelGUI to find GPSBabel.exe is for both of' +CR+
    'them to be in the same directory.' +CR+CR+
    'GPSBabelGUI will not permit any processing to take place' +CR+
    'if there is an error during startup, or if it cannot find' +CR+
    'GPSBabel.exe.' +CR+CR+
    'There are 3 classes of messages to expect:' +CR+Tab+
      '°Errors' +CR+Tab+
      '°Warnings' +CR+Tab+
      '°Information' +CR+CR+
    '°If you receive an Error, such as ''GPSBabel can''t be found'',' +CR+
    'something has gone seriously amiss and must be corrected' +CR+
    'before trying to run GPSBabelGUI again.' +CR+CR+
    '°Warnings simply indicate that you are trying to do something' +CR+
    'out of order, such as attempting to Process before defining' +CR+
    'an input or output file.' +CR+CR+
    '°Information dialogs provide you with useful information, such' +CR+
    'as letting you know that a conversion has completed.' +CR+CR+
    'The About box displays copyright information for several seconds.'
  ), 'Introduction', MB_OK);
end;

procedure TGPSBabelGUIDialog.btnOutputFileDialogClick (Sender: TObject);
var
  sExt: string;
  i: integer;
begin
  if OKtoRun then begin
    dlgSaveOutput.filename := eOutput.Text;
    if dlgSaveOutput.Execute then begin
      eOutput.Text := dlgSaveOutput.filename;
      sExt := uppercase (ExtractFileExt (eOutput.text));
      for i := 0 to nFormats-1 do
        if '.' + uppercase (GPSDataFormats[i].sExt) = sExt then
          comboOutput.ItemIndex := i;
    end;
  end;
end;

procedure TGPSBabelGUIDialog.btnProcessClick (Sender: TObject);
var
  f: file;
begin
  if OKtoRun then begin
    if eInput.text <> '' then begin
      if FileExists (eInput.text) then begin
        if (comboInput.ItemIndex) > -1 then begin // Input type is selected
          if ((eOutput.text <> '') and (not DirectoryExists (eOutput.text))) then begin
            // The output file must exist, or else ExtractShortPathName will not function
            if not FileExists (eOutput.text) then begin
              system.assign (f, eOutput.text);
              system.rewrite (f);
              system.close (f);
            end;

            if (comboOutput.ItemIndex) > -1 then begin // Output type is selected
              if cbIgnoreShort.checked then
                sIgnoreShort := '-s'
              else
                sIgnoreShort := '';

              // Construct the command line to execute gpsbabel.exe. ExtractShortPathName
              // is used to reduce any "long" file/directory names in the paths down to
              // 8.3 dos format names (this removes spaces, etc).
              sCmd := 'GPSBabel '+sIgnoreShort // +'-i garmin -f usb:' // for testing
                +' -i ' + GPSDataFormats [comboInput.ItemIndex].sType
                +' -f ' + ExtractShortPathName (eInput.text)
                +' -o ' + GPSDataFormats [comboOutput.ItemIndex].sType
                +' -F ' + ExtractShortPathName (eOutput.text);

              SecurityAttr.nLength := sizeof (TSECURITYATTRIBUTES);
              SecurityAttr.bInheritHandle := true;
              SecurityAttr.lpSecurityDescriptor := nil;

              if CreatePipe (hRead, hWrite, @SecurityAttr, 0) then begin
                FillChar (StartupInfo, Sizeof (StartupInfo), 0);
                StartupInfo.cb := Sizeof (StartupInfo);
                StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
                StartupInfo.wShowWindow := SW_HIDE and SW_SHOWMINNOACTIVE;
                StartupInfo.hStdInput := GetStdHandle (STD_INPUT_HANDLE);
                StartupInfo.hStdOutput:= hWrite;
                StartupInfo.hStdError := hWrite;

                if CreateProcess (
                  nil,                // lpApplicationName    // pointer to name of executable module
                  // sCmd includes both the exec name and the command line parms in this call
                  pchar (sCmd),       // lpCommandLine,       // pointer to command line string
                  nil,                // lpProcessAttributes, // pointer to process security attributes
                  nil,                // lpThreadAttributes,  // pointer to thread security attributes
                  true,               // bInheritHandles,     // handle inheritance flag
                  CREATE_NEW_CONSOLE, // dwCreationFlags,     // creation flags
                  nil,                // lpEnvironment,       // pointer to new environment block
                  nil,                // lpCurrentDirectory,  // pointer to current directory name
                  StartupInfo,        // lpStartupInfo,       // pointer to STARTUPINFO
                  ProcessInfo)        // lpProcessInformation // pointer to PROCESS_INFORMATION
                  then begin

                  while (WaitForSingleObject (ProcessInfo.hProcess, 0)) <> WAIT_OBJECT_0 do;

                  PeekNamedPipe (hRead, nil, 0, nil, @BytesRead, nil);
                  if BytesRead > 0 then begin // pipe not empty
                    ReadFile (hRead, Buffer, 4096, BytesRead, nil);
                    sGPSBabelMsg :=  copy (Buffer, 1, BytesRead);
                    MessageBox (0, pchar (sGPSBabelMsg),'Attention! Message from GPSBabel', MB_OK);
                  end
                  else // pipe is empty
                    MessageDlg ('File conversion complete.', mtInformation, [mbOk], 0);
                end
                else
                  MessageDlg ('Unable to execute GPSBabel.exe.', mtError, [mbOk], 0);

                CloseHandle (hRead);
                CloseHandle (hWrite);
              end
              else
                MessageDlg ('Unable to create pipe!', mtError, [mbOk], 0);
            end
            else
              MessageDlg ('You must select the output file format.', mtWarning, [mbOk], 0);
          end
          else
            MessageDlg ('Output file is not defined.', mtWarning, [mbOk], 0);
        end
        else
          MessageDlg ('You must select the input file format.', mtWarning, [mbOk], 0);
      end
      else
        MessageDlg ('Input file was not found.', mtWarning, [mbOk], 0);
    end
    else
      MessageDlg ('Input file is not defined.', mtWarning, [mbOk], 0);
  end
  else
    MessageDlg ('Can''t run.', mtError, [mbOk], 0);
end;

procedure TGPSBabelGUIDialog.btnUseDefaultOutputClick(Sender: TObject);
begin
  if eInput.text <> '' then begin
    if (comboOutput.ItemIndex)> -1 then // Output type is selected
      eOutput.text := LeftStr (eInput.text, AnsiPos (ExtractFileExt (eInput.text), eInput.text)) +
                      GPSDataFormats [comboOutput.ItemIndex].sExt
    else
      MessageDlg ('You must select the output file format.', mtWarning, [mbOk], 0);
  end
  else
    MessageDlg ('Input file is not defined.', mtWarning, [mbOk], 0);
end;

initialization
end.
