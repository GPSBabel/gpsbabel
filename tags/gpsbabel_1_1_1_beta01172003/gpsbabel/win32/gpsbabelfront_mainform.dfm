object FormGPSBabelFront: TFormGPSBabelFront
  Left = 208
  Top = 103
  BorderStyle = bsDialog
  Caption = 'GPSBabel GUI Frontend'
  ClientHeight = 312
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    369
    312)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 292
    Width = 250
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'GPSBabel: http://sourceforge.net/projects/gpsbabel'
  end
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 43
    Height = 13
    Caption = 'Input file:'
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 51
    Height = 13
    Caption = 'Output file:'
  end
  object Bevel1: TBevel
    Left = -3
    Top = 285
    Width = 380
    Height = 2
    Anchors = [akLeft, akBottom]
  end
  object Label4: TLabel
    Left = 16
    Top = 104
    Width = 35
    Height = 13
    Caption = 'Format:'
  end
  object Label5: TLabel
    Left = 16
    Top = 40
    Width = 35
    Height = 13
    Caption = 'Format:'
  end
  object Label6: TLabel
    Left = 8
    Top = 168
    Width = 33
    Height = 13
    Caption = 'Result:'
  end
  object comboInput: TComboBox
    Left = 64
    Top = 40
    Width = 297
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object editInput: TEdit
    Left = 64
    Top = 16
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object editOutput: TEdit
    Left = 64
    Top = 80
    Width = 273
    Height = 21
    TabOrder = 3
  end
  object comboOutput: TComboBox
    Left = 64
    Top = 104
    Width = 297
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
  end
  object btnProcess: TButton
    Left = 288
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Process'
    TabOrder = 7
    OnClick = btnProcessClick
  end
  object cbIgnoreShort: TCheckBox
    Left = 64
    Top = 136
    Width = 169
    Height = 17
    Caption = 'Ignore "short" names'
    TabOrder = 6
  end
  object btnInput: TButton
    Left = 341
    Top = 16
    Width = 19
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnInputClick
  end
  object btnOutput: TButton
    Left = 341
    Top = 80
    Width = 19
    Height = 21
    Caption = '...'
    TabOrder = 4
    OnClick = btnOutputClick
  end
  object memoStdErr: TMemo
    Left = 64
    Top = 168
    Width = 297
    Height = 113
    Color = clBtnHighlight
    ReadOnly = True
    TabOrder = 8
  end
  object OpenDialogInput: TOpenDialog
    Left = 248
    Top = 192
  end
  object SaveDialogOutput: TSaveDialog
    Left = 248
    Top = 240
  end
end
