object GPSBabelGUIDialog: TGPSBabelGUIDialog
  Left = 461
  Top = 119
  BorderStyle = bsDialog
  Caption = 'GPSBabelGUI'
  ClientHeight = 258
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    380
    258)
  PixelsPerInch = 96
  TextHeight = 13
  object lblGPSBabelURI: TLabel
    Left = 66
    Top = 237
    Width = 250
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'GPSBabel: http://www.gpsbabel.org'
  end
  object lblInputFileName: TLabel
    Left = 17
    Top = 17
    Width = 43
    Height = 13
    Alignment = taRightJustify
    Caption = 'Input file:'
  end
  object lblOutputFileName: TLabel
    Left = 9
    Top = 97
    Width = 51
    Height = 13
    Alignment = taRightJustify
    Caption = 'Output file:'
  end
  object bvlGPSBabelURI: TBevel
    Left = 0
    Top = 222
    Width = 380
    Height = 4
    Anchors = [akLeft, akBottom]
  end
  object lblOutputFormat: TLabel
    Left = 25
    Top = 73
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Format:'
  end
  object lblInputFormat: TLabel
    Left = 25
    Top = 41
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Format:'
  end
  object btnInputFileDialog: TSpeedButton
    Left = 339
    Top = 12
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
      5555555555555555555555555555555555555555555555555555555555555555
      555555555555555555555555555555555555555FFFFFFFFFF555550000000000
      55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
      B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
      000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
      555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
      55555575FFF75555555555700007555555555557777555555555555555555555
      5555555555555555555555555555555555555555555555555555}
    NumGlyphs = 2
    OnClick = btnInputFileDialogClick
  end
  object btnOutputFileDialog: TSpeedButton
    Left = 339
    Top = 92
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333FFFFFFFFFFFFF33000077777770033377777777777773F000007888888
      00037F3337F3FF37F37F00000780088800037F3337F77F37F37F000007800888
      00037F3337F77FF7F37F00000788888800037F3337777777337F000000000000
      00037F3FFFFFFFFFFF7F00000000000000037F77777777777F7F000FFFFFFFFF
      00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
      00037F7F333333337F7F000FFFFFFFFF00037F7F333333337F7F000FFFFFFFFF
      00037F7F333333337F7F000FFFFFFFFF07037F7F33333333777F000FFFFFFFFF
      0003737FFFFFFFFF7F7330099999999900333777777777777733}
    NumGlyphs = 2
    OnClick = btnOutputFileDialogClick
  end
  object comboInput: TComboBox
    Left = 64
    Top = 37
    Width = 300
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object eInput: TEdit
    Left = 64
    Top = 13
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object eOutput: TEdit
    Left = 64
    Top = 93
    Width = 273
    Height = 21
    TabOrder = 2
  end
  object comboOutput: TComboBox
    Left = 64
    Top = 69
    Width = 300
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
  end
  object btnProcess: TButton
    Left = 232
    Top = 141
    Width = 132
    Height = 25
    Caption = '&Process'
    TabOrder = 5
    OnClick = btnProcessClick
  end
  object cbIgnoreShort: TCheckBox
    Left = 64
    Top = 145
    Width = 121
    Height = 17
    Caption = 'Ignore "short" names'
    TabOrder = 4
  end
  object btnExit: TButton
    Left = 319
    Top = 184
    Width = 45
    Height = 25
    Caption = 'E&xit'
    TabOrder = 6
    OnClick = btnExitClick
  end
  object btnAbout: TButton
    Left = 64
    Top = 184
    Width = 45
    Height = 25
    Caption = '&About'
    TabOrder = 7
    OnClick = btnAboutClick
  end
  object btnIntro: TButton
    Left = 149
    Top = 184
    Width = 45
    Height = 25
    Caption = '&Intro'
    TabOrder = 8
    OnClick = btnIntroClick
  end
  object btnUseDefaultOutput: TButton
    Left = 64
    Top = 117
    Width = 114
    Height = 19
    Caption = '&Use Default Filename'
    TabOrder = 9
    OnClick = btnUseDefaultOutputClick
  end
  object btnHowTo: TButton
    Left = 234
    Top = 184
    Width = 48
    Height = 25
    Caption = '&How to...'
    TabOrder = 10
    OnClick = btnHowToClick
  end
  object dlgOpenInput: TOpenDialog
    Options = [ofEnableSizing]
    Left = 363
    Top = 9
  end
  object dlgSaveOutput: TSaveDialog
    Left = 363
    Top = 88
  end
  object TimerLoadFormats: TTimer
    OnTimer = TimerLoadFormatsTimer
    Left = 350
    Top = 229
  end
end
