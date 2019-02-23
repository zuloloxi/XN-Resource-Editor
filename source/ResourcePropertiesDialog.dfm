object dlgResourceProperties: TdlgResourceProperties
  Left = 199
  Top = 124
  Caption = 'Resource Properties'
  ClientHeight = 123
  ClientWidth = 304
  Color = clBtnFace
  Constraints.MaxHeight = 161
  Constraints.MinHeight = 150
  Constraints.MinWidth = 252
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    304
    123)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelName: TLabel
    Left = 8
    Top = 15
    Width = 28
    Height = 13
    Caption = '&Name'
  end
  object LabelLanguage: TLabel
    Left = 8
    Top = 54
    Width = 48
    Height = 13
    Caption = '&Language'
    FocusControl = ComboBoxLanguage
  end
  object ComboBoxLanguage: TComboBox
    Left = 83
    Top = 51
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Sorted = True
    TabOrder = 1
  end
  object ButtonOK: TButton
    Left = 137
    Top = 90
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object ButtonCancel: TButton
    Left = 218
    Top = 90
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object EditName: TEdit
    Left = 83
    Top = 12
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
