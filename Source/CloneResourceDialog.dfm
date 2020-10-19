object FormCloneResource: TFormCloneResource
  Left = 0
  Top = 0
  Caption = 'Clone Resource'
  ClientHeight = 185
  ClientWidth = 340
  Color = clBtnFace
  Constraints.MaxHeight = 212
  Constraints.MinHeight = 212
  Constraints.MinWidth = 275
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    340
    185)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelInfo: TLabel
    Left = 16
    Top = 16
    Width = 305
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Please specify a new name or language for the cloned resource'
    WordWrap = True
  end
  object ComboBoxLanguage: TComboBox
    Left = 96
    Top = 108
    Width = 226
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Sorted = True
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 166
    Top = 152
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object ButtonCancel: TButton
    Left = 247
    Top = 152
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object RadioButtonByName: TRadioButton
    Left = 16
    Top = 70
    Width = 65
    Height = 17
    Caption = '&Name'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object RadioButtonByLanguage: TRadioButton
    Left = 16
    Top = 110
    Width = 73
    Height = 17
    Caption = '&Language'
    TabOrder = 1
  end
  object EditName: TEdit
    Left = 96
    Top = 68
    Width = 225
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
  end
end
