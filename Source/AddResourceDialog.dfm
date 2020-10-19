object FormAddResource: TFormAddResource
  Left = 268
  Top = 226
  HelpContext = 1000
  Caption = 'Add Resource'
  ClientHeight = 325
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    286
    325)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView: TListView
    Left = 16
    Top = 16
    Width = 250
    Height = 256
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Resource'
        Width = -2
        WidthType = (
          -2)
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    SmallImages = FormMain.ImageListResources
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewDblClick
  end
  object ButtonOK: TButton
    Left = 110
    Top = 278
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = ButtonOKClick
  end
  object ButtonCancel: TButton
    Left = 191
    Top = 278
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
