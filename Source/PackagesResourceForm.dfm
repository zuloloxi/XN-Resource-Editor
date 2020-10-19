inherited FormPackagesResource: TFormPackagesResource
  Left = 219
  Top = 214
  ActiveControl = PageControlRequiresContains
  Caption = 'Packages Resource'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 185
    Top = 0
    Height = 455
  end
  object PageControlRequiresContains: TPageControl
    Left = 188
    Top = 0
    Width = 513
    Height = 455
    ActivePage = TabSheetContains
    Align = alClient
    TabOrder = 0
    object TabSheetRequires: TTabSheet
      Caption = 'Requires'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ListViewRequires: TListView
        Left = 0
        Top = 0
        Width = 505
        Height = 427
        Align = alClient
        Columns = <
          item
            Caption = 'Requires Packages'
            Width = -2
            WidthType = (
              -2)
          end>
        ColumnClick = False
        GridLines = True
        ReadOnly = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object TabSheetContains: TTabSheet
      Caption = 'Contains'
      ImageIndex = 2
      object ListViewContains: TListView
        Left = 0
        Top = 0
        Width = 505
        Height = 427
        Align = alClient
        Columns = <
          item
            Caption = 'Contains Unit'
            Width = 300
          end
          item
            AutoSize = True
            Caption = 'Flags'
          end>
        GridLines = True
        ReadOnly = True
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object PanelMain: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 455
    Align = alLeft
    BevelOuter = bvLowered
    FullRepaint = False
    TabOrder = 1
    object PropertyListBoxFlags: TPropertyListBox
      Left = 1
      Top = 1
      Width = 183
      Height = 453
      VertScrollBar.Increment = 17
      VertScrollBar.Range = 102
      Align = alClient
      BorderStyle = bsNone
      TabOrder = 0
      TabStop = False
      Properties = <
        item
          PropertyName = 'Never Build'
          PropertyType = ptBoolean
          Tag = 0
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Design Only'
          PropertyType = ptBoolean
          Tag = 0
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Runtime Only'
          PropertyType = ptBoolean
          Tag = 0
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Check Duplicates'
          PropertyType = ptBoolean
          Tag = 0
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Environment'
          PropertyType = ptEnum
          Tag = 0
          EnumValues.Strings = (
            'Pre-V4'
            'Undefined'
            'C++ Builder'
            'Delphi')
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end
        item
          PropertyName = 'Module Type'
          PropertyType = ptEnum
          Tag = 0
          EnumValues.Strings = (
            'EXE'
            'Package DLL'
            'Module DLL'
            'Undefined')
          Enabled = False
          ParentColor = False
          Color = clBlack
          ReadOnly = False
        end>
      ActualValueColWidth = 0
      SpecialButtonImageIndex = -1
      SpecialButtonDisabledImageIndex = -1
      SpecialButtonHotImageIndex = -1
      SpecialButtonPressedImageIndex = -1
    end
  end
end
