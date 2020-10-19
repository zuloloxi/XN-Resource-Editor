inherited FormPropertyPageRCSettings: TFormPropertyPageRCSettings
  ActiveControl = vstIncludePackages
  Caption = 'RC File Options'
  ClientHeight = 299
  ClientWidth = 278
  Constraints.MinHeight = 274
  Constraints.MinWidth = 269
  OnDestroy = FormDestroy
  ExplicitWidth = 278
  ExplicitHeight = 299
  PixelsPerInch = 96
  TextHeight = 13
  inherited PanelTop: TPanel
    Width = 278
    ExplicitWidth = 278
    inherited BevelTop: TBevel
      Width = 278
      ExplicitWidth = 269
    end
    inherited LabelSectionDetails: TLabel
      Width = 266
      Caption = 
        'Select the method of determining the Include path for .RC file c' +
        'ompilation'
      ExplicitWidth = 257
    end
  end
  object vstIncludePackages: TVirtualStringTree
    Left = 31
    Top = 83
    Width = 231
    Height = 109
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
    NodeDataSize = 4
    TabOrder = 2
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
    OnChecked = vstIncludePackagesChecked
    OnGetText = vstIncludePackagesGetText
    OnInitNode = vstIncludePackagesInitNode
    Columns = <
      item
        Position = 0
        Width = 231
        WideText = 'Include Header Packages'
      end>
  end
  object RadioButtonCustomIncludePath: TRadioButton
    Left = 16
    Top = 235
    Width = 177
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use a Custo&m Include Path'
    TabOrder = 4
    OnClick = RadioButtonCustomIncludePathClick
  end
  object EditCustomIncludePath: TEdit
    Left = 32
    Top = 254
    Width = 203
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 5
  end
  object ButtonCustomIncludePath: TButton
    Left = 241
    Top = 254
    Width = 21
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = '...'
    TabOrder = 6
  end
  object RadioButtonCompilerIncludePath: TRadioButton
    Left = 16
    Top = 60
    Width = 177
    Height = 17
    Caption = 'Use the &Compiler Include Path'
    TabOrder = 1
    OnClick = RadioButtonCustomIncludePathClick
  end
  object RadioButtonEnvironmentVariableIncludePath: TRadioButton
    Left = 16
    Top = 203
    Width = 209
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use the '#39'Include'#39' Environment Variable'
    TabOrder = 3
    OnClick = RadioButtonCustomIncludePathClick
  end
end
