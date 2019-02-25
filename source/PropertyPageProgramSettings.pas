unit PropertyPageProgramSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls,
  StdCtrls, PropertyPageForm, ComponentPersistentPosition;

type
  TPropertyPageProgramSettingsData = class (TPropertyPageData)
  private
    FInternationalFontName: string;
    FInternationalFontHeight: Integer;
    FParserType: Integer;  // 0 = NT API; 1 = Internal
  protected
    procedure Initialize; override;
  public
    procedure Apply; override;
  end;

  TfmPropertyPageProgramSettings = class(TfmPropertyPage)
    ButtonSelectFont: TButton;
    ComboBoxModuleParser: TComboBox;
    FontDialog: TFontDialog;
    LabelModuleParser: TLabel;
    LabelSelectFont: TLabel;
    StaticTextFontDetails: TStaticText;
    procedure ComboBoxModuleParserChange(Sender: TObject);
    procedure ButtonSelectFontClick(Sender: TObject);
  private
    FData: TPropertyPageProgramSettingsData;
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls (AData: TPropertyPageData); override;
  end;

implementation

uses
  ResourceForm, unitCredProperties;

{$R *.DFM}

{ TfmPropertyPageProgramSettings }

procedure TfmPropertyPageProgramSettings.ButtonSelectFontClick(Sender: TObject);
begin
  FontDialog.Font.Name := FData.FInternationalFontName;
  FontDialog.Font.Height := FData.FInternationalFontHeight;
  if FontDialog.Execute(Handle) then
  begin
    FData.FInternationalFontName := FontDialog.Font.Name;
    FData.FInternationalFontHeight := FontDialog.Font.Height;

    StaticTextFontDetails.Caption := FData.FInternationalFontName
  end;
end;

procedure TfmPropertyPageProgramSettings.ComboBoxModuleParserChange(Sender: TObject);
begin
  FData.FParserType := ComboBoxModuleParser.ItemIndex
end;

class function TfmPropertyPageProgramSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageProgramSettingsData;
end;

procedure TfmPropertyPageProgramSettings.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  FData := AData as TPropertyPageProgramSettingsData;
  StaticTextFontDetails.Caption := FData.FInternationalFontName;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    ComboBoxModuleParser.ItemIndex := FData.FParserType
  else
  begin
    ComboBoxModuleParser.Visible := False;
    LabelModuleParser.Visible := False
  end
end;

{ TPropertyPageProgramSettingsData }

procedure TPropertyPageProgramSettingsData.Apply;
begin
  SetInternationalFont(FInternationalFontName, FInternationalFontHeight);
  gProperties.ParserType := FParserType;
end;

procedure TPropertyPageProgramSettingsData.Initialize;
begin
  FInternationalFontName := gProperties.InternationalFontName;
  FInternationalFontHeight := gProperties.InternationalFontHeight;
  FParserType := gProperties.ParserType;
end;

end.
