unit PropertyPageProgramSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls,
  Menus, StdCtrls, VirtualTrees, PropertyPageForm, cmpPersistentPosition;

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
    FontDialog1: TFontDialog;
    stFontDetails: TStaticText;
    Button1: TButton;
    Label1: TLabel;
    stModuleParser: TLabel;
    cbModuleParser: TComboBox;
    procedure cbModuleParserChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FData: TPropertyPageProgramSettingsData;
  protected
  public
    class function GetDataClass: TPropertyPageDataClass; override;
    procedure PopulateControls (AData: TPropertyPageData); override;
  end;

implementation

uses
  ResourceForm, unitCredProperties;

{$R *.DFM}

{ TfmPropertyPageProgramSettings }

procedure TfmPropertyPageProgramSettings.Button1Click(Sender: TObject);
begin
  FontDialog1.Font.Name := FData.FInternationalFontName;
  FontDialog1.Font.Height := FData.FInternationalFontHeight;
  if FontDialog1.Execute(Handle) then
  begin
    FData.FInternationalFontName := FontDialog1.Font.Name;
    FData.FInternationalFontHeight := FontDialog1.Font.Height;

    stFontDetails.Caption := FData.FInternationalFontName
  end;
end;

procedure TfmPropertyPageProgramSettings.cbModuleParserChange(Sender: TObject);
begin
  FData.FParserType := cbModuleParser.ItemIndex
end;

class function TfmPropertyPageProgramSettings.GetDataClass: TPropertyPageDataClass;
begin
  Result := TPropertyPageProgramSettingsData;
end;

procedure TfmPropertyPageProgramSettings.PopulateControls(AData: TPropertyPageData);
begin
  inherited;
  FData := AData as TPropertyPageProgramSettingsData;
  stFontDetails.Caption := FData.FInternationalFontName;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    cbModuleParser.ItemIndex := FData.FParserType
  else
  begin
    cbModuleParser.Visible := False;
    stModuleParser.Visible := False
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
