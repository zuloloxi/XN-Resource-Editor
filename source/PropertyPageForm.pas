unit PropertyPageForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TPropertyPageData = class
  private
    FCaption: string;
    FHelpText: string;
    FHelpKeyword : string;
    FParam: Integer;
    FMinX : Integer;
    FMinY : Integer;
    FInitialized : boolean;
  protected

    property Param : Integer read FParam;
    procedure Initialize; virtual; abstract;
    function GetCaption: string; virtual;
    function GetHelpText: string; virtual;
  public
    constructor Create (const ACaption, AHelpText, AHelpKeyword : string; AMinCX, AMinCY : Integer; AParam : Integer = 0);

    procedure Apply; virtual;
    procedure Cancel; virtual;

    property Caption: string read GetCaption;
    property Initialized: boolean read FInitialized write FInitialized;
    property HelpText: string read GetHelpText;
    property HelpKeyword: string read FHelpKeyword;

    property MinX: Integer read FMinX;
    property MinY: Integer read FMinY;
  end;

  TPropertyPageDataClass = class of TPropertyPageData;

  TfmPropertyPage = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    stSectionDetails: TLabel;
  private
    fAltKeyword: string;
  protected
    fPopulating: boolean;
  public
    class function GetDataClass: TPropertyPageDataClass; virtual; abstract;
    procedure PopulateControls (AData: TPropertyPageData); virtual;

    property Populating: boolean read fPopulating write fPopulating;
    property AltKeyword: string read fAltKeyword;
  end;

  TPropertyPageClass = class of TfmPropertyPage;

implementation

{$R *.dfm}

{ TfmPropertyPage }

procedure TfmPropertyPage.PopulateControls (AData: TPropertyPageData);
begin
  if not AData.FInitialized then
  begin
    AData.Initialize;
    AData.FInitialized := True
  end;
  stSectionDetails.Caption := AData.HelpText;
  fAltKeyword := AData.HelpKeyword;
end;

{ TPropertyPageData }

procedure TPropertyPageData.Apply;
begin
// Stub
end;

procedure TPropertyPageData.Cancel;
begin
// Stub
end;

constructor TPropertyPageData.Create(const ACaption, AHelpText, AHelpKeyword: string; AMinCX, AMinCY: Integer;
  AParam: Integer);
begin
  FCaption := ACaption;
  FHelpText := AHelpText;
  FHelpKeyword := AHelpKeyword;
  FParam := AParam;
  FMinX := AMinCX;
  FMinY := AMinCY;
end;

function TPropertyPageData.GetCaption: string;
begin
  result := FCaption
end;

function TPropertyPageData.GetHelpText: string;
begin
  result := FHelpText
end;

end.
