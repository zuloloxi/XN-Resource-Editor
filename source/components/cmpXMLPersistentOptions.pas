unit cmpXMLPersistentOptions;

interface

uses Windows, Classes, SysUtils, cmpPersistentOptions, unitExSettings;

type

//---------------------------------------------------------------------
// TXMLPersistentOptions class  - persistent options held in an INI
// file
  TXMLPersistentOptions = class (TCustomPersistentOptions)
  private
    FCustomFileName: string;
  protected
    function GetSettingsClass: TExSettingsClass; override;
    procedure SettingsCreated(Settings: TExSettings); override;
  published
    property FileName: string read FCustomFileName write FCustomFileName;
  end;

implementation

uses unitExXMLSettings;

{ TXMLPersistentOptions }

function TXMLPersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  result := TExXMLSettings;
end;

procedure TXMLPersistentOptions.SettingsCreated(Settings: TExSettings);
begin
  inherited;

  TExXMLSettings(Settings).CustomPath := FileName;
end;

end.
