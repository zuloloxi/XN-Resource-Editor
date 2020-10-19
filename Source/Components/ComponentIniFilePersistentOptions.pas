unit ComponentIniFilePersistentOptions;

interface

uses
  Windows, Classes, SysUtils, ComponentPersistentOptions, unitExSettings;

type
//---------------------------------------------------------------------
// TIniFilePersistentOptions class  - persistent options held in an INI
// file
  TIniFilePersistentOptions = class (TCustomPersistentOptions)
  private
    FCustomFileName: string;
  protected
    function GetSettingsClass: TExSettingsClass; override;
    procedure SettingsCreated (settings: TExSettings); override;
  published
    property FileName: string read fCustomFileName write fCustomFileName;
  end;

implementation

uses
  unitExIniSettings;

{ TIniFilePersistentOptions }

function TIniFilePersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  Result := TExIniSettings;
end;

procedure TIniFilePersistentOptions.SettingsCreated(settings: TExSettings);
begin
  inherited;

  TExIniSettings(settings).CustomPath := FCustomFileName;
end;

end.
