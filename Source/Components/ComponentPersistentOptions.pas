(*======================================================================*
 | ComponentPersistentOptions                                           |
 |                                                                      |
 | TRegistryPersistentOptions & TIniFilePersistentOptions components    |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      10/09/2003  CPWW  Original                                  |
 | 2.0      27/04/2004  CPWW  Added impersonation so that interactive   |
 |                            services can access HKEY_CURRENT_USER     |
 |                            for the logged-on user.                   |
 *======================================================================*)

unit ComponentPersistentOptions;

interface

uses
  Windows, SysUtils, Classes, unitExSettings;

const
  systemUser = 'SYSTEM';

type
  TOptions = class;
  TSections = class;
  TPersistentOptions = class;

  TOptionType = (otInteger, otBoolean, otString, otEnum);

//---------------------------------------------------------------------
// TOption class.
  TOption = class (TCollectionItem)
  private
    FDefaultValue: string;
    FName: string;
    FEnumValues: TStringList;
    FOptionType: TOptionType;
    FIntVal: Integer;
    FStrVal: string;
    FBoolVal: Boolean;
    FDirty: Boolean;

    function GetBase: TPersistentOptions;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsEnum: string;

    procedure SetEnumValues(const Value: TStrings);
    function GetEnumValues: TStrings;
    procedure SetOptionType(const Value: TOptionType);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsEnum(const Value: string);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    procedure Flush;
    function GetHasDefaultValue: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create (Collection: TCollection); override;
    destructor Destroy; override;
    property Base: TPersistentOptions read GetBase;

    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsEnum: string read GetAsEnum write SetAsEnum;

    property HasDefaultValue: Boolean read GetHasDefaultValue;

  published
    property Name: string read FName write FName;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property EnumValues: TStrings read GetEnumValues write SetEnumValues;
    property OptionType: TOptionType read FOptionType write SetOptionType;
  end;

//---------------------------------------------------------------------
// TOptions class - a collection of options
  TOptions = class (TOwnedCollection)
  private
    FDeleteObsoleteOptions: Boolean;
    function GetOption(idx: Integer): TOption;
    function GetOptionByName(const name: string): TOption;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    property Option [idx: Integer]: TOption read GetOption; default;
    property OptionByName [const name: string]: TOption read GetOptionByName;
  published
    property DeleteObsoleteOptions: Boolean read FDeleteObsoleteOptions write FDeleteObsoleteOptions default True;
  end;

//---------------------------------------------------------------------
// TSection class
  TSection = class (TCollectionItem)
  private
    FName: string;
    FOptions: TOptions;
    FSections: TSections;
    function GetOption(const name: string): TOption;
    function GetSection(const name: string): TSection;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Option [const name: string]: TOption read GetOption;
    property Section [const name: string]: TSection read GetSection;
  published
    property Name: string read FName write FName;
    property Options: TOptions read FOptions write FOptions;
    property Sections: TSections read FSections write FSections;
  end;

//---------------------------------------------------------------------
// TSections class - a collection of sections.
  TSections = class (TOwnedCollection)
  private
    FDeleteObsoleteSections: Boolean;
    function GetSection(idx: Integer): TSection;
    function GetSectionByName(const name: string): TSection;
  public
    property Section [idx: Integer]: TSection read GetSection; default;
    property SectionByName [const name: string]: TSection read GetSectionByName;
  published
    property DeleteObsoleteSections: Boolean read FDeleteObsoleteSections write FDeleteObsoleteSections default False;
  end;

//---------------------------------------------------------------------
// TPersistentOptions - base class for TRegistryPersistentOptions and
// TIniFilePersistentOptions
  TPersistentOptions = class(TComponent)
  private
    function GetPersist: Boolean;
    function GetLoading: Boolean;
  private
    FManufacturer: string;
    FApplication: string;
    FVersion: string;
    FOptions: TOptions;
    FSections: TSections;
    FUpdating: Boolean;
    function GetDesigning: Boolean;
    function GetOption(path: string): TOption;
    function GetSection(name: string): TSection;
    procedure SetApplication(const Value: string);
    procedure SetManufacturer(const Value: string);
    procedure SetVersion(const Value: string);
    procedure RemoveLeadingSlash (var path: string);

    property Designing: Boolean read GetDesigning;
    property Loading: Boolean read GetLoading;
    property Persist: Boolean read GetPersist;
    function GetDirty: Boolean;
  protected
    procedure Loaded; override;
    procedure DeleteOldOptions (const application, manufacturer, version: string); virtual; abstract;
    property InUpdate: Boolean read FUpdating;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Option [path: string]: TOption read GetOption; default;
    property Section [name: string]: TSection read GetSection;
    property Dirty: Boolean read GetDirty;
  published
    property Application: string read FApplication write SetApplication;
    property Manufacturer: string read FManufacturer write SetManufacturer;
    property Version: string read FVersion write SetVersion;
    property Options: TOptions read FOptions write FOptions;
    property Sections: TSections read FSections write FSections;
  end;

  TCustomPersistentOptions = class (TPersistentOptions)
  private
    FOptionsType: TExSettingsType;
    procedure LoadOptions (settings: TExSettings; Options: TOptions; forceDefaults: Boolean);
    procedure LoadSections (settings: TExSettings; Sections: TSections; forceDefaults: Boolean);
    procedure SaveOptions (settings: TExSettings; Options: TOptions);
    procedure SaveSections (settings: TExSettings; Sections: TSections);
  protected
    procedure DeleteOldOptions (const application, manufacturer, version: string); override;
    property OptionsType: TExSettingsType read FOptionsType write FOptionsType;
    function GetSettingsClass: TExSettingsClass; virtual; abstract;
    function GetSettingsFile: string; virtual;
    procedure SettingsCreated (settings: TExSettings); virtual;
  public
    procedure Load; override;
    procedure Save; override;
  published
  end;

  TOnGetSettingsClass = procedure (sender: TObject; var settingsType: TExSettingsClass) of object;
  TOnGetSettingsFile = procedure (sender: TObject; var fileName: string) of object;

  TUniPersistentOptions = class (TCustomPersistentOptions)
  private
    FOnGetSettingsClass: TOnGetSettingsClass;
    FOnGetSettingsFile: TOnGetSettingsFile;
  protected
    function GetSettingsClass: TExSettingsClass; override;
    function GetSettingsFile: string; override;
  published
    property OptionsType;
    property OnGetSettingsClass: TOnGetSettingsClass read FOnGetSettingsClass write FOnGetSettingsClass;
    property OnGetSettingsFile: TOnGetSettingsFile read FOnGetSettingsFile write FOnGetSettingsFile;
  end;

  EOptionError = class (Exception)
  end;

  EOptionTypeMismatch = class (EOptionError)
  public
    constructor Create (Option: TOption);
  end;

resourcestring
  rstNoAppName = 'Application property can not be blank';

implementation

uses
  unitExFileSettings;

resourcestring
  rstNotEnum = 'Not an enum type';
  rstTypeMismatch = '%s is not of %s type';
  rstSectionNotFound = 'Section %s not found';
  rstOptionNotFound = 'Option %s not found in section %s';

const
  OptionTypeNames: array [TOptionType] of string = ('integer', 'Boolean', 'string', 'emumerated');

{ TPersistentOptions }

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.BeginUpdate                             |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |A
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.BeginUpdate;
begin
  FUpdating := True
end;

(*----------------------------------------------------------------------*
 | constructor TPersistentOptions.Create                                |
 |                                                                      |
 | Constructor for TPersistentOptions                                   |
 *----------------------------------------------------------------------*)
constructor TPersistentOptions.Create (AOwner: TComponent);
begin
  inherited Create (AOwner);
  FOptions := TOptions.Create(Self, TOption);
  FSections := TSections.Create(Self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TPersistentOptions.Destroy                                |
 |                                                                      |
 | Destructor for TPersistentOptions                                    |
 *----------------------------------------------------------------------*)
destructor TPersistentOptions.Destroy;
begin
  if Dirty then
    Save;
  FOptions.Free;
  FSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.EndUpdate                               |
 |                                                                      |
 | End a batch update started with BeginUpdate                          |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.EndUpdate;
begin
  if FUpdating and Dirty then
    Save;

  FUpdating := False
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDesigning                             |
 |                                                                      |
 | Return True if the component is in 'design' mode.                    |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDesigning: Boolean;
begin
  Result := csDesigning in ComponentState
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetDirty                                 |
 |                                                                      |
 | Return True if any option has had it's value changed, but has not    |
 | yet been persisted to the registry or INI file.                      |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetDirty: Boolean;

  function OptionsDirty (options: TOptions): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to options.Count - 1 do
      if options [i].FDirty then
      begin
        Result := True;
        Break;
      end
  end;

  function SectionsDirty (sections: TSections): Boolean;
  var
    i: Integer;
    section: TSection;
  begin
    Result := False;
    for i := 0 to sections.Count - 1 do
    begin
      section := sections [i];
      Result := OptionsDirty (section.Options);
      if not Result then
        Result := SectionsDirty (section.Sections);
      if Result then
        Break;
    end
  end;

begin
  Result := OptionsDirty (Options) or SectionsDirty (Sections)
end;

(*----------------------------------------------------------------------*
 | TPersistentOptions.GetOption                                         |
 |                                                                      |
 | Return an option by name or path.  Note that as a shortcut the       |
 | path passed can contain sections - so you can say                    |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Option ['Width']          |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Option ['Position\Width']                      |
 |                                                                      |
 | or even (because it's the default property):                         |
 |                                                                      |
 |   MyPersistentOptions ['Position\Width']                             |
 |                                                                      |
 | Parameters:                                                          |
 |   path: string               The option name or path                 |
 |                                                                      |
 | The function always returns a valid TOption, or raised an exception  |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetLoading: Boolean;
begin
  Result := (csLoading in ComponentState);

end;

function TPersistentOptions.GetOption(path: string): TOption;
var
  p: PChar;
  n: Integer;
  s: TSection;
  secName: string;
begin
  RemoveLeadingSlash (path);
  p := StrRScan (PChar (path), '\');

  s := Nil;
  if Assigned(p) then
  begin
    n := Integer (p) - Integer (PChar (path)) + 1;
    s := Sections.SectionByName [Trim (Copy (path, 1, n - 1))];
    path := Trim (Copy (path, n + 1, MaxInt));
  end;

  if Assigned(s) then
    Result := s.Options.OptionByName [path]
  else
    Result := Options.OptionByName [path];

  if Result = Nil then
  begin
    if Assigned(s) then
      secName := s.Name
    else
      secName := '[Default]';
    raise EOptionError.CreateFmt(rstOptionNotFound, [path, secName])
  end
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetPersist                               |
 |                                                                      |
 | Return True if changes to the option values should be persisted to   |
 | the registry or INI file - ie. it's not in design mode, and it's not |
 | Loading.                                                             |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetPersist: Boolean;
begin
  Result := not Designing and not(csLoading in ComponentState);
end;

(*----------------------------------------------------------------------*
 | function TPersistentOptions.GetSection                               |
 |                                                                      |
 | Return a section by name or path.  Note that as a shortcut the       |
 | path passed can contain sub-sections - so you can say                |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position'].Section ['Attributes']    |
 |                                                                      |
 | or                                                                   |
 |                                                                      |
 |   MyPersistentOptions.Section ['Position\Attributes']                |
 |                                                                      |
 | Parameters:                                                          |
 |   name: string               The section name or path                |
 |                                                                      |
 | The function returns a valid TSection, or raises an exception        |
 *----------------------------------------------------------------------*)
function TPersistentOptions.GetSection(name: string): TSection;
begin
  RemoveLeadingSlash (name);
  Result := Sections.SectionByName [name]
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.Loaded                                  |
 |                                                                      |
 | Overridden 'Loaded' method.  Load the registry or ini file           |
 | information.                                                         |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.Loaded;
begin
  inherited;
  Load
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.RemoveLeadingSlash                      |
 |                                                                      |
 | Remove the leading slash from a path.                                |
 *----------------------------------------------------------------------*)

procedure TPersistentOptions.RemoveLeadingSlash (var path: string);
begin
  if Copy (path, 1, 1) = '\' then
    path := Copy (path, 2, MaxInt);
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetApplication                          |
 |                                                                      |
 | Set method for 'Application' property.  If this is changed at        |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Application' value         |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetApplication(const Value: string);
begin
  FApplication := Value;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetManufacturer                         |
 |                                                                      |
 | Set method for 'Manufacturer' property.  If this is changed at       |
 | runtime clear the old .ini file, or delete the old registry entries  |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Manufacturer' value        |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetManufacturer(const Value: string);
begin
  FManufacturer := Value;
end;

(*----------------------------------------------------------------------*
 | procedure TPersistentOptions.SetVersion                              |
 |                                                                      |
 | Set method for 'Version' property.  If this is changed at runtime    |
 | clear the old .ini file, or delete the old registry entries          |
 |                                                                      |
 | Parameters:                                                          |
 |   const Value: string                New 'Version' value             |
 *----------------------------------------------------------------------*)
procedure TPersistentOptions.SetVersion(const Value: string);
begin
  FVersion := Value;
end;

{ TOptions }

(*----------------------------------------------------------------------*
 | constructor TOptions.Create                                          |
 |                                                                      |
 | Constructor for TOptions collection                                  |
 *----------------------------------------------------------------------*)
constructor TOptions.Create(AOwner: TPersistent;
  ItemClass: TCollectionItemClass);
begin
  inherited Create (AOwner, ItemClass);
  FDeleteObsoleteOptions := True
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOption                                         |
 |                                                                      |
 | Get method for 'Option' array property                               |
 *----------------------------------------------------------------------*)
function TOptions.GetOption(idx: Integer): TOption;
begin
  Result := TOption (Items [idx])
end;

(*----------------------------------------------------------------------*
 | procedure TOptions.GetOptionByName                                   |
 |                                                                      |
 | 'Get' method for OptionByName array property                         |
 *----------------------------------------------------------------------*)
function TOptions.GetOptionByName(const name: string): TOption;
var
  o: TOption;
  i: Integer;
begin
  Result := Nil;
  for i := 0 to Count - 1 do
  begin
    o := Option [i];
    if AnsiSameText(Name, o.Name) then
    begin
      Result := o;
      Break
    end
  end
end;

{ TOption }

(*----------------------------------------------------------------------*
 | constructor TOption.Create                                           |
 |                                                                      |
 | Constructor for TOption class                                        |
 *----------------------------------------------------------------------*)
constructor TOption.Create(Collection: TCollection);
begin
  inherited;
  if Base.Designing or Base.Loading then
  begin
    FEnumValues := TStringList.Create;
    FEnumValues.CaseSensitive := False
  end
end;

(*----------------------------------------------------------------------*
 | destructor TOption.Destroy                                           |
 |                                                                      |
 | Destructor for TOption class                                         |
 *----------------------------------------------------------------------*)
destructor TOption.Destroy;
begin
  FEnumValues.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TOption.Flush                                              |
 |                                                                      |
 | Save the option.                                                     |
 *----------------------------------------------------------------------*)
procedure TOption.Flush;
begin
  if FDirty and not Base.InUpdate then
    Base.Save
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsBoolean                                        |
 |                                                                      |
 | Return the option value if it's a Boolean option - otherwise raise   |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsBoolean: Boolean;
begin
  if OptionType = otBoolean then
    Result := FBoolVal
  else
    raise EOptionTypeMismatch (Self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsEnum                                           |
 |                                                                      |
 | Return the option value if it's an enum option - otherwise raise an  |
 | exception.                                                           |
 *----------------------------------------------------------------------*)
function TOption.GetAsEnum: string;
begin
  if OptionType = otEnum then
    Result := EnumValues [FIntVal]
  else
    raise EOptionTypeMismatch (Self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsInteger                                        |
 |                                                                      |
 | Return the option value if it's an integer option - otherwise raise  |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsInteger: Integer;
begin
  if OptionType = otInteger then
    Result := FIntVal
  else
    raise EOptionTypeMismatch (Self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetAsString                                         |
 |                                                                      |
 | Return the option value if it's a string option - otherwise raise    |
 | an exception.                                                        |
 *----------------------------------------------------------------------*)
function TOption.GetAsString: String;
begin
  if OptionType = otString then
    Result := FStrVal
  else
    raise EOptionTypeMismatch (Self)
end;

(*----------------------------------------------------------------------*
 | function TOption.GetBase                                             |
 |                                                                      |
 | Return the owning 'TPersistentOptions' derived object.               |
 *----------------------------------------------------------------------*)
function TOption.GetBase: TPersistentOptions;
var
  own: TPersistent;
begin
  own := TOwnedCollection (Collection).Owner;

  while own is TSection do
    own := TOwnedCollection (TSection (own).Collection).Owner;

  Result := own as TPersistentOptions
end;

(*----------------------------------------------------------------------*
 | function TOption.GetDisplayName                                      |
 |                                                                      |
 | Overridden from TCollectionItem base.  Helps the designer.           |
 *----------------------------------------------------------------------*)
function TOption.GetDisplayName: string;
begin
  Result := Name
end;

(*----------------------------------------------------------------------*
 | function TOption.GetEnumValues: TStringList                         |
 |                                                                      |
 | 'Get' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
function TOption.GetEnumValues: TStrings;
begin
  Result := FEnumValues
end;

(*----------------------------------------------------------------------*
 | function TOption.GetHasDefaultValue: Boolean                        |
 |                                                                      |
 | Return True if the option's current value is it's default.           |
 *----------------------------------------------------------------------*)
function TOption.GetHasDefaultValue: Boolean;
begin
  Result := False;
  case OptionType of
    otString: Result := AnsiCompareStr (DefaultValue, FStrVal) = 0;
    otInteger: Result := StrToIntDef (DefaultValue, 0) = FIntVal;
    otBoolean: Result := StrToBoolDef (DefaultValue, False) = FBoolVal;
    otEnum: Result := FIntVal = EnumValues.IndexOf(DefaultValue)
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsBoolean                                       |
 |                                                                      |
 | Set the option's value if it's a Boolean option - otherwise raise    |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsBoolean(const Value: Boolean);
begin
  if OptionType <> otBoolean then
    raise EOptionTypeMismatch (Self);

  if Value <> FBoolVal then
  begin
    FDirty := True;
    FBoolVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsEnum                                          |
 |                                                                      |
 | Set the option's value if it's an enum option - otherwise raise      |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsEnum(const Value: string);
begin
  if Value <> AsEnum then
  begin
    FDirty := True;
    FIntVal := EnumValues.IndexOf(Value);
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsInteger                                       |
 |                                                                      |
 | Set the option's value if it's an integer option - otherwise raise   |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsInteger(const Value: Integer);
begin
  if OptionType <> otInteger then
    raise EOptionTypeMismatch (Self);

  if Value <> FIntVal then
  begin
    FDirty := True;
    FIntVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetAsString                                        |
 |                                                                      |
 | Set the option's value if it's a string option - otherwise raise     |
 | an exception                                                         |
 *----------------------------------------------------------------------*)
procedure TOption.SetAsString(const Value: string);
begin
  if OptionType <> otString then
    raise EOptionTypeMismatch (Self);

  if Value <> FStrVal then
  begin
    FDirty := True;
    FStrVal := Value;
    Flush
  end
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetEnumValues                                      |
 |                                                                      |
 | 'Set' method for EnumValues property                                 |
 *----------------------------------------------------------------------*)
procedure TOption.SetEnumValues(const Value: TStrings);
begin
  if (OptionType = otEnum) then
    FEnumValues.Assign(Value)
end;

(*----------------------------------------------------------------------*
 | procedure TOption.SetOptionType                                      |
 |                                                                      |
 | 'Set' method for the OptionType property.                            |
 *----------------------------------------------------------------------*)
procedure TOption.SetOptionType(const Value: TOptionType);
begin
  if FOptionType <> Value then
  begin
    if Base.Designing then
    begin
      if not Base.Loading then FEnumValues.Clear
    end
    else
      if not Base.Loading or (Value <> otEnum) then
        FreeAndNil (FEnumValues);

    FOptionType := Value;

    if FOptionType = otEnum then
      if not Base.Designing and not Base.Loading then
        FEnumValues := TStringList.Create
  end
end;

{ TSection }

(*----------------------------------------------------------------------*
 | constructor TSection.Create                                          |
 |                                                                      |
 | Constructor for TSection                                             |
 *----------------------------------------------------------------------*)
constructor TSection.Create(Collection: TCollection);
begin
  inherited;

  FOptions := TOptions.Create(Self, TOption);
  FSections := TSections.Create(Self, TSection);
end;

(*----------------------------------------------------------------------*
 | destructor TSection.Destroy                                          |
 |                                                                      |
 | Destructor for TSection                                              |
 *----------------------------------------------------------------------*)
destructor TSection.Destroy;
begin
  FOptions.Free;
  FSections.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TSection.GetDisplayName                                     |
 |                                                                      |
 | Override TCollectionItem method to help the designer                 |
 *----------------------------------------------------------------------*)
function TSection.GetDisplayName: string;
begin
  Result := Name
end;

(*----------------------------------------------------------------------*
 | function TSection.GetOption                                          |
 |                                                                      |
 | 'Get' method for Option property                                     |
 *----------------------------------------------------------------------*)
function TSection.GetOption(const name: string): TOption;
begin
  Result := Options.OptionByName [name];
  if Result = Nil then
    raise Exception.Create ('Option ' + name + ' not found');
end;

(*----------------------------------------------------------------------*
 | function TSection.GetSection                                         |
 |                                                                      |
 | 'Get' method for Section propery                                     |
 *----------------------------------------------------------------------*)
function TSection.GetSection(const name: string): TSection;
begin
  Result := Sections.SectionByName [name];
  if Result = Nil then
    raise Exception.Create ('Section ' + name + ' not found');
end;

{ EOptionTypeMismatch }

constructor EOptionTypeMismatch.Create(Option: TOption);
begin
  inherited CreateFmt(rstTypeMismatch, [Option.Name, OptionTypeNames [Option.OptionType]])
end;

{ TSections }

(*----------------------------------------------------------------------*
 | function TSections.GetSection                                        |
 |                                                                      |
 | Get method for Section property.                                     |
 *----------------------------------------------------------------------*)
function TSections.GetSection(idx: Integer): TSection;
begin
  Result := TSection (Items [idx])
end;

(*----------------------------------------------------------------------*
 | function TSections.GetSectionByName                                  |
 |                                                                      |
 | 'Get' method for SectionByName property                              |
 *----------------------------------------------------------------------*)
function TSections.GetSectionByName(const name: string): TSection;
var
  i, p: Integer;
  s: TSection;
begin
  Result := Nil;

  p := Pos ('\', name);
  if p > 0 then
  begin
    s := SectionByName [Trim (Copy (name, 1, p - 1))];
    if Assigned(s) then
      Result := s.Sections.SectionByName [Trim (Copy (name, p + 1, MaxInt))]
    else
      raise EOptionError.CreateFmt(rstSectionNotFound, [s.Name]);
  end
  else
  for i := 0 to Count - 1 do
  begin
    s := Section [i];
    if AnsiSameText(name, s.Name) then
    begin
      Result := s;
      Break
    end
  end;

  if not Assigned(Result) then
    raise EOptionError.CreateFmt(rstSectionNotFound, [name])
end;

{ TCustomPersistentOptions }


procedure TCustomPersistentOptions.DeleteOldOptions(const application, manufacturer,
  version: string);
begin
//
end;

function TCustomPersistentOptions.GetSettingsFile: string;
begin
// stub
end;

procedure TCustomPersistentOptions.Load;
var
  settings: TExSettings;
  openedOK: Boolean;
begin
  if not Persist then Exit;

  settings := GetSettingsClass.Create (Manufacturer, Application, Version, FOptionsType);
  try
    SettingsCreated (settings);
    openedOk := settings.Open(True);
    LoadOptions (settings, Options, not openedOK);
    LoadSections (settings, Sections, not openedOK)
  finally
    settings.Free
  end
end;

procedure TCustomPersistentOptions.LoadOptions(settings: TExSettings;
  Options: TOptions; forceDefaults: Boolean);
var
  i: Integer;
  option: TOption;
begin
  for i := 0 to Options.Count - 1 do
  begin
    option := Options [i];

    if forceDefaults or not settings.HasValue(option.Name) then
    case option.OptionType of
      otString: option.FStrVal  := option.FDefaultValue;
      otInteger: option.FIntVal  := StrToIntDef (option.FDefaultValue, 0);
      otBoolean: option.FBoolVal := StrToBoolDef (option.FDefaultValue, False);
      otEnum: option.FIntVal  := option.FEnumValues.IndexOf(option.FDefaultValue)
    end
    else
    case option.OptionType of
      otString: option.FStrVal  := settings.StringValue [option.Name];
      otInteger: option.FIntVal  := settings.IntegerValue [option.Name];
      otBoolean: option.FBoolVal := settings.BooleanValue [option.Name];
      otEnum: option.FIntVal  := settings.IntegerValue [option.Name]
    end;
    option.FDirty := False
  end
end;

procedure TCustomPersistentOptions.LoadSections(settings: TExSettings;
  Sections: TSections; forceDefaults: Boolean);
var
  i: Integer;
  section: TSection;
  settings1: TExSettings;
begin
  for i := 0 to Sections.Count - 1 do
  begin
    section := Sections [i];

    settings1 := Nil;
    try
      if not forceDefaults then
      begin
        settings1 := GetSettingsClass.CreateChild(settings, section.Name);
        forceDefaults := not settings1.Open (True);
      end;
      LoadOptions (settings1, section.Options, forceDefaults);
      LoadSections (settings1, section.Sections, forceDefaults)
    finally
      settings1.Free
    end;
  end
end;

procedure TCustomPersistentOptions.Save;
var
  settings: TExSettings;
begin
  if not Persist then Exit;

  settings := GetSettingsClass.Create(Manufacturer, Application, Version, FOptionsType);
  try
    SettingsCreated (settings);
    SaveOptions (settings, Options);
    SaveSections (settings, Sections)
  finally
    settings.Free
  end
end;

procedure TCustomPersistentOptions.SaveOptions(settings: TExSettings; Options: TOptions);
var
  i, idx: Integer;
  deleteValues: TStringList;
  option: TOption;

begin
  deleteValues := TStringList.Create;
  try
  // Get a list of values to delete.  Start by filling it with all the values
  // then remove values as we save them, leaving only the obsolete ones.

    deleteValues.CaseSensitive := False;
    settings.GetValueNames(deleteValues);
    deleteValues.Sort;

    for i := 0 to Options.Count - 1 do
    begin
      Option := Options [i];

      idx := deleteValues.IndexOf(Option.Name);
      if idx >= 0 then
        deleteValues.Delete(idx);

      case Option.OptionType of
        otString: settings.SetStringValue (Option.Name, Option.FStrVal, Option.DefaultValue);
        otInteger: settings.SetIntegerValue (Option.Name,  Option.FIntVal, StrToIntDef (Option.DefaultValue, 0));
        otBoolean: settings.SetBooleanValue (Option.Name, Option.FBoolVal, StrToBoolDef (Option.DefaultValue, false));
        otEnum: settings.SetIntegerValue (Option.Name, Option.FIntVal, Option.EnumValues.IndexOf(Option.DefaultValue));

      // The logic behind the 'enum' stuff works so that if the default string value is
      // not a member of the enum, the actual enum value gets saved.

      end;
      Option.FDirty := False
    end;

    if Options.DeleteObsoleteOptions then          // Delete obsolete values.
      for i := 0 to deleteValues.count - 1 do
        settings.DeleteValue(deleteValues [i])
  finally
    deleteValues.Free
  end
end;

procedure TCustomPersistentOptions.SaveSections(settings: TExSettings; Sections: TSections);
var
  i, idx: Integer;
  section: TSection;
  settings1: TExSettings;
  deleteSections: TStringList;
begin
  deleteSections := TStringList.Create;
  try
  // Build list of obsolete sections to delete.

    deleteSections.CaseSensitive := False;
    settings.GetSectionNames(deleteSections);
    deleteSections.Sort;

  // Save the sections & options
    for i := 0 to Sections.Count - 1 do
    begin
      section := Sections [i];

      settings1 := GetSettingsClass.CreateChild(settings, section.Name);
      try
        idx := deleteSections.IndexOf(Section.Name);
        if idx >= 0 then
          deleteSections.Delete(idx);

        SaveOptions (settings1, section.Options);
        SaveSections (settings1, section.Sections)
      finally
        settings1.Free
      end;
    end;

    // Delete the obsolete sections
    if Sections.DeleteObsoleteSections then
      for i := 0 to deleteSections.Count - 1 do
//        settings.DeleteSection (deleteSections [i])
  finally
    deleteSections.Free;
  end
end;

procedure TCustomPersistentOptions.SettingsCreated(settings: TExSettings);
begin
  if settings is TExFileSettings then
    TExFileSettings (settings).CustomPath := GetSettingsFile;
end;

{ TUniPersistentOptions }

function TUniPersistentOptions.GetSettingsClass: TExSettingsClass;
begin
  if Assigned(FOnGetSettingsClass) then
    OnGetSettingsClass (Self, Result)
  else
    raise EOptionError.Create ('Unable to get settings class');
end;

function TUniPersistentOptions.GetSettingsFile: string;
begin
  if Assigned(FOnGetSettingsFile) then
    OnGetSettingsFile (Self, Result)
end;

end.
