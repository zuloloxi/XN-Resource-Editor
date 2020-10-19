(*======================================================================*
 | unitResourceExaminer                                                 |
 |                                                                      |
 | unit contains TResourceExaminer helper class for enumerating         |
 | resource modules.                                                    |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      11/06/2003  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceExaminer;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, System.Types, unitResourceDetails,
  System.Contnrs, Vcl.Graphics, unitPEFile;

type
  TResourceExaminer = class;
  TResExamType = class;
  TResExamName = class;
  TResExamLang = class;
  
  //---------------------------------------------------------------------
  // TResExamElement - Base class for resource examiner elements
  TResExamElement = class
  private
    function GetElement(idx: Integer): TResExamElement;
  protected
    FOwner: TObject;
    FElements: TObjectList;
    function GetCount: Integer;
    function GetDisplayName: UnicodeString; virtual; abstract;
  public
    constructor Create; overload;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Element [idx: Integer]: TResExamElement read GetElement;
    property DisplayName: UnicodeString read GetDisplayName;
  end;
  
  TUndoName = class
  private
    FName: UnicodeString;
    FDesc: string;
  public
    constructor Create (const ADesc: string; const AName: UnicodeString);
    property Name: UnicodeString read FName;
    property Description: string read FDesc;
  end;
  
  TResExamNamedElement = class (TResExamElement)
  private
    FName: UnicodeString;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetRedoDescription: string;
    function GetUndoDescription: string;
  
  protected
    FUndoNames: TObjectList;
    FRedoNames: TObjectList;
    FUndoing, FRedoing: Boolean;
  
    constructor Create (AOwner: TObject; const AName: UnicodeString);
    procedure SetName (const value: UnicodeString); virtual;
    procedure AddNameToUndoList(const Desc: string; const nm: UnicodeString = '');
  public
    destructor Destroy; override;
    procedure Undo;
    procedure Redo;
    property Name: UnicodeString read FName write SetName;
    property CanRedo: Boolean read GetCanRedo;
    property CanUndo: Boolean read GetCanUndo;
    property UndoDescription: string read GetUndoDescription;
    property RedoDescription: string read GetRedoDescription;
  end;
  
  //---------------------------------------------------------------------
  // TResExamResource - Base class for resource examiner elements that
  // reference resource details.
  TResExamResource = class (TResExamElement)
  private
    FResource: TResourceDetails;
  public
    constructor Create (AOwner: TObject; AResource: TResourceDetails);
    property ResourceDetails: TResourceDetails read FResource;
  end;
  
  TResExamImport = class (TResExamElement)
  private
    FBaseAddr: PByte;
    FImageImportDescriptor: PImageImportDescriptor;
    function GetImportName: AnsiString;
    function GetExaminer: TResourceExaminer;
  protected
    function GetDisplayName: UnicodeString; override;
  public
    constructor Create (AOwner: TObject; ABaseAddr: PByte; AImageImportDescriptor: PImageImportDescriptor);
    property ImportDirectory: PImageImportDescriptor read FImageImportDescriptor;
    property ImportName: AnsiString read GetImportName;
    property Examiner: TResourceExaminer read GetExaminer;
  
    property BaseAddress: PByte read FBaseAddr;
  end;
  
  //---------------------------------------------------------------------
  // TResExamIconCursor - Resource examiner element for icons & cursors
  TResExamIconCursor = class (TResExamResource)
  private
    function GetOwner: TResExamLang;
  protected
    function GetDisplayName: UnicodeString; override;
  public
    property Owner: TResExamLang read GetOwner;
  end;
  
  //---------------------------------------------------------------------
  // TResExamLang - Resource examiner element language
  TResExamLang = class (TResExamResource)
  private
    function GetOwner: TResExamName;
    function GetName: LCID;
  protected
    function GetDisplayName: UnicodeString; override;
  public
    property Name: LCID read GetName;
    property Owner: TResExamName read GetOwner;
  end;
  
  //---------------------------------------------------------------------
  // TResExamName - Resource examiner element name
  TResExamName = class (TResExamNamedElement)
  private
    function GetResExamLang(idx: Integer): TResExamLang;
    function GetOwner: TResExamType;
  protected
    procedure SetName(const Value: UnicodeString); override;
    function GetDisplayName: UnicodeString; override;
  public
    constructor Create (AOwner: TResExamType; const AName: UnicodeString);
  
    property Owner: TResExamType read GetOwner;
    property ResExamLang [idx: Integer]: TResExamLang read GetResExamLang;
  end;
  
  //---------------------------------------------------------------------
  // TResExamName - Resource examiner element type
  TResExamType = class (TResExamNamedElement)
  private
    function GetResExamName(idx: Integer): TResExamName;
    function GetOwner: TResourceExaminer;
  protected
    procedure SetName(const Value: UnicodeString); override;
    function GetDisplayName: UnicodeString; override;
  public
    constructor Create (AOwner: TResourceExaminer; const AName: UnicodeString);
    property Owner: TResourceExaminer read GetOwner;
    property ResExamName [idx: Integer]: TResExamName read GetResExamName;
  end;
  
  TResExamSection = class (TResExamElement)
  private
    FName: UnicodeString;
  protected
    function GetDisplayName: UnicodeString; override;
  public
    constructor Create (const AName: UnicodeString);
    property Name: UnicodeString read FName write FName;
  end;
  
  TResExamSectionSection = class (TResExamSection)
  private
    FSection: TImageSection;
  public
    constructor Create (const AName: UnicodeString; ASection: TImageSection);
    property Section: TImageSection read FSection;
  end;
  
  TExportResExamSection = class (TResExamSection)
  private
    FBaseAddr: PByte;
    function GetExaminer: TResourceExaminer;
    function GetExportCount: Integer;
    function GetExportName(idx: Integer): AnsiString;
    function GetExportOrdinal(idx: Integer): Integer;
  public
    constructor Create (AOwner: TObject; BaseAddr: PByte);
  
    property ExportCount: Integer read GetExportCount;
    property ExportName [idx: Integer]: AnsiString read GetExportName;
    property ExportOrdinal [idx: Integer]: Integer read GetExportOrdinal;
  end;
  
  //---------------------------------------------------------------------
  // TResourceExaminer class
  TResourceExaminer = class
  private
    FResourceModule: TResourceModule;
    FSections: TObjectList;
  
    FOwnsModule: Boolean;
    function GetResource(idx: Integer): TResourceDetails;
    function GetResourceCount: Integer;
    function GetSection(idx: Integer): TResExamSection;
    function GetSectionCount: Integer;
    function GetExportCount: Integer;
    function GetImportCount: Integer;
    function GetImport(idx: Integer): PImageImportDescriptor;
    function GetExportName(idx: Integer): AnsiString;
    function GetExportOrdinal(idx: Integer): Integer;
    function GetResourceSection: TResExamSection;
  protected
    constructor Create (AResourceModule: TResourceModule; AOwnsModule: Boolean; DontExamine: Boolean); overload;
  
    property ResourceCount: Integer read GetResourceCount;
    property Resource [idx: Integer]: TResourceDetails read GetResource;
    property ResourceModule: TResourceModule read FResourceModule;
  
    property ExportCount: Integer read GetExportCount;
    property ExportName [idx: Integer]: AnsiString read GetExportName;
    property ExportOrdinal [idx: Integer]: Integer read GetExportOrdinal;
  
    property ImportCount: Integer read GetImportCount;
    property Import [idx: Integer]: PImageImportDescriptor read GetImport;
  public
    constructor Create (AResourceModule: TResourceModule); overload;
    constructor Create (const FileName: string); overload;
  
    destructor Destroy; override;
  
    procedure Examine;
    procedure SetResourceModule (rm: TResourceModule; ownsModule: Boolean);
  
    property ResourceSection: TResExamSection read GetResourceSection;
    property SectionCount: Integer read GetSectionCount;
    property Section [idx: Integer]: TResExamSection read GetSection;
  end;

function PixelFormatToString (pf: TPixelFormat): string;
function GetTypeName (const tp: string): string;

implementation

uses unitResourceGraphics, unitResourceToolbar;

const
  RT_HTML = MakeIntResource(23);
  RT_XPMANIFEST = MakeIntResource (24);

resourcestring
  rstLanguageNeutral = 'Language Neutral';
  rstBitmap       = 'Bitmap';
  rstIcon         = 'Icon';
  rstCursor       = 'Cursor';
  rstMenu         = 'Menu';
  rstDialog       = 'Dialog';
  rstAccelerator  = 'Accelerator';
  rstString       = 'String Table';
  rstRCData       = 'RC Data';
  rstMessageTable = 'MessageTable';
  rstVersion      = 'Version';
  rstGroupCursor  = 'Cursor Group';
  rstGroupIcon    = 'Icon Group';
  rstHTML         = 'HTML';
  rstXPManifest   = 'XP Theme Manifest';
  rstToolbar      = 'Toolbar';

  rst1Bit         = '2 Colour';
  rst4Bit         = '16 Colour';
  rst8Bit         = '256 Colour';
  rst15Bit        = '15 Bit Colour';
  rst16Bit        = '16 Bit Colour';
  rst24Bit        = '24 Bit Colour';
  rst32Bit        = '32 Bit Colour';

  rstChangeCustomResourceType = 'change custom resource type';
  rstChangeResourceName       = 'change resource name';


(*----------------------------------------------------------------------*
 | function PixelFormatToString (): string                             |
 |                                                                      |
 | Return string representation of a TPixelFormat                       |
 |                                                                      |
 | Parameters:                                                          |
 |   pf: TPixelFormat          The pixel format to use                 |
 *----------------------------------------------------------------------*)
function PixelFormatToString (pf: TPixelFormat): string;
begin
  case pf of
    pf1Bit: Result := rst1Bit;
    pf4Bit: Result := rst4Bit;
    pf8Bit: Result := rst8Bit;
    pf15Bit: Result := rst15Bit;
    pf16Bit: Result := rst16Bit;
    pf24Bit: Result := rst24Bit;
    pf32Bit: Result := rst32Bit
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeName (): string                                     |
 |                                                                      |
 | Return display name for a resource type                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const tp: string          The resource type                       |
 *----------------------------------------------------------------------*)
function GetTypeName (const tp: string): string;
var
  i: Integer;
begin
  i := ResourceNameToInt(tp);

  case i of
    Integer (RT_BITMAP)   : Result := rstBitmap;
    Integer (RT_ICON)     : Result := rstIcon;
    Integer (RT_CURSOR)   : Result := rstCursor;
    Integer (RT_MENU)     : Result := rstMenu;
    Integer (RT_DIALOG)   : Result := rstDialog;
    Integer (RT_STRING)   : Result := rstString;
    Integer (RT_ACCELERATOR): Result := rstAccelerator;
    Integer (RT_RCDATA)   : Result := rstRCData;
    Integer (RT_MESSAGETABLE): Result := rstMessageTable;
    Integer (RT_VERSION)  : Result := rstVersion;
    Integer (RT_GROUP_CURSOR): Result := rstGroupCursor;
    Integer (RT_GROUP_ICON): Result := rstGroupIcon;
    Integer (RT_XPMANIFEST): Result := rstXPManifest;
    Integer (RT_HTML)     : Result := rstHTML;
    Integer (RT_TOOLBAR)  : Result := rstToolbar;
    else
      Result := tp
  end
end;

(*----------------------------------------------------------------------*
 | function GetLangName (): string                                     |
 |                                                                      |
 | Return display name for a language ID                                |
 |                                                                      |
 | Parameters:                                                          |
 |   language: Integer                 The language ID                 |
 *----------------------------------------------------------------------*)
function GetLangName (language: Integer): string;
begin
  if language = 0 then
    Result := rstLanguageNeutral
  else
    Result := Languages.NameFromLocaleID [language]
end;


function FixResourceName (const st: WideString): WideString;
var
  i: Integer;
begin
  Result := WideUpperCase (st);

  for i := 1 to Length (Result) do
    if Result [i] = ' ' then
      Result [i] := '_';
end;

{ TResourceExaminer }

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | protected base constructor                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   AResourceModule: TResourceModule   The module to examine           |
 |                                                                      |
 |   AOwnsModule: Boolean              If this is true we take         |
 |                                      ownership of the module, and    |
 |                                      delete it when we're deleted.   |
 |                                                                      |
 |  DontExamine: Boolean               If this is true, dont examine   |
 |                                      the module in the constructor.  |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(AResourceModule: TResourceModule; AOwnsModule: Boolean; DontExamine: Boolean);
begin
  FSections := TObjectList.Create;
  FResourceModule := AResourceModule;
  FOwnsModule := AOwnsModule;
  if not DontExamine then
    Examine;
end;

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | public constructor #1.  Initialize a TResourceExaminer from          |
 | resources in a PE file.                                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const FileName: string     The name of the PE file                 |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(const FileName: string);
begin
  Create (TPEResourceModule.Create, True, True);
  FResourceModule.LoadFromFile(FileName);
  Examine;
end;

(*----------------------------------------------------------------------*
 | constructor TResourceExaminer.Create                                 |
 |                                                                      |
 | public constructor #2.  Initialize a TResourceExaminer from a        |
 | TResourceModule                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |   const AResourceModule      The resource module to examinme         |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(AResourceModule: TResourceModule);
begin
  Create (AResourceModule, False, False);
end;

destructor TResourceExaminer.Destroy;
begin
  FSections.Free;
  if FOwnsModule then
    FResourceModule.Free;
  inherited;
end;

procedure TResourceExaminer.Examine;
type
  TImageImportByName = record
    hint: WORD;
    name: array [0..0] of char;
  end;
  PImageImportByName = ^TImageImportByName;

var
  i: Integer;
  section: TResExamSection;
  pem: TPEBase;
  imageSection: TImageSection;
  offset: Integer;
  resSectionIDx: Integer;

  procedure GetResourcesInto (section: TResExamSection);
  var
    i, j: Integer;
    res: TResourceDetails;
    currentType: TResExamType;
    currentName: TResExamName;
    currentLang: TResExamLang;
    grp: TIconCursorGroupResourceDetails;

  begin
    currentType := Nil;
    currentName := Nil;

    for i := 0 to ResourceCount - 1 do
    begin
      res := Resource [i];
      if (currentType = Nil) or (currentType.Name <> res.ResourceType) then
      begin
        currentName := Nil;
        if res is TIconCursorResourceDetails then
          Continue;
        currentType := TResExamType.Create(self, res.ResourceType);
        section.FElements.Add(currentType)
      end;

      if (currentName = Nil) or (currentName.Name <> res.ResourceName) then
      begin
        currentName := TResExamName.Create(currentType, res.ResourceName);
        currentType.FElements.Add(currentName)
      end;

      currentLang := TResExamLang.Create(currentName, res);
      currentName.FElements.Add(currentLang);

      if res is TIconCursorGroupResourceDetails then
      begin
        grp := TIconCursorGroupResourceDetails (res);

        currentLang.FElements := TObjectList.Create;
        for j := 0 to grp.ResourceCount - 1 do
          currentLang.FElements.Add(TResExamIconCursor.Create(currentLang, grp.ResourceDetails [j]))
      end
    end
  end;


begin
  FSections.Clear;

  if FResourceModule is TPeBase then
  begin
    pem := TPEBase (FResourceModule);

    if pem is TPEModule then
      resSectionIdx := TPEModule (pem).FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, offset)
    else
      resSectionIdx := -1;

    for i := 0 to pem.ImageSectionCount - 1 do
    begin
      imageSection := pem.ImageSection [i];

      if i = resSectionIdx then
        section := TResExamSectionSection.Create (Format('Resources (%s)', [imageSection.SectionName]), imageSection)
      else
        section := TResExamSectionSection.Create(String (imageSection.SectionName), imageSection);

      FSections.Add(section);

      if i = resSectionIdx then
        GetResourcesInto (section)
    end
  end
  else


(*
  if ExportCount > 0 then
  begin
    sectionData := TPEModule (FResourceModule).ExportSectionData;
    section := TExportResExamSection.Create (self, sectionData);
    FSections.Add(section);
  end;

  if ImportCount > 0 then
  begin
    sectionData := TPEModule (FResourceModule).ImportSectionData;
    section := TResExamSection.Create ('Imported Functions');
    FSections.Add(section);

    for i := 0 to ImportCount - 1 do
    begin
      imp := Import [i];
      impSection := TResExamImport.Create(self, sectionData, imp);
      section.FElements.Add(impSection);
    end
  end;
*)
  if ResourceCount > 0 then
  begin
    section := TResExamSection.Create ('Resources');
    FSections.Add(section);

    GetResourcesInto (section);

  end
end;

function TResourceExaminer.GetExportCount: Integer;
begin
  if FResourceModule is TPEModule then
    Result := TPEModule (FResourceModule).ExportCount
  else
    Result := 0
end;

function TResourceExaminer.GetExportName(idx: Integer): AnsiString;
var
  ord: DWORD;
begin
  if FResourceModule is TPEModule then
    TPEModule (FResourceModule).GetExportDetails(idx, Result, ord)
  else
    Result := ''
end;

function TResourceExaminer.GetExportOrdinal(idx: Integer): Integer;
var
  nm: AnsiString;
  ord: DWORD;
begin
  if FResourceModule is TPEModule then
  begin
    TPEModule (FResourceModule).GetExportDetails(idx, nm, ord);
    Result := ord
  end
  else
    Result := 0
end;

function TResourceExaminer.GetImport(idx: Integer): PImageImportDescriptor;
begin
  if FResourceModule is TPEModule then
    Result := TPEModule (FResourceModule).Import [idx]
  else
    Result := Nil
end;

function TResourceExaminer.GetImportCount: Integer;
begin
  if FResourceModule is TPEModule then
    Result := TPEModule (FResourceModule).ImportCount
  else
    Result := 0
end;

function TResourceExaminer.GetResource(idx: Integer): TResourceDetails;
begin
  Result := FResourceModule.ResourceDetails [idx]
end;

function TResourceExaminer.GetResourceCount: Integer;
begin
  if Assigned(FResourceModule) then
    Result := FResourceModule.ResourceCount
  else
    Result := 0
end;

{ TResExamLang }

function TResExamLang.GetDisplayName: UnicodeString;
begin
  Result := GetLangName (Name);
end;

function TResExamLang.GetName: LCID;
begin
  Result := FResource.ResourceLanguage;
end;

function TResExamLang.GetOwner: TResExamName;
begin
  Result := FOwner as TResExamName;
end;

{ TResExamName }

constructor TResExamName.Create(AOwner: TResExamType; const AName: UnicodeString);
begin
  inherited Create (AOwner, AName);
end;

function TResExamName.GetDisplayName: UnicodeString;
begin
  Result := Name;
end;

function TResExamName.GetOwner: TResExamType;
begin
  Result := TResExamType (FOwner);
end;

function TResExamName.GetResExamLang(idx: Integer): TResExamLang;
begin
  Result := TResExamLang (FElements [idx])
end;

procedure TResExamName.SetName(const Value: UnicodeString);
var
  wst: WideString;

  procedure ReplaceChildResourceNames (elem: TResExamElement);
  var
    i: Integer;
    res: TResourceDetails;
  begin
    if elem is TResExamResource then
    begin
      res := TResExamResource (elem).ResourceDetails;

      // nb.  Don't rename icon/cursor resources.  Rename the
      //      icon/cursor *group* resource instead.
      if not(res is TIconCursorResourceDetails) then
        res.ResourceName := wst;
    end;

    for i := 0 to elem.Count - 1 do
      ReplaceChildResourceNames (elem.Element [i])
  end;

begin
  if FName = Value then Exit;
  wst := FixResourceName(Value);
  AddNameToUndoList(rstChangeResourceName);
  FName := wst;
  ReplaceChildResourceNames(Self);
end;

{ TResExamType }

constructor TResExamType.Create(AOwner: TResourceExaminer;
  const AName: UnicodeString);
begin
  inherited Create (AOwner, AName);
end;

function TResExamType.GetDisplayName: UnicodeString;
begin
  Result := GetTypeName (Name);
end;

function TResExamType.GetOwner: TResourceExaminer;
begin
  Result := TResourceExaminer (FOwner);
end;

function TResExamType.GetResExamName(idx: Integer): TResExamName;
begin
  Result := TResExamName (FElements [idx]);
end;

procedure TResExamType.SetName(const Value: UnicodeString);
var
  wst: WideString;

  procedure ReplaceChildResourceTypes (elem: TResExamElement);
  var
    i: Integer;
  begin
    if elem is TResExamResource then
      TResExamResource (elem).ResourceDetails.ResourceType := wst;

    for i := 0 to elem.Count - 1 do
      ReplaceChildResourceTypes (elem.Element [i])
  end;

begin
  if FName = Value then Exit;
  wst := FixResourceName (Value);
  AddNameToUndoList(rstChangeCustomResourceType);
  FName := wst;
  ReplaceChildResourceTypes (self);
end;

{ TResExamElement }

constructor TResExamElement.Create;
begin
  FElements := TObjectList.Create;
end;

destructor TResExamElement.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TResExamElement.GetCount: Integer;
begin
  if Assigned(FElements) then
    Result := FElements.Count
  else
    Result := 0
end;

function TResExamElement.GetElement(idx: Integer): TResExamElement;
begin
  if Assigned(FElements) then
    Result := TResExamElement(FElements [idx])
  else
    Result := nil
end;

{ TResExamResource }

constructor TResExamResource.Create(AOwner: TObject; AResource: TResourceDetails);
begin
  FOwner := AOwner;
  FResource := AResource;
  if FResource is TIconCursorGroupResourceDetails then
    FElements := TObjectList.Create;
end;

{ TResExamIconCursor }

function TResExamIconCursor.GetDisplayName: UnicodeString;
var
  res: TIconCursorResourceDetails;
  pf: string;
begin
  res := TIconCursorResourceDetails (FResource);

  try
    pf := PixelFormatToString (res.PixelFormat);
  except
    pf := 'Unknown format';
  end;
  Result := Format('%dx%d %s', [res.Width, res.Height, pf]);
end;

function TResExamIconCursor.GetOwner: TResExamLang;
begin
  Result := FOwner as TResExamLang;
end;

function TResourceExaminer.GetResourceSection: TResExamSection;
var
  i: Integer;
begin
  i := 0;
  Result := Nil;
  while i < SectionCount do
    if Copy (Section [i].Name, 1, 9) = 'Resources' then
    begin
      Result := Section [i];
      Break;
    end
    else
      Inc(i);
end;

function TResourceExaminer.GetSection(idx: Integer): TResExamSection;
begin
  Result := TResExamSection(FSections [idx]);
end;

function TResourceExaminer.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

{ TResExamSection }

constructor TResExamSection.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TResExamSection.GetDisplayName: UnicodeString;
begin
  Result := Name;
end;

{ TResExamImport }

constructor TResExamImport.Create(AOwner: TObject;ABaseAddr: PByte;
  AImageImportDescriptor: PImageImportDescriptor);
begin
  inherited Create;
  FOwner := AOwner;
  FBaseAddr := ABaseAddr;
  FImageImportDescriptor := AImageImportDescriptor;
end;

function TResExamImport.GetDisplayName: UnicodeString;
begin
  Result := UnicodeString(ImportName);
end;

function TResExamImport.GetExaminer: TResourceExaminer;
begin
  Result := TResourceExaminer(FOwner);
end;

function TResExamImport.GetImportName: AnsiString;
begin
  Result := PAnsiChar (FBaseAddr) + FImageImportDescriptor^.Name
end;

{ TExportResExamSection }

constructor TExportResExamSection.Create(AOwner: TObject; BaseAddr: PByte);
begin
  inherited Create ('Exported Functions');
  FOwner := AOwner;
  FBaseAddr := BaseAddr;
end;

function TExportResExamSection.GetExaminer: TResourceExaminer;
begin
  Result := FOwner as TResourceExaminer;
end;

function TExportResExamSection.GetExportCount: Integer;
begin
  Result := GetExaminer.ExportCount
end;

function TExportResExamSection.GetExportName(idx: Integer): AnsiString;
begin
  Result := GetExaminer.ExportName [idx];
end;

function TExportResExamSection.GetExportOrdinal(idx: Integer): Integer;
begin
  Result := GetExaminer.ExportOrdinal [idx];
end;

procedure TResourceExaminer.SetResourceModule(rm: TResourceModule; ownsModule: Boolean);
begin
  FSections.Clear;

  if FOwnsModule then
    FreeAndNil (FResourceModule);

  FOwnsModule := ownsModule;
  FResourceModule := rm;
  Examine;
end;

{ TResExamNamedElement }

procedure TResExamNamedElement.AddNameToUndoList(const Desc: string; const nm: UnicodeString);
begin
  if FUndoing then Exit;
  if not Assigned(FUndoNames) then
    FUndoNames := TObjectList.Create;

  if nm = '' then
    FUndoNames.Insert(0, TUndoName.Create(Desc, Name))
  else
    FUndoNames.Insert(0, TUndoName.Create(Desc, nm));

  if not FRedoing then
    if Assigned(FRedoNames) then
      FRedoNames.Clear;
end;

constructor TResExamNamedElement.Create(AOwner: TObject;
  const AName: UnicodeString);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
end;

destructor TResExamNamedElement.Destroy;
begin
  FUndoNames.Free;
  FRedoNames.Free;

  inherited;
end;

function TResExamNamedElement.GetCanUndo: Boolean;
begin
  Result := Assigned(FUndoNames) and (FUndoNames.Count > 0);
end;

function TResExamNamedElement.GetCanRedo: Boolean;
begin
  Result := Assigned(FRedoNames) and (FRedoNames.Count > 0);
end;

procedure TResExamNamedElement.Redo;
var
  RedoName: TUndoName;
begin
  if not CanRedo then Exit;

  RedoName := TUndoName (FRedoNames.Extract(FRedoNames [0]));
  try
    FRedoing := True;
    try
      Name := RedoName.Name;
    finally
      FRedoing := False
    end
  finally
    RedoName.Free
  end
end;

procedure TResExamNamedElement.SetName(const value: UnicodeString);
begin
  raise Exception.Create('Can''t rename element');
end;

procedure TResExamNamedElement.Undo;
var
  OldName: TUndoName;
  ws: WideString;
begin
  if not CanUndo then Exit;

  OldName := TUndoName (FUndoNames.Extract(FUndoNames [0]));
  try
    FUndoing := True;
    try
      ws := Name;
      Name := OldName.Name
    finally
      FUndoing := False
    end;

    OldName.FName := ws;
    if not Assigned(FRedoNames) then
      FRedoNames := TObjectList.Create;
      
    FRedoNames.Insert(0, OldName);
    OldName := Nil
  finally
    OldName.Free
  end
end;

function TResExamNamedElement.GetUndoDescription: string;
begin
  if CanUndo then
    Result := TUndoName (FUndoNames [0]).Description
  else
    Result := '';
end;

function TResExamNamedElement.GetRedoDescription: string;
begin
  if CanRedo then
    Result := TUndoName (FRedoNames [0]).Description
  else
    Result := '';
end;

{ TUndoName }

constructor TUndoName.Create(const ADesc: string; const AName: UnicodeString);
begin
  FName := AName;
  FDesc := ADesc;
end;

{ TResExamSectionSection }

constructor TResExamSectionSection.Create(const AName: UnicodeString;
  ASection: TImageSection);
begin
  inherited Create (AName);
  FSection := ASection
end;

end.
