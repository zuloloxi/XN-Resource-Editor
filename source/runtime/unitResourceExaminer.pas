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

uses Windows, Classes, SysUtils, unitResourceDetails, contnrs, Graphics, unitPEFile;

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
  fOwner : TObject;
  fElements : TObjectList;
  function GetCount : Integer;
  function GetDisplayName: UnicodeString; virtual; abstract;
public
  constructor Create; overload;
  destructor Destroy; override;
  property Count : Integer read GetCount;
  property Element [idx : Integer] : TResExamElement read GetElement;
  property DisplayName : UnicodeString read GetDisplayName;
end;

TUndoName = class
private
  fName: UnicodeString;
  fDesc : string;
public
  constructor Create (const ADesc : string; const AName : UnicodeString);
  property Name : UnicodeString read fName;
  property Description : string read fDesc;
end;

TResExamNamedElement = class (TResExamElement)
private
  fName : UnicodeString;
  function GetCanRedo: boolean;
  function GetCanUndo: boolean;
  function GetRedoDescription: string;
  function GetUndoDescription: string;

protected
  fUndoNames : TObjectList;
  fRedoNames : TObjectList;
  fUndoing, fRedoing : boolean;

  constructor Create (AOwner : TObject; const AName : UnicodeString);
  procedure SetName (const value : UnicodeString); virtual;
  procedure AddNameToUndoList (const Desc : string; const nm : UnicodeString = '');
public
  destructor Destroy; override;
  procedure Undo;
  procedure Redo;
  property Name : UnicodeString read fName write SetName;
  property CanRedo : boolean read GetCanRedo;
  property CanUndo : boolean read GetCanUndo;
  property UndoDescription : string read GetUndoDescription;
  property RedoDescription : string read GetRedoDescription;
end;

//---------------------------------------------------------------------
// TResExamResource - Base class for resource examiner elements that
// reference resource details.
TResExamResource = class (TResExamElement)
private
  fResource : TResourceDetails;
public
  constructor Create (AOwner : TObject; AResource : TResourceDetails);
  property ResourceDetails : TResourceDetails read fResource;
end;

TResExamImport = class (TResExamElement)
private
  fBaseAddr : PByte;
  fImageImportDescriptor : PImageImportDescriptor;
  function GetImportName: AnsiString;
  function GetExaminer: TResourceExaminer;
protected
  function GetDisplayName : UnicodeString; override;
public
  constructor Create (AOwner : TObject; ABaseAddr : PByte; AImageImportDescriptor: PImageImportDescriptor);
  property ImportDirectory : PImageImportDescriptor read fImageImportDescriptor;
  property ImportName : AnsiString read GetImportName;
  property Examiner : TResourceExaminer read GetExaminer;

  property BaseAddress : PByte read fBaseAddr;
end;

//---------------------------------------------------------------------
// TResExamIconCursor - Resource examiner element for icons & cursors
TResExamIconCursor = class (TResExamResource)
private
  function GetOwner: TResExamLang;
protected
  function GetDisplayName: UnicodeString; override;
public
  property Owner : TResExamLang read GetOwner;
end;

//---------------------------------------------------------------------
// TResExamLang - Resource examiner element language
TResExamLang = class (TResExamResource)
private
  function GetOwner: TResExamName;
  function GetName: LCID;
protected
  function GetDisplayName : UnicodeString; override;
public
  property Name : LCID read GetName;
  property Owner : TResExamName read GetOwner;
end;

//---------------------------------------------------------------------
// TResExamName - Resource examiner element name
TResExamName = class (TResExamNamedElement)
private
  function GetResExamLang(idx: Integer): TResExamLang;
  function GetOwner: TResExamType;
protected
  procedure SetName(const Value: UnicodeString); override;
  function GetDisplayName : UnicodeString; override;
public
  constructor Create (AOwner : TResExamType; const AName : UnicodeString);

  property Owner : TResExamType read GetOwner;
  property ResExamLang [idx : Integer] : TResExamLang read GetResExamLang;
end;

//---------------------------------------------------------------------
// TResExamName - Resource examiner element type
TResExamType = class (TResExamNamedElement)
private
  function GetResExamName(idx: Integer): TResExamName;
  function GetOwner: TResourceExaminer;
protected
  procedure SetName(const Value: UnicodeString); override;
  function GetDisplayName : UnicodeString; override;
public
  constructor Create (AOwner : TResourceExaminer; const AName : UnicodeString);
  property Owner : TResourceExaminer read GetOwner;
  property ResExamName [idx : Integer] : TResExamName read GetResExamName;
end;

TResExamSection = class (TResExamElement)
private
  fName: UnicodeString;
protected
  function GetDisplayName: UnicodeString; override;
public
  constructor Create (const AName : UnicodeString);
  property Name : UnicodeString read fName write fName;
end;

TResExamSectionSection = class (TResExamSection)
private
  fSection : TImageSection;
public
  constructor Create (const AName : UnicodeString; ASection : TImageSection);
  property Section : TImageSection read fSection;
end;

TExportResExamSection = class (TResExamSection)
private
  fBaseAddr : PByte;
  function GetExaminer: TResourceExaminer;
  function GetExportCount: Integer;
  function GetExportName(idx: Integer): AnsiString;
  function GetExportOrdinal(idx: Integer): Integer;
public
  constructor Create (AOwner : TObject; BaseAddr : PByte);

  property ExportCount : Integer read GetExportCount;
  property ExportName [idx : Integer] : AnsiString read GetExportName;
  property ExportOrdinal [idx : Integer] : Integer read GetExportOrdinal;
end;

//---------------------------------------------------------------------
// TResourceExaminer class
TResourceExaminer = class
private
  fResourceModule : TResourceModule;
  fSections : TObjectList;

  fOwnsModule : boolean;
  function GetResource(idx: Integer): TResourceDetails;
  function GetResourceCount: Integer;
  function GetSection(idx: Integer): TResExamSection;
  function GetSectionCount: Integer;
  function GetExportCount: Integer;
  function GetImportCount: Integer;
  function GetImport (idx : Integer) : PImageImportDescriptor;
  function GetExportName(idx: Integer): AnsiString;
  function GetExportOrdinal(idx: Integer): Integer;
  function GetResourceSection: TResExamSection;
protected
  constructor Create (AResourceModule : TResourceModule; AOwnsModule : boolean; DontExamine : boolean); overload;

  property ResourceCount : Integer read GetResourceCount;
  property Resource [idx : Integer] : TResourceDetails read GetResource;
  property ResourceModule : TResourceModule read fResourceModule;

  property ExportCount : Integer read GetExportCount;
  property ExportName [idx : Integer] : AnsiString read GetExportName;
  property ExportOrdinal [idx : Integer] : Integer read GetExportOrdinal;

  property ImportCount : Integer read GetImportCount;
  property Import [idx : Integer] : PImageImportDescriptor read GetImport;
public
  constructor Create (AResourceModule : TResourceModule); overload;
  constructor Create (const FileName : string); overload;

  destructor Destroy; override;

  procedure Examine;
  procedure SetResourceModule (rm : TResourceModule; ownsModule : boolean);

  property ResourceSection : TResExamSection read GetResourceSection;
  property SectionCount : Integer read GetSectionCount;
  property Section [idx : Integer] : TResExamSection read GetSection;
end;

function PixelFormatToString (pf : TPixelFormat) : string;
function GetTypeName (const tp : string) : string;

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
 | function PixelFormatToString () : string                             |
 |                                                                      |
 | Return string representation of a TPixelFormat                       |
 |                                                                      |
 | Parameters:                                                          |
 |   pf : TPixelFormat          The pixel format to use                 |
 *----------------------------------------------------------------------*)
function PixelFormatToString (pf : TPixelFormat) : string;
begin
  case pf of
    pf1Bit : result := rst1Bit;
    pf4Bit : result := rst4Bit;
    pf8Bit : result := rst8Bit;
    pf15Bit : result := rst15Bit;
    pf16Bit : result := rst16Bit;
    pf24Bit : result := rst24Bit;
    pf32Bit : result := rst32Bit
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeName () : string                                     |
 |                                                                      |
 | Return display name for a resource type                              |
 |                                                                      |
 | Parameters:                                                          |
 |   const tp : string          The resource type                       |
 *----------------------------------------------------------------------*)
function GetTypeName (const tp : string) : string;
var
  i : Integer;
begin
  i := ResourceNameToInt (tp);

  case i of
    Integer (RT_BITMAP)       : result := rstBitmap;
    Integer (RT_ICON)         : result := rstIcon;
    Integer (RT_CURSOR)       : result := rstCursor;
    Integer (RT_MENU)         : result := rstMenu;
    Integer (RT_DIALOG)       : result := rstDialog;
    Integer (RT_STRING)       : result := rstString;
    Integer (RT_ACCELERATOR)  : Result := rstAccelerator;
    Integer (RT_RCDATA)       : result := rstRCData;
    Integer (RT_MESSAGETABLE) : result := rstMessageTable;
    Integer (RT_VERSION)      : result := rstVersion;
    Integer (RT_GROUP_CURSOR) : result := rstGroupCursor;
    Integer (RT_GROUP_ICON)   : result := rstGroupIcon;
    Integer (RT_XPMANIFEST)   : result := rstXPManifest;
    Integer (RT_HTML)         : result := rstHTML;
    Integer (RT_TOOLBAR)      : result := rstToolbar;
    else
      result := tp
  end
end;

(*----------------------------------------------------------------------*
 | function GetLangName () : string                                     |
 |                                                                      |
 | Return display name for a language ID                                |
 |                                                                      |
 | Parameters:                                                          |
 |   language : Integer                 The language ID                 |
 *----------------------------------------------------------------------*)
function GetLangName (language : Integer) : string;
begin
  if language = 0 then
    result := rstLanguageNeutral
  else
    result := Languages.NameFromLocaleID [language]
end;


function FixResourceName (const st : WideString) : WideString;
var
  i : Integer;
begin
  result := WideUpperCase (st);

  for i := 1 to Length (result) do
    if result [i] = ' ' then
      result [i] := '_';
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
 |   AOwnsModule : boolean              If this is true we take         |
 |                                      ownership of the module, and    |
 |                                      delete it when we're deleted.   |
 |                                                                      |
 |  DontExamine : boolean               If this is true, dont examine   |
 |                                      the module in the constructor.  |
 *----------------------------------------------------------------------*)
constructor TResourceExaminer.Create(AResourceModule: TResourceModule; AOwnsModule : boolean; DontExamine : boolean);
begin
  fSections := TObjectList.Create;
  fResourceModule := AResourceModule;
  fOwnsModule := AOwnsModule;
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
  fResourceModule.LoadFromFile(FileName);
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
  fSections.Free;
  if fOwnsModule then
    fResourceModule.Free;
  inherited;
end;

procedure TResourceExaminer.Examine;
type
  TImageImportByName = record
    hint : WORD;
    name : array [0..0] of char;
  end;
  PImageImportByName = ^TImageImportByName;

var
  i : Integer;
  section : TResExamSection;
  pem : TPEBase;
  imageSection : TImageSection;
  offset : Integer;
  resSectionIDx : Integer;

  procedure GetResourcesInto (section : TResExamSection);
  var
    i, j : Integer;
    res : TResourceDetails;
    currentType : TResExamType;
    currentName : TResExamName;
    currentLang : TResExamLang;
    grp : TIconCursorGroupResourceDetails;

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
        section.fElements.Add(currentType)
      end;

      if (currentName = Nil) or (currentName.Name <> res.ResourceName) then
      begin
        currentName := TResExamName.Create(currentType, res.ResourceName);
        currentType.fElements.Add(currentName)
      end;

      currentLang := TResExamLang.Create(currentName, res);
      currentName.fElements.Add(currentLang);

      if res is TIconCursorGroupResourceDetails then
      begin
        grp := TIconCursorGroupResourceDetails (res);

        currentLang.fElements := TObjectList.Create;
        for j := 0 to grp.ResourceCount - 1 do
          currentLang.fElements.Add(TResExamIconCursor.Create(currentLang, grp.ResourceDetails [j]))
      end
    end
  end;


begin
  fSections.Clear;

  if fResourceModule is TPeBase then
  begin
    pem := TPEBase (fResourceModule);

    if pem is TPEModule then
      resSectionIdx := TPEModule (pem).FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, offset)
    else
      resSectionIdx := -1;

    for i := 0 to pem.ImageSectionCount - 1 do
    begin
      imageSection := pem.ImageSection [i];

      if i = resSectionIdx then
        section := TResExamSectionSection.Create (Format ('Resources (%s)', [imageSection.SectionName]), imageSection)
      else
        section := TResExamSectionSection.Create(String (imageSection.SectionName), imageSection);

      fSections.Add(section);

      if i = resSectionIdx then
        GetResourcesInto (section)
    end
  end
  else


(*
  if ExportCount > 0 then
  begin
    sectionData := TPEModule (fResourceModule).ExportSectionData;
    section := TExportResExamSection.Create (self, sectionData);
    fSections.Add(section);
  end;

  if ImportCount > 0 then
  begin
    sectionData := TPEModule (fResourceModule).ImportSectionData;
    section := TResExamSection.Create ('Imported Functions');
    fSections.Add(section);

    for i := 0 to ImportCount - 1 do
    begin
      imp := Import [i];
      impSection := TResExamImport.Create(self, sectionData, imp);
      section.fElements.Add(impSection);
    end
  end;
*)
  if ResourceCount > 0 then
  begin
    section := TResExamSection.Create ('Resources');
    fSections.Add(section);

    GetResourcesInto (section);

  end
end;

function TResourceExaminer.GetExportCount: Integer;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).ExportCount
  else
    result := 0
end;

function TResourceExaminer.GetExportName(idx: Integer): AnsiString;
var
  ord : DWORD;
begin
  if fResourceModule is TPEModule then
    TPEModule (fResourceModule).GetExportDetails(idx, result, ord)
  else
    result := ''
end;

function TResourceExaminer.GetExportOrdinal(idx: Integer): Integer;
var
  nm : AnsiString;
  ord : DWORD;
begin
  if fResourceModule is TPEModule then
  begin
    TPEModule (fResourceModule).GetExportDetails(idx, nm, ord);
    result := ord
  end
  else
    result := 0
end;

function TResourceExaminer.GetImport(idx: Integer): PImageImportDescriptor;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).Import [idx]
  else
    result := Nil
end;

function TResourceExaminer.GetImportCount: Integer;
begin
  if fResourceModule is TPEModule then
    result := TPEModule (fResourceModule).ImportCount
  else
    result := 0
end;

function TResourceExaminer.GetResource(idx: Integer): TResourceDetails;
begin
  result := fResourceModule.ResourceDetails [idx]
end;

function TResourceExaminer.GetResourceCount: Integer;
begin
  if Assigned (fResourceModule) then
    result := fResourceModule.ResourceCount
  else
    result := 0
end;

{ TResExamLang }

function TResExamLang.GetDisplayName: UnicodeString;
begin
  result := GetLangName (Name);
end;

function TResExamLang.GetName: LCID;
begin
  result := fResource.ResourceLanguage;
end;

function TResExamLang.GetOwner: TResExamName;
begin
  result := fOwner as TResExamName;
end;

{ TResExamName }

constructor TResExamName.Create(AOwner: TResExamType; const AName: UnicodeString);
begin
  inherited Create (AOwner, AName);
end;

function TResExamName.GetDisplayName: UnicodeString;
begin
  result := Name;
end;

function TResExamName.GetOwner: TResExamType;
begin
  result := TResExamType (fOwner);
end;

function TResExamName.GetResExamLang(idx: Integer): TResExamLang;
begin
  result := TResExamLang (fElements [idx])
end;

procedure TResExamName.SetName(const Value: UnicodeString);
var
  wst : WideString;

  procedure ReplaceChildResourceNames (elem : TResExamElement);
  var
    i : Integer;
    res : TResourceDetails;
  begin
    if elem is TResExamResource then
    begin
      res := TResExamResource (elem).ResourceDetails;

      // nb.  Don't rename icon/cursor resources.  Rename the
      //      icon/cursor *group* resource instead.
      if not (res is TIconCursorResourceDetails) then
        res.ResourceName := wst;
    end;

    for i := 0 to elem.Count - 1 do
      ReplaceChildResourceNames (elem.Element [i])
  end;

begin
  if fName = Value then Exit;
  wst := FixResourceName (Value);
  AddNameToUndoList (rstChangeResourceName);
  fName := wst;
  ReplaceChildResourceNames (self);
end;

{ TResExamType }

constructor TResExamType.Create(AOwner: TResourceExaminer;
  const AName: UnicodeString);
begin
  inherited Create (AOwner, AName);
end;

function TResExamType.GetDisplayName: UnicodeString;
begin
  result := GetTypeName (Name);
end;

function TResExamType.GetOwner: TResourceExaminer;
begin
  result := TResourceExaminer (fOwner);
end;

function TResExamType.GetResExamName(idx: Integer): TResExamName;
begin
  result := TResExamName (fElements [idx]);
end;

procedure TResExamType.SetName(const Value: UnicodeString);
var
  wst : WideString;

  procedure ReplaceChildResourceTypes (elem : TResExamElement);
  var
    i : Integer;
  begin
    if elem is TResExamResource then
      TResExamResource (elem).ResourceDetails.ResourceType := wst;

    for i := 0 to elem.Count - 1 do
      ReplaceChildResourceTypes (elem.Element [i])
  end;

begin
  if fName = Value then Exit;
  wst := FixResourceName (Value);
  AddNameToUndoList (rstChangeCustomResourceType);
  fName := wst;
  ReplaceChildResourceTypes (self);
end;

{ TResExamElement }

constructor TResExamElement.Create;
begin
  fElements := TObjectList.Create;
end;

destructor TResExamElement.Destroy;
begin
  fElements.Free;
  inherited;
end;

function TResExamElement.GetCount: Integer;
begin
  if Assigned (fElements) then result := fElements.Count else result := 0
end;

function TResExamElement.GetElement(idx: Integer): TResExamElement;
begin
  if Assigned (fElements) then result := TResExamElement (fElements [idx]) else result := Nil
end;

{ TResExamResource }

constructor TResExamResource.Create(AOwner: TObject; AResource: TResourceDetails);
begin
  fOwner := AOwner;
  fResource := AResource;
  if fResource is TIconCursorGroupResourceDetails then
    fElements := TObjectList.Create
end;

{ TResExamIconCursor }

function TResExamIconCursor.GetDisplayName: UnicodeString;
var
  res : TIconCursorResourceDetails;
  pf : string;
begin
  res := TIconCursorResourceDetails (fResource);

  try
    pf := PixelFormatToString (res.PixelFormat);
  except
    pf := 'Unknown format';

  end;
  result := Format ('%dx%d %s', [res.Width, res.Height, pf]);
end;

function TResExamIconCursor.GetOwner: TResExamLang;
begin
  result := fOwner as TResExamLang
end;

function TResourceExaminer.GetResourceSection: TResExamSection;
var
  i : Integer;
begin
  i := 0;
  result := Nil;
  while i < SectionCount do
    if Copy (Section [i].Name, 1, 9) = 'Resources' then
    begin
      result := Section [i];
      break
    end
    else
      Inc (i)
end;

function TResourceExaminer.GetSection(idx: Integer): TResExamSection;
begin
  result := TResExamSection (fSections [idx])
end;

function TResourceExaminer.GetSectionCount: Integer;
begin
  result := fSections.Count
end;

{ TResExamSection }

constructor TResExamSection.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
end;

function TResExamSection.GetDisplayName: UnicodeString;
begin
  result := Name
end;

{ TResExamImport }

constructor TResExamImport.Create(AOwner: TObject;ABaseAddr : PByte;
  AImageImportDescriptor: PImageImportDescriptor);
begin
  inherited Create;
  fOwner := AOwner;
  fBaseAddr := ABaseAddr;
  fImageImportDescriptor := AImageImportDescriptor
end;

function TResExamImport.GetDisplayName: UnicodeString;
begin
  result := UnicodeString (ImportName)
end;

function TResExamImport.GetExaminer: TResourceExaminer;
begin
  result := TResourceExaminer (fOwner);
end;

function TResExamImport.GetImportName: AnsiString;
begin
  result := PAnsiChar (fBaseAddr) + fImageImportDescriptor^.Name
end;

{ TExportResExamSection }

constructor TExportResExamSection.Create(AOwner: TObject; BaseAddr: PByte);
begin
  inherited Create ('Exported Functions');
  fOwner := AOwner;
  fBaseAddr := BaseAddr;
end;

function TExportResExamSection.GetExaminer: TResourceExaminer;
begin
  result := fOwner as TResourceExaminer;
end;

function TExportResExamSection.GetExportCount: Integer;
begin
  result := GetExaminer.ExportCount
end;

function TExportResExamSection.GetExportName(idx: Integer): AnsiString;
begin
  result := GetExaminer.ExportName [idx];
end;

function TExportResExamSection.GetExportOrdinal(idx: Integer): Integer;
begin
  result := GetExaminer.ExportOrdinal [idx];
end;

procedure TResourceExaminer.SetResourceModule(rm: TResourceModule; ownsModule : boolean);
begin
  fSections.Clear;

  if fOwnsModule then
    FreeAndNil (fResourceModule);

  fOwnsModule := ownsModule;
  fResourceModule := rm;
  Examine
end;

{ TResExamNamedElement }

procedure TResExamNamedElement.AddNameToUndoList(const Desc : string; const nm: UnicodeString);
begin
  if fUndoing then Exit;
  if not Assigned (fUndoNames) then
    fUndoNames := TObjectList.Create;

  if nm = '' then
    fUndoNames.Insert (0, TUndoName.Create(Desc, Name))
  else
    fUndoNames.Insert (0, TUndoName.Create(Desc, nm));

  if not fRedoing then
    if Assigned (fRedoNames) then
      fRedoNames.Clear;
end;

constructor TResExamNamedElement.Create(AOwner: TObject;
  const AName: UnicodeString);
begin
  inherited Create;
  fOwner := AOwner;
  fName := AName;
end;

destructor TResExamNamedElement.Destroy;
begin
  fUndoNames.Free;
  fRedoNames.Free;

  inherited;
end;

function TResExamNamedElement.GetCanUndo: boolean;
begin
  result := Assigned (fUndoNames) and (fUndoNames.Count > 0);
end;

function TResExamNamedElement.GetCanRedo: boolean;
begin
  result := Assigned (fRedoNames) and (fRedoNames.Count > 0);
end;

procedure TResExamNamedElement.Redo;
var
  redoName : TUndoName;
begin
  if not CanRedo then Exit;

  redoName := TUndoName (fRedoNames.Extract(fRedoNames [0]));
  try
    fRedoing := True;
    try
      Name := redoName.Name;
    finally
      fRedoing := False
    end
  finally
    redoName.Free
  end
end;

procedure TResExamNamedElement.SetName(const value: UnicodeString);
begin
  raise Exception.Create('Can''t rename element');
end;

procedure TResExamNamedElement.Undo;
var
  oldName : TUndoName;
  ws : WideString;
begin
  if not CanUndo then Exit;

  oldName := TUndoName (fUndoNames.Extract(fUndoNames [0]));
  try
    fUndoing := True;
    try
      ws := Name;
      Name := oldName.Name
    finally
      fUndoing := False
    end;

    oldName.fName := ws;
    if not Assigned (fRedoNames) then
      fRedoNames := TObjectList.Create;
      
    fRedoNames.Insert (0, oldName);
    oldName := Nil
  finally
    oldName.Free
  end
end;

function TResExamNamedElement.GetUndoDescription: string;
begin
  if CanUndo then
    result := TUndoName (fUndoNames [0]).Description
  else
    result := '';
end;

function TResExamNamedElement.GetRedoDescription: string;
begin
  if CanRedo then
    result := TUndoName (fRedoNames [0]).Description
  else
    result := '';
end;

{ TUndoName }

constructor TUndoName.Create(const ADesc : string; const AName: UnicodeString);
begin
  fName := AName;
  fDesc := ADesc;
end;

{ TResExamSectionSection }

constructor TResExamSectionSection.Create(const AName: UnicodeString;
  ASection: TImageSection);
begin
  inherited Create (AName);
  fSection := ASection
end;

end.
