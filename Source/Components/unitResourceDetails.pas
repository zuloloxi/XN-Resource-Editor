(*======================================================================*
 | unitResourceDetails                                                  |
 |                                                                      |
 | Ultra-light classes to wrap resources and resource modules.          |
 |                                                                      |
 | TResourceModule is an abstract base class for things that can        |
 | provide lists of resources - eg. .RES files, modules, etc.           |
 |                                                                      |
 | TResourceDetails is a base class for resources.                      |
 |                                                                      |
 | ... and here's the neat trick...                                     |
 |                                                                      |
 | Call the class function TResourceDetails.CreateResourceDetails to    |
 | create an instance of the appropriate registered TResourceDetails    |
 | descendant                                                           |
 |                                                                      |
 | ** Gold code **                                                      |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          28/05/2005  CPWW  ClearDirty made Protected instead of      |
 |                            Public                                    |
 |                            TResourceDetails.Create can now take      |
 |                            optional Data.                            |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)


unit unitResourceDetails;

interface

uses Windows, Classes, SysUtils;

type

TResourceDetails = class;
TResourceDetailsClass = class of TResourceDetails;


{$region 'TResourceModule class'}
  //======================================================================
  // TResourceModule class

  TResourceModule = class
  private
    FDirty: Boolean;
    function GetDirty: Boolean;
  protected
    function GetResourceCount: Integer; virtual; abstract;
    function GetResourceDetails(idx: Integer): TResourceDetails; virtual; abstract;
    procedure ClearDirty;

  public
    procedure DeleteResource (idx: Integer); virtual;
    procedure InsertResource (idx: Integer; details: TResourceDetails); virtual;
    function AddResource (details: TResourceDetails): Integer; virtual;
    function IndexOfResource (details: TResourceDetails): Integer; virtual; abstract;
    function GetUniqueResourceName (const tp: UnicodeString): UnicodeString;

    procedure SaveToStream (stream: TStream); virtual;
    procedure LoadFromStream (stream: TStream); virtual;

    procedure SaveToFile (const FileName: string); virtual;
    procedure LoadFromFile (const FileName: string); virtual;
    procedure SortResources; virtual;

    function FindResource (const tp, Name: UnicodeString; ALanguage: Integer): TResourceDetails;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails [idx: Integer]: TResourceDetails read GetResourceDetails;
    property Dirty: Boolean read GetDirty write FDirty;
  end;

{$endregion}

{$region 'TResourceDetails class'}
  //======================================================================
  // TResourceDetails class

  TResourceDetails = class
  private
    FParent: TResourceModule;
    FData: TMemoryStream;
    FCodePage: Integer;
    FResourceLanguage: LCID;
    FResourceName: UnicodeString;
    FResourceType: UnicodeString;

    FMemoryFlags: Word;                    // Resource memory flags
    FDataVersion, FVersion: DWORD;         // Resource header version info
    FCharacteristics: DWORD;
    FDirty: Boolean;
    FTag: Integer;
    procedure SetResourceType(const Value: UnicodeString);
                                           // Resource header characteristics

  protected
    constructor Create (AParent: TResourceModule; ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer; AData: pointer); virtual;
    procedure InitNew; virtual;
    procedure SetResourceName(const Value: UnicodeString); virtual;
    class function SupportsRCData (const AName: UnicodeString; Size: Integer; Data: Pointer): Boolean; virtual;
    class function SupportsData (Size: Integer; Data: Pointer): Boolean; virtual;
    function GetData: TMemoryStream; virtual;
  public
    class function CreateResourceDetails (AParent: TResourceModule; ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer; AData: pointer): TResourceDetails;
    class function GetBaseType: UnicodeString; virtual;

    constructor CreateNew (AParent: TResourceModule; ALanguage: Integer; const AName: UnicodeString); virtual;
    destructor Destroy; override;
    procedure BeforeDelete; virtual;

    procedure ChangeData (newData: TMemoryStream); virtual;

    property Parent: TResourceModule read FParent;
    property Data: TMemoryStream read GetData;
    property RawData: TMemoryStream read FData;
    property ResourceName: UnicodeString read FResourceName write SetResourceName;
    property ResourceType: UnicodeString read FResourceType write SetResourceType;
    property ResourceLanguage: LCID read FResourceLanguage write FResourceLanguage;

    property CodePage: Integer read FCodePage write FCodePage;
    property Characteristics: DWORD read FCharacteristics write FCharacteristics;
    property Version: DWORD read FVersion write FDataVersion;
    property DataVersion: DWORD read FDataVersion write FDataVersion;
    property MemoryFlags: WORD read FMemoryFlags write FMemoryFlags;

    property Dirty: Boolean read FDirty write FDirty;
    property Tag: Integer read FTag write FTag;
  end;
{$endregion}

{$region 'TAnsiResourceDetails class'}
  //======================================================================
  // TAnsiResourceDetails class

  TAnsiResourceDetails = class (TResourceDetails)
  private
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
  protected
    procedure InitNew; override;
    class function SupportsData (Size: Integer; Data: Pointer): Boolean; override;
  public
    property Text: AnsiString read GetText write SetText;
  end;

  TUTF8ResourceDetails = class (TResourceDetails)
  private
    function GetText: UnicodeString;
    procedure SetText(const Value: UnicodeString);
  protected
    procedure InitNew; override;
    class function SupportsData (Size: Integer; Data: Pointer): Boolean; override;
  public
    property Text: UnicodeString read GetText write SetText;
  end;
{$endregion}

{$region 'TUnicodeResourceDetails'}
  //======================================================================
  // TAnsiResourceDetails class

  TUnicodeResourceDetails = class (TResourceDetails)
  private
    function GetText: UnicodeString;
    procedure SetText(const Value: UnicodeString);
  protected
    procedure InitNew; override;
    class function SupportsData (Size: Integer; Data: Pointer): Boolean; override;
  public
    property Text: UnicodeString read GetText write SetText;
  end;
{$endregion}

//======================================================================
// Global function definitions

procedure RegisterResourceDetails (resourceClass: TResourceDetailsClass);
procedure UnRegisterResourceDetails (resourceClass: TResourceDetailsClass);
function ResourceNameToInt(const s: UnicodeString): Integer;
function ResourceWideCharToWideStr (var wstr: PWideChar): UnicodeString;
function ResourceWideCharToAnsiStr (var wstr: PWideChar; codepage: DWORD): AnsiString;
procedure ResourceWideStrToWideChar (const s: UnicodeString; var p: PWideChar);
function CompareDetails (p1, p2: Pointer): Integer;

implementation

{$region 'Local Declarations and Functions'}
var
  registeredResourceDetails: array of TResourceDetailsClass;
  registeredResourceDetailsCount: Integer = 0;

resourcestring
  rstNoBaseType = 'Can''t register resource details class with no base type';
  rstNoStreaming = 'Module doesn''t support streaming';

(*----------------------------------------------------------------------*
 | procedure RegisterResourceDetails                                    |
 |                                                                      |
 | Add a class, derived from TResourceDetails, to the list of           |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure RegisterResourceDetails (resourceClass: TResourceDetailsClass);
begin
  if Length (registeredResourceDetails) = registeredResourceDetailsCount then
    SetLength (registeredResourceDetails, Length (registeredResourceDetails) + 10);

  registeredResourceDetails [registeredResourceDetailsCount] := resourceClass;

  Inc(registeredResourceDetailsCount)
end;

(*----------------------------------------------------------------------*
 | procedure UnRegisterResourceDetails                                  |
 |                                                                      |
 | Remove a class, derived from TResourceDetails, from the list of      |
 | registered resource details classes                                  |
 *----------------------------------------------------------------------*)
procedure UnRegisterResourceDetails (resourceClass: TResourceDetailsClass);
var
  i: Integer;
begin
  i := 0;
  while i < registeredResourceDetailsCount do
    if registeredResourceDetails [i] = resourceClass then
    begin
      if i < Length (registeredResourceDetails) - 1 then
        Move (registeredResourceDetails [i + 1], registeredResourceDetails [i], (Length (registeredResourceDetails) - i - 1) * sizeof (TResourceDetailsClass));

      Dec(registeredResourceDetailsCount)
    end
    else
      Inc(i)
end;

(*----------------------------------------------------------------------*
 | procedure ResourceNameToInt                                          |
 |                                                                      |
 | Get integer value of resource name (or type).  Return -1 if it's     |
 | not numeric.                                                         |
 *----------------------------------------------------------------------*)

function ResourceNameToInt(const s: UnicodeString): Integer;
var
  isNumeric: Boolean;
  i: Integer;
begin
  isNumeric := Length (s) > 0;
  for i := 1 to Length (s) do
    if (s [i] < '0') or (s [i] > '9') then
    begin
      isNumeric := False;
      break
    end;

  if isNumeric then
    Result := StrToInt(s)
  else
    Result := -1
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToWideStr ()                                     |
 |                                                                            |
 | Convert Pascal-style WideChar array to a WideString                        |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr: PWChar             The characters                                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToWideStr (var wstr: PWideChar): UnicodeString;
var
  len: Word;
begin
  len := Word (wstr^);
  SetLength (Result, len);
  Inc(wstr);
  Move (wstr^, PWideChar (Result)^, len * sizeof (WideChar));
  Inc(wstr, len);
end;

function ResourceWideCharToAnsiStr (var wstr: PWideChar; codepage: DWORD): AnsiString;
var
  s: UnicodeString;
  lu, la: Integer;
begin
  s := ResourceWideCharToWideStr (wstr);

  lu := Length (s)+1;
  la := (4 * lu) + 1;
  SetLength (Result, la);

  la := WideChartoMultiByte (codepage, 0, PWideChar (s), lu, PAnsiChar (Result), la, nil, nil);
  SetLength (Result, la-1);
end;
(*----------------------------------------------------------------------------*
 | procedure ResourceWideStrToWideChar ()                                     |
 |                                                                            |
 | Convert a wide string to a Pascal style Wide char array                    |
 |                                                                            |
 | Parameters:                                                                |
 |   s: string                The string                                     |
 |   var p: PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 *----------------------------------------------------------------------------*)
procedure ResourceWideStrToWideChar (const s: UnicodeString; var p: PWideChar);
var
  len: Word;
begin
  len := Length (s);
  p^ := WideChar (len);
  Inc(p);
  Move (PWideChar (s)^, p^, len * sizeof (WideChar));
  Inc(p, len)
end;

(*----------------------------------------------------------------------*
 | function CompareDetails                                              |
 |                                                                      |
 | 'Compare' function used when sorting resources.  p1 and p2 must be   |
 | TResourceDetails references.  Returns > 0 if details at p1 are >     |
 | details at p2.                                                       |
 |                                                                      |
 | *  Compare resource types.  If they match then compare names.        |
 | *  'Integer' ids or names must come *after* non integer ids or names.|
 *----------------------------------------------------------------------*)
function CompareDetails (p1, p2: Pointer): Integer;
var
  d1: TResourceDetails;
  d2: TResourceDetails;
  i1, i2: Integer;
begin
  d1 := TResourceDetails (p1);
  d2 := TResourceDetails (p2);

  i1 := ResourceNameToInt(d1.ResourceType);
  i2 := ResourceNameToInt(d2.ResourceType);

  if i1 >= 0 then
    if i2 >= 0 then
      Result := i1 - i2         // Compare two integer ids
    else
      Result := 1               // id1 is int, so it's greater than non-int id2
  else
    if i2 >= 0 then
      Result := -1              // id2 is int, so it's less than non-int id1
    else
                                // Compare two string resource ids
      Result := CompareText(d1.ResourceType, d2.ResourceType);

  if Result = 0 then            // If they match, do the same with the names
  begin
    i1 := ResourceNameToInt(d1.ResourceName);
    i2 := ResourceNameToInt(d2.ResourceName);

    if i1 >= 0 then
      if i2 >= 0 then
        Result := i1 - i2
      else
        Result := 1
    else
      if i2 >= 0 then
        Result := -1
      else
        Result := CompareText(d1.ResourceName, d2.ResourceName)
  end
end;

(*----------------------------------------------------------------------*
 | function LCIDTOCodePage                                              |
 |                                                                      |
 | Get the ANSI code page for a given language ID                       |
 *----------------------------------------------------------------------*)
function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result:= StrToIntDef(Buffer, GetACP);
end;

{$endregion}

{$region 'TResourceDetails implementation'}
{ TResourceDetails }

(*----------------------------------------------------------------------*
 | TResourceDetails.BeforeDelete                                        |
 |                                                                      |
 | Can override this to clear up before deleting.  Eg. deleting an      |
 | icon removes it from the icon group it's in.  Deleting an icon group |
 | removes the individual icon resources, etc.                          |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.BeforeDelete;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.ChangeData                                          |
 |                                                                      |
 | Change all the Data.  Handy for implementing 'undo', etc.            |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.ChangeData(newData: TMemoryStream);
begin
  FData.Clear;
  FData.CopyFrom (newData, 0);
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Create                                              |
 |                                                                      |
 | Raw - protected - constructor for resource details.                  |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer;
  AData: pointer);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage (FResourceLanguage);
  FResourceName := AName;
  FResourceType := AType;
  FData := TMemoryStream.Create;
  if AData <> Nil then
    FData.Write (AData^, ASize)
  else
    InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateNew                                           |
 |                                                                      |
 | Constructor to be used when adding new resources to a module.        |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const aName: UnicodeString);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage (FResourceLanguage);
  FResourceName := AName;
  FResourceType := GetBaseType;
  if Assigned(AParent) then
    AParent.AddResource (Self);
  FData := TMemoryStream.Create;
  InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateResourceDetails                               |
 |                                                                      |
 | Create a class derived from TResourceDetals that reflects the 'Type' |
 | If no matching class is registered, create a base 'TResourceDetails' |
 | class.    (Ha!  Try doing *that* in C++ ! )                          |
 *----------------------------------------------------------------------*)
class function TResourceDetails.CreateResourceDetails(
  AParent: TResourceModule; ALanguage: Integer; const AName,
  AType: UnicodeString; ASize: Integer; AData: pointer): TResourceDetails;
var
  i: Integer;
begin
  Result := Nil;

  if (Length (AType) > 0) then
  try

  // Check for exact match

    for i := 0 to registeredResourceDetailsCount - 1 do
      if registeredResourceDetails [i].GetBaseType = AType then
      begin
        if (AType <> IntToStr (Integer (RT_RCDATA))) or registeredResourceDetails [i].SupportsRCData (AName, ASize, AData) then
        begin
          Result := registeredResourceDetails [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
          break
        end
      end;
  except
  end;

  // If no exact match, check each clas to see if it supports the Data
  if Result = nil then
  try
    for i := 0 to registeredResourceDetailsCount - 1 do
      if registeredResourceDetails [i].SupportsData (ASize, AData) then
      begin
        Result := registeredResourceDetails [i].Create (AParent, ALanguage, AName, AType, ASize, AData);
        break
      end;
  except
  end;

  if Result = Nil then
    if TAnsiResourceDetails.SupportsData(ASize, AData) then
      Result := TAnsiResourceDetails.Create (AParent, ALanguage, AName, AType, ASize, AData)
    else
      if TUnicodeResourceDetails.SupportsData(ASize, AData) then
        Result := TUnicodeResourceDetails.Create (AParent, ALanguage, AName, AType, ASize, AData)
      else
        Result := TResourceDetails.Create (AParent, ALanguage, AName, AType, ASize, AData)
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Destroy                                             |
 *----------------------------------------------------------------------*)
destructor TResourceDetails.Destroy;
begin
  FData.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.GetBaseType                                         |
 |                                                                      |
 | Return the base type for the resource details.  This is overridden   |
 | in derived classes.                                                  |
 *----------------------------------------------------------------------*)
class function TResourceDetails.GetBaseType: UnicodeString;
begin
  Result := '0';
end;

function TResourceDetails.GetData: TMemoryStream;
begin
  Result := FData
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.InitNew                                             |
 |                                                                      |
 | Override this to initialize a new resource being added to a module.  |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.InitNew;
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SetResourceName                                     |
 |                                                                      |
 | Set the resource name.                                               |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.SetResourceName(const Value: UnicodeString);
begin
  if FResourceName <> Value then
  begin
    FResourceName := Value;
    FDirty := True
  end
end;

procedure TResourceDetails.SetResourceType(const Value: UnicodeString);
begin
  if FResourceType <> Value then
  begin
    FResourceType := Value;
    FDirty := True
  end
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support a custom resource class, where you can  |
 | determine the custom class from the Data - eg. RIFF Data, etc.       |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
begin
  Result := False; // stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support RC Data where you can determine the     |
 | type from the Data and name - eg. the Delphi splash screen JPEG      |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsRCData(const AName: UnicodeString;
  Size: Integer; Data: Pointer): Boolean;
begin
  Result := False; // stub
end;

{$endregion}

{$region 'TResourceModule implementation'}
{ TResourceModule }

function TResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := -1
  // Stub
end;

procedure TResourceModule.ClearDirty;
var
  i: Integer;
begin
  FDirty := False;
  for i := 0 to ResourceCount - 1 do
    ResourceDetails [i].Dirty := False
end;

(*----------------------------------------------------------------------*
 | TResourceModule.DeleteResource                                       |
 |                                                                      |
 | Must be overridden to remove the resource details object from        |
 | wherever it's stored.  The overriding method must call               |
 | inherited                                                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.DeleteResource(idx: Integer);
begin
  FDirty := True;
  ResourceDetails [idx].BeforeDelete;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.FindResource                                         |
 |                                                                      |
 | Find a resource with a given type/name                               |
 *----------------------------------------------------------------------*)
function TResourceModule.FindResource(const tp,
  Name: UnicodeString; ALanguage: Integer): TResourceDetails;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceDetails [i].FResourceType = tp) and (ResourceDetails [i].FResourceName = Name) and (Integer (ResourceDetails [i].FResourceLanguage) = ALanguage) then
    begin
      Result := ResourceDetails [i];
      break
    end;

  if not Assigned(Result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceDetails [i].FResourceType = tp) and (ResourceDetails [i].FResourceName = Name) and (ResourceDetails [i].FResourceLanguage = 0) then
      begin
        Result := ResourceDetails [i];
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetDirty                                             |
 |                                                                      |
 | Returns true if the module or it's resources are 'dirty'             |
 |                                                                      |
 | nb. FDirty is only set if resources have been deleted.               |
 |     After adding a resource make sure the resource's Dirty is set to |
 |     true.                                                            |
 *----------------------------------------------------------------------*)
function TResourceModule.GetDirty: Boolean;
var
  i: Integer;
begin
  Result := FDirty;
  if not FDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceDetails [i].Dirty then
      begin
        Result := True;
        break
      end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetUniqueResourceName                                |
 |                                                                      |
 | Generate a unique resource name for a given type.  Names start at    |
 | 1 (though string lists downgrade that to '0')                        |
 *----------------------------------------------------------------------*)
function TResourceModule.GetUniqueResourceName(const tp: UnicodeString): UnicodeString;
var
  i: Integer;
  n, n1: Integer;
  details: TResourceDetails;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
  begin
    details := ResourceDetails [i];
    if details.ResourceType = tp then
    begin
      n1 := ResourceNametoInt(details.ResourceName);
      if n1 > n then
        n := n1
    end
  end;

  Result := IntToStr (n + 1);
end;

procedure TResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from file.  This can be overriden but usually isn't as it       |
 | relies on LoadFromStream, which must be.                             |
 *----------------------------------------------------------------------*)
procedure TResourceModule.LoadFromFile(const FileName: string);
var
  s: TFileStream;
begin
  s := TFileStream.Create (FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (s);
  finally
    s.Free
  end
end;


procedure TResourceModule.LoadFromStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

(*----------------------------------------------------------------------*
 | TResourceModule.SaveToFile                                           |
 |                                                                      |
 | Save to file.  This can be overriden but usually isn't as it         |
 | relies on SaveToStream, which must be.                               |
 *----------------------------------------------------------------------*)
procedure TResourceModule.SaveToFile(const FileName: string);
var
  s: TFileStream;
  oldFileName, ext: string;
  p: PChar;
begin
// Rename old file to .~ext'
  oldFileName := FileName;
  UniqueString (oldFileName);
  p := StrRScan (PChar (oldFileName), '.');
  if p <> Nil then
  begin
    p^ := #0;
    Inc(p);
    ext := p;
    oldFileName := PChar (oldFileName);
  end
  else
    ext := '';
  ext := '~' + ext;
  oldFileName := oldFileName + '.' + ext;

  if FileExists (oldFileName) then
    DeleteFile (oldFileName);

  RenameFile (FileName, oldFileName);

  try
    s := TFileStream.Create (FileName, fmCreate);
    try
      SaveToStream (s);
      ClearDirty
    finally
      s.Free
    end
  except
// Failed.  Rename old file back.
    DeleteFile (FileName);
    RenameFile (oldFileName, FileName);
    raise
  end
end;

procedure TResourceModule.SaveToStream(stream: TStream);
begin
  raise Exception.Create (rstNoStreaming);
end;

procedure TResourceModule.SortResources;
begin
// Stub
end;
{$endregion}

{$region 'TAnsiResourceDetails implementation'}
{ TAnsiResourceDetails }

function TAnsiResourceDetails.GetText: AnsiString;
begin
  Data.Seek(0, TSeekOrigin.soBeginning);
  SetString (Result, PAnsiChar (Data.Memory), Data.Size);
end;

procedure TAnsiResourceDetails.InitNew;
begin
  Data.Clear;
end;

procedure TAnsiResourceDetails.SetText(const Value: AnsiString);
begin
  Data.Clear;
  Data.Write(Value [1], Length (Value))
end;

class function TAnsiResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
var
  i, sample: Integer;
  pc: PAnsiChar;
begin
  Result := Size > 0;
  sample := Size;
  if Sample > 1024 then
    Sample := 1024;
  pc := PAnsiChar (Data);

  if Result then
    for i := 0 to Sample - 1 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not(pc^ in [#9, #10, #13]) then
        begin
          Result := False;
          break
        end;

      Inc(pc)
    end
end;
{$endregion}

{$region 'TUnicodeResourceDetails implementation'}
{ TUnicodeResourceDetails }

function TUnicodeResourceDetails.GetText: UnicodeString;
begin
  SetLength(Result, Data.Size div sizeof (WideChar));
  Move(Data.Memory^, Result [1], Data.Size);
end;

procedure TUnicodeResourceDetails.InitNew;
begin
  Data.Clear;
end;

procedure TUnicodeResourceDetails.SetText(const Value: UnicodeString);
begin
  Data.Write(Value [1], Length (Value) * sizeof (WideChar))
end;

class function TUnicodeResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
var
  i, sample: Integer;
  pc: PWideChar;
begin
  Result := Size > 5;
  sample := Size div 2;
  if Sample > 1024 then
    Sample := 1024
  else
    Dec(Sample);
  pc := PWideChar (Data);

  if Result then
    for i := 0 to Sample - 2 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if (pc^ <> #9) and (pc^ <> #10) and (pc^ <> #13) then
        begin
          Result := False;
          break
        end;

      Inc(pc)
    end
end;
{$endregion}

{ TUTF8ResourceDetails }

function TUTF8ResourceDetails.GetText: UnicodeString;
var
  st: UTF8String;
begin
  Data.Seek(0, TSeekOrigin.soBeginning);
  SetString (st, PAnsiChar (Data.Memory), Data.Size);
  Result := UTF8ToUnicodeString (st);   // ******
//  raise Exception.Create('Must check bug is fixed!!!') 
end;

procedure TUTF8ResourceDetails.InitNew;
begin
  Data.Clear;
end;

procedure TUTF8ResourceDetails.SetText(const Value: UnicodeString);
var
  st: UTF8String;
begin
  st := UTf8Encode (Value);
  Data.Clear;
  Data.Write(st [1], Length (st))
end;

class function TUTF8ResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
var
  i, sample: Integer;
  pc: PAnsiChar;
begin
  Result := Size > 0;
  sample := Size;
  if Sample > 1024 then
    Sample := 1024;
  pc := PAnsiChar (Data);

  if Result then
    for i := 0 to Sample - 1 do
    begin
      if (pc^ < ' ') or (pc^ > #127) then
        if not(pc^ in [#9, #10, #13]) then
        begin
          Result := False;
          break
        end;

      Inc(pc)
    end
end;

end.
