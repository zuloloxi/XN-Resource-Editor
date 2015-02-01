(*======================================================================*
 | unitResourceMessages                                                 |
 |                                                                      |
 | Handle string and message resources.                                 |
 |                                                                      |
 | ** Gold code - 24/4/2001 **                                          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      24/04/2001  CPWW  Original                                  |
 *======================================================================*)

unit unitResourceMessages;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type

//-------------------------------------------------------------------------
// TStringInfo

TStringInfo = class
private
  fId : Integer;
  fST : UnicodeString;
public
  constructor Create (const ASt : UnicodeString; AId : Integer);
  function IsUnicode : boolean;

  property Id : Integer read fId write fId;
  property St : UnicodeString read fSt write fSt;
end;

//-------------------------------------------------------------------------
// TTextResourceDetails
//
// Base class for messages & strings
TTextResourceSort = (trString, trID, trReverseString, trReverseID);
TTextResourceDetails = class (TResourceDetails)
private
  fStrings : TObjectList;
  fUpdating : boolean;
  function GetCount: Integer;
  procedure GetStrings;
  function GetString(idx: Integer): UnicodeString;
  function GetId(idx: Integer): Integer;
  procedure SetId(idx: Integer; const Value: Integer);
  procedure SetString(idx: Integer; const Value: UnicodeString);
protected
  procedure DecodeStrings; virtual; abstract;
  procedure EncodeStrings; virtual; abstract;
public
  destructor Destroy; override;
  procedure ChangeData (newData : TMemoryStream); override;
  procedure BeginUpdate;
  procedure EndUpdate;
  procedure Delete (idx : Integer);
  procedure Sort (sortType : TTextResourceSort = trString);
  function IndexOfID (id : Integer) : Integer;

  property Count : Integer read GetCount;
  property Strings [idx : Integer] : UnicodeString read GetString write SetString; default;
  property Ids [idx : Integer] : Integer read GetId write SetId;

end;

//-------------------------------------------------------------------------
// TMessageResourceDetails

TMessageResourceDetails = class (TTextResourceDetails)
private
protected
  procedure DecodeStrings; override;
  procedure EncodeStrings; override;
  procedure InitNew; override;

public
  class function GetBaseType : UnicodeString; override;
end;

//-------------------------------------------------------------------------
// TStringResourceDetails

TStringResourceDetails = class (TTextResourceDetails)
protected
  procedure DecodeStrings; override;
  procedure EncodeStrings; override;
  procedure InitNew; override;
  procedure SetResourceName(const Value: UnicodeString); override;

public
  class function GetBaseType : UnicodeString; override;
end;

//-------------------------------------------------------------------------
// Global functions declarations

function ResIdToStringsId (const resourceName : UnicodeString) : UnicodeString;
function StringsIdToResId (const stringsId : UnicodeString) : UnicodeString;

implementation

type
  TMessageResourceBlock = record
    lowID : DWORD;
    highID : DWORD;
    entryOffset : DWORD;  // Offset to entries from the start of the message resource
  end;
  PMessageResourceBlock = ^TMessageResourceBlock;

  TMessageResourceEntry = packed record
    len : WORD;         // Sizeof the structure (not length of the string!)
    flags : WORD;
    st : array [0..0] of byte;
  end;
  PMessageResourceEntry = ^TMessageResourceEntry;

//-------------------------------------------------------------------------
// Global functions definitions

function ResIdToStringsId (const resourceName : UnicodeString) : UnicodeString;
begin
  Result := IntToStr ((StrToInt (resourceName) - 1) * 16)
end;

function StringsIdToResId (const stringsId : UnicodeString) : UnicodeString;
begin
  Result := IntToStr (StrToInt (stringsId) div 16 + 1)
end;

{ TStringResourceDetails }

(*----------------------------------------------------------------------*
 | TStringResourceDetails.DecodeStrings                                 |
 |                                                                      |
 | Extract strings from string table into fStrings list                 |
 *----------------------------------------------------------------------*)
procedure TStringResourceDetails.DecodeStrings;
var
  p : PWideChar;
  cnt, id : Integer;
  st : UnicodeString;
begin
  p := PWideChar (Data.Memory);
  cnt := 0;

  while Cnt < 16 do
  begin
    id := (StrToInt (ResourceName) - 1) * 16 + cnt;
    st := ResourceWideCharToWideStr (p);
    fStrings.Add (TStringInfo.Create (st, id));
    Inc (Cnt);
  end
end;

(*----------------------------------------------------------------------*
 | TStringResourceDetails.EncodeStrings                                 |
 |                                                                      |
 | Encode strings from fStrings list into string table                  |
 *----------------------------------------------------------------------*)
procedure TStringResourceDetails.EncodeStrings;
var
  n, i : Integer;
  p : PWideChar;
begin
                                // Calculate total size of the 16 null-terminated
                                // wide strings.
  n := 16 * sizeof (WideChar);
  for i := 0 to Count - 1 do
    Inc (n, (Length (Strings [i])) * sizeof (WideChar));

  Data.Size := n;
  p := PWideChar (data.Memory);
  ZeroMemory (p, n);

  for i := 0 to Count - 1 do
    ResourceWideStrToWideChar (Strings [i], p);
end;

class function TStringResourceDetails.GetBaseType: UnicodeString;
begin
  result := IntToStr (Integer (RT_STRING));
end;

procedure TStringResourceDetails.InitNew;
var
  i : Integer;
  wc : WideChar;
begin
  wc := #0;
  for i := 0 to 15 do
    data.Write (wc, SizeOf (wc))
end;

procedure TStringResourceDetails.SetResourceName(const Value: UnicodeString);
var
  i, lid : Integer;

begin
  inherited;

  lid := (StrToInt (Value) - 1) * 16;

  for i := 0 to Count - 1 do
    with TStringInfo (fStrings [i]) do
      fId := lid + i
end;

{ TMessageResourceDetails }

procedure TMessageResourceDetails.DecodeStrings;
var
  i, blockCount, id : Integer;
  block : PMessageResourceBlock;
  entry : PMessageResourceEntry;
  p : PByte;
begin
  blockCount := PInteger (Data.Memory)^;

  block := PMessageResourceBlock (PByte (Data.Memory) + sizeof (Integer));

  for i := 0 to blockCount - 1 do
  begin
    id := block^.lowID;
    p := data.memory;
    Inc (p, block^.entryOffset);              // 'p' points to the block's messages

    entry := PMessageResourceEntry (p);
    while id <= Integer (block^.highID) do
    begin
                                             // nb.  entry^.len is the *size*
                                             // of the entry - not the length
                                             // of the string!

      if (entry^.flags and 1) = 1 then       // Unicode
        fStrings.Add (TStringInfo.Create (PWideChar (@entry^.st [0]), id))
      else
        fStrings.Add (TStringInfo.Create (String (PAnsiChar (@entry^.st [0])), id));

      entry := PMessageResourceEntry (PByte (entry) + entry^.len);
      Inc (id)
    end;
    Inc (block)
  end
end;

procedure TMessageResourceDetails.EncodeStrings;
var
  i, id, lastId, dataSize, blockCount, len : Integer;
  block : PMessageResourceBlock;
  offset : DWORD;
  st : AnsiString;
  uniCode : word;
begin
  dataSize := sizeof (DWORD);   // sizeof initial NumberOfBlocks DWORD;
  lastId := -2;
  blockCount := 0;
  for i := 0 to fStrings.Count - 1 do   // Count non-contiguous blocks & calculate total size
  begin
    id := TStringInfo (fStrings [i]).fId;
    uniCode := Ord (TStringInfo (fStrings [i]).IsUnicode);
    if id <> lastId + 1 then
    begin
      Inc (blockCount);
      Inc (dataSize, SizeOf (TMessageResourceBlock));
    end;

    lastId := id;

    len := Length (Strings [i]) + 1;
    if unicode <> 0 then
      len := len * 2;
                                        // len is now length of string in
                                        // bytes -including nul terminator.

    Inc (len, 2 * sizeof (WORD));       // Add sizeof flags & len

    len := (len + 3) div 4 * 4;         // DWORD align

    Inc (dataSize, len)
  end;

  data.Size := dataSize;
  PInteger (data.Memory)^ := blockCount;
  offset := sizeof (Integer) + blockCount * sizeof (TMessageResourceBlock);
  blockCount := 0;
  block := Nil;

  lastId := -2;
  for i := 0 to fStrings.Count - 1 do
  begin
    id := TStringInfo (fStrings [i]).fId;
    uniCode := Ord (TStringInfo (fStrings [i]).IsUnicode);
    if id <> lastId + 1 then
    begin
      if lastId <> -2 then
        block^.highID := lastId;
      block := PMessageResourceBlock (PByte (data.Memory) + sizeof (Integer));
      Inc (block, blockCount);
      block^.lowID := id;
      block^.entryOffset := offset;
      Inc (blockCount)
    end;

    lastId := id;
    len := Length (Strings [i]) + 1;
    if unicode <> 0 then
      len := len * 2;

    len := (len + 3) div 4 * 4;

    PWORD (PByte (data.Memory) + offset)^ := 2 * sizeof (word) + len;
    Inc (offset, sizeof (WORD));

    PWORD (PByte (data.Memory) + offset)^ := uniCode;
    Inc (offset, sizeof (WORD));

    ZeroMemory (PByte (data.Memory) + offset, len);
    if uniCode = 0 then
    begin
      st := AnsiString (Strings [i]);
      lstrcpya (PAnsiChar (data.Memory) + offset, PAnsiChar (st))
    end
    else
      lstrcpyw (PWideChar (PByte (data.Memory) + offset), PWideChar (Strings [i]));
    Inc (offset, len)
  end;
  if lastId <> -2 then
    block^.highID := lastId;
end;

class function TMessageResourceDetails.GetBaseType: UnicodeString;
begin
  result := IntToStr (Integer (RT_MESSAGETABLE));
end;

procedure TMessageResourceDetails.InitNew;
const
  zero : Integer = 0;
begin
  data.Write (zero, SizeOf (zero));
end;

{ TTextResourceDetails }

procedure TTextResourceDetails.BeginUpdate;
begin
  fUpdating := True;
end;

procedure TTextResourceDetails.ChangeData(newData: TMemoryStream);
begin
  inherited;

  FreeAndNil (fStrings)
end;

procedure TTextResourceDetails.Delete(idx: Integer);
begin
  if idx < fStrings.Count then
  begin
    fStrings.Delete(idx);

    if not fUpdating then
      EncodeStrings
  end
end;

destructor TTextResourceDetails.Destroy;
begin
  fStrings.Free;
  inherited;
end;

procedure TTextResourceDetails.EndUpdate;
begin
  if fUpdating then
  begin
    fUpdating := False;
    EncodeStrings
  end
end;

function TTextResourceDetails.GetCount: Integer;
begin
  GetStrings;
  result := fStrings.Count
end;

function TTextResourceDetails.GetId(idx: Integer): Integer;
begin
  GetStrings;
  result := TStringInfo (fStrings [idx]).fID;
end;

function TTextResourceDetails.GetString(idx: Integer): UnicodeString;
begin
  GetStrings;
  result := TStringInfo (fStrings [idx]).fST;
end;

procedure TTextResourceDetails.GetStrings;
begin
  if not Assigned (fStrings) then
  begin
    fStrings := TObjectList.Create;
    DecodeStrings
  end
end;

function TTextResourceDetails.IndexOfID(id: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;
  if Assigned (fStrings) then
    for i := 0 to fStrings.Count - 1 do
      if TStringInfo (fStrings [i]).fId = id then
      begin
        Result := i;
        break
      end
end;

procedure TTextResourceDetails.SetId(idx: Integer; const Value: Integer);
begin
  with TStringInfo (fStrings [idx]) do
    fId := Value;

  if not fUpdating then
    EncodeStrings
end;

procedure TTextResourceDetails.SetString(idx: Integer;
  const Value: UnicodeString);
begin
  if idx = fStrings.Count then
    fStrings.Add (TStringInfo.Create (Value, idx))
  else
    TStringInfo (fStrings [idx]).fSt := Value;

  if not fUpdating then
    EncodeStrings
end;

function CompareStringInfo (p1, p2 : Pointer) : Integer;
begin
  Result := WideCompareText (TStringInfo (p1).fST, TStringInfo (p2).fST)
end;

function ReverseCompareStringInfo (p1, p2 : Pointer) : Integer;
begin
  Result := -WideCompareText (TStringInfo (p1).fST, TStringInfo (p2).fST)
end;

function CompareIDS (p1, p2 : Pointer) : Integer;
begin
  Result := TStringInfo (p1).fId - TStringInfo (p2).fId
end;

function ReverseCompareIDS (p1, p2 : Pointer) : Integer;
begin
  Result := TStringInfo (p2).fId - TStringInfo (p1).fId
end;

procedure TTextResourceDetails.Sort (sortType : TTextResourceSort);
begin
  case sortType of
    trString        : fStrings.Sort(CompareStringInfo);
    trID            : fStrings.Sort(CompareIDS);
    trReverseString : fStrings.Sort(ReverseCompareStringInfo);
    trReverseID     : fStrings.Sort(ReverseCompareIDS)
  end
end;

{ TStringInfo }

constructor TStringInfo.Create(const ASt : UnicodeString; AId: Integer);
begin
  fSt := ASt;
  fID := AId;
end;

function TStringInfo.IsUnicode : boolean;
begin
  result := WideCharToMultiByte (CP_UTF8, 0, PWideChar (fST), -1, Nil, 0, Nil, Nil) <> Length (fST) + 1;
end;

initialization
  RegisterResourceDetails (TMessageResourceDetails);
  RegisterResourceDetails (TStringResourceDetails);
finalization
  UnregisterResourceDetails (TStringResourceDetails);
  UnregisterResourceDetails (TMessageResourceDetails);
end.
