(*======================================================================*
 | unitResourceRCData                                                   |
 |                                                                      |
 | Encapsulates RC Data resources in resources                          |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001,2008                                 |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)
unit unitResourceRCData;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails, ZLib, unitResourceGraphics;

type
TRCDataResourceDetails = class (TResourceDetails)
private
  fDelegate : TResourceDetails;
public
  class function GetBaseType : UnicodeString; override;
  procedure ChangeData (newData : TMemoryStream); override;
  function GetData: TMemoryStream; override;

  property Delegate : TResourceDetails read fDelegate;
end;

TRCDataDescriptionResourceDetails = class (TRCDataResourceDetails)
private
  function GetDescription: UnicodeString;
  procedure SetDescription(const Value: UnicodeString);
protected
  class function SupportsRCData (const AName : UnicodeString; Size : Integer; data : Pointer) : Boolean; override;
public
  property Description : UnicodeString read GetDescription write SetDescription;
end;

TRCDataFormResourceDetails = class (TRCDataResourceDetails)
private
  function GetText: UnicodeString;
  procedure SetText(const Value: UnicodeString);
protected
  class function SupportsRCData (const AName : UnicodeString; Size : Integer; data : Pointer) : Boolean; override;
public
  property Text : UnicodeString read GetText write SetText;
end;

TRCDataCompressedBitmapResourceDetails = class (TRCDataResourceDetails)
protected
  class function SupportsRCData (const AName : UnicodeString; Size : Integer; data : Pointer) : Boolean; override;
public
  constructor Create (AParent : TResourceModule; ALanguage : Integer; const AName, AType : UnicodeString; ASize : Integer; AData : pointer); override;
end;

TCompressedBitmapResourceDetails = class (TBitmapResourceDetails)
private
  fCompressedData : TMemoryStream;

protected
  function GetData : TMemoryStream; override;
  constructor Create (AParent : TResourceModule; ALanguage : Integer; const AName, AType : UnicodeString; ASize : Integer; AData : pointer); override;

public
  constructor CreateNew (AParent : TResourceModule; ALanguage : Integer; const AName : UnicodeString); override;
  destructor Destroy; override;
  procedure ChangeData (data : TMemoryStream); override;
end;

TPackageEnvironment = (pePreV4, peUndefine, peBCB, peDelphi);
TModuleType = (mtEXE, mtPackageDLL, mtLibraryDLL, mtUndefine);

TRCDataPackagesResourceDetails = class (TRCDataResourceDetails)
private
  fRequiresList : TStrings;
  fContainsList : TStrings;
  fFlags : DWORD;

  function GetRequiresCount: Integer;
  function GetRequires(idx : Integer): string;
  function GetContainsCount: Integer;
  function GetContains(idx: Integer): string;
  function GetContainsFlag(idx: Integer): Byte;

  procedure DecodeData;
  function GetCheckForDuplicates: Boolean;
  function GetDesignTimeOnly: Boolean;
  function GetEnvironment: TPackageEnvironment;
  function GetModuleType: TModuleType;
  function GetNeverBuild: Boolean;
  function GetRunTimeOnly: Boolean;
protected
  class function SupportsRCData (const AName : UnicodeString; Size : Integer; data : Pointer) : Boolean; override;
public
  destructor Destroy; override;
  procedure ChangeData (newData : TMemoryStream); override;
  property RequiresCount : Integer read GetRequiresCount;
  property Requires [idx : Integer] : string read GetRequires;
  property ContainsCount : Integer read GetContainsCount;
  property Contains [idx : Integer] : string read GetContains;
  property ContainsFlag [idx : Integer] : Byte read GetContainsFlag;

  property NeverBuild : Boolean read GetNeverBuild;
  property DesignTimeOnly : Boolean read GetDesignTimeOnly;
  property RunTimeOnly : Boolean read GetRunTimeOnly;
  property CheckForDuplicates : Boolean read GetCheckForDuplicates;
  property Environment : TPackageEnvironment read GetEnvironment;
  property ModuleType : TModuleType read GetModuleType;


end;

implementation

type
  TPkgName = packed record
    HashCode : Byte;
    Name : array [0..255] of AnsiChar;
  end;
  PPkgName = ^TPkgName;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }

  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of AnsiChar;
  end;

{ TRCDataResourceDetails }

procedure TRCDataResourceDetails.ChangeData(newData: TMemoryStream);
begin
  if Delegate <> Nil then
    Delegate.ChangeData(newData)
  else
    inherited;
end;

class function TRCDataResourceDetails.GetBaseType: UnicodeString;
begin
  result := IntToStr (Integer (RT_RCDATA));
end;

function TRCDataResourceDetails.GetData: TMemoryStream;
begin
  if Delegate <> Nil then
    result := Delegate.Data
  else
    result := inherited;
end;

{ TRCDataDescriptionResourceDetails }

function TRCDataDescriptionResourceDetails.GetDescription: UnicodeString;
begin
  Result := PWideChar (data.Memory);
end;

procedure TRCDataDescriptionResourceDetails.SetDescription(
  const Value: UnicodeString);
begin
  data.Size := (Length (Value) + 1) * SizeOf (WideChar);
  Move (Value [1], data.memory^, (Length (Value) + 1) * SizeOf (WideChar))
end;

class function TRCDataDescriptionResourceDetails.SupportsRCData(
  const AName: UnicodeString; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText (AName, 'DESCRIPTION') = 0;
end;

{ TRCDataPackagesResourceDetails }

procedure TRCDataPackagesResourceDetails.ChangeData(
  newData: TMemoryStream);
begin
  inherited;
  FreeAndNil (fRequiresList);
  FreeAndNil (fContainsList);
end;

procedure TRCDataPackagesResourceDetails.DecodeData;
var
  p : PAnsiChar;
  i, Count : Integer;
  pkg : PPkgName;
  unt : PUnitName;
begin
  if not Assigned (fRequiresList) then
  begin
    fRequiresList := TStringList.Create;
    fContainsList := TStringList.Create;

    p := Data.Memory;
    fFlags := PDWORD (p)^;
    Inc (p, SizeOf (DWORD)); //  Flags

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      pkg := PPkgName (p);


      fRequiresList.Add (String (pkg^.Name));
      Inc (p, 2 + lstrlena (pkg^.Name));
    end;

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      unt := PUnitName (p);
      fContainsList.AddObject (String (unt^.Name), TObject (Integer (unt.Flags)));
      Inc (p, 3 + lstrlena (unt^.Name));
    end
  end
end;

destructor TRCDataPackagesResourceDetails.Destroy;
begin
  fRequiresList.Free;
  fContainsList.Free;
  inherited;
end;

function TRCDataPackagesResourceDetails.GetCheckForDuplicates: Boolean;
begin
  DecodeData;
  Result := (fFlags and 8) = 0
end;

function TRCDataPackagesResourceDetails.GetContains(idx: Integer): string;
begin
  DecodeData;
  Result := fContainsList [idx]
end;

function TRCDataPackagesResourceDetails.GetContainsCount: Integer;
begin
  DecodeData;
  Result := fContainsList.Count
end;

function TRCDataPackagesResourceDetails.GetContainsFlag(
  idx: Integer): Byte;
begin
  DecodeData;
  Result := Integer (fContainsList.Objects [idx])
end;

function TRCDataPackagesResourceDetails.GetDesignTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 2) <> 0
end;

function TRCDataPackagesResourceDetails.GetEnvironment: TPackageEnvironment;
begin
  DecodeData;
  Result := TPackageEnvironment ((fFlags shr 26) and 3);
end;

function TRCDataPackagesResourceDetails.GetModuleType: TModuleType;
begin
  DecodeData;
  Result := TModuleType (fFlags shr 30);
end;

function TRCDataPackagesResourceDetails.GetNeverBuild: Boolean;
begin
  DecodeData;
  Result := (fFlags and 1) <> 0
end;

function TRCDataPackagesResourceDetails.GetRequires(idx : Integer): string;
begin
  DecodeData;
  Result := fRequiresList [idx]
end;

function TRCDataPackagesResourceDetails.GetRequiresCount: Integer;
begin
  DecodeData;
  Result := fRequiresList.Count
end;

function TRCDataPackagesResourceDetails.GetRunTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 4) <> 0
end;

class function TRCDataPackagesResourceDetails.SupportsRCData(
  const AName: UnicodeString; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText (AName, 'PACKAGEINFO') = 0;
end;

{ TRCDataFormResourceDetails }

function ObjectTextToUTF8 (src : TStream) : UTF8String;
var
  ach : AnsiChar;
  outp : Integer;
  token : AnsiString;
  i : Integer;
begin
  SetLength (result, src.Size - src.Position);
  outp := 1;

  while src.Read (ach, 1) = 1 do
  begin
    while ach = '#' do
    begin
      token := '';
      repeat
        if src.Read (ach, 1) <> 1 then
          break;

        if not (ach in ['0'..'9']) then
          break;

        token := token + ach

      until false;
      if token = '' then
        break;

      token := UTF8Encode (WideChar (StrToInt (String (token))));
      for i := 1 to Length (token) do
      begin
        result [outp] := token [i];
        Inc (outp)
      end
    end;
    result [outp] := ach;
    Inc (outp)
  end;
  SetLength (result, outp-1)
end;

function TRCDataFormResourceDetails.GetText: UnicodeString;
var
  m : TMemoryStream;
  off : TStreamOriginalFormat;
  s : TStrings;
  st : string;
begin
  s := Nil;
  m := TMemoryStream.Create;
  try
    data.Seek (0, soFromBeginning);
    off := sofUnknown;
    ObjectBinaryToText (data, m, off);
    s := TStringList.Create;
    m.Seek(0, soFromBeginning);
    s.LoadFromStream(m);
    st := s.Text;
    result := UTF8ToUnicodeString (AnsiString (st))
  finally
    m.Free;
    s.Free
  end
end;

procedure TRCDataFormResourceDetails.SetText(const Value: UnicodeString);
var
  m, m1 : TMemoryStream;
  us : UTF8String;
begin
  m := Nil;

  us := Utf8Encode (Value);
  m1 := TMemoryStream.Create;
  m1.Write(us [1], Length (us));

  try
    m := TMemoryStream.Create;
    m1.Seek(0, soFromBeginning);
    ObjectTextToBinary (m1, m);
    ChangeData (m);

  finally
    m.Free;
    m1.Free;
  end
end;

class function TRCDataFormResourceDetails.SupportsRCData(
  const AName: UnicodeString; Size: Integer; data: Pointer): Boolean;
begin
  Result := (Size > 0) and (strlcomp (PAnsiChar (data), 'TPF0', 4) = 0);
end;

{ TRCDataCompressedBitmapResourceDetails }

constructor TRCDataCompressedBitmapResourceDetails.Create(
  AParent: TResourceModule; ALanguage: Integer; const AName,
  AType: UnicodeString; ASize: Integer; AData: pointer);
var
  delBitmap : TBitmapResourceDetails;
  ms : TMemoryStream;
begin
  inherited;
  delBitmap := TCompressedBitmapResourceDetails.CreateNew (Nil, ALanguage, AName);
  fDelegate := delBitmap;

  ms := TMemoryStream.Create;
  try
    ms.Write(AData^, ASize);
    delBitmap.ChangeData(ms);
  finally
    ms.Free
  end
end;

class function TRCDataCompressedBitmapResourceDetails.SupportsRCData(
  const AName: UnicodeString; Size: Integer; data: Pointer): Boolean;
var
  outBuffer : pointer;
  inSize, outSize : Integer;

  procedure ZDecompressPartial(const inBuffer: Pointer; inSize: Integer;
    out outBuffer: Pointer; out outSize: Integer);
  var
    zstream: TZStreamRec;
    delta: Integer;
  begin
    FillChar(zstream, SizeOf(TZStreamRec), 0);

    delta := (inSize + 255) and not 255;
    outSize := delta;

    GetMem(outBuffer, outSize);

    try
      zstream.next_in := inBuffer;
      zstream.avail_in := inSize;
      zstream.next_out := outBuffer;
      zstream.avail_out := outSize;

      if InflateInit_(zstream, ZLIB_VERSION, SizeOf(TZStreamRec)) < 0 then
        raise EZDecompressionError.Create ('ZLib error');

      if inflate (zstream, Z_NO_FLUSH) >= 0 then
        Inc (outSize, delta)
      else
        raise EZDecompressionError.Create ('ZLib error');

      inflateEnd (zstream);
    except
      outSize := 0;
      ReallocMem (outBuffer, 0);
      raise
    end
  end;

begin
  inSize := Size;
  if inSize > 1024 then
    inSize := 1024;

  result := False;
  try
    try
      outSize := 0;
      ZDecompressPartial (data, inSize, outBuffer, outSize);
      try
        result := (PAnsiChar (outBuffer)^ = AnsiChar ('B')) and
                  ((PAnsiChar (outBuffer) + 1)^ = AnsiChar ('M'));

      finally
        ReallocMem (outBuffer, 0)
      end
    except
      MessageBeep ($ffff);
    end;
  finally
  end;
end;

{ TCompressedBitmapResourceDetails }

procedure TCompressedBitmapResourceDetails.ChangeData(data: TMemoryStream);
var
  outb : Pointer;
  outs : Integer;
  ms : TMemoryStream;
begin
  data.Seek(0, soFromBeginning);
  ms := Nil;
  ZDecompress (data.Memory, data.Size, outb, outs);
  try
    ms := TMemoryStream.Create;
    ms.Write((PAnsiChar (outb) + sizeof (TBitmapFileHeader))^, outs - sizeof (TBitmapFileHeader));
    inherited ChangeData (ms);
  finally
    ms.Free;
    ReallocMem (outb, 0)
  end;
end;

constructor TCompressedBitmapResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: UnicodeString; ASize: Integer;
  AData: pointer);
begin
  fCompressedData := TMemoryStream.Create;
  inherited;
end;

constructor TCompressedBitmapResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const AName: UnicodeString);
begin
  fCompressedData := TMemoryStream.Create;
  inherited;
end;

destructor TCompressedBitmapResourceDetails.Destroy;
begin
  fCompressedData.Free;

  inherited;
end;

function TCompressedBitmapResourceDetails.GetData: TMemoryStream;
var
  ms, m : TMemoryStream;
  hdr :TBitmapFileHeader;
  outb : Pointer;
  outs : Integer;
begin
  ms := inherited GetData;
  fCompressedData.Clear;

  hdr.bfType :=$4D42;         // TBitmap.LoadFromStream requires a bitmapfileheader
  hdr.bfSize := ms.size;      // before the data...
  hdr.bfReserved1 := 0;
  hdr.bfReserved2 := 0;
  hdr.bfOffBits := sizeof (hdr);

  outb := Nil;
  m := TMemoryStream.Create;
  try
    m.Write(hdr, sizeof (hdr));
    m.Write(ms.Memory^, ms.Size);

    ZCompress (m.Memory, m.Size, outb, outs);
    fCompressedData.Write(outb^, outs)
  finally
    m.Free;
    ReallocMem (outb, 0);

  end;

  result := fCompressedData
end;

initialization
  RegisterResourceDetails (TRCDataDescriptionResourceDetails);
  RegisterResourceDetails (TRCDataPackagesResourceDetails);
  RegisterResourceDetails (TRCDataFormResourceDetails);
  RegisterResourceDetails (TRCDataCompressedBitmapResourceDetails);
finalization
  UnregisterResourceDetails (TRCDataDescriptionResourceDetails);
  UnregisterResourceDetails (TRCDataPackagesResourceDetails);
  UnregisterResourceDetails (TRCDataFormResourceDetails);
  UnregisterResourceDetails (TRCDataCompressedBitmapResourceDetails);
end.
