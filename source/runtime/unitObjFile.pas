unit unitOBJfile;

interface

uses
  Windows, Classes, SysUtils, unitPEFile, unitResourceDetails;

type
  TObjFile = class (TPEBase)
  private
    FLNames: TStrings;
    function IsValidCOFFMachineType (w: word): boolean;
    procedure DecodeCOFF (memory: pointer; size: Integer);
    procedure DecodeOMF (memory: pointer; size: Integer);
  protected
    function GetResourceCount: Integer; override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;

    procedure Decode (memory: pointer; exeSize: Integer); override;
  public
    destructor Destroy; override;
    function IndexOfResource (details: TResourceDetails): Integer; override;
  end;

implementation

{ TObjFile }

procedure TObjFile.Decode(memory: pointer; exeSize: Integer);
var
  w: word;
begin
  FSectionList.Clear;

  w := PWord (PByte (Memory))^;

  if IsValidCOFFMachineType (w) then
    DecodeCoff (memory, exeSize)
  else
    DecodeOMF (memory, exeSize)
end;

procedure TObjFile.DecodeCOFF(memory: pointer; size: Integer);
var
  i, Offset: Integer;
  SectionHeader: PImageSectionHeader;
  Sect: TImageSection;
begin
  if size < sizeof (TImageFileHeader) then
    raise Exception.Create ('End of data');

  Offset := 0;
  fCOFFHeader := PImageFileHeader (PByte (Memory) + Offset)^;

  if (fCOFFHeader.SizeOfOptionalHeader <> 0) or
     (fCOFFHeader.Characteristics <> 0)
  then
    raise Exception.Create('Invalid .OBJ file');

  DecodeStringTable (Memory);
  DecodeSymbolTable (Memory);

  Inc (Offset, sizeof (TImageFileHeader));

  for i := 0 to fCOFFHeader.NumberOfSections - 1 do
  begin
    SectionHeader := PImageSectionHeader (PByte (memory) + Offset);
    Sect := TImageSection.Create (self, SectionHeader^, PByte (memory), false);
    FSectionList.Add (Sect);
    Inc (Offset, sizeof (TImageSectionHeader));
  end;
end;

procedure TObjFile.DecodeOMF(memory: pointer; size: Integer);
var
  Offset: Integer;
  pmem: PByte;
  recordType: byte;
  recordLen: word;
  hasDWORDS: boolean;

  function GetString (var p: PByte; var len: Integer): string;
  var
    l: byte;
  begin
    if len = 3 then
      raise Exception.Create ('End of data');

    l := p^; Inc (p);

    if len < l then
      raise Exception.Create ('End of data');

    SetLength (Result, l);
    Move (p^, Result [1], l);
    Inc (p, l);
    Dec (len, 1 + l);
  end;

  function GetByte (var p: PByte; var len: Integer): byte;
  begin
    if len < sizeof (byte) then
      raise Exception.Create ('End of data');

    Result := p^;
    Inc (p);
    Dec (len);
  end;

  function GetWord (var p: PByte; var len: Integer): word;
  begin
    if len < sizeof (word) then
      raise Exception.Create ('End of data');

    Result := p^;
    Inc (p, sizeof (word));
    Dec (len, sizeof (word));
  end;

  function GetDWord (var p: PByte; var len: Integer): dword;
  begin
    if len < sizeof (dword) then
      raise Exception.Create ('End of data');

    Result := p^;
    Inc (p, sizeof (dword));
    Dec (len, sizeof (dword));
  end;

  function GetByteOrWord (var p: PByte; var len: Integer; _32BitNumbers: boolean): word;
  begin
    if  _32BitNumbers then
      Result := GetWord (p, len)
    else
      Result := GetByte (p, len)
  end;

  function GetWordOrDWORD (var p: PByte; var len: Integer; _32BitNumbers: boolean): DWORD;
  begin
    if  _32BitNumbers then
      Result := GetDWord (p, len)
    else
      Result := GetWord (p, len)
  end;

  procedure DecodeTHEADR (p: PByte; len: Integer);
  begin
  end;

  procedure DecodeLHEADR (p: PByte; len: Integer);
  begin
  end;

  procedure DecodeIMPDEF (p: PByte; len: Integer);
  var
    byOrd: boolean;
    internalName: string;
    moduleName: string;
  begin
    byOrd := GetByte (p, len) <> 0;
    internalName := GetString (p, len);
    moduleName := GetString (p, len);

    if byOrd = byOrd then

  end;

  procedure DecodeCOMENT (p: PByte; len: Integer);
  var
    comentType: Byte;
    comentClass: Byte;
    subType: Byte;
  begin
    if len < 3 then
      raise Exception.Create ('End of data');

    comentType := GetByte (p, len);
    comentClass := GetByte (p, len);

    case comentClass of
      $00 :;   // Translator
      $a0 :
        begin
          subType := GetByte (p, len);

          case subType of
            1: DecodeIMPDEF (p, len);
          end;

        end;

      $a2 :; // Linker pass 2 marker (???)
      $e8 :; // Source file
      $e9 :; // Dependency file

      else
        raise Exception.CreateFmt ('Invalid OMF COMENT class %x', [comentClass]);
    end;

    if comentType = comentType then

  end;

  procedure DecodeMODEND (p: PByte; len: Integer; _32BitNumbers: boolean);
  begin
  end;

  procedure DecodeEXTDEF (p: PByte; len: Integer);
  begin
  end;

  procedure DecodePUBDEF (p: PByte; len: Integer; _32BitNumbers: boolean);
  begin
  end;

  procedure DecodeLNAMES (p: PByte; len: Integer);
  var
    l: byte;
    ast: AnsiString;
  begin
    while len > 1 do
    begin
      l := GetByte (p, len);
      if l > len-1 then
        raise Exception.Create ('End of data');
      SetString (ast, PAnsiChar (p), l);
      Inc (p, l);Dec (len, l);
      if FLNames = Nil then
        FLNames := TStringList.Create;

      FLNames.Add (String (ast))
    end
  end;

  procedure DecodeSEGDEF (p: PByte; len: Integer; _32BitNumbers: boolean);
  var
    Sect: TImageSection;
    SectionHeader: PImageSectionHeader;
    A, C: byte;
    _B, _P: boolean;
    b, Offset: byte;
    FrameNumber, segNameIdx, clsNameIdx, ovlNameIdx: word;
    segLen: DWORD;
    op: PByte;
    sectName, clsName: AnsiString;

  begin
    op := p;
    b := GetByte (p, len);

    A := b shr 5;
    C := b shr 2 and 7;
    _B := (b and $2) <> 0;
    _P := (b and $1) <> 0;

    Offset := 0;
    FrameNumber := 0;

    if A = 0 then
    begin
      FrameNumber := GetWord (p, len);
      Offset := GetByte (p, len);
    end;

    segLen := GetWordOrDWORD (p, len, _32BitNumbers);
    segNameIdx := GetByteOrWord (p, len, _32BitNumbers);
    clsNameIdx := GetByteOrWord (p, len, _32BitNumbers);
    ovlNameIdx := GetByteOrWord (p, len, _32BitNumbers);

    GetMem (SectionHeader, sizeof (TImageSectionHeader));
    ZeroMemory (SectionHeader, sizeof (TImageSectionHeader));

    sectName := AnsiString (FLNames [segNameIdx-1]);
    lstrcpynA (PAnsiChar (@SectionHeader^.Name [0]), PAnsiChar (sectName), sizeof (SectionHeader^.Name));

    clsName := AnsiString (FLNames [clsNameIdx-1]);

    if SameText (String (clsName), 'CODE') then
      SectionHeader.Characteristics := IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_EXECUTE
    else
      if SameText (string (clsName), 'DATA') then
        SectionHeader.Characteristics := IMAGE_SCN_CNT_INITIALIZED_DATA
      else
        if SameText (string (clsName), 'STACK') then
          SectionHeader.Characteristics := IMAGE_SCN_CNT_UNINITIALIZED_DATA;

    Sect := TImageSection.Create (self, SectionHeader^, PByte (memory), false);
    FSectionList.Add (Sect);

    if ovlNameIdx = ovlNameIdx then;
    if segLen = segLen then;
    if Offset = Offset then;
    if FrameNumber = 0 then;
    if _P then;
    if _B then;
    if C = 0 then;
    if op = Nil then;
  end;

  procedure DecodeFIXUP (p: PByte; len: Integer; _32BitNumbers: boolean);
  begin
  end;

  procedure DecodeLEDATA (p: PByte; len: Integer; _32BitNumbers: boolean);
  var
    segIdx: word;
    enumOfs: dword;
    Sect: TImageSection;
  begin
    segIdx := GetByteOrWord (p, len, _32BitNumbers);
    enumOfs := GetWordOrDWORD (p, len, _32BitNumbers);

    Sect := ImageSection [segIdx-1];
    Sect.AddData(p^, len-1);

    if enumOfs = 0 then;

  end;

  procedure DecodeGRPDEF (p: PByte; len: Integer);
  begin
  end;

begin
  Offset := 0;
  pmem := PByte (memory);

  hasDWORDS := false;
  while Offset < size do
  begin
    recordType := pmem^;        Inc (pmem);  Inc (Offset);
    hasDWORDs := Odd (recordType);
    recordType := recordType and $fe;

    if size - Offset < sizeof (word) then
      raise Exception.Create ('End of data');

    recordLen := PWord (pmem)^; Inc (pmem, sizeof (word));

    case recordType of
      $80: DecodeTHEADR (pmem, recordLen);
      $82: DecodeLHEADR (pmem, recordLen);
      $88: DecodeCOMENT (pmem, recordLen);
      $8a,$8b :
        begin
          DecodeMODEND (pmem, recordLen, recordType = $8b);
          Break
        end;
      $8c: DecodeEXTDEF (pmem, recordLen);
      $90,
      $91: DecodePUBDEF (pmem, recordLen, recordType = $91);
      $96 :
            DecodeLNAMES (pmem, recordLen);
      $98,
      $99: DecodeSEGDEF (pmem, recordLen, recordType = $99);
      $9A: DecodeGRPDEF (pmem, recordlen);
      $9C,
      $9d: DecodeFIXUP  (pmem, recordLen, recordType = $9d);
      $A0,
      $A1: DecodeLEDATA (pmem, recordLen, recordType = $A1);
      else
        raise Exception.CreateFmt ('Invalid OMF record type %x', [recordType]);
    end;

    if size - Offset < recordLen then
      raise Exception.Create ('End of data');


    Inc (pmem, recordLen);
    Inc (Offset, recordLen)
  end;

  if hasDWORDS then;

end;

destructor TObjFile.Destroy;
begin
  FLNames.Free;

  inherited;
end;

function TObjFile.GetResourceCount: Integer;
begin
  Result := 0
end;

function TObjFile.GetResourceDetails(idx: Integer): TResourceDetails;
begin
  Result := Nil
end;

function TObjFile.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := -1
end;

const
  IMAGE_FILE_MACHINE_ARM       = $1c0;
  IMAGE_FILE_MACHINE_ALPHA64   = $284; // Alpha AXP 64-bit.
  IMAGE_FILE_MACHINE_IA64      = $200; // Intel IA64
  IMAGE_FILE_MACHINE_M68K      = $268; // Motorola 68000 series.
  IMAGE_FILE_MACHINE_MIPS16    = $266;
  IMAGE_FILE_MACHINE_MIPSFPU   = $366; // MIPS with FPU
  IMAGE_FILE_MACHINE_MIPSFPU16 = $466; // MIPS16 with FPU
  IMAGE_FILE_MACHINE_SH3       = $1a2; // Hitachi SH3
  IMAGE_FILE_MACHINE_SH4       = $1a6; // Hitachi SH4
  IMAGE_FILE_MACHINE_THUMB     = $1c2;

var
  ValidCOFFMachineTypes: array [0..16] of word = (

  { defined in Windows.Pas }
    IMAGE_FILE_MACHINE_UNKNOWN,
    IMAGE_FILE_MACHINE_I386,
    IMAGE_FILE_MACHINE_R3000,
    IMAGE_FILE_MACHINE_R4000,
    IMAGE_FILE_MACHINE_R10000,
    IMAGE_FILE_MACHINE_ALPHA,
    IMAGE_FILE_MACHINE_POWERPC,

    { defined above }
    IMAGE_FILE_MACHINE_ARM,
    IMAGE_FILE_MACHINE_ALPHA64,
    IMAGE_FILE_MACHINE_IA64,
    IMAGE_FILE_MACHINE_M68K,
    IMAGE_FILE_MACHINE_MIPS16,
    IMAGE_FILE_MACHINE_MIPSFPU,
    IMAGE_FILE_MACHINE_MIPSFPU16,
    IMAGE_FILE_MACHINE_SH3,
    IMAGE_FILE_MACHINE_SH4,
    IMAGE_FILE_MACHINE_THUMB
  );

function TObjFile.IsValidCOFFMachineType(w: word): boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low (ValidCOFFMachineTypes) to High (ValidCOFFMachineTypes) do
    if w = ValidCOFFMachineTypes [i] then
    begin
      Result := true;
      Break
    end
end;

end.
