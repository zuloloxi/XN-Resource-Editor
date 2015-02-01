(*======================================================================*
 | unitPEFile                                                           |
 |                                                                      |
 | Windows PE File Decoder unit                                         |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ----------------------------------------- |
 | 1.0      19/10/2000  CPWW  Original                                  |
 | 1.1      31/05/2001  CPWW  Fixed crash when no resource section.     |
 |                      CPWW  Fixed crash when 'VirtualSize' contains   |
 |                            0, and SizeOfRawData doesn't.             |
 *======================================================================*)

// A PE file looks like this...
//
//   [ DOS Header      ]     First word is 'MK'
//   [ COFF header     ]     Starts at DOSHdr._lfaNew.  First dword is COFF signature
//   [ Optional header ]     Follows COFF header.  First word is IMAGE_NT_OPTIONAL_HDR_MAGIC
//   [ Data Directory  ]     Really part of the optional header
//   [ Image Sections Headers ] Starts at optionalHeader + COFFHdr.SizeOfOptionalHeader
//   [ Mystery padding]
//   [ Section data]         Each one pointed to by it's Image Section Header

unit unitPEFile;

interface

uses Windows, Classes, SysUtils, ConTnrs, unitResourceDetails, ImageHlp;

const
  PE_LINKER_VERSION_HI = 1;
  PE_LINKER_VERSION_LO = 0;

  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;

  NUM_DATA_DIRECTORIES = $10;  // (Pietrick) "At the end of the IMAGE_NT_HEADERS
                               // structure is an array of IMAGE_DATA_DIRECTORY
                               // structures. This field contains the number of
                               // entries in the array. This field has been 16
                               // since the earliest releases of Windows NT."


// Relocation types from 'Microsoft Portable Executable and Common Object File Format Specification'
  IMAGE_REL_I386_ABSOLUTE = $0; // Ignored
  IMAGE_REL_I386_DIR32 = $6;    // Target's 32 bit virtual address
  IMAGE_REL_I386_DIR32NB = $7;  // Target's 32-bit relative virtual address
  IMAGE_REL_I386_REL32 = $14;    // 32-bit displacement relative to the target - eg call .+$7

type

TPEBase = class;

TSectionReloc = packed record   // Relocations in sections (obj files only)
  virtualAddress : DWORD;
  symbolTableIndex : DWORD;
  _type : word;                 // See IMAGE_REL_I386 constants above
end;
PSectionReloc = ^TSectionReloc;

//----------------------------------------------------------------------
// TImageSection class

TImageSection = class
private
  fParent: TPEBase;
  fSectionHeader : TImageSectionHeader;
  fUninitializedDataSize : Integer;
  fDirectoryEntry : DWORD;
  fRelocs : array of TSectionReloc;

  function GetSectionName: AnsiString;
  function GetContainsCode: boolean;
  function GetReloc (idx : Integer): TSectionReloc;
  function GetRelocCount: Integer;
protected
  fRawData : TMemoryStream;
public
  // Changed 'data' to be the base, rather than the pointer to raw data.  Added temporary tst
  // parameter to catch that use the old method & have to be modified
  constructor Create (AParent : TPEBase; const AHeader : TImageSectionHeader; data : pbyte; tst : boolean);
  constructor CreateEmpty (AParent : TPEBase; const AName : AnsiString; const ACharacteristics, ADirectoryEntry : DWORD);
  destructor Destroy; override;
  procedure Initialize; virtual;

  procedure AddData (const data; len : Integer);
  function GetRelocPtr : PSectionReloc;

  procedure Fixup; virtual;

  property Parent : TPEBase read fParent;

  property SectionName : AnsiString read GetSectionName;
  property SectionHeader : TImageSectionHeader read fSectionHeader;
  property RawData : TMemoryStream read fRawData;
  property ContainsCode : boolean read GetContainsCode;
  property RelocCount : Integer read GetRelocCount;
  property Reloc [idx : Integer] : TSectionReloc read GetReloc;
end;

TImageImportDescriptor = packed record
  Characteristics : DWORD; // This is an RVA to a list of pointers. Each of these points to there function name
  TimeDateStamp : DWORD;   // The time/date stamp indicating when the file was built
  ForwarderChain : DWORD;  // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
  Name : DWORD;            // This is an RVA to a NULL-terminated ASCII string containing the imported DLL's name
  FirstThunk : DWORD;      //  Another RVA to a list pointers. Each of these points to their function name
end;
PImageImportDescriptor = ^TImageImportDescriptor;

TRawSymbolName = packed record case boolean of
  false : (shortName : array [0..7] of AnsiChar);
  true : (zeros, offset : DWORD);
end;

TRawSymbol = packed record
  name : TRawSymbolName;
  value : DWORD;
  sectionNumber : word;
  _type : word;
  storageClass : byte;
  numberOfAuxSymbols : byte;
end;
PRawSymbol = ^TRawSymbol;

TSymbol = class
private
  fName : AnsiString;
  fValue : DWORD;
  fSectionNumber : word;
  fType : word;
  fStorageClass : byte;
  fIndex : DWORD;
public
  constructor Create (const ASymbolName : AnsiString; AIndex, AValue : DWORD; ASectionNumber, AType : word; AStorageClass : byte);

  property Name : AnsiString read fName;
  property Value : DWORD read fValue;
  property SectionNumber: word read fSectionNumber;
  property SectionType : word read fType;
  property StorageClass : byte read fStorageClass;
end;

TPEBase = class (TResourceModule)
private

  function GetCOFFHeader: TImageFileHeader;
  function GetImageSection(index: Integer): TImageSection;
  function GetImageSectionCount: Integer;
  function GetSymbol(idx: Integer): TSymbol;
  function GetSymbolCount: Integer;
  function GetStringTableCount: Integer;
  function GetStringTableEntry(idx: Integer): AnsiString;
  function GetSymbolTableIndex: TList;

  procedure ClearSymbolTable;
protected
  fCOFFHeader : TImageFileHeader;
  fSectionList : TObjectList;                   // List of TImageSection objects
  fSymbolTable : TList;
  fSymbolTableIndex : TList;
  fStringTable : TStrings;

  function GetFirstSectionWithCharacteristics (characteristicsMask : DWORD) : TImageSection;
  function GetSectionSize (characteristicsMask : DWORD) : Integer;


  procedure ApplyGlobalFixups; virtual;
  procedure DecodeStringTable (memory : pointer);
  procedure DecodeSymbolTable (memory : pointer);
  procedure Decode (memory : pointer; size : Integer); virtual;
  procedure Encode; virtual;
public
  constructor Create;
  destructor Destroy; override;

  function GetCodeSize : Integer;
  function GetIDataSize : Integer;
  function GetUDataSize : Integer;

  function GetSectionByName (const name : AnsiString) : TImageSection;

  procedure LoadFromStream (s : TStream); override;
  procedure LoadFromFile (const name : string); override;

  property COFFHeader : TImageFileHeader read GetCOFFHeader;
  property ImageSectionCount : Integer read GetImageSectionCount;
  property ImageSection [index : Integer] : TImageSection read GetImageSection;

  property SymbolCount : Integer read GetSymbolCount;
  property Symbol [idx : Integer] : TSymbol read GetSymbol;
  property SymbolTableIndex : TList read GetSymbolTableIndex;

  property StringTableCount : Integer read GetStringTableCount;
  property StringTableEntry [idx : Integer] : AnsiString read GetStringTableEntry;
end;

//----------------------------------------------------------------------
// TPEModule class

TPEModule = class (TPEBase)
private
  fDOSStub : TMemoryStream;
  fCommentBlock : PByte;
  fCommentSize : Integer;
  fEndComment : PByte;
  fEndCommentSize : Integer;

  function GetOptionalHeader: TImageOptionalHeader;
  function GetDataDirectory(index: Integer): PImageDataDirectory;
  function GetDataDirectoryCount: Integer;
  function GetDOSHeader: TImageDosHeader;
  function GetExportCount: Integer;
  function GetImportCount: Integer;
  function GetResourceSection (var offset : Integer) : TImageSection;
  function GetImportSection (var offset : Integer): TImageSection;
  function GetExportSection (var offset : Integer): TImageSection;
  function GetImport(idx: Integer): PImageImportDescriptor;
  function GetImportSectionData: PByte;
  function GetExportSectionData: PByte;

protected
  fDOSHeader : TImageDosHeader;
  fOptionalHeader : PImageOptionalHeader;

  procedure ApplyGlobalFixups; override;
  procedure Decode (memory : pointer; exeSize : Integer); override;
  procedure Encode; override;

  property OptionalHeaderPtr : PImageOptionalHeader read fOptionalHeader;
public
  constructor Create;
  destructor Destroy; override;

  procedure GetExportDetails (idx : Integer; var name : AnsiString; var ordinal : DWORD);
  procedure SaveToStream(s: TStream); override;

  function FindDirectoryEntrySection (entryNo : Integer; var offset : Integer): Integer;

  property DOSHeader : TImageDosHeader read GetDOSHeader;
  property DOSStub : TMemoryStream read fDOSStub;
  property OptionalHeader : TImageOptionalHeader read GetOptionalHeader;

  property DataDirectoryCount : Integer read GetDataDirectoryCount;
  property DataDirectory [index : Integer] : PImageDataDirectory read GetDataDirectory;

  property ImportCount : Integer read GetImportCount;
  property Import [idx : Integer] : PImageImportDescriptor read GetImport;
  property ImportSectionData : PByte read GetImportSectionData;
  property ExportSectionData : PByte read GetExportSectionData;
  property ExportCount : Integer read GetExportCount;
end;

//----------------------------------------------------------------------
// TResourceDirectoryTable record

TResourceDirectoryTable = packed record
  characteristics : DWORD; // Resource flags, reserved for future use; currently set to zero.
  timeDateStamp : DWORD;   // Time the resource data was created by the resource compiler.
  versionMajor : WORD;     // Major version number, set by the user.
  versionMinor : WORD;     // Minor version number.
  cNameEntries : WORD;     // Number of directory entries, immediately following the table, that use strings to identify Type, Name, or Language (depending on the level of the table).
  cIDEntries : WORD;       // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
end;
PResourceDirectoryTable = ^TResourceDirectoryTable;

//----------------------------------------------------------------------
// TPEModule record

TResourceDirectoryEntry = packed record
  name : DWORD;         // RVA Address of integer or string that gives the Type, Name, or Language identifier, depending on level of table.
  RVA : DWORD;          // RVA High bit 0. Address of a Resource Data Entry (a leaf).
                        // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table (the next level down).
end;
PResourceDirectoryEntry = ^TResourceDirectoryEntry;

//----------------------------------------------------------------------
// TResourceDirectoryEntry record

TResourceDataEntry = packed record
  OffsetToData : DWORD;
  Size : DWORD;
  CodePage : DWORD;
  Reserved : DWORD;
end;
PResourceDataEntry = ^TResourceDataEntry;

//----------------------------------------------------------------------
// TPEResourceModule class

TPEResourceModule = class (TPEModule)
private
  fDetailList : TObjectList;             // List of TResourceDetails objects

protected
  procedure Decode (memory : pointer; exeSize : Integer); override;
  procedure Encode; override;
  function GetResourceCount: Integer;  override;
  function GetResourceDetails(idx: Integer): TResourceDetails; override;
public
  constructor Create;
  destructor Destroy; override;


  property ResourceCount : Integer read GetResourceCount;
  property ResourceDetails [idx : Integer] : TResourceDetails read GetResourceDetails;
  procedure DeleteResource (resourceNo : Integer); override;
  procedure InsertResource (idx : Integer; details : TResourceDetails); override;
  function AddResource (details : TResourceDetails) : Integer; override;
  function IndexOfResource (details : TResourceDetails) : Integer; override;
  procedure SortResources; override;
end;


EPEException = class (Exception);

implementation

{ TPEModule }
resourcestring
  rstInvalidDOSSignature   = 'Invalid DOS signature';
  rstInvalidCOFFSignature  = 'Invalid COFF signature';
  rstInvalidOptionalHeader = 'Invalid Windows Image';
  rstBadDirectoryIndex     = 'Index exceeds data directory count';
  rstBadLangID             = 'Unsupported non-integer language ID in resource';
  rstEncode                = 'Error encoding module';

type
  TResourceNode = class;

  TRNode = record
    id : AnsiString;
    intID : boolean;
    case leaf : boolean of
      false : (next : TResourceNode);
      true : (data : TMemoryStream; CodePage : DWORD);
  end;

  TResourceNode = class
    count : Integer;
    nodes : array of TRNode;

    constructor Create (const AType, AName : UnicodeString; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    constructor CreateNameNode (const AName : UnicodeString; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    constructor CreateLangNode (ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure Add (const AType, AName : UnicodeString; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure AddName (const AName : UnicodeString; ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    procedure AddLang (ALang : Integer; aData : TMemoryStream; CodePage : DWORD);
    function IsID (idx : Integer): boolean;
    destructor Destroy; override;
  end;

procedure MoveString (const src : AnsiString; var dst);
begin
  if Length (src) > 0 then
    Move (src [1], dst, Length (src));
end;

procedure TPEModule.ApplyGlobalFixups;
begin
// stub
end;

(*----------------------------------------------------------------------*
 | constructor PEModule.Create                                          |
 |                                                                      |
 | Constructor for TPEModule instance.  Create empty section list       |
 *----------------------------------------------------------------------*)

constructor TPEModule.Create;
begin
  inherited Create;
  fDOSStub := TMemoryStream.Create;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Decode                                            |
 |                                                                      |
 | Decode the PE file.  Load the DOS header, the COFF header and the    |
 | 'optional' header, then load each section into fSectionList          |
 *----------------------------------------------------------------------*)
procedure TPEModule.Decode (Memory : pointer; exeSize : Integer);
var
  offset : LongInt;
  i : Integer;
  sectionHeader : PImageSectionHeader;
  commentOffset, firstSectionOffset : Integer;
begin
  fSectionList.Clear;

                                // Check it's really a PE file.
  if PWORD (Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create (rstInvalidDOSSignature);

                                // Load the DOS header
  fDOSHeader := PImageDosHeader (Memory)^;

  offset := fDOSHeader._lfanew;
  fDOSStub.Write ((PByte (Memory) + sizeof (fDOSHeader))^, fDOSHeader._lfanew - sizeof (fDOSHeader));

                                // Check the COFF signature
  if PDWORD (PByte (Memory) + offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create (rstInvalidCOFFSignature);

                                // Load the COFF header
  Inc (offset, sizeof (DWORD));
  fCOFFHeader := PImageFileHEader (PByte (Memory) + offset)^;

  Inc (offset, sizeof (fCOFFHeader));

                                // Check the Optional Header signature.  nb
                                // the optional header is compulsory for
                                // 32 bit windows modules!
  if PWORD (PByte (Memory) + offset)^ <> IMAGE_NT_OPTIONAL_HDR_MAGIC then
    raise EPEException.Create (rstInvalidOptionalHeader);

                                // Save the 'optional' header
  ReallocMem (fOptionalHeader, fCOFFHeader.SizeOfOptionalHeader);
  Move ((PByte (Memory) + Offset)^, fOptionalHeader^, fCOFFHeader.SizeOfOptionalHeader);

  Inc (offset, fCOFFHeader.SizeOfOptionalHeader);

  commentOffset := offset + fCOFFHeader.NumberOfSections * sizeof (TImageSectionHeader);

  sectionHeader := PImageSectionHeader (PByte (memory) + offset);
  firstSectionOffset := 0;

  for i := 0 to fCoffHeader.NumberOfSections - 1 do
  begin
    if firstSectionOffset = 0 then
      firstSectionOffset := sectionHeader^.PointerToRawData
    else
      if (sectionHeader^.PointerToRawData > 0) and (sectionHeader^.PointerToRawData < DWORD (firstSectionOffset)) then
        firstSectionOffset := sectionHeader^.PointerToRawData;

    Inc (sectionHeader);
  end;

// Save padding between the end of the section headers, and the start of the
// 1st section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  fCommentSize := firstSectionOffset - commentOffset;

  if fCommentSize > 0 then
  begin
    GetMem (fCommentBlock, fCommentSize);
    Move ((PByte (memory) + commentOffset)^, fCommentBlock^, fCommentSize)
  end;

  DecodeStringTable (Memory);
  DecodeSymbolTable (Memory);

                                // Now save each image section in the fSectionList
  for i := 0 to fCOFFHeader.NumberOfSections - 1 do
  begin
    sectionHeader := PImageSectionHeader (PByte (memory) + offset);
    fSectionList.Add (TImageSection.Create (self, sectionHeader^, PByte (memory), false));
    Inc (offset, sizeof (TImageSectionHeader));
  end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

// Save the padding between the last section and the end of the file.
// This appears to hold debug info and things ??

  fEndCommentSize := exeSize - i;
  if fEndCommentSize > 0 then
  begin
    GetMem (fEndComment, fEndCommentSize);
    Move ((PByte (memory) + i)^, fEndComment^, fEndCommentSize)
  end
end;

(*----------------------------------------------------------------------*
 | destructor PEModule.Destroy                                          |
 |                                                                      |
 | Destructor for TPEModule instance.                                   |
 *----------------------------------------------------------------------*)
destructor TPEModule.Destroy;
begin
  ReallocMem (fOptionalHeader, 0);
  fDOSStub.Free;
  ReallocMem (fCommentBlock, 0);
  ReallocMem (fEndComment, 0);
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Encode                                            |
 |                                                                      |
 | Fix up the data prior to writing to stream.                          |
 |                                                                      |
 | Ensure that the headers match what we've got...                      |
 *----------------------------------------------------------------------*)
procedure TPEModule.Encode;
var
  offset : DWORD;
  i : Integer;
  section : TImageSection;
  align : Integer;
  addrAlign : Integer;
  address : Integer;
  alignedSize, AddrAlignedSize : Integer;
  codeSize, iDataSize, uDataSize, iSize, vs : Integer;
begin
  codeSize := 0;
  iDataSize := 0;
  uDataSize := 0;
                                               // Use the DOS stub from their .EXE
  fDOSHeader._lfanew := sizeof (fDosHeader) + fDOSStub.Size;

                                               // Fixup sections count
  fCOFFHeader.NumberOfSections := 0;

  iSize :=  fDOSHeader._lfanew +               // File offset for start of sections
            SizeOf (DWORD) +                   // NT signature
            sizeof (fCoffHeader) +
            fCOFFHeader.SizeOfOptionalHeader +
            fSectionList.Count * sizeof (TImageSectionHeader);

  offset := iSize + fCommentSize;

  align := fOptionalHeader^.FileAlignment;
  addrAlign := fOptionalHeader^.SectionAlignment;

  address := addrAlign;
  offset := DWORD ((integer (offset) + align - 1) div align * align);

                                                // First section starts at $1000 (when loaded)
                                                // and at 'offset' in file.

  fOptionalHeader^.SizeOfHeaders := DWORD ((integer (iSize) + align - 1) div align * align);

  fOptionalHeader^.BaseOfCode := $ffffffff;
  fOptionalHeader^.CheckSum := 0;               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize  := DWORD ((integer (iSize) + addrAlign - 1) div addrAlign * addrAlign);

  for i := 0 to fSectionList.Count - 1 do      // Recalculate the section offsets
  begin
    section := TImageSection (fSectionList [i]);

    section.fSectionHeader.PointerToRawData := offset;
    section.fSectionHeader.VirtualAddress := address;

  // Apply fixups to the section (Just a stub - only used by derived classes)
    section.Fixup;

// Virtual size is size of data in memory, and is not padded to an 'alignment'.
//
// SizeOfRawData is size of data in file, padded to (file) alignment.
//
// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.
//
// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with zeros when it's loaded.
//
// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.
//
// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    vs := section.fRawData.Size + section.fUninitializedDataSize;
    section.fSectionHeader.Misc.VirtualSize := vs;

    if vs <> 0 then
      Inc (fCOFFHeader.NumberOfSections);

    section.fSectionHeader.SizeOfRawData := (section.fRawData.Size + align - 1) div align * align;

    if (section.fDirectoryEntry <> $ffffffff) and (vs <> 0) then
    begin
      fOptionalHeader^.DataDirectory [section.fDirectoryEntry].VirtualAddress := address;
      fOptionalHeader^.DataDirectory [section.fDirectoryEntry].Size := section.fSectionHeader.Misc.VirtualSize
    end;

    alignedSize := (Integer (vs) + align - 1) div align * align;
    addrAlignedSize := (Integer (vs) + addrAlign - 1) div addrAlign * addrAlign;

    if (section.fSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
    begin
      Inc (codeSize, alignedSize);
      if DWORD (address) < fOptionalHeader^.BaseOfCode then
        fOptionalHeader^.BaseOfCode := address;
    end
    else
      if (section.fSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
        Inc (iDataSize, alignedSize)
      else
        if (section.fSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
          Inc (uDataSize, alignedSize);

    Inc (iSize, addrAlignedSize);
    Inc (offset, section.fSectionHeader.SizeOfRawData);
    Inc (address, (Integer (vs) + addrAlign - 1) div addrAlign * addrAlign);
  end;

  // Apply fixups to the code section (This function's just a stub - used in
  // derived classes)
  ApplyGlobalFixups;

  fOptionalHeader^.SizeOfCode := codeSize;
  fOptionalHeader^.SizeOfInitializedData := iDataSize;
  fOptionalHeader^.SizeOfUninitializedData := uDataSize;

  i := SizeOf (DWORD) +                   // NT signature
       sizeof (fCoffHeader) +
       fCOFFHeader.SizeOfOptionalHeader +
       codeSize;

  i := (i + addrAlign - 1) div addrAlign * addrAlign;

  // With explorer.exe, codeSize is $14800, i is 148E8, so aligned 'i' is $15000
  // .. so BaseOfData should be $15000 + BaseOfCode ($1000) = $16000.
  //
  // ... but it's not - it's $15000, which means that the last $8e8 bytes of code
  // should be stampled over by the data!
  //
  // But obviously explorer.exe works, so I'm, missing a trick here.  Never mind - it
  // doesn't do any harm making it $16000 instead, and the formula works for everything
  // else I've tested...

  fOptionalHeader^.BaseOfData := fOptionalHeader.BaseOfCode + DWORD (i);

  fOptionalHeader^.SizeOfImage := iSize;

  if fOptionalHeader.AddressOfEntryPoint = 0 then
  begin
    Section := GetFirstSectionWithCharacteristics (IMAGE_SCN_CNT_CODE);
    if Section <> Nil then
      fOptionalHeader.AddressOfEntryPoint := Section.fSectionHeader.VirtualAddress
  end;
end;

(*----------------------------------------------------------------------*
 | function PEModule.FindDirectoryEntrySection                         |
 |                                                                      |
 | Return the index of the specified section.  The 'entryNo' to find    |
 | should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in         |
 | Windows.pas.                                                         |
 *----------------------------------------------------------------------*)
function TPEModule.FindDirectoryEntrySection (entryNo: Integer; var offset : Integer): Integer;
var
  i : Integer;
  p : PImageDataDirectory;
begin
  result := -1;
  p := DataDirectory [entryNo];
                                // Find section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (p^.VirtualAddress >= ImageSection [i].fSectionHeader.VirtualAddress) and (p^.VirtualAddress < ImageSection [i].fSectionHeader.VirtualAddress + ImageSection [i].fSectionHeader.Misc.VirtualSize) then
    begin
      if p^.Size <> 0 then
      begin
        result := i;
        offset := p^.VirtualAddress - ImageSection [i].fSectionHeader.VirtualAddress;
      end;
      break
    end
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDirectory                                 |
 |                                                                      |
 | Return the data dictionary for a specified                           |
 | IMAGE_DIRECTORY_ENTRY_xxxx  index                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDirectory(index: Integer): PImageDataDirectory;
var
  p : PImageDataDirectory;
begin
  if index < DataDirectoryCount then
  begin
    p := @fOptionalHeader.DataDirectory [0];
    Inc (p, index);
    result := p
  end
  else
    raise ERangeError.Create (rstBadDirectoryIndex);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDirectoryCount                            |
 |                                                                      |
 | Return no of entries in the Data Directory                           |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDirectoryCount: Integer;
begin
  result := fOptionalHeader^.NumberOfRvaAndSizes
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDosHeader                                      |
 |                                                                      |
 | Return DOS header                                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  result := fDOSHeader;
end;

function TPEModule.GetExportCount: Integer;
var
  ExportSection : PImageExportDirectory;
  section : TImageSection;
  offset : Integer;
begin
  section := GetExportSection (offset);
  if Assigned (section) then
  begin
    ExportSection := PImageExportDirectory (PByte (section.fRawData.memory) + offset);
    result := ExportSection^.NumberOfNames
  end
  else
    result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var name: AnsiString;
  var ordinal: DWORD);
var
  ExportSection : PImageExportDirectory;
  section : TImageSection;
  offset : Integer;
  po : DWORD;
  pw : PWORD;
  p : PDWORD;
  data : PByte;
begin
  section := GetExportSection (offset);
  if Assigned (section) then
  begin
    data := GetExportSectionData;
    ExportSection := PImageExportDirectory (PByte (section.fRawData.memory) + offset);
    po := DWORD (ExportSection^.AddressOfNameOrdinals);
    pw := PWORD (Data + po);
    Inc (pw, idx);
    ordinal := pw^;

    po := DWORD (ExportSection^.AddressOfNames);
    p := PDWORD (Data + po);
    Inc (p, idx);
    name := PAnsiChar (data) + p^
  end
end;

function TPEModule.GetExportSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  offset := 0;
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_EXPORT, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

function TPEModule.GetExportSectionData: PByte;
var
  section : TImageSection;
  offset : Integer;
begin
  section := GetExportSection (offset);
  result := PByte (section.fRawData.Memory) - section.fSectionHeader.VirtualAddress;
end;

function DirValid (dir : PImageImportDescriptor) : boolean;
begin
  DirValid := (dir^.Characteristics <> 0) or (dir^.TimeDateStamp <> 0) or (dir^.ForwarderChain <> 0) or
              (dir^.Name <> 0) or (dir^.FirstThunk <> 0)
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Get the optional header                                              |
 *----------------------------------------------------------------------*)

function TPEModule.GetImport(idx: Integer): PImageImportDescriptor;
var
  ImportSection : PImageImportDescriptor;
  section : TImageSection;
  offset : Integer;

begin
  section := GetImportSection (offset);
  result := Nil;
  if Assigned (section) then
  begin
    ImportSection := PImageImportDescriptor (PByte (section.fRawData.memory) + offset);

    while DirValid (ImportSection) and (idx > 0) do
    begin
      Inc (ImportSection);
      Dec (idx)
    end;

    if DirValid (ImportSection) then
      result := ImportSection
  end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection : PImageImportDescriptor;
  section : TImageSection;
  offset : Integer;
begin
  section := GetImportSection (offset);
  result := 0;
  if Assigned (section) then
  begin
    ImportSection := PImageImportDescriptor (PByte (section.fRawData.memory) + offset);

    while DirValid (ImportSection) do
    begin
      Inc (result);
      Inc (ImportSection)
    end
  end
end;

function TPEModule.GetImportSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_IMPORT, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

function TPEModule.GetImportSectionData: PByte;
var
  section : TImageSection;
  offset : Integer;
begin
  section := GetImportSection (offset);
  result := PByte (section.fRawData.Memory) - section.fSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TImageOptionalHeader;
begin
  result := fOptionalHeader^
end;

function TPEModule.GetResourceSection (var offset : Integer): TImageSection;
var
  idx : Integer;
begin
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, offset);
  if idx = -1 then
    result := Nil
  else
    result := ImageSection [idx]
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToStream                                     |
 |                                                                      |
 | Save the module to a stream                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.SaveToStream(s: TStream);
var
  NTSignature : DWORD;
  i : Integer;
  section : TImageSection;
  paddingSize, paddingLen : Integer;
  padding : PByte;
  f : TMemoryStream;
  oldCheckSum, newCheckSum : DWORD;
  ntHeaders : PImageNTHEaders;
  ckOffset : DWORD;
begin
  Encode;                       // Encode the data.

  NTSignature := IMAGE_NT_SIGNATURE;

                                // Write the DOS stub
  s.Write (fDOSHeader, sizeof (fDOSHeader));
  s.CopyFrom (fDOSStub, 0);

                                // Write NT sig and COFF header
  s.Write (NTSignature, sizeof (NTSignature));
  s.Write (fCOFFHeader, sizeof (fCOFFHeader));
  ckOffset := s.Position + Integer (@fOptionalHeader^.CheckSum) - Integer (@fOptionalHeader^);
  s.Write (fOptionalHeader^, fCOFFHeader.SizeOfOptionalHeader);

                                // Write the section headers
  for i := 0 to fSectionList.Count - 1 do
  begin
    section := TImageSection (fSectionList [i]);
    if section.fSectionHeader.Misc.VirtualSize <> 0 then
      s.Write (section.fSectionHeader, sizeof (section.fSectionHeader))
  end;

  if fCommentSize > 0 then      // Save the 'comment' section.  See 'Decode' for details
    s.Write (fCommentBlock^, fCommentSize);

                                // Write the sections
  padding := Nil;
  paddingLen := 0;
  try
    for i := 0 to fSectionList.Count - 1 do
    begin
                                // Write padding up to file offset of the section
      section := TImageSection (fSectionList [i]);

      if section.fSectionHeader.Misc.VirtualSize = 0 then
        Continue;

      paddingSize := section.fSectionHeader.PointerToRawData - DWORD (s.Position);

      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory (padding, paddingLen);
      end;

      if paddingSize > 0 then   // Put our signature at the start of the first
          s.Write (padding^, paddingSize);

                                // Write the section data.
      s.CopyFrom (section.fRawData, 0);

                                // Write data
      with section.fSectionHeader do
        paddingSize := SizeOfRawData - misc.VirtualSize;

                                // Pad data
      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory (padding, paddingLen);
      end;

      if paddingSize > 0 then
        s.Write (padding^, paddingSize)

    end;

    if fEndCommentSize > 0 then  // Save the debug info.
      s.Write (fEndComment^, fEndCommentSize)
  finally
    ReallocMem (padding, 0)
  end;

  f := TMemoryStream.Create;    // Now calculate the checksum....
  try
    s.Seek (0, soFromBeginning);
    f.LoadFromStream (s);
    ntHeaders := ChecksumMappedFile (f.Memory, f.Size, @oldCheckSum, @newCheckSum);

    if Assigned (ntHeaders) then
    begin
      s.Seek (ckOffset, soFromBeginning);
      s.Write (newChecksum, sizeof (newChecksum))
    end
  finally
    f.Free
  end;

  s.Seek (0, soFromEnd);
end;


{ TImageSection }

procedure TImageSection.AddData(const data; len: Integer);
begin
  RawData.Write(data, len);
  fSectionHeader.SizeOfRawData := fSectionHeader.SizeOfRawData + DWORD (len)
end;

(*----------------------------------------------------------------------*
 | constructor TImageSection.Create                                     |
 |                                                                      |
 | Constructor for TImageSection.                                       |
 *----------------------------------------------------------------------*)
constructor TImageSection.Create(AParent: TPEBase;
  const AHeader : TImageSectionHeader; data : pbyte; tst : boolean);
var
  i : Integer;
  psr : PSectionReloc;
  sectionData : PByte;
begin
  fSectionHeader := AHeader;
  fDirectoryEntry := $ffffffff;
  fRawData := TMemoryStream.Create;

  sectionData := data;
  Inc (sectionData, AHeader.PointerToRawData);

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if fSectionHeader.Misc.VirtualSize <= fSectionHeader.SizeOfRawData then
  begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if fSectionHeader.Misc.VirtualSize = 0 then
      fSectionHeader.Misc.VirtualSize := fSectionHeader.SizeOfRawData;
    fRawData.Write (sectionData^, fSectionHeader.Misc.VirtualSize)
  end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
  begin
    fRawData.Write (sectionData^, fSectionHeader.SizeOfRawData);
    fUninitializedDataSize := fSectionHeader.Misc.VirtualSize - fSectionHeader.SizeOfRawData;
  end;

  SetLength (fRelocs, fSectionHeader.NumberOfRelocations);
  psr := PSectionReloc (DWORD (data) + fSectionHeader.PointerToRelocations);
  for i := 0 to fSectionHeader.NumberOfRelocations - 1 do
  begin
    fRelocs [i] := psr^;
    Inc (psr)
  end;


  fParent := AParent;

  Initialize
end;

(*----------------------------------------------------------------------*
 | function TImageSection.GetSectionName                                |
 |                                                                      |
 | Return the section name - eg. .data                                  |
 *----------------------------------------------------------------------*)
function TImageSection.GetContainsCode: boolean;
begin
  result := SectionHeader.Characteristics and (IMAGE_SCN_CNT_CODE) <> 0;
end;

function TImageSection.GetReloc (idx : Integer): TSectionReloc;
begin
  result := fRelocs [idx]
end;

function TImageSection.GetRelocCount: Integer;
begin
  result := Length (fRelocs)
end;

function TImageSection.GetRelocPtr: PSectionReloc;
begin
  result := @fRelocs [0];
end;

function TImageSection.GetSectionName: AnsiString;
begin
  SetString (result, PAnsiChar (@fSectionHeader.Name), sizeof (fSectionHeader.Name));
  result := PAnsiChar (result);
end;

procedure TImageSection.Initialize;
begin
// stub
end;

(*----------------------------------------------------------------------*
 | destructor TImageSection.Destroy                                     |
 |                                                                      |
 | destructor for TImageSection.                                        |
 *----------------------------------------------------------------------*)
constructor TImageSection.CreateEmpty(AParent: TPEBase; const AName: AnsiString;
  const ACharacteristics, ADirectoryEntry: DWORD);
begin
  fParent := AParent;
  fRawData := TMemoryStream.Create;
  FillChar (fSectionHeader, sizeof (fSectionHeader), 0);
  MoveString (AName, fSectionHeader.Name);
  fSectionHeader.Characteristics := ACharacteristics;
  fDirectoryEntry := ADirectoryEntry;
  Initialize
end;

destructor TImageSection.destroy;
begin
  fRawData.Free;
  inherited;
end;

procedure TImageSection.Fixup;
begin
// stub
end;

{ TPEResourceModule }

(*----------------------------------------------------------------------*
 | procedure TPEResourceModule.DeleteResource                           |
 |                                                                      |
 | Delete the specified resource (by index)                             |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.DeleteResource(resourceNo: Integer);
var
  res : TResourceDetails;
begin
  res := ResourceDetails [resourceNo];
  inherited;
  resourceNo := IndexOfResource (Res);
  if resourceNo <> -1 then
    fDetailList.Delete (resourceNo);
end;

(*----------------------------------------------------------------------*
 | constructor TPEResourceModule.Create                                 |
 |                                                                      |
 | Constructor for TPEResourceModule                                    |
 *----------------------------------------------------------------------*)
constructor TPEResourceModule.Create;
begin
  inherited Create;
  fDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TPEResourceModule.Destroy                                 |
 |                                                                      |
 | Destructor for TPEResourceModule                                     |
 *----------------------------------------------------------------------*)
destructor TPEResourceModule.Destroy;
begin
  fDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Decode                                    |
 |                                                                      |
 | Decode the section's resource tree into a list of resource details   |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Decode;
var
  section : TImageSection;
  tp, name : string;
  lang : Integer;
  offset : Integer;

  // Get string resource name
  function GetResourceStr (IdorName : boolean; section : TImageSection; n : DWORD) : string;
  var
    p : PWideChar;
  begin
    if IdorName then
      result := IntToStr (n)
    else
    begin
      p := PWideChar (PByte (section.fRawData.Memory) + (n and $7fffffff));
      result := String (ResourceWideCharToAnsiStr (p, CP_ACP))
    end
  end;

  // (recursively) get resources
  procedure GetResource (offset, level : Integer);
  var
    entry : PResourceDirectoryEntry;
    i, count : Integer;
    IDorName : boolean;
    dataEntry : PResourceDataEntry;
    table : PResourceDirectoryTable;
    details : TResourceDetails;
  begin
    table := PResourceDirectoryTable (PByte (section.fRawData.memory) + offset);
    with table^ do
      count := cNameEntries + cIDEntries;

    entry := PResourceDirectoryEntry (PByte (section.fRawData.memory) + offset + sizeof (TResourceDirectoryTable));
    for i := 0 to count - 1 do
    begin
      idOrName := i >= table^.cNameEntries;
      case level of
        0 : tp := GetResourceStr (IDOrName, section, entry^.name);
        1 :
            name := GetResourceStr (IDOrName, section, entry^.name);
        2 :
          begin
            if not IdOrName then
              raise EPEException.Create (rstBadLangID);

            lang := entry^.name
          end
      end;

      if (entry^.RVA and $80000000) > 0 then // Not a leaf node - traverse the tree
        GetResource (entry^.RVA and $7fffffff, level + 1)
      else
      begin
                                             // It's a leaf node - create resource details
        dataEntry := PResourceDataEntry (PByte (section.fRawData.Memory) + entry^.RVA);
        details := TResourceDetails.CreateResourceDetails (self, lang, name, tp, dataEntry^.Size, PByte (section.fRawData.Memory) + dataEntry^.OffsetToData - section.fSectionHeader.VirtualAddress);
        details.CodePage := dataEntry^.CodePage;
        details.Characteristics := table^.characteristics;
        details.DataVersion := DWORD (table^.versionMajor) * 65536 + DWORD (table^.versionMinor);
        fDetailList.Add (details);

      end;

      Inc (entry)
    end
  end;

begin
  inherited;
  section := GetResourceSection (offset);
  if section <> nil then
    GetResource (offset, 0)
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceCount                          |
 |                                                                      |
 | Return the number of resources in the resource section               |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceCount: Integer;
begin
  result := fDetailList.Count
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceDetails                        |
 |                                                                      |
 | Get the resource details for the specified resource.                 |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceDetails(
  idx: Integer): TResourceDetails;
begin
  result := TResourceDetails (fDetailList [idx]);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.IndexOfResource                           |
 |                                                                      |
 | Return the index of the specified resource details in the resource   |
 *----------------------------------------------------------------------*)
function TPEResourceModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  result := fDetailList.IndexOf (details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.InsertResource                            |
 |                                                                      |
 | Insert a resource in the list.                                       |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  fDetailList.Insert (idx, details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Encode                                    |
 |                                                                      |
 | Complicated?  I'll give you complicated ...                          |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Encode;
var
  i : Integer;
  details : TResourceDetails;
  section : TImageSection;
  root : TResourceNode;
  versMajor, versMinor : word;
  TimeStamp : DWORD;
  nameSize, nameOffset, namePos, tableOffset : DWORD;
  deOffset, dePos, deSize : DWORD;
  dataOffset, dataPos, dataSize : DWORD;
  offset : Integer;

  nameTable : PByte;
  deTable : PByte;
  data : PByte;
  zeros : PByte;

  //------------------------------------------------------------------
  // Calculate offset and size of name table and DirectoryEntry table.
  // Calculate size of data

  procedure GetNameTableSize (node : TResourceNode);
  var
    i : Integer;
  begin
    Inc (nameOffset, sizeof (TResourceDirectoryTable));
    Inc (deOffset, sizeof (TResourceDirectoryTable));

    for i := 0 to node.count - 1 do
    begin
      Inc (nameOffset, sizeof (TResourceDirectoryEntry));
      Inc (deOffset, sizeof (TResourceDirectoryEntry));

      if not node.nodes [i].intID then
        Inc (nameSize, Length (node.nodes [i].id) * sizeof (WideChar) + sizeof (word));

      if not node.nodes [i].leaf then
        GetNameTableSize (node.nodes [i].next)
      else
      begin
        Inc (nameOffset, sizeof (TResourceDataEntry));
        Inc (deSize, sizeof (TResourceDataEntry));
        dataSize := (dataSize + DWORD (node.nodes [i].data.Size) + 3) div 4 * 4;
      end
    end
  end;

  //------------------------------------------------------------------
  // Save a node to section.fRawData (and save it's child nodes recursively)

  procedure SaveToSection (node : TResourceNode);
  var
    table : TResourceDirectoryTable;
    entry : TResourceDirectoryEntry;
    dataEntry : PResourceDataEntry;
    i, n : Integer;
    w : WideString;
    wl : word;

  //------------------------------------------------------------------
  // Save entry (i), and the child nodes

    procedure SaveNode (i : Integer);
    begin
      if node.nodes [i].intID then      // id is a simple integer
        entry.name := StrToInt (String (node.nodes [i].id))
      else
      begin                             // id is an offset to a name in the
                                        // name table.
        entry.name := nameOffset + namePos + $80000000;
        w := String (node.nodes [i].id);
        wl := Length (node.nodes [i].id);
        Move (wl, nameTable [namePos], sizeof (wl));
        Inc (namePos, sizeof (wl));
        Move (w [1], nameTable [namePos], wl * sizeof (WideChar));
        Inc (namePos, wl * sizeof (WideChar))
      end;

      if node.nodes [i].leaf then       // RVA points to a TResourceDataEntry in the
      begin                             // data entry table.
        entry.RVA := deOffset + dePos;
        dataEntry := PResourceDataEntry (deTable + dePos);
        dataEntry^.CodePage := node.nodes [i].CodePage;
        dataEntry^.Reserved := 0;
        dataEntry^.Size := node.nodes [i].data.Size;
        dataEntry^.OffsetToData := dataOffset + dataPos + section.fSectionHeader.VirtualAddress;

        Move (node.nodes [i].data.memory^, data [dataPos], dataEntry^.Size);

        Inc (dePos, sizeof (TResourceDataEntry));
        dataPos := (dataPos + dataEntry^.size + 3) div 4 * 4;
        section.fRawData.Write (entry, sizeof (entry));
      end
      else                              // RVA points to another table.
      begin
        entry.RVA := $80000000 + tableOffset;
        section.fRawData.Write (entry, sizeof (entry));
        n := section.fRawData.Position;
        SaveToSection (node.nodes [i].next);
        section.fRawData.Seek (n, soFromBeginning);
      end
    end;

  begin { SaveToSection }
    table.characteristics := 0;
    table.timeDateStamp := TimeStamp;
    table.versionMajor := versMajor;
    table.versionMinor := versMinor;
    table.cNameEntries := 0;
    table.cIDEntries := 0;

                                        // Calculate no of integer and string IDs
    for i := 0 to node.count - 1 do
      if node.nodes [i].intID then
        Inc (table.cIDEntries)
      else
        Inc (table.cNameEntries);

    section.fRawData.Seek (tableOffset, soFromBeginning);
    section.fRawData.Write (table, sizeof (table));

    tableOffset := tableOffset + sizeof (TResourceDirectoryTable) + DWORD (node.Count) * sizeof (TResourceDirectoryEntry);

                                        // The docs suggest that you save the nodes
                                        // with string entries first.  Goodness knows why,
                                        // but play along...
    for i := 0 to node.count - 1 do
      if not node.nodes [i].intID then
       SaveNode (i);

    for i := 0 to node.count - 1 do
      if node.nodes [i].intID then
       SaveNode (i);

    section.fRawData.Seek (0, soFromEnd);
  end;


begin { Encode }
  section := GetResourceSection (offset);

  if section = Nil then
  begin
    inherited;
    exit
  end;

                                        // Get the details in a tree structure
  root := Nil;
  data := Nil;
  deTable := Nil;
  zeros := Nil;

  try
    for i := 0 to fDetailList.Count - 1 do
    begin
      details := TResourceDetails (fDetailList.Items [i]);
      if root = Nil then
        root := TResourceNode.Create (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
      else
        root.Add (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
    end;

                                          // Save elements of their original EXE
    versMajor := PResourceDirectoryTable (section.fRawData.Memory)^.versionMajor;
    versMinor := PResourceDirectoryTable (section.fRawData.Memory)^.versionMinor;
    TimeStamp := PResourceDirectoryTable (section.fRawData.Memory)^.timeDateStamp;


    section.fRawData.Clear;               // Clear the data.  We're gonna recreate
                                          // it from our resource details.

    nameSize := 0; nameOffset := offset;
    deSize := 0; deOffset := offset;
    dataSize := 0;

    GetNameTableSize (root);              // Calculate sizes and offsets of the
                                          // name table, the data entry table and
                                          // the size of the data.

                                          // Calculate the data offset.  Must be aligned.
    dataOffset := (nameOffset + nameSize + 15) div 16 * 16;

                                          // Initialize globals...
    namePos := 0;                         //   Offset of next entry in the string table
    dePos := 0;                           //   Offset of next entry in the data entry table
    dataPos := 0;                         //   Offset of next data block.
    tableOffset := 0;                     //   Offset of next TResourceDirectoryTable


    GetMem (nameTable, nameSize);         // Allocate buffers for tables
    GetMem (data, dataSize);
    GetMem (deTable, deSize);

    SaveToSection (root);               // Do the work.

                                        // Save the tables
    section.fRawData.Write (deTable^, deSize);
    section.fRawData.Write (nameTable^, nameSize);

                                        // Add padding so the data goes on a
                                        // 16 byte boundary.
    if DWORD (section.fRawData.Position) < dataOffset then
    begin
      GetMem (zeros, dataOffset - DWORD (section.fRawData.Position));
      ZeroMemory (zeros, dataOffset - DWORD (section.fRawData.Position));
      section.fRawData.Write (zeros^, dataOffset - DWORD (section.fRawData.Position))
    end;

                                        // Write the data.
    section.fRawData.Write (data^, dataSize);

    inherited; // **** Must call inherited !

  finally       // Tidy up.
    ReallocMem (zeros, 0);
    FreeMem (nameTable);
    FreeMem (deTable);
    FreeMem (data);
    root.Free
  end
end;


{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: string; ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;

begin
  for i := 0 to count - 1 do
    if AType = String (nodes [i].id) then
    begin
      nodes [i].next.AddName (AName, ALang, aData, codePage);
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (AType);
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateNameNode (AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if IntToStr (ALang) = String (nodes [i].id) then
    begin
      nodes [i].data := aData;
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (IntToStr (ALang));
  nodes [count - 1].intId := True;
  nodes [count - 1].leaf := True;
  nodes [count - 1].data := aData;
  nodes [count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: string; ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if AName = String (nodes [i].id) then
    begin
      nodes [i].next.AddLang (ALang, aData, codePage);
      exit
    end;

  Inc (count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (AName);
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateLangNode (ALang, aData, codePage)
end;

constructor TResourceNode.Create(const AType, AName: string;
  ALang: Integer; aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AnsiString (AType);
  nodes [count - 1].intID := isID (count - 1);
  nodes [0].leaf := False;
  nodes [0].next := TResourceNode.CreateNameNode (AName, ALang, aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer;
  aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AnsiString (IntToStr (ALang));
  nodes [count - 1].intID := True;
  nodes [0].leaf := True;
  nodes [0].data := aData;
  nodes [0].CodePage := codePage
end;

constructor TResourceNode.CreateNameNode(const AName: string;
  ALang: Integer; aData: TMemoryStream; codePage : DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AnsiString (AName);
  nodes [count - 1].intID := isID (count - 1);

  nodes [0].leaf := False;
  nodes [0].next := TResourceNode.CreateLangNode (ALang, aData, codePage)
end;

destructor TResourceNode.Destroy;
var
  i : Integer;
begin
  for i := 0 to count - 1 do
    if not nodes [i].leaf then
      nodes [i].next.Free;

  inherited;
end;

function TResourceNode.IsID (idx : Integer): boolean;
var
  i : Integer;
begin
  result := True;
  for i := 1 to Length (nodes [idx].id) do
    if not (nodes [idx].id [i] in ['0'..'9']) then
    begin
      result := False;
      break
    end;

  if result then
    result := IntToStr (StrToInt (String (nodes [idx].id))) = String (nodes [idx].id);
end;

function TPEResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := fDetailList.Add (details);
end;

procedure TPEResourceModule.SortResources;
begin
  fDetailList.Sort (compareDetails);
end;

{ TPEBase }

procedure TPEBase.ApplyGlobalFixups;
begin
// stub
end;

procedure TPEBase.ClearSymbolTable;
var
  i : Integer;
begin
  if fSymbolTable <> Nil then
    for i := 0 to fSymbolTable.Count - 1 do
      TSymbol (fSymbolTable [i]).Free;

  FreeAndNil (fSymbolTable);
  FreeAndNil (fSymbolTableIndex);
end;

constructor TPEBase.Create;
begin
  inherited Create;
  fSectionList := TObjectList.Create;
end;

procedure TPEBase.Decode(memory: pointer; Size: Integer);
begin
// stub
end;

procedure TPEBase.DecodeStringTable (memory : pointer);
var
  stringTableSize : PDWORD;
  stringTableRemain : DWORD;
  stringTablePtr : PByte;
  ist : PAnsiChar;
  len : Integer;
begin
  FreeAndNil (fStringTable);
  if CoffHeader.PointerToSymbolTable = 0 then
    Exit;

  stringTableSize := PDWORD (DWORD (memory) + CoffHeader.PointerToSymbolTable + CoffHeader.NumberOfSymbols * sizeof (TRawSymbol));
  stringTablePtr := PByte (Integer (stringTableSize));
  stringTableRemain := stringTableSize^ - sizeof (stringTableSize);

  ist := PAnsichar (stringTablePtr) + sizeof (stringTableSize);
  if stringTableRemain > 0 then
  begin
    fStringTable := TStringList.Create;

    while stringTableRemain > 0 do
    begin
      len := lstrlenA (ist);
      fStringTable.Add(String (ist));
      Inc (ist, len + 1);
      Dec (stringTableRemain, len+1)
    end
  end;
end;

procedure TPEBase.DecodeSymbolTable(memory: pointer);
var
  prs : PRawSymbol;
  ps : TSymbol;
  symbolName : AnsiString;
  stringTableSize : PDWORD;
  stringTablePtr : PByte;
  i : Integer;
begin
  ClearSymbolTable;
  if CoffHeader.NumberOfSymbols = 0 then
    Exit;

  fSymbolTable := TList.Create;
  fSymbolTableIndex := TList.Create;

  stringTableSize := PDWORD (DWORD (memory) + CoffHeader.PointerToSymbolTable + CoffHeader.NumberOfSymbols * sizeof (TRawSymbol));
  stringTablePtr := PByte (Integer (stringTableSize));

  prs := PRawSymbol (DWORD (memory) + CoffHeader.PointerToSymbolTable);
  i := 0;
  while DWORD (i) < CoffHeader.NumberOfSymbols do
  begin
    if prs^.name.zeros = 0 then
      symbolName := PAnsiChar (stringTablePtr) + prs^.name.offset
    else
    begin
      SetString (symbolName, prs^.name.shortName, sizeof (prs^.name.shortName));
      symbolName := PansiChar (symbolName)
    end;

    ps := TSymbol.Create (symbolName, i, prs^.value, prs^.sectionNumber, prs^._type, prs^.storageClass);
    fSymbolTable.Add(ps);
    fSymbolTableIndex.Add (ps);

    while prs^.numberOfAuxSymbols > 0 do
    begin
      fSymbolTableIndex.Add (ps);
      Inc (prs);
      Inc (i)
    end;

    Inc (prs);
    Inc (i);
  end
end;

destructor TPEBase.Destroy;
begin
  fSectionList.Free;
  ClearSymbolTable;
  inherited;
end;

procedure TPEBase.Encode;
begin
// stub
end;

function TPEBase.GetCodeSize: Integer;
begin
  result := GetSectionSize (IMAGE_SCN_CNT_CODE);
end;

function TPEBase.GetCOFFHeader: TImageFileHeader;
begin
  result := fCOFFHeader
end;

function TPEBase.GetFirstSectionWithCharacteristics(
  characteristicsMask: DWORD): TImageSection;
var
  i : Integer;
  section : TImageSection;
begin
  result := nil;
  for i := 0 to ImageSectionCount - 1 do
  begin
    section := ImageSection [i];

    if (section.SectionHeader.Characteristics and characteristicsMask) <> 0 then
    begin
      result := section;
      break
    end
  end
end;

function TPEBase.GetIDataSize: Integer;
begin
  result := GetSectionSize (IMAGE_SCN_CNT_INITIALIZED_DATA);
end;

function TPEBase.GetImageSection(index: Integer): TImageSection;
begin
  result := TImageSection (fSectionList [index]);
end;

function TPEBase.GetImageSectionCount: Integer;
begin
  result := fSectionList.Count
end;

function TPEBase.GetSectionByName(const name: AnsiString): TImageSection;
var
  i : Integer;
begin
  result := Nil;
  for i:= 0 to ImageSectionCount - 1 do
    if name = ImageSection [i].GetSectionName then
    begin
      result := ImageSection [i];
      break
    end
end;

function TPEBase.GetSectionSize(characteristicsMask: DWORD): Integer;
var
  i : Integer;
  section : TImageSection;
begin
  result := 0;
  for i := 0 to ImageSectionCount - 1 do
  begin
    section := ImageSection [i];

    if (section.SectionHeader.Characteristics and characteristicsMask) <> 0 then
      Inc (result, section.fSectionHeader.Misc.VirtualSize)
  end
end;

function TPEBase.GetStringTableCount: Integer;
begin
  if fStringTable = Nil then
    result := 0
  else
    result := fStringTable.Count
end;

function TPEBase.GetStringTableEntry(idx: Integer): AnsiString;
begin
  if fStringTable <> Nil then
    result := AnsiString (fStringTable [idx])
  else
    result := AnsiString (fStringTable [idx])
end;

function TPEBase.GetSymbol(idx: Integer): TSymbol;
begin
  if fSymbolTable <> Nil then
    result := TSymbol (fSymbolTable [idx])
  else
    result := Nil
end;

function TPEBase.GetSymbolCount: Integer;
begin
  if fSymbolTable <> Nil then
    result := fSymbolTable.Count
  else
    result := 0
end;

function TPEBase.GetSymbolTableIndex: TList;
begin
  SymbolCount;
  result := fSymbolTableIndex
end;

function TPEBase.GetUDataSize: Integer;
begin
  result := GetSectionSize (IMAGE_SCN_CNT_UNINITIALIZED_DATA);
end;

procedure TPEBase.LoadFromFile(const name: string);
var
  f : TFileStream;
begin
  f := TFileStream.Create (name, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (f)
  finally
    f.Free
  end
end;

procedure TPEBase.LoadFromStream(s: TStream);
var
  m : TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    m.CopyFrom (s, 0);

    Decode (m.memory, m.size)
  finally
    m.Free
  end
end;

{ TSymbol }

constructor TSymbol.Create(const ASymbolName: AnsiString; AIndex, AValue: DWORD;
  ASectionNumber, AType: word; AStorageClass: byte);
begin
  fName := ASymbolName;
  fIndex := AIndex;
  fValue := AValue;
  fSectionNumber := ASectionNumber;
  fType := AType;
  fStorageClass := AStorageClass;
end;

end.
