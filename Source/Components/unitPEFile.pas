(*======================================================================*
 | unitPEFile                                                           |
 |                                                                      |
 | Windows PE File Decoder unit                                         |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001                                      |
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
//   [ DOS Header      ]     First Word is 'MZ'
//   [ COFF header     ]     Starts at DOSHdr._lfaNew.  First dWord is COFF signature
//   [ Optional header ]     Follows COFF header.  First Word is IMAGE_NT_OPTIONAL_HDR_MAGIC
//   [ Data Directory  ]     Really part of the optional header
//   [ Image Sections Headers ] Starts at optionalHeader + COFFHdr.SizeOfOptionalHeader
//   [ Mystery padding]
//   [ Section data]         Each one pointed to by it's Image Section Header

unit unitPEFile;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails, ImageHlp;

type
  TXnImageOptionalHeader = class
  strict private
    FIsPE32Plus: Boolean;
    FCheckSumOffset: NativeUInt;
    FIOH32: PImageOptionalHeader32;
    FIOH64: PImageOptionalHeader64;
  private
    function GetAddressOfEntryPoint: DWORD;
    function GetBaseOfCode: DWORD;
    function GetDataDirectory(Index: Cardinal): TImageDataDirectory;
    function GetDllCharacteristics: Word;
    function GetFileAlignment: DWORD;
    function GetPE32PlusImageBase: ULONGLONG;
    function GetLoaderFlags: DWORD;
    function GetMagic: Word;
    function GetMajorImageVersion: Word;
    function GetMajorLinkerVersion: Byte;
    function GetMajorOperatingSystemVersion: Word;
    function GetMajorSubsystemVersion: Word;
    function GetMinorImageVersion: Word;
    function GetMinorOperatingSystemVersion: Word;
    function GetMinorSubsystemVersion: Word;
    function GetNumberOfRvaAndSizes: DWORD;
    function GetPE32BaseOfData: DWORD;
    function GetSectionAlignment: DWORD;
    function GetSizeOfCode: DWORD;
    function GetPE32PlusSizeOfHeapCommit: ULONGLONG;
    function GetPE32PlusSizeOfHeapReserve: ULONGLONG;
    function GetSizeOfImage: DWORD;
    function GetSizeOfInitializedData: DWORD;
    function GetPE32PlusSizeOfStackCommit: ULONGLONG;
    function GetPE32PlusSizeOfStackReserve: ULONGLONG;
    function GetSizeOfUninitializedData: DWORD;
    function GetSubsystem: Word;
    function GetWin32VersionValue: DWORD;
    function GetMinorLinkerVersion: Byte;
    function GetCheckSum: DWORD;
    function GetSizeOfHeaders: DWORD;
    procedure SetSizeOfHeaders(const Value: DWORD);
    procedure SetBaseOfCode(const Value: DWORD);
    procedure SetCheckSum(const Value: DWORD);
    procedure SetDataDirectory(Index: Cardinal;
      const Value: TImageDataDirectory);
    procedure SetSizeOfCode(const Value: DWORD);
    procedure SetSizeOfInitializedData(const Value: DWORD);
    procedure SetSizeOfUninitializedData(const Value: DWORD);
    procedure SetSizeOfImage(const Value: DWORD);
    procedure SetAddressOfEntryPoint(const Value: DWORD);
    function GetRawData: Pointer;
    function GetMagicAsString: string;
    function GetPE32ImageBase: DWORD;
    function GetPE32SizeOfHeapCommit: DWORD;
    function GetPE32SizeOfHeapReserve: DWORD;
    function GetPE32SizeOfStackCommit: DWORD;
    function GetPE32SizeOfStackReserve: DWORD;
    procedure SetPE32BaseOfData(const Value: DWORD);
  public
    constructor Create(ImageOptionalHeader: Pointer; Size: Integer);
    destructor Destroy; override;

    property IsPE32Plus: Boolean read FIsPE32Plus;
    property CheckSumOffset: NativeUInt read FCheckSumOffset;
    property RawData: Pointer read GetRawData;
  public
    property Magic: Word read GetMagic;
    property MagicAsString: string read GetMagicAsString;
    property MajorLinkerVersion: Byte read GetMajorLinkerVersion;
    property MinorLinkerVersion: Byte read GetMinorLinkerVersion;
    property SizeOfCode: DWORD read GetSizeOfCode write SetSizeOfCode;
    property SizeOfInitializedData: DWORD read GetSizeOfInitializedData
      write SetSizeOfInitializedData;
    property SizeOfUninitializedData: DWORD read GetSizeOfUninitializedData
      write SetSizeOfUninitializedData;
    property AddressOfEntryPoint: DWORD read GetAddressOfEntryPoint write
      SetAddressOfEntryPoint;
    property BaseOfCode: DWORD read GetBaseOfCode write SetBaseOfCode;

    property PE32_BaseOfData: DWORD read GetPE32BaseOfData write SetPE32BaseOfData;

    property PE32_ImageBase: DWORD read GetPE32ImageBase;
    property PE32Plus_ImageBase: ULONGLONG read GetPE32PlusImageBase;
    property SectionAlignment: DWORD read GetSectionAlignment;
    property FileAlignment: DWORD read GetFileAlignment;
    property MajorOperatingSystemVersion: Word read GetMajorOperatingSystemVersion;
    property MinorOperatingSystemVersion: Word read GetMinorOperatingSystemVersion;
    property MajorImageVersion: Word read GetMajorImageVersion;
    property MinorImageVersion: Word read GetMinorImageVersion;
    property MajorSubsystemVersion: Word read GetMajorSubsystemVersion;
    property MinorSubsystemVersion: Word read GetMinorSubsystemVersion;
    property Win32VersionValue: DWORD read GetWin32VersionValue;
    property SizeOfImage: DWORD read GetSizeOfImage write SetSizeOfImage;
    property SizeOfHeaders: DWORD read GetSizeOfHeaders write SetSizeOfHeaders;
    property CheckSum: DWORD read GetCheckSum write SetCheckSum;
    property Subsystem: Word read GetSubsystem;
    property DllCharacteristics: Word read GetDllCharacteristics;

    property PE32_SizeOfStackReserve: DWORD read GetPE32SizeOfStackReserve;
    property PE32_SizeOfStackCommit: DWORD read GetPE32SizeOfStackCommit;
    property PE32_SizeOfHeapReserve: DWORD read GetPE32SizeOfHeapReserve;
    property PE32_SizeOfHeapCommit: DWORD read GetPE32SizeOfHeapCommit;

    property PE32Plus_SizeOfStackReserve: ULONGLONG read GetPE32PlusSizeOfStackReserve;
    property PE32Plus_SizeOfStackCommit: ULONGLONG read GetPE32PlusSizeOfStackCommit;
    property PE32Plus_SizeOfHeapReserve: ULONGLONG read GetPE32PlusSizeOfHeapReserve;
    property PE32Plus_SizeOfHeapCommit: ULONGLONG read GetPE32PlusSizeOfHeapCommit;
    property LoaderFlags: DWORD read GetLoaderFlags;
    property NumberOfRvaAndSizes: DWORD read GetNumberOfRvaAndSizes;
    property DataDirectory[Index: Cardinal]: TImageDataDirectory
      read GetDataDirectory write SetDataDirectory;
  end;

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
    VirtualAddress: DWORD;
    SymbolTableIndex: DWORD;
    _type: Word;                 // See IMAGE_REL_I386 constants above
  end;
  PSectionReloc = ^TSectionReloc;

  //----------------------------------------------------------------------
  // TImageSection class

  TImageSection = class
  private
    FParent: TPEBase;
    FSectionHeader: TImageSectionHeader;
    FUninitializedDataSize: Integer;
    FDirectoryEntry: DWORD;
    FRelocs: array of TSectionReloc;

    function GetSectionName: AnsiString;
    function GetContainsCode: Boolean;
    function GetReloc (idx: Integer): TSectionReloc;
    function GetRelocCount: Integer;
  protected
    FRawData: TMemoryStream;
  public
    // Changed 'data' to be the base, rather than the pointer to raw data.  Added temporary tst
    // parameter to catch that use the old method & have to be modified
    constructor Create (AParent: TPEBase; const AHeader: TImageSectionHeader; data: pbyte; tst: Boolean);
    constructor CreateEmpty (AParent: TPEBase; const AName: AnsiString; const ACharacteristics, ADirectoryEntry: DWORD);
    destructor Destroy; override;
    procedure Initialize; virtual;

    procedure AddData (const data; len: Integer);
    function GetRelocPtr: PSectionReloc;

    procedure Fixup; virtual;

    property Parent: TPEBase read FParent;

    property SectionName: AnsiString read GetSectionName;
    property SectionHeader: TImageSectionHeader read FSectionHeader;
    property RawData: TMemoryStream read FRawData;
    property ContainsCode: Boolean read GetContainsCode;
    property RelocCount: Integer read GetRelocCount;
    property Reloc [idx: Integer]: TSectionReloc read GetReloc;
  end;

  TImageImportDescriptor = packed record
    Characteristics: DWORD; // This is an RVA to a list of pointers. Each of these points to there function Name
    TimeDateStamp: DWORD;   // The time/date stamp indicating when the file was built
    ForwarderChain: DWORD;  // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
    Name: DWORD;            // This is an RVA to a NULL-terminated ASCII string containing the imported DLL's name
    FirstThunk: DWORD;      //  Another RVA to a list pointers. Each of these points to their function Name
  end;
  PImageImportDescriptor = ^TImageImportDescriptor;

  TRawSymbolName = packed record case Boolean of
    False: (ShortName: array [0..7] of AnsiChar);
    True: (Zeros, Offset: DWORD);
  end;

  TRawSymbol = packed record
    Name: TRawSymbolName;
    Value: DWORD;
    SectionNumber: Word;
    _type: Word;
    StorageClass: Byte;
    NumberOfAuxSymbols: Byte;
  end;
  PRawSymbol = ^TRawSymbol;

  TSymbol = class
  private
    FName: AnsiString;
    FValue: DWORD;
    FSectionNumber: Word;
    FType: Word;
    FStorageClass: Byte;
    FIndex: DWORD;
  public
    constructor Create (const ASymbolName: AnsiString; AIndex, AValue: DWORD; ASectionNumber, AType: Word; AStorageClass: Byte);

    property Name: AnsiString read FName;
    property Value: DWORD read FValue;
    property SectionNumber: Word read FSectionNumber;
    property SectionType: Word read FType;
    property StorageClass: Byte read FStorageClass;
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
    FCOFFHeader: TImageFileHeader;
    FSectionList: TObjectList;                   // List of TImageSection objects
    FSymbolTable: TList;
    FSymbolTableIndex: TList;
    FStringTable: TStrings;

    function GetFirstSectionWithCharacteristics (characteristicsMask: DWORD): TImageSection;
    function GetSectionSize (characteristicsMask: DWORD): Integer;


    procedure ApplyGlobalFixups; virtual;
    procedure DecodeStringTable (memory: pointer);
    procedure DecodeSymbolTable (memory: pointer);
    procedure Decode (memory: pointer; size: Integer); virtual;
    procedure Encode; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCodeSize: Integer;
    function GetIDataSize: Integer;
    function GetUDataSize: Integer;

    function GetSectionByName (const Name: AnsiString): TImageSection;

    procedure LoadFromStream (s: TStream); override;
    procedure LoadFromFile (const Name: string); override;

    property COFFHeader: TImageFileHeader read GetCOFFHeader;
    property ImageSectionCount: Integer read GetImageSectionCount;
    property ImageSection [index: Integer]: TImageSection read GetImageSection;

    property SymbolCount: Integer read GetSymbolCount;
    property Symbol [idx: Integer]: TSymbol read GetSymbol;
    property SymbolTableIndex: TList read GetSymbolTableIndex;

    property StringTableCount: Integer read GetStringTableCount;
    property StringTableEntry [idx: Integer]: AnsiString read GetStringTableEntry;
  end;

  //----------------------------------------------------------------------
  // TPEModule class

  TPEModule = class (TPEBase)
  private
    FDOSStub: TMemoryStream;
    FCommentBlock: PByte;
    FCommentSize: Integer;
    FEndComment: PByte;
    FEndCommentSize: Integer;
    FIsPE32Plus: Boolean;

    function GetOptionalHeader: TXnImageOptionalHeader;
    function GetDataDirectory(index: Integer): TImageDataDirectory;
    function GetDataDirectoryCount: Integer;
    function GetDOSHeader: TImageDosHeader;
    function GetExportCount: Integer;
    function GetImportCount: Integer;
    function GetResourceSection (var Offset: Integer): TImageSection;
    function GetImportSection (var Offset: Integer): TImageSection;
    function GetExportSection (var Offset: Integer): TImageSection;
    function GetImport(idx: Integer): PImageImportDescriptor;
    function GetImportSectionData: PByte;
    function GetExportSectionData: PByte;
      procedure SetDataDirectory(index: Integer;
        const Value: TImageDataDirectory);

  protected
    FDOSHeader: TImageDosHeader;
    FOptionalHeader: TXnImageOptionalHeader;

    procedure ApplyGlobalFixups; override;
    procedure Decode (memory: pointer; exeSize: Integer); override;
    procedure Encode; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetExportDetails (idx: Integer; var Name: AnsiString; var ordinal: DWORD);
    procedure SaveToStream(s: TStream); override;

    function FindDirectoryEntrySection (entryNo: Integer; var Offset: Integer): Integer;

    property IsPE32Plus: Boolean read FIsPE32Plus;

    property DOSHeader: TImageDosHeader read GetDOSHeader;
    property DOSStub: TMemoryStream read FDOSStub;
    property OptionalHeader: TXnImageOptionalHeader read GetOptionalHeader;

    property DataDirectoryCount: Integer read GetDataDirectoryCount;
    property DataDirectory [index: Integer]: TImageDataDirectory
      read GetDataDirectory write SetDataDirectory;

    property ImportCount: Integer read GetImportCount;
    property Import [idx: Integer]: PImageImportDescriptor read GetImport;
    property ImportSectionData: PByte read GetImportSectionData;
    property ExportSectionData: PByte read GetExportSectionData;
    property ExportCount: Integer read GetExportCount;
  end;

  //----------------------------------------------------------------------
  // TResourceDirectoryTable record

  TResourceDirectoryTable = packed record
    characteristics: DWORD; // Resource flags, reserved for future use; currently set to zero.
    timeDateStamp: DWORD;   // Time the resource data was created by the resource compiler.
    versionMajor: Word;     // Major version number, set by the user.
    versionMinor: Word;     // Minor version number.
    cNameEntries: Word;     // Number of directory entries, immediately following the table, that use strings to identify Type, Name, or Language (depending on the level of the table).
    cIDEntries: Word;       // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
  end;
  PResourceDirectoryTable = ^TResourceDirectoryTable;

  //----------------------------------------------------------------------
  // TPEModule record

  TResourceDirectoryEntry = packed record
    Name: DWORD;         // RVA Address of integer or string that gives the Type, Name, or Language identifier, depending on level of table.
    RVA: DWORD;          // RVA High bit 0. Address of a Resource Data Entry (a leaf).
                          // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table (the next level down).
  end;
  PResourceDirectoryEntry = ^TResourceDirectoryEntry;

  //----------------------------------------------------------------------
  // TResourceDirectoryEntry record

  TResourceDataEntry = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  PResourceDataEntry = ^TResourceDataEntry;

  //----------------------------------------------------------------------
  // TPEResourceModule class

  TPEResourceModule = class (TPEModule)
  private
    FDetailList: TObjectList;             // List of TResourceDetails objects
  protected
    procedure Decode (memory: pointer; exeSize: Integer); override;
    procedure Encode; override;
    function GetResourceCount: Integer;  override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;


    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails [idx: Integer]: TResourceDetails read GetResourceDetails;
    procedure DeleteResource (resourceNo: Integer); override;
    procedure InsertResource (idx: Integer; details: TResourceDetails); override;
    function AddResource (details: TResourceDetails): Integer; override;
    function IndexOfResource (details: TResourceDetails): Integer; override;
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
  rstPropertyOnlyValidForPE32 = '%s property only exists in PE32 format';
  rstPropertyOnlyVAlidForPE32Plus = '%s property only exists in PE32+ format';

type
  TResourceNode = class;

  TRNode = record
    id: AnsiString;
    intID: Boolean;
    case leaf: Boolean of
      False: (next: TResourceNode);
      True: (data: TMemoryStream; CodePage: DWORD);
  end;

  TResourceNode = class
    count: Integer;
    nodes: array of TRNode;

    constructor Create (const AType, AName: UnicodeString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateNameNode (const AName: UnicodeString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateLangNode (ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure Add (const AType, AName: UnicodeString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddName (const AName: UnicodeString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddLang (ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    function IsID (idx: Integer): Boolean;
    destructor Destroy; override;
  end;

procedure MoveString (const src: AnsiString; var dst);
begin
  if Length(src) > 0 then
    Move(src [1], dst, Length (src));
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
  FDOSStub := TMemoryStream.Create;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Decode                                            |
 |                                                                      |
 | Decode the PE file.  Load the DOS header, the COFF header and the    |
 | 'optional' header, then load each section into FSectionList          |
 *----------------------------------------------------------------------*)
procedure TPEModule.Decode (Memory: pointer; exeSize: Integer);
var
  Offset: LongInt;
  i: Integer;
  sectionHeader: PImageSectionHeader;
  commentOffset, firstSectionOffset: Integer;
begin
  FSectionList.Clear;

                                // Check it's really a PE file.
  if PWORD (Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create (rstInvalidDOSSignature);

                                // Load the DOS header
  FDOSHeader := PImageDosHeader (Memory)^;

  Offset := FDOSHeader._lfanew;
  FDOSStub.Write ((PByte (Memory) + sizeof (FDOSHeader))^, FDOSHeader._lfanew - sizeof (FDOSHeader));

                                // Check the COFF signature
  if PDWORD (PByte (Memory) + Offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create (rstInvalidCOFFSignature);

                                // Load the COFF header
  Inc(Offset, sizeof (DWORD));
  FCOFFHeader := PImageFileHEader (PByte (Memory) + Offset)^;

  Inc(Offset, sizeof (FCOFFHeader));

                                // Check the Optional Header signature.  nb
                                // the optional header is compulsory for
                                // 32 bit windows modules!
  if (PWORD (PByte (Memory) + Offset)^ <> IMAGE_NT_OPTIONAL_HDR32_MAGIC) and
    (PWORD (PByte (Memory) + Offset)^ <> IMAGE_NT_OPTIONAL_HDR64_MAGIC) then
    raise EPEException.Create (rstInvalidOptionalHeader);

  FIsPE32Plus := PWORD (PByte (Memory) + Offset)^ = IMAGE_NT_OPTIONAL_HDR64_MAGIC;

                                // Save the 'optional' header

  FOptionalHeader := TXnImageOptionalHeader.Create((PByte (Memory) + Offset),
    FCOFFHeader.SizeOfOptionalHeader);

  Inc(Offset, FCOFFHeader.SizeOfOptionalHeader);

  commentOffset := Offset + FCOFFHeader.NumberOfSections * sizeof (TImageSectionHeader);

  sectionHeader := PImageSectionHeader (PByte (memory) + Offset);
  firstSectionOffset := 0;

  for i := 0 to FCOFFHeader.NumberOfSections - 1 do
  begin
    if firstSectionOffset = 0 then
      firstSectionOffset := sectionHeader^.PointerToRawData
    else
      if (sectionHeader^.PointerToRawData > 0) and (sectionHeader^.PointerToRawData < DWORD (firstSectionOffset)) then
        firstSectionOffset := sectionHeader^.PointerToRawData;

    Inc(sectionHeader);
  end;

// Save padding between the end of the section headers, and the start of the
// 1st section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  FCommentSize := firstSectionOffset - commentOffset;

  if FCommentSize > 0 then
  begin
    GetMem (FCommentBlock, FCommentSize);
    Move ((PByte (memory) + commentOffset)^, FCommentBlock^, FCommentSize)
  end;

  DecodeStringTable (Memory);
  DecodeSymbolTable (Memory);

                                // Now save each image section in the FSectionList
  for i := 0 to FCOFFHeader.NumberOfSections - 1 do
  begin
    sectionHeader := PImageSectionHeader (PByte (memory) + Offset);
    FSectionList.Add (TImageSection.Create (self, sectionHeader^, PByte (memory), False));
    Inc(Offset, sizeof (TImageSectionHeader));
  end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

// Save the padding between the last section and the end of the file.
// This appears to hold debug info and things ??

  FEndCommentSize := exeSize - i;
  if FEndCommentSize > 0 then
  begin
    GetMem (FEndComment, FEndCommentSize);
    Move ((PByte (memory) + i)^, FEndComment^, FEndCommentSize)
  end
end;

(*----------------------------------------------------------------------*
 | destructor PEModule.Destroy                                          |
 |                                                                      |
 | Destructor for TPEModule instance.                                   |
 *----------------------------------------------------------------------*)
destructor TPEModule.Destroy;
begin
  FOptionalHeader.Free;
  FDOSStub.Free;
  ReallocMem (FCommentBlock, 0);
  ReallocMem (FEndComment, 0);
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
  Offset: DWORD;
  i: Integer;
  section: TImageSection;
  align: Integer;
  addrAlign: Integer;
  address: Integer;
  alignedSize, AddrAlignedSize: Integer;
  codeSize, iDataSize, uDataSize, iSize, vs: Integer;
  DataDir: TImageDataDirectory;
begin
  codeSize := 0;
  iDataSize := 0;
  uDataSize := 0;
                                               // Use the DOS stub from their .EXE
  FDOSHeader._lfanew := sizeof (FDOSHeader) + FDOSStub.Size;

                                               // Fixup sections count
  FCOFFHeader.NumberOfSections := 0;

  iSize :=  FDOSHeader._lfanew +               // File Offset for start of sections
            SizeOf (DWORD) +                   // NT signature
            sizeof (FCOFFHeader) +
            FCOFFHeader.SizeOfOptionalHeader +
            FSectionList.Count * sizeof (TImageSectionHeader);

  Offset := iSize + FCommentSize;

  align := FOptionalHeader.FileAlignment;
  addrAlign := FOptionalHeader.SectionAlignment;

  address := addrAlign;
  Offset := DWORD ((integer (Offset) + align - 1) div align * align);

                                                // First section starts at $1000 (when loaded)
                                                // and at 'offset' in file.

  FOptionalHeader.SizeOfHeaders := DWORD ((integer (iSize) + align - 1) div align * align);

  FOptionalHeader.BaseOfCode := $ffffffff;
  FOptionalHeader.CheckSum := 0;               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize  := DWORD ((integer (iSize) + addrAlign - 1) div addrAlign * addrAlign);

  for i := 0 to FSectionList.Count - 1 do      // Recalculate the section offsets
  begin
    section := TImageSection (FSectionList [i]);

    section.FSectionHeader.PointerToRawData := Offset;
    section.FSectionHeader.VirtualAddress := address;

  // Apply fixups to the section (Just a stub - only used by derived classes)
    section.Fixup;

// Virtual size is size of data in memory, and is not padded to an 'alignment'.
//
// SizeOfRawData is size of data in file, padded to (file) alignment.
//
// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.
//
// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with Zeros when it's loaded.
//
// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.
//
// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    vs := section.FRawData.Size + section.FUninitializedDataSize;
    section.FSectionHeader.Misc.VirtualSize := vs;

    if vs <> 0 then
      Inc(FCOFFHeader.NumberOfSections);

    section.FSectionHeader.SizeOfRawData := (section.FRawData.Size + align - 1) div align * align;

    if (section.FDirectoryEntry <> $ffffffff) and (vs <> 0) then
    begin
      DataDir.VirtualAddress := address;
      DataDir.Size := section.FSectionHeader.Misc.VirtualSize;
      FOptionalHeader.DataDirectory [section.FDirectoryEntry] := DataDir;
    end;

    alignedSize := (Integer (vs) + align - 1) div align * align;
    addrAlignedSize := (Integer (vs) + addrAlign - 1) div addrAlign * addrAlign;

    if (section.FSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
    begin
      Inc(codeSize, alignedSize);
      if DWORD (address) < FOptionalHeader.BaseOfCode then
        FOptionalHeader.BaseOfCode := address;
    end
    else
      if (section.FSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
        Inc(iDataSize, alignedSize)
      else
        if (section.FSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
          Inc(uDataSize, alignedSize);

    Inc(iSize, addrAlignedSize);
    Inc(Offset, section.FSectionHeader.SizeOfRawData);
    Inc(address, (Integer (vs) + addrAlign - 1) div addrAlign * addrAlign);
  end;

  // Apply fixups to the code section (This function's just a stub - used in
  // derived classes)
  ApplyGlobalFixups;

  FOptionalHeader.SizeOfCode := codeSize;
  FOptionalHeader.SizeOfInitializedData := iDataSize;
  FOptionalHeader.SizeOfUninitializedData := uDataSize;

  i := SizeOf (DWORD) +                   // NT signature
       sizeof (FCOFFHeader) +
       FCOFFHeader.SizeOfOptionalHeader +
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

  if not FOptionalHeader.IsPE32Plus then
    FOptionalHeader.PE32_BaseOfData := FOptionalHeader.BaseOfCode + DWORD (i);

  FOptionalHeader.SizeOfImage := iSize;

  if FOptionalHeader.AddressOfEntryPoint = 0 then
  begin
    Section := GetFirstSectionWithCharacteristics (IMAGE_SCN_CNT_CODE);
    if Section <> Nil then
      FOptionalHeader.AddressOfEntryPoint := Section.FSectionHeader.VirtualAddress
  end;
end;

(*----------------------------------------------------------------------*
 | function PEModule.FindDirectoryEntrySection                         |
 |                                                                      |
 | Return the index of the specified section.  The 'entryNo' to find    |
 | should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in         |
 | Windows.pas.                                                         |
 *----------------------------------------------------------------------*)
function TPEModule.FindDirectoryEntrySection (entryNo: Integer; var Offset: Integer): Integer;
var
  i: Integer;
  dir: TImageDataDirectory;
begin
  Result := -1;
  dir := DataDirectory [entryNo];
                                // Find section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (dir.VirtualAddress >= ImageSection [i].FSectionHeader.VirtualAddress) and (dir.VirtualAddress < ImageSection [i].FSectionHeader.VirtualAddress + ImageSection [i].FSectionHeader.Misc.VirtualSize) then
    begin
      if dir.Size <> 0 then
      begin
        Result := i;
        Offset := dir.VirtualAddress - ImageSection [i].FSectionHeader.VirtualAddress;
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
function TPEModule.GetDataDirectory(index: Integer): TImageDataDirectory;
begin
  if index < DataDirectoryCount then
  begin
    Result := FOptionalHeader.DataDirectory [index];
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
  Result := FOptionalHeader.NumberOfRvaAndSizes
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDosHeader                                      |
 |                                                                      |
 | Return DOS header                                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  Result := FDOSHeader;
end;

function TPEModule.GetExportCount: Integer;
var
  ExportSection: PImageExportDirectory;
  section: TImageSection;
  Offset: Integer;
begin
  section := GetExportSection (Offset);
  if Assigned(section) then
  begin
    ExportSection := PImageExportDirectory (PByte (section.FRawData.memory) + Offset);
    Result := ExportSection^.NumberOfNames
  end
  else
    Result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var Name: AnsiString;
  var ordinal: DWORD);
var
  ExportSection: PImageExportDirectory;
  section: TImageSection;
  Offset: Integer;
  po: DWORD;
  pw: PWORD;
  p: PDWORD;
  data: PByte;
begin
  section := GetExportSection (Offset);
  if Assigned(section) then
  begin
    data := GetExportSectionData;
    ExportSection := PImageExportDirectory (PByte (section.FRawData.memory) + Offset);
    po := DWORD (ExportSection^.AddressOfNameOrdinals);
    pw := PWORD (Data + po);
    Inc(pw, idx);
    ordinal := pw^;

    po := DWORD (ExportSection^.AddressOfNames);
    p := PDWORD (Data + po);
    Inc(p, idx);
    Name := PAnsiChar (data) + p^
  end
end;

function TPEModule.GetExportSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  Offset := 0;
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_EXPORT, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

function TPEModule.GetExportSectionData: PByte;
var
  section: TImageSection;
  Offset: Integer;
begin
  section := GetExportSection (Offset);
  Result := PByte (section.FRawData.Memory) - section.FSectionHeader.VirtualAddress;
end;

function DirValid (dir: PImageImportDescriptor): Boolean;
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
  ImportSection: PImageImportDescriptor;
  section: TImageSection;
  Offset: Integer;

begin
  section := GetImportSection (Offset);
  Result := Nil;
  if Assigned(section) then
  begin
    ImportSection := PImageImportDescriptor (PByte (section.FRawData.memory) + Offset);

    while DirValid (ImportSection) and (idx > 0) do
    begin
      Inc(ImportSection);
      Dec(idx)
    end;

    if DirValid (ImportSection) then
      Result := ImportSection
  end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection: PImageImportDescriptor;
  section: TImageSection;
  Offset: Integer;
begin
  section := GetImportSection (Offset);
  Result := 0;
  if Assigned(section) then
  begin
    ImportSection := PImageImportDescriptor (PByte (section.FRawData.memory) + Offset);

    while DirValid (ImportSection) do
    begin
      Inc(Result);
      Inc(ImportSection)
    end
  end
end;

function TPEModule.GetImportSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_IMPORT, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

function TPEModule.GetImportSectionData: PByte;
var
  section: TImageSection;
  Offset: Integer;
begin
  section := GetImportSection (Offset);
  Result := PByte (section.FRawData.Memory) - section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TXnImageOptionalHeader;
begin
  Result := FOptionalHeader
end;

function TPEModule.GetResourceSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDirectoryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToStream                                     |
 |                                                                      |
 | Save the module to a stream                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.SaveToStream(s: TStream);
var
  NTSignature: DWORD;
  i: Integer;
  section: TImageSection;
  paddingSize, paddingLen: Integer;
  padding: PByte;
  f: TMemoryStream;
  oldCheckSum, newCheckSum: DWORD;
  ntHeaders: PImageNTHEaders;
  ckOffset: NativeUInt;
begin
  Encode;                       // Encode the data.

  NTSignature := IMAGE_NT_SIGNATURE;

                                // Write the DOS stub
  s.Write (FDOSHeader, sizeof (FDOSHeader));
  s.CopyFrom (FDOSStub, 0);

                                // Write NT sig and COFF header
  s.Write (NTSignature, sizeof (NTSignature));
  s.Write (FCOFFHeader, sizeof (FCOFFHeader));
  ckOffset := s.Position + FOptionalHeader.CheckSumOffset;
  s.Write (FOptionalHeader.RawData^, FCOFFHeader.SizeOfOptionalHeader);

                                // Write the section headers
  for i := 0 to FSectionList.Count - 1 do
  begin
    section := TImageSection (FSectionList [i]);
    if section.FSectionHeader.Misc.VirtualSize <> 0 then
      s.Write (section.FSectionHeader, sizeof (section.FSectionHeader))
  end;

  if FCommentSize > 0 then      // Save the 'comment' section.  See 'Decode' for details
    s.Write (FCommentBlock^, FCommentSize);

                                // Write the sections
  padding := Nil;
  paddingLen := 0;
  try
    for i := 0 to FSectionList.Count - 1 do
    begin
                                // Write padding up to file Offset of the section
      section := TImageSection (FSectionList [i]);

      if section.FSectionHeader.Misc.VirtualSize = 0 then
        Continue;

      paddingSize := section.FSectionHeader.PointerToRawData - DWORD (s.Position);

      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory (padding, paddingLen);
      end;

      if paddingSize > 0 then   // Put our signature at the start of the first
          s.Write (padding^, paddingSize);

                                // Write the section data.
      s.CopyFrom (section.FRawData, 0);

                                // Write data
      with section.FSectionHeader do
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

    if FEndCommentSize > 0 then  // Save the debug info.
      s.Write (FEndComment^, FEndCommentSize)
  finally
    ReallocMem (padding, 0)
  end;

  f := TMemoryStream.Create;    // Now calculate the checksum....
  try
    s.Seek (0, TSeekOrigin.soBeginning);
    f.LoadFromStream (s);
    ntHeaders := ChecksumMappedFile (f.Memory, f.Size, @oldCheckSum, @newCheckSum);

    if Assigned(ntHeaders) then
    begin
      s.Seek (ckOffset, TSeekOrigin.soBeginning);
      s.Write (newChecksum, sizeof (newChecksum))
    end
  finally
    f.Free
  end;

  s.Seek (0, soFromEnd);
end;


procedure TPEModule.SetDataDirectory(index: Integer;
  const Value: TImageDataDirectory);
begin
  if index < DataDirectoryCount then
  begin
    FOptionalHeader.DataDirectory [index] := Value;
  end
  else
    raise ERangeError.Create (rstBadDirectoryIndex);
end;

{ TImageSection }

procedure TImageSection.AddData(const data; len: Integer);
begin
  RawData.Write(data, len);
  FSectionHeader.SizeOfRawData := FSectionHeader.SizeOfRawData + DWORD (len)
end;

(*----------------------------------------------------------------------*
 | constructor TImageSection.Create                                     |
 |                                                                      |
 | Constructor for TImageSection.                                       |
 *----------------------------------------------------------------------*)
constructor TImageSection.Create(AParent: TPEBase;
  const AHeader: TImageSectionHeader; data: pbyte; tst: Boolean);
var
  i: Integer;
  psr: PSectionReloc;
  sectionData: PByte;
begin
  FSectionHeader := AHeader;
  FDirectoryEntry := $ffffffff;
  FRawData := TMemoryStream.Create;

  sectionData := data;
  Inc(sectionData, AHeader.PointerToRawData);

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if FSectionHeader.Misc.VirtualSize <= FSectionHeader.SizeOfRawData then
  begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if FSectionHeader.Misc.VirtualSize = 0 then
      FSectionHeader.Misc.VirtualSize := FSectionHeader.SizeOfRawData;
    FRawData.Write (sectionData^, FSectionHeader.Misc.VirtualSize)
  end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
  begin
    FRawData.Write (sectionData^, FSectionHeader.SizeOfRawData);
    FUninitializedDataSize := FSectionHeader.Misc.VirtualSize - FSectionHeader.SizeOfRawData;
  end;

  SetLength (FRelocs, FSectionHeader.NumberOfRelocations);
  psr := PSectionReloc (DWORD (data) + FSectionHeader.PointerToRelocations);
  for i := 0 to FSectionHeader.NumberOfRelocations - 1 do
  begin
    FRelocs [i] := psr^;
    Inc(psr)
  end;


  FParent := AParent;

  Initialize
end;

(*----------------------------------------------------------------------*
 | function TImageSection.GetSectionName                                |
 |                                                                      |
 | Return the section Name - eg. .data                                  |
 *----------------------------------------------------------------------*)
function TImageSection.GetContainsCode: Boolean;
begin
  Result := SectionHeader.Characteristics and (IMAGE_SCN_CNT_CODE) <> 0;
end;

function TImageSection.GetReloc (idx: Integer): TSectionReloc;
begin
  Result := FRelocs [idx]
end;

function TImageSection.GetRelocCount: Integer;
begin
  Result := Length (FRelocs)
end;

function TImageSection.GetRelocPtr: PSectionReloc;
begin
  Result := @FRelocs [0];
end;

function TImageSection.GetSectionName: AnsiString;
begin
  SetString (Result, PAnsiChar (@FSectionHeader.Name), sizeof (FSectionHeader.Name));
  Result := PAnsiChar (Result);
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
  FParent := AParent;
  FRawData := TMemoryStream.Create;
  FillChar (FSectionHeader, sizeof (FSectionHeader), 0);
  MoveString (AName, FSectionHeader.Name);
  FSectionHeader.Characteristics := ACharacteristics;
  FDirectoryEntry := ADirectoryEntry;
  Initialize
end;

destructor TImageSection.destroy;
begin
  FRawData.Free;
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
  res: TResourceDetails;
begin
  res := ResourceDetails [resourceNo];
  inherited;
  resourceNo := IndexOfResource (Res);
  if resourceNo <> -1 then
    FDetailList.Delete (resourceNo);
end;

(*----------------------------------------------------------------------*
 | constructor TPEResourceModule.Create                                 |
 |                                                                      |
 | Constructor for TPEResourceModule                                    |
 *----------------------------------------------------------------------*)
constructor TPEResourceModule.Create;
begin
  inherited Create;
  FDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TPEResourceModule.Destroy                                 |
 |                                                                      |
 | Destructor for TPEResourceModule                                     |
 *----------------------------------------------------------------------*)
destructor TPEResourceModule.Destroy;
begin
  FDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Decode                                    |
 |                                                                      |
 | Decode the section's resource tree into a list of resource details   |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Decode;
var
  section: TImageSection;
  tp, Name: string;
  lang: Integer;
  Offset: Integer;

  // Get string resource Name
  function GetResourceStr (IdorName: Boolean; section: TImageSection; n: DWORD): string;
  var
    p: PWideChar;
  begin
    if IdorName then
      Result := IntToStr (n)
    else
    begin
      p := PWideChar (PByte (section.FRawData.Memory) + (n and $7fffffff));
      Result := String (ResourceWideCharToAnsiStr (p, CP_ACP))
    end
  end;

  // (recursively) get resources
  procedure GetResource (Offset, level: Integer);
  var
    entry: PResourceDirectoryEntry;
    i, count: Integer;
    IDorName: Boolean;
    dataEntry: PResourceDataEntry;
    table: PResourceDirectoryTable;
    details: TResourceDetails;
  begin
    table := PResourceDirectoryTable (PByte (section.FRawData.memory) + Offset);
    with table^ do
      count := cNameEntries + cIDEntries;

    entry := PResourceDirectoryEntry (PByte (section.FRawData.memory) + Offset + sizeof (TResourceDirectoryTable));
    for i := 0 to count - 1 do
    begin
      idOrName := i >= table^.cNameEntries;
      case level of
        0: tp := GetResourceStr (IDOrName, section, entry^.Name);
        1 :
            Name := GetResourceStr (IDOrName, section, entry^.Name);
        2 :
          begin
            if not IdOrName then
              raise EPEException.Create (rstBadLangID);

            lang := entry^.Name
          end
      end;

      if (entry^.RVA and $80000000) > 0 then // Not a leaf node - traverse the tree
        GetResource (entry^.RVA and $7fffffff, level + 1)
      else
      begin
                                             // It's a leaf node - create resource details
        dataEntry := PResourceDataEntry (PByte (section.FRawData.Memory) + entry^.RVA);
        details := TResourceDetails.CreateResourceDetails (self, lang, Name, tp, dataEntry^.Size, PByte (section.FRawData.Memory) + dataEntry^.OffsetToData - section.FSectionHeader.VirtualAddress);
        details.CodePage := dataEntry^.CodePage;
        details.Characteristics := table^.characteristics;
        details.DataVersion := DWORD (table^.versionMajor) * 65536 + DWORD (table^.versionMinor);
        FDetailList.Add (details);

      end;

      Inc(entry)
    end
  end;

begin
  inherited;
  section := GetResourceSection (Offset);
  if section <> nil then
    GetResource (Offset, 0)
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceCount                          |
 |                                                                      |
 | Return the number of resources in the resource section               |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceCount: Integer;
begin
  Result := FDetailList.Count
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceDetails                        |
 |                                                                      |
 | Get the resource details for the specified resource.                 |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceDetails(
  idx: Integer): TResourceDetails;
begin
  Result := TResourceDetails (FDetailList [idx]);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.IndexOfResource                           |
 |                                                                      |
 | Return the index of the specified resource details in the resource   |
 *----------------------------------------------------------------------*)
function TPEResourceModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.IndexOf (details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.InsertResource                            |
 |                                                                      |
 | Insert a resource in the list.                                       |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  FDetailList.Insert(idx, details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Encode                                    |
 |                                                                      |
 | Complicated?  I'll give you complicated ...                          |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Encode;
var
  i: Integer;
  details: TResourceDetails;
  section: TImageSection;
  root: TResourceNode;
  versMajor, versMinor: Word;
  TimeStamp: DWORD;
  nameSize, nameOffset, namePos, tableOffset: DWORD;
  deOffset, dePos, deSize: DWORD;
  dataOffset, dataPos, dataSize: DWORD;
  Offset: Integer;

  nameTable: PByte;
  deTable: PByte;
  data: PByte;
  Zeros: PByte;

  //------------------------------------------------------------------
  // Calculate Offset and size of Name table and DirectoryEntry table.
  // Calculate size of data

  procedure GetNameTableSize (node: TResourceNode);
  var
    i: Integer;
  begin
    Inc(nameOffset, sizeof (TResourceDirectoryTable));
    Inc(deOffset, sizeof (TResourceDirectoryTable));

    for i := 0 to node.count - 1 do
    begin
      Inc(nameOffset, sizeof (TResourceDirectoryEntry));
      Inc(deOffset, sizeof (TResourceDirectoryEntry));

      if not node.nodes [i].intID then
        Inc(nameSize, Length (node.nodes [i].id) * sizeof (WideChar) + sizeof (Word));

      if not node.nodes [i].leaf then
        GetNameTableSize (node.nodes [i].next)
      else
      begin
        Inc(nameOffset, sizeof (TResourceDataEntry));
        Inc(deSize, sizeof (TResourceDataEntry));
        dataSize := (dataSize + DWORD (node.nodes [i].data.Size) + 3) div 4 * 4;
      end
    end
  end;

  //------------------------------------------------------------------
  // Save a node to section.FRawData (and save it's child nodes recursively)

  procedure SaveToSection (node: TResourceNode);
  var
    table: TResourceDirectoryTable;
    entry: TResourceDirectoryEntry;
    dataEntry: PResourceDataEntry;
    i, n: Integer;
    w: WideString;
    wl: Word;

  //------------------------------------------------------------------
  // Save entry (i), and the child nodes

    procedure SaveNode (i: Integer);
    begin
      if node.nodes [i].intID then      // id is a simple integer
        entry.Name := StrToInt(String (node.nodes [i].id))
      else
      begin                             // id is an Offset to a Name in the
                                        // Name table.
        entry.Name := nameOffset + namePos + $80000000;
        w := String (node.nodes [i].id);
        wl := Length (node.nodes [i].id);
        Move (wl, nameTable [namePos], sizeof (wl));
        Inc(namePos, sizeof (wl));
        Move (w [1], nameTable [namePos], wl * sizeof (WideChar));
        Inc(namePos, wl * sizeof (WideChar))
      end;

      if node.nodes [i].leaf then       // RVA points to a TResourceDataEntry in the
      begin                             // data entry table.
        entry.RVA := deOffset + dePos;
        dataEntry := PResourceDataEntry (deTable + dePos);
        dataEntry^.CodePage := node.nodes [i].CodePage;
        dataEntry^.Reserved := 0;
        dataEntry^.Size := node.nodes [i].data.Size;
        dataEntry^.OffsetToData := dataOffset + dataPos + section.FSectionHeader.VirtualAddress;

        Move (node.nodes [i].data.memory^, data [dataPos], dataEntry^.Size);

        Inc(dePos, sizeof (TResourceDataEntry));
        dataPos := (dataPos + dataEntry^.size + 3) div 4 * 4;
        section.FRawData.Write (entry, sizeof (entry));
      end
      else                              // RVA points to another table.
      begin
        entry.RVA := $80000000 + tableOffset;
        section.FRawData.Write (entry, sizeof (entry));
        n := section.FRawData.Position;
        SaveToSection (node.nodes [i].next);
        section.FRawData.Seek (n, TSeekOrigin.soBeginning);
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
        Inc(table.cIDEntries)
      else
        Inc(table.cNameEntries);

    section.FRawData.Seek (tableOffset, TSeekOrigin.soBeginning);
    section.FRawData.Write (table, sizeof (table));

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

    section.FRawData.Seek (0, soFromEnd);
  end;


begin { Encode }
  section := GetResourceSection (Offset);

  if section = Nil then
  begin
    inherited;
    exit
  end;

                                        // Get the details in a tree structure
  root := Nil;
  data := Nil;
  deTable := Nil;
  Zeros := Nil;

  try
    for i := 0 to FDetailList.Count - 1 do
    begin
      details := TResourceDetails (FDetailList.Items [i]);
      if root = Nil then
        root := TResourceNode.Create (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
      else
        root.Add (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
    end;

                                          // Save elements of their original EXE
    versMajor := PResourceDirectoryTable (section.FRawData.Memory)^.versionMajor;
    versMinor := PResourceDirectoryTable (section.FRawData.Memory)^.versionMinor;
    TimeStamp := PResourceDirectoryTable (section.FRawData.Memory)^.timeDateStamp;


    section.FRawData.Clear;               // Clear the data.  We're gonna recreate
                                          // it from our resource details.

    nameSize := 0; nameOffset := Offset;
    deSize := 0; deOffset := Offset;
    dataSize := 0;

    GetNameTableSize (root);              // Calculate sizes and offsets of the
                                          // Name table, the data entry table and
                                          // the size of the data.

                                          // Calculate the data Offset.  Must be aligned.
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
    section.FRawData.Write (deTable^, deSize);
    section.FRawData.Write (nameTable^, nameSize);

                                        // Add padding so the data goes on a
                                        // 16 Byte boundary.
    if DWORD (section.FRawData.Position) < dataOffset then
    begin
      GetMem (Zeros, dataOffset - DWORD (section.FRawData.Position));
      ZeroMemory (Zeros, dataOffset - DWORD (section.FRawData.Position));
      section.FRawData.Write (Zeros^, dataOffset - DWORD (section.FRawData.Position))
    end;

                                        // Write the data.
    section.FRawData.Write (data^, dataSize);

    inherited; // **** Must call inherited !

  finally       // Tidy up.
    ReallocMem (Zeros, 0);
    FreeMem (nameTable);
    FreeMem (deTable);
    FreeMem (data);
    root.Free
  end
end;


{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: string; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;

begin
  for i := 0 to count - 1 do
    if AType = String (nodes [i].id) then
    begin
      nodes [i].next.AddName (AName, ALang, aData, codePage);
      exit
    end;

  Inc(count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (AType);
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateNameNode (AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if IntToStr (ALang) = String (nodes [i].id) then
    begin
      nodes [i].data := aData;
      exit
    end;

  Inc(count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (IntToStr (ALang));
  nodes [count - 1].intId := True;
  nodes [count - 1].leaf := True;
  nodes [count - 1].data := aData;
  nodes [count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: string; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if AName = String (nodes [i].id) then
    begin
      nodes [i].next.AddLang (ALang, aData, codePage);
      exit
    end;

  Inc(count);
  SetLength (nodes, count);
  nodes [count - 1].id := AnsiString (AName);
  nodes [count - 1].intID := isID (count - 1);
  nodes [count - 1].leaf := False;
  nodes [count - 1].next := TResourceNode.CreateLangNode (ALang, aData, codePage)
end;

constructor TResourceNode.Create(const AType, AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
  count := 1;
  SetLength (nodes, 1);
  nodes [0].id := AnsiString (AType);
  nodes [count - 1].intID := isID (count - 1);
  nodes [0].leaf := False;
  nodes [0].next := TResourceNode.CreateNameNode (AName, ALang, aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
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
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
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
  i: Integer;
begin
  for i := 0 to count - 1 do
    if not nodes [i].leaf then
      nodes [i].next.Free;

  inherited;
end;

function TResourceNode.IsID (idx: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length (nodes [idx].id) do
    if not(nodes [idx].id [i] in ['0'..'9']) then
    begin
      Result := False;
      break
    end;

  if Result then
    Result := IntToStr (StrToInt(String (nodes [idx].id))) = String (nodes [idx].id);
end;

function TPEResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.Add (details);
end;

procedure TPEResourceModule.SortResources;
begin
  FDetailList.Sort(compareDetails);
end;

{ TPEBase }

procedure TPEBase.ApplyGlobalFixups;
begin
// stub
end;

procedure TPEBase.ClearSymbolTable;
var
  i: Integer;
begin
  if FSymbolTable <> Nil then
    for i := 0 to FSymbolTable.Count - 1 do
      TSymbol (FSymbolTable [i]).Free;

  FreeAndNil (FSymbolTable);
  FreeAndNil (FSymbolTableIndex);
end;

constructor TPEBase.Create;
begin
  inherited Create;
  FSectionList := TObjectList.Create;
end;

procedure TPEBase.Decode(memory: pointer; Size: Integer);
begin
// stub
end;

procedure TPEBase.DecodeStringTable (memory: pointer);
var
  stringTableSize: PDWORD;
  stringTableRemain: DWORD;
  stringTablePtr: PByte;
  ist: PAnsiChar;
  len: Integer;
begin
  FreeAndNil (FStringTable);
  if CoffHeader.PointerToSymbolTable = 0 then
    Exit;

  stringTableSize := PDWORD (DWORD (memory) + CoffHeader.PointerToSymbolTable + CoffHeader.NumberOfSymbols * sizeof (TRawSymbol));
  stringTablePtr := PByte (Integer (stringTableSize));
  stringTableRemain := stringTableSize^ - sizeof (stringTableSize);

  ist := PAnsichar (stringTablePtr) + sizeof (stringTableSize);
  if stringTableRemain > 0 then
  begin
    FStringTable := TStringList.Create;

    while stringTableRemain > 0 do
    begin
      len := lstrlenA (ist);
      FStringTable.Add(String (ist));
      Inc(ist, len + 1);
      Dec(stringTableRemain, len+1)
    end
  end;
end;

procedure TPEBase.DecodeSymbolTable(memory: pointer);
var
  prs: PRawSymbol;
  ps: TSymbol;
  symbolName: AnsiString;
  stringTableSize: PDWORD;
  stringTablePtr: PByte;
  i: Integer;
begin
  ClearSymbolTable;
  if CoffHeader.NumberOfSymbols = 0 then
    Exit;

  FSymbolTable := TList.Create;
  FSymbolTableIndex := TList.Create;

  stringTableSize := PDWORD (DWORD (memory) + CoffHeader.PointerToSymbolTable + CoffHeader.NumberOfSymbols * sizeof (TRawSymbol));
  stringTablePtr := PByte (Integer (stringTableSize));

  prs := PRawSymbol (DWORD (memory) + CoffHeader.PointerToSymbolTable);
  i := 0;
  while DWORD (i) < CoffHeader.NumberOfSymbols do
  begin
    if prs^.Name.Zeros = 0 then
      symbolName := PAnsiChar (stringTablePtr) + prs^.Name.Offset
    else
    begin
      SetString (symbolName, prs^.Name.ShortName, sizeof (prs^.Name.ShortName));
      symbolName := PansiChar (symbolName)
    end;

    ps := TSymbol.Create (symbolName, i, prs^.Value, prs^.SectionNumber, prs^._type, prs^.StorageClass);
    FSymbolTable.Add(ps);
    FSymbolTableIndex.Add (ps);

    while prs^.NumberOfAuxSymbols > 0 do
    begin
      FSymbolTableIndex.Add (ps);
      Inc(prs);
      Inc(i)
    end;

    Inc(prs);
    Inc(i);
  end
end;

destructor TPEBase.Destroy;
begin
  FSectionList.Free;
  ClearSymbolTable;
  inherited;
end;

procedure TPEBase.Encode;
begin
// stub
end;

function TPEBase.GetCodeSize: Integer;
begin
  Result := GetSectionSize (IMAGE_SCN_CNT_CODE);
end;

function TPEBase.GetCOFFHeader: TImageFileHeader;
begin
  Result := FCOFFHeader
end;

function TPEBase.GetFirstSectionWithCharacteristics(
  characteristicsMask: DWORD): TImageSection;
var
  i: Integer;
  section: TImageSection;
begin
  Result := nil;
  for i := 0 to ImageSectionCount - 1 do
  begin
    section := ImageSection [i];

    if (section.SectionHeader.Characteristics and characteristicsMask) <> 0 then
    begin
      Result := section;
      break
    end
  end
end;

function TPEBase.GetIDataSize: Integer;
begin
  Result := GetSectionSize (IMAGE_SCN_CNT_INITIALIZED_DATA);
end;

function TPEBase.GetImageSection(index: Integer): TImageSection;
begin
  Result := TImageSection (FSectionList [index]);
end;

function TPEBase.GetImageSectionCount: Integer;
begin
  Result := FSectionList.Count
end;

function TPEBase.GetSectionByName(const Name: AnsiString): TImageSection;
var
  i: Integer;
begin
  Result := Nil;
  for i:= 0 to ImageSectionCount - 1 do
    if Name = ImageSection [i].GetSectionName then
    begin
      Result := ImageSection [i];
      break
    end
end;

function TPEBase.GetSectionSize(characteristicsMask: DWORD): Integer;
var
  i: Integer;
  section: TImageSection;
begin
  Result := 0;
  for i := 0 to ImageSectionCount - 1 do
  begin
    section := ImageSection [i];

    if (section.SectionHeader.Characteristics and characteristicsMask) <> 0 then
      Inc(Result, section.FSectionHeader.Misc.VirtualSize)
  end
end;

function TPEBase.GetStringTableCount: Integer;
begin
  if FStringTable = Nil then
    Result := 0
  else
    Result := FStringTable.Count
end;

function TPEBase.GetStringTableEntry(idx: Integer): AnsiString;
begin
  if FStringTable <> Nil then
    Result := AnsiString (FStringTable [idx])
  else
    Result := AnsiString (FStringTable [idx])
end;

function TPEBase.GetSymbol(idx: Integer): TSymbol;
begin
  if FSymbolTable <> Nil then
    Result := TSymbol (FSymbolTable [idx])
  else
    Result := Nil
end;

function TPEBase.GetSymbolCount: Integer;
begin
  if FSymbolTable <> Nil then
    Result := FSymbolTable.Count
  else
    Result := 0
end;

function TPEBase.GetSymbolTableIndex: TList;
begin
  SymbolCount;
  Result := FSymbolTableIndex
end;

function TPEBase.GetUDataSize: Integer;
begin
  Result := GetSectionSize (IMAGE_SCN_CNT_UNINITIALIZED_DATA);
end;

procedure TPEBase.LoadFromFile(const Name: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create (Name, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (f)
  finally
    f.Free
  end
end;

procedure TPEBase.LoadFromStream(s: TStream);
var
  m: TMemoryStream;
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
  ASectionNumber, AType: Word; AStorageClass: Byte);
begin
  FName := ASymbolName;
  FIndex := AIndex;
  FValue := AValue;
  FSectionNumber := ASectionNumber;
  FType := AType;
  FStorageClass := AStorageClass;
end;

{ TXnImageOptionalHeader }

destructor TXnImageOptionalHeader.Destroy;
begin
  FreeMem(FIOH32);
  FreeMem(FIOH64);
  inherited;
end;

function TXnImageOptionalHeader.GetAddressOfEntryPoint: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.AddressOfEntryPoint
  else
    Result := FIOH32.AddressOfEntryPoint;
end;

function TXnImageOptionalHeader.GetBaseOfCode: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.BaseOfCode
  else
    Result := FIOH32.BaseOfCode;
end;

function TXnImageOptionalHeader.GetCheckSum: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.CheckSum
  else
    Result := FIOH32.CheckSum;
end;

function TXnImageOptionalHeader.GetDataDirectory(
  Index: Cardinal): TImageDataDirectory;
begin
  if FIsPE32Plus then
    Result := FIOH64.DataDirectory[Index]
  else
    Result := FIOH32.DataDirectory[Index]
end;

function TXnImageOptionalHeader.GetDllCharacteristics: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.DllCharacteristics
  else
    Result := FIOH32.DllCharacteristics;
end;

function TXnImageOptionalHeader.GetFileAlignment: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.FileAlignment
  else
    Result := FIOH32.FileAlignment;
end;

function TXnImageOptionalHeader.GetPE32PlusImageBase: ULONGLONG;
begin
  if not FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyVAlidForPE32Plus,
      ['PE32PlusImageBase']);

  Result := FIOH64.ImageBase
end;

function TXnImageOptionalHeader.GetLoaderFlags: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.LoaderFlags
  else
    Result := FIOH32.LoaderFlags;
end;

function TXnImageOptionalHeader.GetMagic: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.Magic
  else
    Result := FIOH32.Magic;
end;

function TXnImageOptionalHeader.GetMagicAsString: string;
begin
  if FIsPE32Plus then
    Result := 'PE32+'
  else
    Result := 'PE32';
end;

function TXnImageOptionalHeader.GetMajorImageVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MajorImageVersion
  else
    Result := FIOH32.MajorImageVersion;
end;

function TXnImageOptionalHeader.GetMajorLinkerVersion: Byte;
begin
  if FIsPE32Plus then
    Result := FIOH64.MajorLinkerVersion
  else
    Result := FIOH32.MajorLinkerVersion;
end;

function TXnImageOptionalHeader.GetMajorOperatingSystemVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MajorOperatingSystemVersion
  else
    Result := FIOH32.MajorOperatingSystemVersion;
end;

function TXnImageOptionalHeader.GetMajorSubsystemVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MajorSubsystemVersion
  else
    Result := FIOH32.MajorSubsystemVersion;
end;

function TXnImageOptionalHeader.GetMinorImageVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MinorImageVersion
  else
    Result := FIOH32.MinorImageVersion;
end;

function TXnImageOptionalHeader.GetMinorLinkerVersion: Byte;
begin
  if FIsPE32Plus then
    Result := FIOH64.MinorLinkerVersion
  else
    Result := FIOH32.MinorLinkerVersion;
end;

function TXnImageOptionalHeader.GetMinorOperatingSystemVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MinorOperatingSystemVersion
  else
    Result := FIOH32.MinorOperatingSystemVersion;
end;

function TXnImageOptionalHeader.GetMinorSubsystemVersion: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.MinorSubsystemVersion
  else
    Result := FIOH32.MinorSubsystemVersion;
end;

function TXnImageOptionalHeader.GetNumberOfRvaAndSizes: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.NumberOfRvaAndSizes
  else
    Result := FIOH32.NumberOfRvaAndSizes;
end;

function TXnImageOptionalHeader.GetPE32BaseOfData: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32, ['BaseOfData']);
  Result := FIOH32.BaseOfData;
end;

function TXnImageOptionalHeader.GetPE32ImageBase: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32,
      ['PE32ImageBase']);

  Result := FIOH32.ImageBase
end;

function TXnImageOptionalHeader.GetRawData: Pointer;
begin
  if FIsPE32Plus then
    Result := FIOH64
  else
    Result := FIOH32;
end;

function TXnImageOptionalHeader.GetSectionAlignment: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SectionAlignment
  else
    Result := FIOH32.SectionAlignment;
end;

function TXnImageOptionalHeader.GetSizeOfCode: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SizeOfCode
  else
    Result := FIOH32.SizeOfCode;
end;

function TXnImageOptionalHeader.GetSizeOfHeaders: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SizeOfHeaders
  else
    Result := FIOH32.SizeOfHeaders;
end;

function TXnImageOptionalHeader.GetPE32PlusSizeOfHeapCommit: ULONGLONG;
begin
  if not FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyVAlidForPE32Plus,
      ['PE32PlusSizeOfHeapCommit']);
  Result := FIOH64.SizeOfHeapCommit
end;

function TXnImageOptionalHeader.GetPE32PlusSizeOfHeapReserve: ULONGLONG;
begin
  if not FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyVAlidForPE32Plus,
      ['PE32PlusSizeOfHeapReserve']);

  Result := FIOH64.SizeOfHeapReserve
end;

function TXnImageOptionalHeader.GetSizeOfImage: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SizeOfImage
  else
    Result := FIOH32.SizeOfImage;
end;

function TXnImageOptionalHeader.GetSizeOfInitializedData: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SizeOfInitializedData
  else
    Result := FIOH32.SizeOfInitializedData;
end;

function TXnImageOptionalHeader.GetPE32PlusSizeOfStackCommit: ULONGLONG;
begin
  if not FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyVAlidForPE32Plus,
      ['PE32PlusSizeOfStackCommit']);

  Result := FIOH64.SizeOfStackCommit
end;

function TXnImageOptionalHeader.GetPE32PlusSizeOfStackReserve: ULONGLONG;
begin
  if not FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyVAlidForPE32Plus,
      ['PE32PlusSizeOfStackReserve']);

  Result := FIOH64.SizeOfStackReserve
end;

function TXnImageOptionalHeader.GetPE32SizeOfHeapCommit: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32,
      ['PE32SizeOfHeapCommit']);

  Result := FIOH32.SizeOfHeapCommit;
end;

function TXnImageOptionalHeader.GetPE32SizeOfHeapReserve: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32,
      ['PE32SizeOfHeapReserve']);

  Result := FIOH32.SizeOfHeapReserve;
end;

function TXnImageOptionalHeader.GetPE32SizeOfStackCommit: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32,
      ['PE32SizeOfStackCommit']);

  Result := FIOH32.SizeOfStackCommit;
end;

function TXnImageOptionalHeader.GetPE32SizeOfStackReserve: DWORD;
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32,
      ['PE32SizeOfStackReserve']);

  Result := FIOH32.SizeOfStackReserve;
end;

function TXnImageOptionalHeader.GetSizeOfUninitializedData: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.SizeOfUninitializedData
  else
    Result := FIOH32.SizeOfUninitializedData;
end;

function TXnImageOptionalHeader.GetSubsystem: Word;
begin
  if FIsPE32Plus then
    Result := FIOH64.Subsystem
  else
    Result := FIOH32.Subsystem;
end;

function TXnImageOptionalHeader.GetWin32VersionValue: DWORD;
begin
  if FIsPE32Plus then
    Result := FIOH64.Win32VersionValue
  else
    Result := FIOH32.Win32VersionValue;
end;

procedure TXnImageOptionalHeader.SetAddressOfEntryPoint(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.AddressOfEntryPoint := Value
  else
    FIOH32.AddressOfEntryPoint := Value;
end;

procedure TXnImageOptionalHeader.SetBaseOfCode(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.BaseOfCode := Value
  else
    FIOH32.BaseOfCode := Value;
end;

procedure TXnImageOptionalHeader.SetCheckSum(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.CheckSum := Value
  else
    FIOH32.CheckSum := Value;
end;

procedure TXnImageOptionalHeader.SetDataDirectory(Index: Cardinal;
  const Value: TImageDataDirectory);
begin
  if FIsPE32Plus then
    FIOH64.DataDirectory[Index] := Value
  else
    FIOH32.DataDirectory[Index] := Value;
end;

procedure TXnImageOptionalHeader.SetPE32BaseOfData(const Value: DWORD);
begin
  if FIsPE32Plus then
    raise EPEException.CreateFmt(rstPropertyOnlyValidForPE32, ['BaseOfData']);
  FIOH32.BaseOfData := Value;
end;

procedure TXnImageOptionalHeader.SetSizeOfCode(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.SizeOfCode := Value
  else
    FIOH32.SizeOfCode := Value;
end;

procedure TXnImageOptionalHeader.SetSizeOfHeaders(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.SizeOfHeaders := Value
  else
    FIOH32.SizeOfHeaders := Value;
end;

procedure TXnImageOptionalHeader.SetSizeOfImage(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.SizeOfImage := Value
  else
    FIOH32.SizeOfImage := Value;
end;

procedure TXnImageOptionalHeader.SetSizeOfInitializedData(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.SizeOfInitializedData := Value
  else
    FIOH32.SizeOfInitializedData := Value;
end;

procedure TXnImageOptionalHeader.SetSizeOfUninitializedData(const Value: DWORD);
begin
  if FIsPE32Plus then
    FIOH64.SizeOfUninitializedData := Value
  else
    FIOH32.SizeOfUninitializedData := Value;
end;

constructor TXnImageOptionalHeader.Create(ImageOptionalHeader: Pointer;
  Size: Integer);
var
  Magic: Word;
begin
  inherited Create;

  Magic := PWord(ImageOptionalHeader)^;

  FIsPE32Plus := Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;

  if FIsPE32Plus then
  begin
    FIOH32 := nil;

    GetMem(FIOH64, Size);
    Move(ImageOptionalHeader^, FIOH64^, Size);

    FCheckSumOffset := UIntPtr(@(FIOH64^.CheckSum)) - UIntPtr(FIOH64);
  end
  else
  begin
    FIOH64 := nil;

    GetMem(FIOH32, Size);
    Move(ImageOptionalHeader^, FIOH32^, Size);

    FCheckSumOffset := UIntPtr(@(FIOH32^.CheckSum)) - UIntPtr(FIOH32);
  end;
end;

end.
