unit unitPEModuleRW;

interface

uses
  Windows, Classes, SysUtils, unitPEFile;

type
  TImportFunction = class
  private
    FFixups: TList;
    FFunctionName: string;
  public
    constructor Create (const AFunctionName: string);
    destructor Destroy; override;
  end;

  TImportDLL = class
  private
    FImportFunctions: TStrings;
    FDLLname: string;
  public
    constructor Create (const ADLLName: string);
    destructor Destroy; override;
  end;

  TImportSection = class (TImageSection)
  private
    FImportDLLS: TStrings;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Fixup; override;
  end;

  TRelocSection = class (TImageSection)
  private
    FRelocs: TList;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Fixup; override;
  end;

  TFixup = record
    OffsetInCode: DWORD;
    Section: TImageSection;
  end;
  PFixup = ^TFixup;

  TPEModuleRW = class (TPEResourceModule)
  private
    FFixups: array of TFixup;
    FFixupsCount: DWORD;
    FCodeSectionIdx: Integer;
    FDataSectionIdx: Integer;
    FUDataSectionIdx: Integer;

  protected
    procedure Decode (memory: pointer; exeSize: Integer); override;
    procedure AddFixup (ACodeOffset: DWORD; ASection: TImageSection);
    procedure AddReloc (ACodeOffset: DWORD);
    procedure ApplyGlobalFixups; override;
  public
    constructor Create;
    procedure Initialize (IsExe, IsGUI: Boolean);

    function GetCodeSection: TImageSection;
    function GetDataSection: TImageSection;
    function GetUDataSection: TImageSection;

    function Emit(Section: TImageSection; data: array of byte): DWORD; overload;
    function Emit(Section: TImageSection; const data: string): DWORD; overload;

    procedure EmitRelocPtr (Section: TImageSection; ptr: DWORD; destSection: TImageSection);
    procedure EmitAPICall (Section: TImageSection; const dllName, functionName: string);

    function AddOrGetImportFunction (const dllName, functionName: string): TImportFunction;
  end;

implementation

uses DateUtils;

const
  DOSStubInit: array [0..255] of byte =
  ($4D,$5A,$50,$00,$02,$00,$00,$00,$04,$00,$0F,$00,$FF,$FF,$00,$00,  // MZP.............
   $B8,$00,$00,$00,$00,$00,$00,$00,$40,$00,$1A,$00,$00,$00,$00,$00,  // ........@.......
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,  // ................
   $BA,$10,$00,$0E,$1F,$B4,$09,$CD,$21,$B8,$01,$4C,$CD,$21,$90,$90,  // ........!..L.!..
   $54,$68,$69,$73,$20,$70,$72,$6F,$67,$72,$61,$6D,$20,$6D,$75,$73,  // This program mus
   $74,$20,$62,$65,$20,$72,$75,$6E,$20,$75,$6E,$64,$65,$72,$20,$57,  // t be run under W
   $69,$6E,$33,$32,$0D,$0A,$24,$37,$00,$00,$00,$00,$00,$00,$00,$00,  // in32..$7........
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,  // ................
   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00); // ................

  EXECharacteristics: array [Boolean] of DWORD = (IMAGE_FILE_DLL, 0);
  DefaultImageBase: array [Boolean] of DWORD = ($10000000, $400000);
  Subsystems: array [Boolean, Boolean] of DWORD = ((IMAGE_SUBSYSTEM_NATIVE, IMAGE_SUBSYSTEM_NATIVE),
                                                    (IMAGE_SUBSYSTEM_WINDOWS_CUI, IMAGE_SUBSYSTEM_WINDOWS_GUI));

{ TPEModuleRW }

function TPEModuleRW.Emit(Section: TImageSection; data: array of byte): DWORD;
begin
  Result := Section.RawData.Position;
  Section.RawData.Write(data [0], length (data))
end;

procedure TPEModuleRW.AddFixup(ACodeOffset: DWORD; ASection: TImageSection);
var
  FixupLen: DWORD;
begin
  FixupLen := Length (FFixups);
  if FixupLen = FFixupsCount then
    SetLength (FFixups, FixupLen + 1024);

  with FFixups [FFixupsCount] do
  begin
    OffsetInCode := ACodeOffset;
    Section := ASection;
  end;
  Inc(FFixupsCount);
  AddReloc (ACodeOffset)
end;

function TPEModuleRW.AddOrGetImportFunction(const dllName,
  functionName: string): TImportFunction;
var
  isection: TImportSection;
  dll: TImportDLL;
  Func: TImportFunction;
  i: Integer;
begin
  Result := Nil;
  iSection := GetSectionByName ('.idata') as TImportSection;
  if iSection = Nil then Exit;

  i := iSection.FImportDLLS.IndexOf(dllName);
  if i >= 0 then
    dll := TImportDll (iSection.FImportDLLS.Objects [i])
  else
  begin
    dll := TImportDll.Create(dllName);
    iSection.FImportDLLS.AddObject(dllName, dll)
  end;

  i := dll.FImportFunctions.IndexOf(functionName);
  if i >= 0 then
    Func := TImportFunction (dll.FImportFunctions.Objects [i])
  else
  begin
    Func := TImportFunction.Create(functionName);
    dll.FImportFunctions.AddObject(functionName, Func)
  end;

  Result := Func
end;

procedure TPEModuleRW.AddReloc(ACodeOffset: DWORD);
var
  rSection: TRelocSection;
begin
  rSection := GetSectionByName ('.reloc') as TRelocSection;
  if rSection = Nil then Exit;

  rSection.FRelocs.Add (Pointer (ACodeOffset));
end;

procedure TPEModuleRW.ApplyGlobalFixups;
var
  i: Integer;
  CodeSection: TImageSection;
  Fixup: PFixup;
  fcs: PDWORD;
begin
  if FFixupsCount = 0 then Exit;

  CodeSection := GetCodeSection;

  for i := 0 to FFixupsCount - 1 do
  begin
    Fixup := @FFixups [i];

    fcs := PDWORD (PByte (CodeSection.RawData.Memory) + Fixup^.OffsetInCode);
    Inc(fcs^, Fixup^.Section.SectionHeader.VirtualAddress + fOptionalHeader.PE32_ImageBase);
  end
end;

constructor TPEModuleRW.Create;
begin
  inherited;
  FCodeSectionIdx := -1;
  FDataSectionIdx := -1;
end;

procedure TPEModuleRW.Decode(memory: pointer; exeSize: Integer);
const
  IDATA = IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
  UDATA = IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
var
  s: TImageSection;
  i: Integer;
begin
  inherited;

  for i := 0 to ImageSectionCount - 1 do
  begin
    s := ImageSection [i];

    if (FCodeSectionIdx = -1) and ((s.SectionHeader.Characteristics and IMAGE_SCN_CNT_CODE) = IMAGE_SCN_CNT_CODE) then
      FCodeSectionIdx := i;

    if (FDataSectionIdx = -1) and ((s.SectionHeader.Characteristics and IDATA) = IDATA) then
      FDataSectionIdx := i;

    if (FUDataSectionIdx = -1) and ((s.SectionHeader.Characteristics and UDATA) = UDATA) then
      FUDataSectionIdx := i
  end
end;

function TPEModuleRW.Emit(Section: TImageSection; const data: string): DWORD;
begin
  Result := Section.RawData.Position;
  Section.RawData.Write(data [1], length (data))
end;

procedure TPEModuleRW.EmitAPICall(Section: TImageSection; const dllName,
  functionName: string);
var
  Func: TImportFunction;
  offset: DWORD;
begin
  Func := AddOrGetImportFunction (dllName, functionName);
  Emit(Section, [$ff, $15]);             // opcode for CALL DWORD PTR
  offset := Section.RawData.Position;

  Emit(Section, [$00, $00, $00, $00]);
   //               ^    ^    ^    ^
   // Address of the entry for this function in the IAT =
   //   module base address (eg. 400000) +
   //   rva of import Section +
   //   offset(in import Section) of this function's IAT entry.
   //
   // Because we don't know the RVA of the import section or the offset of
   // the IAT within the import Section yet we have to fix this up later.
   //
   // TODO:
   //
   // Maybe make the import Section the first Section (so it's RVA would always
   // be $1000), and move the IAT to the beginning of the import Section.  That
   // way we could generate the address here, rather than resort to fixups

  Func.FFixups.Add(Pointer (offset));
end;

procedure TPEModuleRW.EmitRelocPtr(Section: TImageSection; ptr: DWORD;
  destSection: TImageSection);
var
  b: array [0..3] of byte absolute ptr;
  co: DWORD;
begin
  co := Section.RawData.Position;
  Emit(Section, b);
  AddFixup (co, destSection);
end;

function TPEModuleRW.GetCodeSection: TImageSection;
begin
  Result := ImageSection [FCodeSectionIdx];
end;

function TPEModuleRW.GetDataSection: TImageSection;
begin
  Result := ImageSection [FDataSectionIdx];
end;

function TPEModuleRW.GetUDataSection: TImageSection;
begin
  Result := ImageSection [FUDataSectionIdx];
end;

procedure TPEModuleRW.Initialize(IsExe, IsGUI: Boolean);
var
  dt: TDateTime;
  st: TSystemTime;
  optionalHeaderSize: DWORD;
  OptionalHeader: PImageOptionalHeader32;
begin
  Move (DOSStubInit [0], fDOSHeader, sizeof (fDOSHeader));

  DOSStub.Clear;
  DOSStub.Write(DOSStubInit [sizeof (fDOSHeader)], sizeof (DOSStub) - sizeof (fDOSHeader));

  FillChar (fCOFFHeader, sizeof (fCOFFHeader), 0);
  fCOFFHeader.Machine := IMAGE_FILE_MACHINE_I386;

  GetSystemTime (st);
  with st do
    dt := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);


  fSectionList.Clear;

  optionalHeaderSize := Integer (@(PImageOptionalHeader32 (0)^.DataDirectory));
  Inc(optionalHeaderSize, NUM_DATA_DIRECTORIES * sizeof (TImageDataDirectory));

  fCOFFHeader.TimeDateStamp := SecondsBetween (dt, UnixDateDelta);
  fCOFFHeader.NumberOfSections := fSectionList.Count;

  fCOFFHeader.Characteristics := IMAGE_FILE_BYTES_REVERSED_HI or IMAGE_FILE_BYTES_REVERSED_LO
                                 or IMAGE_FILE_32BIT_MACHINE or IMAGE_FILE_EXECUTABLE_IMAGE
                                 or IMAGE_FILE_LOCAL_SYMS_STRIPPED or IMAGE_FILE_LINE_NUMS_STRIPPED
                                 or EXECharacteristics [IsExe];

  fCOFFHeader.PointerToSymbolTable := 0;  // Symbol table rarely used these days (Pietrick)
  fCOFFHeader.NumberOfSymbols := 0;
  fCOFFHeader.SizeOfOptionalHeader := optionalHeaderSize;

  ReallocMem (OptionalHeader, optionalHeaderSize);
  ZeroMemory (OptionalHeader, optionalHeaderSize);

  OptionalHeader.Magic := IMAGE_NT_OPTIONAL_HDR32_MAGIC;
  OptionalHeader.MajorLinkerVersion := PE_LINKER_VERSION_HI;
  OptionalHeader.MinorLinkerVersion := PE_LINKER_VERSION_LO;

  OptionalHeader.SizeOfCode := 0;               // Filled in by encode
  OptionalHeader.SizeOfInitializedData := 0;    //   "     "  "    "
  OptionalHeader.SizeOfUninitializedData := 0;  //   "     "  "    "

  OptionalHeader.AddressOfEntryPoint := 0;
  OptionalHeader.BaseOfCode := 0;                //   "     "  "    "
  OptionalHeader.BaseOfData := 0;                //   "     "  "    "
  OptionalHeader.ImageBase := DefaultImageBase [IsExe];
  OptionalHeader.SectionAlignment := $1000;
  OptionalHeader.FileAlignment := $200;

                                          // Pietrek
  OptionalHeader.MajorOperatingSystemVersion := 5;
  OptionalHeader.MinorOperatingSystemVersion := 0;
  OptionalHeader.MajorImageVersion := 0;
  OptionalHeader.MinorImageVersion := 0;
  OptionalHeader.MajorSubsystemVersion := 4;
  OptionalHeader.MinorSubsystemVersion := 0;
  OptionalHeader.Win32VersionValue := 0;

  OptionalHeader.SizeOfImage := 0;    // Filled in by Encode
  OptionalHeader.SizeOfHeaders := 0;  //   "     "  "   "

  OptionalHeader.CheckSum := 0;       //   "     "  "    "
  OptionalHeader.Subsystem := Subsystems [IsExe, IsGUI];
  OptionalHeader.DllCharacteristics := 0;

  OptionalHeader.SizeOfStackReserve := $00100000;
  OptionalHeader.SizeOfStackCommit  := $00004000;
  OptionalHeader.SizeOfHeapReserve  := $00100000;
  OptionalHeader.SizeOfHeapCommit   := $00004000;
  OptionalHeader.LoaderFlags := 0;

  OptionalHeader.NumberOfRvaAndSizes := NUM_DATA_DIRECTORIES;

  fOptionalHeader := TXnImageOptionalHeader.Create(OptionalHeader, optionalHeaderSize);

  FreeMem(OptionalHeader, optionalHeaderSize);

  fSectionList.Add(TImportSection.CreateEmpty(self, '.idata', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, IMAGE_DIRECTORY_ENTRY_IMPORT));

  FCodeSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.text', IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_MEM_READ, $ffffffff));
  FDataSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.data', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, $ffffffff));
  FUDataSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.udata', IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, $ffffffff));

  fSectionList.Add(TImageSection.CreateEmpty(self, '.tls',  IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, IMAGE_DIRECTORY_ENTRY_TLS));
  fSectionList.Add(TRelocSection.CreateEmpty(self, '.reloc', IMAGE_SCN_MEM_DISCARDABLE or IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ, IMAGE_DIRECTORY_ENTRY_BASERELOC));
  fSectionList.Add(TImageSection.CreateEmpty(self, '.rsrc', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ, IMAGE_DIRECTORY_ENTRY_RESOURCE));
end;

{ TImportFunction }

constructor TImportFunction.Create(const AFunctionName: string);
begin
  FFixups := TList.Create;
  FFunctionName := AFunctionName
end;

destructor TImportFunction.Destroy;
begin
  FFixups.Free;

  inherited;
end;

{ TImportDLL }

constructor TImportDLL.Create(const ADLLName: string);
begin
  FDLLname := ADLLName;
  FImportFunctions := TStringList.Create;
  TStringList(FImportFunctions).CaseSensitive := False
end;

destructor TImportDLL.Destroy;
var
  i: Integer;
begin
  for i := 0 to FImportFunctions.Count - 1 do
    FImportFunctions.Objects [i].Free;
  FImportFunctions.Free;

  inherited;
end;

{ TImportSection }

destructor TImportSection.Destroy;
var
  i: Integer;
begin
  for i := 0 to FImportDLLS.Count - 1 do
    FImportDLLS.Objects [i].Free;

  FImportDLLS.Free;

  inherited;
end;

procedure TImportSection.Fixup;
var
  size, nameOffset, iatOffset, iatSize, i, j, k, base, fNameOffset, rva: Integer;
  pd: PImageImportDescriptor;
  piat, prevPiat: PDWORD;
  fn: TImportFunction;
  dll: TImportDll;
  pNames: PAnsiChar;
  adllname: AnsiString;
  codeOffset: Integer;
  CodeSection: TImageSection;
  parentImage: TPEModule;
  DataDir: TImageDataDirectory;
begin
  parentImage := parent as TPEModule;
  CodeSection := TPEModuleRW (Parent).GetCodeSection;
  rva := SectionHeader.VirtualAddress;
  size := sizeof (TImageImportDescriptor) * (FImportDLLS.Count+1);
  iatOffset := size;

  iatSize := 0;
  for i := 0 to FImportDLLS.Count - 1 do
  begin
    dll := TImportDll (FImportDLLS.Objects [i]);
    Inc(size, (dll.FImportFunctions.Count + 1) * 2 * sizeof (DWORD));
    Inc(iatSize, (dll.FImportFunctions.Count + 1) * sizeof (DWORD));
  end;

  nameOffset := size;

  fnameOffset := nameOffset;

  for i := 0 to FImportDLLS.Count - 1 do
  begin
    dll := TImportDll (FImportDLLS.Objects [i]);
    Inc(size, Length (dll.FDLLname) + 1);
    Inc(fNameOffset, Length (dll.FDLLname) + 1);

    for j := 0 to dll.FImportFunctions.Count - 1 do
    begin
      fn := TImportFunction (dll.FImportFunctions.Objects [j]);
      Inc(size, sizeof (Word) + Length (fn.FFunctionName) + 1);
    end
  end;

  fRawData.Size := size;

  base := Integer (fRawData.Memory);
  pd := PImageImportDescriptor (base);
  piat := PDWORD (base + iatOffset);
  pnames := PAnsiChar (base + nameOffset);

  for i := 0 to FImportDLLS.Count - 1 do
  begin
    dll := TImportDll (FImportDLLS.Objects [i]);

    pd^.TimeDateStamp := 0;
    pd^.ForwarderChain := 0;
    pd^.Name := Integer (pnames) - base + rva;
    adllname := AnsiString (dll.FDLLname);
    Move (adllname [1], pnames^, Length (dll.FDLLname) + 1);
    Inc(pNames, Length (dll.FDLLname) + 1);

    prevPiat := piat;
    pd^.FirstThunk := Integer (piat) - base + rva;
    for j := 0 to dll.FImportFunctions.Count - 1 do
    begin
      fn := TImportFunction (dll.FImportFunctions.Objects [j]);
      piat^ := fnameOffset+rva;


      for k := 0 to fn.FFixups.Count - 1 do
      begin
        codeOffset := Integer (fn.FFixups [k]);
        PDWORD (PByte (CodeSection.RawData.Memory) + codeOffset)^ := Integer (piat) - base + rva + Integer (parentImage.OptionalHeader.PE32_ImageBase);
        TPEModuleRW (Parent).AddReloc (codeOffset);
      end;

      Inc(fNameOffset, sizeof (Word));
      adllname := AnsiString (fn.FFunctionName);
      Move (adllname [1], PAnsiChar (base + fNameOffset)^, Length (fn.FFunctionName) + 1);
      Inc(fNameOffset, Length (fn.FFunctionName) + 1);
      Inc(piat);
    end;
    piat^ := 0;
    Inc(piat);

    pd^.Characteristics := Integer (piat) - base + rva;
    for j := 0 to dll.FImportFunctions.Count - 1 do
    begin
      piat^ := prevPiat^;
      Inc(prevPiat);
      Inc(piat);
    end;
    piat^ := 0;
    Inc(piat);

    Inc(pd);
  end;

  FillChar (pd^, sizeof (pd^), 0);

  DataDir := ParentImage.DataDirectory [IMAGE_DIRECTORY_ENTRY_IAT];
  DataDir.VirtualAddress := iatOffset + rva;
  DataDir.Size := iatSize;
  ParentImage.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT] := DataDir;
end;

procedure TImportSection.Initialize;
begin
  FImportDLLS := TStringList.Create;
  TStringList(FImportDLLS).CaseSensitive := False
end;

{ TRelocSection }

destructor TRelocSection.Destroy;
begin
  FRelocs.Free;

  inherited;
end;

function CompareRelocs (p1, p2: Pointer): Integer;
begin
  Result := DWORD(p1) - DWORD(p2);
end;

const
  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGHLOW = 3;

type
  TImageBaseRelocation = record
    RVA: DWORD;
    Size: DWORD;
  end;
  PImageBaseRelocation = ^TImageBaseRelocation;

procedure TRelocSection.Fixup;
var
  reloc, block, ofs, csrva: DWORD;
  i: Integer;
  pHdr: PImageBaseRelocation;
  hdr: TImageBaseRelocation;
  w: Word;
begin
  FRelocs.Sort(CompareRelocs);

  fRawData.Clear;
  pHdr := Nil;
  csrva := TPEModuleRW (Parent).GetCodeSection.SectionHeader.VirtualAddress;

  for i := 0 to FRelocs.Count - 1 do
  begin
    reloc := DWORD (FRelocs [i]) + csrva;

    block := reloc shr 4;
    ofs := reloc and $fff;

    if (pHdr = Nil) or (pHdr^.RVA <> block) then
    begin
      if pHdr <> Nil then
      begin
        if hdr.Size mod sizeof (DWORD) <> 0 then
        begin
          w := IMAGE_REL_BASED_ABSOLUTE shl 12 + 0;
          fRawData.Write (w, sizeof (w));
          Inc(pHdr^.Size, sizeof (w));
        end
      end;

      hdr.RVA := block;
      hdr.Size := sizeof (TImageBaseRelocation);
      fRawData.Write(hdr, sizeof (hdr));
      pHdr := PImageBaseRelocation (PByte (fRawData.Memory) + fRawData.Position - sizeof (hdr));
    end;

    w := IMAGE_REL_BASED_HIGHLOW shl 12 + ofs;
    fRawData.Write (w, sizeof (w));
    Inc(pHdr^.Size, sizeof (w));
  end
end;

procedure TRelocSection.Initialize;
begin
  inherited;

  FRelocs := TList.Create;
end;

end.
