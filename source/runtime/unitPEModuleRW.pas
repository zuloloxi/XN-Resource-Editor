unit unitPEModuleRW;

interface

uses Windows, Classes, SysUtils, unitPEFile;

type
TImportFunction = class
private
  fFixups : TList;
  fFunctionName : string;
public
  constructor Create (const AFunctionName : string);
  destructor Destroy; override;
end;

TImportDLL = class
private
  fImportFunctions : TStrings;
  fDLLname : string;
public
  constructor Create (const ADLLName : string);
  destructor Destroy; override;
end;

TImportSection = class (TImageSection)
private
  fImportDLLS : TStrings;
public
  destructor Destroy; override;
  procedure Initialize; override;
  procedure Fixup; override;
end;

TRelocSection = class (TImageSection)
private
  fRelocs : TList;
public
  destructor Destroy; override;
  procedure Initialize; override;
  procedure Fixup; override;
end;

TFixup = record
  OffsetInCode : DWORD;
  Section : TImageSection;
end;
PFixup = ^TFixup;

TPEModuleRW = class (TPEResourceModule)
private
  fFixups : array of TFixup;
  fFixupsCount : DWORD;
  fCodeSectionIdx : Integer;
  fDataSectionIdx : Integer;
  fUDataSectionIdx : Integer;

protected
  procedure Decode (memory : pointer; exeSize : Integer); override;
  procedure AddFixup (ACodeOffset : DWORD; ASection : TImageSection);
  procedure AddReloc (ACodeOffset : DWORD);
  procedure ApplyGlobalFixups; override;
public
  constructor Create;
  procedure Initialize (IsExe, IsGUI : boolean);

  function GetCodeSection : TImageSection;
  function GetDataSection : TImageSection;
  function GetUDataSection : TImageSection;

  function Emit (section : TImageSection; data : array of byte) : DWORD; overload;
  function Emit (section : TImageSection; const data : string) : DWORD; overload;

  procedure EmitRelocPtr (section : TImageSection; ptr : DWORD; destSection : TImageSection);

  procedure EmitAPICall (section : TImageSection; const dllName, functionName : string);

  function AddOrGetImportFunction (const dllName, functionName : string) : TImportFunction;

end;

implementation

uses DateUtils;

const
  DOSStubInit : array [0..255] of byte =
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

  EXECharacteristics : array [boolean] of DWORD = (IMAGE_FILE_DLL, 0);
  DefaultImageBase : array [boolean] of DWORD = ($10000000, $400000);
  Subsystems : array [boolean, boolean] of DWORD = ((IMAGE_SUBSYSTEM_NATIVE, IMAGE_SUBSYSTEM_NATIVE),
                                                    (IMAGE_SUBSYSTEM_WINDOWS_CUI, IMAGE_SUBSYSTEM_WINDOWS_GUI));

{ TPEModuleRW }

function TPEModuleRW.Emit(section: TImageSection; data: array of byte): DWORD;
begin
  result := section.RawData.Position;
  section.RawData.Write(data [0], length (data))
end;

procedure TPEModuleRW.AddFixup(ACodeOffset: DWORD; ASection: TImageSection);
var
  fixupLen : DWORD;
begin
  fixupLen := Length (fFixups);
  if fixupLen = fFixupsCount then
    SetLength (fFixups, fixupLen + 1024);

  with fFixups [fFixupsCount] do
  begin
    OffsetInCode := ACodeOffset;
    Section := ASection;
  end;
  Inc (fFixupsCount);
  AddReloc (ACodeOffset)
end;

function TPEModuleRW.AddOrGetImportFunction(const dllName,
  functionName: string): TImportFunction;
var
  isection : TImportSection;
  dll : TImportDLL;
  func : TImportFunction;
  i : Integer;
begin
  result := Nil;
  iSection := GetSectionByName ('.idata') as TImportSection;
  if iSection = Nil then Exit;

  i := iSection.fImportDLLS.IndexOf(dllName);
  if i >= 0 then
    dll := TImportDll (iSection.fImportDlls.Objects [i])
  else
  begin
    dll := TImportDll.Create(dllName);
    iSection.fImportDLLS.AddObject(dllName, dll)
  end;

  i := dll.fImportFunctions.IndexOf(functionName);
  if i >= 0 then
    func := TImportFunction (dll.fImportFunctions.Objects [i])
  else
  begin
    func := TImportFunction.Create(functionName);
    dll.fImportFunctions.AddObject(functionName, func)
  end;

  result := func
end;

procedure TPEModuleRW.AddReloc(ACodeOffset: DWORD);
var
  rSection : TRelocSection;
begin
  rSection := GetSectionByName ('.reloc') as TRelocSection;
  if rSection = Nil then Exit;

  rSection.fRelocs.Add (Pointer (ACodeOffset));
end;

procedure TPEModuleRW.ApplyGlobalFixups;
var
  i : Integer;
  codeSection : TImageSection;
  fixup : PFixup;
  fcs : PDWORD;
begin
  if fFixupsCount = 0 then Exit;

  codeSection := GetCodeSection;

  for i := 0 to fFixupsCount - 1 do
  begin
    fixup := @fFixups [i];

    fcs := PDWORD (PByte (codeSection.RawData.Memory) + fixup^.OffsetInCode);
    Inc (fcs^, fixup^.Section.SectionHeader.VirtualAddress + fOptionalHeader.ImageBase);
  end
end;

constructor TPEModuleRW.Create;
begin
  inherited;
  fCodeSectionIDx := -1;
  fDataSectionIdx := -1;
end;

procedure TPEModuleRW.Decode(memory: pointer; exeSize: Integer);
const
  IDATA = IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
  UDATA = IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE;
var
  s : TImageSection;
  i : Integer;
begin
  inherited;

  for i := 0 to ImageSectionCount - 1 do
  begin
    s := ImageSection [i];

    if (fCodeSectionIdx = -1) and ((s.SectionHeader.Characteristics and IMAGE_SCN_CNT_CODE) = IMAGE_SCN_CNT_CODE) then
      fCodeSectionIdx := i;

    if (fDataSectionIdx = -1) and ((s.SectionHeader.Characteristics and IDATA) = IDATA) then
      fDataSectionIdx := i;

    if (fUDataSectionIdx = -1) and ((s.SectionHeader.Characteristics and UDATA) = UDATA) then
      fUDataSectionIdx := i
  end
end;

function TPEModuleRW.Emit(section: TImageSection; const data: string): DWORD;
begin
  result := section.RawData.Position;
  section.RawData.Write(data [1], length (data))
end;

procedure TPEModuleRW.EmitAPICall(section: TImageSection; const dllName,
  functionName: string);
var
  func : TImportFunction;
  offset : DWORD;
begin
  func := AddOrGetImportFunction (dllName, functionName);
  Emit (section, [$ff, $15]);             // opcode for CALL DWORD PTR
  offset := section.RawData.Position;

  Emit (section, [$00, $00, $00, $00]);
   //               ^    ^    ^    ^
   // Address of the entry for this function in the IAT =
   //   module base address (eg. 400000) +
   //   rva of import section +
   //   offset (in import section) of this function's IAT entry.
   //
   // Because we don't know the RVA of the import section or the offset of
   // the IAT within the import section yet we have to fix this up later.
   //
   // TODO:
   //
   // Maybe make the import section the first section (so it's RVA would always
   // be $1000), and move the IAT to the beginning of the import section.  That
   // way we could generate the address here, rather than resort to fixups

  func.fFixups.Add(Pointer (offset));
end;

procedure TPEModuleRW.EmitRelocPtr(section: TImageSection; ptr: DWORD;
  destSection: TImageSection);
var
  b : array [0..3] of byte absolute ptr;
  co : DWORD;
begin
  co := section.RawData.Position;
  Emit (section, b);
  AddFixup (co, destSection);
end;

function TPEModuleRW.GetCodeSection: TImageSection;
begin
  result := ImageSection [fCodeSectionIdx];
end;

function TPEModuleRW.GetDataSection: TImageSection;
begin
  result := ImageSection [fDataSectionIdx];
end;

function TPEModuleRW.GetUDataSection: TImageSection;
begin
  result := ImageSection [fUDataSectionIdx];
end;

procedure TPEModuleRW.Initialize(IsExe, IsGUI: boolean);
var
  dt : TDateTime;
  st : TSystemTime;
  optionalHeaderSize : DWORD;
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

  optionalHeaderSize := Integer (@(PImageOptionalHeader (0)^.DataDirectory));
  Inc (optionalHeaderSize, NUM_DATA_DIRECTORIES * sizeof (TImageDataDirectory));

  fCOFFHeader.TimeDateStamp := SecondsBetween (dt, UnixDateDelta);
  fCOFFHeader.NumberOfSections := fSectionList.Count;

  fCOFFHeader.Characteristics := IMAGE_FILE_BYTES_REVERSED_HI or IMAGE_FILE_BYTES_REVERSED_LO
                                 or IMAGE_FILE_32BIT_MACHINE or IMAGE_FILE_EXECUTABLE_IMAGE
                                 or IMAGE_FILE_LOCAL_SYMS_STRIPPED or IMAGE_FILE_LINE_NUMS_STRIPPED
                                 or EXECharacteristics [IsExe];

  fCOFFHeader.PointerToSymbolTable := 0;  // Symbol table rarely used these days (Pietrick)
  fCOFFHeader.NumberOfSymbols := 0;
  fCOFFHeader.SizeOfOptionalHeader := optionalHeaderSize;

  ReallocMem (fOptionalHeader, optionalHeaderSize);
  ZeroMemory (fOptionalHeader, optionalHeaderSize);

  fOptionalHeader^.Magic := IMAGE_NT_OPTIONAL_HDR32_MAGIC;
  fOptionalHeader^.MajorLinkerVersion := PE_LINKER_VERSION_HI;
  fOptionalHeader^.MinorLinkerVersion := PE_LINKER_VERSION_LO;

  fOptionalHeader^.SizeOfCode := 0;               // Filled in by encode
  fOptionalHeader^.SizeOfInitializedData := 0;    //   "     "  "    "
  fOptionalHeader^.SizeOfUninitializedData := 0;  //   "     "  "    "

  fOptionalHeader^.AddressOfEntryPoint := 0;
  fOptionalHeader.BaseOfCode := 0;                //   "     "  "    "
  fOptionalHeader.BaseOfData := 0;                //   "     "  "    "
  fOptionalHeader.ImageBase := DefaultImageBase [IsExe];
  fOptionalHeader.SectionAlignment := $1000;
  fOptionalHeader.FileAlignment := $200;

                                          // Pietrek
  fOptionalHeader^.MajorOperatingSystemVersion := 5;
  fOptionalHeader^.MinorOperatingSystemVersion := 0;
  fOptionalHeader^.MajorImageVersion := 0;
  fOptionalHeader^.MinorImageVersion := 0;
  fOptionalHeader^.MajorSubsystemVersion := 4;
  fOptionalHeader^.MinorSubsystemVersion := 0;
  fOptionalHeader^.Win32VersionValue := 0;

  fOptionalHeader^.SizeOfImage := 0;    // Filled in by Encode
  fOptionalHeader^.SizeOfHeaders := 0;  //   "     "  "   "

  fOptionalHeader^.CheckSum := 0;       //   "     "  "    "
  fOptionalHeader^.Subsystem := Subsystems [IsExe, IsGUI];
  fOptionalHeader^.DllCharacteristics := 0;

  fOptionalHeader^.SizeOfStackReserve := $00100000;
  fOptionalHeader^.SizeOfStackCommit  := $00004000;
  fOptionalHeader^.SizeOfHeapReserve  := $00100000;
  fOptionalHeader^.SizeOfHeapCommit   := $00004000;
  fOptionalHeader^.LoaderFlags := 0;

  fOptionalHeader^.NumberOfRvaAndSizes := NUM_DATA_DIRECTORIES;

  fSectionList.Add(TImportSection.CreateEmpty(self, '.idata', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, IMAGE_DIRECTORY_ENTRY_IMPORT));

  fCodeSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.text', IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_MEM_READ, $ffffffff));
  fDataSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.data', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, $ffffffff));
  fUDataSectionIdx := fSectionList.Add(TImageSection.CreateEmpty(self, '.udata', IMAGE_SCN_CNT_UNINITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, $ffffffff));

  fSectionList.Add(TImageSection.CreateEmpty(self, '.tls',  IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_WRITE, IMAGE_DIRECTORY_ENTRY_TLS));
  fSectionList.Add(TRelocSection.CreateEmpty(self, '.reloc', IMAGE_SCN_MEM_DISCARDABLE or IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ, IMAGE_DIRECTORY_ENTRY_BASERELOC));
  fSectionList.Add(TImageSection.CreateEmpty(self, '.rsrc', IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ, IMAGE_DIRECTORY_ENTRY_RESOURCE));
end;

{ TImportFunction }

constructor TImportFunction.Create(const AFunctionName: string);
begin
  fFixups := TList.Create;
  fFunctionName := AFunctionName
end;

destructor TImportFunction.Destroy;
begin
  fFixups.Free;

  inherited;
end;

{ TImportDLL }

constructor TImportDLL.Create(const ADLLName: string);
begin
  fDLLName := ADLLName;
  fImportFunctions := TStringList.Create;
  TStringList (fImportFunctions).CaseSensitive := False
end;

destructor TImportDLL.Destroy;
var
  i : Integer;
begin
  for i := 0 to fImportFunctions.Count - 1 do
    fImportFunctions.Objects [i].Free;
  fImportFunctions.Free;

  inherited;
end;

{ TImportSection }

destructor TImportSection.Destroy;
var
  i : Integer;
begin
  for i := 0 to fImportDlls.Count - 1 do
    fImportDlls.Objects [i].Free;

  fImportDLLs.Free;

  inherited;
end;

procedure TImportSection.Fixup;
var
  size, nameOffset, iatOffset, iatSize, i, j, k, base, fNameOffset, rva : Integer;
  pd : PImageImportDescriptor;
  piat, prevPiat : PDWORD;
  fn : TImportFunction;
  dll : TImportDll;
  pNames : PAnsiChar;
  adllname : AnsiString;
  codeOffset : Integer;
  codeSection : TImageSection;
  parentImage : TPEModule;
begin
  parentImage := parent as TPEModule;
  codeSection := TPEModuleRW (Parent).GetCodeSection;
  rva := SectionHeader.VirtualAddress;
  size := sizeof (TImageImportDescriptor) * (fImportDlls.Count+1);
  iatOffset := size;

  iatSize := 0;
  for i := 0 to fImportDlls.Count - 1 do
  begin
    dll := TImportDll (fImportDlls.Objects [i]);
    Inc (size, (dll.fImportFunctions.Count + 1) * 2 * sizeof (DWORD));
    Inc (iatSize, (dll.fImportFunctions.Count + 1) * sizeof (DWORD));
  end;

  nameOffset := size;

  fnameOffset := nameOffset;

  for i := 0 to fImportDlls.Count - 1 do
  begin
    dll := TImportDll (fImportDlls.Objects [i]);
    Inc (size, Length (dll.fDLLname) + 1);
    Inc (fNameOffset, Length (dll.fDllName) + 1);

    for j := 0 to dll.fImportFunctions.Count - 1 do
    begin
      fn := TImportFunction (dll.fImportFunctions.Objects [j]);
      Inc (size, sizeof (word) + Length (fn.fFunctionName) + 1);
    end
  end;

  fRawData.Size := size;

  base := Integer (fRawData.Memory);
  pd := PImageImportDescriptor (base);
  piat := PDWORD (base + iatOffset);
  pnames := PAnsiChar (base + nameOffset);

  for i := 0 to fImportDlls.Count - 1 do
  begin
    dll := TImportDll (fImportDlls.Objects [i]);

    pd^.TimeDateStamp := 0;
    pd^.ForwarderChain := 0;
    pd^.Name := Integer (pnames) - base + rva;
    adllname := AnsiString (dll.fDLLname);
    Move (adllname [1], pnames^, Length (dll.fDllName) + 1);
    Inc (pNames, Length (dll.fDllName) + 1);

    prevPiat := piat;
    pd^.FirstThunk := Integer (piat) - base + rva;
    for j := 0 to dll.fImportFunctions.Count - 1 do
    begin
      fn := TImportFunction (dll.fImportFunctions.Objects [j]);
      piat^ := fnameOffset+rva;


      for k := 0 to fn.fFixups.Count - 1 do
      begin
        codeOffset := Integer (fn.fFixups [k]);
        PDWORD (PByte (codeSection.RawData.Memory) + codeOffset)^ := Integer (piat) - base + rva + Integer (parentImage.OptionalHeader.ImageBase);
        TPEModuleRW (Parent).AddReloc (codeOffset);
      end;

      Inc (fNameOffset, sizeof (word));
      adllname := AnsiString (fn.fFunctionName);
      Move (adllname [1], PAnsiChar (base + fNameOffset)^, Length (fn.fFunctionName) + 1);
      Inc (fNameOffset, Length (fn.fFunctionName) + 1);
      Inc (piat);
    end;
    piat^ := 0;
    Inc (piat);

    pd^.Characteristics := Integer (piat) - base + rva;
    for j := 0 to dll.fImportFunctions.Count - 1 do
    begin
      piat^ := prevPiat^;
      Inc (prevPiat);
      Inc (piat);
    end;
    piat^ := 0;
    Inc (piat);

    Inc (pd);
  end;

  FillChar (pd^, sizeof (pd^), 0);

  ParentImage.DataDirectory [IMAGE_DIRECTORY_ENTRY_IAT]^.VirtualAddress := iatOffset + rva;
  ParentImage.DataDirectory [IMAGE_DIRECTORY_ENTRY_IAT]^.Size := iatSize;
end;

procedure TImportSection.Initialize;
begin
  fImportDLLs := TStringList.Create;
  TStringList (fImportDlls).CaseSensitive := False
end;

{ TRelocSection }

destructor TRelocSection.Destroy;
begin
  fRelocs.Free;

  inherited;
end;

function CompareRelocs (p1, p2 : Pointer) : Integer;
begin
  result := DWORD (p1) - DWORD (p2);
end;

const
  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGHLOW = 3;

type
TImageBaseRelocation = record
  RVA : DWORD;
  Size : DWORD;
end;
PImageBaseRelocation = ^TImageBaseRelocation;

procedure TRelocSection.Fixup;
var
  reloc, block, ofs, csrva : DWORD;
  i : Integer;
  pHdr : PImageBaseRelocation;
  hdr : TImageBaseRelocation;
  w : word;
begin
  fRelocs.Sort(CompareRelocs);

  fRawData.Clear;
  pHdr := Nil;
  csrva := TPEModuleRW (Parent).GetCodeSection.SectionHeader.VirtualAddress;

  for i := 0 to fRelocs.Count - 1 do
  begin
    reloc := DWORD (fRelocs [i]) + csrva;

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
          Inc (pHdr^.Size, sizeof (w));
        end
      end;

      hdr.RVA := block;
      hdr.Size := sizeof (TImageBaseRelocation);
      fRawData.Write(hdr, sizeof (hdr));
      pHdr := PImageBaseRelocation (PByte (fRawData.Memory) + fRawData.Position - sizeof (hdr));
    end;

    w := IMAGE_REL_BASED_HIGHLOW shl 12 + ofs;
    fRawData.Write (w, sizeof (w));
    Inc (pHdr^.Size, sizeof (w));
  end
end;

procedure TRelocSection.Initialize;
begin
  inherited;

  fRelocs := TList.Create;
end;

end.
