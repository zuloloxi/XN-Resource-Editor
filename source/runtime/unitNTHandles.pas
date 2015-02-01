(*======================================================================*
 | unitNTHandles unit                                                   |
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
 | Copyright © Colin Wilson 2002-2004  All Rights Reserved              |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      12/06/2002  CPWW  Original                                  |
 | 1.1      05/03/2003  CPWW  TSystemFileHandles now just returns file  |
 |                            handles - not pipe, etc. handles.         |
 *======================================================================*)

unit unitNTHandles;

interface

uses Windows, Classes, SysUtils, NTDDK, WinSock, ConTnrs, IPHlpAPI, iprtrmib;

type

//--------------------------------------------------------------
// TKernelMemory class.  Copy Kernel memory to our address space
//
// See MSDN article "The Virtual-Memory Manager in Windows NT"
//
// Base on a newsgoup posting by Gary Nebbett
TKernelMemory = class
private
  fSectionHandle : THandle;
  fPageDirectory : PHARDWARE_PTE;
  function FindPageDirectory : DWORD;
  function AnyPageDirectory : DWORD;
  function MyPageDirectory : DWORD;
  function CreatedMappedSystemPage (va : DWORD; prot : DWORD) : pointer;
public
  constructor Create;
  destructor Destroy; override;
  function Copy(va : DWORD; p : pointer; n : DWORD) : DWORD;
  function RawCopy(pa : DWORD; p : pointer; n : DWORD) : DWORD;
end;

TSystemHandles = class
private
  fInfo : PSystemHandleInformation;
  function GetCount: Integer;
  function GetInfo (idx : Integer) : PSystemHandleTableEntryInfo;
  function GetHandle(idx: Integer): THandle;
  function GetObj(idx: Integer): Pointer;
  function GetPID(idx: Integer): DWORD;
  function GetKernelMemory: TKernelMemory;
  function GetObjectTypeIndex(idx: Integer): DWORD;
  function GetDeviceObject(idx: Integer): PDeviceObject;
protected
  property Obj [idx : Integer] : Pointer read GetObj;
public
  constructor Create;
  destructor Destroy; override;
  procedure Refresh;

  function Find (pid : DWORD; Handle : THandle) : Integer;

  property Count : Integer read GetCount;
  property PID [idx : Integer] : DWORD read GetPID;
  property Handle [idx : Integer] : THandle read GetHandle;
  property ObjectTypeIndex [idx : Integer] : DWORD read GetObjectTypeIndex;
  property DeviceObject [idx : Integer] : PDeviceObject read GetDeviceObject;
  property Info [idx : Integer] : PSystemHandleTableEntryInfo read GetInfo;
end;

TIPHandleInfo = class
private
  fProtocol : string;
  fIPAddr : string;
  fPort : DWORD;
  fPID : DWORD;
  fProcessName : string;
  fProcessFileName : string;
  fRemAddr : string;
  fRemPort : DWORD;
public
  constructor Create (const AProtocol : string; APID, APort : DWORD; const AIPAddr : string; ARemPort : DWORD; const ARemAddr : string);

  property Protocol : string read fProtocol;
  property IPAddr : string read fIPAddr;
  property Port : DWORD read fPort;
  property PID : DWORD read fPID;
  property ProcessName : string read fProcessName;
  property ProcessFileName : string read fProcessFileName;
  property RemoteAddr : string read fRemAddr;
  property RemotePort : DWORD read fRemPort;
end;


TDeviceHandlesSortType = (stProtocol, stPort, stIPAddress, stRemotePort, stRemoteAddr, stPID, stProcessName, stProcessFileName);
TFileHandlesSortType = (fstFileName, fstHandle, fstPID, fstProcessName);

TSystemDeviceHandles = class
private
  fSystemHandles : TSystemHandles;
  fDevice  : string;
  fDeviceHandles : TObjectList;
  fObjectTypeIndex : DWORD;
  fDeviceObject : PDeviceObject;
  fLastSortType : TDeviceHandlesSortType;
  fLastSortBackwards : boolean;
  function GetCount: Integer;
  function GetDeviceHandle(idx: Integer): TIPHandleInfo;
public
  constructor Create (ASystemHandles : TSystemHandles; const ADevice : string);
  destructor Destroy; override;
  procedure Refresh;
  procedure Sort (sortType : TDeviceHandlesSortType);

  property Count : Integer read GetCount;
  property DeviceHandle [idx : Integer] : TIPHandleInfo read GetDeviceHandle;
end;

TFileHandleInfo = class
 private
  fPID: DWORD;
  fProcessName: string;
  fFileName: string;
  fHandle: DWORD;
public
  constructor Create (const AFileName : string; APID : DWORD; AHandle : DWORD; lookupProcess : boolean = true);
  property FileName : string read fFileName;
  property PID : DWORD read fPID;
  property ProcessName : string read fProcessName;
  property Handle : DWORD read fHandle;
end;

TSystemFileHandles = class
private
  fSystemHandles : TSystemHandles;
  fObjectTypeIndex : DWORD;
  fDeviceObject : PDeviceObject;
  fFileHandles : TObjectList;
  fLastSortType : TFileHandlesSortType;
  fLastSortBackwards : boolean;
  flookupProcess : boolean;
  function GetCount: Integer;
  function GetFileHandle(idx: Integer): TFileHandleInfo;
public
  constructor Create (ASystemHandles : TSystemHandles; lookupProcess : boolean = true);
  destructor Destroy; override;
  procedure Refresh;
  procedure Sort (sortType : TFileHandlesSortType);

  property Count : Integer read GetCount;
  property FileHandle [idx : Integer] : TFileHandleInfo read GetFileHandle;
end;

TTCPEndpoint = class
private
  dwState : DWORD;
  dwLocalAddr : DWORD;
  dwLocalPort : DWORD;
  dwRemoteAddr : DWORD;
  dwRemotePort : DWORD;
public
  constructor Create (AState, ALocalAddr, ALocalPort, ARemoteAddr, ARemotePort : DWORD);
end;

TTCPEndpoints = class
private
  fEndpointList : TObjectList;
public
  constructor Create;
  destructor Destroy; override;
  procedure Refresh;
  function FindEndpoint (locAddr, locPort : Integer) : TTCPEndpoint;
end;


implementation

uses NTProcess;

resourcestring
  rstInvalidHandleIndex = 'Invalid handle index';

var
  gKernelMemory : TKernelMemory;
  gNTProcesses : TNTProcessList;
  gTCPEndpoints : TTCPEndpoints;

function GetKernelMemory : TKernelMemory;
begin
  if not Assigned (gKernelMemory) then
    gKernelMemory := TKernelMemory.Create;

  result := gKernelMemory
end;

function GetNTProcesses : TNTProcessList;
begin
  if not Assigned (gNTProcesses) then
    gNTProcesses := TNTProcessList.Create(nil);

  result := gNTProcesses
end;

function GetTCPEndpoints : TTCPEndpoints;
begin
  if not Assigned (gTCPEndpoints) then
    gTCPEndpoints := TTCPEndpoints.Create;

  result := gTCPEndpoints
end;

function GetKernelString (st : TUnicodeString) : string;
var
  buf : WideString;
  nWideChars : Integer;
begin
  nWideChars := st.Length div sizeof (WideChar);
  if nWideChars <> 0 then
  begin
    SetLength (buf, nWideChars);
    GetKernelMemory.Copy(DWORD (st.Buffer), @buf [1], nWideChars * sizeof (WideChar));
    result := buf
  end
  else
    result := ''
end;


{ TKernelMemory }

(*----------------------------------------------------------------------*
 | function TKernelMemory.AnyPageDirectory                              |
 |                                                                      |
 | Find a page directory within physical memory.  I'm mystified
 *----------------------------------------------------------------------*)
function TKernelMemory.AnyPageDirectory: DWORD;
const
  reserved = $a0;
var
  lowmem, p1 : PDWORD;
  pfn : DWORD;
begin
  lowmem := MapViewOfFile (fSectionHandle, FILE_MAP_READ, 0, 0, reserved * $1000);
  try
    for pfn := 0 to reserved - 1 do
    begin
      p1 := LowMem;
      Inc (p1,pfn * $400 + $300);
      if p1^ = pfn * $1000 + $67 then
        break
    end
  finally
    UnmapViewOfFile (lowmem);
  end;

  result := pfn * $1000
end;

(*----------------------------------------------------------------------*
 | function TKernelMemory.Copy                                          |
 |                                                                      |
 | Copy n bytes of physical memory at virtual address 'va' to pointer p |
 |                                                                      |
 | The function returns the number of bytes copied                      |
 *----------------------------------------------------------------------*)
function TKernelMemory.Copy(va: DWORD; p: pointer; n: DWORD): DWORD;
var
  y, x : DWORD;
  q : pointer;
  bo : DWORD;
begin
  result := 0;
  x := va;
  while x < va + n do           // Copy the memory a page at a time.
  begin
                                // Map physical memory page containing x to 'q'
    q := createdMappedSystemPage (x, FILE_MAP_READ);
    try
      if q = nil then
        exit;

      y := n - result;          // y = bytes left to copy

                                // Limit this to remaining bytes in the current page (max = PAGE_SIZE)
      bo := BYTE_OFFSET (Pointer (x));
      if PAGE_SIZE - bo < y then
        y := PAGE_SIZE - bo;

                                // Copy memory from physical page to 'p'
      CopyMemory (PByte (p) + result, PByte (q) + bo, y);
    finally
      UnmapViewOfFile(q);
    end;

    Inc (result, y);
    Inc (x, y)
  end
end;

(*----------------------------------------------------------------------*
 | constructor TKernelMemory.Create                                     |
 |                                                                      |
 | Create TKernelMemory object.  Create a section handle for physical   |
 | memory and find the PageDirectory for this process.             |
 *----------------------------------------------------------------------*)
constructor TKernelMemory.Create;
var
  name : TUnicodeStr;
  oa : TObjectAttributes;
begin
  name := TUnicodeStr.CreateFromStr('\Device\PhysicalMemory');
  try
    ZeroMemory (@oa, SizeOf (oa));
    oa.Length := SizeOf (TObjectAttributes);
    oa.RootDirectory := 0;
    oa.ObjectName := @name.Value;
    oa.Attributes := OBJ_CASE_INSENSITIVE;

    fnZwOpenSection (fSectionHandle, SECTION_MAP_READ, oa);
    fPageDirectory := MapViewOfFile (fSectionHandle, FILE_MAP_READ, 0, FindPageDirectory, $1000)
  finally
    name.Free
  end
end;

(*----------------------------------------------------------------------*
 | destructore TKernelMemory.Destroy                                    |
 *----------------------------------------------------------------------*)
destructor TKernelMemory.Destroy;
begin
  UnmapViewOfFile(fPageDirectory);

  CloseHandle(fSectionHandle);

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TKernelMemory.FindPageDirectory : DWORD                     |
 |                                                                      |
 | Find the offset in physical memory of the PageDirectory for          |
 | this process.  If that can't be found, find any page directory entry.|
 |                                                                      |
 | Parameters:                                                          |
 |   None
 |                                                                      |
 | The function returns DWORD
 *----------------------------------------------------------------------*)
function TKernelMemory.FindPageDirectory: DWORD;
begin
  result := MyPageDirectory;
  if result = 0 then
    result := AnyPageDirectory
end;

(*----------------------------------------------------------------------*
 | procedure TKernelMemory.CreatedMappedSystemPage                      |
 |                                                                      |
 | Map page of memory containing phsyical memory at VA                  |
 |                                                                      |
 | nb.  a Virtual Address goes like this:                               |
 |                                                                      |
 |  1111111111 1111111111 111111111111                                  |
 |    |            |           |                                        |
 |    |            |           +-- Low 12 bits = Offset of byte in PF   |
 |    |            +-------------- Mid 10 bits = idx of PF in PT        |
 |    +--------------------------- Hi  10 bits = idx of PT in PD        |
 |                                                                      |
 | Parameters:                                                          |
 |   va                 Virtual Address of memory to map                |
 |   prot: DWORD        Protect level                                   |
 |                                                                      |
 | The function returns a pointer to the mapped page containing 'p'     |
 *----------------------------------------------------------------------*)
function TKernelMemory.CreatedMappedSystemPage(va, prot: DWORD): pointer;
var
  offset : DWORD;
  ppte1, ppte2, table : PHARDWARE_PTE;
  pte1, pte2 : HARDWARE_PTE;
  pfn : DWORD;
begin
  ppte1 := fPageDirectory;
  Inc (ppte1, va shr 22);       // ppte1 = PageDirectory [Upper 10 bits of va] - the PDE - which points to the PTE
  pte1 := ppte1^;               // pte1 is the Page Table

  if (pte1 and PTE_VALID) = 0 then
  begin
    result := nil;
    exit
  end;

                                        // Don't quite understand this
                                        // large page stuff.  Hope it works!
  if (pte1 and PTE_LARGEPAGE) = 0 then
  begin
    table := PHARDWARE_PTE (MapViewOfFile (fSectionHandle, FILE_MAP_READ, 0, PTE_PageFrameNumber (pte1) shl 12, $1000));
    try
      ppte2 := table;
      Inc (ppte2, ((va shr 12) and $3ff));
      pte2 := ppte2^;
    finally
      UnmapViewOfFile (table)
    end;

    if (pte2 and PTE_VALID) = 0 then
    begin
      result := nil;
      exit
    end;

    offset := PTE_PageFrameNumber (pte2) shl 12;
  end
  else
  begin
    pfn := PTE_PageFrameNumber (pte1);  // Get20-bit page frame
                                        // Make page frame the top 20-bits, and add the middle 10 bits
                                        // from the virtual address.  We want the offset of the
                                        // start of the page - so ignore the low 12 bits.

    offset := (pfn shl 12) + (va and $3ff000)
  end;

  // Map physical memory at VA to result
  result := MapViewOfFile(fSectionHandle, prot, 0, offset, PAGE_SIZE);
end;

(*----------------------------------------------------------------------*
 | function TKernelMemory.MyPageDirectory                          |
 |                                                                      |
 | Return the offset in physical memory of our PDE                      |
 *----------------------------------------------------------------------*)
function TKernelMemory.MyPageDirectory: DWORD;
var
  TaskRegister : word;
  tss : LDT_ENTRY;
  va, pa, pd : DWORD;
  td : DWORD;
begin
  asm str TaskRegister; end;      // Get the Task Register (TR)
                                  // This is always $28 in current versions of
                                  // NT - but get it anyway!

  td := GetCurrentThread;         // Get the TSS structure
  if not GetThreadSelectorEntry (td, TaskRegister, tss) then
    RaiseLastOSError;

  va := (tss.BaseHi shl 24) + (tss.BaseMid shl 16) + tss.BaseLow;
  pa := va and not $80000000;
                                        // pa is physical address of mythical hardware TSS structure
                                        // Get the CR3 register containing the Page Directory for
                                        // this process - at offset 1C in TSS - which looks like this...
                                        //  struct TSSEG
                                        //  {
                                        //    00  WORD   link,   res1;
                                        //    04  DWORD  esp0;
                                        //    08  WORD   ss0,    res2;
                                        //    0C  DWORD  esp1;
                                        //    10  WORD   ss1,    res3;
                                        //    14  DWORD  esp2;
                                        //    18  WORD   ss2,    res4;
                                        //    1C  DWORD  cr3,eip,eflags,eax,ecx,edx,ebx,esp,ebp,esi,edi;
                                        //    48  WORD   es,     res5;
                                        //    4C  WORD   cs,     res6;
                                        //    50  WORD   ss,     res7;
                                        //    54  WORD   ds,     res8;
                                        //    58  WORD   fs,     res9;
                                        //    5C  WORD   gs,     res10;
                                        //    60  WORD   res11,  ldtr;
                                        //    64  WORD   res12,  iopb;
                                        //    68
                                        //  };

  RawCopy (pa + $1C, @pd, sizeof (pd)); // Get CR3 ( = the Page Directory)

  if (pd and $fff) = 0 then             // Page Directories are 4K in size
    result := pd                        // So pd mod 4096 must be zero
  else
    result := 0                         // ... otherwise something's wrong!
end;

(*----------------------------------------------------------------------*
 | procedure TKernelMemory.RawCopy                                      |
 |                                                                      |
 | Used internally.  Copy 'n' bytes of memory at physical address 'pa'  |
 | to 'p'.                                                              |
 |                                                                      |
 | See also the 'Copy' function which copies from a virtual, rather     |
 | than a physical address.                                             |
 |                                                                      |
 | The function returns the number of bytes copied                      |
 *----------------------------------------------------------------------*)
function TKernelMemory.RawCopy(pa: DWORD; p: pointer; n: DWORD): DWORD;
var
  y, x : DWORD;
  q : pointer;
begin
  result := 0;
  x := pa;
  while x < pa + n do
  begin
                        // Map page containing 'x'
    q := MapViewOfFile(fSectionHandle, FILE_MAP_READ, 0, x and not $FFF, $1000);
    try
      if q = nil then
        exit;

      y := n - result;  // y = bytes left to copy ...
                        // ... limited to bytes left this page
      if (PAGE_SIZE - BYTE_OFFSET (Pointer (x))) < y then
        y := PAGE_SIZE - BYTE_OFFSET (Pointer (x));

      CopyMemory (PByte (p) + result, PByte (q) + BYTE_OFFSET (Pointer (x)), y);
    finally
      UnmapViewOfFile(q);
    end;
    Inc (result, y);
    Inc (x, y)
  end
end;

{ TSystemHandles }

constructor TSystemHandles.Create;
begin
  refresh;
end;

destructor TSystemHandles.Destroy;
begin
  ReallocMem (fInfo, 0);
  inherited;
end;

function TSystemHandles.Find(pid: DWORD; Handle: THandle): Integer;
var
  i : Integer;
  info : PSystemHandleTableEntryInfo;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    info := GetInfo (i);
    if (info^.UniqueProcessId = pid) and (info^.HandleValue = handle) then
    begin
      result := i;
      break
    end
  end
end;

function TSystemHandles.GetCount: Integer;
begin
  if Assigned (fInfo) then
    result := fInfo^.NumberOfHandles
  else
    result := 0
end;

function TSystemHandles.GetDeviceObject(idx: Integer): PDeviceObject;
var
  fo : TFileObject;
begin
  GetKernelMemory.Copy(DWORD (GetInfo (idx)^.Obj), @fo, sizeof (fo));
  result := fo.DeviceObject
end;

function TSystemHandles.GetHandle(idx: Integer): THandle;
begin
  result := GetInfo (idx)^.HandleValue
end;

function TSystemHandles.GetInfo(idx: Integer): PSystemHandleTableEntryInfo;
begin
  if idx < Count then
  begin
    result := @fInfo^.Handles [0];
    Inc (result, idx)
  end
  else
    raise ERangeError.Create(rstInvalidHandleIndex);
end;

function TSystemHandles.GetKernelMemory: TKernelMemory;
begin
  result := unitNTHandles.GetKernelMemory;
end;

function TSystemHandles.GetObj(idx: Integer): Pointer;
begin
  result := GetInfo (idx)^.Obj
end;

function TSystemHandles.GetObjectTypeIndex(idx: Integer): DWORD;
begin
  result := GetInfo (idx)^.ObjectTypeIndex
end;

function TSystemHandles.GetPID(idx: Integer): DWORD;
begin
  result := GetInfo (idx)^.UniqueProcessId
end;

procedure TSystemHandles.Refresh;
var
  n : DWORD;
  rv, rs : DWORD;
begin
  if not Assigned (fInfo) then
    n := $1000
  else
    n := (fInfo^.NumberOfHandles + $100) div $100 * $100;

  repeat
    rs := SizeOf (TSystemHandleInformation) + SizeOf (TSystemHandleTableEntryInfo) * (n - 1);
    ReallocMem (fInfo, rs);

    rv := fnZwQuerySystemInformation (SystemHandleInformation, fInfo, rs, rs);
    n := n * 2;
  until rv <> STATUS_INFO_LENGTH_MISMATCH;

  if rv <> 0 then
  begin
    ReallocMem (fInfo, 0);
    RaiseLastOSError
  end
  else
  begin
    n := (fInfo^.NumberOfHandles + $100) div $100 * $100;
    rs := SizeOf (TSystemHandleInformation) + SizeOf (TSystemHandleTableEntryInfo) * (n - 1);
    ReallocMem (fInfo, rs)
  end;

  if Assigned (gNTProcesses) then
    gNTProcesses.Refresh;

  if Assigned (gTCPEndpoints) then
    gTCPEndpoints.Refresh;
end;

{ TSystemDeviceHandles }

constructor TSystemDeviceHandles.Create(ASystemHandles: TSystemHandles;
  const ADevice: string);
var
  handle : THandle;
  i : Integer;

  function OpenDevice (const name : string) : THandle;
  var
    s : TUnicodeStr;
    oa : TObjectAttributes;
    iosb : TIOStatusBlock;
  begin
    s := TUnicodeStr.CreateFromStr(name);
    try
      ZeroMemory (@oa, sizeof (oa));
      oa.Length := SizeOf (oa);
      oa.RootDirectory := 0;
      oa.ObjectName := @s.Value;
      oa.Attributes := OBJ_CASE_INSENSITIVE;
      fnZwOpenFile (result, SYNCHRONIZE, oa, iosb, FILE_SHARE_READ or FILE_SHARE_WRITE, 0)
    finally
      s.Free
    end
  end;

begin
  fLastSortType := stPID;
  fSystemHandles := ASystemHandles;
  fDevice := ADevice;
  fDeviceHandles := TObjectList.Create;

  handle := OpenDevice ('\Device\' + fDevice);
  if handle = 0 then
    RaiseLastOSError;

  try
    fSystemHandles.Refresh;

    i := fSystemHandles.Find (GetCurrentProcessID, handle);

    fObjectTypeIndex := fSystemHandles.ObjectTypeIndex [i];
    fDeviceObject := fSystemHandles.DeviceObject [i]
  finally
    CloseHandle (handle)
  end;

  Refresh
end;

destructor TSystemDeviceHandles.Destroy;
begin
  fDeviceHandles.Free;
  inherited;
end;

function TSystemDeviceHandles.GetCount: Integer;
begin
  result := fDeviceHandles.Count;
end;

function TSystemDeviceHandles.GetDeviceHandle(idx: Integer): TIPHandleInfo;
begin
  result := TIPHandleInfo (fDeviceHandles [idx])
end;

procedure TSystemDeviceHandles.Refresh;
var
  i : Integer;
  fo : TFileObject;
  hObject : THandle;
  reqAddr, reqConn, remAddr : TTDIRequestQueryInformation;
  conn : TTDIConnectionInfo;
  addr : array [0..2] of TTDIAddressInfo;
  rem  : TTDIConnectionInformation;
  addrStr : String;
  br : DWORD;
  o : Overlapped;
  pTDIaddr : PTDIAddressInfo;
  ipaddr : PTAIPAddress;
  port : word;
  pc : PByte;
  ip : Integer;
  sortType : TDeviceHandlesSortType;
  ta : TTransportAddress;
  ep : TTCPEndpoint;
  remPort : word;
  remAddrStr : string;

  function CopyHandle (pid : DWORD; h : THandle) : THandle;
  var
    hProcess : THandle;
    hObject : THandle;
  begin
    hObject := 0;
    hProcess := OpenProcess (PROCESS_DUP_HANDLE, FALSE, pid);
    DuplicateHandle (hProcess, h, GetCurrentProcess, @hObject, 0, FALSE, DUPLICATE_SAME_ACCESS);
    CloseHandle (hProcess);
    result := hObject
  end;

begin
  fDeviceHandles.Clear;

  ZeroMemory (@o, SizeOf (o));
  o.hEvent := CreateEvent (nil, True, False, nil);

  for i := 0 to fSystemHandles.Count - 1 do
    if fSystemHandles.ObjectTypeIndex [i] = fObjectTypeIndex then
    begin
      GetKernelMemory.Copy(DWORD (fSystemHandles.Obj [i]), @fo, sizeof (fo));
      if (fDeviceObject = fo.DeviceObject) and ((fo.FsContext2 = Pointer (TDI_TRANSPORT_ADDRESS_FILE)) or (fo.FsContext2 = Pointer (TDI_CONNECTION_FILE))) then
      begin
        hObject := CopyHandle (fSystemHandles.PID [i], fSystemHandles.Handle [i]);
        if hObject <> 0 then
        try
          ZeroMemory (@ta, SizeOf (ta));
          ZeroMemory (@rem, SizeOf (rem));
          rem.RemoteAddressLength := sizeof (ta);
          rem.RemoteAddress := @ta;

          ZeroMemory (@reqAddr, SizeOf (reqAddr));
          reqAddr.QueryType := TDI_QUERY_ADDRESS_INFO;
          ZeroMemory (@reqConn, SizeOf (reqConn));
          reqConn.QueryType := TDI_QUERY_CONNECTION_INFO;
          reqConn.RequestConnectionInformation := @rem;
          ZeroMemory (@remAddr, SizeOf (reqAddr));
          remAddr.QueryType := TDI_QUERY_NETWORK_ADDRESS;

          ZeroMemory (@conn, SizeOf (conn));
          ZeroMemory (@addr, Sizeof (addr));

          br := 0;
          if (fo.FsContext2 = Pointer (TDI_TRANSPORT_ADDRESS_FILE)) or DeviceIoControl (hObject, IOCTL_TDI_QUERY_INFORMATION, @reqconn, sizeof (reqconn), @conn, sizeof (conn), br, @o) then
          begin
            if DeviceIoControl (hObject, IOCTL_TDI_QUERY_INFORMATION, @reqaddr, SizeOf (reqaddr), @addr, sizeof (addr), br, @o) then
            begin
              pTDIAddr := @addr [0];
              ipaddr := PTAIPAddress (@pTDIAddr^.Address);
              port := ipaddr^.Address [0].Address [0].sin_port;
              ip := ipaddr^.Address [0].Address [0].in_addr;

              if  CompareText (fDevice, 'TCP') = 0 then
                ep := GetTCPEndpoints.FindEndpoint(ip, port)
              else
                ep := Nil;
              if Assigned (ep) then
              begin
                remPort := ep.dwRemotePort;
                pc := PByte (@ep.dwRemoteAddr);
                remAddrStr := Format ('%d.%d.%d.%d', [Ord (pc [0]), Ord (pc [1]), Ord (pc [2]), Ord (pc [3])]);
              end
              else
              begin
                remAddrStr := '';
                remPort := $ffff
              end;

              pc := PByte (@ip);
              addrStr := Format ('%d.%d.%d.%d', [Ord (pc [0]), Ord (pc [1]), Ord (pc [2]), Ord (pc [3])]);
              fDeviceHandles.Add(TIPHandleInfo.Create (fDevice, fSystemHandles.PID [i], ntohs (port), addrStr, ntohs (remPort), remAddrStr))
            end
            else
//              RaiseLastOSError
          end
        finally
          CloseHandle (hObject)
        end
      end
    end;

  sortType := fLastSortType;
  fLastSortType := TDeviceHandlesSortType (-1);
  Sort (sortType);
end;

var
  gSortType : TDeviceHandlesSortType;
  gSortBackwards : boolean;

  gFileSortType : TFileHandlesSortType;
  gFileSortBackwards : boolean;

//(stProtocol, stPort, stIPAddress, stPID, stProcessName, stProcessFileName);

function CompareDeviceHandles (p1, p2 : pointer) : Integer;
var
  d1 : TIPHandleInfo absolute p1;
  d2 : TIPHandleInfo absolute p2;
begin
  result := 0;
  case gSortType of
    stProtocol : result := CompareText (d1.Protocol, d2.Protocol);
    stPort : result := d1.Port - d2.Port;
    stIPAddress : result := CompareText (d1.IPAddr, d2.IPAddr);
    stPID : result := d1.PID - d2.PID;
    stProcessname : result := CompareText (d1.ProcessName, d2.ProcessName);
    stProcessFileName : result := CompareText (d1.ProcessFileName, d2.ProcessFileName);
    stRemotePort : result := d1.RemotePort - d2.RemotePort;
    stRemoteAddr : result := CompareText (d1.RemoteAddr, d2.RemoteAddr);

  end;

  if gSortBackwards then
    result := -result;
end;

procedure TSystemDeviceHandles.Sort(sortType: TDeviceHandlesSortType);
begin
  if fLastSortType = sortType then
    fLastSortBackwards := not fLastSortBackwards
  else
    fLastSortBackwards := False;

  fLastSortType := sortType;

  gSortType := fLastSortType;
  gSortBackwards := fLastSortBackwards;

  fDeviceHandles.Sort (CompareDeviceHandles);

end;

{ TIPHandleInfo }

constructor TIPHandleInfo.Create(const AProtocol: string; APID,
  APort: DWORD; const AIPAddr: string; ARemPort : DWORD; const ARemAddr : string);
var
  processes : TNTProcessList;
  process : TNTProcess;
begin
  fPort := APort;
  fProtocol := AProtocol;
  fIPAddr := AIPAddr;
  fPID := APID;
  fRemPort := ARemPort;
  fRemAddr := ARemAddr;

  process := TNTProcess.Create(nil);
  try
    processes := GetNTProcesses;
    try
      processes.FindProcess(fPID, process);
      fProcessName := process.BaseName;
//      fProcessFileName := process.ModuleFileName [0]
    except
    end
  finally
    process.Free
  end
end;

{ TTCPEndpoints }

constructor TTCPEndpoints.Create;
begin
  fEndpointList := TObjectList.Create;

  Refresh;

end;

destructor TTCPEndpoints.Destroy;
begin
  fEndpointList.Free;

  inherited;
end;

function TTCPEndpoints.FindEndpoint(locAddr,
  locPort: Integer): TTCPEndpoint;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to fEndpointList.Count - 1 do
    with TTCPEndpoint (fEndpointList [i]) do
      if (dwLocalAddr = DWORD (locAddr)) and (dwLocalPort = DWORD (locPort)) then
      begin
        result := TTCPEndpoint (fEndpointList [i]);
        break
      end

end;

procedure TTCPEndpoints.Refresh;
var
  size, err, i : DWORD;
  table : PMIB_TCPTABLE;
begin
  fEndpointList.Clear;

  size := 65536;
  table := Nil;

  try
    repeat
      ReallocMem (table, size);
      err := GetTCPTable (table, size, True);
    until err <> ERROR_INSUFFICIENT_BUFFER;

    if err = NO_ERROR then
      if table^.dwNumEntries > 0 then
        for i := 0 to table^.dwNumEntries - 1 do
          with table^.table [i] do
            fEndpointList.Add(TTCPEndpoint.Create (dwState, dwLocalAddr, dwLocalPort, dwRemoteAddr, dwRemotePort))
  finally
    ReallocMem (table, 0)
  end
end;

{ TTCPEndpoint }

constructor TTCPEndpoint.Create(AState, ALocalAddr, ALocalPort, ARemoteAddr,
  ARemotePort: DWORD);
begin
  dwState := AState;
  dwLocalAddr := ALocalAddr;
  dwLocalPort := ALocalPort;
  dwRemoteAddr := ARemoteAddr;
  dwRemotePort := ARemotePort;
end;

{ TSystemFileHandles }

constructor TSystemFileHandles.Create(ASystemHandles: TSystemHandles; lookupProcess : boolean);
var
  handle : THandle;
  i : Integer;
  st, tempfName : string;

begin
  fLookupProcess := lookupProcess;
  fLastSortType := fstPID;
  fSystemHandles := ASystemHandles;
  fFileHandles := TObjectList.Create;

  st := ExtractFilePath (ParamStr (0));
  SetLength (tempfName, MAX_PATH + 1);
  GetTempFileName (PChar (st), 'fanal', 0, PChar (tempfName));
  tempfName := PChar (tempfName);

  handle := CreateFile (PChar (tempfName), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if handle = 0 then
    RaiseLastOSError;

  try
    fSystemHandles.Refresh;

    i := fSystemHandles.Find (GetCurrentProcessID, handle);

    fObjectTypeIndex := fSystemHandles.ObjectTypeIndex [i];
    fDeviceObject := fSystemHandles.DeviceObject [i]
  finally
    CloseHandle (handle)
  end;

  DeleteFile (tempfName);

  Refresh
end;

destructor TSystemFileHandles.Destroy;
begin
  fFileHandles.Free;

  inherited;
end;

function TSystemFileHandles.GetCount: Integer;
begin
  result := fFileHandles.Count
end;

function TSystemFileHandles.GetFileHandle(idx: Integer): TFileHandleInfo;
begin
  result := TFileHandleInfo (fFileHandles [idx])
end;

procedure TSystemFileHandles.Refresh;
var
  i : Integer;
  fo : TFileObject;
  sortType : TFileHandlesSortType;
  fName : string;

begin
  fFileHandles.Clear;

  for i := 0 to fSystemHandles.Count - 1 do
    if fSystemHandles.ObjectTypeIndex [i] = fObjectTypeIndex then
    begin
      GetKernelMemory.Copy(DWORD (fSystemHandles.Obj [i]), @fo, sizeof (fo));
      if (fDeviceObject = fo.DeviceObject) then
      begin
        fName := GetKernelString (fo.FileName);

        if fName <> '' then
          fFileHandles.Add(TFileHandleInfo.Create(fName, fSystemHandles.PID [i], fSystemHandles.Handle [i], fLookupProcess));
      end
    end;

  sortType := fLastSortType;
  fLastSortType := TFileHandlesSortType (-1);
  Sort (sortType);
end;

function CompareFileHandles (p1, p2 : pointer) : Integer;
var
  d1 : TFileHandleInfo absolute p1;
  d2 : TFileHandleInfo absolute p2;
begin
  result := 0;
  case gFileSortType of
    fstFileName : result := CompareText (d1.FileName, d2.FileName);
    fstPID : result := d1.PID - d2.PID;
    fstProcessname : result := CompareText (d1.ProcessName, d2.ProcessName);
    fstHandle : result := d1.Handle - d2.Handle
  end;

  if gSortBackwards then
    result := -result;
end;

procedure TSystemFileHandles.Sort(sortType: TFileHandlesSortType);
begin
  if fLastSortType = sortType then
    fLastSortBackwards := not fLastSortBackwards
  else
    fLastSortBackwards := False;

  fLastSortType := sortType;

  gFileSortType := fLastSortType;
  gFileSortBackwards := fLastSortBackwards;

  fFileHandles.Sort (CompareFileHandles);
end;

{ TFileHandleInfo }

constructor TFileHandleInfo.Create(const AFileName: string; APID: DWORD; AHandle : DWORD; lookupProcess : boolean);
var
  process : TNTProcess;
  processes : TNTProcessList;
begin
  fFileName := AFileName;
  fPID := APID;
  fHandle := AHandle;

  if lookupProcess then
  begin
    process := TNTProcess.Create(nil);
    try
      processes := GetNTProcesses;
      try
        processes.FindProcess(fPID, process);
        fProcessName := process.BaseName;
  //      fProcessFileName := process.ModuleFileName [0]
      except
      end
    finally
      process.Free
    end
  end
end;

initialization
finalization
  gNTProcesses.Free;
  gKernelMemory.Free;
  gTCPEndPoints.Free;
end.
