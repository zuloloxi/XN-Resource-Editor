(*======================================================================*
 | unitProcesses                                                        |
 |                                                                      |
 | OS agnostic process enumeration classes.                             |
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
 | Copyright © Colin Wilson 2005  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 10.0     08/03/2006  CPWW  BDS 2006 release version                  |
 *======================================================================*)
unit unitProcesses;

interface

uses
  Windows, Classes, SysUtils, Contnrs, PsAPI, TlHelp32, DateUtils;

type
  TProcessSnapshot = class;
  TProcess = class;

  TModule = class
  private
    FOwner: TProcess;
    FFileName: string;
    FhModule: THandle;
    FVersionCache: string;
    FFileDateTime: TDateTime;
    FFileSize: DWORD;
    FObjectTag: TObject;
    function GetVersion: string;  // Module handle (in the context of the module's process)
    procedure GetFileDetails;
    function GetFileDateTime: TDateTime;
    function GetFileSize: DWORD;
    function GetFileName: string;
  protected
    constructor Create (AOwner: TProcess; fInitDetails: PModuleEntry32); overload;
    constructor Create (AOwner: TProcess; hOwner, hModule: THandle); overload;
  public
    property FileName: string read GetFileName;
    property Version: string read GetVersion;
    property FileDateTime: TDateTime read GetFileDateTime;
    property FileSize: DWORD read GetFileSize;
    property ObjectTag: TObject read FObjectTag write FObjectTag;
    property Owner: TProcess read FOwner;
  end;

  TProcess = class
  private
    FParentProcessID: DWORD;
    FProcessID: DWORD;
    FEXEName: string;
    FBasePriority: Longint;
    FModules: TObjectList;
    FObjectTag: TObject;
    FOwner: TProcessSnapshot;

    constructor Create (AOwner: TProcessSnapshot; fInitDetails: PProcessEntry32); overload;
    constructor Create (AOwner: TProcessSnapshot; fPID: DWORD); overload;
    function GetModule(idx: Integer): TModule;
    function GetModuleCount: Integer;
    function GetBaseModule: TModule;
  public
    destructor Destroy; override;
    property ProcessID: DWORD read FProcessID;
    property ParentProcessID: DWORD read FParentProcessID;
    property EXEName: string read FEXEName;
    property BasePriority: Longint read FBasePriority;

    property ModuleCount: Integer read GetModuleCount;
    property Module [idx: Integer]: TModule read GetModule; default;
    property BaseModule: TModule read GetBaseModule;
    property ObjectTag: TObject read FObjectTag write FObjectTag;
  end;

  TPSSortColumn = (psPID, psParentPID, psBasePriority, psName, psDate, psSize, psVersion);
  TPSSortDirection = (psAscending, psDescending);
  TProcessEnumerator = (peAuto, peToolHelp, pePSAPI);
  TProcessSnapshot = class (TComponent)
  private
    FProcesses: TObjectList;
    FProcessEnumerator: TProcessEnumerator;
    FSortColumn: TPSSortColumn;
    FSortDirection: TPSSortDirection;
    function GetProcess(idx: Integer): TProcess;
    function GetProcessCount: Integer;
    procedure SetProcessEnumerator(const Value: TProcessEnumerator);
    procedure LoadEnumerator;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SortProcesses (column: TPSSortColumn; direction: TPSSortDirection);
    procedure TakeSnapshot;
    property ProcessCount: Integer read GetProcessCount;
    property Process [idx: Integer]: TProcess read GetProcess; default;
    property SortColumn: TPSSortColumn read FSortColumn;
    property SortDirection: TPSSortDirection read FSortDirection;
  published
    property ProcessEnumerator: TProcessEnumerator read FProcessEnumerator write SetProcessEnumerator;
  end;

  EProcessSnapshot = class (exception);

procedure CheckToolHelp;
procedure CheckPSAPI;

implementation

resourcestring
  rstNoToolHelp   = 'Internal error.  No ToolHelp';
  rstNoPSAPI      = 'Internal error.  No PSAPI';
  rstCantSnapshot = 'Internal error.  Can''t enumerate processes on this computer';
  rstIdle         = 'Idle';
  rstSystem       = 'System';

const SE_DEBUG_NAME = 'SeDebugPrivilege';

type  // Support for NtQueryInformationProcess
  TProcessInfoClass = (
    ProcessBasicInformation,
    ProcessQuotaLimits,
    ProcessIoCounters,
    ProcessVmCounters,
    ProcessTimes,
    ProcessBasePriority,
    ProcessRaisePriority,
    ProcessDebugPort,
    ProcessExceptionPort,
    ProcessAccessToken,
    ProcessLdtInformation,
    ProcessLdtSize,
    ProcessDefaultHardErrorMode,
    ProcessIoPortHandlers,          // Note: this is kernel mode only
    ProcessPooledUsageAndLimits,
    ProcessWorkingSetWatch,
    ProcessUserModeIOPL,
    ProcessEnableAlignmentFaultFixup,
    ProcessPriorityClass,
    ProcessWx86Information,
    ProcessHandleCount,
    ProcessAffinityMask,
    ProcessPriorityBoost,
    ProcessDeviceMap,
    ProcessSessionInformation,
    ProcessForegroundInformation,
    ProcessWow64Information,
    MaxProcessInfoClass);

  TProcessBasicInformation = record
    ExitStatus: Longint;
    PebBaseAddress: Pointer;
    AffinityMask: DWORD;
    BasePriority: Longint;
    UniqueProcessId: DWORD;
    InheritedFromUniqueProcessId: DWORD;
  end;

  TfnNtQueryInformationProcess = function (
    Handle: THandle;
    infoClass: TProcessInfoClass;
    processInformation: Pointer;
    processInformationLength: ULONG;
    returnLength: PULONG
  ): DWORD; stdcall;

var
  gUsesToolHelp: Boolean = False;
  gUsesPSAPI: Boolean = False;
  gCheckedToolHelp: Boolean = False;
  gCheckedPSAPI: Boolean = False;
  gOldState: DWORD = $BadF00d;
  NTQueryInformationProcess: TfnNTQueryInformationProcess;

function EnableNTPrivilege (const privilege: string; state: Integer): Integer;
var
  hToken: THandle;
  aluid: TLargeInteger;
  cbPrevTP: DWORD;
  tp, fPrevTP: PTokenPrivileges;
begin
  Result := 0;
  if OpenProcessToken (GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then
  try
    LookupPrivilegeValue (Nil, PChar (privilege), aluid);

    cbPrevTP := SizeOf (TTokenPrivileges) + sizeof (TLUIDAndAttributes);

    GetMem (tp, cbPrevTP);
    GetMem (fPrevTP, cbPrevTP);
    try

      tp^.PrivilegeCount := 1;
      tp^.Privileges [0].Luid := aLuid;
      tp^.Privileges [0].Attributes := state;

      if not AdjustTokenPrivileges (hToken, False, tp^, cbPrevTP, fPrevTP^, cbPrevTP) then
        RaiseLastOSError;

      Result := fPrevTP^.Privileges [0].Attributes;

    finally
     FreeMem (fPrevTP);
     FreeMem (tp);
    end
  finally
    CloseHandle (hToken);
  end
end;

function GetFileVersion (const fileName: string): string;
var
  size, zero: DWORD;
  Buffer, pBuffer: pointer;
  info: PVSFixedFileInfo;
begin
  Result := '';
  size := GetFileVersionInfoSize (PChar (FileName), zero);
  if size > 0 then
  begin
    GetMem (Buffer, size);
    if not GetFileVersionInfo (PChar (FileName), zero, size, Buffer) then
      RaiseLastOSError;

    if not VerQueryValue (Buffer, '\', pBuffer, size) then
      RaiseLastOSError;

    info := PVSFixedFileInfo (pBuffer);

    Result := Format('%d.%d.%d.%d', [HiWord (info^.dwProductVersionMS), LoWord (info^.dwProductVersionMS), HiWord (info^.dwProductVersionLS), LoWord (info^.dwProductVersionLS)])
  end
  else
    Result := '-'
end;

function UsesToolHelp: Boolean;
var
  KernelHandle: THandle;
begin
  if not gCheckedToolHelp then
  begin
    gCheckedToolHelp := True;
    KernelHandle := GetModuleHandle(kernel32);
    gUsesToolHelp := Assigned(GetProcAddress(KernelHandle, 'CreateToolhelp32Snapshot'))
  end;

  Result := gUsesToolHelp
end;

function UsesPSAPI: Boolean;
var
  hPSAPI: THandle;
  hNTDLL: THandle;
begin
  if not gCheckedPSAPI then
  begin
    gCheckedPSAPI := True;
    hPSAPI := LoadLibrary('PSAPI.dll');
    if hPSAPI <> 0 then
    begin
      FreeLibrary (hPSAPI);
      gUsesPSAPI := True;

      hNTDLL := GetModuleHandle ('ntdll.dll');
      if hNTDLL <> 0 then
      begin
        NTQueryInformationProcess := GetProcAddress (hNTDLL, 'ZwQueryInformationProcess');
        if not Assigned(NTQueryInformationProcess) then
          raise EProcessSnapshot.Create(rstCantSnapshot);
       end

    end
  end;
  Result := gUsesPSAPI
end;

procedure CheckToolHelp;
begin
  if not UsesToolHelp then
    raise EProcessSnapshot.Create(rstNoToolhelp);
end;

procedure CheckPSAPI;
begin
  if not UsesPSAPI then
    raise EProcessSnapshot.Create (rstNoPSAPI)
end;

{ TProcess }

constructor TProcess.Create(AOwner: TProcessSnapshot;
  fInitDetails: PProcessEntry32);
var
  HandleSnapshot: THandle;
  ModuleEntry: TModuleEntry32;
begin
  FOwner := AOwner;
  CheckToolHelp;
  FModules := TObjectList.Create;

  FProcessID := fInitDetails^.th32ProcessID;
  FParentProcessID := fInitDetails^.th32ParentProcessID;
  FEXEName := fInitDetails^.szExeFile;
  FBasePriority := fInitDetails^.pcPriClassBase;

  if FParentProcessID <> 0 then
  begin
    HandleSnapshot := TlHelp32.CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, FProcessID);
    if HandleSnapshot <> INVALID_HANDLE_VALUE then
    try
      ModuleEntry.dwSize := sizeof (ModuleEntry);

      if Module32First(HandleSnapshot, ModuleEntry) then
      begin
        FEXEName := ModuleEntry.szExePath;
        repeat
          FModules.Add(TModule.Create(self, @ModuleEntry))
        until not Module32Next(HandleSnapshot, ModuleEntry)
      end
    finally
      Closehandle (HandleSnapshot)
    end
  end
end;

constructor TProcess.Create(AOwner: TProcessSnapshot; fPID: DWORD);
var
  HandleProcess: THandle;
  ProcessInfo: TProcessBasicInformation;
  BaseName: array [0..MAX_PATH] of char;
  Buffer, p: PHMODULE;
  BufferLen: DWORD;
  i, modCount: Integer;
begin
  FOwner := AOwner;
  CheckPSAPI;
  FModules := TObjectList.Create;
  FProcessID := fPID;
  if fPID = 0 then
    FEXEName := rstIdle
  else
  begin
    HandleProcess := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, fPid);
    if HandleProcess <> 0 then
    try
      Buffer := Nil;
      try
        ReallocMem (Buffer, 1024*1024);
        if not psapi.EnumProcessModules(HandleProcess, Buffer, 1024*1024, BufferLen) then BufferLen := 0;
        ReallocMem (Buffer, BufferLen);
        modCount := BufferLen div sizeof (PDWORD);

        p := Buffer;
        for i := 0 to modCount - 1 do
        begin
          FModules.Add(TModule.Create(self, HandleProcess, p^));
          Inc(p)
        end
      finally
        ReallocMem (Buffer, 0)
      end;

      if psapi.GetModuleBaseName(HandleProcess, 0, BaseName, sizeof (BaseName)) > 0 then
        FEXEName := BaseName
      else
        FEXEName := rstSystem;


      if NTQueryInformationProcess (HandleProcess, ProcessBasicInformation, @ProcessInfo, SizeOf (ProcessInfo), nil) = 0 then
      begin
        FParentProcessID := ProcessInfo.InheritedFromUniqueProcessId;
        FBasePriority := ProcessInfo.BasePriority
      end
    finally
      CloseHandle (HandleProcess)
    end
  end
end;

destructor TProcess.Destroy;
begin
  FModules.Free;

  inherited;
end;

function TProcess.GetBaseModule: TModule;
begin
  if ModuleCount > 0 then
    Result := Module [0]
  else
    Result := Nil
end;

function TProcess.GetModule(idx: Integer): TModule;
begin
  Result := TModule (FModules [idx])
end;

function TProcess.GetModuleCount: Integer;
begin
  Result := FModules.Count
end;

{ TProcessSnapshot }

constructor TProcessSnapshot.Create;
begin
  inherited Create (Aowner);
  FSortColumn := psPID;
  FProcesses := TObjectList.Create;
  if not(csDesigning in ComponentState) then
    LoadEnumerator;
end;

destructor TProcessSnapshot.Destroy;
begin
  FProcesses.Free;

  inherited;
end;

function TProcessSnapshot.GetProcess(idx: Integer): TProcess;
begin
  Result := TProcess (FProcesses [idx]);
end;

function TProcessSnapshot.GetProcessCount: Integer;
begin
  Result := FProcesses.Count
end;

procedure TProcessSnapshot.Loaded;
begin
  inherited;

  LoadEnumerator;
end;

procedure TProcessSnapshot.LoadEnumerator;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;

  gUsesToolHelp := False;
  gUsesPSAPI := False;
  gCheckedToolHelp := False;
  gCheckedPSAPI := False;

  case ProcessEnumerator of
    peAuto: 
      if not UsesPSAPI and not UsesToolHelp then
        raise EProcessSnapshot.Create (rstCantSnapshot);
    pePSAPI: 
      CheckPSAPI;
    peToolHelp: 
      CheckToolHelp;
  end;

  TakeSnapshot;
end;

procedure TProcessSnapshot.SetProcessEnumerator(
  const Value: TProcessEnumerator);
begin
  if Value <> FProcessEnumerator then
  begin
    FProcessEnumerator := Value;

    if not(csLoading in ComponentState) then
      LoadEnumerator;
  end
end;

function CompareProcesses (p1, p2: Pointer): Integer;
var
  pr1, pr2: TProcess;
  m1, m2: TModule;
  column: TPSSortColumn;
  direction: TPSSortDirection;
begin
  pr1 := TProcess (p1);
  pr2 := TProcess (p2);
  m1 := pr1.BaseModule;
  m2 := pr2.BaseModule;

  column := pr1.FOwner.FSortColumn;
  direction := pr1.FOwner.FSortDirection;

  Result := 99;
  if (column in [psDate, psSize]) and ((m1 = Nil) or (m2 = Nil)) then
  begin
    if m1 = Nil then
      if m2 = Nil then
        Result := 0
      else
        Result := -1
    else
      if m2 = Nil then
        Result := 1
  end;


  if result = 99 then
  case column of
    psPID: Result := pr1.ProcessID - pr2.ProcessID;
    psParentPID: Result := pr1.ParentProcessID - pr2.ParentProcessID;
    psBasePriority: Result := pr1.BasePriority - pr2.BasePriority;
    psName: Result := CompareText(pr1.EXEName, pr2.EXEName);
    psDate: Result := CompareDateTime (m1.FileDateTime, m2.FileDateTime);
    psSize: Result := m1.FileSize - m2.FileSize
    else
      Result := 0
  end;

  if direction = psDescending then
    Result := -result
end;

procedure TProcessSnapshot.SortProcesses (column: TPSSortColumn; direction: TPSSortDirection);
begin
  FSortColumn := column;
  FSortDirection := direction;
  FProcesses.Sort(CompareProcesses);
end;

procedure TProcessSnapshot.TakeSnapshot;
var
  Buffer, p: PDWORD;
  BufferLen: DWORD;
  i, count: Integer;
  HandleSnapshot: THandle;
  processEntry: TProcessEntry32;
begin
  if csDesigning in ComponentState then
    Exit;

  FProcesses.Clear;
  if gUsesToolhelp then
  begin
    HandleSnapshot := TlHelp32.CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if HandleSnapshot <> INVALID_HANDLE_VALUE then
    try
      processEntry.dwSize := sizeof (processEntry);

      if Process32First(HandleSnapshot, processEntry) then
      repeat
        FProcesses.Add(TProcess.Create(self, @processEntry))
      until not Process32Next(HandleSnapshot, processEntry)
    finally
      Closehandle (HandleSnapshot)
    end
  end
  else
  begin
    Buffer := Nil;
    try
      ReallocMem (Buffer, 1024*1024);
      if not EnumProcesses (Buffer, 1024*1024, BufferLen) then BufferLen := 0;
      ReallocMem (Buffer, BufferLen);
      count := BufferLen div sizeof (DWORD);

      p := Buffer;
      for i := 0 to count - 1 do
      begin
        FProcesses.Add(TProcess.Create(self, p^));
        Inc(p)
      end
    finally
      ReallocMem (Buffer, 0)
    end
  end;

  SortProcesses (FSortColumn, FSortDirection);
end;

{ TModule }

constructor TModule.Create(AOwner: TProcess; fInitDetails: PModuleEntry32);
begin
  FVersionCache := '~';
  FOwner := AOwner;
  FFileName := fInitDetails.szExePath;
  FhModule := fInitDetails.hModule;
end;

constructor TModule.Create(AOwner: TProcess; hOwner, hModule: THandle);
var
  szfileName: array [0..MAX_PATH] of char;
begin
  FVersionCache := '~';
  FOwner := AOwner;
  FhModule := hModule;
  GetModuleFileNameEx (hOwner, hModule, szFileName, sizeof (szFileName));
  FFileName := szFilename
end;

function TModule.GetFileDateTime: TDateTime;
begin
  GetFileDetails;
  Result := FFileDateTime;
end;

procedure TModule.GetFileDetails;
var
  f: TSearchRec;
begin
  if FVersionCache = '~' then
  begin
    FVersionCache := GetFileVersion (FileName);
    if FindFirst(FileName, faAnyFile, f) = 0 then
    try
      FFileDateTime := f.TimeStamp;
      FFileSize := f.Size
    finally
      FindClose (f)
    end
  end
end;

function TModule.GetFileName: string;
var
  sysDir: string;
begin
  Result := FFileName;
  if Copy (result, 1, 4) = '\??\' then
    Delete (result, 1, 4);

  if Copy (result, 1, 11) = '\SystemRoot' then
  begin
    SetLength (sysDir, MAX_PATH);
    GetWindowsDirectory (PChar (sysDir), MAX_PATH);
    sysDir := PChar (sysDir);
    Result := sysDir + Copy (result, 12, MaxInt)
  end
end;

function TModule.GetFileSize: DWORD;
begin
  GetFileDetails;
  Result := FFileSize;
end;

function TModule.GetVersion: string;
begin
  GetFileDetails;
  Result := FVersionCache;
end;

initialization
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    gOldState := EnableNTPrivilege (SE_DEBUG_NAME, SE_PRIVILEGE_ENABLED);
finalization
  if gOldState <> $badf00d then
    EnableNTPrivilege (SE_DEBUG_NAME, gOldState)
end.
