unit NTProcess;

interface

uses
  Windows, SysUtils, Classes, psapi;

type
  TWowModule = class
  end;

  TWoWTask = class
  private
    fThreadID : DWORD;
    fHMod16 : WORD;
    fHTask16 : WORD;
    fModName : string;
    fFileName : string;

  public
    property ThreadID : DWORD read fThreadID;
    property HMod16 : WORD read fHMOD16;
    property HTask16 : WORD read fHTask16;
    property ModName : string read fModName;
    property FileName : string read fFileName;
  end;

  TNTProcess = class (TComponent)
  private
    fPID : Integer;
    fModuleList : PDWORD;
    fModuleCount : Integer;
    fWOWTaskList : TList;

    procedure SetPID (value : Integer);
    function GetBaseName : string;
    procedure GetModuleList;
    function GetModuleCount : Integer;
    function GetModule (index : Integer) : HInst;
    function GetModulebaseName (index : Integer) : string;
    function GetModuleFileName (index : Integer) : string;

    procedure GetWOWTaskList;
    function GetWOWTaskCount : Integer;
    function GetWOWTask (index : Integer) : TWowTask;


  public
    destructor Destroy; override;
    property PID : Integer read fPID write SetPID;
    property BaseName : string read GetBaseName;
    property ModuleCount : Integer read GetModuleCount;
    property Module [index : Integer] : HInst read GetModule;
    property ModuleBaseName [index : Integer] : string read GetModuleBaseName;
    property ModuleFileName [index : Integer] : string read GetModuleFileName;

    function IsVDM : boolean;
    property WOWTaskCount : Integer read GetWOWTaskCount;
    property WOWTask [index : Integer] : TWOWTask read GetWowTask;
    procedure ResetWOWTaskList;
    procedure Terminate;

  end;

  TNTProcessList = class(TComponent)
  private
    fPIDList : PDWORD;
    fPIDCount : Integer;

    function GetPID (index : Integer) : Integer;
  protected
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Refresh;

    procedure GetProcess (n : Integer; var process : TNTProcess);
    procedure FindProcess (PID : Integer; var process : TNTProcess);

    property Count : Integer read fPIDCount;
    property PID [index : Integer] : Integer read GetPID;

  published
  end;

function EnableNTPrivilege (const privilege : string; state : Integer) : Integer;
procedure Register;

implementation

const SE_DEBUG_NAME = 'SeDebugPrivilege';

function EnableNTPrivilege (const privilege : string; state : Integer) : Integer;
var
  hToken : THandle;
  aluid : TLargeInteger;
  cbPrevTP : DWORD;
  tp, fPrevTP : PTokenPrivileges;
begin
  result := 0;
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

      result := fPrevTP^.Privileges [0].Attributes;

    finally
     FreeMem (fPrevTP);
     FreeMem (tp);
    end
  finally
    CloseHandle (hToken);
  end
end;

constructor TNTProcessList.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  Refresh
end;

destructor TNTProcessList.Destroy;
begin
  ReallocMem (fPIDList, 0);
  inherited
end;

procedure TNTProcessList.Refresh;
var cbNeeded : DWORD;
begin
  ReallocMem (fPIDList, 65536);
  if not EnumProcesses (fPIDList, 65536, cbNeeded) then cbNeeded := 0;
  ReallocMem (fPIDList, cbNeeded);
  fPIDCount := cbNeeded div sizeof (Integer);
end;

function TNTProcessList.GetPID (index : Integer) : Integer;
begin
  if (index >= 0) and (index < Count) then
    result := PInteger (PChar (fPIDList) + index * sizeof (Integer))^
  else
    raise ERangeError.Create ('PID index out of range');
end;

procedure TNTProcessList.GetProcess (n : Integer; var process : TNTProcess);
begin
  process.PID := PID [n]
end;

procedure TNTProcessList.FindProcess (PID : Integer; var process : TNTProcess);
var
  p : PDWORD;
  i : Integer;
begin
  p := fPIDList;
  for i := 0 to count - 1 do
  begin
    if Integer (p^) = PID then
    begin
      GetProcess (i, process);
      exit
    end
    else
      Inc (p)
  end;
  raise Exception.CreateFmt ('PID %d not found', [pid])
end;

destructor TNTProcess.Destroy;
begin
  ReallocMem (fModuleList, 0);
  ResetWOWTaskList;
  inherited
end;

procedure TNTProcess.SetPID (value : Integer);
begin
  if fPID <> value then
  begin
    fPID := value;
    ReallocMem (fModuleList, 0);
  end
end;

function TNTProcess.GetBaseName : string;
var
  handle : THandle;
  szName : array [0..MAX_PATH - 1] of char;

begin
  handle := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if handle <> 0 then
  try
    if psapi.GetModuleBaseName (handle, 0, szName, sizeof (szName)) > 0 then
      result := szName
    else
      result := 'System'
  finally
    CloseHandle (handle)
  end
  else
    if PID = 0 then
      result := 'Idle'
    else
      RaiseLastOSError;
end;

procedure TNTProcess.GetModuleList;
var
  handle : THandle;
  cbNeeded : DWORD;
begin
  handle := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  fModuleCount := 0;
  if handle <> 0 then
  try
    ReallocMem (fModuleList, 65536);
    if not EnumProcessModules (handle, fModuleList, 65536, cbNeeded) then cbNeeded := 0;
    ReallocMem (fModuleList, cbNeeded);
    fModuleCount := cbNeeded div sizeof (hInst)
  finally
    CloseHandle (Handle)
  end
  else ReallocMem (fModuleList, 0);
end;

function TNTProcess.GetModuleCount : Integer;
begin
  if fModuleList = Nil then
    GetModuleList;
  result := fModuleCount
end;

function TNTProcess.GetModule (index : Integer) : HInst;
begin
  if (index >= 0) and (index < ModuleCount) then
    result := PDWORD (PChar (fModuleList) + index * sizeof (HInst))^
  else
    raise ERangeError.Create ('Module index out of range');
end;

function TNTProcess.GetModuleBaseName (index : Integer) : string;
var
  inst : HInst;
  handle : THandle;
  szName : array [0..MAX_PATH - 1] of char;
begin
  inst := Module [index];
  handle := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if handle <> 0 then
  try
    if psapi.GetModuleBaseName (handle, inst, szName, sizeof (szName)) > 0 then
      result := szName
    else
      result := 'System'
  finally
    CloseHandle (Handle)
  end
  else
    if PID = 0 then
      result := 'Idle'
    else
      RaiseLastOSError;
end;

function TNTProcess.GetModuleFileName (index : Integer) : string;
var
  inst : HInst;
  handle : THandle;
  szName : array [0..MAX_PATH - 1] of char;
begin
  inst := Module [index];
  handle := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  if handle <> 0 then
  try
    if psapi.GetModuleFileNameEx (handle, inst, szName, sizeof (szName)) > 0 then
    begin
      result := szName;
      if Copy (result, 1, 4) = '\??\' then
        Delete (result, 1, 4);
    end
    else
      result := 'System'
  finally
    CloseHandle (Handle)
  end
  else
    if PID = 0 then
      result := 'Idle'
    else
      RaiseLastOSError;
end;

function TNTProcess.IsVDM : boolean;
var
  s : string;
  p : Integer;
begin
  s := BaseName;
  p := pos ('.', s);
  if p > 0 then s := Copy (s, 1, p - 1);
  result :=  LowerCase (s) = 'ntvdm'
end;

type
TaskEnumProcEx = function (threadID : DWORD; hMod16 : WORD; hTask16 : WORD; modName : PChar; fileName : PChar; param : DWORD) : BOOL; stdcall;

function VDMEnumTaskWOWEx (pid : DWORD; callback : TaskEnumProcEx; param : DWORD) : Integer; stdcall; external 'vdmdbg.dll';

function EnumTaskCallback (threadID : DWORD; hMod16 : WORD; hTask16 : WORD; modName : PChar; fileName : PChar; param : DWORD) : BOOL; stdcall;
var
	WOWProcess : TWowTask;
begin
	WowProcess := TWowTask.Create;
    WowProcess.fThreadID := threadID;
    WowProcess.fHMod16 := hMod16;
    WowProcess.fHTask16 := hTask16;
    WowProcess.fModName := modName;
    WowProcess.fFileName := fileName;

    TNTProcess (param).fWOWTaskList.Add (WOWProcess);

    result := False;
end;

procedure TNTProcess.GetWOWTaskList;
begin
  if fWOWTaskList = Nil then
  begin
    fWOWTaskList := TList.Create;
    VDMEnumTaskWOWEx (pid, EnumTaskCallback, DWORD (self));
  end
end;

function TNTProcess.GetWOWTaskCount : Integer;
begin
  GetWOWTaskList;
  result := fWOWTaskList.Count;
end;

function TNTProcess.GetWOWTask (index : Integer) : TWowTask;
begin
  GetWOWTaskList;
  result := TWOWTask (fWowTaskList [index])
end;

procedure TNTProcess.ResetWOWTaskList;
var
  i : Integer;
begin
  if fWOWTaskList <> Nil then
  begin
    for i := 0 to fWOWTaskList.Count - 1 do
      TWOWTask (fWOWTaskList [i]).Free;
    fWOWTaskList.Free;
    fWOWTaskList := Nil;
  end
end;

procedure Register;
begin
  RegisterComponents('NT', [TNTProcessList, TNTProcess]);
end;

procedure TNTProcess.Terminate;
var
  handle : THandle;
begin
  handle := OpenProcess (PROCESS_TERMINATE, False, pid);
  if handle <> 0 then
  try
    if not TerminateProcess (handle, 0) then
      RaiseLastOSError
  finally
    CloseHandle (handle)
  end
  else
    RaiseLastOSError;
end;

var
  oldState : DWORD;
initialization
  oldState := EnableNTPrivilege (SE_DEBUG_NAME, SE_PRIVILEGE_ENABLED);
finalization
  EnableNTPrivilege (SE_DEBUG_NAME, oldState)
end.
