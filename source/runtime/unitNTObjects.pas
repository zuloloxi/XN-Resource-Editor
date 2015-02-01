unit unitNTObjects;

interface

uses Windows, Classes, SysUtils, ntddk, ConTnrs, SyncObjs;

type

TSystemHandles = class
private
  fInfo : PSystemHandleInformation;
  function GetCount: Integer;
  function GetInfo (idx : Integer) : PSystemHandleTableEntryInfo;
  function GetHandle(idx: Integer): THandle;
  function GetObj(idx: Integer): Pointer;
  function GetPID(idx: Integer): DWORD;
  function GetObjectTypeIndex(idx: Integer): DWORD;
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
  property Info [idx : Integer] : PSystemHandleTableEntryInfo read GetInfo;
end;

TNameResolverState = (rsNotResolved, rsResolving, rsResolved, rsCantResolve);

TFileHandle = class
private
  fHandle : word;
  fPID : DWORD;
  fType : String;
  fAccess : DWORD;
  fNameResolverState : TNameResolverState;
  fName : string;
public
  constructor Create (AHandleValue : word; AUniqueProcessId, AAccess : DWORD; AType : String);

  property Handle : word read fHandle;
  property PID : DWORD read fPID;
  property HandleType : string read fType;
  property Name : string read fName;
end;

TFileHandles = class
private
  fSystemHandles : TSystemHandles;
  fFileHandles : TObjectList;
  fFileObjectIndex : Integer;

  procedure InitFileObjectIndex;
  procedure Load;
  function GetCount: Integer;
  function GetHandle(idx: Integer): TFileHandle;
public
  constructor Create (ASysHandles : TSystemHandles);
  destructor Destroy; override;

  procedure Refresh;

  property Count : Integer read GetCount;
  property Handle [idx : Integer] : TFileHandle read GetHandle; default;
end;


implementation

resourcestring
  rstInvalidHandleIndex = 'Invalid handle index';

type
  THandleNameResolver = class;

  THandleNameResolverThread = class
  private
    fOwner : THandleNameResolver;
    fStuck : boolean;
    fResolving : boolean;
    fStartResolveTickCount : DWORD;
    fCurrentlyResolving : TFileHandle;
    fHandle : THandle;
    fThreadId : DWORD;
    fExitCode: DWORD;

    function GetStuck: boolean;
  protected
    procedure Execute;
  public
    constructor Create (AOwner : THandleNameResolver);
    destructor Destroy; override;
    procedure Resume;

    property Handle : THandle read fHandle;
    property Owner : THandleNameResolver read fOwner;
    property Stuck : boolean read GetStuck;
    property ExitCode : DWORD read fExitCode write fExitCode;
  end;

  THandleNameResolver = class
  private
    fFileHandles : TFileHandles;
    fThreads : TObjectList;
    fDone : boolean;
    fTimeLimit : DWORD;
    fCurrentHandleIdx : DWORD;
    fSync : TCriticalSection;

    function GetNextHandle (var handle : TFileHandle) : boolean;
  public
    constructor Create (AFileHandles : TFileHandles; AThreadCount : Integer = 3);
    destructor Destroy; override;
    procedure ResolveNames;

    property TimeLimit : DWORD read fTimeLimit write fTimeLimit default 100;
    
  end;

function CopyHandle (pid : DWORD; h : THandle) : THandle;
var
  hProcess : THandle;
  hObject : THandle;
begin
  hObject := 0;
  hProcess := OpenProcess (PROCESS_DUP_HANDLE, FALSE, pid);
  if hProcess <> 0 then
  try
    if not DuplicateHandle (hProcess, h, GetCurrentProcess, @hObject, 0, FALSE, DUPLICATE_SAME_ACCESS) then
      hObject := 0;
  finally
    CloseHandle (hProcess);
  end;
  result := hObject
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
  end
end;

{ TFileHandles }

constructor TFileHandles.Create(ASysHandles: TSystemHandles);
begin
  fSystemHandles := ASysHandles;
  InitFileObjectIndex
end;

destructor TFileHandles.Destroy;
begin
  fFileHandles.Free;

  inherited;
end;

function TFileHandles.GetCount: Integer;
begin
  if fFileHandles = Nil then
    Load;
  result := fFileHandles.Count
end;

function TFileHandles.GetHandle(idx: Integer): TFileHandle;
begin
  if fFileHandles = Nil then
    Load;
  result := TFileHandle (fFileHandles [idx]);
end;

procedure TFileHandles.InitFileObjectIndex;
var
  sysHandles : TSystemHandles;
  handle : THandle;
  i : Integer;
  st, tempfName : string;
begin
  SetLength (st, MAX_PATH + 1);
  if GetTempPath (MAX_PATH + 1, PChar (st)) = 0 then
    RaiseLastOSError;
    
  SetLength (tempfName, MAX_PATH+1);
  if GetTempFileName (PChar (st), 'hxp', 0, PChar (tempfName)) = 0 then
    RaiseLastOSError;

  tempfName := PChar (tempfName);

  sysHandles := Nil;
  handle := CreateFile (PChar (tempfName), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  try
    if handle = 0 then
      RaiseLastOSError;

    sysHandles := TSystemHandles.Create;

    i := sysHandles.Find (GetCurrentProcessID, handle);

    fFileObjectIndex := sysHandles.ObjectTypeIndex [i];
  finally
    sysHandles.Free;
    CloseHandle (handle);
    DeleteFile (tempfName)
  end
end;

procedure TFileHandles.Load;
var
  i : Integer;
  sysHandleInfo : PSystemHandleTableEntryInfo;
  objBufSize, err, bytesWritten : DWORD;
  objBuf : PObjectNameInformation;
  dupHandle : THandle;
  tp : WideString;
  nameResolver : THandlenameResolver;

begin
  if Assigned (fFileHandles) then
    fFileHandles.Clear
  else
    fFileHandles := TObjectList.Create;

  objBufSize := 2048;
  GetMem (objBuf, objBufSize);
  try

    for i := 0 to fSystemHandles.Count - 1 do
    begin
      sysHandleInfo := fSystemHandles.Info [i];
      if sysHandleInfo.ObjectTypeIndex <> fFileObjectIndex then
        Continue;

      with sysHandleInfo^ do
      begin
        dupHandle := CopyHandle (UniqueProcessID, HandleValue);
        try
          objBuf^.Name.Length := 0;
          objBuf^.Name.MaximumLength := objBufSize - sizeof (TUnicodeString);
          
          err := fnZwQueryObject (dupHandle, objectTypeInformation, objBuf, objBufSize, @bytesWritten);
        finally
          CloseHandle (dupHandle)
        end;

        if err = 0 then
        begin
          SetString (tp, objBuf^.Name.Buffer, objBuf^.Name.Length div sizeof (WideChar));
          fFileHandles.Add(TFileHandle.Create (HandleValue, UniqueProcessID, GrantedAccess, tp))
        end
      end
    end;

    nameResolver := THandleNameResolver.Create(self);
    try
      nameResolver.ResolveNames
    finally
      nameResolver.Free
    end;
  finally
    FreeMem (objBuf)
  end;
end;

procedure TFileHandles.Refresh;
begin
  fSystemHandles.Refresh;
  Load
end;

{ TFileHandle }

constructor TFileHandle.Create(AHandleValue: word; AUniqueProcessId, AAccess : DWORD; AType : String);
begin
  fHandle := AHandleValue;
  fPID := AUniqueProcessId;
  fType := AType;
  fAccess := AAccess;

  if fType <> 'File' then
    MessageBeep ($ffff);
end;

{ THandleNameResolverThread }

function ThreadProc (param : Pointer) : DWORD; stdcall;
var
  thread : THandleNameResolverThread;
begin
  thread := THandleNameResolverThread (param);
  thread.Execute;

  result := thread.ExitCode;
end;

constructor THandleNameResolverThread.Create(AOwner: THandleNameResolver);
begin
  fOwner := AOwner;
  fHandle := CreateThread (Nil, 0, @ThreadProc, self, CREATE_SUSPENDED, fThreadID);
end;

destructor THandleNameResolverThread.Destroy;
begin
  CloseHandle (fHandle);
end;


procedure THandleNameResolverThread.Execute;
var
  h : TFileHandle;
  objBufSize, err, bytesWritten : DWORD;
  objBuf : PObjectNameInformation;
  dupHandle : THandle;
  s : WideString;
begin
  try
    objBufSize := 2048;
    GetMem (objBuf, objBufSize);

    while not Owner.fDone do
    begin
      if Owner.GetNextHandle (h) then
      begin
    (*
        if h.fAccess = $0012019f then
        begin
          h.fNameResolverState := rsCantResolve;
          Continue
        end;
*)
        fCurrentlyResolving := h;
        dupHandle := 0;
        try
          dupHandle := CopyHandle (h.fPID, h.fHandle);

          objBuf^.Name.Length := 0;
          objBuf^.Name.MaximumLength := objBufSize - sizeof (TUnicodeString);

          fStartResolveTickCount := GetTickCount;
          fResolving := True;
          try
            err := fnZwQueryObject (dupHandle, objectNameInformation, objBuf, objBufSize, @bytesWritten);
          finally
            fResolving := False;
          end;

          if err = 0 then
          begin
            SetString (s, objBuf^.Name.Buffer, objBuf^.Name.Length div sizeof (WideChar));
            h.fName := s;
            h.fNameResolverState := rsResolved
          end
          else
            h.fNameResolverState := rsCantResolve;
        finally
          if dupHandle <> 0 then
            CloseHandle (dupHandle);

        end;
      end
      else
        if not Owner.fDone then
          Sleep (50);
    end
  except
  end
end;

function THandleNameResolverThread.GetStuck: boolean;
begin
  if fStuck then
    result := True
  else
    if fResolving and ((GetTickCount - fStartResolveTickCount) > Owner.TimeLimit) then
      result := true
    else
      result := false;
      
  fStuck := result
end;

procedure THandleNameResolverThread.Resume;
begin
  ResumeThread (fHandle);
end;

{ THandleNameResolver }

constructor THandleNameResolver.Create(AFileHandles: TFileHandles;
  AThreadCount: Integer);
var
  i : Integer;
begin
  fTimeLimit := 200; // ms
  fFileHandles := AFileHandles;
  fSync := TCriticalSection.Create;

  fThreads := TObjectList.Create;
  for i := 0 to AThreadCount - 1 do
    fThreads.Add(THandleNameResolverThread.Create(self))
end;

destructor THandleNameResolver.Destroy;
begin
  fThreads.Free;
  fSync.Free;

  inherited;
end;

function THandleNameResolver.GetNextHandle(var handle: TFileHandle): boolean;
var
  h : TFileHandle;
  start : DWORD;
  done : boolean;
begin
  if fDone or (fFileHandles.Count = 0) then
  begin
    result := False;
    exit
  end;

  fSync.Enter;
  try
    start := fCurrentHandleIdx;
    done := true;

    repeat
      h := TFileHandle (fFileHandles [fCurrentHandleIdx]);
      Inc (fCurrentHandleIdx);
      if fCurrentHandleIdx = DWORD (fFileHandles.Count) then
        fCurrentHandleIdx := 0;

      if h.fNameResolverState < rsResolved then
        done := False;

      if h.fNameResolverState = rsNotResolved then
      begin
        handle := h;
        result := True;
        h.fNameResolverState := rsResolving;
        Exit
      end
    until fCurrentHandleIdx = start;

    result := False;
    fDone := done
  finally
    fSync.Leave
  end
end;

procedure THandleNameResolver.ResolveNames;
var
  i : Integer;
  thread : THandleNameResolverThread;
begin
  for i := 0 to fThreads.Count - 1 do
  begin
    thread := THandleNameResolverThread (fThreads [i]);
    thread.Resume;
  end;

  while not fDone do
  begin
    i := 0;
    if i = fThreads.Count then
    begin
      fDone := True;
      break
    end;
    
    while i < fThreads.Count do
    begin
      thread := THandleNameResolverThread (fThreads [i]);

      if thread.Stuck then
      begin
        thread.fCurrentlyResolving.fNameResolverState := rsCantResolve;
        if not TerminateThread (thread.Handle, 0) then
          RaiseLastOSError;
        fThreads.Delete(i);

        thread := THandleNameResolverThread.Create(self);
        fThreads.Add(thread);
        thread.Resume
      end;

      Inc (i);
    end
  end
end;

end.
