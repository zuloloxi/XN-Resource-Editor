unit ComponentRunOnce;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes, Vcl.Forms;

const
  WM_PARAMS = WM_USER + $200;

type
  TOnOtherInstance = procedure (Sender: TObject; ParamCount: DWORD; ParamStr: array of string) of object;

  TRunOnce = class(TComponent)
  private
    FOtherWindowHandle: HWND;
    FUniqueMessage: DWORD;
    FParamsMessage: DWORD;
    FOldOwnerWindowProc: TFNWndProc;
    FObjectInstance: pointer;
    FOnOtherInstance: TOnOtherInstance;
    FMutex: THandle;
    FName: string;
    function CheckOtherApp (hwnd: HWND): Boolean;
    procedure OwnerWindowProc(var msg: TMessage);
    procedure ProcessParameters (remoteMemHandle: THandle; remoteProcessID: DWORD);
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

implementation

{ TRunOnce }

function TRunOnce.CheckOtherApp(hwnd: HWND): Boolean;
var
  msgResult: DWORD;
begin
  Result := False;
  if hwnd <> TForm (Owner).Handle then
  begin
    if GetWindowLong (hwnd, GWL_USERDATA) = $badf00d then
      if (SendMessageTimeout(hwnd, FUniqueMessage, 0, 0, SMTO_BLOCK or SMTO_ABORTIFHUNG, 1000, @msgResult) <> 0) and (msgResult = FUniqueMessage) then
      begin
        FOtherWindowHandle := hwnd;
        Result := True
      end
  end
end;

constructor TRunOnce.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
end;

destructor TRunOnce.Destroy;
begin
  if Assigned(FObjectInstance) then
    System.Classes.FreeObjectInstance(FObjectInstance);


  if FMutex <> 0 then
  begin
    ReleaseMutex (FMutex);
    CloseHandle (FMutex)
  end;
  inherited;
end;

function EnumWindowsProc (hwnd: HWND; LParam: LParam): BOOL; stdcall;
begin
  Result := not TRunOnce (LParam).CheckOtherApp (hwnd)
end;

procedure TRunOnce.OwnerWindowProc (var msg: TMessage);
begin
  with msg do
    if Msg = FUniqueMessage then
      Result := FUniqueMessage
    else
      if Msg = FParamsMessage then
      try
        ProcessParameters (WParam, LParam)
      except
        Application.HandleException(Self)
      end
      else
        Result := CallWindowProc (FOldOwnerWindowProc, TForm (Owner).Handle, msg, WParam, LParam);
end;

procedure TRunOnce.Loaded;
var
  MapHandle: THandle;
  ParamPtr, p: PChar;
  ParamSize: DWORD;
  i: Integer;
begin
  inherited;
  if not(csDesigning in ComponentState) and (Owner is TForm) then
  begin
    FName := UpperCase (ExtractFileName (Application.Exename));
    FMutex := CreateMutex (Nil, True, PChar (FName));
    if GetLastError <>  0 then
    begin
      CloseHandle (FMutex);
      FMutex := 0
    end;
    FUniqueMessage := RegisterWindowMessage (PChar (FName));
    FParamsMessage := RegisterWindowMessage ('WoozleRunOnce');

    FObjectInstance := System.Classes.MakeObjectInstance (OwnerWindowProc);
    FOldOwnerWindowProc := TfnWndProc (SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (FObjectInstance)));

    if FMutex = 0 then
    begin
      Sleep (100);
      EnumWindows (@EnumWindowsProc, LParam (Self));

      if FOtherWindowHandle <> 0 then
      begin
        ParamSize := 1;
        for i := 0 to ParamCount do
          Inc(ParamSize, 1 + Length (ParamStr (i)));
        MapHandle := CreateFileMapping ($ffffffff, Nil, PAGE_READWRITE, 0, 65536, Nil);
        if MapHandle <> 0 then
        try
          ParamPtr := MapViewOfFile (MapHandle, FILE_MAP_WRITE, 0, 0, ParamSize);
          if ParamPtr <> Nil then
          try
            p := ParamPtr;
            for i := 0 to ParamCount do
            begin
              lstrcpy (p, PChar (ParamStr (i)));
              Inc(p, Length (ParamStr (i)) + 1)
            end;
            p^ := #0;
          finally
            UnmapViewOfFile (ParamPtr);
          end
          else
            RaiseLastOSError;

          SendMessage (FOtherWindowHandle, FParamsMessage, MapHandle, GetCurrentProcessID);
        finally
          CloseHandle (MapHandle);
        end
        else
          RaiseLastOSError;

        SetForegroundWindow (FOtherWindowHandle)
      end;
      Halt;
    end
    else
    SetWindowLong (TForm (Owner).Handle, GWL_USERDATA, $badf00d)
  end
end;

procedure TRunOnce.ProcessParameters(remoteMemHandle: THandle; remoteProcessID: DWORD);
var
  memHandle: THandle;
  remoteProcessHandle: THandle;
  ParamPtr: PChar;
  p: PChar;
  paramCount: DWORD;
  params: array of string;
  i: Integer;
begin
  remoteProcessHandle := OpenProcess (PROCESS_DUP_HANDLE, false, remoteProcessID);
  if remoteProcessHandle <> 0 then
  try
    if DuplicateHandle (remoteProcessHandle, remoteMemHandle, GetCurrentProcess, @memHandle, FILE_MAP_READ, False, 0) then
    try
      ParamPtr := MapViewOfFile (memHandle, FILE_MAP_READ, 0, 0, 65536);
      if ParamPtr <> Nil then
      try
        if Assigned(FOnOtherInstance) and not(csDestroying in ComponentState) then
        begin
          p := ParamPtr;
          paramCount := 0;
          while p^ <> #0 do
          begin
            Inc(paramCount);
            Inc(p, lstrlen (p) + 1);
          end;
          SetLength (params, paramCount);
          p := ParamPtr;
          i := 0;
          while p^ <> #0 do
          begin
            params [i] := p;
            Inc(p, lstrlen (p) + 1);
            Inc(i);
          end;

          OnOtherInstance (Self, paramCount - 1, params);
        end
      finally
        UnmapViewOfFile (ParamPtr);
      end
      else
        RaiseLastOSError;
    finally
      CloseHandle (memHandle);
    end
    else
      RaiseLastOSError;
  finally
    CloseHandle (remoteProcessHandle);
  end
  else
    RaiseLastOSError;
end;

end.
