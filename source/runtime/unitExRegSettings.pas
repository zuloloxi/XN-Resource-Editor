(*======================================================================*
 | unitExRegSettings                                                    |
 |                                                                      |
 | Registry application settings classes.                               |
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
 | Copyright © Colin Wilson 2006  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      02/03/2006  CPWW  Original                                  |
 *======================================================================*)
unit unitExRegSettings;

interface

uses
  Windows, Classes, SysUtils, Registry, unitExSettings;

type
  //-----------------------------------------------------------------------
  // TExRegSettings.
  //
  // Class to store application and other settings to the registry
  TExRegSettings = class (TExSettings)
  private
    FCurrentReg: TRegistry;
    FParentKey: HKEY;
    FAutoReadOnly: Boolean;
    FChildSection: string;
    function GetCurrentKey: HKEY;

    function ApplicationKeyName: string;
  protected
    function IsOpen: Boolean; override;
    function CheckIsOpen (readOnly, autoReadOnly: Boolean): TIsOpen; override;
    procedure SetSection (const SectionPath: string); override;
    procedure InternalSetIntegerValue (const valueName: string; value: Integer); override;
    procedure InternalSetStringValue (const valueName, value: string); override;
  public
    constructor CreateChild (AParent: TExSettings; const ASection: string); override;
    procedure Close; override;
    function Open (readOnly: Boolean = false): Boolean; override;
    procedure Flush; override;

    property CurrentKey: HKEY read GetCurrentKey;

    procedure DeleteValue (const valueName: string); override;
    procedure DeleteSection (const sectionName: string); override;
    function HasSection (const ASection: string): Boolean; override;
    function HasValue (const AValue: string): Boolean; override;
    procedure GetValueNames (names: TStrings); override;
    procedure GetSectionNames (names: TStrings); override;
    function GetExportValue (const valueName: string): string; override;

    function GetStringValue  (const valueName: string; const deflt: string = ''): string; override;
    function GetIntegerValue (const valueName: string; deflt: Integer = 0): Integer; override;
    function GetStrings      (const valueName: string; sl: TStrings): Integer; override;

    procedure SetStrings      (const valueName: string; sl: TStrings); override;

    procedure RenameSection (const oldValue, newValue: string); override;
    procedure RenameValue (const oldValue, newValue: string); override;
  end;

implementation

resourcestring
  rstReadError = 'Error %d reading value %s';
  rstOpenError = 'Error %d opening %s';
  rstDeleteError = 'Error %d deleting %s';
  rstWriteError = 'Error %d writing value %s';


{ TExRegSettings }

(*----------------------------------------------------------------------*
 | function TExRegSettings.ApplicationKeyName                           |
 |                                                                      |
 | Return the key name for the currently selected section               |
 *----------------------------------------------------------------------*)
function TExRegSettings.ApplicationKeyName: string;
begin
  if Application = '' then
    Result := Section
  else
  begin
    if Manufacturer = '' then
      Result := 'Software\' + Application
    else
      Result := 'Software\' + Manufacturer + '\' + Application;

    if Version <> '' then
      Result := Result + '\' + Version;

    if Section <> '' then
      Result := Result + '\' + Section
  end;

  if Result = '' then
    raise EExSettings.Create('Must specify an Application or a Section');
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.CheckIsOpen                                  |
 |                                                                      |
 | Ensure that the registry is open for reading or writing.             |
 *----------------------------------------------------------------------*)
function TExRegSettings.CheckIsOpen (readOnly, autoReadOnly: Boolean): TIsOpen;
var
  ok: Boolean;
begin
  Result := inherited CheckIsOpen (readOnly, autoReadOnly);

  if (Result = woClosed) or (Result = woReopen) then
  begin
    ok := Open (readOnly);
    if Result = woClosed then
      fReadOnly := False;

    if ok then
      Result := woOpen
    else
      Result := woClosed
  end
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.Close                                        |
 |                                                                      |
 | Close the registry                                                   |
 *----------------------------------------------------------------------*)
procedure TExRegSettings.Close;
begin
  FreeAndNil (FCurrentReg);
end;

constructor TExRegSettings.CreateChild(AParent: TExSettings;
  const ASection: string);
begin
  inherited;
  FChildSection := ASection;
  FParentKey := TExRegSettings (AParent).CurrentKey;
end;

procedure TExRegSettings.DeleteSection(const sectionName: string);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.DeleteKey (sectionName);
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.DeleteValue                                  |
 |                                                                      |
 | Delete the specified value from the current key                      |
 *----------------------------------------------------------------------*)
procedure TExRegSettings.DeleteValue(const valueName: string);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.DeleteValue(valueName)
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.Flush                                        |
 |                                                                      |
 | Flush the registry (don't do anything!)
 *----------------------------------------------------------------------*)
procedure TExRegSettings.Flush;
begin
  inherited;
  // Could call RegFlushKey - but isn't usually required
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.GetIntegerValue                              |
 |                                                                      |
 | Return an integer value, or the default if the value doesn't exist   |
 *----------------------------------------------------------------------*)
function TExRegSettings.GetCurrentKey: HKEY;
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    Result := FCurrentReg.CurrentKey
  else
    Result := 0;
end;

function MakeCStringConst(const s: string; len: Integer = -1): string;
var
  i: Integer;
begin
  Result := '';
  if len = -1 then
    len := Length (s);
  for i := 1 to len do
  begin
    if (s [i] = '\') or (s [i] = '"') then
      Result := Result + '\';
    Result := Result + s [i]
  end;
  Result := PChar (Result)
end;

function TExRegSettings.GetExportValue(const valueName: string): string;
var
  tp: DWORD;
  st, st1: string;
  j, dataLen: Integer;
  data: PByte;
begin
  Result := '';
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
  begin
    if RegQueryValueEx (FCurrentReg.CurrentKey, PChar (valueName), Nil, @tp, Nil, PDWORD (@dataLen)) = ERROR_SUCCESS then
    begin
      if valueName = '' then
        st := '@='
      else
        st := Format('"%s"=', [MakeCStringConst(valueName)]);

      case tp of
        REG_DWORD :
        begin
          st1 := LowerCase (Format('%8.8x', [IntegerValue [valueName]]));
          st := st + format('dWord:%s', [st1])
        end;

        REG_SZ    :
            st := st + format('"%s"', [MakeCStringConst(StringValue [valueName])]);

        else
        begin
          if tp = REG_BINARY then
            st := st + 'hex:'
          else
            st := st + format('hex(%d):', [tp]);
          GetMem (data, dataLen);
          RegQueryValueEx (FCurrentReg.CurrentKey, PChar (valueName), Nil, @tp, data, @dataLen);
          for j := 0 to dataLen - 1 do
          begin
            st1 := LowerCase (format('%02.2x', [Byte (PChar (data) [j])]));
            if j < dataLen - 1 then
              st1 := st1 + ',';

            if Length (st) + Length (st1) >= 77 then
            begin
              Result := Result + st + st1 + '\' + #13#10;
              st := '  ';
            end
            else
              st := st + st1;
          end
        end
      end;

      Result := Result + st;
    end
  end;
end;

function TExRegSettings.GetIntegerValue(const valueName: string;
  deflt: Integer): Integer;
begin
  if (CheckIsOpen (true, FAutoReadOnly) = woOpen) and FCurrentReg.ValueExists (valueName) then
    Result := FCurrentReg.ReadInteger(valueName)
  else
    Result := deflt
end;

procedure TExRegSettings.GetSectionNames(names: TStrings);
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    FCurrentReg.GetKeyNames(names)
  else
    names.Clear;
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.GetStringValue                               |
 |                                                                      |
 | Return a string value, or the default if the value doesn't exist     |
 *----------------------------------------------------------------------*)
function TExRegSettings.GetStrings(const valueName: string;
  sl: TStrings): Integer;
var
  valueType, rv: DWORD;
  valueLen: DWORD;
  p, buffer: PChar;
begin
  sl.Clear;
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
  begin
    rv := RegQueryValueEx (CurrentKey, PChar (valueName), Nil, @valueType, Nil, @valueLen);
    if rv = ERROR_SUCCESS then
      if valueType = REG_MULTI_SZ then
      begin
        GetMem (buffer, valueLen);
        try
          RegQueryValueEx (CurrentKey, PChar (valueName), Nil, Nil, PBYTE (buffer), @valueLen);
          p := buffer;
          while p^ <> #0 do
          begin
            sl.Add (p);
            Inc(p, lstrlen (p) + 1)
          end
        finally
          FreeMem (buffer)
        end
      end
      else
        raise EExSettings.Create ('String list expected')
    else
      raise EExSettings.Create ('Unable read MULTI_SZ value');
  end;
  Result := sl.Count
end;

function TExRegSettings.GetStringValue(const valueName, deflt: string): string;
begin
  if (CheckIsOpen (true, FAutoReadOnly) = woOpen) and FCurrentReg.ValueExists (valueName) then
    Result := FCurrentReg.ReadString(valueName)
  else
    Result := deflt
end;

procedure TExRegSettings.GetValueNames(names: TStrings);
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    FCurrentReg.GetValueNames (names)
  else
    names.Clear;
end;

function TExRegSettings.HasSection(const ASection: string): Boolean;
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    Result := FCurrentReg.KeyExists(ASection)
  else
    Result := false
end;

function TExRegSettings.HasValue(const AValue: string): Boolean;
begin
  if CheckIsOpen (true, FAutoReadOnly) = woOpen then
    Result := FCurrentReg.ValueExists (AValue)
  else
    Result := false
end;

(*----------------------------------------------------------------------*
 | procedure TExRegSettings.InternalSetStringValue                      |
 |                                                                      |
 | Set an integer value.                                                |
 *----------------------------------------------------------------------*)

procedure TExRegSettings.InternalSetIntegerValue(const valueName: string;
  value: Integer);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.WriteInteger(valueName, value)
  else
    raise EExSettings.Create ('Unable to write value ' + valueName);
end;

(*----------------------------------------------------------------------*
 | procedure TExRegSettings.InternalSetStringValue                      |
 |                                                                      |
 | Set a string value.                                                  |
 *----------------------------------------------------------------------*)
procedure TExRegSettings.InternalSetStringValue(const valueName, value: string);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.WriteString(valueName, value)
  else
    raise EExSettings.Create ('Unable to write value ' + valueName);
end;

(*----------------------------------------------------------------------*
 | function TExRegSettings.IsOpen                                       |
 |                                                                      |
 | Return true if the object is Open                                    |
 *----------------------------------------------------------------------*)
function TExRegSettings.IsOpen: Boolean;
begin
  Result := Assigned(FCurrentReg)
end;

(*----------------------------------------------------------------------*
 | procedure TExRegSettings.Open                                        |
 |                                                                      |
 | Open the registry key.  Create it if it doesn't exist                |
 *----------------------------------------------------------------------*)
function TExRegSettings.Open(readOnly: Boolean): Boolean;
var
  rootKey: HKEY;
  section: string;
  rights: DWORD;
begin
  inherited Open (readOnly);
  Close;
  FAutoReadOnly := readOnly;

  if FParentKey = 0 then
  begin
    section := ApplicationKeyName;
    if SettingsType = stMachine then
      rootKey := HKEY_LOCAL_MACHINE
    else
      rootKey := HKEY_CURRENT_USER
  end
  else
  begin
    rootKey := FParentKey;
    section := FChildSection
  end;

  if readOnly then
    rights := KEY_READ
  else
    rights := KEY_READ or KEY_WRITE;


  FCurrentReg := TRegistry.Create (rights);
  FCurrentReg.RootKey := rootKey;

  Result := FCurrentReg.OpenKey(section, not readOnly)
end;

procedure TExRegSettings.RenameSection(const oldValue, newValue: string);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.MoveKey(oldValue, newValue, true);
end;

procedure TExRegSettings.RenameValue(const oldValue, newValue: string);
begin
  if CheckIsOpen (false, FAutoReadOnly) = woOpen then
    FCurrentReg.RenameValue(oldValue, newValue);
end;

(*----------------------------------------------------------------------*
 | procedure TExRegSettings.SetSection                                  |
 |                                                                      |
 | Override the 'Set' method for the Section property                   |
 *----------------------------------------------------------------------*)
procedure TExRegSettings.SetSection(const SectionPath: string);
begin
  Close;      // Close the registry

  if FParentKey <> 0 then
    FChildSection := SectionPath
  else
    inherited
end;

procedure TExRegSettings.SetStrings(const valueName: string; sl: TStrings);
var
  p, buffer: PChar;
  i: Integer;
  size: DWORD;
begin
  if CheckIsOpen (false, FAutoReadOnly) <> woOpen then
    raise EExSettings.Create ('Unable to write MULTI_SZ value');

  size := 0;
  for i := 0 to sl.Count - 1 do
    Inc(size, Length (sl [i]) + 1);
  Inc(size);
  GetMem (buffer, size);
  try
    p := buffer;
    for i := 0 to sl.count - 1 do
    begin
      lstrcpy (p, PChar (sl [i]));
      Inc(p, lstrlen (p) + 1)
    end;
    p^ := #0;
    SetLastError (RegSetValueEx (CurrentKey, PChar (valueName), 0, REG_MULTI_SZ, buffer, size));
    if GetLastError <> ERROR_SUCCESS then
      raise EExSettings.Create ('Unable to write MULTI_SZ value');
  finally
    FreeMem (buffer)
  end
end;

end.
