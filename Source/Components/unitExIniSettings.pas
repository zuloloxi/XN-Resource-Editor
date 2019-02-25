(*======================================================================*
 | unitExIniSettings                                                    |
 |                                                                      |
 | Ini file application settings classes.                               |
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
unit unitExIniSettings;

interface

uses
  Classes, Sysutils, IniFiles, unitExSettings, unitExFileSettings;

type
  //-----------------------------------------------------------------------
  // TIniExSettings.
  //
  // Class to store application and other settings to INI files
  TExIniSettings = class (TExFileSettings)
  private
    FIniFile: TCustomIniFile;
    FAutoReadOnly: Boolean;

    function FixSection: string;
  protected
    function IsOpen: Boolean; override;
    function CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen; override;
    procedure InternalSetStringValue (const valueName, value: string); override;
  public
    procedure Close; override;
    function Open(readOnly: Boolean = False): Boolean; override;

    procedure DeleteValue(const valueName: string); override;
    procedure DeleteSection(const sectionName: string); override;

    function HasSection(const ASection: string): Boolean; override;
    function HasValue(const AValue: string): Boolean; override;
    procedure GetValueNames(names: TStrings); override;
    procedure GetSectionNames(names: TStrings); override;

    function GetStringValue(const valueName: string; const deflt: string = ''): string; override;
    function GetIntegerValue(const valueName: string; deflt: Integer = 0): Integer; override;
  end;

implementation

{ TExIniSettings }

(*----------------------------------------------------------------------*
 | function TExIniSettings.CheckIsOpen                                  |
 |                                                                      |
 | Ensure that the file is open.                                        |
 *----------------------------------------------------------------------*)
function TExIniSettings.CheckIsOpen(readOnly, autoReadOnly: Boolean): TIsOpen;
var
  fn: string;
begin
  Result := inherited CheckIsOpen(readOnly, autoReadOnly);

  case Result of
    woClosed :
      begin
        if Open (readOnly) then
        begin
          Result := woOpen;
          fReadOnly := False
        end
        else
          Result := woClosed;
      end;

    woReopen:
      begin
        FAutoReadOnly := readOnly;

        fn := GetFileName ('.ini');
        if not readOnly then
          ForceDirectories (ExtractFilePath (fn));
        Result := woOpen;
      end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.Close                                       |
 |                                                                      |
 | Close the INI file                                                   |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.Close;
begin
  FreeAndNil(FIniFile);
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.DeleteValue                                 |
 |                                                                      |
 | Delete a value                                                       |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.DeleteSection(const sectionName: string);
begin
  CheckIsOpen(False, FAutoReadOnly);

  FIniFile.EraseSection(FixSection);
end;

procedure TExIniSettings.DeleteValue(const valueName: string);
begin
  CheckIsOpen(False, FAutoReadOnly);

  FIniFile.DeleteKey(FixSection, valueName);
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.FixSection                                  |
 |                                                                      |
 | Return a valid INI file section name for the current section         |
 *----------------------------------------------------------------------*)
function TExIniSettings.FixSection: string;
begin
  Result := Section;
  if Result = '' then
    Result := 'Default';
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.GetIntegerValue                              |
 |                                                                      |
 | Return an integer value, or default if the value doesn't exist       |
 *----------------------------------------------------------------------*)
function TExIniSettings.GetIntegerValue(const valueName: string;
  deflt: Integer): Integer;
var
  st: string;
begin
  CheckIsOpen(true, FAutoReadOnly);

  st := FIniFile.ReadString(FixSection, valueName, #1);
  if st = #1 then
    Result := deflt
  else
    if not TryStrToInt(st, Result) then
      raise EExSettings.Create('Integer value expected');
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.GetStringValue                               |
 |                                                                      |
 | Return a string value, or default if the value doesn't exist         |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.GetSectionNames(names: TStrings);
begin
  names.Clear;
  if CheckIsOpen(true, FAutoReadOnly) = woOpen then
    FIniFile.ReadSections(names);
end;

function TExIniSettings.GetStringValue(const valueName, deflt: string): string;
begin
  if CheckIsOpen(true, FAutoReadOnly) = woOpen then
    Result := FIniFile.ReadString(FixSection, valueName, deflt)
  else
    Result := deflt;
end;

procedure TExIniSettings.GetValueNames(names: TStrings);
begin
  names.Clear;
  if CheckIsOpen(true, FAutoReadOnly) = woOpen then
    FIniFile.ReadSection(FixSection, names);
end;

function TExIniSettings.HasSection(const ASection: string): Boolean;
var
  sec: string;
begin
  if CheckIsOpen(true, FAutoReadOnly) = woOpen then
  begin
    if Section = '' then
      sec := ASection
    else
      sec := Section + '\' + ASection;

    if sec = '' then
      Result := True
    else
      Result := FIniFile.SectionExists(sec)
  end
  else
    Result := False
end;

function TExIniSettings.HasValue(const AValue: string): Boolean;
begin
  if CheckIsOpen(true, FAutoReadOnly) = woOpen then
    Result := FIniFile.ValueExists(FixSection, AValue)
  else
    Result := False
end;

(*----------------------------------------------------------------------*
 | procedure TExINISettings.InternalSetStringValue                      |
 |                                                                      |
 | Set a string value                                                   |
 *----------------------------------------------------------------------*)
procedure TExIniSettings.InternalSetStringValue(const valueName, value: string);
begin
  CheckIsOpen(False, FAutoReadOnly);

  FIniFile.WriteString(FixSection, valueName, value);
end;

(*----------------------------------------------------------------------*
 | function TExINISettings.IsOpen                                       |
 |                                                                      |
 | Return true if the object is Open                                    |
 *----------------------------------------------------------------------*)
function TExIniSettings.IsOpen: Boolean;
begin
  Result := FIniFile <> Nil;
end;

(*----------------------------------------------------------------------*
 | procedure TExIniSettings.Open                                        |
 |                                                                      |
 | Open the Ini file.  Create it if it doesn't exist                    |
 *----------------------------------------------------------------------*)
function TExIniSettings.Open(readOnly: Boolean): Boolean;
var
  fn: string;
begin
  inherited Open (readOnly);
  Result := True;
  Close;
  FAutoReadOnly := readOnly;

  fn := GetFileName ('.ini');
  if not readOnly then
    ForceDirectories (ExtractFilePath (fn))
  else
    if not FileExists (fn) then
    begin
      Result := False;
      Exit;
    end;


  FIniFile := TIniFile.Create(fn);
end;

end.
