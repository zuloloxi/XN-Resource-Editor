(*======================================================================*
 | cmpMRUList                                                           |
 |                                                                      |
 | Container for list of recently used files (or strings)               |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/08/2002  CPWW  Original                                  |
 | 1.1      09/05/2007  CPWW  Ensures duplicates are removed when       |
 |                            ths list is saved                         |
 *======================================================================*)
unit cmpMRUList;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, Vcl.Menus, Vcl.Forms;

type
  TMRUList = class(TComponent)
  private
    FMRU: TStringList;
    FManufacturer: string;
    FCapacity: Integer;
    FLoaded: boolean;
    FPopupMenu: TPopupMenu;
    FOnPopupMenuClick: TNotifyEvent;
    FAppSection: string;
    FApplication: string;

    procedure SetManufacturer(const Value: string);
    procedure SetCapacity(const Value: Integer);
    function GetStrings: TStrings;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure PopulateMenu;
    procedure PopupMenuItemOnClick (sender: TObject);
    procedure SetAppSection(const Value: string);
    function GetMRUDirectory: string;
    function GetMRUFile: string;
    procedure SetApplication(const Value: string);
  protected
    function GetKeyName: string; virtual;

  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddFile (fileName: string);
    procedure SaveList;
    procedure LoadList;
    property Strings: TStrings read GetStrings;
    property MRUFile: string read GetMRUFile;
    property MRUDirectory: string read GetMRUDirectory;


  published
    property Manufacturer: string read FManufacturer write SetManufacturer;
    property Application: string read FApplication write SetApplication;
    property AppSection: string read FAppSection write SetAppSection;
    property Capacity: Integer read FCapacity write SetCapacity default 5;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;

    property OnPopupMenuClick: TNotifyEvent read FOnPopupMenuClick write FOnPopupMenuClick;
  end;

implementation

uses
  Registry;

{ TMRUList }

procedure TMRUList.AddFile(fileName: string);
var
  idx: Integer;
begin
  LoadList;
  idx := 0;
  while idx < FMRU.Count do
    if SameText (FMRU [idx], fileName) then
      FMRU.Delete(idx)
    else
      Inc (idx);

  while FMRU.Count >= Capacity do
    FMRU.Delete (FMRU.Count - 1);

  FMRU.Insert (0, fileName);
  PopulateMenu
end;

constructor TMRUList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMRU := TStringList.Create;
  FMRU.CaseSensitive := False;
  FCapacity := 5
end;

destructor TMRUList.Destroy;
begin
  if not (csDesigning in ComponentState) then
    SaveList;
  FMRU.Free;
  inherited;
end;

function TMRUList.GetKeyName: string;
var
  app: string;
begin
  if Application = '' then
    app := Vcl.Forms.Application.Title
  else
    app := Application;

  if FAppSection <> '' then
    result := Format ('SOFTWARE\%s\%s\%s', [Manufacturer, Application, FAppSection])
  else
    result := Format ('SOFTWARE\%s\%s\Recent Files', [Manufacturer, Application]);
end;

function TMRUList.GetMRUDirectory: string;
begin
  result := ExtractFilePath (MRUFile)
end;

function TMRUList.GetMRUFile: string;
begin
  if strings.Count > 0 then
    result := strings [0]
  else
    result := ''
end;

function TMRUList.GetStrings: TStrings;
begin
  LoadList;
  result := FMRU
end;

procedure TMRUList.LoadList;
var
  values: TStringList;
  i: Integer;
begin
  if not FLoaded then
  begin
    FMRU.Clear;
    if Manufacturer <> '' then
      with TRegistry.Create do
      try
        if OpenKeyReadOnly (GetKeyName) then
        begin
          values := TStringList.Create;
          try
            GetValueNames (values);
            for i := 0 to values.Count - 1 do
              FMRU.Add (ReadString (values [i]));
          finally
            values.Free
          end
        end
      finally
        Free
      end;
    FLoaded := True
  end
end;

procedure TMRUList.PopulateMenu;
var
  i: Integer;
  item: TMenuItem;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FPopupMenu) then
    begin
      FPopupMenu.Items.Clear;
      for i := 0 to Strings.Count - 1 do
      begin
        item := TMenuItem.Create (Self);
        item.Caption := '&' + IntToHex (i, 0) + ' ' + Strings [i];
        item.OnClick := PopupMenuItemOnClick;
        PopupMenu.Items.Add (item)
      end
    end
  end
end;

procedure TMRUList.PopupMenuItemOnClick(sender: TObject);
begin
  if Assigned(OnPopupMenuClick) then
    OnPopupMenuClick (sender);
end;

procedure TMRUList.SaveList;
var
  i, idx: Integer;
  s: TStrings;
  vn: string;
begin
  if (Manufacturer <> '') and FLoaded then
    if FMRU.Count > 0 then
    begin
      s := Nil;
      with TRegistry.Create do
      try
        if OpenKey (GetKeyName, True) then
        begin
          s := TStringList.Create;
          TStringList (s).CaseSensitive := False;

          GetValueNames (s);

          for i := 0 to FMRU.Count - 1 do
          begin
            vn := Format ('File %d', [i]);
            WriteString (vn, FMRU [i]);

            idx := s.IndexOf (vn);
            if idx >= 0 then
              s.Delete(idx)
          end;

          for i := 0 to s.Count - 1 do
            DeleteValue (s [i]);
        end
      finally
        s.Free;
        Free
      end
    end
    else
      RegDeleteKey (HKEY_CURRENT_USER, PChar (GetKeyName));
end;

procedure TMRUList.SetApplication(const Value: string);
begin
  if FApplication <> value then
  begin
    SaveList;
    FApplication := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetAppSection(const Value: string);
begin
  if FAppSection <> value then
  begin
    SaveList;
    FAppSection := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetCapacity(const Value: Integer);
begin
  if Capacity <> value then
  begin
    FCapacity := Value
  end
end;

procedure TMRUList.SetManufacturer(const Value: string);
begin
  if FManufacturer <> value then
  begin
    SaveList;
    FManufacturer := Value;
    FLoaded := False
  end
end;

procedure TMRUList.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> value then
  begin
    FPopupMenu := value;
    PopulateMenu
  end
end;

end.
