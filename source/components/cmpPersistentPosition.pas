(*======================================================================*
 | cmpPersistentPosition unit for MiscUnits package                     |
 |                                                                      |
 | Drop one on your main form, and it will run in the same position     |
 | as when it was previously closed.                                    |
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
 | Copyright © Colin Wilson 2005.  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      26/02/2002  CPWW  Original                                  |
 | 1.1      11/01/2005  CPWW  Fixed bug with saving position when       |
 |                            maximized.                                |
 | 1.2      12/05/2005  CPWW  Big changes to cope with RecreateHandle   |
 |                            happening on the owner form.              |
 |                                                                      |
 |                            Now restores 'minimized' state            |
 | 1.12.0.1 24/04/2008  CPWW  Tiburon version                           |
 *======================================================================*)

unit cmpPersistentPosition;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, unitExSettings;

type
//------------------------------------------------------------------------
// TPersistentPosition class

  TOnGetSettingsClass = procedure (Owner: TObject; var SettingsClass: TExSettingsClass) of object;
  TOnGetSettingsFile = procedure (Owner: TObject; var fileName: string) of object;
  TPersistentPosition = class(TComponent)
  private
    FOldOwnerWindowMethod: TWndMethod;
    FObjectInstance: pointer;
    FManufacturer: string;
    FProduct: string;
    FVersion: string;
    FSubKey: string;
    FSaved: Boolean;
    FMoved: Boolean;
    FEnabled: Boolean;
    FSubclassed: Boolean;
    FOnGetSettingsClass: TOnGetSettingsClass;
    FOnGetSettingsFile: TOnGetSettingsFile;
    procedure OwnerWindowMethod (var msg: TMessage);
    function GetAppKey: string;
    procedure MoveToPosition;
    function GetPosition: TRect;
    procedure SavePosition;
    function CreateReg(canCreate: Boolean; var Reg: TExSettings): Boolean;
    procedure Subclass;
    procedure UnSubClass;
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    property ApplicationKey: string read GetAppKey;

    function GetValue (const valueName: string): Integer;
    function GetSzValue (const valueName: string): string;
    procedure SetValue (const valueName: string; value: Integer);
    procedure SetSzValue (const valueName, value: string);

    property Position: TRect read GetPosition;

  published
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Version: string read FVersion write FVersion;
    property Product: string read FProduct write FProduct;
    property SubKey: string read FSubKey write FSubKey;
    property Enabled: Boolean read FEnabled write FEnabled default True;

    property OnGetSettingsClass: TOnGetSettingsClass read FOnGetSettingsClass write FOnGetSettingsClass;
    property OnGetSettingsFile: TOnGetSettingsFile read FOnGetSettingsFile write FOnGetSettingsFile;
  end;

implementation

uses
  unitExFileSettings, unitExRegSettings;

resourcestring
  rstWoozle = 'Woozle';

{ TPersistentPosition }

{*----------------------------------------------------------------------*
 | constructor TPersistentPosition.Create                               |
 *----------------------------------------------------------------------*}
constructor TPersistentPosition.Create(AOwner: TComponent);
begin
  FEnabled := True;
  FManufacturer := rstWoozle;
  inherited Create (AOwner);
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.CreateReg                               |
 |                                                                      |
 | Create a TExSettings object for the application\position             |
 |                                                                      |
 | The function returns false if the registry key couldn't be opened    |
 | - maybe because canCreate was false and it didn't already exist      |
 *----------------------------------------------------------------------*}
function TPersistentPosition.CreateReg(canCreate: Boolean; var Reg: TExSettings): Boolean;
var
  prod: string;
  settingsClass: TExSettingsClass;
  st: string;
begin
  if Product = '' then
    prod := Application.Title
  else
    prod := Product;

  settingsClass := TExRegSettings;
  if Assigned(OnGetSettingsClass) then
    OnGetSettingsClass(Self, SettingsClass);

  Reg := settingsClass.Create (Manufacturer, prod, Version);
  try
    if (Reg is TExFileSettings) and Assigned(FOnGetSettingsFile) then
    begin
      st := '';
      OnGetSettingsFile(Self, st);
      TExFileSettings(Reg).CustomPath := st
    end;

    if FSubKey = '' then
      Reg.Section := 'Position'
    else
      Reg.Section := FSubKey + '\Position';

    Rresult := Reg.Open(not canCreate);
  except
    FreeAndNil (Reg);
    raise
  end
end;

(*----------------------------------------------------------------------*
 | TPersistentPosition.Destroy                                          |
 |                                                                      |
 | Destructor - Un-subclass main window.                                |
 *----------------------------------------------------------------------*)
destructor TPersistentPosition.Destroy;
begin
  UnSubclass;
  if Assigned(FObjectInstance) then
    System.Classes.FreeObjectInstance (FObjectInstance);
  inherited;
end;

{*----------------------------------------------------------------------*
 | functionTPersistentPosition.GetAppKey                                |
 |                                                                      |
 | 'Get' method for the ApplicationKey property                         |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetAppKey: string;
var
  prod: string;
begin
  if Product = '' then
    prod := Application.Title
  else
    prod := Product;

  Rresult := 'Software';
  if Manufacturer <> '' then
    Rresult := Rresult + '\' + Manufacturer;

  Rresult := Rresult + '\' + Prod;
  if Version <> '' then
    Rresult := Rresult + '\' + Version;

  if SubKey <> '' then
    Rresult := Rresult + '\' + SubKey
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetSzValue                              |
 |                                                                      |
 | Get a string value from the Position area in the registry            |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetSzValue(const valueName: string): string;
var
  Reg: TExSettings;
begin
  if CreateReg (false, Reg) then
  try
    Rresult := Reg.StringValue [valueName];
  finally
    Reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | function TPersistentPosition.GetValue                                |
 |                                                                      |
 | Get an integer value from the Position area in the registry          |
 *----------------------------------------------------------------------*}
function TPersistentPosition.GetValue(const valueName: string): Integer;
var
  Reg: TExSettings;
begin
  if CreateReg (false, Reg) then
  try
    Rresult := Reg.GetIntegerValue(valueName, 0);
  finally
    Reg.Free
  end
  else
    Rresult := 0
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.Loaded                                 |
 |                                                                      |
 | Override 'Loaded' to subclass the form's window handle so we can     |
 | intercept it's messages                                              |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    Subclass;
    if Application.MainForm = Nil then
      MoveToPosition
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.MoveToPosition                         |
 |                                                                      |
 | Move to the saved position                                           |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.MoveToPosition;
var
  CurrentForm: TForm;
  wp: TWindowPlacement;
  wasVisible: Boolean;
  Reg: TExSettings;
  requiredState: TWindowState;
begin
  if FMoved or not FEnabled then Exit;
  FMoved := True;
  if Owner is TForm then
  begin
    CurrentForm := TForm (Owner);

    if CreateReg (false, Reg) then
    try
      requiredState := TWindowState (Reg.GetIntegerValue ('State', Integer (wsNormal)));
      try
        ZeroMemory (@wp, SizeOf (wp));
        wp.length := SizeOf (wp);

        wasVisible := CurrentForm.Visible;
        if wasVisible then
          case requiredState of
            wsNormal: wp.ShowCmd := SW_SHOW;
            wsMinimized: wp.showCmd := SW_SHOWMINIMIZED;
            wsMaximized: wp.showCmd := SW_SHOWMAXIMIZED
          end
        else
          wp.ShowCmd := SW_HIDE;

        // You'd be tempted to set ShowCmd to SW_SHOW,
        // SW_SHOWMAXIMIZED, etc.  But that causes a blank
        // window to be shown then filled in after a short
        // delay - which looks ghastly!

        wp.rcNormalPosition.Left := Reg.GetIntegerValue ('Left', -1);
        wp.rcNormalPosition.Top := Reg.GetIntegerValue ('Top', -1);
        wp.rcNormalPosition.Right := Reg.GetIntegerValue ('Width', -1) + wp.rcNormalPosition.Left;
        wp.rcNormalPosition.Bottom := Reg.GetIntegerValue ('Height', -1) + wp.rcNormalPosition.Top;

        if (wp.rcNormalPosition.Left   <> -1) and
           (wp.rcNormalPosition.Top    <> -1) and
           (wp.rcNormalPosition.Right  <> -1) and
           (wp.rcNormalPosition.Bottom <> -1) then
          SetWindowPlacement (CurrentForm.Handle, @wp);

        if not wasVisible then
          CurrentForm.WindowState := requiredState;
      except
      end
    finally
      Reg.Free;
    end;
  end;
end;

procedure TPersistentPosition.SavePosition;
var
  CurrentForm: TForm;
  state: Integer;
  wp: TWindowPlacement;
  Reg: TExSettings;
begin
  if FSaved then Exit;
  FSaved := True;

  if FEnabled and (Owner is TForm) then
  begin
    CurrentForm := TForm(Owner);
    ZeroMemory (@wp, SizeOf(wp));
    wp.length := SizeOf(wp);
    GetWindowPlacement(CurrentForm.Handle, @wp);

    if IsIconic(Application.Handle) then
      state := Ord(wsMinimized)
    else
      case wp.showCmd of
        SW_SHOWMINIMIZED:
          state := Ord(wsMinimized);
        SW_SHOWMAXIMIZED:
          state := Ord(wsMaximized);
        else
          state := Ord (wsNormal)
      end;

    CreateReg (true, Reg);
    with wp.rcNormalPosition do
    try
      Reg.SetIntegerValue ('Left', Left, MaxInt);
      Reg.SetIntegerValue ('Top', Top, MaxInt);
      Reg.SetIntegerValue ('Width', Right - Left, MaxInt);
      Reg.SetIntegerValue ('Height', Bottom - Top, MaxInt);
      Reg.SetIntegerValue ('State', state, MaxInt);
    finally
      Reg.Free
    end
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set an integer value in the 'Position' area of the registry          |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetValue(const valueName: string;
  value: Integer);
var
  Reg: TExSettings;
begin
  if CreateReg (true, Reg) then
  try
    Reg.IntegerValue  [valueName] := value
  finally
    Reg.Free
  end
end;

{*----------------------------------------------------------------------*
 | procedure TPersistentPosition.SetValue                               |
 |                                                                      |
 | Set a string value in the 'Position' area of the registry            |
 *----------------------------------------------------------------------*}
procedure TPersistentPosition.SetSzValue(const valueName, value: string);
var
  Reg: TExSettings;
begin
  if CreateReg (true, Reg) then
  try
    Reg.StringValue [valueName] := value
  finally
    Reg.Free
  end
end;

function TPersistentPosition.GetPosition: TRect;
var
  Reg: TExSettings;
begin
  if CreateReg (false, Reg) then
  try
    try
      Rresult := Rect (Reg.IntegerValue ['Left'], Reg.IntegerValue ['Top'], Reg.IntegerValue ['Width'], Reg.IntegerValue ['Height']);
      Rresult.Right := Rresult.Right + Rresult.Left;
      Rresult.Bottom := Rresult.Bottom + Rresult.Top;
    except
    end
  finally
    Reg.Free
  end
  else
    Rresult := Rect (0, 0, 0, 0);
end;

procedure TPersistentPosition.Subclass;
var
  OwnerForm: TForm;
begin
  if not FSubclassed then
  begin
    if Owner is TForm then
    begin
      OwnerForm := TForm (Owner);
      FOldOwnerWindowMethod := OwnerForm.WindowProc;
      OwnerForm.WindowProc := OwnerWindowMethod
    end;
    FSubclassed := True
  end
end;

procedure TPersistentPosition.UnSubClass;
var
  OwnerForm: TForm;
begin
  if FSubclassed then
  begin
    if Owner is TForm then
    begin
      OwnerForm := TForm (Owner);
      OwnerForm.WindowProc := FOldOwnerWindowMethod
    end;

    FSubclassed := False
  end
end;

procedure TPersistentPosition.OwnerWindowMethod(var msg: TMessage);
begin
  case Msg.Msg of
    WM_MOVE:
      if not FMoved then
        MoveToPosition;
    WM_DESTROY:
      if csDestroying in ComponentState then
        SavePosition;
  end;

  FOldOwnerWindowMethod (msg)
end;

end.
