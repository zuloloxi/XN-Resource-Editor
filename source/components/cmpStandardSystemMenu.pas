(*======================================================================*
 | cmpStandardSystemMenu unit for Miscellaneous Components package      |
 |                                                                      |
 | Drop one on your programs main form, and you program will have the   |
 | correct system menu entries when you right-click on the taskbar icon |
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
 | Copyright © Colin Wilson 2002  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/01/2002  CPWW  Original                                  |
 *======================================================================*)

unit cmpStandardSystemMenu;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus;

const
  scxRESTORE  = $fff0;
  scxMINIMIZE = $fff2;
  scxMAXIMIZE = $fff3;
  scxSIZE     = $fff4;
  scxMOVE     = $fff5;

type
  TStandardSystemMenu = class(TComponent)
  private
    fMenuHandle : HMenu;
    fWindowMenuHandle : HMenu;
    fObjectInstance : pointer;
    fOldOwnerWindowProc : TFNWndProc;

    fSysObjectInstance : pointer;
    fOldSysWindowProc : TFNWndProc;

    fIconic : boolean;
    fMaximized : boolean;

    procedure CloneSystemMenu;
    procedure OwnerWindowProc(var msg: TMessage);
    procedure SysOwnerWindowProc(var msg: TMessage);

    procedure OnMinimized;
    procedure OnMaximized;
    procedure OnRestored (resetmax : boolean);

    function HookProc (var Msg : TMessage) : boolean;

  protected
    procedure Loaded; override;
    { Protected declarations }
  public
    procedure SetItemState (itemID, state : Integer);
    destructor Destroy; override;
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents ('Samples', [TStandardSystemMenu])
end;

{ TStandardSystemMenu }

procedure TStandardSystemMenu.CloneSystemMenu;
var
  count : Integer;
  item : TMenuItemInfo;
  buffer : array [0..256] of char;
  i : Integer;
begin
  count := GetMenuItemCount (fMenuHandle);
  while count > 0 do
  begin
    DeleteMenu (fMenuHandle, 0, MF_BYPOSITION);
    Dec (count)
  end;

  count := GetMenuItemCount (fWindowMenuHandle);

  for i := 0 to count - 1 do
  begin
    FillChar (item, sizeof (item), 0);

    if Win32MajorVersion > 4 then       // Ie Win2K or '98
    begin
      item.cbSize := sizeof (item);
      item.fMask := MIIM_STATE or MIIM_BITMAP or MIIM_ID or MIIM_STRING or MIIM_FTYPE;
    end
    else
    begin
      item.cbSize := 44;                // Sizeof old-style MENUITEMINFO
      item.fMask := MIIM_STATE or MIIM_ID or MIIM_TYPE;
    end;
    item.cch := sizeof (buffer);
    item.dwTypeData := buffer;

    if GetMenuItemInfo (fWindowMenuHandle, i, True, item) then
    begin
      case item.wID of
        SC_RESTORE  : item.wID := scxRestore;
        SC_MINIMIZE : item.wID := scxMinimize;
        SC_MAXIMIZE : item.wID := scxMaximize;
        SC_MOVE     : item.wID := scxMove;
        SC_SIZE     : item.wID := scxSize;
      end;
      InsertMenuItem (fMenuHandle, i, True, item)
    end
    else
      RaiseLastOSError
  end
end;

destructor TStandardSystemMenu.Destroy;
begin
  if Assigned (fObjectInstance) then
    System.Classes.FreeObjectInstance (fObjectInstance);

  if Assigned (fObjectInstance) then
    System.Classes.FreeObjectInstance (fSysObjectInstance);

  inherited;
end;

function TStandardSystemMenu.HookProc(var Msg: TMessage): boolean;
var
  LocalFlags: word;
begin
  Result := false;
  if Msg.Msg = WM_WindowPosChanging then
  begin
    with TWMWindowPosMsg(Msg).WindowPos^do
    begin
      if (hWnd = Application.Handle) and
         not IsIconic(hWnd)          and
         (cx > 0) and (cy > 0)       then
      begin
        LocalFlags := flags or SWP_NoZOrder;
        if TForm (Owner).BorderStyle = bsSizeable then
          LocalFlags := LocalFlags and not SWP_NoSize
        else
          LocalFlags := LocalFlags or SWP_NoSize;
        SetWindowPos(TForm (Owner).Handle, 0, x, y, cx, cy, LocalFlags);
        TForm (Owner).Invalidate
      end
    end
  end
end;

procedure TStandardSystemMenu.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    fMenuHandle := GetSystemMenu (Application.Handle, False);
    fWindowMenuHandle := GetSystemMenu ((Owner as TForm).Handle, False);
    CloneSystemMenu;

    fObjectInstance := System.Classes.MakeObjectInstance (OwnerWindowProc);
    fOldOwnerWindowProc := TfnWndProc (SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (fObjectInstance)));

    fSysObjectInstance := System.Classes.MakeObjectInstance (SysOwnerWindowProc);
    fOldSysWindowProc := TfnWndProc (SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (fSysObjectInstance)));

    Application.HookMainWindow(HookProc);
  end
end;

procedure TStandardSystemMenu.OnMaximized;
begin
  fIconic := False;
  fMaximized := True;
  SetItemState (scxMinimize, MFS_ENABLED);
  SetItemState (scxMaximize, MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxMove,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxSize,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxRestore,  MFS_ENABLED);
end;

procedure TStandardSystemMenu.OnMinimized;
begin
  fIconic := True;
  SetItemState (scxMinimize, MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxMaximize, MFS_ENABLED);
  SetItemState (scxMove,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxSize,     MFS_DISABLED or MFS_GRAYED);
  SetItemState (scxRestore,  MFS_ENABLED);
end;

procedure TStandardSystemMenu.OnRestored (resetmax : boolean);
begin
  fIconic := False;
  if resetmax then fMaximized := False;
  if fMaximized then
    OnMaximized
  else
  begin
    SetItemState (scxMinimize, MFS_ENABLED);
    SetItemState (scxMaximize, MFS_ENABLED);
    SetItemState (scxMove,     MFS_ENABLED);
    SetItemState (scxSize,     MFS_ENABLED);
    SetItemState (scxRestore,  MFS_DISABLED or MFS_GRAYED)
  end
end;

procedure TStandardSystemMenu.OwnerWindowProc (var msg : TMessage);
begin
  with msg do
  begin
    if msg = WM_SIZE then
    begin
      case wParam of
        SIZE_MAXIMIZED : OnMaximized;
        SIZE_MINIMIZED : OnMinimized;
        SIZE_RESTORED  : OnRestored (true)
      end
    end
    else
      if msg = WM_DESTROY then
      begin
        SetWindowLong (TForm (Owner).Handle, GWL_WNDPROC, Integer (fOldOwnerWindowProc));
        SetWindowLong (Application.Handle, GWL_WNDPROC, Integer (fOldSysWindowProc));
        Application.UnHookMainWindow(HookProc);
      end;
    result := CallWindowProc (fOldOwnerWindowProc, TForm (Owner).Handle, msg, wParam, lParam)
  end
end;

procedure TStandardSystemMenu.SetItemState(itemID, state: Integer);
var
  item : TMenuItemInfo;
begin
  FillChar (item, SizeOf (item), 0);
  item.cbSize := 44;
  item.fMask := MIIM_STATE;
  if GetMenuItemInfo (fMenuHandle, itemID, False, item) then
  begin
    item.fState := state;
    SetMenuItemInfo (fMenuHandle, itemID, False, item)
  end
end;

procedure TStandardSystemMenu.SysOwnerWindowProc(var msg: TMessage);
var
  m : Integer;
begin
  with msg do
  begin
    if msg = WM_SYSCOMMAND then
    begin
      m := -1;
      case wParam of
        scxRestore  : m := SC_RESTORE;
        scxMinimize : m := SC_MINIMIZE;
        scxMaximize : if fMaximized then  // It's also minimized, but it *was* maximized so restore!
                        SendMessage (Application.Handle, WM_SYSCOMMAND, SC_RESTORE, lParam)
                      else
                      begin
                        if fIconic then
                          SendMessage (Application.Handle, WM_SYSCOMMAND, SC_RESTORE, lParam);
                        SendMessage (TForm (owner).Handle, WM_SYSCOMMAND, SC_MAXIMIZE, lParam);
                      end;

        scxMove     : m := SC_MOVE;
        scxSize     : m := SC_SIZE;
      end;

      if m <> -1 then
        if fIconic then
          SendMessage (Application.Handle, WM_SYSCOMMAND, m, lParam)
        else
          SendMessage (TForm (owner).Handle, WM_SYSCOMMAND, m, lParam);
    end
    else
      if msg = WM_SIZE then
        case wParam of
          SIZE_MAXIMIZED : OnMaximized;
          SIZE_MINIMIZED : OnMinimized;
          SIZE_RESTORED  : OnRestored (false)
        end;

    result := CallWindowProc (fOldSysWindowProc, Application.Handle, msg, wParam, lParam);
  end
end;

end.
