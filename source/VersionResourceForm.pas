(*======================================================================*
 | VersionResourceForm                                                  |
 |                                                                      |
 | Display/Edit version resources                                       |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 *======================================================================*)

unit VersionResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, StdCtrls, ComCtrls, ExtCtrls, Menus, ActnList, Actions,
  cmpPropertyListBox, unitResourceVersionInfo, cmpCWRichEdit;

//=======================================================================
// TfmResource class

type
  TfmVersionResource = class(TfmResource)
    ActionList: TActionList;
    ActionStringAddString: TAction;
    ActionStringDeleteString: TAction;
    ActionStringModifyString: TAction;
    ActionStringModifyStringName: TAction;
    ListViewVersionStrings: TListView;
    MainMenu: TMainMenu;
    MenuItemAddString1: TMenuItem;
    MenuItemAddString2: TMenuItem;
    MenuItemDeleteString1: TMenuItem;
    MenuItemDeleteString2: TMenuItem;
    MenuItemModifyString1: TMenuItem;
    MenuItemModifyString2: TMenuItem;
    MenuItemModifyStringName2: TMenuItem;
    MenuItemStringModifyStringName1: TMenuItem;
    MenuItemStrings: TMenuItem;
    Panel: TPanel;
    PopupMenu: TPopupMenu;
    PropertyListBox: TPropertyListBox;
    RichEditMessage: TExRichEdit;
    Splitter: TSplitter;
    procedure FormResize(Sender: TObject);
    procedure ActionStringDeleteStringExecute(Sender: TObject);
    procedure ActionStringModifyStringExecute(Sender: TObject);
    procedure RichEditMessageExit(Sender: TObject);
    procedure ListViewVersionStringsEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure ActionStringModifyStringNameExecute(Sender: TObject);
    procedure ListViewVersionStringsDblClick(Sender: TObject);
    procedure ActionStringAddStringExecute(Sender: TObject);
    procedure PropertyListBoxPropertyChanged(Sender: TObject);
  private
    FInitializing: Boolean;
    FSelectedItem: TListItem;
    FAdding: Boolean;
    FDetails: TVersionInfoResourceDetails;
    procedure SaveFlags;
    function GetNewStringName: string;
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem: TMenuItem; override;
    procedure UpdateActions; override;
  end;

implementation

uses
  DialogStrings;

{$R *.DFM}

//=======================================================================
// Translatable strings

resourcestring
  rstVersionFormatError   = 'Error in version string';
  rstChangeFlags          = 'version flags change';
  rstChangeProductVersion = 'product version change';
  rstChangeFileVersion    = 'file version change';
  rstDeleteString         = 'delete version string';
  rstChangeString         = 'modify version string';
  rstChangeStringName     = 'modify version string name';
  rstAddString            = 'add version string';
  rstNewString            = 'New String %d';

//=======================================================================
// Property constants

const
  prProductVersion = 0;
  prFileVersion = 1;
  prDebug = 2;
  prInferred = 3;
  prPatched = 4;
  prPreRelease = 5;
  prPrivateBuild = 6;
  prSpecialBuild = 7;

(*----------------------------------------------------------------------------*
 | function VersionToString                                                   |
 |                                                                            |
 | Convert a version large integer to a string                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version: TULargeInteger     The version integer to convert              |
 |                                                                            |
 | The function returns string representation of the version no               |
 *----------------------------------------------------------------------------*)
function VersionToString(version: TULargeInteger): string;
begin
  with _ULARGE_INTEGER(version) do
    Result := Format('%d.%d.%d.%d', [HiWord (HighPart), LoWord (HighPart), HiWord (LowPart), LoWord (LowPart)]);
end;

(*----------------------------------------------------------------------------*
 | function StringToVersion ()                                                |
 |                                                                            |
 | Convert a version string to a large integer                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version: string       The version string to convert                     |
 |                                                                            |
 | The function returns the integer representation of the version string      |
 *----------------------------------------------------------------------------*)
function StringToVersion(const version: string): TULargeInteger;
var
  p: Integer;
  s: string;
  hh, h, l, ll: Word;
  ok: Boolean;
begin
  hh := 0;
  ll := 0;
  h := 0;
  l := 0;

  s := version;
  p := Pos ('.', s);
  ok := False;
  if p > 0 then
  begin
    hh := StrToInt(Copy (s, 1, p - 1));
    s := Copy (s, p + 1, MaxInt);
    p := Pos ('.', s);
    if p > 0 then
    begin
      h := StrToInt(Copy (s, 1, p - 1));
      s := Copy (s, p + 1, MaxInt);
      p := Pos ('.', s);
      if p > 0 then
      begin
        l := StrToInt(Copy (s, 1, p - 1));
        ll := StrToInt(Copy (s, p + 1, MaxInt));
        ok := True;
      end
    end
  end;

  if not ok then
    raise Exception.Create(rstVersionFormatError);

  _ULARGE_INTEGER(Result).HighPart := 65536 * hh + h;
  _ULARGE_INTEGER(Result).LowPart := 65536 * l + ll;
end;


{ TfmVersionResource }

(*----------------------------------------------------------------------*
 | TfmVersionResource.GetMenuItem                                       |
 |                                                                      |
 | Return out forms menu item to the framework.                         |
 *----------------------------------------------------------------------*)
function TfmVersionResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemStrings;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.SaveFlags                                         |
 |                                                                      |
 | Save the version Flags, product version and file version if they've  |
 | changed.                                                             |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.SaveFlags;
var
  Flags: TVersionFileFlags;
  v: TULargeInteger;
begin
  if not FInitializing then     // Ignore check box 'OnClick' handlers
                                // when we're being initialized.

    with FDetails do
    begin
      Flags := FileFlags;
      if PropertyListBox.Properties [prDebug].PropertyValue then Flags := Flags + [ffDebug] else Flags := Flags - [ffDebug];
      if PropertyListBox.Properties [prInferred].PropertyValue then Flags := Flags + [ffInfoInferred] else Flags := Flags - [ffInfoInferred];
      if PropertyListBox.Properties [prPatched].PropertyValue then Flags := Flags + [ffPatched] else Flags := Flags - [ffPatched];
      if PropertyListBox.Properties [prPreRelease].PropertyValue then Flags := Flags + [ffPreRelease] else Flags := Flags - [ffPreRelease];
      if PropertyListBox.Properties [prPrivateBuild].PropertyValue then Flags := Flags + [ffPrivateBuild] else Flags := Flags - [ffPrivateBuild];
      if PropertyListBox.Properties [prSpecialBuild].PropertyValue then Flags := Flags + [ffSpecialBuild] else Flags := Flags - [ffSpecialBuild];

      if Flags <> FileFlags then        // Has a flag changed ?
      begin
        AddUndoEntry (rstChangeFlags);
        FileFlags := Flags
      end;

      v := StringToVersion (PropertyListBox.Properties [prProductVersion].PropertyValue);
                                        // Has the product version changed ??
      if _ULARGE_INTEGER(v).QuadPart <> _ULARGE_INTEGER(ProductVersion).QuadPart then
      begin
        AddUndoEntry (rstChangeProductVersion);
        ProductVersion := v
      end;

      v := StringToVersion (PropertyListBox.Properties [prFileVersion].PropertyValue);
                                        // Has the file version changed ??
      if _ULARGE_INTEGER(v).QuadPart <> _ULARGE_INTEGER(FileVersion).QuadPart then
      begin
        AddUndoentry (rstChangeFileVersion);
        FileVersion := v
      end
    end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.SetObject                                         |
 |                                                                      |
 | Overriden 'Set' method for ancestor Obj property.  The 'Obj' must be |
 | a TVersionInfoResourceDetails object.                                |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.SetObject(const Value: TObject);
var
  FilVersion: TULargeInteger;
  ProdVersion: TULargeInteger;
  Flags: TVersionFileFlags;
  k: TVersionStringValue;
  i: Integer;
begin
  inherited;
  FDetails := ResourceDetails as TVersionInfoResourceDetails;
  FInitializing := True;
  with FDetails do
  try
    FilVersion := FileVersion;
    ProdVersion := ProductVersion;
    Flags := FileFlags;

    // Initialize the form

    PropertyListBox.Properties [prFileVersion].PropertyValue := VersionToString (FilVersion);
    PropertyListBox.Properties [prProductVersion].PropertyValue := VersionToString (ProdVersion);

    PropertyListBox.Properties [prDebug].PropertyValue := ffDebug in Flags;
    PropertyListBox.Properties [prInferred].PropertyValue := ffInfoInferred in Flags;
    PropertyListBox.Properties [prPatched].PropertyValue := ffPatched in Flags;
    PropertyListBox.Properties [prPreRelease].PropertyValue := ffPreRelease in Flags;
    PropertyListBox.Properties [prPrivateBuild].PropertyValue := ffPrivateBuild in Flags;
    PropertyListBox.Properties [prSpecialBuild].PropertyValue := ffSpecialBuild in Flags;

    ListViewVersionStrings.Items.BeginUpdate;
    with ListViewVersionStrings.Items do
    try
      Clear;
      for i := 0 to KeyCount - 1 do
        with Add do
        begin
          k := Key [i];
          Caption := k.KeyName;
          SubItems.Add (StringToCString (k.Value));
        end;
      if Count > 0 then
        ListViewVersionStrings.ItemIndex := 0
    finally
      EndUpdate
    end
  finally
    FInitializing := False
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringDeleteStringExecute                      |
 |                                                                      |
 | Delete the selected version string                                   |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ActionStringDeleteStringExecute(Sender: TObject);
var
  n: Integer;
begin
  if Assigned(ListViewVersionStrings.Selected) then
  begin
    AddUndoEntry (rstDeleteString);
    n := ListViewVersionStrings.Selected.Index;

    // Delete the string from the resouce
    FDetails.DeleteKey (n);

    // Delete the string from the list view
    ListViewVersionStrings.Selected.Delete;

    // Select the next entry in the list after deleting
    if n >= ListViewVersionStrings.Items.Count then
      n := ListViewVersionStrings.Items.Count - 1;

    if n >= 0 then
      ListViewVersionStrings.Items.Item [n].Selected := True
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringModifyStringExecute                      |
 |                                                                      |
 | Start modifying the version string.  Reposition and reveal the       |
 | hidden memo.                                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ActionStringModifyStringExecute(Sender: TObject);
var
  idx: Integer;
begin
  if Assigned(ListViewVersionStrings.Selected) then
  begin
    FSelectedItem := ListViewVersionStrings.Selected;
    idx := FDetails.IndexOf(FSelectedItem.Caption);
    if idx > -1 then
    begin
      RichEditMessage.Width := ListViewVersionStrings.Width - 2;
      RichEditMessage.Top := ListViewVersionStrings.Selected.DisplayRect(drLabel).Bottom + 1;
      RichEditMessage.Left := ListViewVersionStrings.Left + 1;
      RichEditMessage.Text := FDetails.Key [idx].Value;
      RichEditMessage.Visible := True;
      RichEditMessage.Enabled := True;
      RichEditMessage.SetFocus
    end
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.mmoMessageExit                                    |
 |                                                                      |
 | They'v existed the memo.  Re-coneal it, and change the modified      |
 | string valur                                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.RichEditMessageExit(Sender: TObject);
begin
  RichEditMessage.Enabled := False;
  RichEditMessage.Visible := False;
  if RichEditMessage.CanUndo or FAdding then
  begin
    if FAdding then
      AddUndoEntry(rstAddString)
    else
      AddUndoEntry(rstChangeString);

    // Update the resource
    FDetails.SetKeyValue(FSelectedItem.Caption, RichEditMessage.Text);

    // Update the list view
    FSelectedItem.SubItems[0] := StringToCString (RichEditMessage.Text)
  end;
  FAdding := False;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.lvVersionStringsEdited                            |
 |                                                                      |
 | They've finished editing the key name.  Update the resource          |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ListViewVersionStringsEdited(Sender: TObject;
  Item: TListItem; var S: String);
begin
  if s <> Item.Caption then
  begin
    AddUndoEntry (rstChangeStringName);
    FDetails.ChangeKey (Item.Caption, s)
  end
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringModifyStringNameExecute                  |
 |                                                                      |
 | Start modifying the key name                                         |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ActionStringModifyStringNameExecute(
  Sender: TObject);
begin
  if Assigned(ListViewVersionStrings.Selected) then
    ListViewVersionStrings.Selected.EditCaption;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.lvVersionStringsDblClick                          |
 |                                                                      |
 | Check where abouts on the list view is being clicked.                |
 |                                                                      |
 | If a selected item is clicked in the 'Key Name' column, start        |
 | editing the key name.                                                |
 |                                                                      |
 | If a selected item is clicked in the 'String' column, start editing  |
 | the string value.                                                    |
 |                                                                      |
 | If no item was selcted, add a new string.                            |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ListViewVersionStringsDblClick(Sender: TObject);
var
  p: TPoint;
begin
  if Assigned(ListViewVersionStrings.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (HWND_DESKTOP, ListViewVersionStrings.Handle, p, 1);

    if p.x > ListViewVersionStrings.Columns [0].Width then
      ActionStringModifyString.Execute
    else
      ActionStringModifyStringName.Execute
  end
  else
    ActionStringAddString.Execute;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.UpdateActions                                     |
 |                                                                      |
 | Enable or disable the 'Strings' menu items appropriately             |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.UpdateActions;
var
  sel: Boolean;
begin
  sel := Assigned(ListViewVersionStrings.Selected) and ListViewVersionStrings.Focused;
  ActionStringDeleteString.Enabled := sel;
  ActionStringModifyString.Enabled := sel;
  ActionStringModifyStringName.Enabled := sel;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.actStringAddStringExecute                         |
 |                                                                      |
 | Add a new string.  Simply add it to the list view, then call Modify  |
 *----------------------------------------------------------------------*)
procedure TfmVersionResource.ActionStringAddStringExecute(Sender: TObject);
var
  keyName: string;
begin
  keyName := GetNewStringName;
  with ListViewVersionStrings.Items.Add do
  begin
    Caption := keyName;
    SubItems.Add ('');
    Selected := True;
    FAdding := True;
    ActionStringModifyString.Enabled := True;
    ActionStringModifyString.Execute;
  end;
end;

(*----------------------------------------------------------------------*
 | TfmVersionResource.GetNewStringName                                  |
 |                                                                      |
 | Calculate the default name for new string values.                    |
 *----------------------------------------------------------------------*)
function TfmVersionResource.GetNewStringName: string;
var
  m: Integer;
begin
  m := 1;
  repeat
    Result := Format(rstNewString, [m]);

    if ListViewVersionStrings.FindCaption (0, Result, False, True, False) = nil then
      break;

    Inc(m);
  until m = 0;
end;

procedure TfmVersionResource.PropertyListBoxPropertyChanged(
  Sender: TObject);
begin
  SaveFlags;
end;

procedure TfmVersionResource.FormResize(Sender: TObject);
begin
  inherited;

  if RichEditMessage.Visible then
    RichEditMessage.Width := ListViewVersionStrings.Width - 2;
end;

end.
