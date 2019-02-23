(*======================================================================*
 | unit TextResourceForm                                                |
 |                                                                      |
 | Display / edit text(string and message) resources                   |
 |                                                                      |
 | Beware - string table ids must be numeric, consecutive, etc.         |
 |                                                                      |
 | ** Gold code - 24/4/2001 **                                          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      31/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit TextResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, StdCtrls, Contnrs, ComCtrls, ResourceForm, VirtualTrees,
  unitResourceMessages, cmpCWRichEdit, System.Actions;

type
  TfmTextResource = class(TfmResource)
    ActionList: TActionList;
    ActionStringsAdd: TAction;
    ActionStringsChangeID: TAction;
    ActionStringsDelete: TAction;
    ActionStringsModify: TAction;
    MenuItemStrings: TMenuItem;
    MainMenuStrings: TMainMenu;
    MenuItemAddString1: TMenuItem;
    MenuItemAddString2: TMenuItem;
    MenuItemChangeID1: TMenuItem;
    MenuItemChangeID2: TMenuItem;
    MenuItemDeleteString1: TMenuItem;
    MenuItemDeleteString2: TMenuItem;
    MenuItemModifyString1: TMenuItem;
    MenuItemModifyString2: TMenuItem;
    PopupMenuStrings: TPopupMenu;
    RichEditMessage: TExRichEdit;
    vstStrings: TVirtualStringTree;
    procedure vstStringsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure FormShow(Sender: TObject);
    procedure vstStringsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstStringsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vstStringsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vstStringsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ActionStringsModifyExecute(Sender: TObject);
    procedure RichEditMessageExit(Sender: TObject);
    procedure ActionStringsDeleteExecute(Sender: TObject);
    procedure ActionStringsAddExecute(Sender: TObject);
    procedure ActionStringsChangeIDExecute(Sender: TObject);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem: TMenuItem; override;
    procedure UpdateActions; override;
  private
    FDetails: TTextResourceDetails;
    FAdding: Boolean;
    FChangeId: Boolean;
    FHexMode: Boolean;
    FIsStrings: Boolean;
    FWorkStrings: TObjectList;
    procedure SaveResource (const undoDetails: string);
    function SelectedString: TStringInfo;
    function NodeString(node: PVirtualNode): TStringInfo;
    procedure UpdateDisplay (selectedString: TStringInfo);
    function NodeN (n: Integer): PVirtualNode;
  public
    procedure TidyUp; override;
    procedure UpdateFonts; override;
  end;


implementation

uses
  DialogStrings;

{$R *.DFM}

resourcestring
  rstChangeString  = 'change string';
  rstDeleteString  = 'delete string';
  rstChangeMessage = 'change message';
  rstDeleteMessage = 'delete message';
  rstAddString     = 'add string';
  rstAddMessage    = 'add message';
  rstChangeID      = 'change message ID';
  rstDuplicateMessageID = 'Duplicate Message ID';

{ TfmTextResource }

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsAddExecute                                 |
 |                                                                      |
 | Add a new string.  Just create a new item in the list view, then     |
 | modify it.                                                           |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.ActionStringsAddExecute(Sender: TObject);
var
  newID: Integer;
  si: TStringInfo;
begin
  if (FWorkStrings.Count < 16) or not FIsStrings then
  begin
    if FWorkStrings.Count = 0 then
      if FIsStrings then
        newID := StrToIntDef(ResIdToStringsId(FDetails.ResourceName), 1)
      else
        newID := 1
    else
      newId := TStringInfo (FWorkStrings [FWorkStrings.Count - 1]).Id + 1;

    si := TStringInfo.Create('', newId);
    FWorkStrings.Add(si);
    UpdateDisplay (si);
    ActionStringsModifyExecute (nil);
  end
end;

procedure TfmTextResource.ActionStringsChangeIDExecute(Sender: TObject);
begin
  if Assigned(vstStrings.FocusedNode) then
    if not FIsStrings then
      vstStrings.EditNode(vstStrings.FocusedNode, 0)
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsDeleteExecute                              |
 |                                                                      |
 | Delete the selected string                                           |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.ActionStringsDeleteExecute(Sender: TObject);
var
  idx: Integer;
  p: PVirtualNode;
begin
  if Assigned(vstStrings.FocusedNode) then
  begin
    idx := vstStrings.FocusedNode^.Index;
    FWorkStrings.Delete(idx);
    UpdateDisplay (Nil);

    while idx >= FWorkStrings.Count do
      Dec(idx);

    p := NodeN (idx);
    if Assigned(p) then
      vstStrings.Selected [p] := True;

    if FDetails is TMessageResourceDetails then
      SaveResource (rstDeleteMessage)
    else
      SaveResource (rstDeleteString)
  end
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.actStringsModifyExecute                              |
 |                                                                      |
 | Allow the user to edit a string by unhiding and repositioning the    |
 | hidden memo                                                          |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.ActionStringsModifyExecute(Sender: TObject);
var
  r: TRect;
begin
  if Assigned(vstStrings.FocusedNode) then
  begin
    RichEditMessage.Width := vstStrings.Width - 2;
    r := vstStrings.GetDisplayRect(vstStrings.FocusedNode, 1, False);
    RichEditMessage.Left := r.Left + 2;
    RichEditMessage.Width := r.Right - r.Left + 2;
    RichEditMessage.Top := r.Bottom;
    if RichEditMessage.Top + RichEditMessage.Height > vstStrings.Top + vstStrings.Height then
      RichEditMessage.Top := r.Top - RichEditMessage.Height;
    RichEditMessage.Visible := True;
    RichEditMessage.Text := SelectedString.St;
    RichEditMessage.SetFocus
  end
  else
    ActionStringsAdd.Execute;
end;

procedure TfmTextResource.FormCreate(Sender: TObject);
begin
  inherited;

  FWorkStrings := TObjectList.Create;
end;

procedure TfmTextResource.FormDestroy(Sender: TObject);
begin
  FWorkStrings.Free;
  inherited;
end;

procedure TfmTextResource.FormShow(Sender: TObject);
begin
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.GetMenuItem                                          |
 |                                                                      |
 | Override GetMenuItem to return our menu to the resource editor       |
 | framework                                                            |
 *----------------------------------------------------------------------*)
function TfmTextResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemStrings;
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.RichEditMessageExit                                       |
 |                                                                      |
 | Finished editing the string.  Hide the memo, and update the resource |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.RichEditMessageExit(Sender: TObject);
var
  st: string;
  adding: Boolean;
  si: TStringInfo;
  ws: string;
begin
  adding := FAdding;
  FAdding := False;
  RichEditMessage.Visible := False;

  si := SelectedString;

  if RichEditMessage.CanUndo or adding then  // ie.  If it's changed
  begin
    ws := RichEditMessage.Text;
    si.St := ws;

    if adding then
      if FDetails is TMessageResourceDetails then
        st := rstAddmessage
      else
        st := rstAddString
    else
      if FDetails is TMessageResourceDetails then
        st := rstChangeMessage
      else
        st := rstChangeString;

    SaveResource(st);
    RichEditMessage.ClearUndoBuffer;
  end;
end;

function TfmTextResource.NodeN(n: Integer): PVirtualNode;
begin
  if n >= 0 then
  begin
    Result := vstStrings.GetFirst;
    while (Result <> Nil) and (n > 0) do
    begin
      Result := vstStrings.GetNextSibling(Result);
      Dec(n);
    end;
  end
  else
    Result := nil;
end;

function TfmTextResource.NodeString(node: PVirtualNode): TStringInfo;
begin
  if Assigned(node) and (Integer(node^.Index) < FWorkStrings.Count) then
    Result := TStringInfo (FWorkStrings [node^.Index])
  else
    Result := nil;
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.SaveResource                                         |
 |                                                                      |
 | Update the resource from the list box strings.                      |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.SaveResource(const undoDetails: string);
var
  i: Integer;
  si: TStringInfo;
begin
  AddUndoEntry (undoDetails);
  FDetails.BeginUpdate;
  try
    for i := 0 to FWorkStrings.Count - 1 do
    begin
      si := TStringInfo(FWorkStrings [i]);

      FDetails.Strings [i] := si.St;
      FDetails.Ids [i] := si.Id
    end;

    i := FWorkStrings.Count;
    while i < FDetails.Count do
      FDetails.Delete(i);
  finally
    FDetails.EndUpdate
  end
end;

function TfmTextResource.SelectedString: TStringInfo;
begin
  Result := NodeString(vstStrings.FocusedNode);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.SetObject                                            |
 |                                                                      |
 | Initialize the form with data from the object                        |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.SetObject(const Value: TObject);
var
  i: Integer;
begin
  inherited; // *Must* call inherited

  FDetails := ResourceDetails as TTextResourceDetails;

  FIsStrings := not(FDetails is TMessageResourceDetails);
  FHexMode := not FIsStrings;

  FWorkStrings.Clear;
  for i := 0 to FDetails.Count - 1 do
    FWorkStrings.Add(TStringInfo.Create(FDetails [i], FDetails.Ids [i]));

  UpdateDisplay(nil);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.UpdateActions                                        |
 |                                                                      |
 | Disallow modifying/deleting if nothing's selected                    |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.UpdateActions;
var
  sel: Boolean;
begin
  sel := Assigned(vstStrings.FocusedNode);

  ActionStringsAdd.Enabled := FDetails is TMessageResourceDetails;
  ActionStringsModify.Enabled := sel;
  ActionStringsDelete.Enabled := sel;
  ActionStringsChangeID.Enabled := sel and not FIsStrings;

  if FChangeId then
  begin
    FChangeId := False;
    ActionStringsChangeId.Execute;
  end;
end;

procedure TfmTextResource.UpdateDisplay(selectedString: TStringInfo);
var
  p: PVirtualNode;
  i: Integer;
begin
  vstStrings.BeginUpdate;
  try
    if FWorkStrings.Count <> Integer(vstStrings.RootNodeCount) then
      vstStrings.RootNodeCount := FWorkStrings.Count
    else
      vstStrings.ReinitNode(nil, True);
  finally
    vstStrings.EndUpdate;
  end;

  vstStrings.ClearSelection;
  if Assigned(selectedString) then
  begin
    i := FWorkStrings.IndexOf (selectedString);
    p := NodeN (i);
    if p <> Nil then
    begin
      vstStrings.Selected [p] := True;
      vstStrings.FocusedNode := p;
    end;
  end;
end;

procedure TfmTextResource.UpdateFonts;
begin
  UseInternationalFont(RichEditMessage.Font);
  UseInternationalFont(vstStrings.Font);
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.vstStringsDblClick                                    |
 |                                                                      |
 | Respond to double click.  Modify (or add) a string                   |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.vstStringsDblClick(Sender: TObject);
var
  p: TPoint;
begin
  if Assigned(vstStrings.FocusedNode) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (0, vstStrings.Handle, p, 1);
    if p.X <= vstStrings.Header.Columns [0].Width then
      FChangeId := True
    else
      ActionStringsModify.Execute;
  end
  else
    ActionStringsAdd.Execute;
end;

procedure TfmTextResource.vstStringsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  si: TStringInfo;
begin
  si := NodeString(Node);

  if si <> Nil then
  case Column of
    0:
      if FHexMode then
        CellText := IntToHex(si.Id , 8)
      else
        CellText := IntToStr(si.Id);
    1:
      CellText := si.St;
  end;
end;

(*----------------------------------------------------------------------*
 | TfmTextResource.vstStringsKeyDown                                     |
 |                                                                      |
 | Respond to the 'Return' key - Modify the string if it's pressed.     |
 *----------------------------------------------------------------------*)
procedure TfmTextResource.vstStringsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ActionStringsModify.Execute;
end;

procedure TfmTextResource.vstStringsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  i, newID: Integer;
  st: TStringInfo;
begin
  if Column <> 0 then
    Exit;

  st := NodeString(node);
  if not Assigned(st) then
    Exit;

  try
    if FHexMode then
      newId := StrToInt('$' + NewText)
    else
      newID := StrToInt(NewText);

    if st.Id <> newID then
    begin
      for i := 0 to FDetails.Count - 1 do
        if FDetails.Ids [i] = newID then
          raise Exception.Create(rstDuplicateMessageID);

      st.Id := NewId;
      SaveResource (rstChangeID)
    end;
  except
    raise;
  end
end;

procedure TfmTextResource.vstStringsEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := (Column = 0) and (NodeString(Node) <> nil);
end;

procedure TfmTextResource.TidyUp;
begin
 if RichEditMessage.Visible then
   RichEditMessageExit(nil);
end;

end.
