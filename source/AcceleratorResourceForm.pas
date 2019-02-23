unit AcceleratorResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Menus, Graphics, Controls, Forms,
  Dialogs, ResourceForm, ComCtrls, unitResourceAccelerator, StdCtrls, ActnList,
  System.Actions;

type
  TfmAcceleratorResource = class(TfmResource)
    ActionAccelAdd: TAction;
    ActionAccelChangeFlags: TAction;
    ActionAccelChangeID: TAction;
    ActionAccelDelete: TAction;
    ActionAccelModify: TAction;
    ActionList: TActionList;
    ComboBoxKey: TComboBox;
    ComboBoxType: TComboBox;
    ListViewAccelerator: TListView;
    MainMenuAccelerator: TMainMenu;
    MenuItemAccelerators: TMenuItem;
    MenuItemAddAccelerator1: TMenuItem;
    MenuItemAddAccelerator2: TMenuItem;
    MenuItemChangeFlags1: TMenuItem;
    MenuItemChangeFlags2: TMenuItem;
    MenuItemChangeID1: TMenuItem;
    MenuItemChangeID2: TMenuItem;
    MenuItemDeleteAccelerator1: TMenuItem;
    MenuItemDeleteAccelerator2: TMenuItem;
    MenuItemModifyAccelerator1: TMenuItem;
    MenuItemModifyAccelerator2: TMenuItem;
    PopupMenuAccel: TPopupMenu;
    procedure ActionAccelDeleteExecute(Sender: TObject);
    procedure ComboBoxTypeExit(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure ActionAccelChangeFlagsExecute(Sender: TObject);
    procedure ActionAccelChangeIDExecute(Sender: TObject);
    procedure ComboBoxKeyChange(Sender: TObject);
    procedure ListViewAcceleratorEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ListViewAcceleratorDblClick(Sender: TObject);
    procedure ComboBoxKeyExit(Sender: TObject);
    procedure ActionAccelModifyExecute(Sender: TObject);
    procedure ActionAccelAddExecute(Sender: TObject);
    procedure ListViewAcceleratorEdited(Sender: TObject; Item: TListItem;
      var S: string);
  private
    FAdding: Boolean;
    FKeyChanged: Boolean;
    FDetails: TAcceleratorResourceDetails;
    FAllowEdit: Boolean;
  protected
    function GetMenuItem: TMenuItem; override;
    procedure SetObject(const Value: TObject); override;
  public
    procedure SaveResource(const undoDetails: string);
  end;

implementation

{$R *.dfm}

resourcestring
  rstAddAccelerator    = 'add accelerator';
  rstDeleteAccelerator = 'delete accelerator';
  rstChangeAccelerator = 'change accelerator';
  rstChangeAcceleratorType = 'change accelerator type';
  rstChangeID          = 'change accelerator ID';
  rstDuplicateMessageID = 'Duplicate Accelerator ID';
  rstVirtKey = 'Virtual Key';
  rstCharCode = 'Character Code';


{ TfmAcceleratorResource }

function AcceleratorToText(const Accel: TAccelerator): string;
begin
  if (Accel.Flags and FVIRTKEY) <> 0 then
    Result := ShortcutToText(Accel.Code)
  else
    if Accel.Code < 32 then
      Result := 'Ctrl+' + Char(Accel.Code + Ord ('@'))
    else
      Result := Char(Accel.Code);

  if (Accel.Flags and FSHIFT) <> 0 then
    Result := 'Shift+' + Result;

  if (Accel.Flags and FALT) <> 0 then
    Result := 'Alt+' + Result;

  if (Accel.Flags and FCONTROL) <> 0 then
    Result :='Ctrl+' + Result
end;

procedure TextToAccelerator(const st: string; virtKey: Boolean; var Code, Flags: Integer);
var
  Temp: Integer;
  Key: Word;
  Shift: TShiftState;
  Ok: Boolean;
begin
  Ok := False;
  if not virtKey then
  begin
    Temp := TextToShortcut(st);
    Key := Temp and not(scAlt or scShift or scCtrl);
    if (Key >= $30) and (Key <= $39) or
       (Key >= $41) and (Key <= $5A) then
    begin
      ShortcutToKey(Temp, Key, Shift);
      Code := Key;
      Flags := 0;
      if ssShift in Shift then
        Flags := Flags or FSHIFT;
      if ssAlt in Shift then
        Flags := Flags or FALT;
      if ssCtrl in Shift then
        Flags := Flags or FCONTROL;
        Ok := True;
    end
  end;

  if not Ok then
  begin
    Code := TextToShortcut(st);
    Flags := FVIRTKEY;
    if (Code and scAlt) <> 0 then
      Flags := Flags or FALT;
    if (Code and scShift) <> 0 then
      Flags := Flags or FSHIFT;
    if (Code and scCtrl) <> 0 then
      Flags := Flags or FCONTROL;
    Code := Code and not(scAlt or scShift or scCtrl);
  end
end;

procedure TfmAcceleratorResource.SetObject(const Value: TObject);
var
  i: Integer;
  Accel: TAccelerator;
begin
  inherited;            // *Must* call inherited
  Application.ProcessMessages;

  FDetails := Obj as TAcceleratorResourceDetails;

  ListViewAccelerator.Items.BeginUpdate;
  try
    ListViewAccelerator.Items.Clear;
    for i := 0 to FDetails.Count - 1 do
    begin
      Accel := FDetails.Accelerator [i];
      with ListViewAccelerator.Items.Add do
      begin
        Caption := IntToStr(Accel.id);
        SubItems.Add(AcceleratorToText(Accel));
        if (Accel.Flags and FVIRTKEY) <> 0 then
          SubItems.Add(rstVirtKey)
        else
          SubItems.Add(rstCharCode);
//        SubItems.Add(IntToStr(Accel.Flags))
      end
    end
  finally
    ListViewAccelerator.Items.EndUpdate;
  end
end;

procedure TfmAcceleratorResource.ListViewAcceleratorEdited(Sender: TObject;
  Item: TListItem; var S: string);
var
  newId: Integer;
  i: Integer;
begin
  inherited;
  FAllowEdit := False;
  ActionAccelDelete.ShortCut := ActionAccelDelete.Tag;    // Restore 'Delete' shortcut
  if s <> Item.Caption then
  try
    newID := StrToInt(s);

    for i := 0 to FDetails.Count - 1 do
      if FDetails.Accelerator [i].id = newid then
        raise Exception.Create(rstDuplicateMessageID);

    Item.Caption := s;          // need to do this so 'SaveResource' picks it up...
    SaveResource(rstChangeID);
  except
    s := Item.Caption;
    raise
  end
end;

function TfmAcceleratorResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemAccelerators;
end;

procedure TfmAcceleratorResource.ActionAccelAddExecute(Sender: TObject);
var
  Item: TListItem;
  newID: Integer;
begin
  Item := ListViewAccelerator.Items.Add;

  // Work out string / message ID
  if Item.Index = 0 then
    Item.Caption := '1'
  else
  begin
    newId :=StrToInt(ListViewAccelerator.Items[ListViewAccelerator.Items.Count - 2].Caption);
    Item.Caption := IntToStr(newId + 1)
  end;

  Item.SubItems.Add('');
  Item.SubItems.Add(rstVirtKey);
  Item.Selected := True;

  FAdding := True;                    // Clear in mmoMessageExit

  ActionAccelModifyExecute(Sender)
end;

procedure TfmAcceleratorResource.ActionAccelModifyExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    ComboBoxKey.Width := ListViewAccelerator.Columns [1].Width - 2;
    ComboBoxKey.Left := ListViewAccelerator.Columns [0].Width + 2;
    t := ListViewAccelerator.Selected.DisplayRect(drLabel).Top - 3;
    if t < 0 then
      t := 0;
    ComboBoxKey.Top := t;
    ComboBoxKey.Text := ListViewAccelerator.Selected.SubItems[0];
    ComboBoxKey.Visible := True;
    ComboBoxKey.SetFocus
  end
  else
    ActionAccelAdd.Execute;
end;

procedure TfmAcceleratorResource.ComboBoxKeyExit(Sender: TObject);
var
  st: string;
  adding: Boolean;
begin
  adding := FAdding;
  FAdding := False;
  ComboBoxKey.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged or FAdding then  // ie.  If it's changed
    begin
      selected.SubItems[0] := ComboBoxKey.Text;

      if adding then
        st := rstAddAccelerator
      else
        st := rstChangeAccelerator;

      SaveResource(st);
    end
  end;
  FKeyChanged := False
end;

procedure TfmAcceleratorResource.SaveResource(const undoDetails: string);
var
  i, Flags, Code, id: Integer;
  Item: TListItem;
  virtKey: Boolean;
begin
  AddUndoEntry(undoDetails);
  for i := 0 to ListViewAccelerator.Items.Count - 1 do
  begin
    Item := ListViewAccelerator.Items[i];
    id := StrToInt(Item.Caption);
    virtKey := Item.SubItems[1] = rstVirtKey;
    TextToAccelerator(Item.SubItems[0], virtKey, Code, Flags);

    if not virtKey and ((Flags and FVIRTKEY) <> 0) then
      Item.SubItems[1] := rstVirtKey;

    if i < FDetails.Count then
      FDetails.SetAccelDetails(i, Flags, Code, id)
    else
      FDetails.Add(Flags, Code, id)
  end;

  i := ListViewAccelerator.Items.Count;
  while i < FDetails.Count do
    FDetails.Delete(i);
end;

procedure TfmAcceleratorResource.ListViewAcceleratorDblClick(Sender: TObject);
var
  p: TPoint;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints(HWND_DESKTOP, ListViewAccelerator.Handle, p, 1);

    if p.x > ListViewAccelerator.Columns [0].Width + ListViewAccelerator.Columns [1].Width then
      ActionAccelChangeFlags.Execute
    else
      if p.x > ListViewAccelerator.Columns [0].Width then
        ActionAccelModify.Execute
      else
        ActionAccelChangeID.Execute
  end
  else
    ActionAccelAdd.Execute
end;

procedure TfmAcceleratorResource.FormCreate(Sender: TObject);

  procedure AddShortcutRange(const st: string; lowRange, highRange: char);
  var
    ch: char;
  begin
    for ch := lowRange to highRange do
      ComboBoxKey.Items.Add(st + ch)
  end;

begin
  inherited;

  ComboBoxKey.Items.BeginUpdate;
  try
    AddShortcutRange('Ctrl+', 'A', 'Z');
    AddShortcutRange('Ctrl+Alt+', 'A', 'Z');
    AddShortcutRange('F', '1', '9');
    AddShortcutRange('F1', '0', '2');
    AddShortcutRange('Ctrl+F', '1', '9');
    AddShortcutRange('Ctrl+F1', '0', '2');
    AddShortcutRange('Shift+F', '1', '9');
    AddShortcutRange('Shift+F1', '0', '2');
    AddShortcutRange('Shift+Ctrl+F', '1', '9');
    AddShortcutRange('Shift+Ctrl+F1', '0', '2');
    AddShortcutRange('In', 's', 's');
    AddShortcutRange('Shift+In', 's', 's');
    AddShortcutRange('Ctrl+In', 's', 's');
    AddShortcutRange('De', 'l', 'l');
    AddShortcutRange('Shift+De', 'l', 'l');
    AddShortcutRange('Ctrl+De', 'l', 'l');
    AddShortcutRange('Alt+BkS', 'p', 'p');
    AddShortcutRange('Shift+Alt+BkS', 'p', 'p');
  finally
    ComboBoxKey.Items.EndUpdate
  end;

  ComboBoxType.Items.Add(rstVirtKey);
  ComboBoxType.Items.Add(rstCharCode);
end;

procedure TfmAcceleratorResource.ListViewAcceleratorEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := FAllowEdit;
  ActionAccelDelete.Tag := ActionAccelDelete.ShortCut;    // Suspend 'Delete' shortcut.
  ActionAccelDelete.ShortCut := 0;
end;

procedure TfmAcceleratorResource.ComboBoxKeyChange(Sender: TObject);
begin
  inherited;
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.ActionAccelChangeIDExecute(Sender: TObject);
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    FAllowEdit := True;
    ListViewAccelerator.Selected.EditCaption;
  end;
end;

procedure TfmAcceleratorResource.ActionAccelChangeFlagsExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    ComboBoxType.Width := ListViewAccelerator.Columns [2].Width - 2;
    ComboBoxType.Left := ListViewAccelerator.Columns [0].Width + ListViewAccelerator.Columns [1].Width + 2;
    t := ListViewAccelerator.Selected.DisplayRect(drLabel).Top - 3;
    if t < 0 then
      t := 0;
    ComboBoxType.Top := t;
    ComboBoxType.Text := ListViewAccelerator.Selected.SubItems[1];
    ComboBoxType.Visible := True;
    ComboBoxType.SetFocus;
  end;
end;

procedure TfmAcceleratorResource.ComboBoxTypeChange(Sender: TObject);
begin
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.ComboBoxTypeExit(Sender: TObject);
begin
  ComboBoxType.Visible := False;
  with ListViewAccelerator do if Assigned(Selected) then
  begin
    if FKeyChanged then  // ie.  If it's changed
    begin
      selected.SubItems[1] := ComboBoxType.Text;

      SaveResource(rstChangeAcceleratorType);
    end;
  end;
  FKeyChanged := False;
end;

procedure TfmAcceleratorResource.ActionAccelDeleteExecute(Sender: TObject);
var
  idx: Integer;
begin
  if Assigned(ListViewAccelerator.Selected) then
  begin
    idx := ListViewAccelerator.Selected.Index;
    ListViewAccelerator.Selected.Free;

                                // Select next string
    if idx < ListViewAccelerator.Items.Count then
      ListViewAccelerator.Selected := ListViewAccelerator.Items[idx]
    else
    begin
      Dec(idx);
      if idx >= 0 then
        ListViewAccelerator.Selected := ListViewAccelerator.Items[idx];
    end;

   SaveResource(rstDeleteAccelerator);
  end;
end;

end.
