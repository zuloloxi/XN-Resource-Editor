unit AcceleratorResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Menus, Graphics, Controls, Forms,
  Dialogs, ResourceForm, ComCtrls, unitResourceAccelerator, StdCtrls, ActnList;

type
  TfmAcceleratorResource = class(TfmResource)
    lvAccelerator: TListView;
    ActionList1: TActionList;
    pomAccel: TPopupMenu;
    mnuAccelMenu: TMainMenu;
    actAccelAdd: TAction;
    actAccelDelete: TAction;
    actAccelModify: TAction;
    actAccelChangeID: TAction;
    AddAccelerator1: TMenuItem;
    ModifyAccelerator1: TMenuItem;
    DeleteAccelerator1: TMenuItem;
    ChangeID1: TMenuItem;
    mnuAccelerators: TMenuItem;
    AddAccelerator2: TMenuItem;
    ModifyAccelerator2: TMenuItem;
    DeleteAccelerator2: TMenuItem;
    ChangeID2: TMenuItem;
    cbKey: TComboBox;
    cbType: TComboBox;
    actAccelChangeFlags: TAction;
    ChangeFlags1: TMenuItem;
    ChangeFlags2: TMenuItem;
    procedure actAccelDeleteExecute(Sender: TObject);
    procedure cbTypeExit(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure actAccelChangeFlagsExecute(Sender: TObject);
    procedure actAccelChangeIDExecute(Sender: TObject);
    procedure cbKeyChange(Sender: TObject);
    procedure lvAcceleratorEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure lvAcceleratorDblClick(Sender: TObject);
    procedure cbKeyExit(Sender: TObject);
    procedure actAccelModifyExecute(Sender: TObject);
    procedure actAccelAddExecute(Sender: TObject);
    procedure lvAcceleratorEdited(Sender: TObject; Item: TListItem;
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
    procedure SaveResource (const undoDetails: string);
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

function AcceleratorToText (const Accel: TAccelerator): string;
begin
  if (Accel.Flags and FVIRTKEY) <> 0 then
    Result := ShortcutToText (Accel.Code)
  else
    if Accel.Code < 32 then
      Result := 'Ctrl+' + char (Accel.Code + Ord ('@'))
    else
      Result := Char (Accel.Code);

  if (Accel.Flags and FSHIFT) <> 0 then
    Result := 'Shift+' + Result;

  if (Accel.Flags and FALT) <> 0 then
    Result := 'Alt+' + Result;

  if (Accel.Flags and FCONTROL) <> 0 then
    Result :='Ctrl+' + Result
end;

procedure TextToAccelerator (const st: string; virtKey: Boolean; var Code, Flags: Integer);
var
  Temp: Integer;
  Key: Word;
  Shift: TShiftState;
  Ok: Boolean;
begin
  Ok := False;
  if not virtKey then
  begin
    Temp := TextToShortcut (st);
    Key := Temp and not (scAlt or scShift or scCtrl);
    if (Key >= $30) and (Key <= $39) or
       (Key >= $41) and (Key <= $5A) then
    begin
      ShortcutToKey (Temp, Key, Shift);
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
    Code := TextToShortcut (st);
    Flags := FVIRTKEY;
    if (Code and scAlt) <> 0 then
      Flags := Flags or FALT;
    if (Code and scShift) <> 0 then
      Flags := Flags or FSHIFT;
    if (Code and scCtrl) <> 0 then
      Flags := Flags or FCONTROL;
    Code := Code and not (scAlt or scShift or scCtrl);
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

  lvAccelerator.Items.BeginUpdate;
  try
    lvAccelerator.Items.Clear;
    for i := 0 to FDetails.Count - 1 do
    begin
      Accel := FDetails.Accelerator [i];
      with lvAccelerator.Items.Add do
      begin
        Caption := IntToStr (Accel.id);
        SubItems.Add(AcceleratorToText (Accel));
        if (Accel.Flags and FVIRTKEY) <> 0 then
          SubItems.Add(rstVirtKey)
        else
          SubItems.Add(rstCharCode);
//        SubItems.Add(IntToStr (Accel.Flags))
      end
    end
  finally
    lvAccelerator.Items.EndUpdate
  end
end;

procedure TfmAcceleratorResource.lvAcceleratorEdited(Sender: TObject;
  Item: TListItem; var S: string);
var
  newId: Integer;
  i: Integer;
begin
  inherited;
  FAllowEdit := False;
  actAccelDelete.ShortCut := actAccelDelete.Tag;    // Restore 'Delete' shortcut
  if s <> item.Caption then
  try
    newID := StrToInt (s);

    for i := 0 to FDetails.Count - 1 do
      if FDetails.Accelerator [i].id = newid then
        raise Exception.Create (rstDuplicateMessageID);

    item.Caption := s;          // need to do this so 'SaveResource' picks it up...
    SaveResource (rstChangeID);
  except
    s := item.Caption;
    raise
  end
end;

function TfmAcceleratorResource.GetMenuItem: TMenuItem;
begin
  Result := mnuAccelerators
end;

procedure TfmAcceleratorResource.actAccelAddExecute(Sender: TObject);
var
  item: TListItem;
  newID: Integer;
begin
  item := lvAccelerator.Items.Add;

  // Work out string / message ID
  if item.Index = 0 then
    item.Caption := '1'
  else
  begin
    newId :=StrToInt (lvAccelerator.Items [lvAccelerator.Items.Count - 2].Caption);
    item.Caption := IntToStr (newId + 1)
  end;

  item.SubItems.Add ('');
  item.SubItems.Add (rstVirtKey);
  item.Selected := True;

  FAdding := True;                    // Clear in mmoMessageExit

  actAccelModifyExecute (Sender)
end;

procedure TfmAcceleratorResource.actAccelModifyExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    cbKey.Width := lvAccelerator.Columns [1].Width - 2;
    cbKey.Left := lvAccelerator.Columns [0].Width + 2;
    t := lvAccelerator.Selected.DisplayRect (drLabel).Top - 3;
    if t < 0 then
      t := 0;
    cbKey.Top := t;
    cbKey.Text := lvAccelerator.Selected.SubItems [0];
    cbKey.Visible := True;
    cbKey.SetFocus
  end
  else
    actAccelAdd.Execute;
end;

procedure TfmAcceleratorResource.cbKeyExit(Sender: TObject);
var
  st: string;
  adding: Boolean;
begin
  adding := FAdding;
  FAdding := False;
  cbKey.Visible := False;
  with lvAccelerator do if Assigned (Selected) then
  begin
    if FKeyChanged or FAdding then  // ie.  If it's changed
    begin
      selected.SubItems [0] := cbKey.Text;

      if adding then
        st := rstAddAccelerator
      else
        st := rstChangeAccelerator;

      SaveResource (st);
    end
  end;
  FKeyChanged := False
end;

procedure TfmAcceleratorResource.SaveResource(const undoDetails: string);
var
  i, Flags, Code, id: Integer;
  item: TListItem;
  virtKey: Boolean;
begin
  AddUndoEntry (undoDetails);
  for i := 0 to lvAccelerator.Items.Count - 1 do
  begin
    item := lvAccelerator.Items [i];
    id := StrToInt (item.Caption);
    virtKey := item.SubItems [1] = rstVirtKey;
    TextToAccelerator (item.SubItems [0], virtKey, Code, Flags);

    if not virtKey and ((Flags and FVIRTKEY) <> 0) then
      item.SubItems [1] := rstVirtKey;

    if i < FDetails.Count then
      FDetails.SetAccelDetails (i, Flags, Code, id)
    else
      FDetails.Add(Flags, Code, id)
  end;

  i := lvAccelerator.Items.Count;
  while i < FDetails.Count do
    FDetails.Delete(i);
end;

procedure TfmAcceleratorResource.lvAcceleratorDblClick(Sender: TObject);
var
  p: TPoint;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    p := Mouse.CursorPos;
    MapWindowPoints (HWND_DESKTOP, lvAccelerator.Handle, p, 1);

    if p.x > lvAccelerator.Columns [0].Width + lvAccelerator.Columns [1].Width then
      actAccelChangeFlags.Execute
    else
      if p.x > lvAccelerator.Columns [0].Width then
        actAccelModify.Execute
      else
        actAccelChangeID.Execute
  end
  else
    actAccelAdd.Execute
end;

procedure TfmAcceleratorResource.FormCreate(Sender: TObject);

  procedure AddShortcutRange (const st: string; lowRange, highRange: char);
  var
    ch: char;
  begin
    for ch := lowRange to highRange do
      cbKey.Items.Add(st + ch)
  end;

begin
  inherited;

  cbKey.Items.BeginUpdate;
  try
    AddShortcutRange ('Ctrl+', 'A', 'Z');
    AddShortcutRange ('Ctrl+Alt+', 'A', 'Z');
    AddShortcutRange ('F', '1', '9');
    AddShortcutRange ('F1', '0', '2');
    AddShortcutRange ('Ctrl+F', '1', '9');
    AddShortcutRange ('Ctrl+F1', '0', '2');
    AddShortcutRange ('Shift+F', '1', '9');
    AddShortcutRange ('Shift+F1', '0', '2');
    AddShortcutRange ('Shift+Ctrl+F', '1', '9');
    AddShortcutRange ('Shift+Ctrl+F1', '0', '2');
    AddShortcutRange ('In', 's', 's');
    AddShortcutRange ('Shift+In', 's', 's');
    AddShortcutRange ('Ctrl+In', 's', 's');
    AddShortcutRange ('De', 'l', 'l');
    AddShortcutRange ('Shift+De', 'l', 'l');
    AddShortcutRange ('Ctrl+De', 'l', 'l');
    AddShortcutRange ('Alt+BkS', 'p', 'p');
    AddShortcutRange ('Shift+Alt+BkS', 'p', 'p');
  finally
    cbKey.Items.EndUpdate
  end;

  cbType.Items.Add(rstVirtKey);
  cbType.Items.Add(rstCharCode);
end;

procedure TfmAcceleratorResource.lvAcceleratorEditing(Sender: TObject;
  Item: TListItem; var AllowEdit: Boolean);
begin
  inherited;
  AllowEdit := FAllowEdit;
  actAccelDelete.Tag := actAccelDelete.ShortCut;    // Suspend 'Delete' shortcut.
  actAccelDelete.ShortCut := 0;
end;

procedure TfmAcceleratorResource.cbKeyChange(Sender: TObject);
begin
  inherited;
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.actAccelChangeIDExecute(Sender: TObject);
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    FAllowEdit := true;
    lvAccelerator.Selected.EditCaption
  end
end;

procedure TfmAcceleratorResource.actAccelChangeFlagsExecute(Sender: TObject);
var
  t: Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    cbType.Width := lvAccelerator.Columns [2].Width - 2;
    cbType.Left := lvAccelerator.Columns [0].Width + lvAccelerator.Columns [1].Width + 2;
    t := lvAccelerator.Selected.DisplayRect (drLabel).Top - 3;
    if t < 0 then
      t := 0;
    cbType.Top := t;
    cbType.Text := lvAccelerator.Selected.SubItems [1];
    cbType.Visible := True;
    cbType.SetFocus
  end
end;

procedure TfmAcceleratorResource.cbTypeChange(Sender: TObject);
begin
  FKeyChanged := True;
end;

procedure TfmAcceleratorResource.cbTypeExit(Sender: TObject);
begin
  cbType.Visible := False;
  with lvAccelerator do if Assigned (Selected) then
  begin
    if FKeyChanged then  // ie.  If it's changed
    begin
      selected.SubItems [1] := cbType.Text;

      SaveResource (rstChangeAcceleratorType);
    end
  end;
  FKeyChanged := False
end;

procedure TfmAcceleratorResource.actAccelDeleteExecute(Sender: TObject);
var
  idx: Integer;
begin
  if Assigned (lvAccelerator.Selected) then
  begin
    idx := lvAccelerator.Selected.Index;
    lvAccelerator.Selected.Free;

                                // Select next string
    if idx < lvAccelerator.Items.Count then
      lvAccelerator.Selected := lvAccelerator.Items [idx]
    else
    begin
      Dec (idx);
      if idx >= 0 then
        lvAccelerator.Selected := lvAccelerator.Items [idx]
    end;

   SaveResource (rstDeleteAccelerator)
  end
end;

end.
