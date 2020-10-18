unit MenuResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  System.Actions, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.ActnList, VCL.Menus, VCL.ExtCtrls, ResourceForm, unitResourceMenus,
  ComponentMenuDesigner, ComponentPropertyListBox;

type
  TFormMenuResource = class(TFormResource)
    ActionList: TActionList;
    ActionMenuAddChildItem: TAction;
    ActionMenuAppendItem: TAction;
    ActionMenuDeleteItem: TAction;
    ActionMenuInsertItem: TAction;
    MainMenu: TMainMenu;
    MenuDesigner: TMenuDesigner;
    MenuItemAddChildItem1: TMenuItem;
    MenuItemAddChildItem2: TMenuItem;
    MenuItemAddItemAfter1: TMenuItem;
    MenuItemAddItemAfter2: TMenuItem;
    MenuItemDeleteItem1: TMenuItem;
    MenuItemDeleteItem2: TMenuItem;
    MenuItemInsetrItem1: TMenuItem;
    MenuItemInsetrItem2: TMenuItem;
    MenuItemMenu: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PanelLeft: TPanel;
    PanelMain: TPanel;
    PopupMenu: TPopupMenu;
    PropertyListBox: TPropertyListBox;
    Splitter: TSplitter;
    procedure MenuDesignerSelectedItemChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionMenuDeleteItemExecute(Sender: TObject);
    procedure MenuDesignerKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PropertyListBoxPropertyChanged(Sender: TObject);
    procedure ActionMenuInsertItemExecute(Sender: TObject);
    procedure ActionMenuAppendItemExecute(Sender: TObject);
    procedure ActionMenuAddChildItemExecute(Sender: TObject);
  private
    FDetails: TMenuResourceDetails;
    procedure SaveResource (const undoDetails: string);
  protected
    procedure SetObject(const Value: TObject); override;
    function GetMenuItem: TMenuItem; override;
  public
    procedure UpdateFonts; override;
  end;

implementation

{$R *.DFM}

resourcestring
  rstCaption = 'Caption';
  rstShortcut = 'Shortcut';
  rstID = 'ID';
  rstEnabled = 'Enabled';
  rstChecked = 'Checked';
  rstNone = '(None)';
  rstCtrl = 'Ctrl';
  rstDel = 'Del';
  rstIns = 'Ins';

  rstDeleteItem = 'delete menu item';
  rstInsertItem = 'insert menu item';
  rstChangeItemCaption = 'change menu item caption';
  rstChangeItemShortcut = 'change menu item shortcut';
  rstChangeItemID = 'change menu item ID';
  rstChangeItemEnabled = 'change menu item enabled state';
  rstChangeItemChecked = 'change menu item checked state';

const
  taCaption = 0;
  taShortcut = 1;
  taID = 2;
  taEnabled = 3;
  taChecked = 4;

{ TFormMenuResource }

procedure TFormMenuResource.SetObject(const Value: TObject);
var
  item: TMenuItem;
begin
  inherited;
  FDetails := Obj as TMenuResourceDetails;

  item := TMenuItem.Create(nil);
  try
    FDetails.GetItems (item);
    MenuDesigner.SetItems (item, False)
  finally
    item.Free;
  end;
end;

procedure TFormMenuResource.UpdateFonts;
begin
  useInternationalFont(PropertyListBox.Font);
  useInternationalFont(MenuDesigner.Font);
end;

procedure TFormMenuResource.MenuDesignerSelectedItemChange(Sender: TObject);
var
  item: TDesignerMenuItem;
  s: WideString;
begin
  inherited;

  item := TDesignerMenuItem (MenuDesigner.SelectedItem);

  with PropertyListBox.FindProperty(rstCaption) do
  begin
    Tag := taCaption;
    PropertyValue := ExtractCaption (UTF8ToWideString (RawByteString(item.Caption)));
  end;

  with PropertyListBox.FindProperty(rstShortcut) do
  begin
    Tag := taShortcut;
    s := ExtractShortcut(Utf8ToWideString (RawByteString(item.Caption)));

    if s = '' then
      s := rstNone;

    PropertyValue := s;
  end;

  with PropertyListBox.FindProperty(rstID) do
  begin
    Tag := taID;
    PropertyValue := IntToStr(item.ID);
  end;

  with PropertyListBox,FindProperty(rstEnabled) do
  begin
    Tag := taEnabled;
    PropertyValue := item.Enabled;
  end;

  with PropertyListBox,FindProperty(rstChecked) do
  begin
    Tag := taChecked;
    PropertyValue := item.Checked;
  end;
end;

procedure TFormMenuResource.FormShow(Sender: TObject);
var
  prop: TPropertyListProperty;

  procedure AddShortcutRange (lo, hi, flags: Word);
  var
    w: Word;
    s: string;
  begin
    for w := lo to hi do
    begin
      s := ShortCutToText(w + flags);
      if s <> '' then
        prop.EnumValues.Add (s)
    end
  end;

begin
  prop := PropertyListBox.FindProperty(rstShortcut);

  prop.EnumValues.Add (rstNone);

  AddShortcutRange (Word ('A'), Word ('Z'), scCtrl);
  AddShortcutRange (Word ('A'), Word ('Z'), scCtrl + scAlt);

  AddShortcutRange (VK_F1, VK_F12, 0);
  AddShortcutRange (VK_F1, VK_F12, scCtrl);
  AddShortcutRange (VK_F1, VK_F12, scShift);
  AddShortcutRange (VK_F1, VK_F12, scCtrl + scCtrl);

  AddShortcutRange (VK_INSERT, VK_INSERT, 0);
  AddShortcutRange (VK_INSERT, VK_INSERT, scCtrl);
  AddShortcutRange (VK_INSERT, VK_INSERT, scShift);

  AddShortcutRange (VK_DELETE, VK_DELETE, 0);
  AddShortcutRange (VK_DELETE, VK_DELETE, scCtrl);
  AddShortcutRange (VK_DELETE, VK_DELETE, scShift);

  AddShortcutRange (VK_BACK, VK_BACK, scAlt);
  AddShortcutRange (VK_BACK, VK_BACK, scAlt + scShift);
end;

function TFormMenuResource.GetMenuItem: TMenuItem;
begin
  Result := MenuItemMenu;
end;

procedure TFormMenuResource.ActionMenuDeleteItemExecute(Sender: TObject);
begin
  if Assigned(MenuDesigner.SelectedItem) then
  begin
    MenuDesigner.DeleteItem(MenuDesigner.SelectedItem);
    SaveResource(rstDeleteItem);
  end;
end;

procedure TFormMenuResource.MenuDesignerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  inherited;

  if key = VK_DELETE then
    ActionMenuDeleteItem.Execute;
end;

procedure TFormMenuResource.SaveResource(const undoDetails: string);
var
  sel: TMenuItem;
begin
  AddUndoEntry(undoDetails);
  sel := MenuDesigner.SelectedItem;
  try
    MenuDesigner.SelectedItem := nil;
    MenuDesigner.RestoreTags;    // Ensure that tags hold the item ID - even if the item is
                                  // selected - that's what SetItems expects.

    FDetails.SetItems (MenuDesigner.Items);
  finally
    MenuDesigner.SelectedItem := sel;
  end;
end;

procedure TFormMenuResource.PropertyListBoxPropertyChanged(Sender: TObject);
var
  prop: TPropertyListProperty;
  s: WideString;
  idx: Integer;
begin
  s := '';
  prop := PropertyListBox.Properties [PropertyListBox.SelectedPropertyNo];
  case PropertyListBox.SelectedPropertyNo of
    taCaption:
      begin
        MenuDesigner.SelectedItem.Caption := String(Utf8Encode (MergeCaption (prop.PropertyValue, ExtractShortcut(Utf8ToString (RawByteString(MenuDesigner.SelectedItem.Caption))))));
        s := rstChangeItemCaption;
      end;

    taShortcut:
      begin
        idx := prop.PropertyValue;
        s := prop.EnumValues [idx];
        if s = rstNone then
          s := '';
        MenuDesigner.SelectedItem.Caption := string(Utf8Encode (MergeCaption (ExtractCaption (UTF8ToWideString (RawByteString(MenuDesigner.SelectedItem.Caption))), s)));
        s := rstChangeItemShortcut;
      end;

    taID:
      begin
        if MenuDesigner.SelectedItem is TDesignerMenuItem then
          TDesignerMenuItem (MenuDesigner.SelectedItem).ID := prop.PropertyValue;
        s := rstChangeItemID;
      end;

    taEnabled:
      begin
        MenuDesigner.SelectedItem.Enabled := prop.PropertyValue;
        s := rstChangeItemEnabled;
      end;

    taChecked:
      begin
        MenuDesigner.SelectedItem.Checked := prop.PropertyValue;
        s := rstChangeItemChecked;
      end;
  end;

  if s <> '' then
    SaveResource(s);
end;

procedure TFormMenuResource.ActionMenuInsertItemExecute(Sender: TObject);
begin
  MenuDesigner.InsertItem(MenuDesigner.SelectedItem);
  SaveResource(rstInsertItem);
end;

procedure TFormMenuResource.ActionMenuAppendItemExecute(Sender: TObject);
begin
  MenuDesigner.AppendItem(MenuDesigner.SelectedItem);
  SaveResource(rstInsertItem);
end;

procedure TFormMenuResource.ActionMenuAddChildItemExecute(Sender: TObject);
begin
  MenuDesigner.AddChildItem(MenuDesigner.SelectedItem);
  SaveResource(rstInsertItem);
end;

end.
