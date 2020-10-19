(*======================================================================*
 | DialogResourceForm                                                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/03/2001  CPWW  Original                                  |
 *======================================================================*)

unit DialogResourceForm;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  Vcl.ExtCtrls, Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  ResourceForm, ComponentSizingPageControl, ComponentDialogBox, ComponentRuler,
  ComponentPropertyListBox, ComponentDialogEditor, unitResourceDialogs;

type
  TFormDialogResource = class(TFormResource)
    pnlProperties: TPanel;
    Splitter1: TSplitter;
    ScrollBox1: TScrollBox;
    pnlPropertyCombo: TPanel;
    cbControls: TComboBox;
    Panel3: TPanel;
    Panel4: TPanel;
    DialogEditor1: TDialogEditor;
    Ruler1: TRuler;
    Ruler2: TRuler;
    tcPropertyKind: TTabControl;
    SizingPageControl1: TSizingPageControl;
    pnlPalette: TPanel;
    ImageList1: TImageList;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    FontDialog1: TFontDialog;
    PropertyListBox1: TPropertyListBox;
    procedure DialogEditor1Resize(Sender: TObject);
    procedure DialogEditor1DesignModeSelectedItemChange(Sender: TObject);
    procedure DialogEditor1Show(Sender: TObject);
    procedure cbControlsChange(Sender: TObject);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
    procedure tcPropertyKindChange(Sender: TObject);
    procedure DialogEditor1ControlResize(Sender: TObject;
      ctrlInfo: TControlInfo; newRect: TRect);
    procedure DialogEditor1ControlPropertyChange(Sender: TObject;
      ctrlInfo: TControlInfo);
    procedure FormShow(Sender: TObject);
    procedure SizingPageControl1UnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure SizingPageControl1DockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure ToolButton2Click(Sender: TObject);
    procedure DialogEditor1DesignModeDropControl(sender: TObject; x,
      y: Integer; ctrl: TControlInfo);
    procedure DialogEditor1DeleteControl(Sender: TObject;
      ctrl: TControlInfo);
    procedure ToolButton1Click(Sender: TObject);
    procedure DialogEditor1GetControlImage(Sender: TObject; tp: Integer;
      const id: string; var Handle: HGDIOBJ);

    procedure DoSpecialButtonClick (Sender: TObject);
  private
    FDetails: TDialogResourceDetails;
    FPCWidth: Integer;

    procedure FillPropertyBox (info: TControlInfo; reset: Boolean);
    procedure SaveResource (const undoDetails: string);

  protected
    procedure SetObject(const Value: TObject); override;
    function GetCanDelete: Boolean; override;
  public
    procedure EditDelete; override;
    procedure UpdateFonts; override;
  end;

var
  fmDialogResource: TFormDialogResource;

implementation

uses 
  Variants, unitResourceDetails, unitResourceGraphics, unitExIcon, 
  unitResourceMenus;

resourcestring
  rstChangeProperty = 'change %s';
  rstResize = 'resize';
  rstAddControl = 'add control';
  rstDeleteControl = 'delete control';
  rstSetFont = 'set font';

{$R *.DFM}

{ TFormDialogResource }

(*----------------------------------------------------------------------*
 | TFormDialogResource.SetObject                                          |
 |                                                                      |
 | Set to display a new dialog object                                   |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.SetObject(const Value: TObject);
begin
  inherited;
  FDetails := obj as TDialogResourceDetails;

  DialogEditor1.ResourceTemplate := FDetails.Data.Memory;
  tcPropertyKind.TabIndex := 0
end;


(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1Resize                                |
 |                                                                      |
 | Dialog box has been resized.  Adjust the rulers                      |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditor1Resize(Sender: TObject);
begin
  inherited;

  Ruler1.Width := DialogEditor1.Width;
  Ruler2.Height := DialogEditor1.Height;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1DesignModeSelectedItemChange          |
 |                                                                      |
 | A control has been selected.   Find the control in the 'Controls'    |
 | combo, and set the properties to match the contol's                  |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditor1DesignModeSelectedItemChange(
  Sender: TObject);
var
  info: TControlInfo;
  i: Integer;
begin
  info := DialogEditor1.SelectedControl;

  if Assigned(info) then
  begin
    for i := 0 to cbControls.Items.Count - 1 do
      if info = TControlInfo (cbControls.Items.Objects [i]) then
      begin
        cbControls.ItemIndex := i;
        cbControls.Hint := cbControls.Text;
        break
      end;

    FillPropertyBox (info, True)
  end
end;

(*----------------------------------------------------------------------*
 | GetInfoDescription                                                   |
 |                                                                      |
 | Get the description for a control to display in the 'controls'       |
 | combo.                                                               |
 *----------------------------------------------------------------------*)
function GetInfoDescription (info: TControlInfo): string;
var
  idx: Integer;
begin
  idx := Info.FindProperty (pkGeneral, 'Text');
  if idx = -1 then
    idx := Info.FindProperty (pkGeneral, 'Caption');

  if idx = -1 then
    Result := Format('%d %s', [info.ItemID, info.GetDescription])
  else
    Result := Format('%d %s "%s"', [info.ItemID, info.GetDescription, info.PropertyValue [pkGeneral, idx]])
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1Show                                  |
 |                                                                      |
 | OnShow handler for the dialog editor.  Called when the dialog editor |
 | has created the dialog box, so we can read it's info and initialize  |
 | accordingly.  NB  We can't do this in 'SetObject' - the dialog box   |
 | hasn't been created then.                                            |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditor1Show(Sender: TObject);
var
  i: Integer;
  info: TControlInfo;
begin
  Ruler1.DialogBox := DialogEditor1.DialogHandle;
  Ruler2.DialogBox := DialogEditor1.DialogHandle;

  cbControls.Items.Clear;               // Fill in the Controls combo
  cbControls.Items.BeginUpdate;
  try
    info := DialogEditor1.DialogInfo;
    cbControls.Items.AddObject(GetInfoDescription (info), info);

    for i := 0 to DialogEditor1.ControlInfoCount - 1 do
    begin
      info := DialogEditor1.ControlInfo [i];
      cbControls.Items.AddObject(GetInfoDescription (info), info);
    end
  finally
    cbControls.Items.EndUpdate
  end;

  cbControls.ItemIndex := 0;
  cbControls.Hint := cbControls.Text;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.cbControlsChange                                   |
 |                                                                      |
 | Select a control, based on the controls combo                        |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.cbControlsChange(Sender: TObject);
var
  info: TControlInfo;
begin
  info := TControlInfo (cbControls.Items.Objects [cbControls.ItemIndex]);
  cbControls.Hint := cbControls.Text;
  DialogEditor1.SelectedControl := info
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.PropertyListBox1PropertyChanged                    |
 |                                                                      |
 | A property has been changed in the property list box.  Update the    |
 | dialog editor.                                                       |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.PropertyListBox1PropertyChanged(
  Sender: TObject);
var
  info: TControlInfo;
  prop: TPropertyListProperty;
  idx: Integer;
  kind: TPropertyKind;
  st: string;
begin
  info := TControlInfo (cbControls.Items.Objects [cbControls.ItemIndex]);

  prop := PropertyListBox1.Properties [PropertyListBox1.SelectedPropertyNo];


  case tcPropertyKind.TabIndex of
    1: kind := pkStyle;
    2: kind := pkExtended
    else
      kind := pkGeneral
  end;

  st := prop.PropertyName;
  idx := info.FindProperty (kind, st);
  info.PropertyValue [kind, idx] := prop.PropertyValue;  // nb. May make 'prop' invalid!!

  st := Format(rstChangeProperty, [st]);

  idx := cbControls.ItemIndex;
  cbControls.Items [idx] := GetInfoDescription (info);
  cbControls.ItemIndex := idx;

  SaveResource (st);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.FillPropertyBox                                    |
 |                                                                      |
 | Fill the property list box with properties and their values for the  |
 | control.                                                             |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.FillPropertyBox(info: TControlInfo; reset: Boolean);
var
  kind: TPropertyKind;
  i, j: Integer;
  prop: TPropertyListProperty;
  propName: string;
  idx: Integer;
  val: Variant;
begin

  // Get the property kind
  case tcPropertyKind.TabIndex of
    1:
      kind := pkStyle;
    2:
      kind := pkExtended
    else
      kind := pkGeneral
  end;

  idx := PropertyListBox1.SelectedPropertyNo;

  PropertyListBox1.Properties.Clear;
  PropertyListBox1.Properties.BeginUpdate;
  try
    for i := 0 to info.PropertyCount [kind] - 1 do
    begin
      // Add each property.
      prop := TPropertyListProperty (PropertyListBox1.Properties.Add);
      propName := info.PropertyName [kind, i];
      prop.PropertyName := propName;
      val := info.PropertyValue [kind, i];
      prop.Enabled := not VarIsEmpty (val);

      case info.PropertyType [kind, i] of
        ptString:
          prop.PropertyType := ComponentPropertyListBox.ptString;
        ptInteger:
          prop.PropertyType := ComponentPropertyListBox.ptInteger;
        ptBoolean:
          prop.PropertyType := ComponentPropertyListBox.ptBoolean;
        ptSpecial:
          begin
            prop.PropertyType := ComponentPropertyListBox.ptSpecial;
            prop.OnSpecialButtonClick := DoSpecialButtonClick
          end;

        ptEnum:
          begin
            // Add enumerated property values
            prop.PropertyType := ComponentPropertyListBox.ptEnum;
            prop.EnumValues.Clear;
            prop.EnumValues.BeginUpdate;
            try
              for j := 0 to info.PropertyEnumCount[kind, i] - 1 do
                prop.EnumValues.Add(info.PropertyEnumName [kind, i, j])
            finally
              prop.EnumValues.EndUpdate
            end
          end
      end;
                                        // Set the property value
      if not VarIsEmpty (val) then
        prop.PropertyValue := val
    end
  finally
    PropertyListBox1.Properties.EndUpdate
  end;

  if reset then
    // Select first property
    PropertyListBox1.SelectedPropertyNo := 0
  else
    PropertyListBox1.SelectedPropertyNo := idx;
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.tcPropertyKindChange                               |
 |                                                                      |
 | A different propery tab has been selected.  Display the appropriate  |
 | properties.                                                          |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.tcPropertyKindChange(Sender: TObject);
var
  info: TControlInfo;
begin
  info := DialogEditor1.SelectedControl;

  FillPropertyBox (info, True)
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.SaveResource                                       |
 |                                                                      |
 | Get and save a new resource template from the editor.                |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.SaveResource(const undoDetails: string);
begin
  AddUndoEntry (undoDetails);

  FDetails.Data.Clear;
  DialogEditor1.SaveToStream (FDetails.Data);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1ControlResize                         |
 |                                                                      |
 | Update the display when a control is resized by dragging             |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditor1ControlResize(Sender: TObject;
  ctrlInfo: TControlInfo; newRect: TRect);
begin
  FillPropertyBox (ctrlInfo, False);
  SaveResource (rstResize);
end;

(*----------------------------------------------------------------------*
 | TFormDialogResource.DialogEditor1ControlPropertyChange                 |
 |                                                                      |
 | Update the property display when a control property changes          |
 | 'magically' - eg, when setting Show Maximize Box, automagically      |
 | changes 'Show System Menu'                                           |
 *----------------------------------------------------------------------*)
procedure TFormDialogResource.DialogEditor1ControlPropertyChange(
  Sender: TObject; ctrlInfo: TControlInfo);
begin
  FillPropertyBox (ctrlInfo, False);
end;

procedure TFormDialogResource.FormShow(Sender: TObject);
var
  i: Integer;
  dc: TDropControl;
  ic: TControlInfoClass;
begin
  Ruler1.Left := DialogEditor1.Left + DialogEditor1.Margin;
  Ruler2.Top := DialogEditor1.Top + DialogEditor1.Margin - Panel3.Height;
  Ruler1.Width := DialogEditor1.Width - 2 * DialogEditor1.Margin;
  Ruler2.Height := DialogEditor1.Height - 2 * DialogEditor1.Margin;
  FPCWidth := pnlPalette.Width;
  pnlPalette.ManualDock (SizingPageControl1, Nil, alNone);

  for i := 0 to ToolBar1.ButtonCount - 1 do
  begin
    dc := TDropControl (ToolBar1.Buttons [i].Tag);

    if dc <> drNone then
    begin
      ic := GetControlInfoClass (dc);
      if Assigned(ic) then
        ToolBar1.Buttons [i].Hint := ic.GetDescription
      else
        ToolBar1.Buttons [i].ShowHint := False
    end
    else
      ToolBar1.Buttons [i].ShowHint := False
  end;
end;

procedure TFormDialogResource.SizingPageControl1UnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  if SizingPageControl1.PageCount = 1 then
    SizingPageControl1.Width := 0;
end;

procedure TFormDialogResource.SizingPageControl1DockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
var
  i: Integer;
begin
  with SizingPageControl1 do
  begin
    for i := 0 to PageCount - 1 do
      Pages [i].Caption := TPanel (Pages [i].Controls [0]).Caption;

    Width := FPCWidth + 8;      // Restore the width to it's original setting
                                // - we've got at least one tab.
  end
end;

procedure TFormDialogResource.ToolButton2Click(Sender: TObject);
begin
  DialogEditor1.DropControl := TDropControl ((Sender as TToolButton).Tag)
end;

procedure TFormDialogResource.UpdateFonts;
begin
  UseInternationalFont(PropertyListBox1.Font);
end;

procedure TFormDialogResource.DialogEditor1DesignModeDropControl(
  sender: TObject; x, y: Integer; ctrl: TControlInfo);
var
  idx: Integer;
begin
  idx := cbControls.Items.AddObject(GetInfoDescription (ctrl), ctrl);
  cbControls.ItemIndex := idx;
  SaveResource (rstAddControl);
  ToolButton1.Down := True;
  DialogEditor1.SelectedControl := ctrl
end;

function TFormDialogResource.GetCanDelete: Boolean;
begin
  Result := Assigned(DialogEditor1.SelectedControl);
end;

procedure TFormDialogResource.EditDelete;
begin
  DialogEditor1.DeleteControl (DialogEditor1.SelectedControl);
end;

procedure TFormDialogResource.DialogEditor1DeleteControl(Sender: TObject;
  ctrl: TControlInfo);
var
  i: Integer;
begin
  i := 0;
  while i < cbControls.Items.Count do
    if cbControls.Items.Objects [i] = ctrl then
      cbControls.Items.Delete (i)
    else
      Inc(i);

  SaveResource (rstDeleteControl)
end;

procedure TFormDialogResource.ToolButton1Click(Sender: TObject);
begin
  DialogEditor1.DropControl := drNone
end;

procedure TFormDialogResource.DialogEditor1GetControlImage(Sender: TObject;
  tp: Integer; const id: string; var Handle: HGDIOBJ);
var
  details: TResourceDetails;
  picture: TPicture;
  resType: Integer;
begin
  if tp = IMAGE_ICON then
    resType := Integer (RT_GROUP_ICON)
  else
    if tp = IMAGE_BITMAP then
      resType := Integer (RT_BITMAP)
    else
      resType := 0;

  details := FDetails.Parent.FindResource (IntToStr (resType), id, FDetails.ResourceLanguage);
  if Assigned(details) then
  begin
    picture := TPicture.Create;
    try
      if details is TIconGroupResourceDetails then
      begin
        TIconGroupResourceDetails (details).GetImage (picture);
        Handle := TExIcon (picture.graphic).ReleaseHandle
      end
      else
        if details is TBitmapResourceDetails then
        begin
          TBitmapResourceDetails (details).GetImage (picture);
          Handle := TBitmap (picture.Graphic).ReleaseHandle
        end
    finally
      picture.Free
    end
  end
end;

procedure TFormDialogResource.DoSpecialButtonClick(Sender: TObject);
var
  prop: TPropertyListProperty;
  st: string;
begin
  prop := TPropertyListProperty (Sender);

  st := prop.PropertyName;

  if st = 'Font' then
  begin
    FontDialog1.Font.Handle := DialogEditor1.FontHandle;
    if FontDialog1.Execute then
    begin
      DialogEditor1.SetTemplateFont(FontDialog1.Font);
      SaveResource (rstSetFont);
      DialogEditor1.ResourceTemplate := FDetails.Data.Memory;
    end
  end
end;

end.
