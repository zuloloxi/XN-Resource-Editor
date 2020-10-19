unit CursorGraphicsResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  System.Actions, System.ImageList, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.Menus, VCL.ActnList, VCL.ImgList, VCL.ComCtrls, VCL.ExtCtrls,
  VCL.ToolWin, IconCursorGraphicsResourceForm,
  ComponentColorSelector, ComponentSizingPageControl,
  ComponentBitmapEditor, ComponentPropertyListBox, unitExIcon;

type
  TFormCursorGraphicsResource = class(TFormIconCursorGraphicsResource)
    SetHotspot1: TMenuItem;
    procedure actImageHotSpotExecute(Sender: TObject);
    procedure SetHotspot1Click(Sender: TObject);
    procedure PropertyListBox1PropertyChanged(Sender: TObject);
  private
    function GetCursor: TExCursor;
    procedure SetHotspot(const ScreenPt: TPoint);
  protected
    procedure SetObject(const Value: TObject); override;
  public
    procedure PreviewKey (var key: Word; shift: TShiftState); override;
    procedure SaveResource (const undoDetails: string); override;

    property Cursor: TExCursor read GetCursor;
  end;

implementation

{$R *.dfm}

resourcestring
  rstHotX = 'Hot Spot Left';
  rstHotY = 'Hot Spot Top';
  rstHotspotChanged = 'hotspot change';

const
  crCurrentCursor = 10;

  taHotX = 20;
  taHotY = 21;


{ TFormCursorGraphicsResource }

procedure TFormCursorGraphicsResource.SetHotspot(const ScreenPt: TPoint);
var
  Pt: TPoint;
  Change: string;
begin
  Pt := ScreenPt;
  MapWindowPoints (0, BitmapEditor.Handle, Pt, 1);
  Pt.X := Pt.X div BitmapEditor.Magnification;
  Pt.Y := Pt.Y div BitmapEditor.Magnification;

  Cursor.Hotspot := Pt.X + Pt.Y shl 16;
  Change := rstHotspotChanged;
  BitmapEditor.HotSpotX := LoWord(Self.Cursor.Hotspot);
  BitmapEditor.HotSpotY := HiWord(Self.Cursor.Hotspot);
  BitmapEditor.Picture := Image.Picture;      // Set the editor picture
  SaveResource(Change);
end;

procedure TFormCursorGraphicsResource.actImageHotSpotExecute(Sender: TObject);
begin
  inherited;
//
end;

function TFormCursorGraphicsResource.GetCursor: TExCursor;
begin
  Result := TExCursor (Image.Picture.Graphic);
end;

procedure TFormCursorGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin
  inherited;

  case key of
    VK_MULTIPLY:
      SetHotspot(Mouse.CursorPos);
  end;
end;

procedure TFormCursorGraphicsResource.PropertyListBox1PropertyChanged(
  Sender: TObject);
var
  Change: string;
  prop: TPropertyListProperty;
begin
  with PropertyListBox do
    prop := Properties [SelectedPropertyNo];

  Change := '';

  case Prop.Tag of
    taHotX:
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $ffff0000) or Prop.PropertyValue;
        Change := rstHotspotChanged;
        BitmapEditor.HotSpotX := LoWord (Self.Cursor.Hotspot);
      end;
    taHotY:
      begin
        Cursor.Hotspot := (Cursor.Hotspot and $0000ffff) or (Prop.PropertyValue shl 16);
        Change := rstHotspotChanged;
        BitmapEditor.HotSpotY := HiWord (Self.Cursor.Hotspot);
      end;
  end;

  if Change <> '' then
  begin
    BitmapEditor.Picture := Image.Picture;      // Set the editor picture
    SaveResource (Change);
  end
  else
    inherited;
end;

procedure TFormCursorGraphicsResource.SaveResource(const undoDetails: string);
begin
  inherited;
  Screen.Cursors [crCurrentCursor] := Cursor.Handle;
end;

type
  TCrackPopupMenu = class (TPopupMenu)
  end;
procedure TFormCursorGraphicsResource.SetHotspot1Click(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt := PopupMenu.PopupPoint;
  if Pt.X = -1 then
    Pt := Mouse.CursorPos;
  SetHotspot(Pt);
  Pt.x := -1;
  Pt.y := -1;
  TCrackPopupMenu (PopupMenu).SetPopupPoint(Pt);
end;

procedure TFormCursorGraphicsResource.SetObject(const Value: TObject);
begin
  inherited;

  with PropertyListBox do                      // Set the properties
  begin
    with FindProperty (rstHotX) do
    begin
      Tag := taHotX;
      PropertyValue := LoWord (Self.Cursor.Hotspot);
    end;

    with FindProperty (rstHotY) do
    begin
      Tag := taHotY;
      PropertyValue := HiWord (Self.Cursor.Hotspot);
    end
  end;

  Screen.Cursors [crCurrentCursor] := Cursor.Handle;
  ScrollBoxThumbnail.Cursor := crCurrentCursor;
  BitmapEditor.HotSpotX := LoWord(Self.Cursor.Hotspot);
  BitmapEditor.HotSpotY := HiWord(Self.Cursor.Hotspot);
end;

end.
