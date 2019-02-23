unit CursorGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ActnList, ImgList, ComCtrls, ExtCtrls, ToolWin,
  IconCursorGraphicsResourceForm, cmpColorSelector, cmpSizingPageControl,
  cmpBitmapEditor, cmpPropertyListBox, unitExIcon, System.Actions,
  System.ImageList;

type
  TfmCursorGraphicsResource = class(TfmIconCursorGraphicsResource)
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


{ TfmCursorGraphicsResource }

procedure TfmCursorGraphicsResource.SetHotspot(const ScreenPt: TPoint);
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

procedure TfmCursorGraphicsResource.actImageHotSpotExecute(Sender: TObject);
begin
  inherited;
//
end;

function TfmCursorGraphicsResource.GetCursor: TExCursor;
begin
  Result := TExCursor (Image.Picture.Graphic);
end;

procedure TfmCursorGraphicsResource.PreviewKey(var key: Word;
  shift: TShiftState);
begin
  inherited;

  case key of
    VK_MULTIPLY:
      SetHotspot(Mouse.CursorPos);
  end;
end;

procedure TfmCursorGraphicsResource.PropertyListBox1PropertyChanged(
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

procedure TfmCursorGraphicsResource.SaveResource(const undoDetails: string);
begin
  inherited;
  Screen.Cursors [crCurrentCursor] := Cursor.Handle;
end;

type
  TCrackPopupMenu = class (TPopupMenu)
  end;
procedure TfmCursorGraphicsResource.SetHotspot1Click(Sender: TObject);
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

procedure TfmCursorGraphicsResource.SetObject(const Value: TObject);
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
