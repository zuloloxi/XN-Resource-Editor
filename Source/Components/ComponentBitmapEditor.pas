unit ComponentBitmapEditor;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TDrawingTool = (
    dtNone, dtPencil, dtLine, dtFrameRect, dtFillRect, dtRect, dtFloodFill,
    dtFrameRoundRect, dtFillRoundRect, dtRoundRect, dtFrameEllipse,
    dtFillEllipse, dtEllipse, dtMagnifier, dtBrush, dtSelectRect, dtSelectArea,
    dtDropper, dtText, dtEraser, dtAirbrush, dtGradRectLR, dtGradRectTB,
    dtGradRectTLBR, dtGradRectBLTR
  );

  TOnGetText = procedure (Sender: TObject; Font: TFont; var Txt: WideString) of object;

  TBitmapEditor = class(TCustomControl)
  private
    FPicture: TPicture;
    FMagnification: Integer;
    FGridLines: Integer;
    FBorderStyle: TBorderStyle;
    FDrawingTool: TDrawingTool;
    FLastDrawingTool: TDrawingTool;    // Reselect after using dropper
    FDrawBrush: TBrush;
    FDrawPen: TPen;
    FEraser: TPen;
    FDrawBmp: TBitmap;
    FScratchBmp: TBitmap;
    FSelectionBmp: TBitmap;
    FPos: TPoint;
    FOnChange: TNotifyEvent;
    FTransparentColor: TColor;
    FOnDrawToolChange: TNotifyEvent;
    FCrossX, FCrossY: Integer;
    FSelectionRect: TRect;
    FLButtonIsDown: Boolean;
    FMouseCaptured: Boolean;
    FOnEndChange: TNotifyEvent;
    FCallEndChange: Boolean;
    FOnSelectionRectChange: TNotifyEvent;
    FClipboardPalette: HPALETTE;
    FClipboardPixelFormat: TPixelFormat;
    FOnGetText: TOnGetText;
    FHotSpotX: Integer;
    FHotSpotY: Integer;

    procedure SetPicture(const Value: TPicture);
    procedure PaintBitmap (bmp: TBitmap);
    procedure SetMagnification(const Value: Integer);
    procedure SizeToPicture;
    procedure SetGridLines(const Value: Integer);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetDrawBrush(const Value: TBrush);
    procedure SetDrawingTool(const Value: TDrawingTool);
    procedure SetDrawPen(const Value: TPen);
    procedure ChangeSelectionRect(const Rect: TRect);
    procedure RedrawBitmap;
    procedure Initialize;
    procedure SetTransparentColor(const Value: TColor);
    procedure DisplayCrossHairs;
    function GetSelectionValid: Boolean;
    procedure SetHotSpotX(const Value: Integer);
    procedure SetHotSpotY(const Value: Integer);
    procedure DrawHotSpot(canvas: TCanvas);
  protected
    procedure Paint; override;
    procedure CreateParams (var params: TCreateParams ); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ZoomIn;
    procedure ZoomOut;
    property DrawBmp: TBitmap read FDrawBmp;
    property SelectionValid: Boolean read GetSelectionValid;
    property SelectionRect: TRect read FSelectionRect;
    property ClipboardPalette: HPALETTE read FClipboardPalette write FClipboardPalette;
    property ClipboardPixelFormat: TPixelFormat read FClipboardPixelFormat write FClipboardPixelFormat;
    procedure SelectAll;
    procedure DeleteSelection;
    procedure CopySelection;
    procedure CutSelection;
    procedure PasteSelection;
    procedure PictureChanged;

    procedure Rotate180;
    procedure Rotate90;
    procedure Rotate270;

    function GetDrawingChangeDescription: string;

    property HotSpotX: Integer read FHotSpotX write SetHotSpotX default -1;
    property HotSpotY: Integer read FHotSpotY write SetHotSpotY default -1;

  published
    property Picture: TPicture read FPicture write SetPicture;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clTeal;
    property Magnification: Integer read FMagnification write SetMagnification default 4;
    property GridLines: Integer read FGridLines write SetGridLines default 4;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property DoubleBuffered;
    property PopupMenu;
    property TabStop;
    property DrawingTool: TDrawingTool read FDrawingTool write SetDrawingTool;
    property DrawPen: TPen read FDrawPen write SetDrawPen;
    property DrawBrush: TBrush read FDrawBrush write SetDrawBrush;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEndChange: TNotifyEvent read FOnEndChange write FOnEndChange;
    property OnDrawToolChange: TNotifyEvent read FOnDrawToolChange write FOnDrawToolChange;
    property OnSelectionRectChange: TNotifyEvent read FOnSelectionRectChange write FOnSelectionRectChange;
    property OnGetText: TOnGetText read FOnGetText write FOnGetText;
  end;

const
  crPencil = 1;
  crPotOPaint = 2;
  crMagnifier = 3;
  crDotCross = 4;
  crDropper = 5;
  crAirbrush = 6;

  DrawingCursors: array [TDrawingTool] of TCursor = (
    crArrow, crPencil, crCross, crCross, crCross, crCross, crPotOPaint,
    crCross, crCross, crCross, crCross, crCross, crCross, crMagnifier,
    crDotCross, crNone, crNone, crDropper, crIBeam, crCross, crAirbrush,
    crCross, crCross, crCross, crCross
  );

implementation

{$R BitmapEditorCursors.res}

uses
  Vcl.Clipbrd, Vcl.Imaging.GifImg, GraphFlip, ComponentGradientShape;

{ TBitmapEditor }

procedure TBitmapEditor.ChangeSelectionRect(const Rect: TRect);
var
  OldValid: Boolean;
begin
  OldValid := SelectionValid;

  if Rect.Left <> -2 then FSelectionRect.Left := Rect.Left;
  if Rect.Top <> -2 then FSelectionRect.Top := Rect.Top;
  if Rect.Right <> -2 then FSelectionRect.Right := Rect.Right;
  if Rect.Bottom <> -2 then FSelectionRect.Bottom := Rect.Bottom;

  if SelectionValid or OldValid then
    Invalidate;

  if Assigned(OnSelectionRectChange) then
    OnSelectionRectChange (Self)
end;

procedure TBitmapEditor.CopySelection;
var
  Bitmap: TBitmap;
  MemoryStream: TMemoryStream;
  AData: THandle;
  LockPointer: PChar;
  Size: Integer;
  Rct: TRect;

begin
  MemoryStream := nil;
  Bitmap := TBitmap.Create;
  try
    MemoryStream := TMemoryStream.Create;
    Bitmap.PixelFormat := ClipboardPixelFormat;
    Bitmap.Palette := ClipboardPalette;
    Bitmap.Width := FSelectionRect.Right - FSelectionRect.Left + 1;
    Bitmap.Height := FSelectionRect.Bottom - FSelectionRect.Top + 1;

    Rct := FSelectionRect;
    Inc(Rct.Right);
    Inc(Rct.Bottom);

    Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), DrawBmp.Canvas, Rct);

    Bitmap.SaveToStream (MemoryStream);
    Size := MemoryStream.Size - SizeOf (TBitmapFileHeader);

    AData := GlobalAlloc (GMEM_DDESHARE, size);
    try
      LockPointer := GlobalLock(AData);
      Move((PChar(MemoryStream.Memory) + SizeOf (TBitmapFileHeader))^, LockPointer^, Size);
      GlobalUnlock(AData);
      clipboard.SetAsHandle(CF_DIB, AData);
    except
      GlobalFree (AData);
      raise;
    end;
  finally
    Bitmap.Free;
    MemoryStream.Free;
  end;
end;

constructor TBitmapEditor.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  FSelectionRect.Right := -1;
  FSelectionRect.Bottom := -1;
  FPicture := TPicture.Create;
  FMagnification := 4;
  FGridLines := 4;
  FBorderStyle := bsSingle;
  FDrawBrush := TBrush.Create;
  FDrawPen := TPen.Create;
  FEraser := TPen.Create;
  FHotSpotX := -1;
  FHotSpotY := -1;

  FDrawBmp := TBitmap.Create;

  FScratchBmp := TBitmap.Create;
  FSelectionBmp := TBitmap.Create;
  FTransparentColor := clTeal;
  Screen.Cursors [crPencil] := LoadCursor (HInstance, 'CR_PENCIL');
  Screen.Cursors [crPotOPaint] := LoadCursor (HInstance, 'CR_POTOPAINT');
  Screen.Cursors [crMagnifier] := LoadCursor (HInstance, 'CR_MAGNIFIER');
  Screen.Cursors [crDotCross] := LoadCursor (HInstance, 'CR_DOTCROSS');
  Screen.Cursors [crDropper] := LoadCursor (HInstance, 'CR_DROPPER');
  Screen.Cursors [crAirbrush] := LoadCursor (HInstance, 'CR_SPRAYGUN');
  Width := 32 * 4;
  Height := 32 * 4;
  Cursor := crArrow
end;

procedure TBitmapEditor.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams (params);

  if BorderStyle = bsSingle then
    params.Style := params.Style or WS_BORDER;
end;

procedure TBitmapEditor.CutSelection;
begin
  CopySelection;
  DeleteSelection;
end;

procedure TBitmapEditor.DeleteSelection;
var
  hrgn: THandle;
  Brush: TBrush;
begin
  Brush := nil;
  hrgn := CreateRectRgn (FSelectionRect.Left, FSelectionRect.Top, FSelectionRect.Right, FSelectionRect.Bottom);
  if hrgn = 0 then
    RaiseLastOSError;

  try
    Brush := TBrush.Create;
    Brush.Color := TransparentColor;
    FillRgn (DrawBmp.Canvas.Handle, hrgn, Brush.Handle);
  finally
    Brush.Free;
    DeleteObject(hrgn);
  end;
  RedrawBitmap;
end;

destructor TBitmapEditor.Destroy;
begin
  FPicture.Free;
  FDrawPen.Free;
  FDrawBrush.Free;
  FDrawBmp.Free;
  FScratchBmp.Free;
  FSelectionBmp.Free;
  FEraser.Free;
  inherited
end;

procedure TBitmapEditor.DisplayCrossHairs;
var
  pt: TPoint;
  oldColor: TColor;
  oldMode: TPenMode;
begin
  GetCursorPos (pt);
  MapWindowPoints (HWND_DESKTOP, handle, pt, 1);
  pt.x := pt.x div Magnification;
  pt.y := pt.y div Magnification;

  if (pt.x <> FCrossX) or (pt.y <> FCrossY) then
  begin
    oldColor := Canvas.Pen.Color;
    oldMode := Canvas.Pen.Mode;
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := Magnification;
    Canvas.Pen.Mode := pmNotXor;

    try
      if pt.x <> FCrossX then
      begin
        FCrossX := pt.x;
        Canvas.MoveTo (FCrossX * Magnification + Magnification div 2, 0);
        Canvas.LineTo (FCrossX * Magnification + Magnification div 2, ClientHeight)
      end;
      if pt.y <> FCrossY then
      begin
        FCrossY := pt.y;
        Canvas.MoveTo (0, FCrossY * Magnification + Magnification div 2);
        Canvas.LineTo (ClientWidth, FCrossY * Magnification + Magnification div 2)
      end;
    finally
      Canvas.Pen.Color := oldColor;
      Canvas.Pen.Width := 1;
      Canvas.Pen.Mode := oldMode;
    end
  end
end;

function TBitmapEditor.GetDrawingChangeDescription: string;
resourcestring
  rstFreeDraw       = 'freehand drawing';
  rstLine           = 'line';
  rstFrameRect      = 'frame';
  rstFillRect       = 'filled frame';
  rstRect           = 'rectangle';
  rstFloodFill      = 'flood fill';
  rstFrameRoundRect = 'rounded frame';
  rstFillRoundRect  = 'filled rounded frame';
  rstRoundRect      = 'rounded rectangle';
  rstFrameEllipse   = 'elliptical frame';
  rstFillEllipse    = 'filled ellipse';
  rstEllipse        = 'ellipse';
  rstBrushStroke    = 'brush stroke';
  rstText           = 'text';
  rstEraser         = 'eraser';
  rstAirbrush       = 'airbrush stroke';
  rstGradientRect   = 'gradient rectangle';

const
  DrawingToolDescription: array [TDrawingTool] of string = (
    '', rstFreeDraw, rstLine,
    rstFrameRect, rstFillRect, rstRect,
    rstFloodFill,
    rstFrameRoundRect, rstFillRoundRect, rstRoundRect,
    rstFrameEllipse, rstFillEllipse, rstEllipse,
    '',
    rstBrushStroke,
    '', '', '', rstText, rstEraser, rstAirbrush,
    rstGradientRect, rstGradientRect, rstGradientRect, rstGradientRect);
begin
  Result := DrawingToolDescription [DrawingTool]
end;

function TBitmapEditor.GetSelectionValid: Boolean;
begin
  Result := (FSelectionRect.Right <> -1) and (FSelectionRect.Bottom <> -1);
end;

procedure TBitmapEditor.Initialize;
var
  Rct: TRect;
begin
  if Assigned(FPicture.Graphic) then
  begin
    FDrawBmp.Assign (FPicture.Graphic);
    if FPicture.Graphic is TGifImage then
      FDrawBmp.Transparent := FPicture.Graphic.Transparent;
    if FDrawBmp.Transparent then
      FTransparentColor := FDrawBmp.TransparentColor
  end
  else
  begin
    Rct.Left := 0;
    Rct.top := 0;
    Rct.right := 32;
    Rct.bottom := 32;
    FDrawBmp.TransparentColor := TransparentColor;
    FDrawBmp.Width := Rct.right;
    FDrawBmp.Height := Rct.bottom;
    FDrawBmp.Canvas.pen.Color := clWhite;
    FDrawBmp.Canvas.FillRect(Rct)
  end;

  ChangeSelectionRect(Rect(-1, -1, -1, -1));

  ClientWidth := FDrawBmp.Width * Magnification;
  ClientHeight := FDrawBmp.Height * Magnification;

  if FMouseCaptured then
  begin
    ReleaseCapture;
    FMouseCaptured := False
  end
end;

procedure TBitmapEditor.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LockPointer: TPoint;
  st: WideString;
begin
  SetFocus;
  FCallEndChange := DrawingTool in [dtPencil..dtEllipse, dtBrush, dtText, dtEraser, dtAirbrush, dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR];
  LockPointer.x := x div magnification;
  LockPointer.y := y div magnification;
  FScratchBmp.Assign (FDrawBmp);

  if Button = mbLeft then
  begin
    FLButtonIsDown := True;
    FPos := LockPointer;

    case DrawingTool of
      dtPencil, dtLine:
        begin
          FDrawBmp.Canvas.Pixels [LockPointer.x, LockPointer.y] := FDrawPen.Color;
          RedrawBitmap
        end;
      dtFloodFill:
        begin
          with FDrawBmp.Canvas do
          begin
            Brush := FDrawBrush;
            FloodFill (LockPointer.x, LockPointer.y, Pixels [LockPointer.x, LockPointer.y], fsSurface);
          end;
          RedrawBitmap
        end;
      dtMagnifier:
        ZoomIn;

      dtDropper:
        begin
          FDrawPen.Color := FDrawBmp.Canvas.Pixels [LockPointer.x, LockPointer.y];
          DrawingTool := FLastDrawingTool;
          ReleaseCapture;
          if Assigned(FOnDrawToolChange) and not(csDestroying in ComponentState) then
            OnDrawToolChange (self);
        end;

      dtSelectRect,
      dtSelectArea:
          ChangeSelectionRect(Rect(LockPointer.x, LockPointer.y, -1, -1));
      dtEraser:
        begin
          FEraser.Color := TransparentColor;
          FDrawBmp.Canvas.Pixels [LockPointer.x, LockPointer.y] := FEraser.Color;
          RedrawBitmap
        end;
      dtText:
        if Assigned(OnGetText) then
        begin
          FDrawBmp.Canvas.Font.Color := FDrawPen.Color;
          OnGetText(self, FDrawBmp.Canvas.Font, st);
          if st <> '' then
          begin
            SetBkMode (FDrawBmp.Canvas.Handle, TRANSPARENT);
            ExtTextOutW (FDrawBmp.Canvas.Handle, LockPointer.x, LockPointer.y, 0, nil, PWideChar (st), Length (st), nil);
            RedrawBitmap;
            if FCallEndChange then
            begin
              FCallEndChange := False;
              Picture.Graphic.Assign (drawBmp);
              if Assigned(OnEndChange) then
                OnEndChange (self)
            end
          end
        end
    end
  end
  else
    if Button = mbRight then
      case DrawingTool of
        dtMagnifier: ZoomOut;
        dtDropper:
          begin
            FDrawBrush.Color := FDrawBmp.Canvas.Pixels [LockPointer.x, LockPointer.y];
            DrawingTool := FLastDrawingTool;
            ReleaseCapture;
            if Assigned(FOnDrawToolChange) and not(csDestroying in ComponentState) then
              OnDrawToolChange (self);
          end
        end

end;

procedure TBitmapEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LockPointer: TPoint;
  hrgn: THandle;
  Rct: TRect;
  inRect: Boolean;
  gradType: TGradientShapeRectType;
begin
  LockPointer.x := x;
  LockPointer.y := y;
  ScreenToClient(LockPointer);
  if Parent is TScrollingWinControl then
  begin
    Rct := Parent.ClientRect;
    MapWindowPoints (Parent.Handle, handle, Rct, 2);
    IntersectRect(Rct, Rct, ClientRect)
  end
  else
    Rct := ClientRect;

  inRect := PtInRect(Rct, LockPointer);
  if inRect  or (ssLeft in Shift) then
  begin
    LockPointer.x := LockPointer.x div magnification;
    LockPointer.y := LockPointer.y div magnification;
    if inRect and not FMouseCaptured then
    begin
      SetCapture (handle);
      FMouseCaptured := True
    end;

    if not InRect and (DrawingTool in [dtSelectRect, dtSelectArea]) then
    begin
      if LockPointer.x >= drawBmp.Width then LockPointer.x := drawBmp.Width - 1;
      if LockPointer.y >= drawBmp.Height then LockPointer.y := drawBmp.Height - 1;
      ChangeSelectionRect(Rect(-2, -2, LockPointer.x, LockPointer.y));
    end
    else
    begin
      if (LockPointer.x <> FPos.x) or (LockPointer.y <> FPos.y) then
        if FLButtonIsDown then
        begin
          case DrawingTool of
            dtPencil :
              with FDrawBmp.Canvas do
                begin
                  Pen := FDrawPen;
                  MoveTo (FPos.x, FPos.y);
                  LineTo (LockPointer.x, LockPointer.y);
                  Pixels [LockPointer.x, LockPointer.y] := FDrawPen.Color;
                  FPos := LockPointer;
                end;

            dtLine :
              begin
                FDrawBmp.Assign (FScratchBmp);
                with FDrawBmp.Canvas do
                begin
                  Pen := FDrawPen;
                  MoveTo (FPos.x, FPos.y);
                  LineTo (LockPointer.x, LockPointer.y);
                  Pixels [LockPointer.x, LockPointer.y] := FDrawPen.Color;
                end
              end;
            dtFrameRect, dtFillRect, dtRect,
            dtFrameEllipse, dtFillEllipse, dtEllipse,
            dtFrameRoundRect, dtFillRoundRect, dtRoundRect,
            dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR :
              begin
                FDrawBmp.Assign (FScratchBmp);
                case DrawingTool of
                  dtFrameRect, dtFillRect, dtRect: 
                    hrgn := CreateRectRgn (FPos.x, FPos.y, LockPointer.x + 1, LockPointer.y + 1);
                  dtFrameEllipse, dtFillEllipse, dtEllipse: 
                    hrgn := CreateEllipticRgn (FPos.x, FPos.y, LockPointer.x + 1, LockPointer.y + 1);
                  dtFrameRoundRect, dtFillRoundRect, dtRoundRect: 
                    hrgn := CreateRoundRectRgn (FPos.x, FPos.y, LockPointer.x + 1, LockPointer.y + 1, 5, 5);
                  else
                    hrgn := 0;
                end;
                if hrgn <> 0 then
                  with FDrawBmp.Canvas do
                  try
                    case DrawingTool of
                      dtRect,
                      dtEllipse,
                      dtRoundRect  :
                        FillRgn (handle, hrgn, DrawBrush.Handle);

                      dtFillRect,
                      dtFillEllipse,
                      dtFillRoundRect  :
                        begin
                          FillRgn (handle, hrgn, DrawBrush.Handle);
                          Brush.Color := DrawPen.Color;
                          FrameRgn (handle, hrgn, brush.Handle, 1, 1)
                        end;

                      dtFrameRect,
                      dtFrameEllipse,
                      dtFrameRoundRect :
                        begin
                          Brush.Color := DrawPen.Color;
                          FrameRgn (handle, hrgn, brush.Handle, 1, 1);
                        end
                    end
                  finally
                    DeleteObject(hrgn)
                  end
                else
                  case DrawingTool of
                    dtGradRectLR, dtGradRectTB, dtGradRectTLBR, dtGradRectBLTR :
                      begin
                        case DrawingTool of
                          dtGradRectLR: gradType := gsrLR;
                          dtGradRectTB: gradType := gsrTB;
                          dtGradRectTLBR: gradType := gsrTLBR;
                          dtGradRectBLTR: gradType := gsrBLTR;
                          else
                            gradType := gsrLR
                        end;
                        GradientRect(FDrawBmp.Canvas.Handle, gradType, FPos.x, FPos.y, LockPointer.x + 1, LockPointer.y + 1, DrawPen.Color, DrawBrush.Color);
                      end
                  end
              end;
            dtSelectRect,
            dtSelectArea :
              ChangeSelectionRect(Rect(-2, -2, LockPointer.x, LockPointer.y));
            dtEraser :
              with FDrawBmp.Canvas do
              begin
                Pen := FEraser;
                MoveTo (FPos.x, FPos.y);
                LineTo (LockPointer.x, LockPointer.y);
                Pixels [LockPointer.x, LockPointer.y] := FEraser.Color;
                FPos := LockPointer;
              end;
          end;

          if not(DrawingTool in [dtSelectRect, dtSelectArea]) then
            RedrawBitmap
          else
            Invalidate
        end;

        if Cursor = crNone then
          Invalidate
      end
  end
  else
  begin  // Pt not in Rect, and not LButtonDown
    ReleaseCapture;
    FMouseCaptured := False;
    Invalidate
  end
end;

procedure TBitmapEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  LockPointer: TPoint;
begin
  if button = mbLeft then
    FLButtonIsDown := False;

  LockPointer.x := x;
  LockPointer.y := y;
  ScreenToClient(LockPointer);

  if PtInRect(ClientRect, LockPointer) then
  begin
    SetCapture (handle);
    FMouseCaptured := True
  end;

  if FCallEndChange then
  begin
    FCallEndChange := False;
    Picture.Graphic.Assign (drawBmp);
    if Assigned(OnEndChange) then
      OnEndChange (self)
  end
end;

procedure TBitmapEditor.DrawHotSpot(canvas: TCanvas);
var
  hsx, hsy: Integer;
begin
  if (FHotSpotX <> -1) and (FHotSpotY <> -1) then
  begin
    hsx := FHotSpotX * Magnification;
    hsy := FHotSpotY * Magnification;
    if Magnification = 1 then
      Canvas.Pixels [hsx, hsy] := clLime
    else
    begin
      Canvas.Brush.Color := clLime;
      Canvas.FillRect(Rect(hsx, hsy, hsx+Magnification-1, hsy + Magnification-1))
    end
  end
end;

procedure TBitmapEditor.Paint;
var
  x: Integer;
  Rct: TRect;
  pts: array [0..4] of TPoint;
begin
  PaintBitmap (FDrawBmp);

  if GridLines <= Magnification then
  begin
    x := Magnification - 1;
    while x < Width - 1 do
    begin
      Canvas.MoveTo (x, 0);
      Canvas.LineTo (x, Height);
      Inc(x, Magnification)
    end;

    x := Magnification - 1;
    while x < Height - 1do
    begin
      Canvas.MoveTo (0, x);
      Canvas.LineTo (Width, x);
      Inc(x, Magnification)
    end
  end;

  FCrossX := -1;
  FCrossY := -1;

  if SelectionValid then
    with FSelectionRect do
    begin
      Rct.left := left * Magnification + Magnification div 2;
      Rct.top := top * Magnification + Magnification div 2;
      Rct.right := right * Magnification + Magnification div 2;
      Rct.bottom := bottom * Magnification + Magnification div 2;

      pts [0] := Rct.TopLeft;
      pts [1].x := Rct.Right;
      pts [1].y := Rct.Top;
      pts [2] := Rct.BottomRight;
      pts [3].x := Rct.Left;
      pts [3].y := Rct.Bottom;
      pts [4] := pts [0];

      Canvas.Pen.Width := Magnification;
      Canvas.Pen.Mode := pmNotXOR;
      Canvas.PolyLine (pts);
      Canvas.Pen.Width := 1;
      Canvas.Pen.Mode := pmCopy;
    end;

  if FMouseCaptured and (Cursor = crNone) and not FLButtonIsDown then
    DisplayCrossHairs;

  DrawHotspot(Canvas);
end;

procedure TBitmapEditor.PaintBitmap (bmp: TBitmap);
begin
  if bmp.Transparent then
  begin
    Canvas.Brush.Color := TransparentColor;
    Canvas.FillRect(ClientRect);
  end;
  Canvas.StretchDraw (ClientRect, bmp);
end;

procedure TBitmapEditor.PasteSelection;
var
  b: TBitmap;
  s: TMemoryStream;
  f: TBitmapFileHeader;
  Data: THandle;
  LockPointer: PChar;
  Size: Integer;
  Rct: TRect;
begin
  s := nil;
  b := TBitmap.Create;
  try
    data := clipboard.GetAsHandle(CF_DIB);
    if data = 0 then
      RaiseLastOSError;

    s := TMemoryStream.Create;

    FillChar(f, sizeof(f), 0);
    f.bfType := $4D42;

    s.Write (f, SizeOf (f));

    Size := GlobalSize (data);
    LockPointer := GlobalLock (data);
    try
      s.Write (LockPointer^, Size);
    finally
      GlobalUnlock (data)
    end;
    s.Seek (0, TSeekOrigin.soBeginning);
    b.LoadFromStream (s);

    b.Palette := FClipboardPalette;
    b.PixelFormat := FClipboardPixelFormat;

    Rct := FSelectionRect;
    Inc(Rct.Right);
    Inc(Rct.Bottom);

    FDrawBmp.Canvas.StretchDraw (Rct, b);
    Invalidate;
    Picture.Graphic.Assign (drawBmp);
    if Assigned(OnEndChange) then
      OnEndChange (self)
  finally
    b.Free;
    s.Free
  end
end;

procedure TBitmapEditor.PictureChanged;
begin
  Initialize;
  Invalidate;
end;

procedure TBitmapEditor.RedrawBitmap;
begin
  Invalidate;
  if not(csDestroying in ComponentState) and Assigned(OnChange) then
    OnChange (self);
end;

procedure TBitmapEditor.Rotate180;
var
  destRect, srcRect: TRect;
begin
  with FDrawBmp do
  begin
    srcRect := Rect(0, 0, Width, Height);

      // Should technically be Width-1, Height-1,
      // But work round a bug in CopyRect...

    destRect := srcRect;
    destRect.Left := srcRect.Right;
    destRect.Top := srcRect.Bottom;
    destRect.Right := 0;
    destRect.Bottom := 0;
    Canvas.CopyRect(destRect, Canvas, srcRect);
    RedrawBitmap
  end

end;

procedure TBitmapEditor.Rotate270;
var
  newBmp: TBitmap;
begin
  FDrawBmp.PixelFormat := pf24Bit;
  newBmp := RotateBitmap270 (FDrawBmp);
  FDrawBmp.Free;

  newBmp.PixelFormat := pfDevice;
  FDrawBmp := NewBmp;
  ClientWidth := FDrawBmp.Width * Magnification;
  ClientHeight := FDrawBmp.Height * Magnification;
  RedrawBitmap
end;

procedure TBitmapEditor.Rotate90;
var
  newBmp: TBitmap;
begin
  FDrawBmp.PixelFormat := pf24Bit;
  newBmp := RotateBitmap90 (FDrawBmp);
  FDrawBmp.Free;

  newBmp.PixelFormat := pfDevice;
  FDrawBmp := NewBmp;
  ClientWidth := FDrawBmp.Width * Magnification;
  ClientHeight := FDrawBmp.Height * Magnification;
  RedrawBitmap
end;

procedure TBitmapEditor.SelectAll;
begin
  ChangeSelectionRect(Rect(0, 0,drawBmp.Width - 1, drawBmp.Height - 1));
end;

procedure TBitmapEditor.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> value then
  begin
    FBorderStyle := Value;
    RecreateWnd
  end
end;

procedure TBitmapEditor.SetDrawBrush(const Value: TBrush);
begin
  FDrawBrush.Assign (value)
end;

procedure TBitmapEditor.SetDrawingTool(const Value: TDrawingTool);
begin
  if value <> FDrawingTool then
  begin
    FLastDrawingTool := FDrawingTool;
    FDrawingTool := Value;
    Cursor := drawingCursors [FDrawingTool];
    if SelectionValid then
      ChangeSelectionRect(Rect(-1, -1, -1, -1));
  end
end;

procedure TBitmapEditor.SetDrawPen(const Value: TPen);
begin
  FDrawPen.Assign (value)
end;

procedure TBitmapEditor.SetGridLines(const Value: Integer);
begin
  if FGridLines <> value then
  begin
    FGridLines := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetHotSpotX(const Value: Integer);
begin
  if Value <> FHotSpotX then
  begin
    FHotSpotX := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetHotSpotY(const Value: Integer);
begin
  if Value <> FHotSpotY then
  begin
    FHotSpotY := Value;
    Invalidate
  end
end;

procedure TBitmapEditor.SetMagnification(const Value: Integer);
begin
  if FMagnification <> value then
  begin
    FMagnification := Value;
    SizeToPicture;
    Invalidate
  end
end;

procedure TBitmapEditor.SetPicture(const Value: TPicture);
begin
  FPicture.Assign (Value);
  if Value.Graphic is TGifImage then
    FPicture.Graphic.Transparent := Value.Graphic.Transparent;
  
  PictureChanged;
end;

procedure TBitmapEditor.SetTransparentColor(const Value: TColor);
begin
  if (value <> FTransparentColor) then
  begin
    FTransparentColor := value;
    Initialize;
    Invalidate;
  end
end;

procedure TBitmapEditor.SizeToPicture;
begin
  if Assigned(FDrawBmp) and (FDrawBmp.Width > 0) then
  begin
    ClientWidth := FDrawBmp.Width * Magnification;
    ClientHeight := FDrawBmp.Height * Magnification
  end
  else
  if FPicture.Width = 0 then
  begin
    ClientWidth := 32 * Magnification;
    ClientHeight := 32 * Magnification
  end
  else
  begin
    ClientWidth := FPicture.Width * Magnification;
    ClientHeight := FPicture.Height * Magnification;
  end;
end;

procedure TBitmapEditor.ZoomIn;
begin
  if Magnification = 1 then
    Magnification := 2
  else
    if Magnification < 32 then
      Magnification := Magnification + 2;
end;

procedure TBitmapEditor.ZoomOut;
begin
  if Magnification = 2 then
    Magnification := 1
  else
    if Magnification > 1 then
      Magnification := Magnification - 2;
end;

end.
