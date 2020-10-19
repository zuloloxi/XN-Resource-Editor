(*======================================================================*
 | ComponentExSplitter                                                  |
 |                                                                      |
 | Splitters with 'minimize' arrows.                                    |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      15/11/2001  CPWW  Original                                  |
 *======================================================================*)

unit ComponentExSplitter;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, Vcl.Forms, System.Classes,
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics;

type
  TExSplitterArrow = (arLeft, arRight, arUp, arDown);
  TExSplitter = class(TSplitter)
  private
    FShrinkButton: Boolean;
    FShrunken: Boolean;
    FOrigCursor: TCursor;
    FOldControlSize: Integer;
    FSaveSize: Integer;
    procedure SetShrinkButton(const Value: Boolean);
    function GetShrinkButtonRect: TRect;
    function GetRequiredArrow: TExSplitterArrow;
    procedure SetShrunken(const Value: Boolean);
    function GetResizeControl: TControl;
    function GetResizeControlSize: Integer;
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property ResizeControl: TControl read GetResizeControl;
    procedure StopSizing; override;
  public
    constructor Create (AOwner: TComponent); override;
    property Shrunken: Boolean read FShrunken write SetShrunken;
    property ResizeControlSize: Integer read GetResizeControlSize;
  published
    property ShrinkButton: Boolean read FShrinkButton write SetShrinkButton default True;
  end;


implementation

{ TExSplitter }

(*----------------------------------------------------------------------*
 | TExSplitter.Create                                                   |
 |                                                                      |
 | Constructor for TExSplitter                                          |
 |                                                                      |
 | Parameters:                                                          |
 |   AOwner: TComponent                                                 |
 *----------------------------------------------------------------------*)
constructor TExSplitter.Create(AOwner: TComponent);
begin
  inherited;
  FShrinkButton := True;
end;

(*----------------------------------------------------------------------*
 | TExSplitter.GetRequiredArrow                                         |
 |                                                                      |
 | Get the appropriate arow for the splitter state.                     |
 |                                                                      |
 |                                                                      |
 | The function returns the appropriate arrow for the state.            |
 *----------------------------------------------------------------------*)


function TExSplitter.GetRequiredArrow: TExSplitterArrow;
begin
  Result := arLeft;
  case Align of
    alLeft: 
      if Shrunken then Result := arRight else Result := arLeft;
    alRight: 
      if Shrunken then Result := arLeft  else Result := arRight;
    alTop: 
      if Shrunken then Result := arDown  else Result := arUp;
    alBottom: 
      if Shrunken then Result := arUp    else Result := arDown
  end
end;

(*----------------------------------------------------------------------*
 | TExSplitter.GetResizeControl                                         |
 |                                                                      |
 | Get the control that's gonna be resized by clicking the arrow.       |
 | TSplitter doesn't provide a non-private way of doing this, but we    |
 | can deduce it by looking for a control with the same parent and      |
 | align as ourself.                                                    |
 |
 | The function returns the control that's resized by the splitter.     |
 *----------------------------------------------------------------------*)
function TExSplitter.GetResizeControl: TControl;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Parent.ControlCount - 1 do
    if (Parent.Controls [i] <> Self) and (Parent.Controls [i].Align = Align) then
    begin
      Result := Parent.Controls [i];
      break
    end
end;

(*----------------------------------------------------------------------*
 | TExSplitter.GetResizeControlSize                                     |
 |                                                                      |
 | Get the size of that the resize control would have if it wasn't      |
 | shrunken.  Handy for saving form layout - you don't want to save '0' |
 | as the width if it's been minimized                                  |
 |                                                                      |
 | The function returns the true width or height of the resize control  |
 *----------------------------------------------------------------------*)
function TExSplitter.GetResizeControlSize: Integer;
var
  ctrl: TControl;
begin
  if Shrunken then
    Result := FOldControlSize
  else
  begin
    ctrl := ResizeControl;
    if Assigned(ctrl) then
      if Align in [alLeft, alRight] then
        Result := ctrl.Width
      else
        Result := ctrl.Height
    else
      Result := 0
  end
end;

(*----------------------------------------------------------------------*
 | TExSplitter.GetShrinkButtonRect                                      |
 |                                                                      |
 | Get the rect for the arrow.  Note that this is a bit bigger than our |
 | width because Windows draws it with a border.                        |
 |                                                                      |
 |                                                                      |
 | The function returns the rect required to draw the arrow             |
 *----------------------------------------------------------------------*)
function TExSplitter.GetShrinkButtonRect: TRect;
var
  tl: TPoint;
  bs: Integer;

begin
  bs := 0;
  if Align in [alLeft, alRight] then
  begin
    bs := ClientWidth + 8;
    tl := Point(-4, (ClientHeight - bs) div 2);
    if not FShrunken then
      Inc(tl.X);
  end
  else
    if Align in [alTop, alBottom] then
     begin
       bs := ClientHeight + 8;
       tl := Point((ClientWidth - bs) div 2, -4);
       if not FShrunken then
         Inc(tl.y);
     end;

  Result := rect(tl.X, tl.Y, tl.x + bs - 1, tl.Y + bs - 1);
end;

procedure TExSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin

  if PtInRect(GetShrinkButtonRect, Point(x, y)) then
    Shrunken := not Shrunken
  else
  begin
    if Assigned(ResizeControl) then
      if Align in [alLeft, alRight] then
        FSaveSize := ResizeControl.Width
      else
        FSaveSize := ResizeControl.Height;
    inherited
  end
end;

procedure TExSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if PtInRect(GetShrinkButtonRect, Point(x, y)) then
  begin
    if Cursor <> crArrow then
    begin
      FOrigCursor := Cursor;
      Cursor := crArrow
    end
  end
  else
    if FOrigCursor <> crDefault then
      Cursor := FOrigCursor
end;

procedure TExSplitter.Paint;
var
  rect: TRect;
  requiredArrow: TExSplitterArrow;
  dfcs: DWORD;
begin
  inherited;

  rect := GetShrinkButtonRect;
  requiredArrow := GetRequiredArrow;

  if ShrinkButton then
  begin
    case requiredArrow of
      arLeft: dfcs := DFCS_SCROLLLEFT;
      arRight: dfcs := DFCS_SCROLLRIGHT;
      arUp: dfcs := DFCS_SCROLLUP;
      arDown: dfcs := DFCS_SCROLLDOWN;
      else
        dfcs := 0;
    end;
    DrawFrameControl (Canvas.Handle, rect, DFC_SCROLL, dfcs or DFCS_FLAT or DFCS_INACTIVE or DFCS_TRANSPARENT);
    FrameRect(Canvas.Handle, rect, Canvas.Brush.Handle)
  end
end;

procedure TExSplitter.SetShrinkButton(const Value: Boolean);
begin
  FShrinkButton := Value;
end;

procedure TExSplitter.SetShrunken(const Value: Boolean);
var
  ctrl: TControl;
begin
  if Value <> FShrunken then
  begin
    FShrunken := Value;
    Invalidate;

    ctrl := ResizeControl;

    if Assigned(ctrl) then
    begin
      if Value then
        if Align in [alLeft, alRight] then
        begin
          FOldControlSize := ctrl.Width;
          ctrl.Width := 1  // Set it to 1 - not 0.  There's a bug...
        end
        else
        begin
          FOldControlSize := ctrl.Height;
          ctrl.Height := 1
        end
      else
      begin
        if ctrl.Align in [alLeft, alRight] then
          ctrl.Width := FOldControlSize
        else
          ctrl.Height := FOldControlSize;
      end
    end;

    if Assigned(OnMoved) then
      OnMoved(Self);
  end
end;

procedure TExSplitter.StopSizing;
var
  Resized: Boolean;
begin
  inherited;

  Resized := False;
  if Assigned(ResizeControl) then
    if Align in [alLeft, alRight] then
      Resized := ResizeControl.Width <> FSaveSize
    else
      Resized := ResizeControl.Height <> FSaveSize;

  if Resized then
  begin
    FShrunken := False;
    Invalidate;
  end
end;

end.
