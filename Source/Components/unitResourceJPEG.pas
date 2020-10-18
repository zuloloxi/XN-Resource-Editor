(*======================================================================*
 | unitResourceJPEG                                                     |
 |                                                                      |
 | Encapsulates JPEG images in custom (non-RC data) resources.          |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      14/06/2001  CPWW  Original                                  |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)

unit unitResourceJPEG;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, Vcl.Graphics,
  Vcl.Imaging.jpeg, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// Jpeg resource details class

  TJpegResourceDetails = class (TGraphicsResourceDetails)
  protected
    FWidth, FHeight: Integer;
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    class function SupportsData (Size: Integer; data: Pointer): Boolean; override;
  public
    class function GetBaseType: UnicodeString; override;
    procedure GetImage (picture: TPicture); override;
    procedure SetImage (image: TPicture); override;
  end;

implementation

var
  ParameterlessSegments: AnsiString = #$01#$d0#$d1#$d2#$d3#$d4#$d5#$d6#$d7#$d8#$d9;

function FindJpegSegment(var data: PAnsiChar; segment: byte): Boolean;
var
  p: PAnsiChar;
  seg: Byte;
  len: Word;
begin
  p := data;
  Result := False;

  repeat
    if p^ <> #$ff then
      raise Exception.Create ('Invalid JPEG Image');

    Inc(p);

    seg := Byte (p^);

    if seg <> $ff then
    begin
      Inc(p);

      if seg = segment then
      begin
        Result := True;
        data := p;
        break
      end;

      if seg = $d9 then // end of image
        break;

      if Pos (AnsiChar (seg), ParameterlessSegments) = 0 then
      begin
        len := 256 * Byte (p^) + Byte ((p + 1)^);
        Inc(p, len)
      end;
    end;
  until False;
end;

procedure GetJpegSize(data: PAnsiChar; var Width, Height: Integer);
var
  len: Integer;
begin
  if FindJpegSegment(data, $c0) then
  begin
    len := 256 * Byte (data^) + Byte ((data + 1)^);

    if len > 5 then
    begin
      Inc(data, 3);  // Skip len Word & precision byte
      Height := 256 * Byte (data^) + Byte ((data + 1)^);

      Inc(data, 2);
      Width := 256 * Byte (data^) + Byte ((data + 1)^);
    end;
  end;
end;

{ TJpegResourceDetails }

class function TJpegResourceDetails.GetBaseType: UnicodeString;
begin
  Result := 'JPEG';
end;

function TJpegResourceDetails.GetHeight: Integer;
begin
  if FHeight = 0 then
    GetJpegSize (data.Memory, FWidth, FHeight);

  Result := FHeight;
end;

procedure TJpegResourceDetails.GetImage(picture: TPicture);
begin
  picture.graphic := TJpegImage.Create;
  data.Seek (0, TSeekOrigin.soBeginning);
  TJpegImage (picture.graphic).LoadFromStream (data);
  FWidth := picture.graphic.Width;
  FHeight := picture.graphic.Height;
end;

function TJpegResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf24Bit;
end;

function TJpegResourceDetails.GetWidth: Integer;
begin
  if FWidth = 0 then
    GetJpegSize (data.Memory, FWidth, FHeight);
  Result := FWidth;
end;

procedure TJpegResourceDetails.InitNew;
var
  img: TJpegImage;
  bmp: TBitmap;
begin
  bmp := nil;
  img := TJpegImage.Create;
  try
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    img.Assign(bmp);
    img.SaveToStream (data);
  finally
    img.Free;
    bmp.Free;
  end;
end;

procedure TJpegResourceDetails.SetImage(image: TPicture);
begin
  inherited;
  FWidth := image.Width;
  FHeight := image.Height;
end;

class function TJpegResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  len: Integer;
begin
  Result := False;
  if PWORD (data)^ = $d8ff then
    if FindJpegSegment(PAnsiChar (data), $e0) then
    begin
      len := 256 * Byte (PAnsiChar (data)^) + Byte ((PAnsiChar (data) + 1)^);

      if len >= 16 then
      begin
        Inc(PAnsiChar (data), 2);

        if StrLIComp (data, 'JFIF', 4) = 0 then
          Result := True;
      end;
    end;
end;

initialization
  RegisterResourceDetails (TJpegResourceDetails);
finalization
  UnRegisterResourceDetails (TJpegResourceDetails);
end.
