(*======================================================================*
 | unitResourceGIF                                                      |
 |                                                                      |
 | Encapsulates GIF image resources in resources                        |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001,2008                                 |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      06/02/2001  CPWW  Original                                  |
 |          16/5/2008   CPWW  Tiburon version                           |
 *======================================================================*)
unit unitResourceGIF;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, System.AnsiStrings, Vcl.Graphics,
  Vcl.Imaging.GIFImg, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// GIF resource details class

  TGifResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    procedure InitNew; override;
    class function SupportsData (Size: Integer; data: Pointer): Boolean; override;
  public
    class function GetBaseType: UnicodeString; override;
    procedure GetImage (picture: TPicture); override;
  end;


implementation

{ TGifResourceDetails }

class function TGifResourceDetails.GetBaseType: UnicodeString;
begin
  Result := 'GIF';
end;

function TGifResourceDetails.GetHeight: Integer;
begin
  Result := PWORD(PByte(Data) + 6 + SizeOf(Word))^;
end;

procedure TGifResourceDetails.GetImage(picture: TPicture);
var
  gif: TGIFImage;
begin
  Gif := TGifImage.Create;
  Picture.Graphic := Gif;
  Data.Seek (0, TSeekOrigin.soBeginning);
  TGifImage(Picture.Graphic).LoadFromStream (data);
end;

function TGifResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TGifResourceDetails.GetWidth: Integer;
begin
  Result := PWORD(PByte(data) + 6)^;
end;

procedure TGifResourceDetails.InitNew;
var
  img: TGIFImage;
  bmp: TBitmap;
begin
  bmp := nil;
  img := TGIFImage.Create;
  try
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    img.Assign(bmp);
    img.Transparent := True;
    img.SaveToStream (data);
  finally
    img.Free;
    bmp.Free
  end
end;

class function TGifResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p: PAnsiChar;
begin
  p := PAnsiChar(data);

  Result := (System.AnsiStrings.StrLIComp (p, 'GIF87', 5) = 0) or (System.AnsiStrings.StrLIComp (p, 'GIF89', 5) = 0);
end;

initialization
  RegisterResourceDetails (TGIFResourceDetails);
finalization
  UnregisterResourceDetails (TGIFResourceDetails);
end.
