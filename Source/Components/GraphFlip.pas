(*======================================================================*
 | GraphFlip unit                                                       |
 |                                                                      |
 | Simple functions to flip and rotate 24-bit bitmaps                   |
 |                                                                      |
 | Not that these functions *create* copies of the Bitmap which must be |
 | freed.                                                               |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2002  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      29/08/2002  CPWW  Original                                  |
 | 10.0     08/03/2006  CPWW  Tidied up for BDS 2006                    |
 | 12.0     09/06/2008  CPWW  Tiburonized                               |
 *======================================================================*)
unit GraphFlip;

interface

uses
  WinAPI.Windows, System.Classes, System.Sysutils, Vcl.Graphics;

function RotateBitmap270(const Bitmap: TBitmap): TBitmap;
function RotateBitmap90(const Bitmap: TBitmap): TBitmap;
function ConvertToGrayscale(const Bitmap: TBitmap; TransparentColor: TColor = clNone): TBitmap;
function ConvertToNegative(const Bitmap: TBitmap): TBitmap;

implementation

(*----------------------------------------------------------------------*
 | function BytesPerScanLine: LongInt                                  |
 |                                                                      |
 | Returns the bytes required per scanline                              |
 |                                                                      |
 | Parameters:                                                          |
 |   PixelsPerScanline: LongInt     The width of the Bitmap in pixels  |
 |   BitsPerPixel                    No. of bits per pixel - eg. 24     |
 |   Alignment                       The Bitmap byte alignment - eg. 32 |
 *----------------------------------------------------------------------*)
function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

(*----------------------------------------------------------------------*
 | function RotateBitmap270                                             |
 |                                                                      |
 | Rotate a Bitmap clockwise through 270 degrees                        |
 |                                                                      |
 | Parameters:                                                          |
 |   Bitmap                          The Bitmap to rotate               |
 |                                                                      |
 | The function creates a rotated copy of the Bitmap.                   |
 *----------------------------------------------------------------------*)
function RotateBitmap270 (const Bitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bpss, bpsr: Integer;

begin
  Assert(Bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  Result := TBitmap.Create;
  try
    result.PixelFormat := Bitmap.PixelFormat;
    result.Height := Bitmap.Width;
    result.Width := Bitmap.Height;

    ps1 := Bitmap.ScanLine [0];
    pr1 := result.ScanLine [Bitmap.Width - 1];

    bpss := BytesPerScanLine (Bitmap.Width, 24, 32);
    bpsr := BytesPerScanLine (result.Width, 24, 32);

    for y := 0 to Bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PByte (ps1) - bpss * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        pr := PRGBTriple (PByte (pr1) + bpsr * x);
        Inc(pr, y);
        pr^ := ps^;
        Inc(ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end
end;

(*----------------------------------------------------------------------*
 | function RotateBitmap90                                              |
 |                                                                      |
 | Rotate a Bitmap clockwise through 90 degrees                         |
 |                                                                      |
 | Parameters:                                                          |
 |   Bitmap                          The Bitmap to rotate               |
 |                                                                      |
 | The function creates a rotated copy of the Bitmap.                   |
 *----------------------------------------------------------------------*)
function RotateBitmap90 (const Bitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bpss, bpsr: Integer;

begin
  Assert(Bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  Result := TBitmap.Create;
  try
    result.PixelFormat := Bitmap.PixelFormat;
    result.Height := Bitmap.Width;
    result.Width := Bitmap.Height;

    ps1 := Bitmap.ScanLine [Bitmap.Height - 1];
    pr1 := result.ScanLine [0];

    bpss := BytesPerScanLine (Bitmap.Width, 24, 32);
    bpsr := BytesPerScanLine (result.Width, 24, 32);

    for y := 0 to Bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PByte (ps1) + bpss * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        pr := PRGBTriple (PByte (pr1) - bpsr * x);
        Inc(pr, y);
        pr^ := ps^;
        Inc(ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

(*----------------------------------------------------------------------*
 | function ConvertToGrayscale                                          |
 |                                                                      |
 | Convert a Bitmap to it's greyscale equivalent                        |
 |                                                                      |
 | Parameters:                                                          |
 |   Bitmap                          The Bitmap to convert              |
 |                                                                      |
 | The function creates a greyscale copy of the Bitmap.                 |
 *----------------------------------------------------------------------*)
function ConvertToGrayscale (const Bitmap: TBitmap; TransparentColor: TColor): TBitmap;
var
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bps: Integer;
  n: Integer;
  transparent: Boolean;
  transparentTriple: TRGBTriple;
  rgb :DWORD;

begin
  Assert(Bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  transparent := TransparentColor <> clNone;
  if transparent then
  begin
    rgb := ColorToRGB (TransparentColor);
    transparentTriple.rgbtBlue := GetBValue (rgb);
    transparentTriple.rgbtGreen := GetGValue (rgb);
    transparentTriple.rgbtRed := GetRValue (rgb)
  end;

  Result := TBitmap.Create;
  try
    result.PixelFormat := Bitmap.PixelFormat;
    result.Height := Bitmap.Height;
    result.Width := Bitmap.Width;

    ps1 := Bitmap.ScanLine [0];
    pr1 := result.ScanLine [0];

    bps := BytesPerScanLine (Bitmap.Width, 24, 32);

    for y := 0 to Bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PByte (ps1) - bps * y);
      pr := PRGBTriple (PByte (pr1) - bps * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        if not Transparent
          or (ps^.rgbtBlue <> transparentTriple.rgbtBlue)
          or (ps^.rgbtGreen <> transparentTriple.rgbtGreen)
          or (ps^.rgbtRed <> transparentTriple.rgbtRed) then
        begin
          n := ((DWORD (ps^.rgbtBlue) * 28) +            // 11% Blue
                (DWORD (ps^.rgbtRed) * 77) +             // 30% Red
                (DWORD (ps^.rgbtGreen) * 151)) div 256;  // 59% Green

          pr^.rgbtBlue := n;
          pr^.rgbtGreen := n;
          pr^.rgbtRed := n;
        end
        else
          pr^ := ps^;

        Inc(pr);
        Inc(ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

(*----------------------------------------------------------------------*
 | function ConvertToNegative                                           |
 |                                                                      |
 | Convert a Bitmap to it's negative equivalent                         |
 |                                                                      |
 | Parameters:                                                          |
 |   Bitmap                          The Bitmap to convert              |
 |                                                                      |
 | The function creates a negative copy of the Bitmap.                  |
 *----------------------------------------------------------------------*)
function ConvertToNegative (const Bitmap: TBitmap): TBitmap;
var
  x, y: Integer;
  ps, ps1, pr, pr1: PRGBTriple;
  bps: Integer;
begin
  Assert(Bitmap.PixelFormat = pf24Bit, 'Invalid pixel format');

  Result := TBitmap.Create;
  try
    result.PixelFormat := Bitmap.PixelFormat;
    result.Height := Bitmap.Height;
    result.Width := Bitmap.Width;

    ps1 := Bitmap.ScanLine [0];
    pr1 := result.ScanLine [0];

    bps := BytesPerScanLine (Bitmap.Width, 24, 32);

    for y := 0 to Bitmap.Height - 1 do
    begin
      ps := PRGBTriple (PByte (ps1) - bps * y);
      pr := PRGBTriple (PByte (pr1) - bps * y);

      for x := 0 to Bitmap.Width - 1 do
      begin
        pr^.rgbtBlue := 255 - ps^.rgbtBlue;
        pr^.rgbtGreen := 255 - ps^.rgbtGreen;
        pr^.rgbtRed := 255 - ps^.rgbtRed;

        Inc(pr);
        Inc(ps)
      end
    end;
    GDIFlush
  except
    result.Free;
    raise
  end;
end;

end.
