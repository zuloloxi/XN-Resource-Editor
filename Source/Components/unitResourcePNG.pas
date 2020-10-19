(*======================================================================*
 | unitResourcePNG                                                      |
 |                                                                      |
 | Encapsulates Png image resources in resources                        |
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
unit unitResourcePNG;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, System.AnsiStrings, Vcl.Graphics,
  Vcl.Imaging.PngImage, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// PNG resource details class

  TPngResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    class function SupportsData(Size: Integer; Data: Pointer): Boolean; override;
  public
    class function GetBaseType: UnicodeString; override;
    procedure GetImage (Picture: TPicture); override;
  end;


implementation

{ TPngResourceDetails }

class function TPngResourceDetails.GetBaseType: UnicodeString;
begin
  Result := 'PNG';
end;

function TPngResourceDetails.GetHeight: Integer;
begin
  Result := PWORD(PByte(Data) + 6 + SizeOf(Word))^;
end;

procedure TPngResourceDetails.GetImage(Picture: TPicture);
begin
  Picture.Graphic := TPngImage.Create;
  Data.Seek (0, TSeekOrigin.soBeginning);
  TPngImage(Picture.Graphic).LoadFromStream(Data)
end;

function TPngResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TPngResourceDetails.GetWidth: Integer;
begin
  Result := PWORD(PByte(Data) + 6)^;
end;

class function TPngResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
var
  p: PAnsiChar;
begin
  p := PAnsiChar(Data);
  Inc(p);

  Result := (System.AnsiStrings.StrLIComp (p, 'PNG', 3) = 0);
end;

initialization
  RegisterResourceDetails(TPngResourceDetails);
finalization
  UnregisterResourceDetails(TPngResourceDetails);
end.


