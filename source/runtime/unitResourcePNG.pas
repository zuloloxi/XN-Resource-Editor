(*======================================================================*
 | unitResourcePNG                                                      |
 |                                                                      |
 | Encapsulates Png image resources in resources                        |
 |                                                                      |
 | Copyright (c) Colin Wilson 2001,2008                                 |
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

uses Windows, Classes, SysUtils, graphics, pngimage, unitResourceDetails, unitResourceGraphics;

type
//------------------------------------------------------------------------
// PNG resource details class

  TPngResourceDetails = class (TGraphicsResourceDetails)
  protected
    function GetHeight: Integer; override;
    function GetPixelFormat: TPixelFormat; override;
    function GetWidth: Integer; override;
    class function SupportsData (Size : Integer; data : Pointer) : Boolean; override;
  public
    class function GetBaseType : UnicodeString; override;
    procedure GetImage (picture : TPicture); override;
  end;


implementation

{ TPngResourceDetails }

class function TPngResourceDetails.GetBaseType: UnicodeString;
begin
  Result := 'PNG';
end;

function TPngResourceDetails.GetHeight: Integer;
begin
  Result := PWORD (PByte (data) + 6 + SizeOf (Word))^;
end;

procedure TPngResourceDetails.GetImage(picture: TPicture);
begin
  picture.graphic := TPngImage.Create;
  data.Seek (0, soFromBeginning);
  TPngImage (picture.graphic).LoadFromStream (data)
end;

function TPngResourceDetails.GetPixelFormat: TPixelFormat;
begin
  Result := pf8Bit;
end;

function TPngResourceDetails.GetWidth: Integer;
begin
  result := PWORD (PByte (data) + 6)^;
end;

class function TPngResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p : PAnsiChar;
begin
  p := PAnsiChar (data);
  Inc (p);

  Result := (StrLIComp (p, 'PNG', 3) = 0);
end;

initialization
  RegisterResourceDetails (TPngResourceDetails);
finalization
  UnregisterResourceDetails (TPngResourceDetails);
end.


