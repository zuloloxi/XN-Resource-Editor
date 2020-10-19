(*======================================================================*
 | unitResourceHTML                                                     |
 |                                                                      |
 | Encapsulates HTML resources in resources                             |
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
unit unitResourceHTML;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, System.AnsiStrings, unitResourceDetails;

const
  RT_HTML = MakeIntResource(23);

type
//------------------------------------------------------------------------
// HTML resource details class

  THTMLResourceDetails = class (TAnsiResourceDetails)
  protected
    procedure InitNew; override;
    class function SupportsData (Size: Integer; data: Pointer): Boolean; override;
  public
    class function GetBaseType: UnicodeString; override;
  end;


implementation

{ THTMLResourceDetails }

class function THTMLResourceDetails.GetBaseType: UnicodeString;
begin
  Result := IntToStr (Integer (RT_HTML))
end;

procedure THTMLResourceDetails.InitNew;
begin
  Text := '<HTML>'#13#10'</HTML>';
end;

class function THTMLResourceDetails.SupportsData(Size: Integer;
  data: Pointer): Boolean;
var
  p: PAnsiChar;
begin
  p := PAnsiChar (data);
  Result := (System.AnsiStrings.StrLIComp (p, '<HTML', 5) = 0) or (System.AnsiStrings.StrLIComp (p, '<!', 2) = 0);
end;

initialization
  RegisterResourceDetails (THTMLResourceDetails);
finalization
  UnregisterResourceDetails (THTMLResourceDetails);
end.
