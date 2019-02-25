unit HelpContext;

interface

uses
  Windows, Classes, SysUtils;

const
  NO_CONTEXTS = 2;
  hcAddResourceDialog = 1000;
  hcNewImageDialog = 1001;

function HelpLink (Context: THelpContext): string;

implementation

type
  THelpLink = record
    Context: THelpContext;
    Jump: string;
  end;

const
  HelpLinks: array [0..NO_CONTEXTS - 1] of THelpLink = (
    (Context:hcAddResourceDialog; Jump:'AddResourceDialog.htm'),
    (Context:hcNewImageDialog; Jump:'NewImage.htm')
  );

function HelpLink (Context: THelpContext): string;

  function BSearch (s, e: Integer): string;
  var
    m: Integer;
  begin
    if e >= s then
    begin
      m := s + (e - s) div 2;

      if Context > HelpLinks [m].Context then
        Result := BSearch (m + 1, e)
      else
        if Context < HelpLinks [m].Context then
          Result := BSearch (s, m - 1)
        else
          Result := HelpLinks [m].Jump
    end
    else
      Result := 'notfound.htm';
  end;

begin
  Result := BSearch (0, NO_CONTEXTS - 1)
end;


end.
