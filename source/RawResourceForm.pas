unit RawResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ResourceForm, cmpHexDump, unitResourceDetails;

type
  TfmRawResource = class(TfmResource)
    HexDump1: THexDump;
  protected
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

{ TfmRawResource }

procedure TfmRawResource.SetObject(const Value: TObject);
var
  Details: TResourceDetails;
begin
  inherited;

  Details := obj as TResourceDetails;

  HexDump1.Address := Details.Data.Memory;
  HexDump1.DataSize := Details.Data.Size
end;

end.
