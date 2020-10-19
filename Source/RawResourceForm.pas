unit RawResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  ResourceForm, ComponentHexDump, unitResourceDetails;

type
  TFormRawResource = class(TFormResource)
    HexDump: THexDump;
  protected
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

{ TFormRawResource }

procedure TFormRawResource.SetObject(const Value: TObject);
var
  Details: TResourceDetails;
begin
  inherited;

  Details := obj as TResourceDetails;

  HexDump.Address := Details.Data.Memory;
  HexDump.DataSize := Details.Data.Size
end;

end.
