unit PropertiesForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Menus,
  VCL.StdCtrls, VCL.ExtCtrls, VirtualTrees, PropertyBaseForm,
  ComponentPersistentPosition;

type
  TFormProperties = class(TFormPropertyBase)
    procedure FormDestroy(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  unitCredProperties, PropertyPageProgramSettings, PropertyPageRCSettings;

{$R *.DFM}

{ TFormProperties }

constructor TFormProperties.Create(AOwner: TComponent);
begin
  inherited;

  gProperties.BeginUpdate;
  AddPropertyPageDetails(TFormPropertyPageProgramSettings, nil);
  AddPropertyPageDetails(TFormPropertyPageRCSettings, nil);
end;

procedure TFormProperties.FormDestroy(Sender: TObject);
begin
  inherited;
  gProperties.EndUpdate;
end;

end.
