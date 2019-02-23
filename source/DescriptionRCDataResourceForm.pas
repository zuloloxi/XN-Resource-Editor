unit DescriptionRCDataResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ResourceForm;

type
  TfmRCDataDescriptionResource = class(TfmResource)
    LabelDescription: TLabel;
    EditDescription: TEdit;
    procedure EditDescriptionExit(Sender: TObject);
  private
    procedure SaveResource (const UndoDetails: string);
  protected
    procedure SetObject(const Value: TObject); override;
  public
    procedure UpdateFonts; override;
  end;

implementation

uses
  unitResourceRCData;

{$R *.DFM}

resourcestring
  rstChangeDescription = 'change description';

{ TfmRCDataDescriptionResource }

procedure TfmRCDataDescriptionResource.SetObject(const Value: TObject);
var
  Details: TRCDataDescriptionResourceDetails;
begin
  inherited;

  Details := Obj as TRCDataDescriptionResourceDetails;
  EditDescription.Text := Details.Description;
end;

procedure TfmRCDataDescriptionResource.UpdateFonts;
begin
  UseInternationalFont(EditDescription.Font);
end;

procedure TfmRCDataDescriptionResource.SaveResource (const UndoDetails: string);
var
  Details: TRCDataDescriptionResourceDetails;
begin
  AddUndoEntry (UndoDetails);
  Details := Obj as TRCDataDescriptionResourceDetails;
  Details.Description := EditDescription.Text;
end;

procedure TfmRCDataDescriptionResource.EditDescriptionExit(Sender: TObject);
begin
  if EditDescription.CanUndo then
    SaveResource (rstChangeDescription);
end;

end.
