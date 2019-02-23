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
    procedure SaveResource (const undoDetails: string);
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
  details: TRCDataDescriptionResourceDetails;
begin
  inherited;

  details := Obj as TRCDataDescriptionResourceDetails;
  EditDescription.Text := details.Description;
end;

procedure TfmRCDataDescriptionResource.UpdateFonts;
begin
  UseInternationalFont(EditDescription.Font);
end;

procedure TfmRCDataDescriptionResource.SaveResource (const undoDetails: string);
var
  details: TRCDataDescriptionResourceDetails;
begin
  AddUndoEntry (undoDetails);
  details := Obj as TRCDataDescriptionResourceDetails;
  details.Description := EditDescription.Text;
end;

procedure TfmRCDataDescriptionResource.EditDescriptionExit(Sender: TObject);
begin
  if EditDescription.CanUndo then
    SaveResource (rstChangeDescription);
end;

end.
