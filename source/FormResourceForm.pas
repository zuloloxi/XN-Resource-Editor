unit FormResourceForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ResourceForm, unitResourceRCData;

type
  TfmRCDataFormResource = class(TfmResource)
    Memo: TMemo;
    procedure MemoExit(Sender: TObject);
  private
    FDetails: TRCDataFormResourceDetails;
    procedure SaveResource (const undoDetails: string);
  public
    procedure SetObject(const Value: TObject); override;
  end;

implementation

{$R *.DFM}

resourcestring
  rstFormChange = 'form change';

{ TfmRCDataFormResource }

procedure TfmRCDataFormResource.SetObject(const Value: TObject);
begin
  inherited;

  FDetails := obj as TRCDataFormResourceDetails;

  Memo.Text := FDetails.Text;
end;

procedure TfmRCDataFormResource.MemoExit(Sender: TObject);
begin
  inherited;

  if Memo.CanUndo then
   SaveResource(rstFormChange);
end;

procedure TfmRCDataFormResource.SaveResource(const undoDetails: string);
begin
  AddUndoEntry(undoDetails);
  FDetails.Text := Memo.Text;
end;

end.
