unit ResourceEditorComponentsReg;

interface

procedure Register;

implementation

uses
  Classes,
  ComponentBitmapEditor,
  ComponentMenuDesigner,
  ComponentDialogBox,
  ComponentDialogEditor,
  ComponentGradientShape,
  ComponentStandardSystemMenu;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TBitmapEditor,
    TMenuDesigner,
    TPopupMenuDesigner,
    TDialogBox,
    TDialogEditor,
    TStandardSystemMenu
  ])
end;

end.
