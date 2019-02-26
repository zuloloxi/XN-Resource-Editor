unit IconCursorGraphicsResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  System.Actions, System.ImageList, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.ExtCtrls, VCL.ToolWin, VCL.Menus, VCL.ActnList, VCL.ImgList,
  VCL.ComCtrls, GraphicsResourceForm, ComponentColorSelector,
  ComponentSizingPageControl, ComponentBitmapEditor, ComponentPropertyListBox;

type
  TFormIconCursorGraphicsResource = class(TFormGraphicsResource)
  end;

implementation

{$R *.dfm}

end.
