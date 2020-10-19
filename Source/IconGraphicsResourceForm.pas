unit IconGraphicsResourceForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  System.Actions, System.ImageList, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.ExtCtrls, VCL.ToolWin, VCL.Menus, VCL.ActnList, VCL.ImgList,
  VCL.ComCtrls, IconCursorGraphicsResourceForm, ComponentColorSelector,
  ComponentSizingPageControl, ComponentBitmapEditor, ComponentPropertyListBox;

type
  TFormIconGraphicsResource = class(TFormIconCursorGraphicsResource)
  end;

implementation

{$R *.dfm}

end.
