unit IconGraphicsResourceForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ToolWin, Menus, ActnList, ImgList, ComCtrls,
  System.Actions, System.ImageList,
  IconCursorGraphicsResourceForm, ComponentColorSelector,
  ComponentSizingPageControl, ComponentBitmapEditor, ComponentPropertyListBox;

type
  TfmIconGraphicsResource = class(TfmIconCursorGraphicsResource)
  end;

implementation

{$R *.dfm}

end.
