unit cmpSizingPageControl;

interface

uses
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TSizingPageControl = class(TPageControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    property AutoSize;
  end;

implementation

end.
