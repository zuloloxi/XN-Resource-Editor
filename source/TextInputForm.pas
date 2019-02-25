unit TextInputForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TFormTextInput = class(TForm)
    MemoText: TMemo;
    ButtonOK: TButton;
    ButtonSelectFont: TButton;
    ButtonCancel: TButton;
    FontDialog: TFontDialog;
    procedure ButtonSelectFontClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TFormTextInput.ButtonSelectFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(MemoText.Font);
  if FontDialog.Execute then
    MemoText.Font.Assign(FontDialog.Font)
end;

end.