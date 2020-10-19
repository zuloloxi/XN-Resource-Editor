unit TextInputForm;

interface

uses
  WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls;

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
