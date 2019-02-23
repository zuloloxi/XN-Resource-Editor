unit ResourcePropertiesDialog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, unitResourceDetails;

type
  TdlgResourceProperties = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ComboBoxLanguage: TComboBox;
    EditName: TEdit;
    LabelLanguage: TLabel;
    LabelName: TLabel;
    procedure FormShow(Sender: TObject);
  private
    function GetLanguage: LCID;
  public
    ResourceDetails: TResourceDetails;

    property Language: LCID read GetLanguage;
  end;

implementation

{$R *.DFM}

uses
  unitResourceMessages, ResourceForm;

resourcestring
  rstNeutral = 'Language Neutral';

procedure TdlgResourceProperties.FormShow(Sender: TObject);
var
  LanguageIndex: Integer;
  LanguageName: string;
begin
  UseInternationalFont(EditName.Font);
  if Assigned(ResourceDetails) then
  begin
    if resourceDetails is TStringResourceDetails then
      EditName.Text := ResIdToStringsId(ResourceDetails.ResourceName)
    else
      EditName.Text := ResourceDetails.ResourceName;
  end;

  ComboBoxLanguage.Items.Add ('- ' + rstNeutral);
  LanguageName := '-';

  for LanguageIndex := 0 to Languages.Count - 1 do
  begin
    ComboBoxLanguage.Items.Add (Languages.Name[LanguageIndex]);
    if Assigned(ResourceDetails) and (ResourceDetails.ResourceLanguage <> 0) and
      (DWORD(ResourceDetails.ResourceLanguage) = Languages.LocaleID[LanguageIndex]) then
      LanguageName := Languages.Name[LanguageIndex];
  end;

  if LanguageName = '-' then
    ComboBoxLanguage.ItemIndex := 0
  else
    ComboBoxLanguage.Text := LanguageName;
end;

function TdlgResourceProperties.GetLanguage: LCID;
var
  LanguageIndex: Integer;
begin
  Result := 0;
  if ComboBoxLanguage.ItemIndex <> 0 then
    for LanguageIndex := 0 to Languages.Count -1 do
      if Languages.Name[LanguageIndex] = ComboBoxLanguage.Text then
      begin
        Result := Languages.LocaleID[LanguageIndex];
        Break;
      end;
end;

end.
