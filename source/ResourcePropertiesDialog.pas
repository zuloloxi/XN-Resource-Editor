unit ResourcePropertiesDialog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, unitResourceDetails;

type
  TdlgResourceProperties = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cbLanguage: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    ntedName: TEdit;
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
  UseInternationalFont (ntedName.Font);
  if Assigned (ResourceDetails) then
  begin
    if resourceDetails is TStringResourceDetails then
      ntedName.Text := ResIdToStringsId (ResourceDetails.ResourceName)
    else
      ntedName.Text := ResourceDetails.ResourceName;
  end;

  cbLanguage.Items.Add ('- ' + rstNeutral);
  LanguageName := '-';

  for LanguageIndex := 0 to Languages.Count - 1 do
  begin
    cbLanguage.Items.Add (Languages.Name[LanguageIndex]);
    if Assigned (ResourceDetails) and (ResourceDetails.ResourceLanguage <> 0) and (DWORD (ResourceDetails.ResourceLanguage) = Languages.LocaleID[LanguageIndex]) then
      LanguageName := Languages.Name[LanguageIndex];
  end;

  if LanguageName = '-' then
    cbLanguage.ItemIndex := 0
  else
    cbLanguage.Text := LanguageName;
end;

function TdlgResourceProperties.GetLanguage: LCID;
var
  LanguageIndex: Integer;
begin
  Result := 0;
  if cbLanguage.ItemIndex <> 0 then
    for LanguageIndex := 0 to Languages.Count -1 do
      if Languages.Name[LanguageIndex] = cbLanguage.Text then
      begin
        Result := Languages.LocaleID[LanguageIndex];
        Break;
      end;
end;

end.
