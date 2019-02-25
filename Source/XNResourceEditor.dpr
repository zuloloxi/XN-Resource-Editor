(*======================================================================*
 | program XNResourceEditor                                                     |
 |                                                                      |
 | Colin's Resource Editor.                                             |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      07/02/2001  CPWW  Original                                  |
 | 2.0      25/11/2004  CPWW  Further enhancements for new 'XN Resource |
 |                            Editor' product                           |
 | 3.0      13/06/2005  CPWW  Unicode enabled.  Support for Windows 98  |
 |                            dropped                                   |
 *======================================================================*)

program XNResourceEditor;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  RawResourceForm in 'RawResourceForm.pas' {fmRawResource},
  GraphicsResourceForm in 'GraphicsResourceForm.pas' {FormGraphicsResource},
  TextResourceForm in 'TextResourceForm.pas' {fmTextResource},
  PropertiesForm in 'PropertiesForm.pas' {FormProperties},
  GroupResourceForm in 'GroupResourceForm.pas' {FormGroupResource},
  AddResourceDialog in 'AddResourceDialog.pas' {FormAddResource},
  VersionResourceForm in 'VersionResourceForm.pas' {fmVersionResource},
  MenuResourceForm in 'MenuResourceForm.pas' {FormMenuResource},
  ResourcePropertiesDialog in 'ResourcePropertiesDialog.pas' {dlgResourceProperties},
  DescriptionRCDataResourceForm in 'DescriptionRCDataResourceForm.pas' {FormRCDataDescriptionResource},
  PackagesResourceForm in 'PackagesResourceForm.pas' {FormPackagesResource},
  RCDataResourceForm in 'RCDataResourceForm.pas' {fmRCDataFormResource},
  XPManifestResourceForm in 'XPManifestResourceForm.pas' {fmXPManifestResource},
  AcceleratorResourceForm in 'AcceleratorResourceForm.pas' {FormAcceleratorResource},
  CloneResourceDialog in 'CloneResourceDialog.pas' {FormCloneResource},
  PropertyBaseForm in 'PropertyBaseForm.pas' {FormPropertyBase},
  PropertyPageForm in 'PropertyPageForm.pas' {FormPropertyPage},
  PropertyPageRCSettings in 'PropertyPageRCSettings.pas' {fmPropertyPageRCSettings},
  PropertyPageProgramSettings in 'PropertyPageProgramSettings.pas' {fmPropertyPageProgramSettings},
  HelpContext in 'HelpContext.pas',
  IconCursorGraphicsResourceForm in 'IconCursorGraphicsResourceForm.pas' {FormIconCursorGraphicsResource},
  CursorGraphicsResourceForm in 'CursorGraphicsResourceForm.pas' {FormCursorGraphicsResource},
  IconGraphicsResourceForm in 'IconGraphicsResourceForm.pas' {fmIconGraphicsResource},
  TextInputForm in 'TextInputForm.pas' {FormTextInput},
  ResourceObjectForm in 'components\ResourceObjectForm.pas' {fmResourceObject},
  ResourceForm in 'components\ResourceForm.pas' {fmResourceForm};

{$R *.RES}
{$R i.res}

begin
  Application.Initialize;
  Application.Title := 'XN Resource Editor';
  Application.HelpFile := 'XNResourceEditor.chm';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
