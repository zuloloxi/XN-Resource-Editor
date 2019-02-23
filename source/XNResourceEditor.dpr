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
  MainForm in 'MainForm.pas' {fmMain},
  RawResourceForm in 'RawResourceForm.pas' {fmRawResource},
  GraphicsResourceForm in 'GraphicsResourceForm.pas' {fmGraphicsResource},
  TextResourceForm in 'TextResourceForm.pas' {fmTextResource},
  PropertiesForm in 'PropertiesForm.pas' {fmProperties},
  GroupResourceForm in 'GroupResourceForm.pas' {fmGroupResource},
  AddResourceDialog in 'AddResourceDialog.pas' {dlgAddResource},
  VersionResourceForm in 'VersionResourceForm.pas' {fmVersionResource},
  MenuResourceForm in 'MenuResourceForm.pas' {fmMenuResource},
  ResourcePropertiesDialog in 'ResourcePropertiesDialog.pas' {dlgResourceProperties},
  DescriptionRCDataResourceForm in 'DescriptionRCDataResourceForm.pas' {fmRCDataDescriptionResource},
  PackagesResourceForm in 'PackagesResourceForm.pas' {fmPackagesResource},
  FormResourceForm in 'FormResourceForm.pas' {fmRCDataFormResource},
  XPManifestResourceForm in 'XPManifestResourceForm.pas' {fmXPManifestResource},
  AcceleratorResourceForm in 'AcceleratorResourceForm.pas' {fmAcceleratorResource},
  CloneResourceDialog in 'CloneResourceDialog.pas' {dlgCloneResource},
  PropertyBaseForm in 'PropertyBaseForm.pas' {fmPropertyBase},
  PropertyPageForm in 'PropertyPageForm.pas' {fmPropertyPage},
  PropertyPageRCSettings in 'PropertyPageRCSettings.pas' {fmPropertyPageRCSettings},
  PropertyPageProgramSettings in 'PropertyPageProgramSettings.pas' {fmPropertyPageProgramSettings},
  HelpContext in 'HelpContext.pas',
  IconCursorGraphicsResourceForm in 'IconCursorGraphicsResourceForm.pas' {fmIconCursorGraphicsResource},
  CursorGraphicsResourceForm in 'CursorGraphicsResourceForm.pas' {fmCursorGraphicsResource},
  IconGraphicsResourceForm in 'IconGraphicsResourceForm.pas' {fmIconGraphicsResource},
  FormTextInput in 'FormTextInput.pas' {fmTextInput},
  ResourceObjectForm in 'components\ResourceObjectForm.pas' {fmResourceObject},
  ResourceForm in 'components\ResourceForm.pas' {fmResourceForm};

{$R *.RES}
{$R i.res}

begin
  Application.Initialize;
  Application.Title := 'XN Resource Editor';
  Application.HelpFile := 'XNResourceEditor.chm';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
