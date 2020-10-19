unit MiscComponentsReg;

interface

procedure Register;

implementation

{$R MiscComponentsReg.dcr}

uses
  Classes,
  ComponentRunOnce,
  ComponentPersistentPosition,
  ComponentNTAboutBox,
  ComponentHyperlinkButton,
  ComponentExSplitter,
  ComponentMRUList,
  ComponentThemedScrollBox,
  ComponentCWRichEdit,
  ComponentNewsRichEdit,
  ComponentRuler,
  ComponentSplitterPanel,
  ComponentHexDump,
  ComponentPersistentOptions,
  ComponentIniFilePersistentOptions,
  ComponentRegistryPersistentOptions,
  ComponentXMLPersistentOptions,
  ComponentPropertyListBox,
  ComponentSizingPageControl,
  ComponentColorSelector,
  ComponentExWebBrowser,
  ComponentCountryComboBox;

procedure Register;
begin
  RegisterComponents ('Colin Wilson''s Components', [
    TRunOnce,
    TPersistentPosition,
    TNTAboutBox,
    THyperlinkButton,
    TExSplitter,
    TMRUList,
    TThemedScrollBox,
    TExRichEdit,
    TNewsRichEdit,
    TRuler,
    TSplitterPanel,
    THexDump,
    TPersistentOptions,
    TRegistryPersistentOptions,
    TIniFilePersistentOptions,
    TXMLPersistentOptions,
    TPropertyListBox,
    TUniPersistentOptions,
    TColorSelector,
    TSizingPageControl,
    TExWebBrowser,
    TCountryComboBox
  ]);
end;

end.
