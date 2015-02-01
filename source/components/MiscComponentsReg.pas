unit MiscComponentsReg;

interface

procedure Register;

implementation

// {$R MiscComponentsReg.dcr}

uses Classes,
     cmpRunOnce,
     cmpPersistentPosition,
     cmpNTAboutBox,
     cmpHyperlinkButton,
     cmpExSplitter,
     cmpMRUList,
     cmpThemedScrollBox,
     cmpCWRichEdit,
     cmpNewsRichEdit,
     cmpRuler,
     cmpSplitterPanel,
     cmpHexDump,
     cmpPersistentOptions,
     cmpIniFilePersistentOptions,
     cmpRegistryPersistentOptions,
     cmpXMLPersistentOptions,
     cmpPropertyListBox,
     cmpSizingPageControl,
     cmpColorSelector,
     cmpExWebBrowser,
     cmpCountryComboBox;

(*
     cmpMessageDisplay;

     cmpSpellChecker,
     cmpCWSpellChecker,
     cmpTexturedPanel,
     cmpFileCopier,
     cmpTextDisplay,
     CustomAlignPanel,
     cmpSuDoku,
     cmpExToolbar,
     cmpUserDatabase,
     ExCoolBar,
     cmpExplorerTree,
     cmpCountryComboBox; *)

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
(*
    TMessageDisplay,
    TSpellChecker,
    TCWSpellChecker,
    TTexturedPanel,
    TFileCopier,
    TTextDisplay,
    TCustomAlignPanel,
    TSuDoku,
    TExToolbar,
    TUserDatabase,
    TExCoolBar,
    TExplorerTree,
    TCountryComboBox*)
  ]);
end;

end.
