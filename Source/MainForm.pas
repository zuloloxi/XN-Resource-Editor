(*======================================================================*
 | MainForm                                                             |
 |                                                                      |
 | Main Form for PE Resource Explorer /Editor                           |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/01/2001  CPWW  Original                                  |
 *======================================================================*)

unit MainForm;

{$WARN SYMBOL_PLATFORM OFF}

interface

{$region 'Interface Uses Section'}
//------------------------------------------------------
uses
  WinApi.Windows, WinApi.Messages, WinApi.ShellApi, System.SysUtils,
  System.SysConst, System.Classes, System.Actions, System.ImageList,
  VCL.Graphics, VCL.Controls, VCL.Forms, Vcl.Dialogs, VCL.ComCtrls, VCL.Menus,
  VCL.ToolWin, Vcl.ExtCtrls, Vcl.ImgList, Vcl.StdActns, VCL.ActnList,
  Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.ActnMan, Vcl.ActnCtrls, Vcl.ActnMenus,
  Vcl.Imaging.PngImage, Vcl.Imaging.GIFImg, Vcl.Imaging.Jpeg, VCL.AppEvnts,
  VCL.XPStyleActnCtrls, VirtualTrees, ExVirtualStringTree,
  ComponentStandardSystemMenu, ComponentPersistentPosition, ComponentMRUList,
  ComponentNTAboutBox, ResourceForm, unitResourceDetails, unitCREdProperties,
  unitHTMLHelpViewer, unitResourceExaminer;
{$endregion}

{$region 'Constant Definitions'}
//------------------------------------------------------
const
  WM_INITIALIZE = WM_USER + $200;
  WM_STATUSBAR = WM_USER + $203;
  WM_ADDIMAGERESOURCE = WM_USER + $204;
{$endregion}

{$region 'Type Definitions'}
//------------------------------------------------------
type

//======================================================================
// TFormMain
  TFormMain = class(TForm)
    MainMenu: TMainMenu;
    ImageListMain: TImageList;
    ActionList: TActionList;
    ActionHelpContents: THelpContents;
    ActionHelpHelpOnHelp: THelpOnHelp;
    ActionHelpTopicSearch: THelpTopicSearch;
    MenuItemEdit: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemEditUndo: TMenuItem;
    N1: TMenuItem;
    MenuItemEditCut: TMenuItem;
    MenuItemEditCopy: TMenuItem;
    MenuItemEditPaste: TMenuItem;
    MenuItemEditSelectAll: TMenuItem;
    MenuItemHelpContents: TMenuItem;
    MenuItemHelpHelpOnHelp: TMenuItem;
    MenuItemHelpTopicSearch: TMenuItem;
    ActionFileNew: TAction;
    ActionFileOpenFile: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFilePrint: TAction;
    MenuItemFile: TMenuItem;
    MenuItemFileNew: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    N2: TMenuItem;
    MenuItemFilePrint: TMenuItem;
    N3: TMenuItem;
    ActionFileExit: TAction;
    MenuItemFileExit: TMenuItem;
    StatusBarMain: TStatusBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PrintDialog: TPrintDialog;
    Splitter: TSplitter;
    MRUList: TMRUList;
    PersistentPosition: TPersistentPosition;
    StandardSystemMenu: TStandardSystemMenu;
    PopupMenuMRU: TPopupMenu;
    PanelResource: TPanel;
    MenuItemResource: TMenuItem;
    ActionResourceAddResource: TAction;
    ActionResourceDeleteResource: TAction;
    MenuItemResourceAddResource: TMenuItem;
    MenuItemResourceDeleteResource: TMenuItem;
    N4: TMenuItem;
    ActionViewToolbar: TAction;
    ActionViewStatusbar: TAction;
    ActionViewProperties: TAction;
    MenuItemView: TMenuItem;
    MenuItemViewToolbar: TMenuItem;
    MenuItemViewStatusbar: TMenuItem;
    N5: TMenuItem;
    MenuItemViewProperties: TMenuItem;
    ActionEditCopy: TAction;
    ActionEditPaste: TAction;
    ActionEditCut: TAction;
    ActionEditSelectAll: TAction;
    ActionEditUndo: TAction;
    ActionEditRedo: TAction;
    MenuItemEditRedo: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    ActionHelpAbout: TAction;
    N6: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    NTAboutBox: TNTAboutBox;
    ActionResourceExportResource: TAction;
    MenuItemExportResource: TMenuItem;
    ImageListResources: TImageList;
    ActionResourceImportResource: TAction;
    MenuItemImportImageResource: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    ActionResourceProperties: TAction;
    N8: TMenuItem;
    MenuItemResourceProperties: TMenuItem;
    PopupMenuResources: TPopupMenu;
    MenuItemAddResource1: TMenuItem;
    MenuItemDeleteResource1: TMenuItem;
    N9: TMenuItem;
    MenuItemImportImageResource1: TMenuItem;
    MenuItemExportResource2: TMenuItem;
    N10: TMenuItem;
    MenuItemProperties: TMenuItem;
    ActionEditDelete: TAction;
    MenuItemEditDelete: TMenuItem;
    ActionResourceGrab: TAction;
    MenuItemGrab: TMenuItem;
    MenuItemGrabBitmap: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    ApplicationEvents: TApplicationEvents;
    ActionResourceImportRCDataResource: TAction;
    MenuItemImportRCDataResource: TMenuItem;
    MenuItemImportRCDataResource2: TMenuItem;
    OpenDialogAllFiles: TOpenDialog;
    ActionResourceImportOtherResource: TAction;
    MenuItemImportUserResource: TMenuItem;
    N11: TMenuItem;
    MenuItemImportUserResource2: TMenuItem;
    N7: TMenuItem;
    ActionResourceClone: TAction;
    MenuItemCloneResource: TMenuItem;
    vstResources: TExVirtualStringTree;
    ToolBarMenu: TToolBar;
    ToolButtonFile: TToolButton;
    ToolButtonEdit: TToolButton;
    ToolButtonView: TToolButton;
    ToolButtonResource: TToolButton;
    ToolButtonResourceObject: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolBarMain: TToolBar;
    ToolButtonFileNew: TToolButton;
    ToolButtonFileOpen: TToolButton;
    ToolButtonFileSave: TToolButton;
    ToolButtonSplitter: TToolButton;
    ToolButtonAdd: TToolButton;
    ToolButtonDelete: TToolButton;
    procedure vstResourcesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure vstResourcesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vstResourcesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vstResourcesInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure vstResourcesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
    procedure vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ActionResourceGrabExecute(Sender: TObject);
    procedure ActionResourceCloneExecute(Sender: TObject);
    procedure ActionFileOpenFileExecute(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionFilePrintExecute(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MRUListPopupMenuClick(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionViewToolbarExecute(Sender: TObject);
    procedure ActionViewStatusbarExecute(Sender: TObject);
    procedure ActionViewPropertiesExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionResourceExportResourceExecute(Sender: TObject);
    procedure ActionResourceAddResourceExecute(Sender: TObject);
    procedure ActionResourceDeleteResourceExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionResourceImportResourceExecute(Sender: TObject);
    procedure ActionResourcePropertiesExecute(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    function ApplicationEventsHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure ActionResourceImportRCDataResourceExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionResourceImportOtherResourceExecute(Sender: TObject);
    procedure ApplicationEventsMessage(var Msg: tagMSG;
      var Handled: Boolean);
    procedure vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vstResourcesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstResourcesIncrementalSearch(Sender: TBaseVirtualTree;
      Node: PVirtualNode; const SearchText: string; var Result: Integer);
  private
    FResourceModule: TResourceModule;
    FUndo, FRedo: string;
    FFileName: string;
    FIgnoreChar: Boolean;
    FWasDirty: Boolean;
    FExaminer: TResourceExaminer;

    procedure SetCaption;
    procedure UpdateDisplay(selectDetails: TResourceDetails);
    procedure OpenFile(const FileName: string);
    procedure SwitchView (Details: TObject);
    procedure WmInitialize(var msg: TMessage); message WM_INITIALIZE;
    procedure WmStatusBar(var Msg: TMessage); message WM_STATUSBAR;
    procedure WmPropertiesChanged (var msg: TMessage); message WM_PROPERTIES_CHANGED;
    procedure WmAddImageResource(var msg: TMessage); message WM_ADDIMAGERESOURCE;
    procedure SetFormMenuButton(item: TMenuItem);
    procedure SetFileName(const Value: string);

    function SaveFile: Boolean;
    function SaveFileAs: Boolean;
    function SaveChanges: Boolean;
    function IsDirty: Boolean;

    function GetNodeElement(node: PVirtualNode; var elem: TResExamElement): Boolean;
    function GetNodeResourceDetails(node: PVirtualNode): TResourceDetails;
    function GetResourceDetailsNode(Details: TResourceDetails): PVirtualNode;
    function SelectedResourceDetails: TResourceDetails;
    function ResourceForm: TFormResource;
    procedure CheckDetails(p: PVirtualNode; param: Integer; var continue: Boolean);

    property FileName: string read FFileName write SetFileName;
    function GetPersistDirectoryName(Section: string): string;
    procedure SetPersistDirectoryName(Section: string;
      const Value: string);
    function CloneResource(res: TResourceDetails; newName: string; newLanguage: Integer): TResourceDetails;
    function EditAllowed (node: PVirtualNode): Boolean;
    procedure UpdateResourceNodes (res: TResourceDetails; node: PVirtualNode);
  protected
    procedure Loaded; override;
    procedure UpdateActions; override;
  public
    property PersistDirectoryName [Section: string]: string read GetPersistDirectoryName write SetPersistDirectoryName;
  end;
{$endregion}

//----------------------------------------------------------------------
var
  FormMain: TFormMain;

//----------------------------------------------------------------------
procedure AttachMenu(const buttonCaption: string; menu: TMenuItem);
procedure DetachMenu(const buttonCaption: string);
function GetTypeImage(const tp: string): Integer;

implementation

{$R *.DFM}

{$region 'Implementation Uses Section'}
uses
  System.Win.Registry, RawResourceForm,
   unitPEFile,                        // Accept resources from PE files
   unitNTModule,                      // Use this instead if NT
   unitResFile,                       // Accept resources from .RES files
   unitRCFile,                        // Accept resources from .RC files
   unitExIcon,

   unitResourceGraphics,              // Decoder unit for Icons, Bitmaps, Cursors
   unitResourceMessages,              //    "     "    "  String and Message tables
   unitResourceVersionInfo,           //    "     "    "  Version Info
   unitResourceMenus,                 //    "     "    "  Menus
   unitResourceDialogs,               //    "     "    "  Dialogs
   unitResourceRCData,                //    "     "    "  RCData
   unitResourceJPEG,                  //    "     "    "  JPEG Images
   unitResourceGIF,                   //    "     "    "  GIF Images
   unitResourceXPManifests,           //    "     "    "  XP Manifests
   unitResourceAccelerator,           //    "     "    "  Accelerator tables
   unitResourceToolbar,

   GroupResourceForm,                 // Display Form for Icons & cursor groups
   IconGraphicsResourceForm,          // Editor   "   "  Icons
   CursorGraphicsResourceForm,        // Editor   "   "  Cursors
   GraphicsResourceForm,              // Editor   "   "  Other graphics - bitmaps etc.
   TextResourceForm,                  //   "      "   "   String and MEssage tables
   VersionResourceForm,               //   "      "   "   Version Info
   MenuResourceForm,                  //   "      "   "   Menus
   DialogResourceForm,                //   "      "   "   Dialogs
   DescriptionRCDataResourceForm,     //   "      "   "   RC Data Description
   PackagesResourceForm,              //   "      "   "   Borland 'package' RC data
   RCDataResourceForm,                //   "      "   "   Borland TForm data
   XPManifestResourceForm,            //   "      "   "   XML XP Manifest
   AcceleratorResourceForm,           //   "      "   "   Accelerators


   PropertiesForm,                    // Program properties dialog
   AddResourceDialog,                 // Add Resource dialog
   ResourcePropertiesDialog,
   HelpContext, ResourceObjectForm, unitSearchString, CloneResourceDialog;
{$endregion}

{$region 'Resource String Definitions' }
//----------------------------------------------------------------------
resourcestring
  rstColors = '%d colours';
  rstHighColor = 'High Colour';
  rstTrueColor = 'True Colour';
  rstLanguageNeutral = 'Language Neutral';
  rstDuplicateResourceName = 'Duplicate Resource Name';
  rstAnyFileFilter = 'Any File (*.*)|*.*';

  rstBitmap       = 'Bitmap';
  rstIcon         = 'Icon';
  rstCursor       = 'Cursor';
  rstMenu         = 'Menu';
  rstToolbar      = 'Toolbar';
  rstDialog       = 'Dialog';
  rstAccelerator  = 'Accelerator Table';
  rstString       = 'String Table';
  rstRCData       = 'RC Data';
  rstMessageTable = 'MessageTable';
  rstVersion      = 'Version';
  rstGroupCursor  = 'Cursor Group';
  rstGroupIcon    = 'Icon Group';
  rstXPManifest   = 'XP Theme Manifest';

  rstUndo         = '&Undo %s';
  rstRedo         = '&Redo %s';
  rstUntitled     = '<Untitled>';
  rstResFilter    = 'Resource Files (*.res;*.dcr)|*.RES;*.DCR';
  rstModuleFilter = 'Module Files (*.exe;*.dll;*.bpl;*.scr;*.cpl;*.ocx)|*.EXE;*.DLL;*.OCX;*.BPL;*.SCR;*.OCX';
  rstChanges      = 'Save changes to %s';

  rstChangeResourceProperties = 'change resource properties';

const
  c = RT_TOOLBAR;
{$endregion}

{$region 'Local Definitions'}
const
  imgClosedFolder = 10;
  imgOpenFolder = 11;
{$endregion}

{$region 'Helper Functions'}

(*----------------------------------------------------------------------*
 | procedure AttachMenu                                                 |
 |                                                                      |
 | Global procedure - attaches a menu to a button (the ...Object one)   |
 *----------------------------------------------------------------------*)
procedure AttachMenu(const buttonCaption: string; menu: TMenuItem);
var
  i: Integer;
  btn: TToolButton;
begin
  exit;
  if csDestroying in FormMain.ComponentState then Exit;
  btn := nil;
  for i := 0 to FormMain.ToolBarMenu.ButtonCount - 1 do
    if FormMain.ToolBarMenu.Buttons [i].Caption = buttonCaption then
    begin
      btn := FormMain.ToolBarMenu.Buttons [i];
      break
    end;

  if Assigned(btn) then
  begin
    btn.MenuItem := menu;
    btn.Visible := True
  end
end;

(*----------------------------------------------------------------------*
 | procedure DetachMenu                                                 |
 |                                                                      |
 | Global procedure - detaches a menu to a button (the ...Object one)   |
 *----------------------------------------------------------------------*)
procedure DetachMenu(const buttonCaption: string);
var
  i: Integer;
  btn: TToolButton;
begin
  if csDestroying in FormMain.ComponentState then Exit;
  btn := nil;
  for i := 0 to FormMain.ToolBarMenu.ButtonCount - 1 do
    if FormMain.ToolBarMenu.Buttons [i].Caption = buttonCaption then
    begin
      btn := FormMain.ToolBarMenu.Buttons [i];
      break
    end;

  if Assigned(btn) then
  begin
    btn.Visible := False;
    btn.MenuItem := nil
  end
end;

(*----------------------------------------------------------------------*
 | function GetTypeImage                                                |
 |                                                                      |
 | Get the image index for a resource type.  The 'magic number'         |
 | returned is an index in the ilResources image list.                  |
 *----------------------------------------------------------------------*)
function GetTypeImage(const tp: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := ResourceNametoInt(tp);

  case i of
    Integer(RT_VERSION):
      i := 8;
    Integer(RT_MESSAGETABLE):
      i := Integer(RT_STRING)
  end;

  if (i >= 0) and (i < 10) then
    Result := i
  else
    if tp = 'MIDI' then
      Result := 12
    else
      if tp = 'WAVE' then
       Result := 13
end;

(*----------------------------------------------------------------------*
 | function GetGraphicsClass: TGraphicClass                             |
 |                                                                      |
 | Get a graphic class associated with an extension                     |
 |                                                                      |
 | Parameters:                                                          |
 |                                                                      |
 |   ext: string                 The extension to find.                 |
 |                                                                      |
 | The function returns the graphics class                              |
 *----------------------------------------------------------------------*)
function GetGraphicsClass (ext: string): TGraphicClass;
begin
  ext := UpperCase(ext);
  if (ext <> '') and (ext [1] = '.') then
    Delete(ext, 1, 1);

  if ext = 'BMP' then
    Result := TBitmap
  else
    if (ext = 'WMF') or (ext = 'EMF') then
      Result := TMetafile
    else
      if ext = 'ICO' then
        Result := TExIcon
      else
        if ext = 'CUR' then
          Result := TExCursor
        else
          if (ext = 'JPG') or (ext = 'JPEG') then
            Result := TJpegImage
          else
            if ext = 'GIF' then
              Result := TGIFImage
            else
              if ext = 'PNG' then
                Result := TPngImage
              else
                Result := nil;
end;

{*----------------------------------------------------------------------*
 | function CreateCompatibleGraphic                                     |
 |                                                                      |
 | Create a graphic of a specified type, using the image and dimensions |
 | of another graphic.                                                  |
 |                                                                      |
 | Parameters:                                                          |
 |   graphic: TGraphic;                The original graphic             |
 |   newCls: TGraphicClass             The new graphic class            |
 *----------------------------------------------------------------------*}
function CreateCompatibleGraphic (graphic: TGraphic; newCls: TGraphicClass): TGraphic;

  type
    TRGB      = packed record b, g, r: Byte end;
    TRGBA      = packed record b, g, r, a: Byte end;
    TRGBAArray = array[0..0] of TRGBA;

  function PNG4TransparentBitMap(bmp:TBitmap):TPNGImage;
  //201011 Thomas Wassermann
  var
    x, y:Integer;
    vBmpRGBA: ^TRGBAArray;
    vPngRGB: ^TRGB;
  begin
    Assert(bmp.PixelFormat = pf32bit, 'bmp pixelformat not 32 bit');

    Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, bmp.Width , bmp.Height);
    Result.CreateAlpha;
    Result.Canvas.CopyMode:= cmSrcCopy;
    Result.Canvas.Draw(0,0,bmp);

    for y := 0 to pred(bmp.Height) do begin
      vBmpRGBA := bmp.ScanLine[y];
      vPngRGB:= Result.Scanline[y];

      for x := 0 to pred(bmp.width) do begin
        Result.AlphaScanline[y][x] :=  vBmpRGBA[x].A;
        if bmp.AlphaFormat in [afDefined,afPremultiplied] then begin
          if vBmpRGBA[x].A <> 0 then begin
            vPngRGB^.b:= round(vBmpRGBA[x].b/vBmpRGBA[x].A*255);
            vPngRGB^.r:= round(vBmpRGBA[x].r/vBmpRGBA[x].A*255);
            vPngRGB^.g:= round(vBmpRGBA[x].g/vBmpRGBA[x].A*255);
          end else begin
            vPngRGB^.b:= round(vBmpRGBA[x].b*255);
            vPngRGB^.r:= round(vBmpRGBA[x].r*255);
            vPngRGB^.g:= round(vBmpRGBA[x].g*255);
          end;
        end;
        inc(vPngRGB);
      end;
    end;
  end;

var
  gif: TGifImage;
begin
  try
    // GIF images can do cool things with
    // a special case, so that these get used
    // dithering and palettes.  Treat as a
    if newCls = TGifImage then
    begin
      Result := newCls.Create;
      gif := TGifImage(Result);
      gif.DitherMode := dmFloydSteinberg;
      gif.ColorReduction := rmQuantizeWindows;
      Result.Assign (graphic);
    end
    else
    // PNG from 32 Bit Bitmap loose Alpha Channel
    // on doing an simple Assign
    if newCls = TPngImage then
    begin
      if (graphic is TBitmap) and ((graphic as TBitmap).PixelFormat = pf32Bit) then
      begin
        Result:= PNG4TransparentBitMap(graphic as TBitmap);
      end
      else
      begin
        Result := newCls.Create;
        Result.Assign (graphic)
      end;
    end
    else
    // do an simple assign for anything else
    begin
      Result := newCls.Create;
      Result.Assign (graphic)
    end;
  except
    FreeAndNil(Result);
  end
end;
{$endregion}

{ TFormMain }

(*----------------------------------------------------------------------*
 | TFormMain.actEditCopyExecute                                         |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditCopyExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Copy;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditCutExecute                                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditCutExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Cut;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actEditDeleteExecute                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditDeleteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.EditDelete;
end;


(*----------------------------------------------------------------------*
 | TFormMain.actEditPasteExecute                                        |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditPasteExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.Paste
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditRedoExecute                                           |
 |                                                                      |
 | Handler for Edit/Redo.  Tell the Form to redo changes.               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditRedoExecute(Sender: TObject);
var
  resForm: TFormResource;
  elem: TResExamElement;
begin
  resForm := ResourceForm;
  if Assigned(resForm) then
  begin
    resForm.Redo;
    fmResourceObject.Obj := resForm.ResourceDetails;
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      TResExamName(elem).Redo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | TFormMain.ActionEditSelectAll                                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditSelectAllExecute(Sender: TObject);
begin
  if ResourceForm <> nil then
    ResourceForm.SelectAll;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actEditUndoExecute                                           |
 |                                                                      |
 | Handler for Edit/Undo.  Tell the Form to undo changes.               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionEditUndoExecute(Sender: TObject);
var
  resForm: TFormResource;
  elem: TResExamElement;
begin
  resForm := ResourceForm;
  if Assigned(resForm) then
  begin

    resForm.Undo;   // Perform the undo
    fmResourceObject.Obj := resForm.ResourceDetails;

                    // Update the resource tree to reflect changes
                    // in the resource name/language
    UpdateResourceNodes (SelectedResourceDetails, vstResources.FocusedNode);
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      TResExamName(elem).Undo;
      vstResources.InvalidateNode(vstResources.FocusedNode)
    end;
  end;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileExitExecute                                 |
 |                                                                      |
 | FileExit action handler                                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileNewExecute                                  |
 |                                                                      |
 | FileNew action handler                                               |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileNewExecute(Sender: TObject);
begin
  if SaveChanges then
  begin
    FreeAndNil(FResourceModule);
    ClearUndoDetails;
    FResourceModule := TResModule.Create; // Create an empty .RES module
    FileName := '';
    UpdateDisplay(nil);
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileOpenExecute                                 |
 |                                                                      |
 | FileOpen action handler.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileOpenFileExecute(Sender: TObject);
begin
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear

  OpenDialog.InitialDir := MRUList.MRUDirectory;
  if OpenDialog.Execute then
    OpenFile(OpenDialog.FileName);
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFilePrintExecute                                |
 |                                                                      |
 | FilePrint action handler                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFilePrintExecute(Sender: TObject);
begin
  if PrintDialog.Execute then
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileSaveAsExecute                               |
 |                                                                      |
 | FileSaveAs action handler                                            |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileSaveAsExecute(Sender: TObject);
begin
  SaveFileAs;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.actFileSaveExecute                                 |
 |                                                                      |
 | File/Save handler.  If we know the file name, overwrite it.          |
 | Otherwise do File/Save As instead.                                   |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
begin
  SaveFile;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actHelpAboutExecute                                          |
 |                                                                      |
 | Display/execute the about box.                                       |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  NTAboutBox.Execute;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceAddResourceExecute                                |
 |                                                                      |
 | Display/execute the Add Resource dialog.                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceAddResourceExecute(Sender: TObject);
var
  dlg: TFormAddResource;
  res: TResourceDetails;
begin
  dlg := TFormAddResource.Create(nil);
  try
    if dlg.ShowModal = mrOk then
    begin
      res := dlg.ResourceDetailsClass.CreateNew (FResourceModule, 0, FResourceModule.GetUniqueResourceName(dlg.ResourceDetailsClass.GetBaseType));
      res.Dirty := True;
      FResourceModule.SortResources;
      UpdateDisplay(res);
    end;
  finally
    dlg.Free;
  end;
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceDeleteResourceExecute                             |
 |                                                                      |
 | Delete the selected resource, and delete the node(s) that refer to   |
 | it.                                                                  |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceDeleteResourceExecute(Sender: TObject);
var
  p: PVirtualNode;
  Details: TResourceDetails;
  idx: Integer;
  nextAction: Integer;
begin
  p := vstResources.FocusedNode;
  Details := GetNodeResourceDetails(p);
  if not Assigned(Details) then Exit;

  if vstResources.GetNextSibling(p) <> nil then
    nextAction := 1
  else
    if p.Index > 0 then
      nextAction := 2
    else
      nextAction := 0;


  idx := FResourceModule.IndexOfResource(Details);
  FResourceModule.DeleteResource(idx);

  if nextAction = 1 then
    Details := FResourceModule.ResourceDetails [idx]
  else
    if nextAction = 2 then
      Details := FResourceModule.ResourceDetails [idx - 1]
    else
      Details := nil;
  UpdateDisplay(Details);
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceExportResourceExecute                             |
 |                                                                      |
 | Display/execute the 'export resource' dialog.  This is either a      |
 | 'save' dialog, or a 'save picture' dialog, depending on the          |
 | resource to be exported.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceExportResourceExecute(Sender: TObject);
var
  pict: TPicture;
  cls, cls1: TGraphicClass;
  ext: string;
  newGraphic: TGraphic;
const
  pfNames: array [TPixelFormat] of string = ('Same as monitor', '2 Colour', '16 Colour', '256 Colour', '', '65536 Colour', 'High Colour', 'True Colour', '');
begin
  pict := nil;

  if fmResourceObject.Obj is TIconCursorGroupResourceDetails then
  begin
    pict := TPicture.Create;
    TIconCursorGroupResourceDetails(fmResourceObject.Obj).GetImage(pict)
  end
  else
    if fmResourceObject.Obj is TGraphicsResourceDetails then
    begin
      pict := TPicture.Create;
      TGraphicsResourceDetails(fmResourceObject.Obj).GetImage(pict);
    end;

  if Assigned(pict) then
  try
    if Assigned(pict.Graphic) then
    begin
      SavePictureDialog.InitialDir := PersistDirectoryName ['Export'];

      cls := TGraphicClass (pict.Graphic.ClassType);
      SavePictureDialog.DefaultExt := GraphicExtension (cls);

      if SavePictureDialog.Execute then
      begin
        ext := ExtractFileExt(SavePictureDialog.FileName);
        cls1 := GetGraphicsClass (ext);

        if cls <> cls1 then
        begin
          newGraphic := CreateCompatibleGraphic (pict.Graphic, cls1);
          try
            newGraphic.SaveToFile(SavePictureDialog.FileName);
          finally
            newGraphic.Free
          end
        end
        else
          pict.SaveToFile(SavePictureDialog.FileName);

        PersistDirectoryName ['Export'] := ExtractFilePath (SavePictureDialog.FileName)
      end
    end
  finally
    pict.Free
  end
  else if fmResourceObject.Obj is TResourceDetails then
  begin
    SaveDialog.Filter := rstAnyFileFilter;
    SaveDialog.InitialDir := ExtractFilePath (SaveDialog.FileName);
    SaveDialog.FileName := TResourceDetails(fmResourceObject.Obj).ResourceName;
    if SaveDialog.Execute then
      TResourceDetails(fmResourceObject.Obj).Data.SaveToFile(SaveDialog.FileName)
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportOtherResourceExecute                        |
 |                                                                      |
 | Import a 'User' resource                                             |
 |                                                                      |
 | Get the resource name and type from the file name and extension      |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportOtherResourceExecute(Sender: TObject);
var
  res: TResourceDetails;
  f: TMemoryStream;
  resType, resName: string;
begin
  OpenDialogAllFiles.InitialDir := PersistDirectoryName ['Import'];

  if OpenDialogAllFiles.Execute then
  begin
    f := TMemoryStream.Create;
    try
      f.LoadFromFile(OpenDialogAllFiles.FileName);

      resType := UpperCase(ExtractFileName(OpenDialogAllFiles.FileName));
      resName := SplitString ('.', resType);

      if resType = '' then
      begin
        resType := resName;
        resName := FResourceModule.GetUniqueResourceName(resType)
      end
      else
        if FResourceModule.FindResource(resType, resName, 0) <> nil then
          resName := FResourceModule.GetUniqueResourceName(resType);

      res := TResourceDetails.CreateResourceDetails(
        FResourceModule, 0,
        resName,
        resType,
        f.Size, f.Memory);

      FResourceModule.AddResource(res);

      res.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath (OpenDialogAllFiles.FileName);

      FResourceModule.SortResources;
      UpdateDisplay(res);
    finally
      f.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportRCDataResourceExecute                       |
 |                                                                      |
 | Import an RC data resource                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportRCDataResourceExecute(Sender: TObject);
var
  res: TResourceDetails;
  f: TMemoryStream;
begin
  OpenDialogAllFiles.InitialDir := PersistDirectoryName ['Import'];
  if OpenDialogAllFiles.Execute then
  begin
    f := TMemoryStream.Create;
    try
      f.LoadFromFile(OpenDialogAllFiles.FileName);

      res := TResourceDetails.CreateResourceDetails(
        FResourceModule, 0,
        FResourceModule.GetUniqueResourceName(IntToStr(Integer(RT_RCDATA))),
        IntToStr(Integer(RT_RCDATA)),
        f.Size, f.Memory);

      FResourceModule.AddResource(res);

      res.Dirty := True;

      PersistDirectoryName ['Import'] := ExtractFilePath (OpenDialogAllFiles.FileName);

      FResourceModule.SortResources;
      UpdateDisplay(res)
    finally
      f.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actResourceImportResourceExecute                             |
 |                                                                      |
 | Import a graphic resource                                            |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionResourceImportResourceExecute(Sender: TObject);
var
  ResourceDetailsClass: TResourcedetailsClass;
  ext: string;
  res: TResourceDetails;
  img: TPicture;
begin
  OpenPictureDialog.InitialDir := PersistDirectoryName ['Import'];

  if OpenPictureDialog.Execute then
  begin
    ext := UpperCase(ExtractFileExt(OpenPictureDialog.FileName));

    if ext = '.ICO' then
      ResourceDetailsClass := TIconGroupResourceDetails
    else
      if (ext = '.CUR') or (ext = '.ANI') then
        ResourceDetailsClass := TCursorGroupResourceDetails
      else
        if (ext = '.GIF') then
          ResourceDetailsClass := TGIFResourceDetails
        else
          if (ext = '.JPG') or (ext = '.JPEG') then
            ResourceDetailsClass := TJPEGResourceDetails
          else
            ResourceDetailsClass := TBitmapResourceDetails;

    res := ResourceDetailsClass.CreateNew (FResourceModule, 0, FResourceModule.GetUniqueResourceName(ResourceDetailsClass.GetBaseType));
    res.Dirty := True;

    if res is TIconCursorGroupResourceDetails then
      TIconCursorGroupResourceDetails(res).LoadImage(OpenPictureDialog.FileName)
    else
    begin
      img := TPicture.Create;
      try
        img.LoadFromFile(OpenPictureDialog.FileName);
        TGraphicsResourceDetails(res).SetImage(img);
      finally
        img.Free
      end
    end;

    PersistDirectoryName ['Import'] := ExtractFilePath (OpenPictureDialog.FileName);

    FResourceModule.SortResources;
    UpdateDisplay(res);
  end

end;

procedure TFormMain.ActionResourcePropertiesExecute(Sender: TObject);
var
  dlg: TdlgResourceProperties;
  fm: TFormResource;
  newLanguage: LCID;
  newName: WideString;
  res, r: TResourceDetails;
begin
  fm := ResourceForm;
  res := SelectedResourceDetails;

  if Assigned(res) and Assigned(fm) and not(res is TIconCursorResourceDetails) then
  begin
    dlg := TdlgResourceProperties.Create(nil);
    try
      dlg.ResourceDetails := res;

      if dlg.ShowModal = mrOK then
      begin
        newLanguage := dlg.Language;
        newName := dlg.EditName.Text;

        if res is TStringResourceDetails then
          newName := StringsIDToResID (newName);

        if (newLanguage <> res.ResourceLanguage) or (newName <> res.ResourceName) then
        begin
          r := FResourceModule.FindResource(res.ResourceType, newName, newLanguage);
          if Assigned(r) and (r <> res) and (r.ResourceLanguage = newLanguage) then
            raise Exception.Create(rstDuplicateResourceName);

          fm.AddUndoEntry(rstChangeResourceProperties);


          if newLanguage <> res.ResourceLanguage then
            res.ResourceLanguage := newLanguage;

          if newName <> res.ResourceName then
          begin
            res.ResourceName := newName;
            if res is TStringResourceDetails then
              newName := ResIdToStringsId (newName);

            UpdateResourceNodes (res, vstResources.FocusedNode);
            if res is TStringResourceDetails then
              SwitchView (res);
          end
          else
            vstResources.Update
        end
      end
    finally
      dlg.Free
    end
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewPropertiesExecute                                     |
 |                                                                      |
 | Display/execute the 'Properties' dialog                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewPropertiesExecute(Sender: TObject);
var
  dlg: TFormProperties;
begin
  dlg := TFormProperties.Create(nil);
  try
    dlg.ShowModal
  finally
    dlg.Free
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewStatusbarExecute                                      |
 |                                                                      |
 | Turn the status bar on/off                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewStatusbarExecute(Sender: TObject);
begin
  gProperties.ShowStatusBar := not gProperties.ShowStatusBar
end;

(*----------------------------------------------------------------------*
 | TFormMain.actViewToolbarExecute                                        |
 |                                                                      |
 | Turn the toolbar on/off.                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.ActionViewToolbarExecute(Sender: TObject);
begin
  gProperties.ShowToolbar := not gProperties.ShowToolbar;
end;

function TFormMain.ApplicationEventsHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
begin
  CallHelp := not(command = HELP_FINDER);
  if not CallHelp then
    PostMessage(Application.Handle, CM_INVOKEHELP, HELP_CONTENTS, 0);
  Result := False
end;

procedure TFormMain.ApplicationEventsMessage(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := not HHPreTranslateMessage(Msg);
end;

procedure TFormMain.CheckDetails(p: PVirtualNode; param: Integer;
  var continue: Boolean);
begin
  continue := Integer (GetNodeResourceDetails(p)) <> param
end;

function TFormMain.CloneResource(res: TResourceDetails; newName: string; newLanguage: Integer): TResourceDetails;
var
  i: Integer;
  icg, clicg: TIconCursorGroupResourceDetails;
  data: pointer;
begin
  if res is TIconCursorGroupResourceDetails then
    data := nil
  else
    data := res.Data.Memory;

  if newLanguage <> -1 then
    Result := TResourceDetails.CreateResourceDetails(
                                res.Parent,
                                newLanguage,
                                res.ResourceName,
                                res.GetBaseType, res.Data.Size, data)
  else
  begin
    if newName = '' then
      newName := FResourceModule.GetUniqueResourceName(res.GetBaseType);

    Result := TResourceDetails.CreateResourceDetails(
                                res.Parent,
                                res.ResourceLanguage,
                                newName,
                                res.GetBaseType, res.Data.Size, data)
  end;

  if res is TIconCursorGroupResourceDetails then
  begin
    icg := TIconCursorGroupResourceDetails(res);
    clicg := Result as TIconCursorGroupResourceDetails;
    for i := 0 to icg.ResourceCount - 1 do
      clicg.AddToGroup(TIconCursorResourceDetails(CloneResource(icg.ResourceDetails [i], '', newLanguage)));

      // Remove first empty image that was created along with
      // the group resource.  Note that you have to do this
      // *after* adding the other images - otherwise the newly
      // created group resource will be zapped!

    clicg.RemoveFromGroup(clicg.ResourceDetails [0]);
  end;
  Result.Dirty := True;
  FResourceModule.AddResource(Result);
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveChanges
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.FormDestroy                                        |
 |                                                                      |
 | Tidy up                                                              |
 *----------------------------------------------------------------------*)
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FResourceModule.Free;
  gProperties.Free;
  FExaminer.Free
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(fmResourceObject) then
  begin
    fmResourceObject.PreviewKey(key, Shift);
    FIgnoreChar := key = 0;
  end
end;

procedure TFormMain.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if FIgnoreChar then
    key := #0
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  vstResources.Header.Options := vstResources.Header.Options + [hoAutoResize];
  InitializeHTMLHelp;
  ToolBarMenu.Font := Screen.MenuFont;
  UseInternationalFont(vstresources.Font);
end;


function TFormMain.GetNodeElement(node: PVirtualNode;
  var elem: TResExamElement): Boolean;
var
  obj: TObject;
begin
  obj := vstResources.NodeObject [node];
  Result := Assigned(obj) and (obj is TResExamElement);
  if Result then
    elem := TResExamElement(obj)
  else
    elem := nil;
end;

function TFormMain.GetNodeResourceDetails(node: PVirtualNode): TResourceDetails;
var
  elem: TResExamElement;
begin
  if GetNodeElement(node, elem) and (elem is TResExamResource) then
    Result := TResExamResource(elem).ResourceDetails
  else
    Result := nil;
end;

function TFormMain.GetPersistDirectoryName(Section: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey('\' + PersistentPosition.ApplicationKey + '\Directories', False) then
      Result := Reg.ReadString(Section);
  finally
    Reg.Free
  end
end;

function TFormMain.GetResourceDetailsNode(Details: TResourceDetails): PVirtualNode;
begin
  Result := vstResources.ForEach(CheckDetails, Integer(Details))
end;

function TFormMain.IsDirty: Boolean;
begin
  if Assigned(FResourceModule) then
    Result := FResourceModule.Dirty
  else
    Result := False
end;

(*----------------------------------------------------------------------*
 | TFormMain.Loaded                                                       |
 |                                                                      |
 | Initialize.                                                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.Loaded;
begin
  inherited;
  SetCaption;
  gProperties := TPEResourceExplorerProperties.Create(self);
  PostMessage(handle, WM_INITIALIZE, 0, 0);
end;

(*----------------------------------------------------------------------*
 | TFormMain.mnuEditClick                                                 |
 |                                                                      |
 | Before displaying the 'Edit' menu, set the undo and redo             |
 | descriptions                                                         |
 *----------------------------------------------------------------------*)
procedure TFormMain.MenuItemEditClick(Sender: TObject);
var
  s: string;
  resForm: TFormResource;
  elem: TResExamElement;
  name: TResExamName;
begin
  resForm := ResourceForm;
  if Assigned(resForm) then
  begin
    s := resForm.UndoDescription;

    if s = '' then
      MenuItemEditUndo.Caption := FUndo
    else
      MenuItemEditUndo.Caption := Format(rstUndo, [s]);

    s := resForm.RedoDescription;

    if s = '' then
      MenuItemEditRedo.Caption := FRedo
    else
      MenuItemEditRedo.Caption := Format(rstRedo, [s])
  end
  else
  begin
    GetNodeElement(vstResources.FocusedNode, elem);
    if elem is TResExamName then
    begin
      name := TResExamName(elem);
      s := name.UndoDescription;
      if s = '' then
        MenuItemEditUndo.Caption := FUndo
      else
        MenuItemEditUndo.Caption := Format(rstUndo, [s]);

      s := name.RedoDescription;

      if s = '' then
        MenuItemEditRedo.Caption := FRedo
      else
        MenuItemEditRedo.Caption := Format(rstRedo, [s])
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.MRUList1PopupMenuClick                             |
 |                                                                      |
 | Open a file from the MRU list popup.                                 |
 *----------------------------------------------------------------------*)
procedure TFormMain.MRUListPopupMenuClick(Sender: TObject);
var
  item: TMenuItem;
  p: Integer;
begin
  if Sender is TMenuItem then
  begin
    item := TMenuItem (sender);
    p := Pos (' ', Item.Caption);
    if p > 0 then
      OpenFile(Copy(Item.Caption, p + 1, MaxInt))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.OpenFile                                           |
 |                                                                      |
 | Open the specified module or .res file                               |
 *----------------------------------------------------------------------*)
procedure TFormMain.OpenFile(const FileName: string);
var
  ext: string;
begin
  if SaveChanges then
  begin
    FreeAndNil(FResourceModule);
    ClearUndoDetails;

    ext := UpperCase(ExtractFileExt(FileName));
    if (ext = '.RES') or (ext = '.DCR') then
      FResourceModule := TResModule.Create
    else
      if (ext = '.RC') then
      begin
        FResourceModule := TRCModule.Create;
        TRCModule(FResourceModule).IncludePath := gProperties.IncludePath;
      end
      else
        if (Win32Platform = VER_PLATFORM_WIN32_NT) and (gProperties.ParserType = 0) then
          FResourceModule := TNTModule.Create
        else
          FResourceModule := TPEResourceModule.Create;

    FResourceModule.LoadFromFile(FileName);
    Self.FileName := FileName;
    MRUList.AddFile(FileName);
    UpdateDisplay(nil);
  end
end;

function TFormMain.ResourceForm: TFormResource;
begin
  if Assigned(fmResourceObject) and (fmResourceObject is TFormResource) then
    Result := TFormResource(fmResourceObject)
  else
    Result := nil;
end;

function TFormMain.SaveChanges: Boolean;
var
  s: string;
begin
  if Assigned(fmResourceObject) then
    fmResourceObject.TidyUp;
  if FileName = '' then
    s := rstUntitled
  else
    s := ExtractFileName(FileName);
  if ISDirty then
    case MessageBox(Handle, PChar(Format(rstChanges, [s])), PChar(Application.Title), MB_YESNOCANCEL or MB_ICONQUESTION) of
      ID_YES: Result := SaveFile;
      ID_CANCEL: Result := False;
      else
        Result := True
    end
  else
    Result := True
end;

function TFormMain.SaveFile: Boolean;
begin
  if FFileName = '' then
    Result := SaveFileAs
  else
  begin
    Application.ProcessMessages;
    FResourceModule.SaveToFile(FFileName);
    Result := True;
  end
end;

function TFormMain.SaveFileAs: Boolean;
var
  s, fName: string;
  newModule: TResModule;
  p: Integer;
  res: TResourceDetails;
  tp, nm: string;
  lg: Integer;
begin
  Result := False;
  Application.ProcessMessages;  // Ensures that toolbar button doesn't temporarily disappear
  if Assigned(FResourceModule) then
  begin
    fName := FileName;
    if FResourceModule is TRCModule then
    begin
      p := Length (fName);
      while p > 0 do
        if fName [p] = '.' then
          break
        else
          Dec(p);

      if p = 0 then
        fName := fName + '.res'
      else
        fName := Copy(s, 1, p - 1) + '.res';
    end;

    SaveDialog.FileName := fName;

    s := '';
    if fName <> '' then
    begin
      s := ExtractFileExt(fName);
      if Length (s) > 0 then
        s := Copy(s, 2, MaxInt);
    end
    else
      if FResourceModule is TResModule then
        s := 'RES';

    SaveDialog.DefaultExt := s;
    if (FResourceModule is TResModule) or (FResourceModule is TRCModule)  then
      SaveDialog.Filter := rstRESFilter
    else
      SaveDialog.Filter := rstModuleFilter + '|' + rstRESFilter;

    if SaveDialog.Execute then
    begin
      s := UpperCase(ExtractFileExt(SaveDialog.FileName));

      if ((FResourceModule is TPEModule) or (FResourceModule is TRCModule)) and ((s = '.DCR') or (s = '.RES')) then
      begin
        res := SelectedResourceDetails;
        if res <> nil then
        begin
          nm := res.ResourceName;
          tp := res.ResourceType;
          lg := res.ResourceLanguage;
        end
        else
        begin
          nm := '';
          lg := 0;
          tp := ''
        end;
        newModule := TResModule.Create;
        newModule.Assign (FResourceModule);
        FreeAndNil(FResourceModule);
        ClearUndoDetails;
        FResourceModule := newModule;
        if nm <> '' then
          res := newModule.FindResource(tp, nm, lg)
        else
          res := nil;
        UpdateDisplay(res);
      end;
      FResourceModule.SaveToFile(SaveDialog.FileName);
      FileName := SaveDialog.FileName;
      MRUList.AddFile(FileName);
      Result := True;
    end
  end
end;

function TFormMain.SelectedResourceDetails: TResourceDetails;
begin
  Result := GetNodeResourceDetails(vstResources.FocusedNode);
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetCaption                                                   |
 |                                                                      |
 | Set the caption to display the loaded file.                          |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetCaption;
var
  st: string;
begin
  if FileName = '' then
    st := Application.Title
  else
  begin
    st := ExtractFileName(FileName + ' - ' + Application.Title);
    if IsDirty then
      st := '*' + st
  end;

  Caption := st
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetFileName                                                  |
 |                                                                      |
 | Set the file name.                                                   |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetFileName(const Value: string);
begin
  if FileName <> Value then
  begin
    FFileName := Value;
    SetCaption
  end
end;

(*----------------------------------------------------------------------*
 | TFormMain.SetFormMenuButton                                            |
 |                                                                      |
 | Turn on or off the '...object' menu button.  This is set to the      |
 | Form's menu item.                                                    |
 *----------------------------------------------------------------------*)
procedure TFormMain.SetFormMenuButton(item: TMenuItem);
begin
  if Assigned(item) then
  begin
    ToolButtonResourceObject.Caption := item.Caption;
    ToolButtonResourceObject.MenuItem := item;
    ToolButtonResourceObject.Visible := True;
  end
  else
  begin
    ToolButtonResourceObject.Visible := False;
    ToolButtonResourceObject.MenuItem := nil;
    ToolButtonResourceObject.Caption := '...object'
  end
end;


procedure TFormMain.SetPersistDirectoryName(Section: string;
  const Value: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    if Reg.OpenKey('\' + PersistentPosition.ApplicationKey + '\Directories', True) then
      Reg.WriteString (Section, Value)
  finally
    Reg.Free
  end
end;

type
  TObjectFormRec = record
    Details: TClass;
    Form: TResourceObjectFormClass;
  end;

const
  NoObjectDetails = 14;
  NoHeaderForms = 3;

var
  ObjectForms: array [0..NoObjectDetails - 1] of TObjectFormRec = (
    (Details: TIconResourceDetails;                    Form: TFormIconGraphicsResource),
    (Details: TCursorResourceDetails;                  Form: TFormCursorGraphicsResource),
    (Details: TGraphicsResourceDetails;                Form: TFormGraphicsResource),
    (Details: TTextResourceDetails;                    Form: TFormTextResource),
    (Details: TIconCursorGroupResourceDetails;         Form: TFormGroupResource),
    (Details: TVersionInfoResourceDetails;             Form: TFormVersionResource),
    (Details: TMenuResourceDetails;                    Form: TFormMenuResource),
    (Details: TDialogResourceDetails;                  Form: TFormDialogResource),
    (Details: TRCDataDescriptionResourceDetails;       Form: TFormRCDataDescriptionResource),
    (Details: TRCDataPackagesResourceDetails;          Form: TFormPackagesResource),
    (Details: TRCDataFormResourceDetails;              Form: TFormRCDataFormResource),
    (Details: TXPManifestResourceDetails;              Form: TFormXPManifestResource),
    (Details: TAcceleratorResourceDetails;             Form: TFormAcceleratorResource),
    (Details: TResourceDetails;                        Form: TFormRawResource)  // Must be last entry!
  );

(*----------------------------------------------------------------------*
 | procedure TFormMain.SwitchView (Details: TObject)                     |
 |                                                                      |
 | 'details' can be:                                                    |
 |                                                                      |
 |   *  A TResourceDetails object                                       |
 |   *  nil                                                             |
 *----------------------------------------------------------------------*)
procedure TFormMain.SwitchView(Details: TObject);
var
  i: Integer;
  FormClass: TResourceObjectFormClass;
begin { SwitchView }
  FormClass := nil;
  if Integer(Details) > 32 then        // It's a genuine TResourceDetails or TImageSection
  begin
    for i := 0 to NoObjectDetails - 1 do
      if Details is ObjectForms [i].Details then
      begin
        FormClass := ObjectForms [i].Form;
        break
      end
  end;

  // FormClass is now a valid Form class - or nil

  if not Assigned(fmResourceObject) or not Assigned(FormClass) or not(fmResourceObject.ClassType = FormClass) then
  begin

    if Assigned(fmResourceObject) then  // Get rid of the old resource Form
    begin
      SetFormMenuButton(nil);
      FreeAndNil(fmResourceObject);
    end;

    if Assigned(FormClass) then         // Create the new resource Form
    begin
      fmResourceObject := FormClass.Create(nil);
      fmResourceObject.Parent := PanelResource;
      fmResourceObject.TabStop := True;
      fmResourceObject.ResourceModule := FResourceModule;
      SetFormMenuButton (fmResourceObject.Menu);
      fmResourceObject.Show;
      fmResourceObject.Font := Self.Font;
      fmResourceObject.Obj := Details;
    end
  end
  else  // Form class is valid, and hasn't changed. Update the form.

    if Assigned(fmResourceObject) then
      fmResourceObject.obj := Details;
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.UpdateActions                                      |
 |                                                                      |
 | Update the action list, depending on the state.                      |
 *----------------------------------------------------------------------*)
procedure TFormMain.UpdateActions;
var
  res: TResourceDetails;
  elem: TResExamElement;
  resForm: TFormResource;
begin
  GetNodeElement(vstResources.FocusedNode, elem);
  if (elem is TResExamResource) then
    res := TResExamResource(elem).ResourceDetails
  else
    res := nil;

  ActionResourceDeleteResource.Enabled := res <> nil;
  resForm := ResourceForm;
  if Assigned(resForm) then
  begin
    ActionEditUndo.Enabled := resForm.CanUndo;
    ActionEditRedo.Enabled := resForm.CanRedo;

    ActionEditCut.Enabled := resForm.CanCut;
    ActionEditCopy.Enabled := resForm.CanCopy;
    ActionEditPaste.Enabled := resForm.CanPaste;
    ActionEditSelectAll.Enabled := resForm.CanSelectAll;
    ActionEditDelete.Enabled := resForm.CanDelete;

    ActionResourceExportResource.Enabled := Assigned(res);
    ActionResourceClone.Enabled := Assigned(res);
    ActionResourceProperties.Enabled := Assigned(res) and not(res is TIconCursorResourceDetails);
  end
  else
  begin
    if elem is TResExamName then
    begin
      ActionEditUndo.Enabled := TResExamName(elem).CanUndo;
      ActionEditRedo.Enabled := TResExamName(elem).CanRedo
    end
    else
    begin
      ActionEditUndo.Enabled := False;
      ActionEditRedo.Enabled := False;
    end;

    ActionEditCut.Enabled := False;
    ActionEditCopy.Enabled := False;
    ActionEditPaste.Enabled := False;
    ActionEditSelectAll.Enabled := False;
    ActionResourceExportResource.Enabled := False;
    ActionResourceProperties.Enabled := False;
    ActionResourceClone.Enabled := False;
    ActionEditDelete.Enabled := False;
  end;

  if FResourceModule is TRCModule then
    ActionFileSave.Enabled := False
  else
    ActionFileSave.Enabled := IsDirty;

  if FWasDirty <> IsDirty then
  begin
    SetCaption;
    FWasDirty := IsDirty
  end;
(*
  if Assigned(ActiveControl) then
  StatusBarMain.Panels [0].Text := ActiveControl.Name; *)
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.UpdateDisplay                                      |
 |                                                                      |
 | Update the display by building the tree view.                        |
 *----------------------------------------------------------------------*)
procedure TFormMain.UpdateDisplay(selectDetails: TResourceDetails);
var
  resSection: TResExamSection;

begin { UpdateDisplay }
  SwitchView (nil);

  if not Assigned(FResourceModule) then Exit;

  FExaminer.SetResourceModule(FResourceModule, false);
  resSection := FExaminer.ResourceSection;

  vstResources.BeginUpdate;
  try
    if Assigned(resSection) then
      vstResources.RootNodeCount := resSection.Count
    else
      vstResources.RootNodeCount := 0;
    vstResources.ReinitNode(nil, true);
  finally
    vstResources.EndUpdate
  end;

  if Assigned(SelectDetails) then
  begin
    vstResources.SelectAndFocusNode(GetResourceDetailsNode(SelectDetails));
    SwitchView (SelectDetails)
  end
  else
  begin
    SwitchView (nil);
    vstResources.FullCollapse;
    vstResources.SelectAndFocusNode(vstResources.GetFirst);
  end
end;

procedure TFormMain.WmAddImageResource(var msg: TMessage);
var
  o: TResourceDetails;
  grp: TIconCursorGroupResourceDetails;
  res: TIconCursorResourceDetails;
  p: PVirtualNode;

begin
  p := vstResources.FocusedNode;
  o := GetNodeResourceDetails(p);

  if Assigned(o) and (o is TIconCursorResourceDetails) then
  begin
    res := TIconCursorResourceDetails(o);
    p := p.Parent
  end
  else
    res := nil;

  if (p <> nil) and (res <> nil) then
  begin
    o := GetNodeResourceDetails(p);

    if o is TIconCursorGroupResourceDetails then
    begin
      grp := TIconCursorGroupResourceDetails(o);
      res := TIconCursorResourceDetails(CloneResource(res, '', -1));
      grp.AddToGroup (res);
      FResourceModule.SortResources;
      UpdateDisplay(res);
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TFormMain.WmInitialize                                       |
 |                                                                      |
 | WM_INITIALIZE handler.  Start with a blank .res file.                |
 *----------------------------------------------------------------------*)
procedure TFormMain.WmInitialize(var msg: TMessage);
begin
  FUndo := ActionEditUndo.Caption;
  FRedo := ActionEditRedo.Caption;
  FExaminer := TResourceExaminer.Create(nil);

  if ParamCount = 0 then
    ActionFileNew.Execute
  else
    OpenFile(ParamStr (1))
end;

(*----------------------------------------------------------------------*
 | TFormMain.WmPropertiesChanged                                          |
 |                                                                      |
 | Handle our 'WM_PROPERTIESCHANGED' message.  Apply persistent         |
 | properties                                                           |
 *----------------------------------------------------------------------*)
procedure TFormMain.WmPropertiesChanged(var msg: TMessage);
begin
  ToolBarMain.Visible := gProperties.ShowToolbar;
  ActionViewToolbar.Checked := ToolBarMain.Visible;

  StatusBarMain.Visible := gProperties.ShowStatusBar;
  ActionViewStatusbar.Checked := StatusBarMain.Visible;
  UseInternationalFont(vstResources.Font);

  if Assigned(fmResourceObject) then
    fmResourceObject.UpdateFonts
end;

procedure TFormMain.WmStatusBar(var Msg: TMessage);
begin
  if Msg.lParam <> 0 then
    StatusBarMain.Panels [1].Text := PChar(Msg.lParam) + '     ';

  if Msg.wParam <> 0 then
    StatusBarMain.Panels [0].Text := PChar(Msg.wParam);
end;

procedure TFormMain.ActionResourceCloneExecute(Sender: TObject);
var
  ResDetails: TResourceDetails;
  DialogCloneResource: TFormCloneResource;
begin
  ResDetails := SelectedResourceDetails;
  if Assigned(ResDetails) then
  begin
    DialogCloneResource := TFormCloneResource.Create(nil);
    try
      DialogCloneResource.ResourceDetails := ResDetails;
      if DialogCloneResource.ShowModal = mrOk then
      begin
        if DialogCloneResource.RadioButtonByLanguage.Checked then
          ResDetails := CloneResource(ResDetails, '', DialogCloneResource.Language)
        else
          ResDetails := CloneResource(ResDetails, DialogCloneResource.EditName.Text, -1);
        FResourceModule.SortResources;
        UpdateDisplay(ResDetails);
      end
    finally
      DialogCloneResource.Free;
    end;
  end
end;

procedure TFormMain.ActionResourceGrabExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.vstResourcesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  SwitchView (GetNodeResourceDetails(Node));
end;

procedure TFormMain.vstResourcesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Details: TResourceDetails;
begin
  if Kind in [ikOverlay, ikState] then
    Exit;

  Details := GetNodeResourceDetails(Node);
  if Assigned(Details) then
    ImageIndex := GetTypeImage(Details.ResourceType)
  else
    if vsExpanded in Node^.States then
      ImageIndex := imgOpenFolder
    else
      ImageIndex := imgClosedFolder;
end;

procedure TFormMain.vstResourcesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; var CellText: string);
var
  elem: TResExamElement;
begin
  if GetNodeElement(Node, elem) then
    CellText := elem.DisplayName
end;

procedure TFormMain.vstResourcesIncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);
var
  elem: TResExamElement;
begin
  if GetNodeElement(Node, elem) then
    Result := CompareText(SearchText, elem.DisplayName);
end;

procedure TFormMain.vstResourcesInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  elem: TResExamElement;
begin
  if GetNodeElement(Node, elem) then
    ChildCount := elem.Count
  else
    ChildCount := 0
end;

procedure TFormMain.vstResourcesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  elem, pelem: TResExamElement;
begin
  if not Assigned(ParentNode) then
    pelem := FExaminer.ResourceSection
  else
    GetNodeElement(ParentNode, pelem);

  if Assigned(pelem) then
  begin
    elem := pelem.Element [Node.Index];
    vstResources.NodeObject [Node] := elem;
    if elem.Count > 0 then
      Include(InitialStates, ivsHasChildren)
  end
end;

procedure TFormMain.vstResourcesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Elem1, Elem2: TResExamElement;
begin
  if GetNodeElement(Node1, Elem1) and GetNodeElement(Node2, Elem2) then
    Result := CompareText(Elem1.DisplayName, Elem2.DisplayName);
end;

procedure TFormMain.vstResourcesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := EditAllowed (Node);
end;

function TFormMain.EditAllowed(node: PVirtualNode): Boolean;
var
  elem: TResExamElement;
  tp: TResExamType;
begin
  Result := False;
  if GetNodeElement(node, elem) then
    if elem is TResExamName then
    begin
      Result := True;
      node := node^.Parent;
      if GetNodeElement(node, elem) and (elem is TResExamType) then
      begin
        tp := TResExamType(elem);
        if StrToIntDef (tp.Name, -1) = Integer(RT_STRING) then
          Result := False
      end
    end
    else
      if (elem is TResExamType) then
      begin
        tp := TResExamType(elem);
        Result:= tp.Name = tp.DisplayName
      end
end;

procedure TFormMain.vstResourcesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  elem: TResExamElement;

begin
  if GetNodeElement(node, elem) then
    if (elem is TResExamName) then
      TResExamName(elem).Name := NewText
    else
      if (elem is TResExamType) then
        TResExamType(elem).Name := NewText;

  vstResources.InvalidateNode(node)
end;

procedure TFormMain.UpdateResourceNodes(res: TResourceDetails;
  node: PVirtualNode);
var
  nm, tp: WideString;
  elem: TResExamElement;
begin
  if Assigned(res) and Assigned(node) then
  begin
    if res is TStringResourceDetails then
      nm := ResIdToStringsId (res.ResourceName)
    else
      nm := res.ResourceName;

    tp := res.ResourceType;

    while Assigned(node) do
    begin
      if GetNodeElement(node, elem) then
        if elem is TResExamName then
          TResExamName(elem).Name := nm
        else
          if elem is TResExamType then
            TResExamType(elem).Name := tp;
      vstResources.InvalidateNode(node);
      node := node.Parent;
      if node = vstResources.RootNode then
        break
    end
  end
end;

end.
