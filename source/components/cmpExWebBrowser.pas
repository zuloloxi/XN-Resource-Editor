unit cmpExWebBrowser;

interface

uses
  Windows, Messages, Menus, SysUtils, Classes, Controls, OleCtrls, SHDocVw, ActiveX, Forms, URLMon;

const
  CGID_DocHostCommandHandler : TGuid = '{f38bc242-b950-11d1-8918-00c04fc2c836}';

  CONTEXT_MENU_DEFAULT        =0;
  CONTEXT_MENU_IMAGE          =1;
  CONTEXT_MENU_CONTROL        =2;
  CONTEXT_MENU_TABLE          =3;
  CONTEXT_MENU_TEXTSELECT     =4;
  CONTEXT_MENU_ANCHOR         =5;
  CONTEXT_MENU_UNKNOWN        =6;
  CONTEXT_MENU_IMGDYNSRC      =7;
  CONTEXT_MENU_IMGART         =8;
  CONTEXT_MENU_DEBUG          =9;
  CONTEXT_MENU_VSCROLL        =10;
  CONTEXT_MENU_HSCROLL        =11;

type
  TDocHostUIInfo = record
    cbSize : ULONG;
    dwFlags : DWORD;
    dwDoubleClick : DWORD;
    pchHostCss : polestr;
    pchHostNS : polestr;
  end;
  pDocHostUIInfo = ^TDocHostUIInfo;

  IDocHostUIHandler = interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown; const pdispReserved: IDispatch):HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject; const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow; const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup:PGUID; const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD):HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
  end;

  IDocHostShowUI = interface(IUnknown)
    ['{c4d244b0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowMessage (hwnd : HWND; lpstrText : POLESTR; lpstrCaption : POLESTR; dwType : DWORD; lpstrHelpFile : POLESTR; dwHelpContext : DWORD; out result : LRESULT) : HRESULT; stdcall;
    function ShowHelp (hwnd : HWND; pszHelpFile : POLESTR; uCommand : UINT; dwData : DWORD; ptMouse : TPoint; out ppDispatchObjectHit : IDispatch) : HRESULT; stdcall;
  end;

  ICustomDoc = interface (IUnknown)
  ['{3050f3f0-98b5-11cf-bb82-00aa00bdce0b}']
    function SetUIHandler (const pUIHandler : IDocHostUIHandler) : HRESULT; stdcall;
  end;

  TExWebBrowser = class;

  TUIProperties = class (TPersistent)
  private
    fOwner : TExWebBrowser;

    fEnableContextMenu: boolean;
    fEnableScrollBars: boolean;
    fFlatScrollBars: boolean;
    fHas3DBorder: boolean;
    fOpenLinksInNewWindow: boolean;
    fEnableScripting: boolean;
    fShowImages: boolean;
    fShowActiveX: boolean;
    fEnableDownloadActiveX: boolean;
    fEnableJava: boolean;
  public
    constructor Create (AOwner : TExWebBrowser);
  published
    property EnableContextMenu : boolean read fEnableContextMenu write fEnableContextMenu;
    property EnableScrollBars : boolean read fEnableScrollBars write fEnableScrollBars;
    property FlatScrollBars : boolean read fFlatScrollBars write fFlatScrollBars;
    property Has3DBorder : boolean read fHas3DBorder write fHas3DBorder;
    property OpenLinksInNewWindow : boolean read fOpenLinksInNewWindow write fOpenLinksInNewWindow;

    property EnableScripting : boolean read fEnableScripting write fEnableScripting;
    property EnableJava : boolean read fEnableJava write fEnableJava;
    property EnableDownloadActiveX : boolean read fEnableDownloadActiveX write fEnableDownloadActiveX;

    property ShowImages : boolean read fShowImages write fShowImages default True;
    property ShowActiveX : boolean read fShowActiveX write fShowActiveX default True;
  end;

  TExWebBrowser = class(TWebBrowser, IDocHostUIHandler, IDocHostShowUI, IDispatch, IOleCommandTarget)
  private
    fUIProperties : TUIProperties;
    fURL : string;
    fExtMenuItem: TMenuItem;
    fOwnedObject: TObject;
//    fInternetSession : IInternetSession;
    { IDispatch }
    function IDispatch.Invoke = Invoke;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;

    function ShowMessage (hwnd : HWND; lpstrText : POLESTR; lpstrCaption : POLESTR; dwType : DWORD; lpstrHelpFile : POLESTR; dwHelpContext : DWORD; out reslt : LRESULT) : HRESULT; stdcall;
    function ShowHelp (hwnd : HWND; pszHelpFile : POLESTR; uCommand : UINT; dwData : DWORD; ptMouse : TPoint; out ppDispatchObjectHit : IDispatch) : HRESULT; stdcall;

    {IOleCommandTarget}
    function QueryStatus(CmdGroup: PGUID; cCmds: Cardinal;
      prgCmds: POleCmd; CmdText: POleCmdText): HResult; stdcall;
    function Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
      const vaIn: OleVariant; var vaOut: OleVariant): HResult; stdcall;

    { IDocHostUIHandler }
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT; const pcmdtReserved: IUnknown; const pdispReserved: IDispatch):HRESULT; stdcall;
    function GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT; stdcall;
    function ShowUI(const dwID: DWORD; const pActiveObject: IOleInPlaceActiveObject; const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;const pDoc: IOleInPlaceUIWindow): HRESULT; stdcall;
    function HideUI: HRESULT; stdcall;
    function UpdateUI: HRESULT; stdcall;
    function EnableModeless(const fEnable: BOOL): HRESULT; stdcall;
    function OnDocWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function OnFrameWindowActivate(const fActivate: BOOL): HRESULT; stdcall;
    function ResizeBorder(const prcBorder: PRECT; const pUIWindow: IOleInPlaceUIWindow; const fRameWindow: BOOL): HRESULT; stdcall;
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup:PGUID; const nCmdID: DWORD): HRESULT; stdcall;
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD):HRESULT; stdcall;
    function GetDropTarget(const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget): HRESULT; stdcall;
    function GetExternal(out ppDispatch: IDispatch): HRESULT; stdcall;
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT; stdcall;
    function FilterDataObject(const pDO: IDataObject; out ppDORet: IDataObject): HRESULT; stdcall;
    function GetURL: string;
    procedure SetURL(const Value: string);
  protected
    procedure Loaded; override;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;

    procedure LoadFromString (const st : string);
    procedure LoadFromStream (s : TStream; takeOwnership : boolean = false);

    property OwnedObject : TObject read fOwnedObject write fOwnedObject;
  published
    property UIProperties : TUIProperties read fUIProperties write fUIProperties;
    property URL : string read GetURL write SetURL;
  end;

implementation

uses ComObj, MSHTML, Registry;

const
  DOCHOSTUIFLAG_DIALOG = $00000001;
  DOCHOSTUIFLAG_DISABLE_HELP_MENU = $00000002;
  DOCHOSTUIFLAG_NO3DBORDER = $00000004;
  DOCHOSTUIFLAG_SCROLL_NO = $00000008;
  DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE = $00000010;
  DOCHOSTUIFLAG_OPENNEWWIN = $00000020;
  DOCHOSTUIFLAG_DISABLE_OFFSCREEN = $00000040;
  DOCHOSTUIFLAG_FLAT_SCROLLBAR = $00000080;
  DOCHOSTUIFLAG_DIV_BLOCKDEFAULT = $00000100;
  DOCHOSTUIFLAG_ACTIVATE_CLIENTHIT_ONLY = $00000200;
  DOCHOSTUIFLAG_OVERRIDEBEHAVIOURFACTORY = $00000400;
  DOCHOSTUIFLAG_CODEPAGELINKEDFONTS = $00000800;
  DOCHOSTUIFLAG_URL_ENCODING_DISABLE_UTF8 = $00001000;
  DOCHOSTUIFLAG_URL_ENCODING_ENABLE_UTF8 = $00002000;
  DOCHOSTUIFLAG_ENABLE_FORMS_AUTOCOMPLETE = $00004000;

{ TExWebBrowser }

constructor TExWebBrowser.Create(AOwner: TComponent);
///var
//  Factory : IClassFactory;
begin
  inherited Create (AOwner);
  fExtMenuItem := TMenuItem.Create(self);
  fUIProperties := TUIProperties.Create (self);
//  OleCheck (CoInternetGetSession (0, fInternetSession, 0));

//  if fInternetSession <> Nil then
//  begin
//    CoGetClassObject(Class_CIDMimeFilter, CLSCTX_SERVER, nil, IClassFactory, Factory);
//    OleCheck (fInternetSession.RegisterMimeFilter(Factory, Class_CIDMimeFilter, 'cid'));
//    OleCheck (fInternetSession.RegisterNameSpace(Factory, CLASS_CIDMIMEFilter, 'cid', 0, nil, 0))
//  end
end;

destructor TExWebBrowser.Destroy;
begin
  fUIProperties.Free;
  fOwnedObject.Free;
//  fInternetSession := Nil;

  inherited;
end;

(*----------------------------------------------------------------------*
 | TExWebBrowser.EnableModeless                                         |
 |                                                                      |
 | IE calls this to tell us that our dialogs should be modeless.        |
 *----------------------------------------------------------------------*)
function TExWebBrowser.EnableModeless(const fEnable: BOOL): HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
  const vaIn: OleVariant; var vaOut: OleVariant): HResult;
var
(*
  pDoc : IHTMLDocument2;
  pWindow : IHTMLWindow2;
  pEventObj : IHTMLEventObj;
  rgwszNames : array [0..4] of PWideChar;
  rgDispIDs : array [0..4] of TDispID;
  rgvaEventInfo : array [0..4] of OleVariant;
  params : DISPPARAMS;
  fContinueRunningScripts : boolean;
  i : Integer;
*)
  vIn, vOut : PVariantArg;
begin
  result := S_OK;

  vIn := PVariantArg (@vaIn);
  vOut := PVariantArg (@vaOut);

  if Assigned (CmdGroup) and IsEqualGUID (CmdGroup^, CGID_DocHostCommandHandler) then
  case nCmdID of
    OLECMDID_SHOWSCRIPTERROR:
      begin

        try
          // We could analyze the script error with the code below - see the
          // Microsoft document Q261003.  However, we just want to not have the
          // control display the popup...

          if vIn.vt <> 0 then;
(*
          rgwszNames [0] := SysAllocString ('errorLine');
          rgwszNames [1] := SysAllocString ('errorCharacter');
          rgwszNames [2] := SysAllocString ('errorCode');
          rgwszNames [3] := SysAllocString ('errorMessage');
          rgwszNames [4] := SysAllocString ('errorUrl');

          params.cArgs := 0;
          params.cNamedArgs := 0;

          pDoc := IUnknown (vIn^.unkVal) as IHTMLDocument2;
          pWindow := pDoc.parentWindow;
          pDoc := Nil;

          pEventObj := pWindow.event;

          for i := 0 to 4 do
          begin
            OleCheck (pEventObj.GetIDsOfNames(GUID_NULL, @rgwszNames [i], 1, LOCALE_SYSTEM_DEFAULT, @rgDispIDs [i]));
            OleCheck (pEventObj.Invoke(rgDispIDs [i], GUID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_PROPERTYGET, params, @rgvaEventInfo [i], Nil, Nil));
            SysFreeString (rgwszNames [i]);
          end;
*)
          vOut^.vt := VT_BOOL;
          vOut^.vbool := true; // Continue running scripts
        except
          result := E_FAIL
        end;
      end;
    else
      result := OLECMDERR_E_NOTSUPPORTED;
  end
  else
    result := OLECMDERR_E_UNKNOWNGROUP;
end;

(*----------------------------------------------------------------------*
 | TExWebBrowser.FilterDataObject                                       |
 |                                                                      |
 | IE calls this before putting data objects on the clipboard.  It      |
 | allows us to replace them or set them to nil.                        |
 |                                                                      |
 | The function returns S_FALSE meaning we didn't replace the object    |
 *----------------------------------------------------------------------*)
function TExWebBrowser.FilterDataObject(const pDO: IDataObject;
  out ppDORet: IDataObject): HRESULT;
begin
  ppDORet := Nil;
  result := S_FALSE;
end;

(*----------------------------------------------------------------------*
 | TExWebBrowser.GetDropTarget                                          |
 |                                                                      |
 | The IE control calls this when it's used as a drop target so we can  |
 | provide a different ppDropTarget if we want.  We don't.              |
 |                                                                      |
 | Parameters:                                                          |
 |   const pDropTarget: IDropTarget; out ppDropTarget: IDropTarget
 |                                                                      |
 | The function returns HRESULT
 *----------------------------------------------------------------------*)
function TExWebBrowser.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HRESULT;
begin
  ppDropTarget := Nil;
  result := E_NOTIMPL
end;

(*----------------------------------------------------------------------*
 | TExWebBrowser.GetExternal                                            |
 |                                                                      |
 | IE calls this to determine our IDispatch interface.  We don't have   |
 | one...                                                               |
 *----------------------------------------------------------------------*)
function TExWebBrowser.GetExternal(out ppDispatch: IDispatch): HRESULT;
begin
  ppDispatch := Application;
  result := S_OK
end;

function TExWebBrowser.GetHostInfo(var pInfo: TDOCHOSTUIINFO): HRESULT;
begin
  FillChar (pInfo, SizeOf (pInfo), 0);

  pInfo.cbSize := sizeof (TDOCHOSTUIINFO);
  pInfo.cbSize := SizeOf(pInfo);
  pInfo.dwFlags := 0;

  if not UIProperties.EnableScrollBars then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_SCROLL_NO;

  if UIProperties.FlatScrollBars then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_FLAT_SCROLLBAR;

  if not UIProperties.Has3DBorder then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER;

  if UIProperties.OpenLinksInNewWindow then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_OPENNEWWIN;

  if not UIProperties.EnableScripting then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE;

  result := S_OK;
end;

function TExWebBrowser.GetOptionKeyPath(var pchKey: POLESTR;
  const dw: DWORD): HRESULT;
begin
  result := S_FALSE;
end;

function TExWebBrowser.GetURL: string;
begin
  if (csDesigning in ComponentState) then
    result := fURL
  else
    result := self.LocationURL
end;

function TExWebBrowser.HideUI: HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;

  const
  DISPID_AMBIENT_DLCONTROL  = -5512;
  DLCTL_DLIMAGES            = $00000010;
  DLCTL_VIDEOS              = $00000020;
  DLCTL_BGSOUNDS            = $00000040;
  DLCTL_NO_SCRIPTS          = $00000080;
  DLCTL_NO_JAVA             = $00000100;
  DLCTL_NO_RUNACTIVEXCTLS   = $00000200;
  DLCTL_NO_DLACTIVEXCTLS    = $00000400;
  DLCTL_DOWNLOADONLY        = $00000800;

var
  ort : HRESULT;
  dlc : Integer;
begin
  result := inherited Invoke (DispID, IID, LocaleID, Flags, Params, VarResult, ExcepInfo, ArgErr);
  if (Flags and DISPATCH_PROPERTYGET <> 0) and (VarResult <> nil) then
  begin
    ort := result;
    result := S_OK;
    case DispID of
(*
      DISPID_AMBIENT_USERMODE:
        PVariant(VarResult)^ := True; // not (csDesigning in ComponentState);
*)

      DISPID_AMBIENT_DLCONTROL:
        begin
          if UIProperties.ShowImages then
          begin
            dlc := DLCTL_DLIMAGES or DLCTL_BGSOUNDS or DLCTL_VIDEOS
          end
          else
            dlc := 0;

          if not UIProperties.EnableJava then
            dlc := dlc or DLCTL_NO_JAVA;

          if not UIProperties.EnableScripting then
            dlc := dlc or DLCTL_NO_SCRIPTS;

          if not UIProperties.EnableDownloadActiveX then
            dlc := dlc or DLCTL_NO_DLACTIVEXCTLS;

          if not UIPRoperties.ShowActiveX then
            dlc := dlc or DLCTL_NO_RUNACTIVEXCTLS;

          PVariant(VarResult)^ := dlc
        end;
      else
        result := ort
    end
  end
end;

procedure TExWebBrowser.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
    if fURL = '' then
      Navigate ('about:blank')
    else
      Navigate (fURL);
end;

procedure TExWebBrowser.LoadFromStream(s: TStream; takeOwnership: boolean);
var
  ownership : TStreamOwnership;
  persistStreamInit : IPersistStreamInit;
  adapter : TStreamAdapter;

begin
  if Document = Nil then
    Navigate ('about:blank');
  if Supports (Document, IPersistStreamInit, persistStreamInit) then
  begin
    if takeOwnership then
      ownership := soOwned
    else
      ownership := soReference;
    adapter := TStreamAdapter.Create(s, ownership);
    OleCheck (persistStreamInit.InitNew);
    OleCheck (persistStreamInit.Load(adapter))
  end
end;

procedure TExWebBrowser.LoadFromString(const st: string);
begin
  LoadFromStream (TStringStream.Create(st), True);
end;

function TExWebBrowser.OnDocWindowActivate(const fActivate: BOOL): HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.OnFrameWindowActivate(
  const fActivate: BOOL): HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.QueryStatus(CmdGroup: PGUID; cCmds: Cardinal;
  prgCmds: POleCmd; CmdText: POleCmdText): HResult;
begin
  result := OLECMDERR_E_NOTSUPPORTED;
end;

function TExWebBrowser.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const fRameWindow: BOOL): HRESULT;
begin
  result := S_OK;
end;

procedure TExWebBrowser.SetURL(const Value: string);
begin
  fURL := Value;

  if (csLoading in ComponentState) or (csDesigning in ComponentState) then
    Exit;

  if Value = '' then
    Navigate ('about:blank')
  else
    Navigate (value);
end;

function TExWebBrowser.ShowContextMenu(const dwID: DWORD;
  const ppt: PPOINT; const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HRESULT;
type
  TPOMData = record
    id : DWORD;
    disp : IDispatch;
  end;

var
  pomData : TPOMData;

begin
  if UIProperties.EnableContextMenu then
    result := S_FALSE
  else
  begin
    result := S_OK;
    if Assigned (PopupMenu) then
    begin
      pomData.id := dwID;
      pomData.disp := pdispReserved;
      PopupMenu.Tag := Integer (@pomData);
      PopupMenu.Popup(ppt.X, ppt.Y);
    end
  end;
end;

function TExWebBrowser.ShowHelp(hwnd: HWND; pszHelpFile: POLESTR;
  uCommand: UINT; dwData: DWORD; ptMouse: TPoint;
  out ppDispatchObjectHit: IDispatch): HRESULT;
begin
  ppDispatchObjectHit := Nil;
  result := S_FALSE;
end;

function TExWebBrowser.ShowMessage(hwnd: HWND; lpstrText, lpstrCaption: POLESTR;
  dwType: DWORD; lpstrHelpFile: POLESTR; dwHelpContext: DWORD;
  out reslt: LRESULT): HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HRESULT;
begin
  result := S_OK;
end;

function TExWebBrowser.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HRESULT;
var
  vk : Integer;
begin
  result := S_OK;
  if not Assigned (lpMsg) or (lpMsg^.message <> WM_KEYDOWN) then
    Exit;

  vk := lpMsg^.wParam;

  if (vk = VK_TAB) or (vk = VK_RETURN) or (vk = VK_DELETE) then
  begin
    result := S_FALSE;
  end
  else
    result := S_OK
end;

function TExWebBrowser.TranslateUrl(const dwTranslate: DWORD;
  const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HRESULT;
begin
  result := S_FALSE;   // URL was not translated
end;

function TExWebBrowser.UpdateUI: HRESULT;
begin
  result := S_FALSE;
end;

{ TUIProperties }

constructor TUIProperties.Create(AOwner: TExWebBrowser);
begin
  fOwner := AOwner;
  fShowImages := True;
  fShowActiveX := True;
end;

end.
