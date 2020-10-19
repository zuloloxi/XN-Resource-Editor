unit unitHTMLHelpViewer;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils;

function HHPreTranslateMessage (var Msg: TMsg): Boolean;
procedure InitializeHTMLHelp;

implementation

uses
  System.HelpIntfs, Vcl.Forms, unitHTMLHelp;

type
  TWHCommandType = (
    twhContext,
    twhCommand,
    twhContents,
    twhQuit
  );

  THTMLHelpViewer = class(TInterfacedObject, ICustomHelpViewer,
    IExtendedHelpViewer, IHelpSelector)
  private
    FViewerID: Integer;
    FLastCommandType: TWHCommandType;
  public
    FHelpManager: IHelpManager;
    procedure InternalShutDown;

    { ICustomHelpViewer }
    function GetViewerName: String;
    function UnderstandsKeyWord(const HelpString: String): Integer;
    function GetHelpStrings(const HelpString: String): TStringList;
    function CanShowTableOfContents: Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: String);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;

    { IExtendedHelpViewer }
    function UnderstandsTopic(const Topic: String): Boolean;
    procedure DisplayTopic(const Topic: String);
    function UnderstandsContext(const ContextID: THelpContext;
      const HelpFileName: string): Boolean;
    procedure DisplayHelpByContext(const ContextID: THelpContext;
      const HelpFileName: string);

    property HelpManager: IHelpManager read FHelpManager write FHelpManager;
    property ViewerID: Integer read FViewerID;

    { IHelpSelector}
    function SelectKeyWord(KeyWords: TStrings): Integer;
    function TableOfContents(Contents: TStrings): Integer;
  end;

var
  HTMLHelpViewer: THTMLHelpViewer;
  HelpSystem: IHelpSystem;
  dwHHCookie: DWORD;
  GInitialized: Boolean = False;

resourcestring
  rstHTMLHelp = 'HTML Help';

{ THTMLHelpViewer }

function HHPreTranslateMessage (var Msg: TMsg): Boolean;
begin
  Result := HtmlHelp (0, Nil, HH_PRETRANSLATEMESSAGE, DWORD (@Msg)) = 0
end;

procedure InitializeHTMLHelp;
begin
  if not GInitialized then
  begin
    HtmlHelp (0, nil, HH_INITIALIZE, DWORD (@dwHHCookie));
    HTMLHelpViewer := THTMLHelpViewer.Create;
    System.HelpIntfs.RegisterViewer(HTMLHelpViewer, HTMLHelpViewer.FHelpManager);
    GetHelpSystem (helpSystem);
    helpSystem.AssignHelpSelector(HTMLHelpViewer);
    GInitialized := True
  end
end;

function THTMLHelpViewer.CanShowTableOfContents: Boolean;
begin
  Result := True;
end;

procedure THTMLHelpViewer.DisplayHelpByContext(const ContextID: THelpContext;
  const HelpFileName: string);
begin

end;

procedure THTMLHelpViewer.DisplayTopic(const Topic: String);
begin

end;

function THTMLHelpViewer.GetHelpStrings(
  const HelpString: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(GetViewerName + ': ' + HelpString)
end;

function THTMLHelpViewer.GetViewerName: String;
begin
  Result := rstHTMLHelp;
end;

procedure THTMLHelpViewer.InternalShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(ViewerID);
    if Assigned(FHelpManager) then HelpManager := nil;
  end;
end;

procedure THTMLHelpViewer.NotifyID(const ViewerID: Integer);
begin
  FViewerID := ViewerID;
end;

function THTMLHelpViewer.SelectKeyWord(KeyWords: TStrings): Integer;
var
  p: Integer;
  st: string;
begin
  Result := 0;
  while Result < keyWords.Count do
  begin
    st := keyWords [Result];
    p := Pos (':', st);
    if p >= 0 then
      st := Copy (st, 1, p - 1);

    if st = rstHTMLHelp then
      Exit
    else
      Inc(Result);
  end;
  Result := -1
end;

procedure THTMLHelpViewer.ShowHelp(const HelpString: String);
begin

end;

procedure THTMLHelpViewer.ShowTableOfContents;
var
  fn, pn: string;
begin
  fn := FHelpManager.GetHelpFile;
  pn := ExtractFilePath (ParamStr (0));
  fn := pn + fn;
  HTMLHelp (Application.Handle, PChar (fn), HH_DISPLAY_TOC, 0);
end;

procedure THTMLHelpViewer.ShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then HelpManager := nil;
end;

procedure THTMLHelpViewer.SoftShutDown;
begin
  FLastCommandType := twhQuit;
  HTMLHelp(0, nil, HH_CLOSE_ALL, 0);
end;

function THTMLHelpViewer.TableOfContents(Contents: TStrings): Integer;
begin
  Result := 0;
  while Result < contents.Count do
    if contents [result] = rstHTMLHelp then
      exit
    else
      Inc(Result);
  Result := -1;
end;

function THTMLHelpViewer.UnderstandsContext(const ContextID: THelpContext;
  const HelpFileName: String): Boolean;
begin
  Result := False;
end;

function THTMLHelpViewer.UnderstandsKeyWord(
  const HelpString: String): Integer;
var
  params: THHAKlink;
  fn, pn: string;
begin
  fn := FHelpManager.GetHelpFile;
  pn := ExtractFilePath (ParamStr (0));
  fn := pn + fn;
  HTMLHelp (Application.Handle, PChar (fn), HH_DISPLAY_TOPIC, 0);

  params.cbStruct := SizeOf (params);
  params.pszKeyWords := PChar (HelpString);
  params.pszUrl := nil;
  params.pszMsgText := nil;
  params.pszMsgTitle := nil;
  params.pszWindow := nil;
  params.fReserved := False;

  HTMLHelp (Application.Handle, PChar (fn), HH_ALINK_LOOKUP, DWORD (@params));
  Result := 1;

end;

function THTMLHelpViewer.UnderstandsTopic(const Topic: String): Boolean;
begin
  Result := False;
end;

initialization

finalization
  if GInitialized then
  begin
    if Assigned(HTMLHelpViewer.FHelpManager) then
      HTMLHelpViewer.InternalShutDown;

    HtmlHelp (0, Nil, HH_UNINITIALIZE, dwHHCookie);  // Access violation!
  end
end.
