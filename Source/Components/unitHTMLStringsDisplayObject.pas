unit unitHTMLStringsDisplayObject;

interface

uses
  WinAPI.Windows, System.Classes, System.SysUtils, Vcl.Graphics, Vcl.Forms,
  Vcl.OleCtrls, System.Win.ComObj, WinAPI.ActiveX, WinAPI.ShellAPI,
  WinAPI.ShlObj, SHDocVw, MSHTML, ComponentMessageDisplay;

type
  THTMLStringsDisplayObjectLink = class (TWinControlObjectLink)
  private
    FOrigObj: TStrings;
    FRendering: Boolean;
    FNavigating: Boolean;
    FXanaLink: string;
    procedure DoOnDocumentComplete (Sender: TObject; const pDisp: IDispatch;
      const URL: OleVariant);
    procedure DoOnBeforeNavigate2 (Sender: TObject; const pDisp: IDispatch;
      const URL: OleVariant; const Flags: OleVariant;
      const TargetFrameName: OleVariant; const PostData: OleVariant;
      const Headers: OleVariant; var Cancel: WordBool);
    procedure DoOnNewWindow2 (Sender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure RecalcHeight;
    procedure LoadFromString (const st: string);
  protected
    class function DisplaysObject(obj: TObject): Boolean; override;
    procedure SetHeight(const Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetObj (const Value: TObject); override;
    function GetBusy: Boolean; override;
  public
    constructor Create (AOwner: TMessageDisplay; AObj: TObject; codepage: Integer); override;
    destructor Destroy; override;
    procedure Stop; override;
  end;

implementation

uses
  ComponentExWebBrowser;

resourcestring
  rstCantSetHeight = 'Can''t set height of this object';

{ THTMLStringsDisplayObjectLink }

constructor THTMLStringsDisplayObjectLink.Create(AOwner: TMessageDisplay;
  AObj: TObject; codepage: Integer);
var
  Ctrl: TExWebBrowser;
  xanalink: string;
  p: Integer;
begin
  FOrigObj := AObj as TStrings;
  Ctrl := TEXWebBrowser.Create(AOwner.Owner);
  inherited Create (AOwner, Ctrl, codepage);
  Ctrl.OnDocumentComplete := DoOnDocumentComplete;
  Ctrl.UIProperties.EnableContextMenu := True;
  Ctrl.Width := AOwner.Parent.Width - Margin * 2 - GetSystemMetrics (SM_CXVSCROLL);
  Ctrl.Height := AOwner.Parent.Height;
  if (FOrigObj.Count > 0) and (Copy (FOrigObj [0], 1, 16) = '<HTML><XanaLink>') then
  begin
    xanaLink := Copy (FOrigObj [0], 17, MaxInt);
    p := Pos ('</XanaLink>', xanaLink);
    if p > 0 then
      xanaLink := Copy (xanaLink, 1, p - 1);
    FXanaLink := xanaLink;
    Ctrl.OnNewWindow2 := DoOnNewWindow2;
    Ctrl.Navigate(xanaLink)
  end
  else
  begin
    Ctrl.OnBeforeNavigate2 := DoOnBeforeNavigate2;
    Ctrl.OnNewWindow2 := DoOnNewWindow2;
    Ctrl.Offline := True;
    LoadFromString (FOrigObj.Text)
  end;
  FRendering := True;
end;

destructor THTMLStringsDisplayObjectLink.Destroy;
begin
  Obj.Free;
  inherited;
end;

class function THTMLStringsDisplayObjectLink.DisplaysObject(
  obj: TObject): Boolean;
var
  s: TStrings;
  i: Integer;
  st, st1: string;
begin
  Result := False;
  if obj is TStrings then
  begin
    s := TStrings (obj);
    for i := 0 to s.Count - 1 do        // Is it HTML ??
    begin
      st := Trim (s [i]);

      if st = '' then                   // Ignore blank lines
        Continue;

      if Copy (st, 1, 2) = '<!' then    // Ignore HTML Comments (eg. <!DOCTYPE
        Continue;

      st1 := Uppercase (Copy (st, 1, 5));
      if st1 = '<HTML' then
        Result := True;

      if st1 = '<BODY' then
        Result := True;

      if st1 = '<HEAD' then
        Result := True;

      break
    end
  end
end;

procedure THTMLStringsDisplayObjectLink.DoOnBeforeNavigate2(
  Sender: TObject; const pDisp: IDispatch; const URL, Flags, TargetFrameName,
  PostData, Headers: OleVariant; var Cancel: WordBool);
var
  urlStr: string;
begin
  urlStr := url;

  if urlStr <> 'about:blank' then
  begin
    Cancel := True;
    ShellExecute (HWND_DESKTOP, 'open', PChar (urlStr), nil, Nil, SW_NORMAL);
    Ctrl.Invalidate
  end
end;
procedure THTMLStringsDisplayObjectLink.DoOnDocumentComplete(
  Sender: TObject; const pDisp: IDispatch; const URL: OleVariant);
var
  Ctrl: TExWebBrowser;
  doc: IHTMLDocument2;
  elm: IHTMLElement2;
begin
  try
    Ctrl := TExWebBrowser (obj);
    if Supports (Ctrl.Document, IHTMLDocument2, doc) then
    begin
      elm := doc.ActiveElement as IHTMLElement2; //body as IHTMLElement2;

      if Assigned(elm) then
      begin
        Ctrl.Width := elm.scrollWidth;
        Ctrl.Height := elm.scrollHeight;
        Owner.RecalcBounds;
      end
    end
  finally
    FRendering := False;
  end
end;

procedure THTMLStringsDisplayObjectLink.DoOnNewWindow2(Sender: TObject;
  var ppDisp: IDispatch; var Cancel: WordBool);
var
  Ctrl: TExWebBrowser;
begin
  Ctrl := TExWebBrowser (obj);
  Cancel := not Ctrl.UIProperties.OpenLinksInNewWindow
end;

function THTMLStringsDisplayObjectLink.GetBusy: Boolean;
var
  b: TExWebBrowser;
begin
//  Application.ProcessMessages;
  try
    if Assigned(obj) then
    begin
      b := TExWebBrowser (obj);
      Result := b.Busy;

      if Result then
        if b.ReadyState = READYSTATE_INTERACTIVE then
          Result := False;
    end
    else
      Result := False
  except
    Result := True
  end
end;

procedure THTMLStringsDisplayObjectLink.LoadFromString(const st: string);
begin
  if FNavigating then
    Exit;

  FNavigating := True;
  try
    TExWebBrowser (obj).LoadFromString(st);
  finally
    FNavigating := False
  end;
end;

procedure THTMLStringsDisplayObjectLink.RecalcHeight;
var
  Ctrl: TExWebBrowser;
  doc: IHTMLDocument2;
  elm: IHTMLElement2;
begin
  Ctrl := TExWebBrowser (obj);
  if Supports (Ctrl.Document, IHTMLDocument2, doc) then
  begin
    elm := doc.body as IHTMLElement2;

    if Assigned(elm) then
    begin
      Ctrl.Height := elm.scrollHeight;
      Owner.RecalcBounds
    end;
  end;
end;

procedure THTMLStringsDisplayObjectLink.SetHeight(const Value: Integer);
begin
end;

procedure THTMLStringsDisplayObjectLink.SetObj(const Value: TObject);
begin
  FOrigObj := Value as TStrings;
  Stop;
  if FRendering or FNavigating or Busy then
  begin
    WinAPI.Windows.Beep (440, 10);
    Exit;
  end;
  Ctrl.Width := Owner.MessageWidth;
  Ctrl.Height := Owner.ClientHeight;
  FRendering := True;
  LoadFromString (FOrigObj.Text);
end;

procedure THTMLStringsDisplayObjectLink.SetWidth(const Value: Integer);
begin
  inherited;
  RecalcHeight;
end;

procedure THTMLStringsDisplayObjectLink.Stop;
var
  n: Integer;
  b: TExWebBrowser;
begin
  if Busy then
    if Assigned(obj) then
    begin
      b := TExWebBrowser (obj);
      try
        n := 0;
        while b.ReadyState < READYSTATE_COMPLETE do
        begin
          Sleep (100);
          if b.ReadyState = READYSTATE_INTERACTIVE then
            Inc(n);
//          Application.ProcessMessages;
          if n > 2 then
            break;
        end;
        if b.ReadyState = READYSTATE_INTERACTIVE then
          b.Stop;
      except
      end;
      Sleep (200);
    end;
end;

initialization
  RegisterDisplayObjectLink (THTMLStringsDisplayObjectLink);
end.
