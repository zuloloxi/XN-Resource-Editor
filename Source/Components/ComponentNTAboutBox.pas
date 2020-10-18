// 21/06/2001  Use 'sysutils' version info instead of calling GetVersionInfoEx

unit ComponentNTAboutBox;

interface

uses
  WinAPI.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls,
  ComponentHyperlinkButton;

type
  TFormNTAboutBox = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    icoProduct: TImage;
    stProduct: TLabel;
    stVersion: TLabel;
    stCopyright: TLabel;
    lblSupport: TLabel;
    Label1: TLabel;
    stLicense1: TLabel;
    stLicense2: TLabel;
    Bevel1: TBevel;
    stMemAvail: TLabel;
    icoProduct1: TImage;
    OKBtn: TButton;
    stThankYou: TLabel;
    lbDonations: TListBox;
    procedure FormShow(Sender: TObject);
  private
    FThanksTo: string;
    procedure GetRegistrationInformation (isNT: Boolean; var owner, organization: string);
  end;

  TNTAboutBox = class(TComponent)
  private
    FCopyright: string;
    FDisplaySupportLink: Boolean;
    FThanksTo: string;
  public
    procedure Execute;
  published
    property Copyright: string read FCopyright Write FCopyright;
    property DisplaySupportLink: Boolean read FDisplaySupportLink write FDisplaySupportLink;
    property ThanksTo: string read FThanksTo write FThanksTo;
  end;

  function LoadGifResource (const resName: string; image: TImage): Boolean;

implementation

uses
  System.Win.Registry, Vcl.Imaging.GIFImg;

{$R *.DFM}

function LoadGifResource (const resName: string; image: TImage): Boolean;
var
  g: TGifImage;
  rs: TResourceStream;
begin
  Result := False;
  g := Nil;
  if FindResource (hInstance, PChar (resName), 'GIF') <> 0 then
  try
    rs := TResourceStream.Create(HInstance, resName, 'GIF');
    try
      if rs.Size > 0 then
      begin
        g := TGifImage.Create;
        g.LoadFromStream(rs);
        image.Picture.Assign(g);
        Result := True
      end
    finally
      g.Free;
      rs.Free
    end;
  except
  end;
end;

procedure TFormNTAboutBox.GetRegistrationInformation (isNT: Boolean; var owner, organization: string);
var
  product: string;
  p: Integer;
  reg: TRegistry;
  gotDetails: Boolean;
begin
  gotDetails := False;
  product := ExtractFileName (Application.ExeName);
  p := Pos ('.', product);
  if p > 0 then Delete (product, p, Length (product));
  reg := TRegistry.Create (KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey (Format('Software\Woozle\%s\CurrentVersion', [product]), False) then
      if reg.ValueExists ('RegisteredOwner') and reg.ValueExists ('RegisteredOrganization') then
      begin
        owner := reg.ReadString ('RegisteredOwner');
        organization := reg.ReadString ('RegisteredOrganization');
        gotDetails := True
      end
  finally
    reg.Free
  end;

  if not gotDetails then
  begin
    owner := 'Owner';
    organization := 'Organization';

    reg := TRegistry.Create (KEY_READ);
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      if isNT then
        product := 'Windows NT'
      else
        product := 'Windows';

      if reg.OpenKey (Format('Software\Microsoft\%s\CurrentVersion', [product]), False) then
      begin
        owner := reg.ReadString ('RegisteredOwner');
        organization := reg.ReadString ('RegisteredOrganization');
        gotDetails := True
      end
    finally
      reg.Free
    end;

    if gotDetails then
    try
      reg := TRegistry.Create (KEY_READ or KEY_WRITE);
      try
        reg.RootKey := HKEY_LOCAL_MACHINE;
        if reg.OpenKey (Format('Software\Woozle\%s\CurrentVersion', [product]), True) then
        begin
          reg.WriteString ('RegisteredOwner', owner);
          reg.WriteString ('RegisteredOrganization', organization)
        end
      finally
        reg.Free
      end
    except
    end
  end
end;

procedure TFormNTAboutBox.FormShow(Sender: TObject);
var
  memInfo: TMemoryStatus;
  os, owner, organization, st: string;
  size, zero: DWORD;
  buffer, pBuffer: pointer;
  info: PVSFixedFileInfo;
begin
  GlobalMemoryStatus (memInfo);
  Caption := 'About ' + Application.Title;

  if not LoadGifResource (Application.Title, icoProduct) then
    if Assigned(Application.Icon) then
      icoProduct.Picture.Icon := Application.Icon;

  st := Application.Title;

  size := GetFileVersionInfoSize (PChar (Application.ExeName), zero);
  if size > 0 then
  begin
    GetMem (buffer, size);
    if not GetFileVersionInfo (PChar (Application.ExeName), zero, size, buffer) then
      RaiseLastOSError;

    if not VerQueryValue (buffer, '\', pBuffer, size) then
      RaiseLastOSError;

    info := PVSFixedFileInfo (pBuffer);

    TabSheet1.Caption := 'About ' + st;

    st := st + Format(' Version %d.%d.%d.%d', [HiWord (info^.dwProductVersionMS), LoWord (info^.dwProductVersionMS), HiWord (info^.dwProductVersionLS), LoWord (info^.dwProductVersionLS)])
  end;

  if FThanksTo = '' then
  begin
    TabSheet2.Free;
    TabSheet1.TabVisible := False
  end
  else
  begin
    stThankYou.Caption := 'Many thanks for the generous donations from the following kind people!  Without these donations, ' + Application.Title + ' couldn''t have been written';
    lbDonations.Items.Text := FThanksTo
  end;

  PageControl1.ActivePageIndex := 0;

  stProduct.Caption := st;

  os := '';
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  case Win32MajorVersion of
    3, 4: os := 'Windows NT';
    5: if Win32MinorVersion = 0 then
          os := 'Windows 2000'
        else
          os := 'Windows XP'
  end
  else
  case Win32MajorVersion of
    4: if Win32MinorVersion = 0 then
          os := 'Windows 95'
        else
          if Win32MinorVersion = 10 then
            os := 'Windows 98'
          else
            os := 'Windows ME'
  end;

  GetRegistrationInformation (Win32Platform = VER_PLATFORM_WIN32_NT, owner, organization);
  stLicense1.Caption := owner;
  stLicense2.Caption := organization;
  stVersion.Caption := Format('%s  (Build %d: %s)', [os, Win32BuildNumber, Win32CSDVersion]);
  stMemAvail.Caption := Format('Physical Memory Available to Windows: %10.0n KB', [memInfo.dwTotalPhys / 1024]);
  LoadGifResource (Application.Title+'1', icoProduct1);
end;

{ TNTAboutBox }

procedure TNTAboutBox.Execute;
var
  dlg: TFormNTAboutBox;
begin
  dlg := TFormNTAboutBox.Create (nil);
  try
    if Copyright <> '' then
      dlg.stCopyright.Caption := Copyright;

    if DisplaySupportLink then
    begin
      dlg.lblSupport.Visible := True;
    end;

    dlg.FThanksTo := FThanksTo;

    dlg.ShowModal
  finally
    dlg.Free
  end


end;

end.
