(*======================================================================*
 | SSPIUtils for SSPI authentication & impersonation                    |
 |                                                                      |
 | Very complicated.  Supports pluggable security packages.  As well as |
 | providing the SSPI framework this unit also provides the plugin      |
 | NTLM security package, so it fully supports NTLM authentication      |
 | and challenge/response client & server sessions.                     |
 |                                                                      |
 |----------------------------------------------------------------------|
 | TSSPI can handle:                                                    |
 |                                                                      |
 | *  Client authentication to validate domain/user/password            |
 |    combinations.  At the moment it doesn't impersonate the validated |
 |    user.                                                             |
 |                                                                      |
 | *  Client challenge/response sessions.  Generate the correct strings |
 |    (based on the domain/user/password) to pass to an SSPI server.    |
 |                                                                      |
 | *  Server challenge/response sessions leading to impersonation of    |
 |    the client's credentials.                                         |
 |                                                                      |
 | *  SSL Client encryption/decryption.                                 |
 |                                                                      |
 |    NB!!!!!  Impersonation doesn't work unless the account you're     |
 |             running under has SeImpersonatePrivilege.  System and    |
 |             Administrator accounts have this by default.  For other  |
 |             accounts, you have to go to c:\windows\system32, run     |
 |             secpol.msc and add it to the 'Impersonate a client after |
 |             authentication' section in                               |
 |             'Local Policies\UserRightsAssignment'                    |
 |                                                                      |
 |----------------------------------------------------------------------|
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2006  All Rights Reserved                   |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      22/09/2005  CPWW  Original                                  |
 | 1.1      23/06/2006  CPWW  Added restricted access                   |
 *======================================================================*)
unit SSPIUtils;

interface

uses Windows, Classes, SysUtils, ConTnrs, sspi, schannel,unitMimeCodec;

type
TSSPI = class;
TSSPICredentials = class;
TSSPIContext = class;

TSSPIPackage = class
private
  fParent : TSSPI;
  fOwnsInfo : boolean;
  fPackageInfo : PSecPkgInfoW;
  fCredentials : TSSPICredentials;
  fClientContext : TSSPIContext;
  fServerContext : TSSPIContext;
  function GetmaxToken: LongWord;
public
  constructor Create (AParent : TSSPI);
  destructor Destroy; override;
  class function PackageName : string; virtual; abstract;

  property Parent : TSSPI read fParent;
  property MaxToken : LongWord read GetmaxToken;
end;
TSSPIPackageClass = class of TSSPIPackage;

TSSPICredentialsUse = (scuInBound, scuOutBound, scuBoth);
TSSPICredentials = class
private
  fParent: TSSPI;
  fPackage: TSSPIPackage;
  fHandle : SecHandle;
  fExpiry : TTimeStamp;
  fUse: TSSPICredentialsUse;

  procedure DoAcquire (const principal, loginID : string; authData : pointer);
  procedure SetUse(const Value: TSSPICredentialsUse);

public
  constructor Create (AParent : TSSPI; APackage : TSSPIPackage);
  destructor Destroy; override;
  procedure Clear;
  procedure Acquire; virtual; abstract;


  property Parent : TSSPI read fParent;
  property Package : TSSPIPackage read fPackage;
  property Use : TSSPICredentialsUse read fUse write SetUse;
  property Handle : SecHandle read fHandle;
end;
TSSPICredentialsClass = class of TSSPICredentials;

TSSPIContext = class
private
  fParent : TSSPI;
  fCredentials: TSSPICredentials;
  fHandle: CtxtHandle;
  fExpiry: TTimeStamp;
  fResp : AnsiString;
  fCtxtStreamSizes : SecPkgContext_StreamSizes;
  fStreamSizesValid : boolean;
  function GetCtxtStreamSizes: SecPkgContext_StreamSizes;
protected
  fStep : Integer;
  function GetRequestedFlags : LongWord; virtual; abstract;
  property CtxtStreamSizes : SecPkgContext_StreamSizes read GetCtxtStreamSizes;
public
  constructor Create (AParent : TSSPI; ACredentials : TSSPICredentials); virtual;
  destructor Destroy; override;
  procedure Clear; virtual;

  procedure ServerInitialize (const st : AnsiString);  virtual;
  function DoInitialize (const aTokenSourceName: string; var aIn, aOut: SecBufferDesc; const errorsToIgnore: array of SECURITY_STATUS): SECURITY_STATUS;
  function NextStep : AnsiString; virtual;abstract;
  procedure VerifyCertificate;

  function Decrypt (st : AnsiString) : AnsiString;
  function Encrypt (const st : AnsiString) : AnsiString;

  property Credentials : TSSPICredentials read fCredentials;
  property Handle : CtxtHandle read fHandle;
  property Parent : TSSPI read fParent;
end;
TSSPIContextClass = class of TSSPIContext;

//--------------------------------------------------------------
// TSSPI can handle:
//
// *  Client authentication to validate domain/user/password combinations.
//    At the moment it doesn't impersonate the valid user.

// *  Client challenge/response sessions.  Generate the correct strings (based
//    on the domain/user/password) to pass to an SSPI server.

// *  Server challenge/response sessions leading to impersonation of the
//    client's credentials

TSSPI = class
private
  fEnumerated : boolean;
  FPackageCount: DWORD;
  fFunctionTable : pSecurityFunctionTableW;
  FPackageEnumInfo : PSecPkgInfoW;
  fSecLibHandle : HMODULE;
  fInUsePackage : TSSPIPackage;
  fUserName: string;
  fPassword: string;
  fDomainName: string;
  fDebugMode: boolean;
  fServerAccessRestrictedAccounts : TStrings;

  procedure Enumerate;
  function GetPackage(idx: Integer): PSecPkgInfoW;
  function GetPackageCount: LongWord;
  function GetPackageName(idx: Integer): string;
  procedure SetDomainName(const Value: string);
  procedure SetPassword(const Value: string);
  procedure SetUserName(const Value: string);

  procedure ClearCredentials;
  function GetClientString (const AServerString : AnsiString) : AnsiString;
  function GetServerString(const clientString: AnsiString): AnsiString;
  function GetHTTPServerString(const HTTPClientString: AnsiString): AnsiString;
  function GetInUsePackageName: string;
  function GetServerRestrictedAccessAccounts: TStrings;
  function GetDomainName: string;
  function GetUserName: string;
  function GetMaxMessageSize: Integer;
protected
  fServerImpersonating: boolean;

  property Enumerated : boolean read fEnumerated;
  property Package [idx : Integer] : PSecPkgInfoW read GetPackage;

public
  constructor Create;
  destructor Destroy; override;

  procedure UsePackage (const packageName : string);
  procedure Authenticate;  // Client authentication

  function Decrypt (const st : AnsiString) : AnsiString;
  function Encrypt (const st : AnsiString) : AnsiString;
  procedure VerifyCertificate;
  property MaxMessageSize : Integer read GetMaxMessageSize;

  property ClientString [const AServerString : AnsiString] : AnsiString read GetClientString;
  property ServerString [const clientString : AnsiString] : AnsiString read GetServerString;
  property HTTPServerString [const HTTPClientString : AnsiString] : AnsiString read GetHTTPServerString;

  property PackageCount : LongWord read GetPackageCount;
  property PackageName [idx : Integer] : string read GetPackageName;
  property InUsePackageName : string read GetInUsePackageName;

  property DomainName : string read GetDomainName write SetDomainName;
  property UserName : string read GetUserName write SetUserName;
  property Password : string read fPassword write SetPassword;

  property ServerRestrictedAccessAccounts : TStrings read GetServerRestrictedAccessAccounts;
  property ServerImpersonating : boolean read fServerImpersonating;
  property DebugMode : boolean read fDebugMode write fDebugMode;
end;

TSSPINTLMPackage = class (TSSPIPackage)
public
  class function PackageName : string; override;
end;

TSSPINegotiatePackage = class (TSSPIPackage)
public
  class function PackageName : string; override;
end;

TSSPISSLPackage = class (TSSPIPackage)
public
  class function PackageName : string; override;
end;

TSSPIWinNTCredentials = class (TSSPICredentials)
public
  procedure Acquire; override;
end;

TSSPISSLCredentials = class (TSSPICredentials)
private
  fSChannelCred : SCHANNEL_CRED;
public
  procedure Acquire; override;
end;

TCustomSSPIConnectionContext = class (TSSPIContext)
private
  fStatus: SECURITY_STATUS;
  fOutBuffDesc, fInBuffDesc: SecBufferDesc;
  fInBuff: SecBuffer;
protected
  function DoUpdateAndGenerateReply(
    var aIn, aOut: SecBufferDesc;
    const aErrorsToIgnore: array of SECURITY_STATUS
    ): SECURITY_STATUS; virtual; abstract;
public
  constructor Create (AParent : TSSPI; ACredentials : TSSPICredentials); override;
  function UpdateAndGenerateReply(const aFromPeerToken: AnsiString; var aToPeerToken: AnsiString): Boolean;
end;

TSSPIClientConnectionContext = class (TCustomSSPIConnectionContext)
private
  fTargetName: string;
  function GenerateInitialChallenge(const aTargetName: string; var aToPeerToken: AnsiString): Boolean;
  function UpdateAndBuildType3Message(aServerType2Message: AnsiString): AnsiString;
protected
  function DoUpdateAndGenerateReply(
    var aIn, aOut: SecBufferDesc;
    const aErrorsToIgnore: array of SECURITY_STATUS
    ): SECURITY_STATUS; override;
  function GetRequestedFlags : LongWord; override;
public
  function NextStep : AnsiString; override;
end;

TSSPISSLClientConnectionContext = class (TSSPIClientConnectionContext)
protected
  function GetRequestedFlags : LongWord; override;
end;

TSSPIServerConnectionContext = class (TCustomSSPIConnectionContext)
private
  fOutBuff: SecBuffer;
  function HandleInitialChallenge : DWORD;
  function HandleSubsequentChallenge: DWORD;
public
  function NextStep : AnsiString; override;
  procedure ServerInitialize (const st : AnsiString); override;
  procedure Clear; override;
end;

ESSPI = class (Exception);

const
  DISABLE_MAX_PRIVILEGE = 1;
  SANDBOX_INERT         = 2;

  SE_GROUP_MANDATORY          = $00000001;
  SE_GROUP_ENABLED_BY_DEFAULT = $00000002;
  SE_GROUP_ENABLED            = $00000004;
  SE_GROUP_OWNER              = $00000008;
  SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;
  SE_GROUP_LOGON_ID           = $C0000000;
  SE_GROUP_RESOURCE           = $20000000;

procedure RegisterSecurityPackage (packageClass : TSSPIPackageClass; credentialsClass : TSSPICredentialsClass; clientContextClass, serverContextClass : TSSPIContextClass);
function CreateRestrictedToken (ExistingTokenHandle : THandle; Flags : DWORD;
                                 DisableSIDCount : DWORD; SidsToDisable : PSIDAndAttributes;
                                 DeletePrivilegeCount : DWORD; PrivilegesToDelete : PLUIDAndAttributes;
                                 RestrictedSIDCount : DWORD; SidsToRestrict : PSidAndAttributes;
                                 var NewTokenHandle : THandle) : BOOL; stdcall;


implementation

uses WinCrypt, unitNTSecurity;


type
TPackageClassInfo = class
private
  fSSPIPackageClass: TSSPIPackageClass;
  fSSPICredentialsClass : TSSPICredentialsClass;
  fClientSSPIContextClass : TSSPIContextClass;
  fServerSSPIContextClass : TSSPIContextClass;
public
  constructor Create (ASSPIPackageClass : TSSPIPackageClass; ASSPICredentialsClass : TSSPICredentialsClass; AClientSSPIContextClass, AServerSSPIContextClass : TSSPIContextClass);

  property SSPIPackageClass : TSSPIPackageClass read fSSPIPackageClass;
  property SSPICredentialsClass : TSSPICredentialsClass read fSSPICredentialsClass;
end;

var
  gRegisteredSecurityPackages : TObjectList;

resourcestring
  rstPackageNotSupported = 'Package %s not supported in this version';
  rstNoPackage = 'Package %s not installed on this computer';
  rstUnableToAcquireCredentials = 'Unable to acquire credentials';
  rstSSPICompleteTokenNotSupported = 'Complete token not supported';

function CreateRestrictedToken; external 'advapi32.dll';

procedure SSPICheck (err : SECURITY_STATUS; const fn : string = '');
var
  msg : string;
begin
  if (err and $8000000) <> 0 then
  begin
    if fn = '' then
      msg := 'SSPI Error:' + IntToHex (err, 8)
    else
      msg := 'SSPI Error in ' + fn + ':' + IntToHex (err, 8);

    raise Exception.Create(msg);
  end
end;

function FindSecurityPackage (const packageName : string) : TPackageClassInfo;
var
  i : Integer;
begin
  result := Nil;
  for i := 0 to gRegisteredSecurityPackages.Count - 1 do
    if CompareText (TPackageClassInfo (gRegisteredSecurityPackages [i]).SSPIPackageClass.PackageName, packageName) = 0 then
    begin
      result := TPackageClassInfo (gRegisteredSecurityPackages [i]);
      break
    end
end;

procedure RegisterSecurityPackage (packageClass : TSSPIPackageClass; credentialsClass : TSSPICredentialsClass; clientContextClass, serverContextClass : TSSPIContextClass);
var
  info : TPackageClassInfo;
begin
  info := FindSecurityPackage (packageClass.PackageName);
  if not Assigned (info) then
  begin
    info := TPackageClassInfo.Create(packageClass, credentialsClass, clientContextClass, serverContextClass);
    gRegisteredSecurityPackages.Add(info)
  end
  else
  begin
    info.fSSPICredentialsClass := credentialsClass;
    info.fClientSSPIContextClass := clientContextClass;
    info.fServerSSPIContextClass := serverContextClass;
  end
end;

function IsSecHandleValid (const handle : SecHandle) : boolean;
begin
  result := not ((handle.dwLower = ULONG (-1)) and (handle.dwUpper = ULONG (-1)))
end;

procedure InvalidateSecHandle (var handle : SecHandle);
begin
  handle.dwLower := ULONG (-1);
  handle.dwUpper := ULONG (-1)
end;

{ TSSPI }

procedure TSSPI.Authenticate;
begin
  if Assigned (fInUsePackage) then
  begin
    repeat
      fInUsePackage.fClientContext.NextStep
    until fInUsePackage.fClientContext.fStep = 2
  end
end;

procedure TSSPI.ClearCredentials;
begin
  if Assigned (fInUsePackage) then
    fInUsePackage.fCredentials.Clear
end;

constructor TSSPI.Create;
const
  SECURITY_DLL_NT       = 'security.dll';    {Do not translate}
  SECURITY_DLL_95       = 'secur32.dll';   {Do not translate}
  SECURITY_ENTRYPOINTW  = 'InitSecurityInterfaceW';    {Do not Localize}
var
  secDLLName : string;
  entrypoint: INIT_SECURITY_INTERFACE_W;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    secDLLName := SECURITY_DLL_NT
  else
    secDLLName := SECURITY_DLL_95;

  fSecLibHandle := LoadLibrary (PChar (secDLLName));
  if fSecLibHandle = 0 then
    RaiseLastOSError;

  entrypoint := GetProcAddress (fSecLibHandle, SECURITY_ENTRYPOINTW);

  fFunctionTable := entrypoint;
end;

function TSSPI.Decrypt(const st : AnsiString) : AnsiString;
begin
  result := fInUsePackage.fClientContext.Decrypt (st);
end;

destructor TSSPI.Destroy;
begin
  fServerAccessRestrictedAccounts.Free;
  fInUsePackage.Free;

  if Assigned (fFunctionTable) then
  begin
    if Assigned (fPackageEnumInfo) then
      fFunctionTable.FreeContextBuffer (fPackageEnumInfo);
  end;

  FreeLibrary (fSecLibHandle);
  inherited;
end;

function TSSPI.Encrypt(const st: AnsiString): AnsiString;
begin
  result := fInUsePackage.fClientContext.Encrypt (st);
end;

procedure TSSPI.Enumerate;
begin
  if not fEnumerated then
  begin
    fFunctionTable.EnumerateSecurityPackagesW (fPackageCount, fPackageEnumInfo);
    fEnumerated := True;
  end
end;

function TSSPI.GetClientString (const AServerString : AnsiString) : AnsiString;
begin
  if Assigned (fInUsePackage) then
  begin
    fInUsePackage.fClientContext.fResp := AServerString;
    result := fInUsePackage.fClientContext.NextStep
  end;
end;

function TSSPI.GetDomainName: string;
var
  use : SID_NAME_USE;
  sid : PSID;
  sidLen, dnLen : DWORD;
begin
  if (fDomainName = '') and fServerImpersonating then
  begin
    GetUserName;

    sidLen := 1024;
    GetMem (sid, sidLen);

    dnLen := 256;
    SetLength (fDomainName, dnLen);

    try
      if LookupAccountName (nil, PChar (fUserName), sid, sidLen, PChar (fDomainName), dnLen, use) then
        fDomainName := PChar (fDomainName)
    finally
      FreeMem (sid);
    end;
    
    result := fDomainName
  end
end;

function TSSPI.GetHTTPServerString(const HTTPClientString: AnsiString): AnsiString;
var
  decoder : TMIMEDecoder;
  encoder : TMIMEEncoder;
begin
  encoder := Nil;
  decoder := TMIMEDecoder.Create;
  try
    result := GetServerString (decoder.DecodeString(HTTPCLientString));
    if result <> '' then
    begin
      encoder := TMIMEEncoder.Create;
      result := encoder.EncodeString(result)
    end;
  finally
    encoder.Free;
    decoder.Free;
  end
end;

function TSSPI.GetInUsePackageName: string;
begin
  if Assigned (fInUsePackage) then
    result := fInUsePackage.PackageName
  else
    result := ''
end;

function TSSPI.GetMaxMessageSize: Integer;
begin
  if Assigned (fInUsePackage) then
    result := fInUsePackage.fClientContext.CtxtStreamSizes.cbMaximumMessage
  else
    result := 0
end;

function TSSPI.GetPackage(idx: Integer): PSecPkgInfoW;
begin
  Enumerate;

  result := fPackageEnumInfo;
  Inc (result, idx)
end;


function TSSPI.GetPackageCount: LongWord;
begin
  Enumerate;
  result := fPackageCount;
end;

function TSSPI.GetPackageName(idx: Integer): string;
begin
  result := Package [idx].Name
end;

function TSSPI.GetServerRestrictedAccessAccounts: TStrings;
begin
  if not Assigned (fServerAccessRestrictedAccounts) then
    fServerAccessRestrictedAccounts := TStringList.Create;

  result := fServerAccessRestrictedAccounts;
end;

function TSSPI.GetServerString(const clientString: AnsiString): AnsiString;
begin
  if Assigned (fInUsePackage) then
  begin
    fInUsePackage.fCredentials.Use := scuInBound;

    if fInUsePackage.fServerContext.fStep = 0 then
    begin
      fInUsePackage.fServerContext.Clear;
      fInUsePackage.fCredentials.DoAcquire('', '', Nil)
    end;

    fInUsePackage.fServerContext.ServerInitialize(clientString);
    result := fInUsePackage.fServerContext.NextStep
  end
end;

function TSSPI.GetUserName: string;
var
  len : DWORD;
begin
  if (fUserName = '') and fServerImpersonating then
  begin
    len := MAX_COMPUTERNAME_LENGTH;
    SetLength (fUserName, len+1);
    windows.GetUserName (PChar (fUserName), len);
    fUserName := PChar (fUserName);
  end;

  result := fUserName
end;

procedure TSSPI.SetDomainName(const Value: string);
begin
  if value <> fDomainName then
  begin
    ClearCredentials;
    fDomainName := Value
  end
end;

procedure TSSPI.SetPassword(const Value: string);
begin
  if value <> fPassword then
  begin
    ClearCredentials;
    fPassword := Value
  end
end;

procedure TSSPI.SetUserName(const Value: string);
begin
  if value <> fUserName then
  begin
    ClearCredentials;
    fUserName := Value
  end
end;

procedure TSSPI.UsePackage(const packageName: string);
var
  info : TPackageClassInfo;
begin
  info := FindSecurityPackage (packageName);

  if not Assigned (info) then
    raise ESSPI.CreateFmt (rstPackageNotSupported, [packageName]);

  FreeAndNil (fInUsePackage);

  fInUsePackage := info.SSPIPackageClass.Create(self);
  fInUsePackage.fCredentials := info.fSSPICredentialsClass.Create(self, fInUsepackage);

  if info.fClientSSPIContextClass <> Nil then
    fInUsePackage.fClientContext := info.fClientSSPIContextClass.Create(self, fInUsePackage.fCredentials);

  if info.fServerSSPIContextClass <> Nil then
    fInUsePackage.fServerContext := info.fServerSSPIContextClass.Create(self, fInUsePackage.fCredentials);
end;

procedure TSSPI.VerifyCertificate;
begin
  fInUsePackage.fClientContext.VerifyCertificate;
end;

{ TPackageClassInfo }

constructor TPackageClassInfo.Create(ASSPIPackageClass: TSSPIPackageClass; ASSPICredentialsClass : TSSPICredentialsClass; AClientSSPIContextClass, AServerSSPIContextClass : TSSPIContextClass);
begin
  fSSPIPackageClass := ASSPIPackageClass;
  fSSPICredentialsClass := ASSPICredentialsClass;
  fClientSSPIContextClass := AClientSSPIContextClass;
  fServerSSPIContextClass := AServerSSPIContextClass;
end;

{ TSSPIPackage }

constructor TSSPIPackage.Create(AParent: TSSPI);
var
  i : Integer;
begin
  fParent := AParent;

  if Parent.fEnumerated then
  begin
    for i := 0 to parent.PackageCount - 1 do
      if CompareText (parent.PackageName [i], PackageName) = 0 then
      begin
        fPackageInfo := parent.FPackageEnumInfo;
        Inc (fPackageInfo, i);
        break
      end
  end
  else
  begin
    if Parent.fFunctionTable.QuerySecurityPackageInfoW (PWideChar (WideString (PackageName)), fPackageInfo) = SEC_E_OK then
      fOwnsInfo := True
    else
      fPackageInfo := nil
  end;

  if not Assigned (fPackageInfo) then
    raise ESSPI.CreateFmt(rstNoPackage, [PackageName])
end;

destructor TSSPIPackage.Destroy;
begin
  if fOwnsInfo then
    Parent.fFunctionTable.FreeContextBuffer (fPackageInfo);

  if Parent.ServerImpersonating then
  begin
    RevertToSelf;
    Parent.fServerImpersonating := False
  end;

  fClientContext.Free;
  fServerContext.Free;
  fCredentials.Free;

  inherited;
end;

function TSSPIPackage.GetmaxToken: LongWord;
begin
  result := fPackageInfo.cbMaxToken
end;

{ TSSPINTLMPackage }

class function TSSPINTLMPackage.PackageName: string;
begin
  result := 'NTLM';
end;

{ TSSPICredentials }

procedure TSSPICredentials.Clear;
begin
  if IsSecHandleValid (fHandle) then
  begin
    Parent.fFunctionTable.FreeCredentialsHandle (@fHandle);
    InvalidateSecHandle (fHandle)
  end
end;

constructor TSSPICredentials.Create(AParent: TSSPI;
  APackage: TSSPIPackage);
begin
  fParent := AParent;
  fPackage := APackage;
  InvalidateSecHandle (fHandle);
end;

destructor TSSPICredentials.Destroy;
begin
  Clear;

  inherited;
end;

procedure TSSPICredentials.DoAcquire(const principal, loginID: string;
  authData: pointer);
var
  cu: ULONG;

  wPrincipal : WideString;
  wLoginId : WideString;

  wcPrincipal : PWideChar;
  wcLoginId : PWideChar;
begin
  Clear;

  cu := 0;
  case Use of
    scuInBound: cu := SECPKG_CRED_INBOUND;
    scuOutBound: cu := SECPKG_CRED_OUTBOUND;
    scuBoth: cu := SECPKG_CRED_BOTH;
  end;

  wPrincipal := principal;
  wLoginId := loginId;

  if wPrincipal = '' then
    wcPrincipal := nil
  else
    wcPrincipal := PWideChar (wPrincipal);

  if wLoginId = '' then
    wcLoginId := Nil
  else
    wcLoginId := PWideChar (wLoginId);

  SSPICheck (Parent.fFunctionTable.AcquireCredentialsHandleW (
    wcPrincipal, PWideChar (WideString (Package.PackageName)), cu, wcLoginId, authData, nil, nil, fHandle, fExpiry), rstUnableToAcquireCredentials);
end;

procedure TSSPICredentials.SetUse(const Value: TSSPICredentialsUse);
begin
  if fUse <> Value then
  begin
    Clear;
    fUse := Value
  end
end;

{ TSSPIWinNTCredentials }

procedure TSSPIWinNTCredentials.Acquire;
var
  ai: SEC_WINNT_AUTH_IDENTITY_W;

  wUser, wDomain, wPassword : WideString;
begin
  if (Length(Parent.UserName) > 0) and (Length(Parent.DomainName) > 0) then
  begin
    wUser := parent.UserName;
    wPassword := parent.Password;
    wDomain := Parent.DomainName;

    with ai do
    begin
      User := PWideChar(wUser);
      UserLength := Length(wUser);
      Domain := PWideChar(wDomain);
      DomainLength := Length(wDomain);
      Password := PWideChar(wPassword);
      PasswordLength := Length(wPassword);
      Flags := SEC_WINNT_AUTH_IDENTITY_UNICODE;
    end;

    DoAcquire ('', '', @ai)
  end
end;

{ TSSPIContext }

procedure TSSPIContext.Clear;
begin
  fStreamSizesValid := False;
  fStep := 0;
  if IsSecHandleValid (fHandle) then
    Parent.fFunctionTable.DeleteSecurityContext (@fHandle);
end;

constructor TSSPIContext.Create(AParent : TSSPI; ACredentials: TSSPICredentials);
begin
  fParent := AParent;
  fCredentials := ACredentials;
  InvalidateSecHandle (fHandle);
end;

function TSSPIContext.Decrypt(st : AnsiString) : AnsiString;
var
  buffers : SecBufferDesc;
  buffs : array [0..3] of SecBuffer;
  sizes : SecPkgContext_StreamSizes;
  buffer : PAnsiChar;
  mLen, err, len, i : Integer;
  s : AnsiString;
  hasExtraData : boolean;
begin
  result := '';
  len := Length (st);
  if len = 0 then
    exit;

  sizes := CtxtStreamSizes;

  mLen := sizes.cbHeader + sizes.cbTrailer + sizes.cbMaximumMessage;

  buffers.ulVersion := SECBUFFER_VERSION;
  buffers.cBuffers := 4;
  buffers.pBuffers := @buffs [0];

  GetMem (buffer, mLen);
  try
    repeat
      hasExtraData := False;
      Move (st [1], buffer^, len);
      buffs [0].BufferType := SECBUFFER_DATA;
      buffs [0].cbBuffer := len;
      buffs [0].pvBuffer := buffer;

      buffs [1].BufferType := SECBUFFER_EMPTY;
      buffs [1].cbBuffer := 0;
      buffs [1].pvBuffer := Nil;

      buffs [2].BufferType := SECBUFFER_EMPTY;
      buffs [2].cbBuffer := 0;
      buffs [2].pvBuffer := Nil;

      buffs [3].BufferType := SECBUFFER_EMPTY;
      buffs [3].cbBuffer := 0;
      buffs [3].pvBuffer := Nil;

      err := Parent.fFunctionTable.DecryptMessage (@fHandle, @buffers, 0, 0);
      SSPICheck (err);

      for i := Low (buffs) to High (buffs) do
      begin
        if buffs [i].BufferType = SECBUFFER_DATA then
        begin
          SetString (s, PChar (buffs [i].pvBuffer), buffs [i].cbBuffer);
          result := result +s
        end
      end;

      st := '';
      for i := Low (buffs) to High (buffs) do
      begin
        if buffs [i].BufferType = SECBUFFER_EXTRA then
        begin
          len := buffs [i].cbBuffer;
          SetString (s, PChar (buffs [i].pvBuffer), len);
          st := st + s;
          hasExtraData := True;
        end
      end;
    until not hasExtraData;
  finally
    FreeMem (buffer);
  end
end;

destructor TSSPIContext.Destroy;
begin
  Clear;
  inherited;
end;

function TSSPIContext.DoInitialize(const aTokenSourceName: string; var aIn,
  aOut: SecBufferDesc;
  const errorsToIgnore: array of SECURITY_STATUS): SECURITY_STATUS;
var
  tmp: PCtxtHandle;
  tmp2: PSecBufferDesc;
  r: ULONG;
  wTokenSource : WideString;
  wcTokenSource : PWideChar;
begin
  wTokenSource := aTokenSourceName;
  if wTokenSource = '' then
    wcTokenSource := Nil
  else
    wcTokenSource := PWideChar (wTokenSource);

  if IsSecHandleValid (fHandle) then
  begin
    tmp := @fHandle;
    tmp2 := @aIn;
  end
  else
  begin
    tmp := nil;
    tmp2 := nil;
  end;

  Result :=
    Parent.fFunctionTable.InitializeSecurityContextW(
    @Credentials.fHandle, tmp, wcTokenSource,
    GetRequestedFlags, 0, SECURITY_NATIVE_DREP, tmp2, 0,
    @fHandle, @aOut, r, fExpiry
    );
end;

function TSSPIContext.Encrypt(const st: AnsiString): AnsiString;
var
  buffers : SecBufferDesc;
  buffs : array [0..3] of SecBuffer;
  buffer : PAnsiChar;
  sizes : SecPkgContext_StreamSizes;
  aLen, mLen : Integer;
begin
  sizes := CtxtStreamSizes;

  aLen := Length (st);
  mLen := sizes.cbHeader + sizes.cbTrailer + sizes.cbMaximumMessage;
  GetMem (buffer, mLen);
  try
    buffers.ulVersion := SECBUFFER_VERSION;
    buffers.cBuffers := 4;
    buffers.pBuffers := @buffs [0];

    buffs [0].BufferType := SECBUFFER_STREAM_HEADER;
    buffs [0].cbBuffer := sizes.cbHeader;
    buffs [0].pvBuffer := buffer;

    buffs [1].BufferType := SECBUFFER_DATA;
    buffs [1].cbBuffer := aLen;
    buffs [1].pvBuffer := buffer + sizes.cbHeader;
    Move (st [1], buffs [1].pvBuffer^, aLen);

    buffs [2].BufferType := SECBUFFER_STREAM_TRAILER;
    buffs [2].cbBuffer := sizes.cbTrailer;
    buffs [2].pvBuffer := buffer + sizes.cbHeader + aLen;

    buffs [3].BufferType := SECBUFFER_EMPTY;
    buffs [3].cbBuffer := 0;
    buffs [3].pvBuffer := Nil;

    SSPICheck (Parent.fFunctionTable.EncryptMessage (@fHandle, 0, @buffers, 0), 'EncryptMessage');

    aLen := buffs [0].cbBuffer + buffs [1].cbBuffer + buffs [2].cbBuffer;
    SetString (result, buffer, aLen);
  finally
    FreeMem (buffer)
  end
end;

function TSSPIContext.GetCtxtStreamSizes: SecPkgContext_StreamSizes;
begin
  if not fStreamSizesValid then
  begin
    SSPICheck (Parent.fFunctionTable.QueryContextAttributesW (@fHandle, SECPKG_ATTR_STREAM_SIZES, @fCtxtStreamSizes), 'QueryContextAttributes');
    fStreamSizesValid := True;
  end;

  result.cbHeader := fCtxtStreamSizes.cbHeader;
  result.cbTrailer := fCtxtStreamSizes.cbTrailer;
  result.cbMaximumMessage := fCtxtStreamSizes.cbMaximumMessage;
  result.cBuffers := fCtxtStreamSizes.cBuffers;
  result.cbBlockSize := fCtxtStreamSizes.cbBlockSize
end;

procedure TSSPIContext.ServerInitialize(const st: AnsiString);
begin
  // Stub
end;

procedure TSSPIContext.VerifyCertificate;
begin

end;

{ TSSPIClientConnectionContext }

function TSSPIClientConnectionContext.DoUpdateAndGenerateReply(var aIn,
  aOut: SecBufferDesc;
  const aErrorsToIgnore: array of SECURITY_STATUS): SECURITY_STATUS;
begin
  Result := DoInitialize(PChar(fTargetName), aIn, aOut, []);
end;

function TSSPIClientConnectionContext.GenerateInitialChallenge(
  const aTargetName: string; var aToPeerToken: AnsiString): Boolean;
begin
  fCredentials.Use := scuOutBound;
  fCredentials.Acquire;

  fTargetName := aTargetName;
  Result := UpdateAndGenerateReply('', aToPeerToken);
end;

function TSSPIClientConnectionContext.GetRequestedFlags: LongWord;
begin
  result := 0;
end;

function TSSPIClientConnectionContext.NextStep : AnsiString;
begin
  case fStep of
    0 :
      begin
        GenerateInitialChallenge ('localhost', fResp);
        Inc (fStep)
      end;

    1 :
      begin
        fResp := UpdateAndBuildType3Message (fResp);
        Inc (fStep)
      end;

    else
      begin
        fResp := UpdateAndBuildType3Message (fResp);
        Inc (fStep)
      end;
  end;

  result := fResp
end;

function TSSPIClientConnectionContext.UpdateAndBuildType3Message(
  aServerType2Message: AnsiString): AnsiString;
begin
//  fCredentials.Use := scuInbound;
//  fCredentials.DoAcquire ('', '', Nil);
  UpdateAndGenerateReply (aServerType2Message, result)
end;

{ TCustomSSPIConnectionContext }

constructor TCustomSSPIConnectionContext.Create(AParent: TSSPI;
  ACredentials: TSSPICredentials);
begin
  inherited Create (AParent, ACredentials);
end;

function TCustomSSPIConnectionContext.UpdateAndGenerateReply(
  const aFromPeerToken: AnsiString; var aToPeerToken: AnsiString): Boolean;
var
  fOutBuff: SecBuffer;
begin
  if not IsSecHandleValid (fCredentials.fHandle) then
    raise ESSPI.Create('Credentials handle invalid');

  with fInBuff do begin
    BufferType := SECBUFFER_TOKEN;
    cbBuffer := Length(aFromPeerToken);
    pvBuffer := @(aFromPeerToken[1]);
  end;

  { prepare output buffer }
  with fOutBuff do begin
    BufferType := SECBUFFER_TOKEN;
    cbBuffer := Credentials.Package.MaxToken;
    pvBuffer := AllocMem(cbBuffer);
  end;
  with fOutBuffDesc do begin
    ulVersion := SECBUFFER_VERSION;
    cBuffers := 1;
    pBuffers := @fOutBuff;
  end;

  with fInBuffDesc do begin
    ulVersion := SECBUFFER_VERSION;
    cBuffers := 1;
    pBuffers := @fInBuff;
  end;
  try
    { do processing }
    fStatus := DoUpdateAndGenerateReply(fInBuffDesc, fOutBuffDesc, []);
    { complete token if applicable }
    case fStatus of
      SEC_I_COMPLETE_NEEDED,
      SEC_I_COMPLETE_AND_CONTINUE:
        begin
          SSPICheck (Parent.fFunctionTable.CompleteAuthToken(@Handle, @fOutBuffDesc), 'CompleteAuthToken');
          if fStatus = SEC_I_COMPLETE_NEEDED then
            fStatus := SEC_E_OK
          else
            fStatus := SEC_I_CONTINUE_NEEDED
        end;
    end;
    Result := (fStatus = SEC_I_CONTINUE_NEEDED) or (fOutBuff.cbBuffer > 0);
    if Result then
      with fOutBuff do
        SetString(aToPeerToken, PChar(pvBuffer), cbBuffer);
  finally
    FreeMem(fOutBuff.pvBuffer);
  end;
end;

{ TSSPIServerConnectionContext }

procedure TSSPIServerConnectionContext.Clear;
begin
  if Parent.ServerImpersonating then
  begin
    RevertToSelf;
    Parent.fServerImpersonating := False
  end;
  inherited;
end;

function TSSPIServerConnectionContext.HandleInitialChallenge : DWORD;
var
  r : SECURITY_STATUS;
  ContextAttributes : ULONG;
  LifeTime : TTimeStamp;
begin
  LifeTime.Time := 0;
  LifeTime.Date := 0;
  ContextAttributes := 0;
  r := Parent.fFunctionTable.AcceptSecurityContext (@Credentials.Handle, Nil, @fInBuffDesc, ASC_REQ_DELEGATE, SECURITY_NATIVE_DREP, @fHandle, @fOutBuffDesc, ContextAttributes, LifeTime);
  if r < 0 then
    Raise ESSPI.Create ('Unable to accept security context');
  SetLength (fResp, fOutBuff.cbBuffer);
  result := r;
end;

function TSSPIServerConnectionContext.HandleSubsequentChallenge : DWORD;
var
  r : SECURITY_STATUS;
  ContextAttributes : ULONG;
  LifeTime : TTimeStamp;
begin
  LifeTime.Time := 0;
  LifeTime.Date := 0;
  ContextAttributes := 0;
  r := Parent.fFunctionTable.AcceptSecurityContext (@Credentials.Handle, @fHandle, @fInBuffDesc, 0, SECURITY_NATIVE_DREP, @fHandle, @fOutBuffDesc, ContextAttributes, LifeTime);
  if r < 0 then
    Raise ESSPI.Create ('Unable to accept security context');
  SetLength (fResp, fOutBuff.cbBuffer);
  result := r;
end;

function TSSPIServerConnectionContext.NextStep: AnsiString;
var
  r, r1 : SECURITY_STATUS;

  procedure PerformDebugRealityCheck;
  var
    unLen : DWORD;
    userName : string;
  begin
    unLen := 256;
    SetLength (userName, unLen + 1);
    GetUserName (PChar (userName), unLen);
    userName := PChar (userName);
  end;

  procedure ImpersonateSSPIUser;
  var
    i : Integer;
    oldState : DWORD;
    accessToken, restrictedAccessToken : THandle;
    restrictingSids : array of TSidAndAttributes;
    restrictingSidCount : DWORD;
    sid : PSID;
    sidLen : DWORD;
    dnLen : DWORD;
    dn : string;
    use : SID_NAME_USE;
  begin
    oldState := $ffffffff;
    restrictedAccessToken := 0;
    accessToken := 0;
    if Parent.fFunctionTable.QuerySecurityContextToken (@fHandle, Pointer (accessToken)) = SEC_E_OK then
    try
      oldState := EnableNTPrivilege (SE_IMPERSONATE_PRIVILEGE_NAME, SE_PRIVILEGE_ENABLED);

      if not Assigned (Parent.fServerAccessRestrictedAccounts) or (Parent.fServerAccessRestrictedAccounts.Count = 0) then
        Parent.fServerImpersonating := ImpersonateLoggedOnUser (accessToken)
      else
      begin
        restrictingSIDCount := Parent.fServerAccessRestrictedAccounts.Count;
        SetLength (RestrictingSids, restrictingSIDCount);

        for i := 0 to restrictingSIDCount - 1 do
          RestrictingSids [i].Sid := Nil;

        try
          for i := 0 to restrictingSIDCount - 1 do
          begin
            sidLen := 1024;
            GetMem (sid, sidLen);
            dnLen := 256;
            SetLength (dn, dnLen);

            if not LookupAccountName (Nil, PChar (Parent.fServerAccessRestrictedAccounts [i]), sid, sidLen, @dn [1], dnLen, use) then
              RaiseLastOSError;

            ReallocMem (sid, sidLen);

            RestrictingSids [i].Sid := sid;
            RestrictingSids [i].Attributes := 0; // SE_GROUP_ENABLED;
          end;
          if not CreateRestrictedToken (accessToken, DISABLE_MAX_PRIVILEGE,
                                        0, Nil, 0, Nil, restrictingSIDCount, @restrictingSIDs [0], restrictedAccessToken) then
            RaiseLastOSError;
        finally
          for i := 0 to restrictingSIDCount - 1 do
            FreeMem (RestrictingSIDs [i].Sid);
        end;

        Parent.fServerImpersonating := ImpersonateLoggedOnUser (restrictedAccessToken);
      end
    finally
      CloseHandle (restrictedAccessToken);
      CloseHandle (accessToken);

      if oldState <> $ffffffff then
        EnableNTPrivilege (SE_IMPERSONATE_PRIVILEGE_NAME, oldState);
    end;

    if parent.DebugMode then
      PerformDebugRealityCheck;

  end;

begin { NextStep }
  case fStep of
    0 : r := HandleInitialChallenge;
    else
      r := HandleSubsequentChallenge
  end;

  if (r = SEC_I_COMPLETE_NEEDED) or (r = SEC_I_COMPLETE_AND_CONTINUE) then
  begin
    if Assigned (Parent.fFunctionTable.CompleteAuthToken) then
    begin
      r1 := Parent.fFunctionTable.CompleteAuthToken (@fHandle, @fOutBuffDesc);

      if r1 < 0 then
        raise ESSPI.Create ('Unable to complete auth token')
    end
    else
      raise ESSPI.Create ('Unable to complete auth token')
  end;

  if (r = SEC_I_CONTINUE_NEEDED) or (r = SEC_I_COMPLETE_AND_CONTINUE) then
  begin
    result := fResp;
    Inc (fStep)
  end
  else
  begin  // Success!

    result := '';
    ImpersonateSSPIUser;
    fStep := 0;
  end
end;

procedure TSSPIServerConnectionContext.ServerInitialize(
  const st: AnsiString);
begin
  fInBuffDesc.ulVersion := 0;
  fInBuffDesc.cBuffers := 1;
  fInBuffDesc.pBuffers := @fInBuff;

  fInBuff.cbBuffer := Length (st);
  fInBuff.pvBuffer := @st [1];
  fInBuff.BufferType := SECBUFFER_TOKEN;

  fOutBuffDesc.ulVersion := 0;
  fOutBuffDesc.cBuffers := 1;
  fOutBuffDesc.pBuffers := @fOutBuff;

  SetLength (fResp, 512);
  fOutBuff.cbBuffer := 512;
  fOutBuff.BufferType := SECBUFFER_TOKEN;
  fOutBuff.pvBuffer := @fResp [1];
end;

{ TSSPINegotiatePackage }

class function TSSPINegotiatePackage.PackageName: string;
begin
  result := 'Negotiate';
end;

{ TSSPISSLPackage }

class function TSSPISSLPackage.PackageName: string;
begin
  result := SCHANNEL_NAME;
end;

{ TSSPISSLCredentials }

procedure TSSPISSLCredentials.Acquire;
var
//  IssuerListInfo : SecPkgContext_IssuerListInfoEx;
  err : SECURITY_STATUS;
  hcs : HCERTSTORE;
  tsExpiry : TTimestamp;
begin
  hcs := CertOpenSystemStore (0, 'MY');

  if hcs = Nil then
    raise ESSPI.Create('Unable to open MY certificate store');

  try

    {$ifdef thumbprint}
    if(thumbPrint != NULL && thumbPrint->Length > 0)
    {
        int HashLen = thumbPrint->Length;
        BYTE* pbData = (BYTE*)malloc(HashLen);
        Marshal::Copy(thumbPrint, 0, pbData, HashLen);
        CRYPT_HASH_BLOB hash={HashLen, pbData};
        SetLastError(0);
        pCertContext = CertFindCertificateInStore(hCertStore,
                                                X509_ASN_ENCODING,
                                                0,
                                                CERT_FIND_HASH,
                                                &hash,
                                                NULL);
        free(pbData);
        Status = GetLastError();
        if(pCertContext == NULL)
        {
            CertCloseStore(hCertStore, 0);
            hCertStore = NULL;
            throw new Common::Exceptions::SSLException(String::Concat(S"Failed to match certificate info. Error: ", Convert::ToString((unsigned int)Status)));
        }
    }
    {$endif}
    fSChannelCred.dwVersion  := SCHANNEL_CRED_VERSION;
    {$ifdef thumbprint}
    if(pCertContext != NULL)
    {
        m_pSChannelCred->cCreds     = 1;
        m_pSChannelCred->paCred     = &pCertContext;
    }
    {$endif}
    fSChannelCred.grbitEnabledProtocols := SP_PROT_NONE;
    fSChannelCred.dwFlags := fSChannelCred.dwFlags or SCH_CRED_NO_DEFAULT_CREDS or SCH_CRED_MANUAL_CRED_VALIDATION;

    err := Parent.fFunctionTable.AcquireCredentialsHandleW (
      Nil, UNISP_NAME,
      SECPKG_CRED_OUTBOUND,
      Nil,
      @fSChannelCred,
      nil,
      nil,
      fHandle,
      tsExpiry);

    SSPICheck (err, 'AcquireCredentialsHandle failed');
  finally
    CertCloseStore (hcs, 0)
  end
end;

{ TSSPISSLClientConnectionContext }

function TSSPISSLClientConnectionContext.GetRequestedFlags: LongWord;
begin
  result := ISC_REQ_SEQUENCE_DETECT or ISC_REQ_REPLAY_DETECT or
            ISC_REQ_CONFIDENTIALITY or ISC_RET_EXTENDED_ERROR  or
            (* ISC_REQ_ALLOCATE_MEMORY or *) ISC_REQ_STREAM;
end;

initialization
  gRegisteredSecurityPackages := TObjectList.Create;

  RegisterSecurityPackage (TSSPINTLMPackage, TSSPIWinNTCredentials, TSSPIClientConnectionContext, TSSPIServerConnectionContext);
  RegisterSecurityPackage (TSSPINegotiatePackage, TSSPIWinNTCredentials, TSSPIClientConnectionContext, TSSPIServerConnectionContext);
  RegisterSecurityPackage (TSSPISSLPackage, TSSPISSLCredentials, TSSPISSLClientConnectionContext, nil);
finalization
  gRegisteredSecurityPackages.Free
end.
