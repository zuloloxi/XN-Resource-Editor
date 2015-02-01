unit sspi;

interface

uses Windows, SysUtils;

const
  SEC_WINNT_AUTH_IDENTITY_ANSI = $01;
  SEC_WINNT_AUTH_IDENTITY_UNICODE  = $02;

  SECPKG_ATTR_SIZES              = 0;
  SECPKG_ATTR_NAMES              = 1;
  SECPKG_ATTR_LIFESPAN           = 2;
  SECPKG_ATTR_DCE_INFO           = 3;
  SECPKG_ATTR_STREAM_SIZES       = 4;
  SECPKG_ATTR_KEY_INFO           = 5;
  SECPKG_ATTR_AUTHORITY          = 6;
  SECPKG_ATTR_PROTO_INFO         = 7;
  SECPKG_ATTR_PASSWORD_EXPIRY    = 8;
  SECPKG_ATTR_SESSION_KEY        = 9;
  SECPKG_ATTR_PACKAGE_INFO       = 10;
  SECPKG_ATTR_USER_FLAGS         = 11;
  SECPKG_ATTR_NEGOTIATION_INFO   = 12;
  SECPKG_ATTR_NATIVE_NAMES       = 13;
  SECPKG_ATTR_FLAGS              = 14;
  SECPKG_ATTR_USE_VALIDATED      = 15;
  SECPKG_ATTR_CREDENTIAL_NAME    = 16;
  SECPKG_ATTR_TARGET_INFORMATION = 17;
  SECPKG_ATTR_ACCESS_TOKEN       = 18;
  SECPKG_ATTR_TARGET             = 19;
  SECPKG_ATTR_AUTHENTICATION_ID  = 20;
  SECPKG_ATTR_LOGOFF_TIME        = 21;

  SECPKG_CRED_INBOUND          = $00000001;
  SECPKG_CRED_OUTBOUND         = $00000002;
  SECPKG_CRED_BOTH             = $00000003;
  SECPKG_CRED_DEFAULT          = $00000004;
  SECPKG_CRED_RESERVED         = $F0000000;

  SECBUFFER_VERSION           = 0;

  SECBUFFER_EMPTY             = 0;   // Undefined, replaced by provider
  SECBUFFER_DATA              = 1;   // Packet data
  SECBUFFER_TOKEN             = 2;   // Security token
  SECBUFFER_PKG_PARAMS        = 3;   // Package specific parameters
  SECBUFFER_MISSING           = 4;   // Missing Data indicator
  SECBUFFER_EXTRA             = 5;   // Extra data
  SECBUFFER_STREAM_TRAILER    = 6;   // Security Trailer
  SECBUFFER_STREAM_HEADER     = 7;   // Security Header
  SECBUFFER_NEGOTIATION_INFO  = 8;   // Hints from the negotiation pkg
  SECBUFFER_PADDING           = 9;   // non-data padding
  SECBUFFER_STREAM            = 10;  // whole encrypted message

  SECBUFFER_ATTRMASK          = $F0000000;
  SECBUFFER_READONLY          = $80000000;  // Buffer is read-only
  SECBUFFER_RESERVED          = $40000000;

  SECURITY_NATIVE_DREP        = $00000010;
  SECURITY_NETWORK_DREP       = $00000000;

  ASC_REQ_DELEGATE                = $00000001;
  ASC_REQ_MUTUAL_AUTH             = $00000002;
  ASC_REQ_REPLAY_DETECT           = $00000004;
  ASC_REQ_SEQUENCE_DETECT         = $00000008;
  ASC_REQ_CONFIDENTIALITY         = $00000010;
  ASC_REQ_USE_SESSION_KEY         = $00000020;
  ASC_REQ_ALLOCATE_MEMORY         = $00000100;
  ASC_REQ_USE_DCE_STYLE           = $00000200;
  ASC_REQ_DATAGRAM                = $00000400;
  ASC_REQ_CONNECTION              = $00000800;
  ASC_REQ_CALL_LEVEL              = $00001000;
  ASC_REQ_EXTENDED_ERROR          = $00008000;
  ASC_REQ_STREAM                  = $00010000;
  ASC_REQ_INTEGRITY               = $00020000;
  ASC_REQ_LICENSING               = $00040000;
  ASC_REQ_IDENTIFY                = $00080000;
  ASC_REQ_ALLOW_NULL_SESSION      = $00100000;
  ASC_REQ_ALLOW_NON_USER_LOGONS   = $00200000;
  ASC_REQ_ALLOW_CONTEXT_REPLAY    = $00400000;
  ASC_REQ_FRAGMENT_TO_FIT         = $00800000;
  ASC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  ASC_REQ_NO_TOKEN                = $01000000;

  ASC_RET_DELEGATE                = $00000001;
  ASC_RET_MUTUAL_AUTH             = $00000002;
  ASC_RET_REPLAY_DETECT           = $00000004;
  ASC_RET_SEQUENCE_DETECT         = $00000008;
  ASC_RET_CONFIDENTIALITY         = $00000010;
  ASC_RET_USE_SESSION_KEY         = $00000020;
  ASC_RET_ALLOCATED_MEMORY        = $00000100;
  ASC_RET_USED_DCE_STYLE          = $00000200;
  ASC_RET_DATAGRAM                = $00000400;
  ASC_RET_CONNECTION              = $00000800;
  ASC_RET_CALL_LEVEL              = $00002000; // skipped 1000 to be like ISC_
  ASC_RET_THIRD_LEG_FAILED        = $00004000;
  ASC_RET_EXTENDED_ERROR          = $00008000;
  ASC_RET_STREAM                  = $00010000;
  ASC_RET_INTEGRITY               = $00020000;
  ASC_RET_LICENSING               = $00040000;
  ASC_RET_IDENTIFY                = $00080000;
  ASC_RET_NULL_SESSION            = $00100000;
  ASC_RET_ALLOW_NON_USER_LOGONS   = $00200000;
  ASC_RET_ALLOW_CONTEXT_REPLAY    = $00400000;
  ASC_RET_FRAGMENT_ONLY           = $00800000;
  ASC_RET_NO_TOKEN                = $01000000;

  ISC_REQ_DELEGATE                = $00000001;
  ISC_REQ_MUTUAL_AUTH             = $00000002;
  ISC_REQ_REPLAY_DETECT           = $00000004;
  ISC_REQ_SEQUENCE_DETECT         = $00000008;
  ISC_REQ_CONFIDENTIALITY         = $00000010;
  ISC_REQ_USE_SESSION_KEY         = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS        = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS      = $00000080;
  ISC_REQ_ALLOCATE_MEMORY         = $00000100;
  ISC_REQ_USE_DCE_STYLE           = $00000200;
  ISC_REQ_DATAGRAM                = $00000400;
  ISC_REQ_CONNECTION              = $00000800;
  ISC_REQ_CALL_LEVEL              = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  ISC_REQ_EXTENDED_ERROR          = $00004000;
  ISC_REQ_STREAM                  = $00008000;
  ISC_REQ_INTEGRITY               = $00010000;
  ISC_REQ_IDENTIFY                = $00020000;
  ISC_REQ_NULL_SESSION            = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION  = $00080000;
  ISC_REQ_RESERVED1               = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT         = $00200000;

  ISC_RET_DELEGATE                = $00000001;
  ISC_RET_MUTUAL_AUTH             = $00000002;
  ISC_RET_REPLAY_DETECT           = $00000004;
  ISC_RET_SEQUENCE_DETECT         = $00000008;
  ISC_RET_CONFIDENTIALITY         = $00000010;
  ISC_RET_USE_SESSION_KEY         = $00000020;
  ISC_RET_USED_COLLECTED_CREDS    = $00000040;
  ISC_RET_USED_SUPPLIED_CREDS     = $00000080;
  ISC_RET_ALLOCATED_MEMORY        = $00000100;
  ISC_RET_USED_DCE_STYLE          = $00000200;
  ISC_RET_DATAGRAM                = $00000400;
  ISC_RET_CONNECTION              = $00000800;
  ISC_RET_INTERMEDIATE_RETURN     = $00001000;
  ISC_RET_CALL_LEVEL              = $00002000;
  ISC_RET_EXTENDED_ERROR          = $00004000;
  ISC_RET_STREAM                  = $00008000;
  ISC_RET_INTEGRITY               = $00010000;
  ISC_RET_IDENTIFY                = $00020000;
  ISC_RET_NULL_SESSION            = $00040000;
  ISC_RET_MANUAL_CRED_VALIDATION  = $00080000;
  ISC_RET_RESERVED1               = $00100000;
  ISC_RET_FRAGMENT_ONLY           = $00200000;

  SEC_E_OK                         = 0;
  SEC_E_INSUFFICIENT_MEMORY        = HRESULT($80090300);
  SEC_E_INVALID_HANDLE             = HRESULT($80090301);
  SEC_E_UNSUPPORTED_FUNCTION       = HRESULT($80090302);
  SEC_E_TARGET_UNKNOWN             = HRESULT($80090303);
  SEC_E_INTERNAL_ERROR             = HRESULT($80090304);
  SEC_E_SECPKG_NOT_FOUND           = HRESULT($80090305);
  SEC_E_NOT_OWNER                  = HRESULT($80090306);
  SEC_E_CANNOT_INSTALL             = HRESULT($80090307);
  SEC_E_INVALID_TOKEN              = HRESULT($80090308);
  SEC_E_CANNOT_PACK                = HRESULT($80090309);
  SEC_E_QOP_NOT_SUPPORTED          = HRESULT($8009030A);
  SEC_E_NO_IMPERSONATION           = HRESULT($8009030B);
  SEC_E_LOGON_DENIED               = HRESULT($8009030C);
  SEC_E_UNKNOWN_CREDENTIALS        = HRESULT($8009030D);
  SEC_E_NO_CREDENTIALS             = HRESULT($8009030E);
  SEC_E_MESSAGE_ALTERED            = HRESULT($8009030F);
  SEC_E_OUT_OF_SEQUENCE            = HRESULT($80090310);
  SEC_E_NO_AUTHENTICATING_AUTHORITY = HRESULT($80090311);
  SEC_I_CONTINUE_NEEDED            = HRESULT($00090312);
  SEC_I_COMPLETE_NEEDED            = HRESULT($00090313);
  SEC_I_COMPLETE_AND_CONTINUE      = HRESULT($00090314);
  SEC_I_LOCAL_LOGON                = HRESULT($00090315);
  SEC_E_BAD_PKGID                  = HRESULT($80090316);
  SEC_E_CONTEXT_EXPIRED            = HRESULT($80090317);
  SEC_E_INCOMPLETE_MESSAGE         = HRESULT($80090318);
  SEC_E_INCOMPLETE_CREDENTIALS     = HRESULT($80090320);
  SEC_E_BUFFER_TOO_SMALL           = HRESULT($80090321);
  SEC_I_INCOMPLETE_CREDENTIALS     = HRESULT($00090320);
  SEC_I_RENEGOTIATE                = HRESULT($00090321);
  SEC_E_WRONG_PRINCIPAL            = HRESULT($80090322);
  SEC_I_NO_LSA_CONTEXT             = HRESULT($00090323);
  SEC_E_TIME_SKEW                  = HRESULT($80090324);
  SEC_E_UNTRUSTED_ROOT             = HRESULT($80090325);
  SEC_E_ILLEGAL_MESSAGE            = HRESULT($80090326);
  SEC_E_CERT_UNKNOWN               = HRESULT($80090327);
  SEC_E_CERT_EXPIRED               = HRESULT($80090328);
  SEC_E_ENCRYPT_FAILURE            = HRESULT($80090329);
  SEC_E_DECRYPT_FAILURE            = HRESULT($80090330);
  SEC_E_ALGORITHM_MISMATCH         = HRESULT($80090331);
  SEC_E_SECURITY_QOS_FAILED        = HRESULT($80090332);

type
SecPkgInfoW = record
  fCapabilities : DWORD;      // Capability bitmask
  wVersion : WORD;            // Version of driver
  wRPCID : WORD;              // ID for RPC Runtime
  cbMaxToken : DWORD;         // Size of authentication token (max)
  Name : PWideChar;
  Comment : PWideChar;            // Comment
end;
PSecPkgInfoW = ^SecPkgInfoW;

SecHandle = record
  dwLower : ULONG;
  dwUpper : ULONG;
end;

CtxtHandle = SecHandle;
pCtxtHandle = ^CtxtHandle;

CredHandle = SecHandle;
pCredHandle = ^CredHandle;

SECURITY_STATUS = LongInt;

SecBuffer = packed record
  cbBuffer : DWORD;
  BufferType : DWORD;           // Type of the buffer (below)
  pvBuffer : pointer;
end;
PSecBuffer = ^SecBuffer;

SecBufferDesc = packed record
  ulVersion,
  cBuffers : DWORD;             // Number of buffers
  pBuffers : PSecBuffer
end;
PSecBufferDesc = ^SecBufferDesc;

SecPkgContext_StreamSizes = packed record
  cbHeader : DWORD;
  cbTrailer : DWORD;
  cbMaximumMessage : DWORD;
  cBuffers : DWORD;
  cbBlockSize : DWORD
end;

SEC_WINNT_AUTH_IDENTITY_W = record
  User : PWideChar;
  UserLength : DWORD;
  Domain : PWideChar;
  DomainLength : DWORD;
  Password : PWideChar;
  PasswordLength : DWORD;
  Flags : DWORD
end;
PSEC_WINNT_AUTH_IDENTITY_W = ^SEC_WINNT_AUTH_IDENTITY_W;

ENUMERATE_SECURITY_PACKAGES_FN_W  = function (var cPackages : DWORD; var PackageInfo : PSecPkgInfoW) : SECURITY_STATUS; stdcall;
QUERY_SECURITY_PACKAGE_INFO_FN_W  = function (packageName : PWideChar; var info : PSecPkgInfoW) : SECURITY_STATUS; stdcall;
QUERY_CREDENTIALS_ATTRIBUTES_FN_W = function (phCredential : pCredHandle; ulAttribute : DWORD; buffer : pointer) : SECURITY_STATUS; stdcall;
EXPORT_SECURITY_CONTEXT_FN        = function (hContext : pCtxtHandle; flags : DWORD; pPackedContext : PSecBuffer; var token : pointer) : SECURITY_STATUS;
SEC_GET_KEY_FN                    = procedure (Arg, Principal : pointer; KeyVer : DWORD; var Key : pointer; var status : SECURITY_STATUS);

ACQUIRE_CREDENTIALS_HANDLE_FN_W      = function (
  pszPrincipal : PWideChar;
  pszPackage : PWideChar;
  fCredentialUse : DWORD;
  pvLogonID : pointer;
  pAuthData : pointer;
  pGetKeyFn : SEC_GET_KEY_FN;
  pvGetKeyArgument : pointer;
  var phCredential : CredHandle;
  var ptsExpiry : TTimeStamp) : SECURITY_STATUS; stdcall;

FREE_CREDENTIALS_HANDLE_FN = function (credHandle : PCredHandle) : SECURITY_STATUS; stdcall;

INITIALIZE_SECURITY_CONTEXT_FN_W  = function (
    phCredential : PCredHandle;
    phContent : PCtxtHandle;
    pszTargetName : PWideChar;
    fContextReq,
    Reserved1,
    TargetDataRep : DWORD;
    pInput : PSecBufferDesc;
    Reserved2 : DWORD;
    phNewContext : PCtxtHandle;
    pOutput : PSecBufferDesc;
    var pfContextAttr : DWORD;
    var ptsExpiry : TTimeStamp) : SECURITY_STATUS; stdcall;

ACCEPT_SECURITY_CONTEXT_FN = function (
    phCredential : PCredHandle;
    phContext : PCtxtHandle;
    pInput : PSecBufferDesc;
    fContextReq,
    TargetDataRep : DWORD;
    phNewContext : PCtxtHandle;
    pOutput : PSecBufferDesc;
    var pfContextAttr : DWORD;
    var ptsExpiry : TTimeStamp) : SECURITY_STATUS; stdcall;

COMPLETE_AUTH_TOKEN_FN           = function (phContext : PCtxtHandle; pToken : PSecBufferDesc) : SECURITY_STATUS; stdcall;
DELETE_SECURITY_CONTEXT_FN       = function (phContext : PCtxtHandle) : SECURITY_STATUS; stdcall;
APPLY_CONTROL_TOKEN_FN           = function (phContext : PCtxtHandle; pInput : PSecBufferDesc) : SECURITY_STATUS; stdcall;
QUERY_CONTEXT_ATTRIBUTES_FN_W    = function (phContext : PCtxtHandle; alAttribute : DWORD; pBuffer : pointer) : SECURITY_STATUS; stdcall;
IMPERSONATE_SECURITY_CONTEXT_FN  = function (phContext : PCtxtHandle) : SECURITY_STATUS; stdcall;
REVERT_SECURITY_CONTEXT_FN       = function (phContext : PCtxtHandle) : SECURITY_STATUS; stdcall;
MAKE_SIGNATURE_FN                = function (phContext : PCtxtHandle; fQOP : DWORD; pMessage : PSecBufferDesc;  MessageSeqNo : DWORD) : SECURITY_STATUS; stdcall;
VERIFY_SIGNATURE_FN              = function (phContext : PCtxtHandle; pMessage : PSecBufferDesc; MessageSeqNo : DWORD; var fQOP : DWORD) : SECURITY_STATUS; stdcall;
FREE_CONTEXT_BUFFER_FN           = function (contextBuffer : pointer) : SECURITY_STATUS; stdcall;
IMPORT_SECURITY_CONTEXT_FN_W     = function (pszPackage : PWideChar; pPackedContext : PSecBuffer; Token : pointer; phContext : PCtxtHandle) : SECURITY_STATUS; stdcall;

ADD_CREDENTIALS_FN_W             = function (
    hCredentials : PCredHandle;
    pszPrincipal,
    pszPackage : PWideChar;
    fCredentialUse : DWORD;
    pAuthData : pointer;
    pGetKeyFn : SEC_GET_KEY_FN;
    pvGetKeyArgument : pointer;
    var ptsExpiry : TTimeStamp) : SECURITY_STATUS; stdcall;

QUERY_SECURITY_CONTEXT_TOKEN_FN = function (phContext : PCtxtHandle; var token : pointer) : SECURITY_STATUS; stdcall;
ENCRYPT_MESSAGE_FN              = function (phContext : PCtxtHandle; fQOP : DWORD; pMessage : PSecBufferDesc; MessageSeqNo : DWORD) : SECURITY_STATUS; stdcall;
DECRYPT_MESSAGE_FN              = function (phContext : PCtxtHandle; pMessage : PSecBufferDesc; MessageSeqNo : DWORD; fQOP : DWORD) : SECURITY_STATUS; stdcall;

TSecurityFunctionTableW = record
  dwVersion : LongInt;
  EnumerateSecurityPackagesW  : ENUMERATE_SECURITY_PACKAGES_FN_W;
  QueryCredentialsAttributesW : QUERY_CREDENTIALS_ATTRIBUTES_FN_W;
  AcquireCredentialsHandleW   : ACQUIRE_CREDENTIALS_HANDLE_FN_W;
  FreeCredentialsHandle       : FREE_CREDENTIALS_HANDLE_FN;
  Reserved2                   : FARPROC;
  InitializeSecurityContextW  : INITIALIZE_SECURITY_CONTEXT_FN_W;
  AcceptSecurityContext       : ACCEPT_SECURITY_CONTEXT_FN;
  CompleteAuthToken           : COMPLETE_AUTH_TOKEN_FN;
  DeleteSecurityContext       : DELETE_SECURITY_CONTEXT_FN;
  ApplyControlToken           : APPLY_CONTROL_TOKEN_FN;
  QueryContextAttributesW     : QUERY_CONTEXT_ATTRIBUTES_FN_W;
  ImpersonateSecurityContext  : IMPERSONATE_SECURITY_CONTEXT_FN;
  RevertSecurityContext       : REVERT_SECURITY_CONTEXT_FN;
  MakeSignature               : MAKE_SIGNATURE_FN;
  VerifySignature             : VERIFY_SIGNATURE_FN;
  FreeContextBuffer           : FREE_CONTEXT_BUFFER_FN;
  QuerySecurityPackageInfoW   : QUERY_SECURITY_PACKAGE_INFO_FN_W;
  Reserved3                   : FARPROC;
  Reserved4                   : FARPROC;
  ExportSecurityContext       : EXPORT_SECURITY_CONTEXT_FN;
  ImportSecurityContextW      : IMPORT_SECURITY_CONTEXT_FN_W;
  AddCredentialsW             : ADD_CREDENTIALS_FN_W;
  Reserved8                   : FARPROC;
  QuerySecurityContextToken   : QUERY_SECURITY_CONTEXT_TOKEN_FN;
  EncryptMessage              : ENCRYPT_MESSAGE_FN;
  DecryptMessage              : DECRYPT_MESSAGE_FN;
end;
PSecurityFunctionTableW = ^TSecurityFunctionTableW;

INIT_SECURITY_INTERFACE_W = function : PSecurityFunctionTableW;

implementation

end.
