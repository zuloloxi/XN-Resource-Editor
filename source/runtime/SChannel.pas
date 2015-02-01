unit SChannel;

interface

uses Windows, WinCrypt;

const
  UNISP_NAME     ='Microsoft Unified Security Protocol Provider';
  SSL2SP_NAME    ='Microsoft SSL 2.0';
  SSL3SP_NAME    ='Microsoft SSL 3.0';
  TLS1SP_NAME    ='Microsoft TLS 1.0';
  PCT1SP_NAME    ='Microsoft PCT 1.0';
  SCHANNEL_NAME  ='Schannel';

  SECPKG_ATTR_ISSUER_LIST          = $50;   // (OBSOLETE) returns SecPkgContext_IssuerListInfo
  SECPKG_ATTR_REMOTE_CRED          = $51;   // (OBSOLETE) returns SecPkgContext_RemoteCredentialInfo
  SECPKG_ATTR_LOCAL_CRED           = $52;   // (OBSOLETE) returns SecPkgContext_LocalCredentialInfo
  SECPKG_ATTR_REMOTE_CERT_CONTEXT  = $53;   // returns PCCERT_CONTEXT
  SECPKG_ATTR_LOCAL_CERT_CONTEXT   = $54;   // returns PCCERT_CONTEXT
  SECPKG_ATTR_ROOT_STORE           = $55;   // returns HCERTCONTEXT to the root store
  SECPKG_ATTR_SUPPORTED_ALGS       = $56;   // returns SecPkgCred_SupportedAlgs
  SECPKG_ATTR_CIPHER_STRENGTHS     = $57;   // returns SecPkgCred_CipherStrengths
  SECPKG_ATTR_SUPPORTED_PROTOCOLS  = $58;   // returns SecPkgCred_SupportedProtocols
  SECPKG_ATTR_ISSUER_LIST_EX       = $59;   // returns SecPkgContext_IssuerListInfoEx
  SECPKG_ATTR_CONNECTION_INFO      = $5a;   // returns SecPkgContext_ConnectionInfo
  SECPKG_ATTR_EAP_KEY_BLOCK        = $5b;   // returns SecPkgContext_EapKeyBlock
  SECPKG_ATTR_MAPPED_CRED_ATTR     = $5c;   // returns SecPkgContext_MappedCredAttr
  SECPKG_ATTR_SESSION_INFO         = $5d;   // returns SecPkgContext_SessionInfo
  SECPKG_ATTR_APP_DATA             = $5e;   // sets/returns SecPkgContext_SessionAppData

  SCH_CRED_V1              = $00000001;
  SCH_CRED_V2              = $00000002;  // for legacy code
  SCH_CRED_VERSION         = $00000002;  // for legacy code
  SCH_CRED_V3              = $00000003;  // for legacy code
  SCHANNEL_CRED_VERSION    = $00000004;

  SCH_CRED_NO_SYSTEM_MAPPER                    = $00000002;
  SCH_CRED_NO_SERVERNAME_CHECK                 = $00000004;
  SCH_CRED_MANUAL_CRED_VALIDATION              = $00000008;
  SCH_CRED_NO_DEFAULT_CREDS                    = $00000010;
  SCH_CRED_AUTO_CRED_VALIDATION                = $00000020;
  SCH_CRED_USE_DEFAULT_CREDS                   = $00000040;
  SCH_CRED_DISABLE_RECONNECTS                  = $00000080;

  SCH_CRED_REVOCATION_CHECK_END_CERT           = $00000100;
  SCH_CRED_REVOCATION_CHECK_CHAIN              = $00000200;
  SCH_CRED_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT = $00000400;
  SCH_CRED_IGNORE_NO_REVOCATION_CHECK          = $00000800;
  SCH_CRED_IGNORE_REVOCATION_OFFLINE           = $00001000;
  SCH_CRED_REVOCATION_CHECK_CACHE_ONLY         = $00004000;

  SCH_CRED_CACHE_ONLY_URL_RETRIEVAL      = $00008000;

  SP_PROT_PCT1_SERVER      = $00000001;
  SP_PROT_PCT1_CLIENT      = $00000002;
  SP_PROT_PCT1             = SP_PROT_PCT1_SERVER or SP_PROT_PCT1_CLIENT;

  SP_PROT_SSL2_SERVER      = $00000004;
  SP_PROT_SSL2_CLIENT      = $00000008;
  SP_PROT_SSL2             = SP_PROT_SSL2_SERVER or SP_PROT_SSL2_CLIENT;

  SP_PROT_SSL3_SERVER      = $00000010;
  SP_PROT_SSL3_CLIENT      = $00000020;
  SP_PROT_SSL3             = SP_PROT_SSL3_SERVER or SP_PROT_SSL3_CLIENT;

  SP_PROT_TLS1_SERVER      = $00000040;
  SP_PROT_TLS1_CLIENT      = $00000080;
  SP_PROT_TLS1             = SP_PROT_TLS1_SERVER or SP_PROT_TLS1_CLIENT;

  SP_PROT_SSL3TLS1_CLIENTS = SP_PROT_TLS1_CLIENT or SP_PROT_SSL3_CLIENT;
  SP_PROT_SSL3TLS1_SERVERS = SP_PROT_TLS1_SERVER or SP_PROT_SSL3_SERVER;
  SP_PROT_SSL3TLS1         = SP_PROT_SSL3 or SP_PROT_TLS1;

  SP_PROT_UNI_SERVER       = $40000000;
  SP_PROT_UNI_CLIENT       = $80000000;
  SP_PROT_UNI              = SP_PROT_UNI_SERVER or SP_PROT_UNI_CLIENT;

  SP_PROT_ALL              = $ffffffff;
  SP_PROT_NONE             = 0;
  SP_PROT_CLIENTS          = SP_PROT_PCT1_CLIENT or SP_PROT_SSL2_CLIENT or SP_PROT_SSL3_CLIENT or SP_PROT_UNI_CLIENT or SP_PROT_TLS1_CLIENT;
  SP_PROT_SERVERS          = SP_PROT_PCT1_SERVER or SP_PROT_SSL2_SERVER or SP_PROT_SSL3_SERVER or SP_PROT_UNI_SERVER or SP_PROT_TLS1_SERVER;

type

SecPkgContext_IssuerListInfoEx = packed record
	aIssuers : PCertNameBlob;
	cIssuers : DWORD;
end;
PSecPkgContext_IssuerListInfoEx = ^SecPkgContext_IssuerListInfoEx;

HMAPPER = record
end;
PHMAPPER = ^HMAPPER;

SCHANNEL_CRED = packed record
  dwVersion : DWORD;
  cCreds : DWORD;
  paCred : ^PCertContext;
  hRootStore : HCERTSTORE;
  cMappers : DWORD;
  aphMappers : ^PHMAPPER;
  cSupportedAlgs : DWORD;
  palgSupportedAlgs : ^ALG_ID;
  grbitEnabledProtocols : DWORD;
  dwMinimumCipherStrength : DWORD;
  dwMaximumCipherStrength : DWORD;
  dwSessionLifespan : DWORD;
  dwFlags : DWORD;
  dwCredFormat : DWORD
end;
PSCHANNEL_CRED = ^SCHANNEL_CRED;


implementation

end.
