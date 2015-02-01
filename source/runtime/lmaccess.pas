//------------------------------------------------------------------------
// lmaccess unit
//
// Lan Manager User and Group function interfaces
//
// Translated to Delphi by Colin Wilson.  Translation copyright (c) Colin
// Wilson 2002.  All rights reserved.
//
// Checked for Tiburon 30/4/2008

unit lmaccess;

{$WEAKPACKAGEUNIT}

interface

uses Windows, LmGlobal;


const

  LOCALGROUP_NAME_PARMNUM          = 1;
  LOCALGROUP_COMMENT_PARMNUM       = 2;

  UF_SCRIPT               = $0001;
  UF_ACCOUNTDISABLE       = $0002;
  UF_HOMEDIR_REQUIRED     = $0008;
  UF_LOCKOUT              = $0010;
  UF_PASSWD_NOTREQD       = $0020;
  UF_PASSWD_CANT_CHANGE   = $0040;
  UF_DONT_EXPIRE_PASSWD   = $10000;

//
// Account type bits as part of usri_flags.
//

  UF_TEMP_DUPLICATE_ACCOUNT       = $0100;
  UF_NORMAL_ACCOUNT               = $0200;
  UF_INTERDOMAIN_TRUST_ACCOUNT    = $0800;
  UF_WORKSTATION_TRUST_ACCOUNT    = $1000;
  UF_SERVER_TRUST_ACCOUNT         = $2000;

  UF_MACHINE_ACCOUNT_MASK = UF_INTERDOMAIN_TRUST_ACCOUNT or
                            UF_WORKSTATION_TRUST_ACCOUNT or
                            UF_SERVER_TRUST_ACCOUNT;

  UF_ACCOUNT_TYPE_MASK = UF_TEMP_DUPLICATE_ACCOUNT or
                         UF_NORMAL_ACCOUNT or
                         UF_INTERDOMAIN_TRUST_ACCOUNT or
                         UF_WORKSTATION_TRUST_ACCOUNT or
                         UF_SERVER_TRUST_ACCOUNT;

  UF_MNS_LOGON_ACCOUNT           = $20000;


  UF_SETTABLE_BITS = UF_SCRIPT or
                     UF_ACCOUNTDISABLE or
                     UF_LOCKOUT or
                     UF_HOMEDIR_REQUIRED or
                     UF_PASSWD_NOTREQD or
                     UF_PASSWD_CANT_CHANGE or
                     UF_ACCOUNT_TYPE_MASK or
                     UF_DONT_EXPIRE_PASSWD or
                     UF_MNS_LOGON_ACCOUNT;

//
// bit masks for the NetUserEnum filter parameter.
//

  FILTER_TEMP_DUPLICATE_ACCOUNT       = $0001;
  FILTER_NORMAL_ACCOUNT               = $0002;
  FILTER_PROXY_ACCOUNT                = $0004;
  FILTER_INTERDOMAIN_TRUST_ACCOUNT    = $0008;
  FILTER_WORKSTATION_TRUST_ACCOUNT    = $0010;
  FILTER_SERVER_TRUST_ACCOUNT         = $0020;

//
// bit masks for the NetUserGetLocalGroups flags
//
  LG_INCLUDE_INDIRECT         = $0001;

//
//  Bit masks for field usri2_auth_flags of USER_INFO_2.
//

  AF_OP_PRINT             = $1;
  AF_OP_COMM              = $2;
  AF_OP_SERVER            = $4;
  AF_OP_ACCOUNTS          = $8;
  AF_SETTABLE_BITS        = AF_OP_PRINT or AF_OP_COMM or AF_OP_SERVER or AF_OP_ACCOUNTS;

//
//  UAS role manifests under NETLOGON
//

  UAS_ROLE_STANDALONE     = 0;
  UAS_ROLE_MEMBER         = 1;
  UAS_ROLE_BACKUP         = 2;
  UAS_ROLE_PRIMARY        = 3;

//
//  Values for ParmError for NetUserSetInfo.
//

  USER_NAME_PARMNUM               = 1;
  USER_PASSWORD_PARMNUM           = 3;
  USER_PASSWORD_AGE_PARMNUM       = 4;
  USER_PRIV_PARMNUM               = 5;
  USER_HOME_DIR_PARMNUM           = 6;
  USER_COMMENT_PARMNUM            = 7;
  USER_FLAGS_PARMNUM              = 8;
  USER_SCRIPT_PATH_PARMNUM        = 9;
  USER_AUTH_FLAGS_PARMNUM         = 10;
  USER_FULL_NAME_PARMNUM          = 11;
  USER_USR_COMMENT_PARMNUM        = 12;
  USER_PARMS_PARMNUM              = 13;
  USER_WORKSTATIONS_PARMNUM       = 14;
  USER_LAST_LOGON_PARMNUM         = 15;
  USER_LAST_LOGOFF_PARMNUM        = 16;
  USER_ACCT_EXPIRES_PARMNUM       = 17;
  USER_MAX_STORAGE_PARMNUM        = 18;
  USER_UNITS_PER_WEEK_PARMNUM     = 19;
  USER_LOGON_HOURS_PARMNUM        = 20;
  USER_PAD_PW_COUNT_PARMNUM       = 21;
  USER_NUM_LOGONS_PARMNUM         = 22;
  USER_LOGON_SERVER_PARMNUM       = 23;
  USER_COUNTRY_CODE_PARMNUM       = 24;
  USER_CODE_PAGE_PARMNUM          = 25;
  USER_PRIMARY_GROUP_PARMNUM      = 51;
  USER_PROFILE                    = 52;
  USER_PROFILE_PARMNUM            = 52;
  USER_HOME_DIR_DRIVE_PARMNUM     = 53;

//
// the new infolevel counterparts of the old info level + parmnum
//

  USER_NAME_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL + USER_NAME_PARMNUM;
  USER_PASSWORD_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_PARMNUM;
  USER_PASSWORD_AGE_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_AGE_PARMNUM;
  USER_PRIV_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL + USER_PRIV_PARMNUM;
  USER_HOME_DIR_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_PARMNUM;
  USER_COMMENT_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + USER_COMMENT_PARMNUM;
  USER_FLAGS_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL + USER_FLAGS_PARMNUM;
  USER_SCRIPT_PATH_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_SCRIPT_PATH_PARMNUM;
  USER_AUTH_FLAGS_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_AUTH_FLAGS_PARMNUM;
  USER_FULL_NAME_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_FULL_NAME_PARMNUM;
  USER_USR_COMMENT_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_USR_COMMENT_PARMNUM;
  USER_PARMS_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL + USER_PARMS_PARMNUM;
  USER_WORKSTATIONS_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_WORKSTATIONS_PARMNUM;
  USER_LAST_LOGON_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGON_PARMNUM;
  USER_LAST_LOGOFF_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGOFF_PARMNUM;
  USER_ACCT_EXPIRES_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_ACCT_EXPIRES_PARMNUM;
  USER_MAX_STORAGE_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_MAX_STORAGE_PARMNUM;
  USER_UNITS_PER_WEEK_INFOLEVEL = PARMNUM_BASE_INFOLEVEL + USER_UNITS_PER_WEEK_PARMNUM;
  USER_LOGON_HOURS_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + USER_LOGON_HOURS_PARMNUM;
  USER_PAD_PW_COUNT_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_PAD_PW_COUNT_PARMNUM;
  USER_NUM_LOGONS_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + USER_NUM_LOGONS_PARMNUM;
  USER_LOGON_SERVER_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_LOGON_SERVER_PARMNUM;
  USER_COUNTRY_CODE_INFOLEVEL   = PARMNUM_BASE_INFOLEVEL + USER_COUNTRY_CODE_PARMNUM;
  USER_CODE_PAGE_INFOLEVEL      = PARMNUM_BASE_INFOLEVEL + USER_CODE_PAGE_PARMNUM;
  USER_PRIMARY_GROUP_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL + USER_PRIMARY_GROUP_PARMNUM;
//  USER_POSIX_ID_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + USER_POSIX_ID_PARMNUM;
  USER_HOME_DIR_DRIVE_INFOLEVEL = PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_DRIVE_PARMNUM;

//
//  For SetInfo call (parmnum 0) when password change not required
//

  NULL_USERSETINFO_PASSWD = '              ';

  TIMEQ_FOREVER               = -1;
  USER_MAXSTORAGE_UNLIMITED   = -1;
  USER_NO_LOGOFF              = -1;
  UNITS_PER_DAY               = 24;
  UNITS_PER_WEEK              = UNITS_PER_DAY * 7;

//
// Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)).
//

  USER_PRIV_MASK      = $3;
  USER_PRIV_GUEST     = 0;
  USER_PRIV_USER      = 1;
  USER_PRIV_ADMIN     = 2;

//
// user modals related defaults
//

  MAX_PASSWD_LEN      = PWLEN;
  DEF_MIN_PWLEN       = 6;
  DEF_PWUNIQUENESS    = 5;
  DEF_MAX_PWHIST      = 8;

  DEF_MAX_PWAGE       = TIMEQ_FOREVER;               // forever
  DEF_MIN_PWAGE       = 0;                           // 0 days
  DEF_FORCE_LOGOFF    = -1;                          // never
  DEF_MAX_BADPW       = 0;                           // no limit
  ONE_DAY             = 01*24*3600;                  // 01 day

//
// User Logon Validation (codes returned)
//

  VALIDATED_LOGON         = 0;
  PASSWORD_EXPIRED        = 2;
  NON_VALIDATED_LOGON     = 3;

  VALID_LOGOFF            = 1;

//
// parmnum manifests for user modals
//

  MODALS_MIN_PASSWD_LEN_PARMNUM      = 1;
  MODALS_MAX_PASSWD_AGE_PARMNUM      = 2;
  MODALS_MIN_PASSWD_AGE_PARMNUM      = 3;
  MODALS_FORCE_LOGOFF_PARMNUM        = 4;
  MODALS_PASSWD_HIST_LEN_PARMNUM     = 5;
  MODALS_ROLE_PARMNUM                = 6;
  MODALS_PRIMARY_PARMNUM             = 7;
  MODALS_DOMAIN_NAME_PARMNUM         = 8;
  MODALS_DOMAIN_ID_PARMNUM           = 9;
  MODALS_LOCKOUT_DURATION_PARMNUM    = 10;
  MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM = 11;
  MODALS_LOCKOUT_THRESHOLD_PARMNUM   = 12;

//
// the new infolevel counterparts of the old info level + parmnum
//

  MODALS_MIN_PASSWD_LEN_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_LEN_PARMNUM;
  MODALS_MAX_PASSWD_AGE_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + MODALS_MAX_PASSWD_AGE_PARMNUM;
  MODALS_MIN_PASSWD_AGE_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_AGE_PARMNUM;
  MODALS_FORCE_LOGOFF_INFOLEVEL       = PARMNUM_BASE_INFOLEVEL + MODALS_FORCE_LOGOFF_PARMNUM;
  MODALS_PASSWD_HIST_LEN_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + MODALS_PASSWD_HIST_LEN_PARMNUM;
  MODALS_ROLE_INFOLEVEL               = PARMNUM_BASE_INFOLEVEL + MODALS_ROLE_PARMNUM;
  MODALS_PRIMARY_INFOLEVEL            = PARMNUM_BASE_INFOLEVEL + MODALS_PRIMARY_PARMNUM;
  MODALS_DOMAIN_NAME_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_NAME_PARMNUM;
  MODALS_DOMAIN_ID_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_ID_PARMNUM;

  GROUPIDMASK                 = $8000;      // MSB set if uid refers
                                            // to a group

//
// Predefined group for all normal users, administrators and guests
// LOCAL is a special group for pinball local security.
//
// nb. Delphi 2.0 Allows you to assign string literals to Wide Strings...

  GROUP_SPECIALGRP_USERS      = 'USERS';
  GROUP_SPECIALGRP_ADMINS     = 'ADMINS';
  GROUP_SPECIALGRP_GUESTS     = 'GUESTS';
  GROUP_SPECIALGRP_LOCAL      = 'LOCAL';

//
// parmnum manifests for SetInfo calls (only comment is settable)
//

  GROUP_ALL_PARMNUM           = 0;
  GROUP_NAME_PARMNUM          = 1;
  GROUP_COMMENT_PARMNUM       = 2;
  GROUP_ATTRIBUTES_PARMNUM    = 3;

//
// the new infolevel counterparts of the old info level + parmnum
//

  GROUP_ALL_INFOLEVEL         = PARMNUM_BASE_INFOLEVEL + GROUP_ALL_PARMNUM;
  GROUP_NAME_INFOLEVEL        = PARMNUM_BASE_INFOLEVEL + GROUP_NAME_PARMNUM;
  GROUP_COMMENT_INFOLEVEL     = PARMNUM_BASE_INFOLEVEL + GROUP_COMMENT_PARMNUM;
  GROUP_ATTRIBUTES_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL + GROUP_ATTRIBUTES_PARMNUM;
//  GROUP_POSIX_ID_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + GROUP_POSIX_ID_PARMNUM;

  MAXPERMENTRIES      = 64;

  ACCESS_NONE         = $0;
  ACCESS_READ         = $1;
  ACCESS_WRITE        = $2;
  ACCESS_CREATE       = $4;
  ACCESS_EXEC         = $8;
  ACCESS_DELETE       = $10;
  ACCESS_ATRIB        = $20;
  ACCESS_PERM         = $40;

  ACCESS_ALL          = ACCESS_READ or
                        ACCESS_WRITE or
                        ACCESS_CREATE or
                        ACCESS_EXEC or
                        ACCESS_DELETE or
                        ACCESS_ATRIB or
                        ACCESS_PERM;


  ACCESS_GROUP        = $8000;

//
// Bit values for the acc1_attr field of the ACCESS_INFO_1 structure.
//

  ACCESS_AUDIT        = $1;

  ACCESS_SUCCESS_OPEN         = $10;
  ACCESS_SUCCESS_WRITE        = $20;
  ACCESS_SUCCESS_DELETE       = $40;
  ACCESS_SUCCESS_ACL          = $80;
  ACCESS_SUCCESS_MASK         = $F0;

  ACCESS_FAIL_OPEN            = $100;
  ACCESS_FAIL_WRITE           = $200;
  ACCESS_FAIL_DELETE          = $400;
  ACCESS_FAIL_ACL             = $800;
  ACCESS_FAIL_MASK            = $F00;

  ACCESS_FAIL_SHIFT           = 4;

//
// Parmnum value for NetAccessSetInfo.
//

  ACCESS_RESOURCE_NAME_PARMNUM    = 1;
  ACCESS_ATTR_PARMNUM             = 2;
  ACCESS_COUNT_PARMNUM            = 3;
  ACCESS_ACCESS_LIST_PARMNUM      = 4;

//
// the new infolevel counterparts of the old info level + parmnum
//

  ACCESS_RESOURCE_NAME_INFOLEVEL  = PARMNUM_BASE_INFOLEVEL + ACCESS_RESOURCE_NAME_PARMNUM;
  ACCESS_ATTR_INFOLEVEL           = PARMNUM_BASE_INFOLEVEL + ACCESS_ATTR_PARMNUM;
  ACCESS_COUNT_INFOLEVEL          = PARMNUM_BASE_INFOLEVEL + ACCESS_COUNT_PARMNUM;
  ACCESS_ACCESS_LIST_INFOLEVEL    = PARMNUM_BASE_INFOLEVEL + ACCESS_ACCESS_LIST_PARMNUM;

//
// ACCESS_LETTERS defines a letter for each bit position in
// the acl_access field of struct access_list.  Note that some
// bits have a corresponding letter of ' ' (space).
//

  ACCESS_LETTERS      = 'RWCXDAP         ';

  NETLOGON_CONTROL_QUERY         = 1;    // No-op: just query
  NETLOGON_CONTROL_REPLICATE     = 2;    // Force replicate on BDC
  NETLOGON_CONTROL_SYNCHRONIZE   = 3;    // Force synchronize on BDC
  NETLOGON_CONTROL_PDC_REPLICATE = 4;    // Force PDC to broadcast change
  NETLOGON_CONTROL_REDISCOVER    = 5;    // Force to re-discover trusted domain DCs
  NETLOGON_CONTROL_TC_QUERY      = 6;    // Query status of specified trusted channel status
  NETLOGON_CONTROL_TRANSPORT_NOTIFY = 7; // Notify netlogon that a new transport has come online
  NETLOGON_CONTROL_FIND_USER     = 8;    // Find named user in a trusted domain

// Debug function codes

  NETLOGON_CONTROL_BACKUP_CHANGE_LOG  = $FFFC;
  NETLOGON_CONTROL_TRUNCATE_LOG       = $FFFD;
  NETLOGON_CONTROL_SET_DBFLAG         = $FFFE;
  NETLOGON_CONTROL_BREAKPOINT         = $FFFF;

  NETLOGON_REPLICATION_NEEDED       =$01;  // Database is out of date
  NETLOGON_REPLICATION_IN_PROGRESS  =$02;  // Replication is happening now
  NETLOGON_FULL_SYNC_REPLICATION    =$04;  // full sync replication required/progress
  NETLOGON_REDO_NEEDED              =$08;  // Redo of previous replication needed

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// well-known domain relative sub-authority values (RIDs)...               //
// from winnt.h                                                            //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

// Well-known users ...

DOMAIN_USER_RID_ADMIN          = $000001F4;
DOMAIN_USER_RID_GUEST          = $000001F5;
DOMAIN_USER_RID_KRBTGT         = $000001F6;


// well-known groups ...

DOMAIN_GROUP_RID_ADMINS        = $00000200;
DOMAIN_GROUP_RID_USERS         = $00000201;
DOMAIN_GROUP_RID_GUESTS        = $00000202;

type
  PLOCALGROUP_INFO_0 = ^LOCALGROUP_INFO_0;
  LOCALGROUP_INFO_0 = record
    lgrpi0_name : PWideChar;
  end;

  PLOCALGROUP_INFO_1 = ^LOCALGROUP_INFO_1;
  LOCALGROUP_INFO_1 = record
    lgrpi1_name : PWideChar;
    lgrpi1_comment : PWideChar;
  end;

  PLOCALGROUP_INFO_1002 = ^LOCALGROUP_INFO_1002;
  LOCALGROUP_INFO_1002 = record
    lgrpi1002_comment : PWideChar;
  end;

  PLOCALGROUP_MEMBERS_INFO_0 = ^LOCALGROUP_MEMBERS_INFO_0;
  LOCALGROUP_MEMBERS_INFO_0 = record
    lgrmi0_sid : PSID;
  end;

  PLOCALGROUP_MEMBERS_INFO_1 = ^LOCALGROUP_MEMBERS_INFO_1;
  LOCALGROUP_MEMBERS_INFO_1 = record
    lgrmi1_sid : PSID;
    lgrmi1_sidusage : SID_NAME_USE;
    lgrmi1_name : PWideChar;
  end;

  PLOCALGROUP_MEMBERS_INFO_2 = ^LOCALGROUP_MEMBERS_INFO_2;
  LOCALGROUP_MEMBERS_INFO_2 = record
    lgrmi2_sid : PSID;
    lgrmi2_sidusage : SID_NAME_USE;
    lgrmi2_domainandname : PWideChar;
  end;

  PLOCALGROUP_MEMBERS_INFO_3 = ^LOCALGROUP_MEMBERS_INFO_3;
  LOCALGROUP_MEMBERS_INFO_3 = record
    lgrmi3_domainandname : PWideChar;
  end;

  PLOCALGROUP_USERS_INFO_0 = ^LOCALGROUP_USERS_INFO_0;
  LOCALGROUP_USERS_INFO_0 = record
    lgrui0_name : PWideChar;
  end;

  USER_INFO_0 = record
    usri0_name : PWideChar;
  end;
  PUSER_INFO_0 = ^USER_INFO_0;

  USER_INFO_1 = record
    usri1_name : PWideChar;
    usri1_password : PWideChar;
    usri1_password_age : Integer;
    usri1_priv : Integer;
    usri1_home_dir : PWideChar;
    usri1_comment : PWideChar;
    usri1_flags : Integer;
    usri1_script_path : PWideChar;
  end;
  PUSER_INFO_1 = ^USER_INFO_1;

  USER_INFO_2 = record
    usri2_name : PWideChar;
    usri2_password : PWideChar;
    usri2_password_age : Integer;
    usri2_priv : Integer;
    usri2_home_dir : PWideChar;
    usri2_comment : PWideChar;
    usri2_flags : Integer;
    usri2_script_path : PWideChar;
    usri2_auth_flags : Integer;
    usri2_full_name : PWideChar;
    usri2_usr_comment : PWideChar;
    usri2_parms : PWideChar;
    usri2_workstations : PWideChar;
    usri2_last_logon : Integer;
    usri2_last_logoff : Integer;
    usri2_acct_expires : Integer;
    usri2_max_storage : Integer;
    usri2_units_per_week : Integer;
    usri2_logon_hours : PByte;
    usri2_bad_pw_count : Integer;
    usri2_num_logons : Integer;
    usri2_logon_server : PWideChar;
    usri2_country_code : Integer;
    usri2_code_page : Integer;
  end;
  PUSER_INFO_2 = ^USER_INFO_2;

  USER_INFO_3 = record
    usri3_name : PWideChar;
    usri3_password : PWideChar;
    usri3_password_age : Integer;
    usri3_priv : Integer;
    usri3_home_dir : PWideChar;
    usri3_comment : PWideChar;
    usri3_flags : Integer;
    usri3_script_path : PWideChar;
    usri3_auth_flags : Integer;
    usri3_full_name : PWideChar;
    usri3_usr_comment : PWideChar;
    usri3_parms : PWideChar;
    usri3_workstations : PWideChar;
    usri3_last_logon : Integer;
    usri3_last_logoff : Integer;
    usri3_acct_expires : Integer;
    usri3_max_storage : Integer;
    usri3_units_per_week : Integer;
    usri3_logon_hours : PByte;
    usri3_bad_pw_count : Integer;
    usri3_num_logons : Integer;
    usri3_logon_server : PWideChar;
    usri3_country_code : Integer;
    usri3_code_page : Integer;
    usri3_user_id : Integer;
    usri3_primary_group_id : Integer;
    usri3_profile : PWideChar;
    usri3_home_dir_drive : PWideChar;
    usri3_password_expired : Integer;
  end;
  PUSER_INFO_3 = ^USER_INFO_3;

  USER_INFO_10 = record
    usri10_name : PWideChar;
    usri10_comment : PWideChar;
    usri10_usr_comment : PWideChar;
    usri10_full_name : PWideChar;
  end;
  PUSER_INFO_10 = ^USER_INFO_10;

  USER_INFO_11 = record
    usri11_name : PWideChar;
    usri11_comment : PWideChar;
    usri11_usr_comment : PWideChar;
    usri11_full_name : PWideChar;
    usri11_priv : Integer;
    usri11_auth_flags : Integer;
    usri11_password_age : Integer;
    usri11_home_dir : PWideChar;
    usri11_parms : PWideChar;
    usri11_last_logon : Integer;
    usri11_last_logoff : Integer;
    usri11_bad_pw_count : Integer;
    usri11_num_logons : Integer;
    usri11_logon_server : PWideChar;
    usri11_country_code : Integer;
    usri11_workstations : PWideChar;
    usri11_max_storage : Integer;
    usri11_units_per_week : Integer;
    usri11_logon_hours : PByte;
    usri11_code_page : Integer;
  end;
  PUSER_INFO_11 = ^USER_INFO_11;

  USER_INFO_20 = record
    usri20_name : PWideChar;
    usri20_full_name : PWideChar;
    usri20_comment : PWideChar;
    usri20_flags : Integer;
    usri20_user_id : Integer;
  end;
  PUSER_INFO_20 = ^USER_INFO_20;

  USER_INFO_21 = record
    usri21_password : array [0..ENCRYPTED_PWLEN-1] of char;
  end;
  PUSER_INFO_21 = ^USER_INFO_21;

  USER_INFO_22 = record
    usri22_name : PWideChar;
    usri22_password : array [0..ENCRYPTED_PWLEN-1] of char;
    usri22_password_age : Integer;
    usri22_priv : Integer;
    usri22_home_dir : PWideChar;
    usri22_comment : PWideChar;
    usri22_flags : Integer;
    usri22_script_path : PWideChar;
    usri22_auth_flags : Integer;
    usri22_full_name : PWideChar;
    usri22_usr_comment : PWideChar;
    usri22_parms : PWideChar;
    usri22_workstations : PWideChar;
    usri22_last_logon : Integer;
    usri22_last_logoff : Integer;
    usri22_acct_expires : Integer;
    usri22_max_storage : Integer;
    usri22_units_per_week : Integer;
    usri22_logon_hours : PByte;
    usri22_bad_pw_count : Integer;
    usri22_num_logons : Integer;
    usri22_logon_server : PWideChar;
    usri22_country_code : Integer;
    usri22_code_page : Integer;
  end;
  PUSER_INFO_22 = ^USER_INFO_22;

  USER_INFO_1003 = record
    usri1003_password : PWideChar;
  end;

  USER_INFO_1005 = record
    usri1005_priv : Integer;
  end;

  USER_INFO_1006 = record
    usri1006_home_dir : PWideChar;
  end;

  USER_INFO_1007 = record
    usri1007_comment : PWideChar;
  end;

  USER_INFO_1008 = record
    usri1008_flags : Integer;
  end;

  USER_INFO_1009 = record
    usri1009_script_path : PWideChar;
  end;

  USER_INFO_1010 = record
    usri1010_auth_flags : Integer;
  end;

  USER_INFO_1011 = record
    usri1011_full_name : PWideChar;
  end;
  PUSER_INFO_1011 = ^USER_INFO_1011;

  USER_INFO_1012 = record
    usri1012_usr_comment : PWideChar;
  end;

  USER_INFO_1013 = record
    usri1013_parms : PWideChar;
  end;

  USER_INFO_1014 = record
    usri1014_workstations : PWideChar;
  end;

  USER_INFO_1017 = record
    usri1017_acct_expires : Integer;
  end;

  USER_INFO_1018 = record
    usri1018_max_storage : Integer;
  end;

  USER_INFO_1020 = record
    usri1020_units_per_week : Integer;
    usri1020_logon_hours : PByte;
  end;

  USER_INFO_1023 = record
    usri1023_logon_server : PWideChar;
  end;

  USER_INFO_1024 = record
    usri1024_country_code : Integer;
  end;

  USER_INFO_1025 = record
    usri1025_code_page : Integer;
  end;

  USER_INFO_1051 = record
    usri1051_primary_group_id : Integer;
  end;

  USER_INFO_1052 = record
    usri1052_profile : PWideChar;
  end;

  USER_INFO_1053 = record
    usri1053_home_dir_drive : PWideChar;
  end;

//
//  Data Structures - User Modals
//

  USER_MODALS_INFO_0 = record
    usrmod0_min_password_len : Integer;
    usrmod0_max_password_age : Integer;
    usrmod0_min_password_age : Integer;
    usrmod0_force_logoff : Integer;
    usrmod0_password_hist_len : Integer;
  end;

  USER_MODALS_INFO_1 = record
    usrmod1_role : Integer;
    usrmod1_primary : PWideChar;
  end;

  USER_MODALS_INFO_2 = record
    usrmod2_domain_name : PWideChar;
    usrmod2_domain_id : Integer;
  end;

  USER_MODALS_INFO_3 = record
    usrmod3_lockout_duration : Integer;
    usrmod3_lockout_observation_window : Integer;
    usrmod3_lockout_threshold : Integer;
  end;

  USER_MODALS_INFO_1001 = record
    usrmod1001_min_passwd_len : Integer;
  end;

  USER_MODALS_INFO_1002 = record
    usrmod1002_max_passwd_age : Integer;
  end;

  USER_MODALS_INFO_1003 = record
    usrmod1003_min_passwd_age : Integer;
  end;

  USER_MODALS_INFO_1004 = record
    usrmod1004_force_logoff : Integer;
  end;

  USER_MODALS_INFO_1005 = record
    usrmod1005_password_hist_len : Integer;
  end;

  USER_MODALS_INFO_1006 = record
    usrmod1006_role : Integer;
  end;

  USER_MODALS_INFO_1007 = record
    usrmod1007_primary : PWideChar;
  end;

  GROUP_INFO_0 = record
    grpi0_name : PWideChar;
  end;
  PGROUP_INFO_0 = ^GROUP_INFO_0;

  GROUP_INFO_1 = record
    grpi1_name : PWideChar;
    grpi1_comment : PWideChar;
  end;
  PGROUP_INFO_1 = ^GROUP_INFO_1;

  GROUP_INFO_2 = record
    grpi2_name : PWideChar;
    grpi2_comment : PWideChar;
    grpi2_group_id : Integer;
    grpi2_attributes : Integer;
  end;
  PGROUP_INFO_2 = ^GROUP_INFO_2;

  GROUP_INFO_1002 = record
    grpi1002_comment : PWideChar;
  end;

  GROUP_INFO_1005 = record
    grpi1005_attributes : Integer;
  end;

  GROUP_USERS_INFO_0 = record
    grpui0_name : PWideChar;
  end;
  PGROUP_USERS_INFO_0 = ^GROUP_USERS_INFO_0;

  GROUP_USERS_INFO_1 = record
    grpui1_name : PWideChar;
    grpui1_attributes : Integer;
  end;

//
// QueryDisplayInformation levels

  NET_DISPLAY_USER = record
    usri1_name : PWideChar;
    usri1_comment : PWideChar;
    usri1_flags : Integer;
    usri1_full_name : PWideChar;
    usri1_user_id : Integer;
    usri1_next_index : Integer;
  end;

  NET_DISPLAY_MACHINE = record
    usri2_name : PWideChar;
    usri2_comment : PWideChar;
    usri2_flags : Integer;
    usri2_user_id : Integer;
    usri2_next_index : Integer;
  end;

  NET_DISPLAY_GROUP = record
    grpi3_name : PWideChar;
    grpi3_comment : PWideChar;
    grpi3_group_id : Integer;
    grpi3_attributes : Integer;
    grpi3_next_index : Integer;
  end;

  ACCESS_INFO_0 = record
    acc0_resource_name : PWideChar;
  end;

  ACCESS_INFO_1 = record
    acc1_resource_name : PWideChar;
    acc1_attr : Integer;
    acc1_count : Integer;
  end;

  ACCESS_INFO_1002 = record
    acc1002_attr : Integer;
  end;

  ACCESS_LIST = record
    acl_ugname : PWideChar;
    acl_access : Integer;
  end;

  NTSTATUS = Integer;
  PNTSTATUS = ^NTSTATUS;

  NETLOGON_INFO_1 = record
    netlog1_flags : Integer;
    netlog1_pdc_connection_status : NetAPIStatus;
  end;
  PNETLOGON_INFO_1 = ^NETLOGON_INFO_1;

  NETLOGON_INFO_2 = record
    netlog2_flags : Integer;
    netlog2_pdc_connection_status : NetAPIStatus;
    netlog2_trusted_dc_names : PWideChar;
    netlog2_tc_connection_status : NetAPIStatus;
  end;

  NETLOGON_INFO_3 = record
    netlog3_flags : Integer;
    netlog3_logon_attempts : Integer;
    netlog3_reserved1 : Integer;
    netlog3_reserved2 : Integer;
    netlog3_reserved3 : Integer;
    netlog3_reserved4 : Integer;
    netlog3_reserved5 : Integer;
  end;

  NETLOGON_INFO_4 = record
    netlog4_trusted_dc_names : PWideChar;
    netlog4_trusted_dc_domaon_name : PWideChar;
  end;

function NetUserAdd (serverName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetUserEnum (serverName : PWideChar; level, filter : Integer; var buffer : Pointer; prefmaxlen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetUserGetInfo (serverName, userName : PWideChar; level : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetUserSetInfo (serverName, userName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetUserDel (serverName, userName : PWideChar) : NetAPIStatus; stdcall;
function NetUserGetGroups (serverName, userName : PWideChar; level : Integer; var buffer : Pointer; prefmaxlen : Integer; var entriesRead, totalEntries : Integer) : NetAPIStatus; stdcall;
function NetUserSetGroups (serverName, userName : PWideChar; level : Integer; buffer : PByte; numEntries : Integer) : NetAPIStatus; stdcall;
function NetUserGetLocalGroups (serverName, userName : PWideChar; level, flags : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries : Integer) : NetAPIStatus; stdcall;
function NetUserModalsGet (serverName : PWideChar; level : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetUserModalsSet (serverName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetUserChangePassword (domainName, userName, oldPassword, newPassword : PWideChar) : NetAPIStatus; stdcall;
function NetGroupAdd (serverName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetGroupAddUser (serverName, GroupName, UserName : PWideChar) : NetAPIStatus; stdcall;
function NetGroupEnum (serverName : PWideChar; level : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetGroupGetInfo (serverName, groupName : PWideChar; level : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetGroupSetInfo (servername, groupName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetGroupDel (serverName, groupName : PWideChar) : NetAPIStatus; stdcall;
function NetGroupDelUser (serverName, groupName, userName : PWideChar) : NetAPIStatus; stdcall;
function NetGroupGetUsers (serverName, groupName : PWideChar; level : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetGroupSetUsers (serverName, groupName : PWideChar; level : Integer; buffer : PByte; totalEntries : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupAdd (serverName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupAddMember (serverName, groupName : PWideChar; memberSid : PSID) : NetAPIStatus; stdcall;
function NetLocalGroupEnum (serverName : PWideChar; level : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupGetInfo (serverName, groupName : PWideChar; level : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetLocalGroupSetInfo (serverName, groupName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupDel (serverName, groupName : PWideChar) : NetAPIStatus; stdcall;
function NetLocalGroupDelMember (serverName, groupName : PWideChar; memberSid : PSID) : NetAPIStatus; stdcall;
function NetLocalGroupGetMembers (serverName, localGroupName : PWideChar; level : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupSetMembers (serverName, groupName : PWideChar; level : Integer; buffer : PByte; totalEntries : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupAddMembers (serverName, groupName : PWideChar; level : Integer; buffer : PByte; totalEntries : Integer) : NetAPIStatus; stdcall;
function NetLocalGroupDelMembers (serverName, groupName : PWideChar; level : Integer; buffer : PByte; totalEntries : Integer) : NetAPIStatus; stdcall;
function NetQueryDisplayInformation(serverName : PWideChar; level, index, entriesRequested, preferredMaximumLength : Integer; var returnedEntryCount : Integer; var SortedBuffer : pointer) : NetAPIStatus; stdcall;
function NetGetDisplayInformationIndex(serverName : PWideChar; level : Integer; prefix : PWideChar; var index : Integer) : NetAPIStatus; stdcall;
function NetAccessAdd (serverName : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetAccessEnum (serverName, basePath : PWideChar; recursive, level : Integer; var buffer : Pointer; prefMaxLen : Integer; var entriesRead, totalEntries, resumeHandle : Integer) : NetAPIStatus; stdcall;
function NetAccessGetInfo (serverName, resource : PWideChar; level : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetAccessSetInfo (serverName, resource : PWideChar; level : Integer; buffer : PByte; var parm_err : Integer) : NetAPIStatus; stdcall;
function NetAccessDel (serverName, resource : PWideChar) : NetAPIStatus; stdcall;
function NetAccessGetUserPerms (serverName, UGName, resource : PWideChar; var perms : Integer) : NetAPIStatus; stdcall;
function NetGetDCName (serverName, domainName : PWideChar; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetGetAnyDCName (serverName, domainName : PWideChar; var buffer : Pointer) : NetAPIStatus; stdcall;
function I_NetLogonControl (serverName : PWideChar; FunctionCode, QueryLevel : Integer; var buffer : Pointer) : NetAPIStatus; stdcall;
function I_NetLogonControl2 (serverName : PWideChar; functionCode, queryLevel : Integer; data : PByte; var buffer : Pointer) : NetAPIStatus; stdcall;
function NetEnumerateTrustedDomains (serverName : PWideChar; var domainNames : PWideChar) : NTSTATUS; stdcall;

implementation

function NetUserAdd;                    external 'NETAPI32.DLL';
function NetUserEnum;                   external 'NETAPI32.DLL';
function NetUserGetInfo;                external 'NETAPI32.DLL';
function NetUserSetInfo;                external 'NETAPI32.DLL';
function NetUserDel;                    external 'NETAPI32.DLL';
function NetUserGetGroups;              external 'NETAPI32.DLL';
function NetUserSetGroups;              external 'NETAPI32.DLL';
function NetUserGetLocalGroups;         external 'NETAPI32.DLL';
function NetUserModalsGet;              external 'NETAPI32.DLL';
function NetUserModalsSet;              external 'NETAPI32.DLL';
function NetUserChangePassword;         external 'NETAPI32.DLL';

function NetGroupAdd;                   external 'NETAPI32.DLL';
function NetGroupAddUser;               external 'NETAPI32.DLL';
function NetGroupEnum;                  external 'NETAPI32.DLL';
function NetGroupGetInfo;               external 'NETAPI32.DLL';
function NetGroupSetInfo;               external 'NETAPI32.DLL';
function NetGroupDel;                   external 'NETAPI32.DLL';
function NetGroupDelUser;               external 'NETAPI32.DLL';
function NetGroupGetUsers;              external 'NETAPI32.DLL';
function NetGroupSetUsers;              external 'NETAPI32.DLL';

function NetLocalGroupAdd;              external 'NETAPI32.DLL';
function NetLocalGroupAddMember;        external 'NETAPI32.DLL';
function NetLocalGroupEnum;             external 'NETAPI32.DLL';
function NetLocalGroupGetInfo;          external 'NETAPI32.DLL';
function NetLocalGroupSetInfo;          external 'NETAPI32.DLL';
function NetLocalGroupDel;              external 'NETAPI32.DLL';
function NetLocalGroupDelMember;        external 'NETAPI32.DLL';
function NetLocalGroupGetMembers;       external 'NETAPI32.DLL';
function NetLocalGroupSetMembers;       external 'NETAPI32.DLL';
function NetLocalGroupAddMembers;       external 'NETAPI32.DLL';
function NetLocalGroupDelMembers;       external 'NETAPI32.DLL';

function NetQueryDisplayInformation;    external 'NETAPI32.DLL';
function NetGetDisplayInformationIndex; external 'NETAPI32.DLL';

function NetAccessAdd;                  external 'NETAPI32.DLL';
function NetAccessEnum;                 external 'NETAPI32.DLL';
function NetAccessGetInfo;              external 'NETAPI32.DLL';
function NetAccessSetInfo;              external 'NETAPI32.DLL';
function NetAccessDel;                  external 'NETAPI32.DLL';
function NetAccessGetUserPerms;         external 'NETAPI32.DLL';

function NetGetDCName;                  external 'NETAPI32.DLL';
function NetGetAnyDCName;               external 'NETAPI32.DLL';
function I_NetLogonControl;             external 'NETAPI32.DLL';
function I_NetLogonControl2;            external 'NETAPI32.DLL';
function NetEnumerateTrustedDomains;    external 'NETAPI32.DLL';

end.
