unit iprtrmib;

interface

{$WEAKPACKAGEUNIT}

uses windows, ipifcons;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// This is the Id for IP Router Manager.  The Router Manager handles        //
// MIB-II, Forwarding MIB and some enterprise specific information.         //
// Calls made with any other ID are passed on to the corresponding protocol //
// For example, an MprAdminMIBXXX call with a protocol ID of PID_IP and    //
// a routing Id of 0xD will be sent to the IP Router Manager and then       //
// forwarded to OSPF                                                        //
// This lives in the same number space as the protocol Ids of RIP, OSPF     //
// etc, so any change made to it should be done keeping this in mind        //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

const

  IPRTRMGR_PID = 10000;
  ANY_SIZE = 1;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// The following  s are the Ids of the MIB variables made accessible  //
// to the user via MprAdminMIBXXX Apis.  It will be noticed that these are  //
// not the same as RFC 1213, since the MprAdminMIBXXX APIs work on rows and //
// groups instead of scalar variables                                       //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////


  IF_NUMBER           = 0;
  IF_TABLE            = (IF_NUMBER          + 1);
  IF_ROW              = (IF_TABLE           + 1);
  IP_STATS            = (IF_ROW             + 1);
  IP_ADDRTABLE        = (IP_STATS           + 1);
  IP_ADDRROW          = (IP_ADDRTABLE       + 1);
  IP_FORWARDNUMBER    = (IP_ADDRROW         + 1);
  IP_FORWARDTABLE     = (IP_FORWARDNUMBER   + 1);
  IP_FORWARDROW       = (IP_FORWARDTABLE    + 1);
  IP_NETTABLE         = (IP_FORWARDROW      + 1);
  IP_NETROW           = (IP_NETTABLE        + 1);
  ICMP_STATS          = (IP_NETROW          + 1);
  TCP_STATS           = (ICMP_STATS         + 1);
  TCP_TABLE           = (TCP_STATS          + 1);
  TCP_ROW             = (TCP_TABLE          + 1);
  UDP_STATS           = (TCP_ROW            + 1);
  UDP_TABLE           = (UDP_STATS          + 1);
  UDP_ROW             = (UDP_TABLE          + 1);
  MCAST_MFE           = (UDP_ROW            + 1);
  MCAST_MFE_STATS     = (MCAST_MFE          + 1);
  BEST_IF             = (MCAST_MFE_STATS    + 1);
  BEST_ROUTE          = (BEST_IF            + 1);
  PROXY_ARP           = (BEST_ROUTE         + 1);
  MCAST_IF_ENTRY      = (PROXY_ARP          + 1);
  MCAST_GLOBAL        = (MCAST_IF_ENTRY     + 1);
  IF_STATUS           = (MCAST_GLOBAL       + 1);
  MCAST_BOUNDARY      = (IF_STATUS          + 1);
  MCAST_SCOPE         = (MCAST_BOUNDARY     + 1);
  DEST_MATCHING       = (MCAST_SCOPE        + 1);
  DEST_LONGER         = (DEST_MATCHING      + 1);
  DEST_SHORTER        = (DEST_LONGER        + 1);
  ROUTE_MATCHING      = (DEST_SHORTER       + 1);
  ROUTE_LONGER        = (ROUTE_MATCHING     + 1);
  ROUTE_SHORTER       = (ROUTE_LONGER       + 1);
  ROUTE_STATE         = (ROUTE_SHORTER      + 1);

  NUMBER_OF_EXPORTED_VARIABLES    = (ROUTE_STATE + 1);


//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// MIB_OPAQUE_QUERY is the structure filled in by the user to identify a    //
// MIB variable                                                             //
//                                                                          //
//  dwVarId     ID of MIB Variable (One of the Ids  d above)          //
//  dwVarIndex  Variable sized array containing the indices needed to       //
//              identify a variable. NOTE: Unlike SNMP we dont require that //
//              a scalar variable be indexed by 0                           //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

type
MIB_OPAQUE_QUERY = record
  dwVarId : DWORD;
  rgdwVarIndex : array [0..ANY_SIZE - 1] of DWORD;
end;
PMIB_OPAQUE_QUERY = ^MIB_OPAQUE_QUERY;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// The following are the structures which are filled in and returned to the //
// user when a query is made, OR  are filled in BY THE USER when a set is   //
// done                                                                     //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

MIB_IFNUMBER = record
  dwValue : DWORD;
end;
PMIB_IFNUMBER = ^MIB_IFNUMBER;


const
  MAXLEN_IFDESCR = 256;
  MAXLEN_PHYSADDR = 8;
  MAX_INTERFACE_NAME_LEN  = 256;


type
MIB_IFROW = record
    wszName : array [0..MAX_INTERFACE_NAME_LEN - 1] of WideChar;
    dwIndex : DWORD;
    dwType : DWORD;
    dwMtu : DWORD;
    dwSpeed : DWORD;
    dwPhysAddrLen : DWORD;
    bPhysAddr : array [0..MAXLEN_PHYSADDR - 1] of Byte;
    dwAdminStatus : DWORD;
    dwOperStatus : DWORD;
    dwLastChange : DWORD;
    dwInOctets : DWORD;
    dwInUcastPkts : DWORD;
    dwInNUcastPkts : DWORD;
    dwInDiscards : DWORD;
    dwInErrors : DWORD;
    dwInUnknownProtos : DWORD;
    dwOutOctets : DWORD;
    dwOutUcastPkts : DWORD;
    dwOutNUcastPkts : DWORD;
    dwOutDiscards : DWORD;
    dwOutErrors : DWORD;
    dwOutQLen : DWORD;
    dwDescrLen : DWORD;
    bDescr : array [0..MAXLEN_IFDESCR - 1] of char;
end;
PMIB_IFROW = ^MIB_IFROW;

MIB_IFTABLE = record
    dwNumEntries : DWORD;
    table : array [0..ANY_SIZE - 1] of MIB_IFROW;
end;
PMIB_IFTABLE = ^MIB_IFTABLE;

MIBICMPSTATS = record
  dwMsgs : DWORD;
  dwErrors : DWORD;
  dwDestUnreachs : DWORD;
  dwTimeExcds : DWORD;
  dwParmProbs : DWORD;
  dwSrcQuenchs : DWORD;
  dwRedirects : DWORD;
  dwEchos : DWORD;
  dwEchoReps : DWORD;
  dwTimestamps : DWORD;
  dwTimestampReps : DWORD;
  dwAddrMasks : DWORD;
  dwAddrMaskReps : DWORD;
end;

MIBICMPINFO = record
  icmpInStats : MIBICMPSTATS;
  icmpOutState : MIBICMPSTATS;
end;

MIB_ICMP = record
  stats : MIBICMPINFO;
end;
PMIB_ICMP = ^MIB_ICMP;

MIB_UDPSTATS = record
   dwInDatagrams : DWORD;
   dwNoPorts : DWORD;
   dwInErrors : DWORD;
   dwOutDatagrams : DWORD;
   dwNumAddrs : DWORD;
end;
PMIB_UDPSTATS = ^MIB_UDPSTATS;

MIB_UDPROW = record
  dwLocalAddr : DWORD;
  dwLocalPort : DWORD;
end;
PMIB_UDPROW = ^MIB_UDPROW;

MIB_UDPTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_UDPROW;
end;
PMIB_UDPTABLE = ^MIB_UDPTABLE;


MIB_TCPSTATS = record
   dwRtoAlgorithm : DWORD;
   dwRtoMin : DWORD;
   dwRtoMax : DWORD;
   dwMaxConn : DWORD;
   dwActiveOpens : DWORD;
   dwPassiveOpens : DWORD;
   dwAttemptFails : DWORD;
   dwEstabResets : DWORD;
   dwCurrEstab : DWORD;
   dwInSegs : DWORD;
   dwOutSegs : DWORD;
   dwRetransSegs : DWORD;
   dwInErrs : DWORD;
   dwOutRsts : DWORD;
   dwNumConns : DWORD;
END;
PMIB_TCPSTATS = ^MIB_TCPSTATS;

const
  MIB_TCP_RTO_OTHER       = 1;
  MIB_TCP_RTO_CONSTANT    = 2;
  MIB_TCP_RTO_RSRE        = 3;
  MIB_TCP_RTO_VANJ        = 4;

  MIB_TCP_MAXCONN_DYNAMIC = DWORD (-1);

type
MIB_TCPROW = record
   dwState : DWORD;
   dwLocalAddr : DWORD;
   dwLocalPort : DWORD;
   dwRemoteAddr : DWORD;
   dwRemotePort : DWORD;
end;
PMIB_TCPROW = ^MIB_TCPROW;

const
  MIB_TCP_STATE_CLOSED            = 1;
  MIB_TCP_STATE_LISTEN            = 2;
  MIB_TCP_STATE_SYN_SENT          = 3;
  MIB_TCP_STATE_SYN_RCVD          = 4;
  MIB_TCP_STATE_ESTAB             = 5;
  MIB_TCP_STATE_FIN_WAIT1         = 6;
  MIB_TCP_STATE_FIN_WAIT2         = 7;
  MIB_TCP_STATE_CLOSE_WAIT        = 8;
  MIB_TCP_STATE_CLOSING           = 9;
  MIB_TCP_STATE_LAST_ACK          = 10;
  MIB_TCP_STATE_TIME_WAIT         = 11;
  MIB_TCP_STATE_DELETE_TCB        = 12;

type
MIB_TCPTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_TCPROW;
end;
PMIB_TCPTABLE = ^MIB_TCPTABLE;

const
  MIB_USE_CURRENT_TTL         = DWORD (-1);
  MIB_USE_CURRENT_FORWARDING  = DWORD (-1);

type
MIB_IPSTATS = record
  dwForwarding : DWORD;
  dwDefaultTTL : DWORD;
  dwInReceives : DWORD;
  dwInHdrErrors : DWORD;
  dwInAddrErrors : DWORD;
  dwForwDatagrams : DWORD;
  dwInUnknownProtos : DWORD;
  dwInDiscards : DWORD;
  dwInDelivers : DWORD;
  dwOutRequests : DWORD;
  dwRoutingDiscards : DWORD;
  dwOutDiscards : DWORD;
  dwOutNoRoutes : DWORD;
  dwReasmTimeout : DWORD;
  dwReasmReqds : DWORD;
  dwReasmOks : DWORD;
  dwReasmFails : DWORD;
  dwFragOks : DWORD;
  dwFragFails : DWORD;
  dwFragCreates : DWORD;
  dwNumIf : DWORD;
  dwNumAddr : DWORD;
  dwNumRoutes : DWORD;
end;
PMIB_IPSTATS = ^MIB_IPSTATS;

const
  MIB_IP_FORWARDING               = 1;
  MIB_IP_NOT_FORWARDING           = 2;

type
MIB_IPADDRROW = record
  dwAddr : DWORD;
  dwIndex : DWORD;
  dwMask : DWORD;
  dwBCastAddr : DWORD;
  dwReasmSize : DWORD;
  unused1 : word;
  unused2 : word;
end;
PMIB_IPADDRROW = ^MIB_IPADDRROW;


MIB_IPADDRTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPADDRROW;
end;
PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;

MIB_IPFORWARDNUMBER = record
  dwValue : DWORD;
end;
PMIB_IPFORWARDNUMBER = ^MIB_IPFORWARDNUMBER;

MIB_IPFORWARDROW = record
  dwForwardDest : DWORD;
  dwForwardMask : DWORD;
  dwForwardPolicy : DWORD;
  dwForwardNextHop : DWORD;
  dwForwardIfIndex : DWORD;
  dwForwardType : DWORD;
  dwForwardProto : DWORD;
  dwForwardAge : DWORD;
  dwForwardNextHopAS : DWORD;
  dwForwardMetric1 : DWORD;
  dwForwardMetric2 : DWORD;
  dwForwardMetric3 : DWORD;
  dwForwardMetric4 : DWORD;
  dwForwardMetric5 : DWORD;
end;
PMIB_IPFORWARDROW = ^MIB_IPFORWARDROW;

const
  MIB_IPROUTE_TYPE_OTHER	= 1;
  MIB_IPROUTE_TYPE_INVALID	= 2;
  MIB_IPROUTE_TYPE_DIRECT	= 3;
  MIB_IPROUTE_TYPE_INDIRECT	= 4;

  MIB_IPROUTE_METRIC_UNUSED	= DWORD (-1);

//
// THESE MUST MATCH the ids in routprot.h
//

  MIB_IPPROTO_OTHER		        = 1;
  MIB_IPPROTO_LOCAL		        = 2;
  MIB_IPPROTO_NETMGMT		        = 3;
  MIB_IPPROTO_ICMP			= 4;
  MIB_IPPROTO_EGP			= 5;
  MIB_IPPROTO_GGP			= 6;
  MIB_IPPROTO_HELLO		        = 7;
  MIB_IPPROTO_RIP			= 8;
  MIB_IPPROTO_IS_IS		        = 9;
  MIB_IPPROTO_ES_IS		        = 10;
  MIB_IPPROTO_CISCO		        = 11;
  MIB_IPPROTO_BBN			= 12;
  MIB_IPPROTO_OSPF			= 13;
  MIB_IPPROTO_BGP			= 14;

  MIB_IPPROTO_NT_AUTOSTATIC       = 10002;
  MIB_IPPROTO_NT_STATIC           = 10006;
  MIB_IPPROTO_NT_STATIC_NON_DOD   = 10007;

type
MIB_IPFORWARDTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPFORWARDROW;
end;
PMIB_IPFORWARDTABLE = ^MIB_IPFORWARDTABLE;



MIB_IPNETROW = record
  dwIndex : DWORD;
  dwPhysAddrLen : DWORD;
  bPhysAddr : array [0..MAXLEN_PHYSADDR - 1] of byte;
  dwAddr : DWORD;
  dwType : DWORD;
end;
PMIB_IPNETROW = ^MIB_IPNETROW;

const
  MIB_IPNET_TYPE_OTHER		= 1;
  MIB_IPNET_TYPE_INVALID   	= 2;
  MIB_IPNET_TYPE_DYNAMIC   	= 3;
  MIB_IPNET_TYPE_STATIC		= 4;

type
MIB_IPNETTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPNETROW;
end;
PMIB_IPNETTABLE = ^MIB_IPNETTABLE;

MIB_IPMCAST_OIF = record
  dwOutIfIndex : DWORD;
  dwNextHopAddr : DWORD;
  pvReserved : pointer;
  dwReserved : DWORD;
end;
PMIB_IPMCAST_OIF = ^MIB_IPMCAST_OIF;

MIB_IPMCAST_MFE = record
  dwGroup : DWORD;
  dwSource : DWORD;
  dwSrcMask : DWORD;
  dwUpStrmNgbr : DWORD;
  dwInIfIndex : DWORD;
  dwInIfProtocol : DWORD;
  dwRouteProtocol : DWORD;
  dwRouteNetwork : DWORD;
  dwRouteMask : DWORD;
  ulUpTime : LongWord;
  ulExpiryTime : LongWord;
  ulTimeOut : LongWord;
  ulNumOutIf : LongWord;
  fFlags : DWORD;
  dwReserved : DWORD;
  rgmioOutInfo : array [0..ANY_SIZE - 1] of MIB_IPMCAST_OIF;
end;
PMIB_IPMCAST_MFE = ^MIB_IPMCAST_MFE;

MIB_MFE_TABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPMCAST_MFE;
end;
PMIB_MFE_TABLE= ^MIB_MFE_TABLE;



MIB_IPMCAST_OIF_STATS = record
  dwOutIfIndex : DWORD;
  dwNextHopAddr : DWORD;
  pvDialContext : pointer;
  ulTtlTooLow : LongWord;
  ulFragNeeded : LongWord;
  ulOutPackets : LongWord;
  ulOutDiscards : LongWord;
end;
PMIB_IPMCAST_OIF_STATS= ^MIB_IPMCAST_OIF_STATS;

MIB_IPMCAST_MFE_STATS = record
  dwGroup : DWORD;
  dwSource : DWORD;
  dwSrcMask : DWORD;
  dwUpStrmNgbr : DWORD;
  dwInIfIndex : DWORD;
  dwInIfProtocol : DWORD;
  dwRouteProtocol : DWORD;
  dwRouteNetwork : DWORD;
  dwRouteMask : DWORD;
  ulUpTime : LongWord;
  ulExpiryTime : LongWord;
  ulNumOutIf : LongWord;
  ulInPkts : LongWord;
  ulInOctets : LongWord;
  ulPktsDifferentIf : LongWord;
  ulQueueOverflow : LongWord;
  rgmiosOutStats : array [0..ANY_SIZE - 1] of MIB_IPMCAST_OIF_STATS;
end;
PMIB_IPMCAST_MFE_STATS= ^MIB_IPMCAST_MFE_STATS;

MIB_MFE_STATS_TABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPMCAST_MFE_STATS;
end;
PMIB_MFE_STATS_TABLE= ^MIB_MFE_STATS_TABLE;

MIB_IPMCAST_GLOBAL = record
  dwEnable : DWORD;
end;
PMIB_IPMCAST_GLOBAL = ^MIB_IPMCAST_GLOBAL;

MIB_IPMCAST_IF_ENTRY = record
  dwIfIndex : DWORD;
  dwTtl : DWORD;
  dwProtocol : DWORD;
  dwRateLimit : DWORD;
  ulInMcastOctets : LongWord;
  ulOutMcastOctets : LongWord;
end;
PMIB_IPMCAST_IF_ENTRY = ^MIB_IPMCAST_IF_ENTRY;

MIB_IPMCAST_IF_TABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPMCAST_IF_ENTRY;
end;
PMIB_IPMCAST_IF_TABLE = ^MIB_IPMCAST_IF_TABLE;

MIB_IPMCAST_BOUNDARY = record
  dwIfIndex : DWORD;
  dwGroupAddress : DWORD;
  dwGroupMask : DWORD;
  dwStatus : DWORD;
end;
PMIB_IPMCAST_BOUNDARY = ^MIB_IPMCAST_BOUNDARY;

MIB_IPMCAST_BOUNDARY_TABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPMCAST_BOUNDARY;
end;
PMIB_IPMCAST_BOUNDARY_TABLE = ^MIB_IPMCAST_BOUNDARY_TABLE;


MIB_BOUNDARYROW = record
  dwGroupAddress : DWORD;
  dwGroupMask : DWORD;
end;
PMIB_BOUNDARYROW = ^MIB_BOUNDARYROW;

// Structure matching what goes in the registry in a block of type
// IP_MCAST_LIMIT_INFO.  This contains the fields of
// MIB_IPMCAST_IF_ENTRY which are configurable.

MIB_MCAST_LIMIT_ROW = record
  dwTtl : DWORD;
  dwRateLimit : DWORD;
end;
PMIB_MCAST_LIMIT_ROW = ^MIB_MCAST_LIMIT_ROW;

const
  MAX_SCOPE_NAME_LEN = 255;

//
// Scope names are unicode.  SNMP and MZAP use UTF-8 encoding.
//

{$DEFINE SN_UNICODE}
type
  SN_CHAR = WideChar;
  SCOPE_NAME_BUFFER = array [0..MAX_SCOPE_NAME_LEN] of SN_CHAR;
  SCOPE_NAME = ^SCOPE_NAME_BUFFER;

MIB_IPMCAST_SCOPE = record
  dwGroupAddress : DWORD;
  dwGroupMask : DWORD;
  snNameBuffer : SCOPE_NAME_BUFFER;
  dwStatus : DWORD;
end;
PMIB_IPMCAST_SCOPE = ^MIB_IPMCAST_SCOPE;

MIB_IPDESTROW = record
  ForwardRow : MIB_IPFORWARDROW;
  dwForwardPreference : DWORD;
  dwForwardViewSet : DWORD;
end;
PMIB_IPDESTROW= ^MIB_IPDESTROW;

MIB_IPDESTTABLE = record
  dwNumEntries : DWORD;
  table : array [0..ANY_SIZE - 1] of MIB_IPDESTROW;
end;
PMIB_IPDESTTABLE= ^MIB_IPDESTTABLE;

MIB_BEST_IF = record
  dwDestAddr : DWORD;
  dwIfIndex : DWORD;
end;
PMIB_BEST_IF = ^MIB_BEST_IF;

MIB_PROXYARP = record
  dwAddress : DWORD;
  dwMask : DWORD;
  dwIfIndex : DWORD;
end;
PMIB_PROXYARP = ^MIB_PROXYARP;

MIB_IFSTATUS = record
  dwIfIndex : DWORD;
  dwAdminStatus : DWORD;
  dwOperationalStatus : DWORD;
  bMHbeatActive : BOOL;
  bMHbeatAlive : BOOL;
end;
PMIB_IFSTATUS = ^MIB_IFSTATUS;

MIB_ROUTESTATE = record
  bRoutesSetToStack : BOOL;
end;
PMIB_ROUTESTATE = ^MIB_ROUTESTATE;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// All the info passed to (SET/CREATE) and from (GET/GETNEXT/GETFIRST)      //
// IP Router Manager is encapsulated in the following "discriminated"       //
// union.  To pass, say MIB_IFROW, use the following code                   //
//                                                                          //
//  PMIB_OPAQUE_INFO    pInfo;                                              //
//  PMIB_IFROW          pIfRow;                                             //
//  gdwBuff[(MAX_MIB_OFFSET + sizeof(MIB_IFROW))/sizeof(DWORD) + 1]; / : DWORD/
//                                                                          //
//  pInfo   = (PMIB_OPAQUE_INFO)rgdwBuffer;                                 //
//  pIfRow  = (MIB_IFROW *)(pInfo->rgbyData);                               //
//                                                                          //
//  This can also be accomplished by using the following macro              //
//                                                                          //
//  DEFINE_MIB_BUFFER(pInfo,MIB_IFROW, pIfRow);                             //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////


MIB_OPAQUE_INFO = record
  case dwId : DWORD of
    0 : (ullAlign : Int64);
    1 : (rgbyData : array [0..0] of byte);
end;
PMIB_OPAQUE_INFO = ^MIB_OPAQUE_INFO;

const
  MAX_MIB_OFFSET      = 8;

implementation
function SIZEOF_IFTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IFTABLE) + (x - 1) * sizeof (MIB_IFROW);
end;

function SIZEOF_UDPTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_UDPTABLE) + (x - 1) * sizeof (MIB_UDPROW);
end;

function SIZEOF_TCPTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_TCPTABLE) + (x - 1) * sizeof (MIB_TCPROW);
end;

function SIZEOF_IPADDRTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPADDRTABLE) + (x - 1) * sizeof (MIB_IPADDRROW);
end;

function SIZEOF_IPFORWARDTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPFORWARDTABLE) + (x - 1) * sizeof (MIB_IPFORWARDROW);
end;

function SIZEOF_IPNETTABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPNETTABLE) + (x - 1) * sizeof (MIB_IPNETROW);
end;

function SIZEOF_BASIC_MIB_MFE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPMCAST_MFE) + (x - 1) * sizeof (MIB_IPMCAST_OIF);
end;

function SIZEOF_MIB_MFE (x : Integer) : Integer;
begin
  result := sizeof (MIB_MFE_TABLE) + (x - 1) * sizeof (MIB_IPMCAST_MFE);
end;

function SIZEOF_BASIC_MIB_MFE_STATS (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPMCAST_MFE_STATS) + (x - 1) * sizeof (MIB_IPMCAST_OIF_STATS);
end;

function SIZEOF_MIB_MFE_STATS (x : Integer) : Integer;
begin
  result := sizeof (MIB_MFE_STATS_TABLE) + (x - 1) * sizeof (MIB_IPMCAST_MFE_STATS);
end;

function SIZEOF_MCAST_IF_TABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPMCAST_IF_TABLE) + (x - 1) * sizeof (MIB_IPMCAST_IF_ENTRY);
end;

function SIZEOF_BOUNDARY_TABLE (x : Integer) : Integer;
begin
  result := sizeof (MIB_IPMCAST_BOUNDARY_TABLE) + (x - 1) * sizeof (MIB_IPMCAST_BOUNDARY);
end;

// MIB_INFO_SIZE(S)                \
//     (MAX_MIB_OFFSET + sizeof(S))

//   MIB_INFO_SIZE_IN_DWORDS(S)      \
//     ((MIB_INFO_SIZE(S))/sizeof(DWORD) + 1)

//   DEFINE_MIB_BUFFER(X,Y,Z)                                        \
//     DWORD		__rgdwBuff[MIB_INFO_SIZE_IN_DWORDS(Y)]; \
//     PMIB_OPAQUE_INFO    X = (PMIB_OPAQUE_INFO)__rgdwBuff;               \
//     Y *                 Z = (Y *)(X->rgbyData)


//   CAST_MIB_INFO(X,Y,Z)    Z = (Y)(X->rgbyData)

end.
