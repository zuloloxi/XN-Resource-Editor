unit iptypes;

interface

{$WEAKPACKAGEUNIT}

uses Windows;

const
  MAX_ADAPTER_DESCRIPTION_LENGTH  = 128; // arb.
  MAX_ADAPTER_NAME_LENGTH         = 256; // arb.
  MAX_ADAPTER_ADDRESS_LENGTH      = 8;   // arb.
  DEFAULT_MINIMUM_ENTITIES        = 32;  // arb.
  MAX_HOSTNAME_LEN                = 128; // arb.
  MAX_DOMAIN_NAME_LEN             = 128; // arb.
  MAX_SCOPE_ID_LEN                = 256; // arb.

//
// types
//

// Node Type

  BROADCAST_NODETYPE              = 1;
  PEER_TO_PEER_NODETYPE           = 2;
  MIXED_NODETYPE                  = 4;
  HYBRID_NODETYPE                 = 8;

// Adapter Type

  IF_OTHER_ADAPTERTYPE            = 0;
  IF_ETHERNET_ADAPTERTYPE         = 1;
  IF_TOKEN_RING_ADAPTERTYPE       = 2;
  IF_FDDI_ADAPTERTYPE             = 3;
  IF_PPP_ADAPTERTYPE              = 4;
  IF_LOOPBACK_ADAPTERTYPE         = 5;
  IF_SLIP_ADAPTERTYPE             = 6;

//
// IP_ADDRESS_STRING - store an IP address as a dotted decimal string
//

type

IP_ADDRESS_STRING = record
  _String : array [0..4*4-1] of char;
end;
PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
IP_MASK_STRING = IP_ADDRESS_STRING;
PIP_MASK_STRING = ^IP_MASK_STRING;

//
// IP_ADDR_STRING - store an IP address with its corresponding subnet mask,
// both as dotted decimal strings
//

PIP_ADDR_STRING = ^IP_ADDR_STRING;
IP_ADDR_STRING = record
  Next : PIP_ADDR_STRING;
  IpAddress : IP_ADDRESS_STRING;
  IpMask : IP_MASK_STRING;
  Context : DWORD;
end;

//
// ADAPTER_INFO - per-adapter information. All IP addresses are stored as
// strings
//

PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
IP_ADAPTER_INFO  = record
  Next : PIP_ADAPTER_INFO;
  ComboIndex : DWORD;
  AdapterName : array [0..MAX_ADAPTER_NAME_LENGTH + 4 - 1] of char;
  Description : array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 4 - 1] of char;
  AddressLength : DWORD;
  Address : array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of byte;
  Index : DWORD;
  _Type : UINT;
  DhcpEnabled : UINT;
  CurrentIpAddress : PIP_ADDR_STRING;
  IpAddressList : IP_ADDR_STRING;
  GatewayList : IP_ADDR_STRING;
  DhcpServer : IP_ADDR_STRING;
  HaveWins : BOOL;
  PrimaryWinsServer : IP_ADDR_STRING;
  SecondaryWinsServer : IP_ADDR_STRING;
  LeaseObtained : DWORD; // time_t
  LeaseExpires : DWORD; // time_t
end;

//
// IP_PER_ADAPTER_INFO - per-adapter IP information such as DNS server list.
//

IP_PER_ADAPTER_INFO = record
  AutoconfigEnabled : UINT;
  AutoconfigActive : UINT;
  CurrentDnsServer : PIP_ADDR_STRING;
  DnsServerList : IP_ADDR_STRING;
end;
PIP_PER_ADAPTER_INFO= ^IP_PER_ADAPTER_INFO;

//
// FIXED_INFO - the set of IP-related information which does not depend on DHCP
//

FIXED_INFO = record
  HostName : array [0..MAX_HOSTNAME_LEN + 4 - 1] of char;
  DomainName : array [0..MAX_DOMAIN_NAME_LEN + 4 - 1] of char;
  CurrentDnsServer : PIP_ADDR_STRING;
  DnsServerList : IP_ADDR_STRING;
  NodeType : UINT;
  ScopeId : array [0..MAX_SCOPE_ID_LEN + 4 - 1] of char;
  EnableRouting : UINT;
  EnableProxy : UINT;
  EnableDns : UINT;
end;
PFIXED_INFO = ^FIXED_INFO;

implementation

end.
