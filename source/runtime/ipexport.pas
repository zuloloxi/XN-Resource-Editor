unit ipexport;

interface

{$WEAKPACKAGEUNIT}

uses Windows;

type

//
// IP type definitions.
//

  IPAddr = LongWord;     // An IP address.
  IPMask = LongWord;     // An IP subnet mask.
  IP_STATUS = LongWord;  // Status code returned from IP APIs.


//
// The ip_option_information structure describes the options to be
// included in the header of an IP packet. The TTL, TOS, and Flags
// values are carried in specific fields in the header. The OptionsData
// bytes are carried in the options area following the standard IP header.
// With the exception of source route options, this data must be in the
// format to be transmitted on the wire as specified in RFC 791. A source
// route option should contain the full route - first hop thru final
// destination - in the route data. The first hop will be pulled out of the
// data and the option will be reformatted accordingly. Otherwise, the route
// option should be formatted as specified in RFC 791.
//

ip_option_information  = record
  Ttl : byte;             // Time To Live
  Tos : byte;             // Type Of Service
  Flags : byte;           // IP header flags
  OptionsSize : byte;     // Size in bytes of options data
  OptionsData : PByte;    // Pointer to options data
end;
pip_option_information = ^ip_option_information;

//
// The icmp_echo_reply structure describes the data returned in response
// to an echo request.
//

icmp_echo_reply = record
  Address : IPAddr;           // Replying address
  Status : LongWord;          // Reply IP_STATUS
  RoundTripTime : LongWord;   // RTT in milliseconds
  DataSize : word;            // Reply data size in bytes
  Reserved : word;            // Reserved for system use
  Data : pointer;             // Pointer to the reply data
  Options : ip_option_information;   // Reply options
end;
picmp_echo_reply = ^icmp_echo_reply;


ArpRequestBuffer = record
  DestAddress : IPAddr;
  SrcAddress : IPAddr;
end;
pArpRequestBuffer = ^ArpRequestBuffer;

TCP_RESERVE_PORT_RANGE = record
  UpperRange : word;
  LowerRange : word;
end;
PTCP_RESERVE_PORT_RANGE = ^TCP_RESERVE_PORT_RANGE;

const
  MAX_ADAPTER_NAME = 128;

type
IP_ADAPTER_INDEX_MAP = record
  Index : LongWord;
  Name : array [0..MAX_ADAPTER_NAME - 1] of WideChar;
end;
PIP_ADAPTER_INDEX_MAP = ^IP_ADAPTER_INDEX_MAP;

IP_INTERFACE_INFO = record
  NumAdapters : LongInt;
  Adapter : array [0..0] of IP_ADAPTER_INDEX_MAP;
end;
PIP_INTERFACE_INFO = ^IP_INTERFACE_INFO;

IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = record
  NumAdapters : LongInt;
  Address : array [0..0] of IPAddr;
end;
PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS= ^IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;

IP_ADAPTER_ORDER_MAP = record
  NumAdapters : LongInt;
  AdapterOrder : array [0..0] of LongInt;
end;
PIP_ADAPTER_ORDER_MAP = ^IP_ADAPTER_ORDER_MAP;

const
//
// IP_STATUS codes returned from IP APIs
//

  IP_STATUS_BASE              = 11000;

  IP_SUCCESS                  = 0;
  IP_BUF_TOO_SMALL            = (IP_STATUS_BASE + 1);
  IP_DEST_NET_UNREACHABLE     = (IP_STATUS_BASE + 2);
  IP_DEST_HOST_UNREACHABLE    = (IP_STATUS_BASE + 3);
  IP_DEST_PROT_UNREACHABLE    = (IP_STATUS_BASE + 4);
  IP_DEST_PORT_UNREACHABLE    = (IP_STATUS_BASE + 5);
  IP_NO_RESOURCES             = (IP_STATUS_BASE + 6);
  IP_BAD_OPTION               = (IP_STATUS_BASE + 7);
  IP_HW_ERROR                 = (IP_STATUS_BASE + 8);
  IP_PACKET_TOO_BIG           = (IP_STATUS_BASE + 9);
  IP_REQ_TIMED_OUT            = (IP_STATUS_BASE + 10);
  IP_BAD_REQ                  = (IP_STATUS_BASE + 11);
  IP_BAD_ROUTE                = (IP_STATUS_BASE + 12);
  IP_TTL_EXPIRED_TRANSIT      = (IP_STATUS_BASE + 13);
  IP_TTL_EXPIRED_REASSEM      = (IP_STATUS_BASE + 14);
  IP_PARAM_PROBLEM            = (IP_STATUS_BASE + 15);
  IP_SOURCE_QUENCH            = (IP_STATUS_BASE + 16);
  IP_OPTION_TOO_BIG           = (IP_STATUS_BASE + 17);
  IP_BAD_DESTINATION          = (IP_STATUS_BASE + 18);


//
// The next group are status codes passed up on status indications to
// transport layer protocols.
//
  IP_ADDR_DELETED             = (IP_STATUS_BASE + 19);
  IP_SPEC_MTU_CHANGE          = (IP_STATUS_BASE + 20);
  IP_MTU_CHANGE               = (IP_STATUS_BASE + 21);
  IP_UNLOAD                   = (IP_STATUS_BASE + 22);
  IP_ADDR_ADDED               = (IP_STATUS_BASE + 23);
  IP_MEDIA_CONNECT            = (IP_STATUS_BASE + 24);
  IP_MEDIA_DISCONNECT         = (IP_STATUS_BASE + 25);
  IP_BIND_ADAPTER             = (IP_STATUS_BASE + 26);
  IP_UNBIND_ADAPTER           = (IP_STATUS_BASE + 27);
  IP_DEVICE_DOES_NOT_EXIST    = (IP_STATUS_BASE + 28);
  IP_DUPLICATE_ADDRESS        = (IP_STATUS_BASE + 29);
  IP_INTERFACE_METRIC_CHANGE  = (IP_STATUS_BASE + 30);
  IP_RECONFIG_SECFLTR         = (IP_STATUS_BASE + 31);
  IP_NEGOTIATING_IPSEC        = (IP_STATUS_BASE + 32);
  IP_INTERFACE_WOL_CAPABILITY_CHANGE  = (IP_STATUS_BASE + 33);
  IP_DUPLICATE_IPADD          = (IP_STATUS_BASE + 34);

  IP_GENERAL_FAILURE          = (IP_STATUS_BASE + 50);
  MAX_IP_STATUS               = IP_GENERAL_FAILURE;
  IP_PENDING                  = (IP_STATUS_BASE + 255);


//
// Values used in the IP header Flags field.
//
  IP_FLAG_DF      = $2;         // Don't fragment this packet.

//
// Supported IP Option Types.
//
// These types define the options which may be used in the OptionsData field
// of the ip_option_information structure.  See RFC 791 for a complete
// description of each.
//
  IP_OPT_EOL      = 0;         // End of list option
  IP_OPT_NOP      = 1;         // No operation
  IP_OPT_SECURITY = $82;       // Security option
  IP_OPT_LSRR     = $83;       // Loose source route
  IP_OPT_SSRR     = $89;       // Strict source route
  IP_OPT_RR       = $7;        // Record route
  IP_OPT_TS       = $44;       // Timestamp
  IP_OPT_SID      = $88;       // Stream ID (obsolete)
  IP_OPT_ROUTER_ALERT = $94;    // Router Alert Option

  MAX_OPT_SIZE    = 40;        // Maximum length of IP options in bytes

// Ioctls code exposed by Memphis tcpip stack.
// For NT these ioctls are define in ntddip.h  (private\inc)

  IOCTL_IP_RTCHANGE_NOTIFY_REQUEST   = 101;
  IOCTL_IP_ADDCHANGE_NOTIFY_REQUEST  = 102;
  IOCTL_ARP_SEND_REQUEST             = 103;
  IOCTL_IP_INTERFACE_INFO            = 104;
  IOCTL_IP_GET_BEST_INTERFACE        = 105;
  IOCTL_IP_UNIDIRECTIONAL_ADAPTER_ADDRESS        = 106;

implementation

end.
