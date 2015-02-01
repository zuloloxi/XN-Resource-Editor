unit iphlpapi;

interface

uses Windows, iprtrmib, ipexport, iptypes;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// IPRTRMIB.H has the definitions of the strcutures used to set and get     //
// information                                                              //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// The GetXXXTable APIs take a buffer and a size of buffer.  If the buffer  //
// is not large enough, they APIs return ERROR_INSUFFICIENT_BUFFER  and     //
// *pdwSize is the required buffer size                                     //
// The bOrder is a BOOLEAN, which if TRUE sorts the table according to      //
// MIB-II (RFC XXXX)                                                        //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Retrieves the number of interfaces in the system. These include LAN and  //
// WAN interfaces                                                           //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////


function GetNumberOfInterfaces(var pdwNumIf : DWORD) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the MIB-II ifEntry                                                  //
// The dwIndex field of the MIB_IFROW should be set to the index of the     //
// interface being queried                                                  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIfEntry(pIfRow : PMIB_IFROW) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the MIB-II IfTable                                                  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIfTable(
    pIfTable : PMIB_IFTABLE;
    var pdwSize : DWORD;
    bOrder : BOOL) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the Interface to IP Address mapping                                 //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIpAddrTable(
    pIpAddrTable : PMIB_IPADDRTABLE;
    var pdwSize : DWORD;
    bOrder : BOOL) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the current IP Address to Physical Address (ARP) mapping            //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIpNetTable(
    pIpNetTable : PMIB_IPNETTABLE;
    var pdwSize : DWORD;
    bOrder : BOOL) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the IP Routing Table  (RFX XXXX)                                    //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIpForwardTable(
    pIpForwardTable : PMIB_IPFORWARDTABLE;
    var pdwSize : DWORD;
    bOrder : BOOL) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets TCP Connection/UDP Listener Table                                   //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetTcpTable(
    pTcpTable : PMIB_TCPTABLE;
    var pdwSize : DWORD;
    bOrder : BOOL) : DWORD; stdcall;

function GetUdpTable(
    pUdpTable : PMIB_UDPTABLE;
    var pdwSize : DWORD;
    bORder : BOOL) : DWORD; stdcall;


//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets IP/ICMP/TCP/UDP Statistics                                          //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetIpStatistics(var pStats : MIB_IPSTATS) : DWORD; stdcall;

function GetIcmpStatistics(var pStats : MIB_ICMP) : DWORD; stdcall;

function GetTcpStatistics(var pStats : MIB_TCPSTATS) : DWORD; stdcall;

function GetUdpStatistics(var pStats : MIB_UDPSTATS) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to set the ifAdminStatus on an interface.  The only fields of the   //
// MIB_IFROW that are relevant are the dwIndex (index of the interface      //
// whose status needs to be set) and the dwAdminStatus which can be either  //
// MIB_IF_ADMIN_STATUS_UP or MIB_IF_ADMIN_STATUS_DOWN                       //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function SetIfEntry(pIfRow : PMIB_IFROW) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to create, modify or delete a route.  In all cases the              //
// dwForwardIfIndex, dwForwardDest, dwForwardMask, dwForwardNextHop and     //
// dwForwardPolicy MUST BE SPECIFIED. Currently dwForwardPolicy is unused   //
// and MUST BE 0.                                                           //
// For a set, the complete MIB_IPFORWARDROW structure must be specified     //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function CreateIpForwardEntry(pRoute :PMIB_IPFORWARDROW) : DWORD; stdcall;
function SetIpForwardEntry(pRoute : PMIB_IPFORWARDROW) : DWORD; stdcall;
function DeleteIpForwardEntry(pRoute :PMIB_IPFORWARDROW) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to set the ipForwarding to ON or OFF (currently only ON->OFF is     //
// allowed) and to set the defaultTTL.  If only one of the fields needs to  //
// be modified and the other needs to be the same as before the other field //
// needs to be set to MIB_USE_CURRENT_TTL or MIB_USE_CURRENT_FORWARDING as  //
// the case may be                                                          //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////


function SetIpStatistics(var pIpStats : MIB_IPSTATS) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to set the defaultTTL.                                              //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function SetIpTTL(nTTL : UINT) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to create, modify or delete an ARP entry.  In all cases the dwIndex //
// dwAddr field MUST BE SPECIFIED.                                          //
// For a set, the complete MIB_IPNETROW structure must be specified         //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function CreateIpNetEntry(pArpEntry : PMIB_IPNETROW) : DWORD; stdcall;

function SetIpNetEntry(pArpEntry : PMIB_IPNETROW) : DWORD; stdcall;

function DeleteIpNetEntry(pArpEntry : PMIB_IPNETROW) : DWORD; stdcall;

function FlushIpNetTable(dwIfIndex : DWORD) : DWORD; stdcall;


//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to create or delete a Proxy ARP entry. The dwIndex is the index of  //
// the interface on which to PARP for the dwAddress.  If the interface is   //
// of a type that doesnt support ARP, e.g. PPP, then the call will fail     //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function CreateProxyArpEntry(dwAddress, dwMask, dwIfIndex : DWORD) : DWORD; stdcall;

function DeleteProxyArpEntry(dwdwAddress, dwMask, dwIfIndex : DWORD) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Used to set the state of a TCP Connection. The only state that it can be //
// set to is MIB_TCP_STATE_DELETE_TCB.  The complete MIB_TCPROW structure   //
// MUST BE SPECIFIED                                                        //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function SetTcpEntry(pTcpRow : PMIB_TCPROW) : DWORD; stdcall;


function GetInterfaceInfo(
  pIfTable : PIP_INTERFACE_INFO;
  var dwOutBufLen : DWORD) : DWORD; stdcall;

function GetUniDirectionalAdapterInfo(
  pIPIfInfo : PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
  var dwOutBufLen : DWORD) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the "best" outgoing interface for the specified destination address //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetBestInterface(
    dwDestAddr : IPAddr;
    var pdwBestIfIndex : DWORD
    ) : DWORD; stdcall;

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// Gets the best (longest matching prefix) route for the given destination  //
// If the source address is also specified (i.e. is not 0x00000000), and    //
// there are multiple "best" routes to the given destination, the returned  //
// route will be one that goes out over the interface which has an address  //
// that matches the source address                                          //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

function GetBestRoute(
    dwDestAddr, dwSourceAddr : DWORD;
    pBestRoute : PMIB_IPFORWARDROW) : DWORD; stdcall;

function NotifyAddrChange(
  handle : PHANDLE;
  overlapped : POVERLAPPED) : DWORD; stdcall;


function NotifyRouteChange(
    Handle : PHandle;
    overlapped : POVERLAPPED) : DWORD; stdcall;


function GetAdapterIndex(
    AdapterName : PWideChar;
    var idx : DWORD) : DWORD; stdcall;

function AddIPAddress(
    Address : IPAddr;
    IpMask : IPMask;
    IfIndex : DWORD;
    var NTEContext : DWORD;
    var NTEInstance : DWORD) : DWORD; stdcall;

function DeleteIPAddress(
    NTEContext : DWORD) : DWORD; stdcall;

function GetNetworkParams(
    pFixedInfo : PFIXED_INFO;
    var OutBufLen : DWORD) : DWORD; stdcall;

function GetAdaptersInfo(
    pAdapterInfo : PIP_ADAPTER_INFO;
    var pOutBufLen : DWORD) : DWORD; stdcall;

function GetPerAdapterInfo(
    IfIndex : DWORD;
    pPerAdapterInfo : PIP_PER_ADAPTER_INFO;
    var pOutBufLen : DWORD) : DWORD; stdcall;

function IpReleaseAddress(
    AdapterInfo : PIP_ADAPTER_INDEX_MAP) : DWORD; stdcall;


function IpRenewAddress(
    AdapterInfo : PIP_ADAPTER_INDEX_MAP) : DWORD; stdcall;

function SendARP(
    DestIP, SrcIP : IPAddr;
    var MacAddr : DWORD;
    var PhyAddrLen : DWORD) : DWORD; stdcall;

function GetRTTAndHopCount(
    DestIpAddress : IPAddr;
    var HopCount : DWORD;
    MaxHops : DWORD;
    var RTT : DWORD) : DWORD; stdcall;

function GetFriendlyIfIndex(
    IfIndex : DWORD) : DWORD; stdcall;

function EnableRouter(
    pHandle : PHANDLE;
    pOverlapped : POverlapped) : DWORD; stdcall;

function UnenableRouter(
    poverlapped : pOverlapped;
    lpdwEnableCount : PDWORD) : DWORD; stdcall;

implementation
const
  iphlpapi_dll = 'iphlpapi.dll';

function GetNumberOfInterfaces;        external iphlpapi_dll;
function GetIfEntry;                   external iphlpapi_dll;
function GetIfTable;                   external iphlpapi_dll;
function GetIpAddrTable;               external iphlpapi_dll;
function GetIpNetTable;                external iphlpapi_dll;
function GetIpForwardTable;            external iphlpapi_dll;
function GetTcpTable;                  external iphlpapi_dll;
function GetUdpTable;                  external iphlpapi_dll;
function GetIpStatistics;              external iphlpapi_dll;
function GetIcmpStatistics;            external iphlpapi_dll;
function GetTcpStatistics;             external iphlpapi_dll;
function GetUdpStatistics;             external iphlpapi_dll;
function SetIfEntry;                   external iphlpapi_dll;
function CreateIpForwardEntry;         external iphlpapi_dll;
function SetIpForwardEntry;            external iphlpapi_dll;
function DeleteIpForwardEntry;         external iphlpapi_dll;
function SetIpStatistics;              external iphlpapi_dll;
function SetIpTTL;                     external iphlpapi_dll;
function CreateIpNetEntry;             external iphlpapi_dll;
function SetIpNetEntry;                external iphlpapi_dll;
function DeleteIpNetEntry;             external iphlpapi_dll;
function FlushIpNetTable;              external iphlpapi_dll;
function CreateProxyArpEntry;          external iphlpapi_dll;
function DeleteProxyArpEntry;          external iphlpapi_dll;
function SetTcpEntry;                  external iphlpapi_dll;
function GetInterfaceInfo;             external iphlpapi_dll;
function GetUniDirectionalAdapterInfo; external iphlpapi_dll;
function GetBestInterface;             external iphlpapi_dll;
function GetBestRoute;                 external iphlpapi_dll;
function NotifyAddrChange;             external iphlpapi_dll;
function NotifyRouteChange;            external iphlpapi_dll;
function GetAdapterIndex;              external iphlpapi_dll;
function AddIPAddress;                 external iphlpapi_dll;
function DeleteIPAddress;              external iphlpapi_dll;
function GetNetworkParams;             external iphlpapi_dll;
function GetAdaptersInfo;              external iphlpapi_dll;
function GetPerAdapterInfo;            external iphlpapi_dll;
function IpReleaseAddress;             external iphlpapi_dll;
function IpRenewAddress;               external iphlpapi_dll;
function SendARP;                      external iphlpapi_dll;
function GetRTTAndHopCount;            external iphlpapi_dll;
function GetFriendlyIfIndex;           external iphlpapi_dll;
function EnableRouter;                 external iphlpapi_dll;
function UnenableRouter;               external iphlpapi_dll;
end.
