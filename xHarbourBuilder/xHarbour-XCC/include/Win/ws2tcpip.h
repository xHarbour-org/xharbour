#ifndef _WS2TCPIP_H
#define _WS2TCPIP_H

/* WinSock2 Extension for TCP/IP protocols */

struct ip_mreq {
    struct in_addr imr_multiaddr;
    struct in_addr imr_interface;
};

struct ip_mreq_source {
    struct in_addr imr_multiaddr;
    struct in_addr imr_sourceaddr;
    struct in_addr imr_interface;
};

struct ip_msfilter {
    struct in_addr imsf_multiaddr;
    struct in_addr imsf_interface;
    u_long imsf_fmode;
    u_long imsf_numsrc;
    struct in_addr imsf_slist[1];
};

#define IP_MSFILTER_SIZE(numsrc)  (sizeof(struct ip_msfilter)-sizeof(struct in_addr) + (numsrc)*sizeof(struct in_addr))

#define MCAST_INCLUDE 0
#define MCAST_EXCLUDE 1

#define SIO_GET_INTERFACE_LIST  _IOR('t',127,u_long)
#define SIO_GET_INTERFACE_LIST_EX  _IOR('t',126,u_long)
#define SIO_SET_MULTICAST_FILTER  _IOW('t',125,u_long)
#define SIO_GET_MULTICAST_FILTER  _IOW('t',124|IOC_IN,u_long)

#define IP_OPTIONS  1
#define IP_HDRINCL  2
#define IP_TOS  3
#define IP_TTL  4
#define IP_MULTICAST_IF  9
#define IP_MULTICAST_TTL  10
#define IP_MULTICAST_LOOP  11
#define IP_ADD_MEMBERSHIP  12
#define IP_DROP_MEMBERSHIP  13
#define IP_DONTFRAGMENT  14
#define IP_ADD_SOURCE_MEMBERSHIP  15
#define IP_DROP_SOURCE_MEMBERSHIP  16
#define IP_BLOCK_SOURCE  17
#define IP_UNBLOCK_SOURCE  18
#define IP_PKTINFO  19
#define IP_RECEIVE_BROADCAST  22

#define IPV6_HDRINCL  2
#define IPV6_UNICAST_HOPS  4
#define IPV6_MULTICAST_IF  9
#define IPV6_MULTICAST_HOPS  10
#define IPV6_MULTICAST_LOOP  11
#define IPV6_ADD_MEMBERSHIP  12
#define IPV6_DROP_MEMBERSHIP  13
#define IPV6_JOIN_GROUP  IPV6_ADD_MEMBERSHIP
#define IPV6_LEAVE_GROUP  IPV6_DROP_MEMBERSHIP
#define IPV6_PKTINFO  19
#define IPV6_HOPLIMIT  21
#define IPV6_PROTECTION_LEVEL  23

#define PROTECTION_LEVEL_UNRESTRICTED  10
#define PROTECTION_LEVEL_DEFAULT  20
#define PROTECTION_LEVEL_RESTRICTED  30

#define UDP_NOCHECKSUM  1
#define UDP_CHECKSUM_COVERAGE  20

#define TCP_EXPEDITED_1122  0x0002

#define NI_MAXHOST  1025
#define NI_MAXSERV  32

#define INET_ADDRSTRLEN  22
#define INET6_ADDRSTRLEN  65

#define NI_NOFQDN  0x01
#define NI_NUMERICHOST  0x02
#define NI_NAMEREQD  0x04
#define NI_NUMERICSERV  0x08
#define NI_DGRAM  0x10

#ifndef s6_addr
struct in6_addr {
    union {
        u_char Byte[16];
        u_short Word[8];
    } u;
};
#define in_addr6 in6_addr

#define _S6_un  u
#define _S6_u8  Byte
#define s6_addr  _S6_un._S6_u8
#define s6_bytes  u.Byte
#define s6_words  u.Word
#endif /* s6_addr */

typedef struct ipv6_mreq {
    struct in6_addr ipv6mr_multiaddr;
    unsigned int ipv6mr_interface;
} IPV6_MREQ;

struct sockaddr_in6_old {
    short sin6_family;
    u_short sin6_port;
    u_long sin6_flowinfo;
    struct in6_addr sin6_addr;
};

struct sockaddr_in6 {
    short sin6_family;
    u_short sin6_port;
    u_long sin6_flowinfo;
    struct in6_addr sin6_addr;
    u_long sin6_scope_id;
};

typedef struct in6_addr IN6_ADDR;
typedef struct in6_addr *PIN6_ADDR;
typedef struct in6_addr *LPIN6_ADDR;

typedef struct sockaddr_in6 SOCKADDR_IN6;
typedef struct sockaddr_in6 *PSOCKADDR_IN6;
typedef struct sockaddr_in6 *LPSOCKADDR_IN6;

#define SS_PORT(ssp)  (((struct sockaddr_in*)(ssp))->sin_port)

#define IN6ADDR_ANY_INIT  { 0 }
#define IN6ADDR_LOOPBACK_INIT  { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 }

#define IFF_UP  0x00000001
#define IFF_BROADCAST  0x00000002
#define IFF_LOOPBACK  0x00000004
#define IFF_POINTTOPOINT 0x00000008
#define IFF_MULTICAST   0x00000010

#define EAI_AGAIN  WSATRY_AGAIN
#define EAI_BADFLAGS  WSAEINVAL
#define EAI_FAIL  WSANO_RECOVERY
#define EAI_FAMILY  WSAEAFNOSUPPORT
#define EAI_MEMORY  WSA_NOT_ENOUGH_MEMORY
#define EAI_NONAME  WSAHOST_NOT_FOUND
#define EAI_SERVICE  WSATYPE_NOT_FOUND
#define EAI_SOCKTYPE  WSAESOCKTNOSUPPORT
#define EAI_NODATA  EAI_NONAME

#define AI_PASSIVE  0x1
#define AI_CANONNAME  0x2
#define AI_NUMERICHOST  0x4

#ifdef  __cplusplus
extern "C" {
#endif

extern const struct in6_addr in6addr_any;
extern const struct in6_addr in6addr_loopback;

#ifdef  __cplusplus
}
#endif

#ifdef _MSC_VER
#define WS2TCPIP_INLINE __inline
#else
#define WS2TCPIP_INLINE extern inline /* GNU style */
#endif

WS2TCPIP_INLINE int IN6_ADDR_EQUAL(const struct in6_addr *a, const struct in6_addr *b)
{
    return (memcmp(a, b, sizeof(struct in6_addr)) == 0);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_UNSPECIFIED(const struct in6_addr *a)
{
    return ((a->s6_words[0] == 0) &&
            (a->s6_words[1] == 0) &&
            (a->s6_words[2] == 0) &&
            (a->s6_words[3] == 0) &&
            (a->s6_words[4] == 0) &&
            (a->s6_words[5] == 0) &&
            (a->s6_words[6] == 0) &&
            (a->s6_words[7] == 0));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_LOOPBACK(const struct in6_addr *a)
{
    return ((a->s6_words[0] == 0) &&
            (a->s6_words[1] == 0) &&
            (a->s6_words[2] == 0) &&
            (a->s6_words[3] == 0) &&
            (a->s6_words[4] == 0) &&
            (a->s6_words[5] == 0) &&
            (a->s6_words[6] == 0) &&
            (a->s6_words[7] == 0x0100));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MULTICAST(const struct in6_addr *a)
{
    return (a->s6_bytes[0] == 0xff);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_LINKLOCAL(const struct in6_addr *a)
{
    return ((a->s6_bytes[0] == 0xfe) && ((a->s6_bytes[1] & 0xc0) == 0x80));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_SITELOCAL(const struct in6_addr *a)
{
    return ((a->s6_bytes[0] == 0xfe) && ((a->s6_bytes[1] & 0xc0) == 0xc0));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_V4MAPPED(const struct in6_addr *a)
{
    return ((a->s6_words[0] == 0) &&
            (a->s6_words[1] == 0) &&
            (a->s6_words[2] == 0) &&
            (a->s6_words[3] == 0) &&
            (a->s6_words[4] == 0) &&
            (a->s6_words[5] == 0xffff));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_V4COMPAT(const struct in6_addr *a)
{
    return ((a->s6_words[0] == 0) &&
            (a->s6_words[1] == 0) &&
            (a->s6_words[2] == 0) &&
            (a->s6_words[3] == 0) &&
            (a->s6_words[4] == 0) &&
            (a->s6_words[5] == 0) &&
            !((a->s6_words[6] == 0) &&
              (a->s6_addr[14] == 0) &&
             ((a->s6_addr[15] == 0) || (a->s6_addr[15] == 1))));
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MC_NODELOCAL(const struct in6_addr *a)
{
    return IN6_IS_ADDR_MULTICAST(a) && ((a->s6_bytes[1] & 0xf) == 1);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MC_LINKLOCAL(const struct in6_addr *a)
{
    return IN6_IS_ADDR_MULTICAST(a) && ((a->s6_bytes[1] & 0xf) == 2);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MC_SITELOCAL(const struct in6_addr *a)
{
    return IN6_IS_ADDR_MULTICAST(a) && ((a->s6_bytes[1] & 0xf) == 5);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MC_ORGLOCAL(const struct in6_addr *a)
{
    return IN6_IS_ADDR_MULTICAST(a) && ((a->s6_bytes[1] & 0xf) == 8);
}

WS2TCPIP_INLINE int IN6_IS_ADDR_MC_GLOBAL(const struct in6_addr *a)
{
    return IN6_IS_ADDR_MULTICAST(a) && ((a->s6_bytes[1] & 0xf) == 0xe);
}

WS2TCPIP_INLINE int IN6ADDR_ISANY(const struct sockaddr_in6 *a)
{
    return ((a->sin6_family == AF_INET6) && IN6_IS_ADDR_UNSPECIFIED(&a->sin6_addr));
}

WS2TCPIP_INLINE int IN6ADDR_ISLOOPBACK(const struct sockaddr_in6 *a)
{
    return ((a->sin6_family == AF_INET6) && IN6_IS_ADDR_LOOPBACK(&a->sin6_addr));
}

WS2TCPIP_INLINE void IN6_SET_ADDR_UNSPECIFIED(struct in6_addr *a)
{
    memset(a->s6_bytes, 0, sizeof(struct in6_addr));
}

WS2TCPIP_INLINE void IN6_SET_ADDR_LOOPBACK(struct in6_addr *a)
{
    memset(a->s6_bytes, 0, sizeof(struct in6_addr));
    a->s6_bytes[15] = 1;
}

WS2TCPIP_INLINE void IN6ADDR_SETANY(struct sockaddr_in6 *a)
{
    a->sin6_family = AF_INET6;
    a->sin6_port = 0;
    a->sin6_flowinfo = 0;
    IN6_SET_ADDR_UNSPECIFIED(&a->sin6_addr);
    a->sin6_scope_id = 0;
}

WS2TCPIP_INLINE void IN6ADDR_SETLOOPBACK(struct sockaddr_in6 *a)
{
    a->sin6_family = AF_INET6;
    a->sin6_port = 0;
    a->sin6_flowinfo = 0;
    IN6_SET_ADDR_LOOPBACK(&a->sin6_addr);
    a->sin6_scope_id = 0;
}

typedef union sockaddr_gen{
    struct sockaddr Address;
    struct sockaddr_in  AddressIn;
    struct sockaddr_in6_old AddressIn6;
} sockaddr_gen;

typedef struct _INTERFACE_INFO {
    u_long iiFlags;
    sockaddr_gen iiAddress;
    sockaddr_gen iiBroadcastAddress;
    sockaddr_gen iiNetmask;
} INTERFACE_INFO, *LPINTERFACE_INFO;

typedef struct _INTERFACE_INFO_EX {
    u_long iiFlags;
    SOCKET_ADDRESS iiAddress;
    SOCKET_ADDRESS iiBroadcastAddress;
    SOCKET_ADDRESS iiNetmask;
} INTERFACE_INFO_EX, *LPINTERFACE_INFO_EX;

typedef struct in_pktinfo {
    IN_ADDR ipi_addr;
    UINT ipi_ifindex;
} IN_PKTINFO;

C_ASSERT(sizeof(IN_PKTINFO) == 8);

typedef struct in6_pktinfo {
    IN6_ADDR ipi6_addr;
    UINT ipi6_ifindex;
} IN6_PKTINFO;

C_ASSERT(sizeof(IN6_PKTINFO) == 20);

typedef struct addrinfo {
    int ai_flags;
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    size_t ai_addrlen;
    char *ai_canonname;
    struct sockaddr *ai_addr;
    struct addrinfo *ai_next;
} ADDRINFOA, *PADDRINFOA;

typedef struct addrinfoW {
    int ai_flags;
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    size_t ai_addrlen;
    PWSTR ai_canonname;
    struct sockaddr *ai_addr;
    struct addrinfoW *ai_next;
} ADDRINFOW, *PADDRINFOW;

#ifdef UNICODE
typedef ADDRINFOW ADDRINFOT, *PADDRINFOT;
#else
typedef ADDRINFOA ADDRINFOT, *PADDRINFOT;
#endif

typedef ADDRINFOA ADDRINFO, *LPADDRINFO;

#ifdef __cplusplus
extern "C" {
#endif

WINSOCK_API_LINKAGE int WSAAPI getaddrinfo(const char*,const char*,const struct addrinfo*,struct addrinfo**);

#if (_WIN32_WINNT >= 0x0502)
WINSOCK_API_LINKAGE int WSAAPI GetAddrInfoW(PCWSTR,PCWSTR,const ADDRINFOW*,PADDRINFOW*);
WINSOCK_API_LINKAGE void WSAAPI FreeAddrInfoW(PADDRINFOW);
WINSOCK_API_LINKAGE INT WSAAPI GetNameInfoW(const SOCKADDR*,socklen_t,PWCHAR,DWORD,PWCHAR,DWORD,INT);
#define GetAddrInfoA  getaddrinfo
#define FreeAddrInfoA  freeaddrinfo
#define GetNameInfoA  getnameinfo

#ifdef UNICODE
#define GetAddrInfo  GetAddrInfoW
#define FreeAddrInfo  FreeAddrInfoW
#define GetNameInfo  GetNameInfoW
#else
#define GetAddrInfo  GetAddrInfoA
#define FreeAddrInfo  FreeAddrInfoA
#define GetNameInfo  GetNameInfoA
#endif /* UNICODE */
#endif /* _WIN32_WINNT >= 0x0502 */

#if INCL_WINSOCK_API_TYPEDEFS
typedef int (WSAAPI *LPFN_GETADDRINFO)(const char*,const char*,const struct addrinfo*,struct addrinfo**);
typedef int (WSAAPI *LPFN_GETADDRINFOW)(PCWSTR,PCWSTR,const ADDRINFOW*,PADDRINFOW*);
typedef void (WSAAPI *LPFN_FREEADDRINFO)(struct addrinfo*);
typedef void (WSAAPI *LPFN_FREEADDRINFOW)(PADDRINFOW*);
typedef int (WSAAPI *LPFN_GETNAMEINFO)(const struct sockaddr*,socklen_t,char*,DWORD,char*,DWORD,int);
typedef INT (WSAAPI *LPFN_GETNAMEINFOW)(const SOCKADDR*,socklen_t,PWCHAR,DWORD,PWCHAR,DWORD,INT);

#define LPFN_GETADDRINFOA  LPFN_GETADDRINFO
#define LPFN_FREEADDRINFOA  LPFN_FREEADDRINFO
#define LPFN_GETNAMEINFOA  LPFN_GETNAMEINFO

#ifdef UNICODE
#define LPFN_GETADDRINFOT  LPFN_GETADDRINFOW
#define LPFN_FREEADDRINFOT  LPFN_FREEADDRINFOW
#define LPFN_GETNAMEINFOT  LPFN_GETNAMEINFOW
#else
#define LPFN_GETADDRINFOT  LPFN_GETADDRINFOA
#define LPFN_FREEADDRINFOT  LPFN_FREEADDRINFOA
#define LPFN_GETNAMEINFOT  LPFN_GETNAMEINFOA
#endif /* UNICODE */
#endif /* INCL_WINSOCK_API_TYPEDEFS */

WINSOCK_API_LINKAGE void WSAAPI freeaddrinfo(LPADDRINFO);

typedef int socklen_t;

WINSOCK_API_LINKAGE int WSAAPI getnameinfo(const struct sockaddr*,socklen_t,char*,DWORD,char*,DWORD,int);

#if INCL_WINSOCK_API_PROTOTYPES
#ifdef UNICODE
#define gai_strerror  gai_strerrorW
#else
#define gai_strerror  gai_strerrorA
#endif  /* UNICODE */

#define GAI_STRERROR_BUFFER_SIZE  1024

WS2TCPIP_INLINE char *gai_strerrorA(int ecode)
{
    DWORD dwMsgLen;
    static char buff[GAI_STRERROR_BUFFER_SIZE + 1];

    dwMsgLen = FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS|FORMAT_MESSAGE_MAX_WIDTH_MASK,
        NULL, ecode, MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT), (LPSTR)buff, GAI_STRERROR_BUFFER_SIZE, NULL);

    return buff;
}

WS2TCPIP_INLINE WCHAR *gai_strerrorW(int ecode)
{
    DWORD dwMsgLen;
    static WCHAR buff[GAI_STRERROR_BUFFER_SIZE + 1];

    dwMsgLen = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS|FORMAT_MESSAGE_MAX_WIDTH_MASK,
        NULL, ecode, MAKELANGID(LANG_NEUTRAL,SUBLANG_DEFAULT), (LPWSTR)buff, GAI_STRERROR_BUFFER_SIZE, NULL);

    return buff;
}
#endif /* INCL_WINSOCK_API_PROTOTYPES */

#ifdef __cplusplus
}
#endif

#if !defined(_WIN32_WINNT) || (_WIN32_WINNT <= 0x0500)
#include <wspiapi.h>
#endif

#endif  /* _WS2TCPIP_H_ */
