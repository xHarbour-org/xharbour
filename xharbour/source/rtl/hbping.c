/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_PING() function
 *
 * Copyright (c) 1998 - 2000 by PJ Naughter.
 * All rights reserved.
 * 2012-12-01: Ported to xHarbour by Andi Jahja <andi.jahja/AT/yahoo.co.id>
 * www - http://www.harbour-project.org http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "inet.h"
#include "hbstack.h"
#include <fcntl.h>
#include <errno.h>
#include <assert.h>

#if defined( HB_OS_WIN )

/* add parens to avoid warning */
#if defined(__BORLANDC__) && (__BORLANDC__<=0x620)
   #undef  MAKEWORD
   #define MAKEWORD(a, b)      ((WORD)(((BYTE)(((DWORD_PTR)(a)) & 0xff)) | (((WORD)((BYTE)(((DWORD_PTR)(b)) & 0xff))) << 8)))
#endif

#define MIN_ICMP_PACKET_SIZE  8     /* minimum 8 byte icmp packet (just header) */
#define MAX_ICMP_PACKET_SIZE  1024  /* Maximum icmp packet size */

/* IP header */
#pragma pack(push, 1) /* The IP_HEADER and ICMP_HEADER should be alligned on 1 byte boundaries */
typedef struct tagIP_HEADER
{
   unsigned int h_len:4;                 /* length of the header */
   unsigned int version:4;               /* Version of IP */
   unsigned char tos;                  /* Type of service */
   unsigned short total_len;           /* total length of the packet */
   unsigned short ident;               /* unique identifier */
   unsigned short frag_and_flags;      /* flags */
   unsigned char ttl;
   unsigned char proto;                /* protocol (TCP, UDP etc) */
   unsigned short checksum;            /* IP checksum */
   unsigned int sourceIP;
   unsigned int destIP;
} IP_HEADER, * LPIP_HEADER;

typedef struct _CPINGREPLY
{
   IN_ADDR       Address;  /* The IP address of the replier */
   unsigned long RTT;      /* Round Trip time in Milliseconds */
} CPINGREPLY, *LPCPINGREPLY;

/* ICMP header */
typedef struct tagICMP_HEADER
{
   BYTE   i_type;
   BYTE   i_code; /* type sub code */
   USHORT i_cksum;
   USHORT i_id;
   USHORT i_seq;
   /* This is not the std header, but we reserve space for time */
   ULONG  timestamp;
} ICMP_HEADER, * LPICMP_HEADER;

#pragma pack(pop)

static BOOL     sm_bAttemptedWinsock2Initialise = FALSE;
static BOOL     sm_bWinsock2OK                  = FALSE;
static __int64  sm_TimerFrequency               = 0;
static char     szResponse[ 1024 ]              = { '\0' };

static BOOL IsSocketReadible( SOCKET socket, DWORD dwTimeout, BOOL *bReadible )
{
   TIMEVAL  timeout; /* = { dwTimeout / 1000, dwTimeout % 1000 }; */
   fd_set   fds;
   int nStatus ;

   FD_ZERO( &fds );
   FD_SET( socket, &fds );

   timeout.tv_sec  = dwTimeout / 1000;
   timeout.tv_usec = dwTimeout % 1000;

   nStatus = select( 0, &fds, NULL, NULL, &timeout );

   if( nStatus == SOCKET_ERROR )
      return FALSE;
   else
   {
      *bReadible = ! ( nStatus == 0 );
      return TRUE;
   }
}

/* Decode the raw Ip packet we get back */
static BOOL DecodeResponse( char * pBuf, int nBytes, SOCKADDR_IN * from )
{
   LARGE_INTEGER TimerTick;
   LPIP_HEADER   pIpHdr      = ( LPIP_HEADER ) pBuf;
   int           nIpHdrlen;

   nIpHdrlen       = pIpHdr->h_len * 4; /* Number of 32-bit words*4 = bytes */

   /* Get the current tick count */
   QueryPerformanceCounter( &TimerTick );

   /* Not enough data recieved */
   if( nBytes < nIpHdrlen + MIN_ICMP_PACKET_SIZE )
   {
      hb_snprintf( szResponse, 1024, "Received too few bytes from %s", inet_ntoa( from->sin_addr ) );
      SetLastError( ERROR_UNEXP_NET_ERR );
      return FALSE;
   }
   else
   {
      /* Check it is an ICMP_ECHOREPLY packet */
      LPICMP_HEADER pIcmpHdr = ( LPICMP_HEADER ) ( pBuf + nIpHdrlen );

      if( pIcmpHdr->i_type != 0 )     //type ICMP_ECHOREPLY is 0
      {
         hb_snprintf( szResponse, 1024, "Non-echo type %d recvd", pIcmpHdr->i_type );
         SetLastError( ERROR_UNEXP_NET_ERR );
         return FALSE;
      }

      /* Check it is the same id as we sent */
      if( pIcmpHdr->i_id != ( USHORT ) GetCurrentProcessId() )
      {
         hb_snprintf( szResponse, 1024, "Received someone else's packet!" );
         SetLastError( ERROR_UNEXP_NET_ERR );
         return FALSE;
      }
   } /* nBytes < nIpHdrlen + MIN_ICMP_PACKET_SIZE */

   return TRUE;
}

/* generate an IP checksum based on a given data buffer */
static USHORT GenerateIPChecksum( USHORT * pBuffer, int nSize )
{
   unsigned long cksum = 0;

   while( nSize > 1 )
   {
      cksum += *pBuffer++;
      nSize -= sizeof( USHORT );
   }

   if( nSize )
      cksum += *( UCHAR * ) pBuffer;

   cksum = ( cksum >> 16 ) + ( cksum & 0xffff );
   cksum += ( cksum >> 16 );
   return ( USHORT ) ( ~cksum );
}

/* Fill up the ICMP packet with defined values */
static void FillIcmpData( LPICMP_HEADER pIcmp, int nData )
{
   int      nHdrSize;
   char *   pData;

   pIcmp->i_type     = 8; /* ICMP_ECHO type */
   pIcmp->i_code     = 0;
   pIcmp->i_id       = ( USHORT ) GetCurrentProcessId();
   pIcmp->i_seq      = 0;
   pIcmp->i_cksum    = 0;
   pIcmp->timestamp  = GetTickCount();

   /* Set up the data which will be sent */
   nHdrSize = sizeof( ICMP_HEADER );
   pData    = ( ( char * ) pIcmp ) + nHdrSize;

   memset( pData, 'E', nData - nHdrSize );

   /* Generate the checksum */
   pIcmp->i_cksum = GenerateIPChecksum( ( USHORT * ) pIcmp, nData );
}

static BOOL hbping_Initialise( void )
{
   if( ! sm_bAttemptedWinsock2Initialise )
   {
      WSADATA wsa;
      LARGE_INTEGER Frequency;

      sm_bAttemptedWinsock2Initialise = TRUE;

      /* Initialise the winsock 2 stack */
      sm_bWinsock2OK = ( WSAStartup( MAKEWORD( 2, 1 ), &wsa ) == 0 );

      /* Use the High performace counter to get an accurate RTT */
      Frequency.QuadPart   = 0;
      sm_bWinsock2OK       = sm_bWinsock2OK && QueryPerformanceFrequency( &Frequency );

      if( sm_bWinsock2OK )
         sm_TimerFrequency = Frequency.QuadPart;
   }

   return sm_bWinsock2OK;
}

BOOL hbping_Ping( LPCTSTR pszHostName, CPINGREPLY *pr, DWORD dwTimeout, UINT nPacketSize )
{
   /* Parameter validation */
   if( nPacketSize > MAX_ICMP_PACKET_SIZE || nPacketSize < MIN_ICMP_PACKET_SIZE )
   {
      assert( FALSE );
      SetLastError( WSAENOBUFS );
      return FALSE;
   }

   /* Make sure everything is initialised */
   if( hbping_Initialise() )
   {
      /* Resolve the address of the host to connect to */
      SOCKADDR_IN    dest;
      LPSTR          lpszAscii   = ( LPTSTR ) pszHostName;
      unsigned long  addr        = inet_addr( lpszAscii );
      SOCKET         sockRaw;

      memset( &dest, 0, sizeof( dest ) );

      if( addr == INADDR_NONE )
      {
         /* Not a dotted address, then do a lookup of the name */
         HOSTENT * hp = gethostbyname( lpszAscii );

         if( hp )
         {
            memcpy( &( dest.sin_addr ), hp->h_addr, hp->h_length );
            dest.sin_family = hp->h_addrtype;
         }
         else
         {
            hb_snprintf( szResponse, 1024, "Could not resolve the host name %s", pszHostName );
            return FALSE;
         }
      }
      else
      {
         dest.sin_addr.s_addr = addr;
         dest.sin_family      = AF_INET;
      }

      /* Create the raw socket */
      sockRaw = WSASocket( AF_INET, SOCK_RAW, IPPROTO_ICMP, NULL, 0, 0 );

      if( sockRaw == INVALID_SOCKET )
      {
         hb_snprintf( szResponse, 1024, "Failed to create a raw socket" );
         return FALSE;
      }
      else
      {
         int            nBufSize = nPacketSize + sizeof( ICMP_HEADER );
         char *         pICMP    = ( char* ) hb_xgrab( nBufSize );
         LARGE_INTEGER  TimerTick;
         __int64        nStartTick;
         int            nWrote;

         /* Allocate the ICMP packet */
         FillIcmpData( ( LPICMP_HEADER ) pICMP, nBufSize );

         /* Get the tick count prior to sending the packet */
         QueryPerformanceCounter( &TimerTick );

         nStartTick  = TimerTick.QuadPart;

         /* Send of the packet */
         nWrote      = sendto( sockRaw, pICMP, nBufSize, 0, ( SOCKADDR * ) &dest, sizeof( dest ) );

         if( nWrote == SOCKET_ERROR )
         {
            DWORD dwError = GetLastError();

            hb_snprintf( szResponse, 1024, "sendto failed" );

            hb_xfree( pICMP );

            closesocket( sockRaw );
            SetLastError( dwError );

            return FALSE;
         }
         else
         {
            /* allocate the recv buffer */
            char *      pRecvBuf = ( char * ) hb_xgrab( MAX_ICMP_PACKET_SIZE );
            BOOL        bReadable;
            SOCKADDR_IN from;
            int         nRead, nFromlen = sizeof( from );

            /* Allow the specified timeout */
            if( IsSocketReadible( sockRaw, dwTimeout, &bReadable ) )
            {
               if( bReadable )
                  /* Receive the response */
                  nRead = recvfrom( sockRaw, pRecvBuf, MAX_ICMP_PACKET_SIZE, 0, ( SOCKADDR * ) &from, &nFromlen );
               else
               {
                  hb_snprintf( szResponse, 1024, "Timeout occured while awaiting recvfrom" );
                  closesocket( sockRaw );

                  hb_xfree( pICMP );
                  hb_xfree( pRecvBuf );

                  /* set the error to timed out */
                  SetLastError( WSAETIMEDOUT );

                  return FALSE;
               }
            }
            else
            {
               DWORD dwError = GetLastError();

               hb_snprintf( szResponse, 1024, "IsReadible call failed" );
               hb_xfree( pICMP );
               hb_xfree( pRecvBuf );

               closesocket( sockRaw );
               SetLastError( dwError );

               return FALSE;
            }

            /* Get the current tick count */
            QueryPerformanceCounter( &TimerTick );

            /* Now check the return response from recvfrom */
            if( nRead == SOCKET_ERROR )
            {
               DWORD dwError = GetLastError();

               hb_snprintf( szResponse, 1024, "recvfrom call failed" );

               hb_xfree( pICMP );
               hb_xfree( pRecvBuf );

               closesocket( sockRaw );
               SetLastError( dwError );

               return FALSE;
            }
            else
            {
               /* Decode the response we got back */
               BOOL bSuccess = DecodeResponse( pRecvBuf, nRead, &from );

               /* If we successfully decoded the response, then return the
                * values in the CPINGREPLY instance
                */
               if( bSuccess )
               {
                  pr->Address  = from.sin_addr;
                  pr->RTT      = ( ULONG ) ( ( TimerTick.QuadPart - nStartTick ) * 1000 / sm_TimerFrequency );
               }

               /* Don't forget to release out socket */
               closesocket( sockRaw );

               /* Free up the memory we allocated */
               hb_xfree( pICMP );
               hb_xfree( pRecvBuf );

               /* return the status */
               return bSuccess;
            } /* nRead == SOCKET_ERROR */
         } /* nWrote == SOCKET_ERROR */
      } /* sockRaw == INVALID_SOCKET */
   } /* hbping_Initialise2() */

   return FALSE;
}

HB_FUNC( HB_PING )
{
   if ( ISCHAR( 1 ) )
   {
      LPCTSTR    pszHostName = hb_parc( 1 );
      CPINGREPLY pr;
      DWORD      dwTimeout   = 5000;
      UINT       nPacketSize = 32;
      BOOL       bSuccess;
      PHB_ITEM pRef  = hb_param( 2, HB_IT_BYREF );

      *szResponse = 0;
      bSuccess    = hbping_Ping( pszHostName, &pr, dwTimeout, nPacketSize );

      if ( bSuccess )
      {
         HOSTENT* phostent = gethostbyaddr((char *)&pr.Address.S_un.S_addr, 4, PF_INET);
         if( phostent )
            hb_snprintf( szResponse, 1024,"%d.%d.%d.%d [%s], replied in RTT:%dms\n",
               pr.Address.S_un.S_un_b.s_b1, pr.Address.S_un.S_un_b.s_b2, pr.Address.S_un.S_un_b.s_b3,
               pr.Address.S_un.S_un_b.s_b4, phostent->h_name, ( int ) pr.RTT);
      }

      if( pRef )
         hb_itemPutC( pRef, szResponse );

      hb_retl( bSuccess );
      WSACleanup();
      return;
   }

   hb_retl( FALSE );
}

#else

HB_FUNC( HB_PING )
{
   hb_retl( FALSE );
}

#endif
