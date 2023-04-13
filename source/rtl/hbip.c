/*  $Id$  */
/*
 * xHarbour Project source code:
 *    The internet protocol / TCP support
 *
 * Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
 *                Ron Pinkas [Ron@RonPinkas.com]
 *                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
 * www - http://www.xharbour.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    updated and ported to Harbour
 * www - http://www.harbour-project.org
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 * updated for Leto db server
 *
 * Copyright 2009 Miguel Angel Marchuet Frutos <soporte-2@dsgsoftware.com>
 * updated for remote file server
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
#include "hbapierr.h"

/* Compile in Unix mode under Cygwin */
//#ifdef OS_UNIX_COMPATIBLE
//  #undef HB_OS_WIN
//#endif

/* HB_INET_H_ */
#include <string.h>

#ifndef HB_ERR_FUNCNAME
   #define HB_ERR_FUNCNAME &hb_errFuncName
#endif

#if defined( HB_OS_WIN )
//    #define _WINSOCKAPI_    /* Prevents inclusion of Winsock.h in Windows.h */
   #define HB_SOCKET_T     SOCKET
   #  define socklen_t int
   #include <winsock2.h>
   #include <windows.h>

   /* add parens to avoid warning */
   #if defined(__BORLANDC__) && (__BORLANDC__<=0x620)
      #undef  MAKEWORD
      #define MAKEWORD(a, b)      ((WORD)(((BYTE)(((DWORD_PTR)(a)) & 0xff)) | (((WORD)((BYTE)(((DWORD_PTR)(b)) & 0xff))) << 8)))
   #endif
   #define HB_IP_CLOSE( x ) closesocket( x )
#else

   #define HB_SOCKET_T     int
   #include <unistd.h>
   #include <sys/types.h>
   #include <sys/socket.h>
   #include <netdb.h>
   #include <netinet/in.h>
   #include <arpa/inet.h>

   #if defined( __WATCOMC__ )
      #define h_errno errno
   #else
extern int h_errno;
   #endif
   #define HB_IP_CLOSE( x ) close( x )
   #include <errno.h>
#endif

#define HB_SOCKET_ZERO_ERROR()   \
   do { errorCode = 0; errorDesc = ""; } while( 0 )

#if defined( HB_OS_WIN )
    #define HB_SOCKET_SET_ERROR()   \
   do { \
      errorCode   = WSAGetLastError(); \
      errorDesc   = hb_strerror( errorCode ); \
      WSASetLastError( 0 ); \
   } while( 0 )

#else
    #define HB_SOCKET_SET_ERROR()      \
   do { errorCode = errno; errorDesc = hb_strerror( errno ); } while( 0 )
#endif

#define HB_SOCKET_SET_ERROR1( code )   \
   do { errorCode = code; errorDesc = hb_strerror( code ); } while( 0 )
#define HB_SOCKET_SET_ERROR2( code, desc )   \
   do { errorCode = code; errorDesc = desc; } while( 0 )

#ifndef MSG_NOSIGNAL
   #define MSG_NOSIGNAL 0
#endif
#ifndef MSG_DONTWAIT
   #define MSG_DONTWAIT 0
#endif
#ifndef MSG_WAITALL
   #define MSG_WAITALL  0
#endif

#if ! defined( HB_OS_WIN_CE )
   #include <fcntl.h>
   #include <errno.h>
#endif

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE ) || defined( HB_OS_BSD ) || defined( HB_OS_OS2 )
   #include <sys/time.h>
#endif

#ifdef HB_OS_LINUX
#include <signal.h>
#define HB_IP_LINUX_INTERRUPT SIGUSR1 + 90
static void hb_ipLinuxSigusrHandle( int sig )
{
   /* nothing to do */
   HB_SYMBOL_UNUSED( sig );
}
#endif

#if defined( _MSC_VER )
   #define SOCKOPT4  char *
#else
   #define SOCKOPT4  void *
#endif

#define BUFFER_SIZE  65536 //8192

static volatile int  s_iSessions = 0;
static char *        errorDesc;
static int           errorCode;
static fd_set        rd_fds, active_fds;
static unsigned int  rd_maxfd;

int hb_iperrorcode( void )
{
   return errorCode;
}

void hb_ipInit( void )
{
   if( s_iSessions )
   {
      s_iSessions++;
   }
   else
   {
      #if defined( HB_OS_WIN )
      WSADATA wsadata;
      WSAStartup( MAKEWORD( 1, 1 ), &wsadata );
      #elif defined( HB_OS_LINUX )
      signal( HB_IP_LINUX_INTERRUPT, hb_ipLinuxSigusrHandle );
      #endif
      s_iSessions = 1;
   }
}

void hb_ipCleanup( void )
{
   if( --s_iSessions == 0 )
   {
      #if defined( HB_OS_WIN )
      WSACleanup();
      #endif
   }
}

void hb_ipSetBufSize( HB_SOCKET_T hSocket, int iBufSend, int iBufRecv )
{
   int   value;
   socklen_t len = sizeof( value );

   if( iBufSend && ! getsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_SNDBUF, ( char * ) ( SOCKOPT4 ) &value, ( socklen_t * ) &len ) )
   {
      if( value < iBufSend )
      {
         value = iBufSend;
         setsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_SNDBUF, ( char const * ) ( SOCKOPT4 ) &value, ( socklen_t ) sizeof( value ) );
      }
   }

   if( iBufRecv && ! getsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_RCVBUF, ( char * ) ( SOCKOPT4 ) &value, ( socklen_t * ) &len ) )
   {
      if( value < iBufRecv )
      {
         value = iBufRecv;
         setsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_RCVBUF, ( char const * ) ( SOCKOPT4 ) &value, ( socklen_t ) sizeof( value ) );
      }
   }
   // getsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_RCVBUF, ( char * ) ( SOCKOPT4 ) &value, ( int * ) len );
   getsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_RCVBUF, ( char * ) ( SOCKOPT4 ) &value, &len );
}

int hb_ipDataReady( HB_SOCKET_T hSocket, int timeout )
{
   fd_set         set;
   struct timeval tv;

   HB_SOCKET_ZERO_ERROR();

   FD_ZERO( &set );
   FD_SET( hSocket, &set );

   if( timeout == -1 )
   {
      if( select( ( int ) hSocket + 1, &set, NULL, NULL, NULL ) < 0 )
      {
         HB_SOCKET_SET_ERROR();
         return 0;
      }
   }
   else
   {
      tv.tv_sec   = timeout / 1000;
      tv.tv_usec  = ( timeout % 1000 ) * 1000;
      if( select( ( int ) hSocket + 1, &set, NULL, NULL, &tv ) < 0 )
      {
         HB_SOCKET_SET_ERROR();
         return 0;
      }
   }

   return FD_ISSET( hSocket, &set );
}

static int hb_selectWriteSocket( HB_SOCKET_T hSocket, int timeout )
{
   fd_set         set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET( hSocket, &set );

   if( timeout == -1 )
   {
      if( select( ( int ) hSocket + 1, NULL, &set, NULL, NULL ) < 0 )
         return 0;
   }
   else
   {
      tv.tv_sec   = timeout / 1000;
      tv.tv_usec  = ( timeout % 1000 ) * 1000;
      if( select( ( int ) hSocket + 1, NULL, &set, NULL, &tv ) < 0 )
         return 0;
   }

   return FD_ISSET( hSocket, &set );
}

#if defined( HB_OS_WIN )
static int hb_selectWriteExceptSocket( HB_SOCKET_T hSocket, int timeout )
{
   fd_set         set, eset;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET( hSocket, &set );
   FD_ZERO( &eset );
   FD_SET( hSocket, &eset );

   if( timeout == -1 )
   {
      if( select( ( int ) hSocket + 1, NULL, &set, &eset, NULL ) < 0 )
         return 2;
   }
   else
   {
      tv.tv_sec   = timeout / 1000;
      tv.tv_usec  = ( timeout % 1000 ) * 1000;
      if( select( ( int ) hSocket + 1, NULL, &set, &eset, &tv ) < 0 )
         return 2;
   }

   if( FD_ISSET( hSocket, &eset ) )
   {
      return 2;
   }

   if( FD_ISSET( hSocket, &set ) )
   {
      return 1;
   }
   return 0;
}
#endif

static ULONG hb_getAddr( const char * name )
{
   ULONG ulAddr = inet_addr( name );

   if( ulAddr == INADDR_NONE )
   {
      struct hostent * Host = gethostbyname( name );

      if( Host )
         return *( UINT * ) Host->h_addr_list[ 0 ];
      else
      {
#if defined( HB_OS_WIN )
         HB_SOCKET_SET_ERROR2( WSAGetLastError(), "Generic error in GetHostByName()" );
         WSASetLastError( 0 );
#elif defined( HB_OS_OS2 ) || defined( HB_OS_HPUX ) || defined( __WATCOMC__ )
         HB_SOCKET_SET_ERROR2( h_errno, "Generic error in GetHostByName()" );
#else
         HB_SOCKET_SET_ERROR2( h_errno, ( char * ) hstrerror( h_errno ) );
#endif
         return INADDR_NONE;
      }

   }

   return ulAddr;
}

static void hb_socketSetNonBlocking( HB_SOCKET_T hSocket )
{
#ifdef HB_OS_WIN
   ULONG mode = 1;
   ioctlsocket( hSocket, FIONBIO, &mode );

#else
   int flags = fcntl( hSocket, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags |= O_NONBLOCK;
      fcntl( hSocket, F_SETFL, ( LONG ) flags );
   }
#endif
}

static void hb_socketSetBlocking( HB_SOCKET_T hSocket )
{
#ifdef HB_OS_WIN
   ULONG mode  = 0;
   ioctlsocket( hSocket, FIONBIO, &mode );
#else
   int   flags = fcntl( hSocket, F_GETFL, 0 );
   if( flags != -1 )
   {
      flags &= ~O_NONBLOCK;
      fcntl( hSocket, F_SETFL, ( long ) flags );
   }
#endif
}

int hb_socketConnect_( HB_SOCKET_T hSocket, struct sockaddr_in * remote, int timeout )
{
   int         iErr1;

   #if ! defined( HB_OS_WIN )
   int         iErrval;
   socklen_t   iErrvalLen;
   #endif
   int         iOpt = 1;

   setsockopt( hSocket, SOL_SOCKET, SO_KEEPALIVE, ( const char * ) &iOpt, sizeof( iOpt ) );

   /* we'll be using a nonblocking function */
   hb_socketSetNonBlocking( hSocket );

   iErr1 = connect( hSocket, ( struct sockaddr * ) remote, sizeof( *remote ) );
   if( iErr1 != 0 )
   {
#if defined( HB_OS_WIN )
      if( WSAGetLastError() != WSAEWOULDBLOCK )
#else
      if( errno != EINPROGRESS )
#endif
      {
         HB_SOCKET_SET_ERROR();
      }
      else
      {
         /* Now we wait for socket connection or timeout */
#if defined( HB_OS_WIN )
         iErr1 = hb_selectWriteExceptSocket( hSocket, timeout );
         if( iErr1 == 2 )
         {
            HB_SOCKET_SET_ERROR2( 2, "Connection failed" );
         }
         else if( iErr1 == 1 )
         {
            /* success */
         }
#else
         if( hb_selectWriteSocket( hSocket, timeout ) )
         {
            /* Connection has been completed with a failure or a success */
            iErrvalLen  = sizeof( iErrval );
            iErr1       = getsockopt( hSocket, SOL_SOCKET, SO_ERROR,
                                      ( SOCKOPT4 ) &iErrval, &iErrvalLen );

            if( iErr1 )
            {
               HB_SOCKET_SET_ERROR1( iErr1 );
            }
            else if( iErrval )
            {
               HB_SOCKET_SET_ERROR1( iErrval );
            }
            /* Success! */
         }
#endif
         /* Timed out */
         else
         {
            HB_SOCKET_SET_ERROR2( -1, "Timeout" );
         }
      }
   }

   hb_socketSetBlocking( hSocket );

   return errorCode == 0;
}

int hb_ipRecv( HB_SOCKET_T hSocket, char * szBuffer, int iBufferLen )
{
   int iLen;

   HB_SOCKET_ZERO_ERROR();

   iLen = recv( hSocket, szBuffer, iBufferLen, MSG_NOSIGNAL );

   if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( -2, "Connection closed" );
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR();
   }

   return iLen;
}


int hb_ipSend( HB_SOCKET_T hSocket, char * szBuffer, int iSend, int timeout )
{
   int   iSent, iBufferLen;
   int   iBufferMax;
   socklen_t   iLen = sizeof( iBufferMax );

   getsockopt( ( unsigned ) hSocket, ( int ) SOL_SOCKET, ( int ) SO_SNDBUF, ( char * ) ( SOCKOPT4 ) &iBufferMax, ( socklen_t * ) &iLen );

   iSent = iLen = 0;

   HB_SOCKET_ZERO_ERROR();

   while( iSent < iSend )
   {
      iBufferLen = ( iBufferMax > iSend - iSent ) ? iSend - iSent : iBufferMax;

      if( hb_selectWriteSocket( hSocket, timeout ) )
         iLen = send( hSocket, szBuffer + iSent, iBufferLen, MSG_NOSIGNAL );
      else
         iLen = 0;

      if( iLen <= 0 )
      {
         if( iLen == 0 )
         {
            HB_SOCKET_SET_ERROR2( -1, "Timeout" );
         }
         else
         {
            HB_SOCKET_SET_ERROR();
         }
         break;
      }

      iSent += iLen;
   }
   if( iLen > 0 )
   {
      return iSent;
   }
   else
   {
      return -1;
   }
}

HB_SOCKET_T hb_ipConnect( const char * szHost, int iPort, int timeout )
{
   HB_SOCKET_T          hSocket = ( HB_SOCKET_T ) -1;
   ULONG                ulAddr;
   struct sockaddr_in   remote;

   HB_SOCKET_ZERO_ERROR();

   ulAddr = hb_getAddr( szHost );

   /* error had been set by get hosts */
   if( ulAddr != INADDR_NONE )
   {
      /* Creates comm socket */
#if defined( HB_OS_WIN )
      hSocket  = socket( AF_INET, SOCK_STREAM, 0 );
#else
      hSocket  = socket( PF_INET, SOCK_STREAM, 0 );
#endif

      if( hSocket == ( HB_SOCKET_T ) -1 )
      {
         HB_SOCKET_SET_ERROR();
      }
      else
      {
         remote.sin_family       = AF_INET;
         remote.sin_port         = ( USHORT ) iPort;
         remote.sin_addr.s_addr  = ulAddr;


         /* Set internal socket send buffer to 64k,
          * this should fix the speed problems some users have reported
          */
         hb_ipSetBufSize( hSocket, BUFFER_SIZE, BUFFER_SIZE );

         if( ! hb_socketConnect_( hSocket, &remote, timeout ) )
            hSocket = ( HB_SOCKET_T ) -1;
      }
   }
   return hSocket;
}

HB_SOCKET_T hb_ipServer( int iPort, const char * szAddress, int iListen )
{
   HB_SOCKET_T          hSocket;
   int                  iOpt = 1;
   struct sockaddr_in   remote;

   HB_SOCKET_ZERO_ERROR();

   /* Creates comm socket */
#if defined( HB_OS_WIN )
   hSocket  = socket( AF_INET, SOCK_STREAM, 0 );
#else
   hSocket  = socket( PF_INET, SOCK_STREAM, 0 );
#endif

   if( hSocket == ( HB_SOCKET_T ) -1 )
   {
      HB_SOCKET_SET_ERROR();
   }

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( hSocket, SOL_SOCKET, SO_REUSEADDR, ( const char * ) &iOpt, sizeof( iOpt ) );

   remote.sin_family       = AF_INET;
   remote.sin_port         = htons( ( u_short ) iPort );

   remote.sin_addr.s_addr  = szAddress ? inet_addr( szAddress ) : INADDR_ANY;

   if( bind( hSocket, ( struct sockaddr * ) &remote, sizeof( remote ) ) )
   {
      HB_SOCKET_SET_ERROR();
      HB_IP_CLOSE( hSocket );
      return ( HB_SOCKET_T ) -1;
   }
   else if( listen( hSocket, iListen ) )
   {
      HB_SOCKET_SET_ERROR();
      HB_IP_CLOSE( hSocket );
      return ( HB_SOCKET_T ) -1;
   }
   else
   {
      return hSocket;
   }
}

HB_SOCKET_T hb_ipAccept( HB_SOCKET_T hSocket, int timeout, char * szAddr, long int * lPort )
{
#if ! defined( EAGAIN )
#define EAGAIN -1
#endif
   HB_SOCKET_T          incoming = 0;
   int                  iError   = EAGAIN;
   struct sockaddr_in   si_remote;

#if defined( _XOPEN_SOURCE_EXTENDED )
   socklen_t            Len;
#elif defined( HB_OS_WIN )
   int                  Len;
#else
   unsigned int         Len;
#endif

   si_remote.sin_port = 0;
   Len = sizeof( struct sockaddr_in );

   /*
    * Accept can (and should) be asynchronously stopped by closing the
    * accepting socket. this will make the wait to terminate, and the
    * calling program will be notified through the status of the
    * returned socket.
    */

   HB_SOCKET_ZERO_ERROR();

   /* Connection incoming */
   while( iError == EAGAIN )
   {
      if( hb_ipDataReady( hSocket, timeout ) )
      {
         /* On error (e.g. async connection closed) , com will be -1 and
            errno == 22 (invalid argument ) */
         incoming = accept( hSocket, ( struct sockaddr * ) &si_remote, &Len );
         if( incoming == ( HB_SOCKET_T ) -1 )
         {
#if defined( HB_OS_WIN )
            iError   = WSAGetLastError();
#else
            iError   = errno;
#endif
         }
         else
            iError = 0;
      }
      /* Timeout expired */
      else
         iError = -1;
   }

   if( iError == -1 )
   {
      HB_SOCKET_SET_ERROR2( -1, "Timeout" );
      return ( HB_SOCKET_T ) -1;
   }
   else if( iError > 0 )
   {
      HB_SOCKET_SET_ERROR1( iError );
      return ( HB_SOCKET_T ) -1;
   }
   else
   {
      char *   ptr   = inet_ntoa( si_remote.sin_addr );
      int      iOpt  = 1;
      HB_MEMCPY( szAddr, ptr, strlen( ptr ) );
      szAddr[ strlen( ptr ) ] = '\0';
      *lPort                  = ntohs( si_remote.sin_port );
      setsockopt( incoming, SOL_SOCKET, SO_KEEPALIVE, ( const char * ) &iOpt, sizeof( iOpt ) );

      /* Set internal socket send buffer to 64k,
       * this should fix the speed problems some users have reported
       */
      hb_ipSetBufSize( hSocket, BUFFER_SIZE, BUFFER_SIZE );
      return incoming;
   }
}

void hb_ipclose( HB_SOCKET_T hSocket )
{

   #if defined( HB_OS_WIN )
   shutdown( hSocket, SD_BOTH );
   #elif defined( HB_OS_OS2 )
   shutdown( hSocket, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
   #elif ! defined( __WATCOMC__ )
   shutdown( hSocket, SHUT_RDWR );
   #endif

   hb_retni( HB_IP_CLOSE( hSocket ) );

   #ifdef HB_OS_LINUX
   kill( 0, HB_IP_LINUX_INTERRUPT );
   #endif
}

HB_FUNC( HB_IPRECV )
{
   HB_SOCKET_T hSocket  = ( HB_SOCKET_T ) hb_parnl( 1 );
   PHB_ITEM    pBuffer  = hb_param( 2, HB_IT_STRING );

   if( ! hSocket || pBuffer == NULL || ! ISBYREF( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   pBuffer = hb_itemUnShare( pBuffer );
   hb_retni( hb_ipRecv( hSocket, hb_itemGetCPtr( pBuffer ), ( int ) hb_itemGetCLen( pBuffer ) ) );
}

HB_FUNC( HB_IPSEND )
{
   HB_SOCKET_T hSocket  = ( HB_SOCKET_T ) hb_parnl( 1 );
   PHB_ITEM    pBuffer  = hb_param( 2, HB_IT_STRING );
   int         timeout  = ( ISNIL( 3 ) ) ? -1 : hb_parni( 3 );

   if( ! hSocket || ! pBuffer )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hb_retni( hb_ipSend( hSocket, hb_itemGetCPtr( pBuffer ), ( int ) hb_itemGetCLen( pBuffer ), timeout ) );
}

HB_FUNC( HB_IPSERVER )
{
   HB_SOCKET_T    hSocket;
   const char *   szAddress;
   int            iPort;
   int            iListen;

   /* Parameter error checking */
   if( ! ISNUM( 1 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   iPort       = hb_parni( 1 );
   szAddress   = ( ISNIL( 2 ) ) ? NULL :  hb_parc( 2 );
   iListen     = ISNUM( 3 ) ? hb_parni( 3 ) : 10;

   hSocket     = hb_ipServer( iPort, szAddress, iListen );
   if( hSocket != ( HB_SOCKET_T ) -1 )
      hb_retnl( ( long ) hSocket );
   else
      hb_ret();
}

HB_FUNC( HB_IPACCEPT )
{
   HB_SOCKET_T hSocket  = ( HB_SOCKET_T ) hb_parnl( 1 );
   HB_SOCKET_T incoming;
   int         timeout  = ( ISNIL( 2 ) ) ? -1 : hb_parni( 2 );
   long int    lPort;
   char        szAddr[ 18 ];

   if( ! hSocket )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   incoming = hb_ipAccept( hSocket, timeout, szAddr, &lPort );
   if( incoming == ( HB_SOCKET_T ) -1 )
      hb_ret();
   else
   {
      PHB_ITEM temp, aInfo = hb_itemArrayNew( 3 );

      temp = hb_itemPutNL( NULL, ( const LONG ) incoming );
      hb_itemArrayPut( aInfo, 1, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutC( NULL, szAddr );
      hb_itemArrayPut( aInfo, 2, temp );
      hb_itemRelease( temp );

      temp = hb_itemPutNL( NULL, lPort );
      hb_itemArrayPut( aInfo, 3, temp );
      hb_itemRelease( temp );

      hb_itemReturn( aInfo );
      hb_itemRelease( aInfo );
   }
}

HB_FUNC( HB_IPCONNECT )
{
   HB_SOCKET_T hSocket;

   if( ! ISCHAR( 1 ) || ! ISNUM( 2 ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   hSocket = hb_ipConnect( hb_parc( 1 ), htons( ( u_short ) hb_parni( 2 ) ), ( ISNIL( 3 ) ) ? -1 : hb_parni( 3 ) );

   hb_retnl( ( long ) hSocket );
}

HB_FUNC( HB_IPDATAREADY )
{
   HB_SOCKET_T hSocket = ( HB_SOCKET_T ) hb_parnl( 1 );

   if( ! hSocket || ( hb_pcount() == 2 && ! ISNUM( 2 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }

   if( hb_ipDataReady( hSocket, ( ISNIL( 2 ) ) ? 0 : hb_parni( 2 ) ) )
      hb_retni( 1 );
   else
      hb_retni( 0 );
}

HB_FUNC( HB_IPERRORCODE )
{
   hb_retni( errorCode );
}

HB_FUNC( HB_IPERRORDESC )
{
   hb_retc( errorDesc );
}

char * hb_ipErrorDesc( void )
{
   return errorDesc;
}

HB_FUNC( HB_IPCLOSE )
{
   HB_SOCKET_T hSocket = ( HB_SOCKET_T ) hb_parnl( 1 );

   if( ! hSocket )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   else
      hb_ipclose( hSocket );
}

HB_FUNC( HB_IPINIT )
{
   hb_ipInit();
}

HB_FUNC( HB_IPCLEANUP )
{
   hb_ipCleanup();
}

BOOL hb_ip_rfd_isset( HB_SOCKET_T hSocket )
{
   return FD_ISSET( hSocket, &active_fds );
}

void hb_ip_rfd_set( HB_SOCKET_T hSocket )
{
   if( ! hSocket )
      return;

   HB_SOCKET_ZERO_ERROR();

   FD_SET( hSocket, &rd_fds );
   rd_maxfd = ( rd_maxfd > ( unsigned int )hSocket ) ? rd_maxfd : ( unsigned int ) hSocket;
}

void hb_ip_rfd_clr( HB_SOCKET_T hSocket )
{
   if( ! hSocket )
      return;
   FD_CLR( hSocket, &rd_fds );
}

void hb_ip_rfd_zero( void )
{
   FD_ZERO( &rd_fds );
   rd_maxfd = 0;
}

int hb_ip_rfd_select( int iTimeOut )
{
   struct timeval tv = { 0, 0 };

   if( iTimeOut != -1 )
   {
      tv.tv_sec   = iTimeOut / 1000;
      tv.tv_usec  = ( iTimeOut % 1000 ) * 1000;
   }
   active_fds  = rd_fds;

   iTimeOut    = select( rd_maxfd + 1, &active_fds, NULL, NULL, &tv );

   return iTimeOut;
}

HB_FUNC( HB_IP_RFD_SET )
{
   HB_SOCKET_T hSocket = ( HB_SOCKET_T ) hb_parnl( 1 );

   hb_ip_rfd_set( hSocket );
}

HB_FUNC( HB_IP_RFD_ZERO )
{
   hb_ip_rfd_zero();
}

HB_FUNC( HB_IP_RFD_CLR )
{
   HB_SOCKET_T hSocket = ( HB_SOCKET_T ) hb_parnl( 1 );

   hb_ip_rfd_clr( hSocket );
}

HB_FUNC( HB_IP_RFD_SELECT )
{
   int iTimeOut = ( ISNIL( 1 ) ) ? -1 : hb_parni( 1 );

   hb_retni( hb_ip_rfd_select( iTimeOut ) );
}

HB_FUNC( HB_IP_RFD_ISSET )
{
   HB_SOCKET_T hSocket = ( HB_SOCKET_T ) hb_parnl( 1 );

   hb_retl( FD_ISSET( hSocket, &active_fds ) );
}
