/*
* $Id: inet.c,v 1.45 2004/03/30 22:47:23 druzus Exp $
*/

/*
* xHarbour Project source code:
* The internet protocol / TCP support
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
*                Ron Pinkas [Ron@RonPinkas.com]
*                Marcelo Lombardo [marcelo.lombardo@newage-software.com.br]
* www - http://www.xharbour.org
*
* this program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General public License as published by
* the Free Software Foundation; either version 2, or (at your option)
* any later version.
*
* this program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
* GNU General public License for more details.
*
* You should have received a copy of the GNU General public License
* along with this software; see the file COPYING.  if not, write to
* the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
* Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* this exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General public License.
*
* this exception applies only to the code released with this xHarbour
* explicit exception.  if you add/copy code from other sources,
* as the General public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* if you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* if you do not wish that, delete this exception notice.
*
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "inet.h"
#include <fcntl.h>
#include <errno.h>

#if defined( HB_OS_UNIX ) || defined( OS_UNIX_COMPATIBLE ) || defined( HB_OS_BSD ) || defined(HB_OS_OS2)
   #include <sys/time.h>
#endif

#if defined(HB_OS_OS2)
   #include <sys/socket.h>
   #include <sys/select.h>
   #include <sys/ioctl.h>
   /* NET_SIZE_T exists because of shortsightedness on the POSIX committee.  BSD
    * systems used "int *" as the parameter to accept(), getsockname(),
    * getpeername() et al.  Consequently many unixes took an int * for that
    * parameter.  The POSIX committee decided that "int" was just too generic and
    * had to be replaced with size_t almost everywhere.  There's no problem with
    * that when you're passing by value.  But when you're passing by reference
    * this creates a gross source incompatibility with existing programs.  On
    * 32-bit architectures it creates only a warning.  On 64-bit architectures it
    * creates broken code -- because "int *" is a pointer to a 64-bit quantity and
    * "size_t *" is frequently a pointer to a 32-bit quantity.
    *
    * Some Unixes adopted "size_t *" for the sake of POSIX compliance.  Others
    * ignored it because it was such a broken interface.  Chaos ensued.  POSIX
    * finally woke up and decided that it was wrong and created a new type
    * socklen_t.  The only useful value for socklen_t is int, and that's how
    * everyone who has a clue implements it.  It is almost always the case that
    * NET_SIZE_T should be defined to be an int, unless the system being compiled
    * for was created in the window of POSIX madness.
    */
   #define socklen_t int
#endif


#ifdef HB_OS_LINUX
#include <signal.h>
#define HB_INET_LINUX_INTERRUPT     SIGUSR1+90
void hb_inetLinuxSigusrHandle( int sig )
{
   // nothing to do
   HB_SYMBOL_UNUSED( sig );
}
#endif

#ifndef HB_NO_DEFAULT_INET
//JC1: we need it volatile to be minimally thread safe.
static volatile int s_iSessions = 0;

/** Useful utility function to have a timeout; */

int hb_selectReadSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);

   if ( Socket->timeout == -1 )
   {
      select(Socket->com + 1, &set, NULL, NULL, NULL);
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      select(Socket->com + 1, &set, NULL, NULL, &tv);
   }

   return FD_ISSET( Socket->com, &set );
}

int hb_selectWriteSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);

   if ( Socket->timeout == -1 )
   {
      select(Socket->com + 1, NULL, &set, NULL, NULL);
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      select(Socket->com + 1, NULL, &set, NULL, &tv);
   }

   return FD_ISSET( Socket->com, &set );
}


int hb_selectWriteExceptSocket( HB_SOCKET_STRUCT *Socket )
{
   fd_set set, eset;
   struct timeval tv;

   FD_ZERO( &set );
   FD_SET(Socket->com, &set);
   FD_ZERO( &eset );
   FD_SET(Socket->com, &eset);

   if( Socket->timeout == -1 )
   {
      select(Socket->com + 1, NULL, &set, &eset, NULL);
   }
   else
   {
      tv.tv_sec = Socket->timeout/ 1000;
      tv.tv_usec = (Socket->timeout % 1000) * 1000;
      select(Socket->com + 1, NULL, &set, &eset, &tv);
   }

   if( FD_ISSET( Socket->com, &eset) )
   {
      return 2;
   }

   if( FD_ISSET( Socket->com, &set ) )
   {
      return 1;
   }
   return 0;
}


/*** Utilty to access host DNS */
struct hostent *hb_getHosts( char *name, HB_SOCKET_STRUCT *Socket )
{
   struct hostent *Host = NULL;

   /* let's see if name is an IP address; not necessary on linux */
   #if defined(HB_OS_WIN_32)
   ULONG ulAddr;

   ulAddr = inet_addr( name );
   if( ulAddr == INADDR_NONE )
   {
      if( strcmp( "255.255.255.255", name ) == 0 )
      {
         Host = gethostbyaddr( (const char*) &ulAddr, sizeof( ulAddr ), AF_INET );
      }
   }
   else
   {
      Host = gethostbyaddr( (const char*)  &ulAddr, sizeof( ulAddr ), AF_INET );
   }
   #endif

   if( Host == NULL )
   {
      Host = gethostbyname( name );
   }

   if( Host == NULL && Socket != NULL )
   {
      #if defined(HB_OS_WIN_32)
         HB_SOCKET_SET_ERROR2( Socket, WSAGetLastError() , "Generic error in GetHostByName()" );
         WSASetLastError( 0 );
      #elif defined(HB_OS_OS2)
         HB_SOCKET_SET_ERROR2( Socket, h_errno, "Generic error in GetHostByName()" );
      #else
         HB_SOCKET_SET_ERROR2( Socket, h_errno, (char *) hstrerror( h_errno ) );
      #endif
   }
   return Host;
}


/*** Setup the non-blocking method **/

void hb_socketSetNonBlocking( HB_SOCKET_STRUCT *Socket )
{

#ifdef HB_OS_WIN_32
   ULONG mode = 1;
   ioctlsocket( Socket->com,  FIONBIO, &mode );

#else
   int flags;

   flags = fcntl( Socket->com, F_GETFL );
   flags |= O_NONBLOCK;
   fcntl( Socket->com, F_SETFL, (LONG) flags );

#endif

}

/*** Utility to connect to a defined remote address ***/

int hb_socketConnect( HB_SOCKET_STRUCT *Socket )
{
   int iErr1;
   #if ! defined(HB_OS_WIN_32)
      int iErrval;
      socklen_t iErrvalLen;
   #endif
   int iOpt = 1;

   setsockopt( Socket->com, SOL_SOCKET, SO_KEEPALIVE, (const char *) &iOpt , sizeof( iOpt ));

   /* we'll be using a nonblocking functions */
   hb_socketSetNonBlocking( Socket );

   iErr1 = connect( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) );
   if( iErr1 != 0 )
   {

   #if defined(HB_OS_WIN_32)
      if( WSAGetLastError() != WSAEWOULDBLOCK )
   #else
      if( errno != EINPROGRESS )
   #endif
      {
         HB_SOCKET_SET_ERROR( Socket );
      }
      else
      {
         /* Now we wait for socket connection or timeout */

         #if defined(HB_OS_WIN_32)
         iErr1 = hb_selectWriteExceptSocket( Socket );
         if ( iErr1 == 2 )
         {
            HB_SOCKET_SET_ERROR2( Socket, 2, "Connection failed" );
         }
         else if ( iErr1 == 1 )
         {
            /* success */
         }
         #else

         if( hb_selectWriteSocket( Socket ) )
         {
            /* Connection has been completed with a failure or a success */
            iErrvalLen = sizeof( iErrval );
            iErr1 = getsockopt( Socket->com,
               SOL_SOCKET,
               SO_ERROR,
               (void *) &iErrval,
               &iErrvalLen
            );

            if( iErr1 )
            {
               HB_SOCKET_SET_ERROR1( Socket, iErr1 );
            }
            else if( iErrval )
            {
               HB_SOCKET_SET_ERROR1( Socket, iErrval );
            }
            /* Success! */
         }
         #endif
         /* Timed out */
         else {
            HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
         }
      }
   }

   return Socket->errorCode == 0;
}


HB_GARBAGE_FUNC( hb_inetSocketFinalize )
{
   HB_SOCKET_STRUCT *Socket = ( HB_SOCKET_STRUCT *) Cargo;

   if ( Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errInternal( HB_EI_MEMCORRUPT,
         "hb_inetSocketFinalize: Corrupted socket item at 0x%p",
         (char *) Socket, NULL );
      return;
   }

   if ( Socket->com > 0 )
   {
      #if defined( HB_OS_WIN_32 )
         shutdown( Socket->com, SD_BOTH );
      #elif defined(HB_OS_OS2)
         shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
      #else
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      HB_INET_CLOSE( Socket->com );
   }

   if ( Socket->caPeriodic != NULL )
   {
      hb_arrayRelease( Socket->caPeriodic );
      Socket->caPeriodic = NULL;
   }
}

/*****************************************************
* Socket Initialization
***/

HB_FUNC( INETINIT )
{
   if( s_iSessions )
   {
      s_iSessions++;
   }
   else
   {
      #if defined(HB_OS_WIN_32)
         WSADATA wsadata;
         WSAStartup( MAKEWORD(1,1), &wsadata );
      #elif defined( HB_OS_LINUX )
         signal( HB_INET_LINUX_INTERRUPT, hb_inetLinuxSigusrHandle );
      #endif
      s_iSessions = 1;
   }
}

HB_FUNC( INETCLEANUP )
{
   if( --s_iSessions == 0 )
   {
      #if defined(HB_OS_WIN_32)
         WSACleanup();
      #endif
   }
}

/*****************************************************
* Socket Creation and destruction
***/

HB_FUNC( INETCREATE )
{
   HB_SOCKET_STRUCT *Socket;
   HB_SOCKET_INIT( Socket );

   if ( ISNUM( 1 ) )
   {
      Socket->timeout = hb_parni(1);
   }

   hb_retptrGC( Socket );
}

HB_FUNC( INETCLOSE )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLOSE", 1, hb_paramError(1) );
      return;
   }

   if( Socket->com )
   {
      #if defined( HB_OS_WIN_32 )
         shutdown( Socket->com, SD_BOTH );
      #elif defined(HB_OS_OS2)
         shutdown( Socket->com, SO_RCV_SHUTDOWN + SO_SND_SHUTDOWN );
      #else
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      hb_retni( HB_INET_CLOSE( Socket->com ) );

      Socket->com = 0;
      #ifdef HB_OS_LINUX
         kill( 0, HB_INET_LINUX_INTERRUPT );
      #endif
   }
   else
   {
      hb_retni( -1 );
   }
}


/* Kept for backward compatibility */

HB_FUNC( INETDESTROY )
{

}

/************************************************
* Socket data access & management
***/

HB_FUNC( INETSTATUS )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSTATUS", 1,
          hb_paramError( 1 ) );
      return;
   }

   /* TODO: hb_retni( Socket->status ); */
   hb_retni( Socket->com == 0 ? -1 : 1 );
}

/* Prepared, but still not used; being in wait for comments
HB_FUNC( INETSTATUSDESC )
{
   HB_SOCKET_STRUCT *Socket = hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSTATUS", 1,
          hb_paramError( 1 ) );
      return;
   }

   switch ( Socket->status )
   {
      case 0: hb_retc( "Connection not opened" ); return;
      case 1: hb_retc( "Connection alive" ); return;
      case 2: hb_retc( "Last operation error" ); return;
      case 3: hb_retc( "Last operation timeout" ); return;
   }
}
*/

HB_FUNC( INETERRORCODE )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETERRORCODE", 1,
         hb_paramError(1) );
      return;
   }

   hb_retni( Socket->errorCode );
}

HB_FUNC( INETERRORDESC )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETERRORDESC", 1,
         hb_paramError(1) );
      return;
   }

   hb_retc( Socket->errorDesc );
}

HB_FUNC( INETCLEARERROR )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLEARERROR", 1,
         hb_paramError( 1 ) );
      return;
   }

   HB_SOCKET_ZERO_ERROR( Socket );
}


HB_FUNC( INETCOUNT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCOUNT", 1,
         hb_paramError(1) );
      return;
   }

   hb_retni( Socket->count );
}

HB_FUNC( INETADDRESS )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );
   char *addr;

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETADDRESS", 1,
         hb_paramError( 1 ) );
      return;
   }

   addr = inet_ntoa( Socket->remote.sin_addr );

   hb_retc( addr );
}

HB_FUNC( INETPORT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETPORT", 1,
         hb_paramError( 1 ) );
      return;
   }


   hb_retni( ntohs( Socket->remote.sin_port ) );
}


HB_FUNC( INETSETTIMEOUT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket != NULL && Socket->sign == HB_SOCKET_SIGN && ISNUM(2) )
   {
      Socket->timeout = hb_parni(2);
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSETTIMEOUT", 2,
         hb_paramError(1), hb_paramError(2) );
   }
}

HB_FUNC( INETGETTIMEOUT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETTIMEOUT", 1,
         hb_paramError(1) );
      return;
   }

   hb_retni( Socket->timeout );
}


HB_FUNC( INETCLEARTIMEOUT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLEARTIMEOUT", 1,
         hb_paramError(1) );
      return;
   }

   Socket->timeout = -1;
}

HB_FUNC( INETSETTIMELIMIT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN || !ISNUM(2))
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSETTIMELIMIT", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   Socket->timelimit = hb_parnl(2);
}

HB_FUNC( INETGETTIMELIMIT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETTIMELIMIT", 1,
        hb_paramError(1) );
      return;
   }

   hb_retni( Socket->timelimit );
}


HB_FUNC( INETCLEARTIMELIMIT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLEARTIMELIMIT", 1,
         hb_paramError(1) );
      return;
   }

   Socket->timelimit = -1;
}

HB_FUNC( INETSETPERIODCALLBACK )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSETPERIODCALLBACK", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   if ( Socket->caPeriodic != NULL )
   {
      hb_arrayRelease( Socket->caPeriodic );
   }

   Socket->caPeriodic  = hb_arrayClone( pArray, NULL );
   hb_gcLock( Socket->caPeriodic );
}


HB_FUNC( INETGETPERIODCALLBACK )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETPERIODCALLBACK", 1,
         hb_paramError(1) );
      return;
   }

   if (  Socket->caPeriodic == NULL )
   {
      hb_ret();
   }
   else
   {
      hb_itemReturnCopy( Socket->caPeriodic );
   }
}

HB_FUNC( INETCLEARPERIODCALLBACK )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLEARPERIODCALLBACK", 1,
         hb_paramError(1) );
   }

   if ( Socket->caPeriodic != NULL )
   {
      hb_arrayRelease( Socket->caPeriodic );
      Socket->caPeriodic = NULL;
   }
}

/********************************************************************
* TCP receive and send functions
***/

static void s_inetRecvInternal( char *szFuncName, int iMode )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_BYREF );
   char *Buffer;
   int iLen, iMaxLen, iReceived, iBufferLen;
   int iTimeElapsed;

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, szFuncName, 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   if( ISNIL( 3 ) )
   {
      iMaxLen = pBuffer->item.asString.length;
   }
   else
   {
      iMaxLen = hb_parni( 3 );

      if( (signed int) pBuffer->item.asString.length < iMaxLen )
      {
         /* Should we issue a runtime error? */
         hb_xrealloc(pBuffer->item.asString.value, iMaxLen );
      }
   }

   Buffer = pBuffer->item.asString.value;
   iReceived = 0;
   iTimeElapsed = 0;
   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      if ( iMode == 1 )
      {
         iBufferLen = HB_SENDRECV_BUFFER_SIZE > iMaxLen-iReceived ?
               iMaxLen-iReceived : HB_SENDRECV_BUFFER_SIZE;
      }
      else {
         iBufferLen = iMaxLen;
      }

      HB_STACK_UNLOCK;
      HB_TEST_CANCEL_ENABLE_ASYN;
      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, Buffer + iReceived, iBufferLen, MSG_NOSIGNAL );
         HB_DISABLE_ASYN_CANC;
         HB_STACK_LOCK;

         if( iLen > 0 )
         {
            iReceived += iLen;
         }

         /* Called from InetRecv()? */
         if ( iMode == 0 )
         {
            break;
         }

      }
      else
      {
         // timed out; let's see if we have to run a cb routine
         HB_DISABLE_ASYN_CANC;
         HB_STACK_LOCK;
         iTimeElapsed += Socket->timeout;

         /* if we have a caPeriodic, timeLimit is our REAL timeout */
         if ( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );
            // do we continue?
            if ( ! hb_itemGetL( &HB_VM_STACK.Return ) ||
               (Socket->timelimit != -1 && iTimeElapsed >= Socket->timelimit ) )
            {
               HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" )
               hb_retni( iReceived );
               return;
            }

            /* Declare success to continue loop */
            iLen = 1;
         }
         else /* the timeout has gone, and we have no recovery routine */
         {
            HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" )
            hb_retni( iReceived );
            return;
         }
      }
   }
   while( iReceived < iMaxLen && iLen > 0 );

   Socket->count = iReceived;

   if ( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      hb_retni( iLen );
   }
   if ( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_retni( iLen );
   }
   else
   {
      hb_retni( iReceived );
   }
}


HB_FUNC( INETRECV )
{
   s_inetRecvInternal( "INETRECV", 0 );
}


HB_FUNC( INETRECVALL )
{
   s_inetRecvInternal( "INETRECVALL", 1 );
}


static void s_inetRecvPattern( char *szFuncName, char *szPattern )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 4, HB_IT_NUMERIC );

   char cChar;
   char *Buffer;
   int iAllocated, iBufferSize, iMax;
   int iLen = 0;
   int iPos = 0, iTimeElapsed;
   ULONG ulPatPos;


   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, szFuncName, 4,
         hb_paramError(1), hb_paramError(2),
         hb_paramError(3), hb_paramError(4) );
      return;
   }

   if( pBufferSize )
   {
      iBufferSize = hb_itemGetNI( pBufferSize );
   }
   else
   {
      iBufferSize = 80;
   }

   if( pMaxSize )
   {
      iMax = hb_itemGetNI( pMaxSize );
   }
   else
   {
      iMax = 0;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = (char *) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;
   iTimeElapsed = 0;

   ulPatPos = 0;
   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         Buffer = ( char * ) hb_xrealloc( Buffer, iAllocated );
      }

      HB_STACK_UNLOCK;
      HB_TEST_CANCEL_ENABLE_ASYN;

      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL );
         HB_DISABLE_ASYN_CANC;
         HB_STACK_LOCK;
      }
      else {
         HB_DISABLE_ASYN_CANC;
         HB_STACK_LOCK;
         iTimeElapsed += Socket->timeout;

         if ( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );
            // do we continue?
            if ( hb_itemGetL( &HB_VM_STACK.Return ) &&
               (Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit ))
            {
               continue;
            }
         }

         /* this signals timeout */
         iLen = -2;
      }

      if( iLen > 0 )
      {
         /* verify endsequence recognition automata status */
         if ( cChar == szPattern[ ulPatPos ] )
         {
            ulPatPos ++;
            if ( ! szPattern[ ulPatPos ] )
            {
               break;
            }
         }
         else
         {
            ulPatPos = 0;
         }

         Buffer[ iPos++ ] = cChar;
      }
      else
      {
         break;
      }

   }
   while( iMax == 0 || iPos < iMax );

   if( iLen <= 0 )
   {
      if( pResult )
      {
         hb_itemPutNL( pResult, iLen );
      }

      if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" )
      }
      else if ( iLen == -2 ) {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" )
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
      }

      hb_xfree( (void *) Buffer );
      hb_ret();
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         iPos--;
         Socket->count = iPos;

         if( pResult )
         {
            hb_itemPutNL( pResult, iPos );
         }

         hb_retclenAdopt( Buffer, iPos );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -3, "Buffer overrun" );

         if( pResult )
         {
            hb_itemPutNL( pResult, -2 );
         }

         hb_xfree( (void *) Buffer );
         hb_retc( NULL );
      }
   }
}

HB_FUNC( INETRECVLINE )
{
   s_inetRecvPattern( "INETRECVLINE", "\r\n");
}


HB_FUNC( INETRECVENDBLOCK )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   PHB_ITEM pProto;
   HB_ITEM  ProtoOpt;
   PHB_ITEM pResult     = hb_param( 3, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 4, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 5, HB_IT_NUMERIC );

   char cChar;
   char *Buffer;
   char **Proto;
   int iAllocated, iBufferSize, iMax;
   int iLen;
   int iPos = 0;
   int iPosProto;
   int iTimeElapsed = 0;
   int iprotos;
   int i;
   int *iprotosize;
   int ifindproto = 0;
   BOOL bProtoFound;


   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETRECVENDBLOCK", 5,
         hb_paramError(1), hb_paramError(2),
         hb_paramError(3), hb_paramError(4), hb_paramError(5) );
      return;
   }

   if( ISARRAY( 2 ) || ISCHAR( 2 ) )
   {
      if( ISARRAY( 2 ) )
      {
         pProto  = hb_param( 2, HB_IT_ARRAY );
         iprotos = (int) pProto->item.asArray.value->ulLen;

         if( iprotos <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETRECVENDBLOCK", 5,
               hb_paramError(1), hb_paramError(2),
               hb_paramError(3), hb_paramError(4), hb_paramError(5) );
            return;
         }

         Proto   = (char**) hb_xgrab( sizeof(char*) * iprotos );
         iprotosize = (int *) hb_xgrab( sizeof(int) * iprotos );

         for(i=0;i<iprotos;i++)
         {
            ProtoOpt.type = HB_IT_NIL;
            hb_arrayGet( pProto, i+1, &ProtoOpt );
            Proto[i]      = (char *) (&ProtoOpt)->item.asString.value;
            iprotosize[i] = (&ProtoOpt)->item.asString.length;
            hb_itemClear( &ProtoOpt );
         }
      }
      else
      {
         pProto        = hb_param( 2, HB_IT_STRING );
         Proto         = (char**) hb_xgrab( sizeof(char*) );
         iprotosize    = (int *) hb_xgrab( sizeof(int) );
         Proto[0]      = (char *) pProto->item.asString.value;
         iprotosize[0] = pProto->item.asString.length;
         iprotos       = 1;
      }
   }
   else
   {
      Proto         = (char**) hb_xgrab( sizeof(char*) );
      iprotosize    = (int *) hb_xgrab( sizeof(int) );
      Proto[0]      = (char *) "\r\n";
      iprotos       = 1;
      iprotosize[0] = 2;
   }

   if( pBufferSize )
   {
      iBufferSize = hb_itemGetNI( pBufferSize );
   }
   else
   {
      iBufferSize = 80;
   }

   if( pMaxSize )
   {
      iMax = hb_itemGetNI( pMaxSize );
   }
   else
   {
      iMax = 0;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Buffer = (char *) hb_xgrab( iBufferSize );
   iAllocated = iBufferSize;

   do
   {
      if( iPos == iAllocated - 1 )
      {
         iAllocated += iBufferSize;
         Buffer = ( char * ) hb_xrealloc( Buffer, iAllocated );
      }

      iLen = 0;

      HB_STACK_UNLOCK;
      HB_TEST_CANCEL_ENABLE_ASYN;
      if( hb_selectReadSocket( Socket ) )
      {
         iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL );
         HB_DISABLE_ASYN_CANC;

      }
      else {
         HB_DISABLE_ASYN_CANC;
         HB_STACK_LOCK;
         iTimeElapsed += Socket->timeout;

         if ( Socket->caPeriodic != NULL )
         {
            hb_execFromArray( Socket->caPeriodic );

            if ( hb_itemGetL( &HB_VM_STACK.Return ) &&
               (Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit ))
            {
               continue;
            }
         }

         iLen = -2;
      }

      if( iLen > 0 )
      {
         int protos;
         bProtoFound = 0;
         for( protos=0;protos < iprotos;protos++)
         {
            if( cChar == Proto[protos][iprotosize[protos]-1] && iprotosize[protos] <= iPos )
            {
               bProtoFound = 1;
               for(iPosProto=0; iPosProto < (iprotosize[protos]-1); iPosProto++)
               {
                  if(Proto[protos][iPosProto] != Buffer[ (iPos-iprotosize[protos])+iPosProto+1 ])
                  {
                     bProtoFound = 0;
                     break;
                  }
               }
               if(bProtoFound)
               {
                  ifindproto = protos;
                  break;
               }
            }
         }
         if(bProtoFound)
            break;

         Buffer[ iPos++ ] = cChar;
      }
      else
      {
         break;
      }
   }
   while( iMax == 0 || iPos < iMax );

   if( iLen <= 0 )
   {
      if( pResult )
      {
         hb_itemPutNL( pResult, iLen );
      }

      if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" )
      }
      else if( iLen == -2 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" )
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
      }

      hb_xfree( (void *) Buffer );
      hb_retc( NULL );
   }
   else
   {
      if( iMax == 0 || iPos < iMax )
      {
         Socket->count = iPos;

         if( pResult )
         {
            hb_itemPutNL( pResult, iPos  - (iprotosize[ifindproto]-1) );
         }

         hb_retclenAdopt( Buffer, iPos  - (iprotosize[ifindproto]-1) );
      }
      else
      {
         HB_SOCKET_SET_ERROR2( Socket, -1, "Buffer overrun" );

         if( pResult )
         {
            hb_itemPutNL( pResult, -2 );
         }

         hb_xfree( (void *) Buffer );
         hb_retc( NULL );
      }
   }
   hb_xfree( Proto );
   hb_xfree( iprotosize );
}


HB_FUNC( INETDATAREADY )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   int iLen;
   fd_set rfds;
   struct timeval tv = {0,0};

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN ||
      ( hb_pcount() == 2 && ! ISNUM(2)) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDATAREADY", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   /* Watch our socket. */
   if( hb_pcount() == 2 ) {
      iLen = hb_parni( 2 );
      tv.tv_sec = iLen / 1000;
      tv.tv_usec = (iLen % 1000) * 1000;
   }

   FD_ZERO(&rfds);
   FD_SET(Socket->com, &rfds);

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   iLen = select(Socket->com + 1, &rfds, NULL, NULL, &tv);

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;
   /* Don't rely on the value of tv now! */

   if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }

   hb_retni( iLen );
}


static void s_inetSendInternal( char *szFuncName, int iMode )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char *Buffer;
   int iLen, iSent, iSend, iBufferLen;

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, szFuncName, 3,
         hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   Buffer = pBuffer->item.asString.value;

   if( ISNUM( 3 ) )
   {
      iSend = hb_parni( 3 );
   }
   else
   {
      iSend = pBuffer->item.asString.length;
   }

   iSent = 0;

   HB_SOCKET_ZERO_ERROR( Socket );

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   iLen = 0;
   while( iSent < iSend )
   {
      if ( iMode == 1 )
      {
         iBufferLen = HB_SENDRECV_BUFFER_SIZE > iSend - iSent ?
               iSend - iSent : HB_SENDRECV_BUFFER_SIZE;
      }
      else
      {
         iBufferLen = iSend;
      }

      iLen = 0;
      if( hb_selectWriteSocket( Socket ) )
      {
         iLen = send( Socket->com, Buffer + iSent, iBufferLen, MSG_NOSIGNAL );
      }

      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else if( iLen == 0 )
      {
         HB_SOCKET_SET_ERROR2( Socket, -1 , "Timeout" );
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
         break;
      }

      if ( iMode == 0 )
      {
         break;
      }
   }
   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   Socket->count = iSent;

   if ( iLen > 0 )
   {
      hb_retni( iSent );
   }
   else
   {
      hb_retni( -1 );
   }
}

HB_FUNC( INETSEND )
{
   s_inetSendInternal( "INETSEND", 0 );
}

HB_FUNC( INETSENDALL )
{
   s_inetSendInternal( "INETSENDALL", 1 );
}


/*******************************************
* Name resolution interface functions
***/

HB_FUNC( INETGETHOSTS )
{
   PHB_ITEM pHost = hb_param( 1, HB_IT_STRING );
   HB_ITEM aHosts, Tmp;
   struct hostent *Host;
   char **cHosts;

   if( pHost == NULL )
   {
      /*
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETHOSTS", 1, pArgs );
      hb_itemRelease( pArgs );
      */
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETHOSTS", 1, hb_paramError(1) );
      return;
   }

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   Host = hb_getHosts( pHost->item.asString.value, NULL );

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   aHosts.type = HB_IT_NIL;
   Tmp.type = HB_IT_NIL;

   hb_arrayNew( &aHosts, 0 );

   if( Host == NULL )
   {
      hb_itemReturn( &aHosts );
      return;
   }

   cHosts = Host->h_addr_list;
   while( *cHosts ) {
      hb_arrayAddForward( &aHosts, hb_itemPutC( &Tmp, inet_ntoa( *( (struct in_addr *)*cHosts ) ) ) );
      cHosts++;
   }

   hb_itemReturn( &aHosts );
}


HB_FUNC( INETGETALIAS )
{
   PHB_ITEM pHost = hb_param( 1, HB_IT_STRING );
   HB_ITEM aHosts, Tmp;
   struct hostent *Host;
   char **cHosts;

   if( pHost == NULL )
   {
      /*
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETHOSTS", 1, pArgs );
      hb_itemRelease( pArgs );
      */
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETGETALIAS", 1, hb_paramError(1) );
      return;
   }

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   Host = hb_getHosts( pHost->item.asString.value, NULL );

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   aHosts.type = HB_IT_NIL;
   Tmp.type = HB_IT_NIL;

   hb_arrayNew( &aHosts, 0 );

   if( Host == NULL )
   {
      hb_itemReturn( &aHosts );
      return;
   }

   cHosts = Host->h_aliases;
   while( *cHosts ) {
      hb_arrayAddForward( &aHosts, hb_itemPutC( &Tmp, *cHosts ) );

      cHosts++;
   }

   hb_itemReturn( &aHosts );
}


/**********************************************
* Server Specific functions
****/

HB_FUNC( INETSERVER )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(2);
   int iPort;
   int iOpt = 1;
   int iListen;

   /* Parameter error checking */
   if( ! ISNUM( 1 ) || ( Socket != NULL && Socket->sign != HB_SOCKET_SIGN ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSERVER", 2,
         hb_paramError(1), hb_paramError(2) );
      return;
   }

   if ( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket );
   }

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_STREAM, 0 );
   #else
      Socket->com = socket( PF_INET, SOCK_STREAM, 0 );
   #endif

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      hb_retptrGC( Socket );
      return;
   }

   /* we'll be using only nonblocking sockets */
   hb_socketSetNonBlocking( Socket );

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   iPort  = htons( hb_parni( 1 ) );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = iPort;

   if ( ! ISCHAR( 2 ) ) {
      Socket->remote.sin_addr.s_addr = INADDR_ANY;
   }
   else {
      Socket->remote.sin_addr.s_addr = inet_addr( hb_parcx( 2 ) );
   }

   if ( ISNUM( 3 ) )
   {
      iListen = hb_parni( 3 );
   }
   else
   {
      iListen = 10;
   }

   if( bind( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }
   else if ( listen( Socket->com, iListen ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }

   hb_retptrGC( Socket );
}


HB_FUNC( INETACCEPT )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 1 );
   HB_SOCKET_STRUCT *NewSocket;
   HB_SOCKET_T incoming = 0;
   int iError = EAGAIN;
   struct sockaddr_in si_remote;

   #if defined(HB_OS_WIN_32)
      int Len;
   #else
      UINT Len;
   #endif

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETACCEPT", 1,
         hb_paramError(1) );
      return;
   }

   Len = sizeof( struct sockaddr_in );

   /*
   * Accept can (and should) be asynchronously stopped by closing the
   * accepting socket. this will make the wait to terminate, and the
   * calling program will be notivfied through the status of the
   * returned socket.
   */


   HB_SOCKET_ZERO_ERROR( Socket );

   HB_STACK_UNLOCK;
   /* Connection incoming */

   while ( iError == EAGAIN )
   {
      HB_TEST_CANCEL_ENABLE_ASYN;
      if( hb_selectReadSocket( Socket ) )
      {
         /* On error (e.g. async connection closed) , com will be -1 and
            errno == 22 (invalid argument ) */
         incoming = accept( Socket->com, (struct sockaddr *) &si_remote, &Len );

         HB_DISABLE_ASYN_CANC;

         if (incoming == -1 )
         {
            #if defined(HB_OS_WIN_32)
            iError = WSAGetLastError();
            #else
            iError = errno;
            #endif
         }
         else
         {
            iError = 0;
         }
      }
      /* Timeout expired */
      else
      {
         iError = -1;
         HB_DISABLE_ASYN_CANC;
      }
   }
   HB_STACK_LOCK;

   if( iError == -1 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      hb_ret();
   }
   else if ( iError > 0 )
   {
      HB_SOCKET_SET_ERROR1( Socket, iError );
      hb_ret();
   }
   else
   {
      /* we'll be using only nonblocking sockets */
      HB_SOCKET_INIT( NewSocket );
      memcpy( &NewSocket->remote, &si_remote, Len );
      NewSocket->com = incoming;
      hb_socketSetNonBlocking( NewSocket );
      hb_retptrGC( NewSocket );
   }

}


/**********************************************
* Client specific (connection functions)
****/

HB_FUNC( INETCONNECT )
{
   PHB_ITEM pHost = hb_param( 1, HB_IT_STRING );
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer( 3 ) ;

   int iPort;
   struct hostent *Host;

   if( pHost == NULL || !ISNUM(2)
         || ( Socket != NULL && Socket->sign != HB_SOCKET_SIGN ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCONNECT", 3,
         hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   if ( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket );
   }

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   Host = hb_getHosts( pHost->item.asString.value, Socket );

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   /* error had been set by get hosts */

   if ( Host == NULL )
   {
      goto ret;
   }

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_STREAM, 0);
   #else
      Socket->com = socket( PF_INET, SOCK_STREAM, 0);
   #endif

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      goto ret;
   }

   iPort = htons( hb_parni( 2 ) );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port= iPort;
   Socket->remote.sin_addr.s_addr = (*(UINT *)Host->h_addr_list[0]);

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   hb_socketConnect( Socket );

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

ret:
   hb_retptrGC( Socket );
}


HB_FUNC( INETCONNECTIP )
{
   PHB_ITEM pHost = hb_param( 1, HB_IT_STRING );
   int iPort = hb_parni( 2 );
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(3);

   if( pHost == NULL || iPort == 0
         || ( Socket != NULL && Socket->sign != HB_SOCKET_SIGN ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCONNECTIP", 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   if ( Socket != NULL )
   {
      HB_SOCKET_ZERO_ERROR( Socket );
   }
   else
   {
      HB_SOCKET_INIT( Socket );
   }

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_STREAM, 0);
   #else
      Socket->com = socket( PF_INET, SOCK_STREAM, 0);
   #endif

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      goto ret;
   }

   iPort = htons( iPort );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port= iPort;
   Socket->remote.sin_addr.s_addr = inet_addr( pHost->item.asString.value );

   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   hb_socketConnect( Socket );

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

ret:
   hb_retptrGC( Socket );
}

/***********************************************************
* Datagram functions
************************************************************/

HB_FUNC( INETDGRAMBIND )
{
   int iPort = hb_parni(1);
   int iOpt = 1;
   HB_SOCKET_STRUCT *Socket;

   /* Parameter error checking */
   if( iPort == 0 )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDGRAMBIND", 1,
         hb_paramError( 1 ) );
      return;
   }

   HB_SOCKET_INIT( Socket );

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
   #else
      Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
   #endif

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      hb_retptrGC( Socket );
      return;
   }

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   /* Setting broadcast if needed. */
   if ( hb_parl( 3 ) ) {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, (const char *) &iOpt, sizeof( iOpt ));
   }

   /* we'll be using non blocking sockets in all functions */
   hb_socketSetNonBlocking( Socket );

   /* Binding here */
   iPort  = htons( iPort );

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = iPort;

   if ( ! ISCHAR( 2 ) ) {
      Socket->remote.sin_addr.s_addr = INADDR_ANY;
   }
   else {
      Socket->remote.sin_addr.s_addr = inet_addr( hb_parcx( 2 ) );
   }


   if( bind( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
      HB_INET_CLOSE( Socket->com );
   }

   hb_retptrGC( Socket );
}

HB_FUNC( INETDGRAM )
{
   int iOpt = 1;
   HB_SOCKET_STRUCT *Socket;

   HB_SOCKET_INIT( Socket );

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );
   #else
      Socket->com = socket( PF_INET, SOCK_DGRAM, 0 );
   #endif

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->com = 0;
      hb_retptrGC( Socket );
      return;
   }

   /* Setting broadcast if needed. */
   if ( hb_parl( 1 ) ) {
      iOpt = 1;
      setsockopt( Socket->com, SOL_SOCKET, SO_BROADCAST, (const char *) &iOpt, sizeof( iOpt ));
   }
   /* we'll be using non blocking sockets in all functions */
   hb_socketSetNonBlocking( Socket );

   hb_retptrGC( Socket );
}


HB_FUNC( INETDGRAMSEND )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   char *szAddress = hb_parcx(2);
   int iPort = hb_parni( 3 );
   PHB_ITEM pBuffer = hb_param( 4, HB_IT_STRING );
   int iLen;
   char *szBuffer ;

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN ||
       szAddress == NULL || iPort == 0 || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDGRAMSEND", 5,
         hb_paramError(1), hb_paramError(2), hb_paramError(3),
         hb_paramError(4), hb_paramError(5) );
      return;
   }

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = htons( iPort );
   Socket->remote.sin_addr.s_addr = inet_addr( szAddress );
   szBuffer = pBuffer->item.asString.value;

   if ( ISNUM( 5 ) )
   {
      iLen = hb_parni( 5 );
   }
   else
   {
      iLen = pBuffer->item.asString.length;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

   Socket->count = 0;
   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;

   if( hb_selectWriteSocket( Socket ) )
   {
      Socket->count = sendto( Socket->com, szBuffer, iLen, 0,
            (const struct sockaddr *) &Socket->remote, sizeof( Socket->remote ) );
   }

   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   hb_retni( Socket->count );

   if( Socket->count == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
   }
   else if( Socket->count < 0 )
   {
      Socket->count = 0;
      HB_SOCKET_SET_ERROR( Socket );
   }

}


HB_FUNC( INETDGRAMRECV )
{
   HB_SOCKET_STRUCT *Socket = (HB_SOCKET_STRUCT *) hb_parpointer(1);
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );

   char *Buffer;
   int iLen,iMaxLen;
   int iTimeElapsed = 0;

   #if defined(HB_OS_WIN_32)
      int iDtLen = sizeof( struct sockaddr );
   #else
      socklen_t iDtLen = (socklen_t) sizeof( struct sockaddr );
   #endif

   if( Socket == NULL || Socket->sign != HB_SOCKET_SIGN || pBuffer == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDGRAMRECV", 3,
         hb_paramError(1), hb_paramError(2), hb_paramError(3) );
      return;
   }

   Buffer = pBuffer->item.asString.value;

   if ( ISNUM( 3 ) )
   {
      iMaxLen = hb_parni( 3 );
   }
   else
   {
      iMaxLen = pBuffer->item.asString.length;
   }

   HB_SOCKET_ZERO_ERROR( Socket );

about_wait:
   HB_STACK_UNLOCK;
   HB_TEST_CANCEL_ENABLE_ASYN;
   iLen = -2;

   if( hb_selectReadSocket( Socket ) )
   {
      iLen = recvfrom( Socket->com, Buffer, iMaxLen, 0,
            (struct sockaddr *) &Socket->remote, &iDtLen );
   }
   HB_DISABLE_ASYN_CANC;
   HB_STACK_LOCK;

   iTimeElapsed += Socket->timeout;

   if ( Socket->caPeriodic != NULL )
   {
      hb_execFromArray( Socket->caPeriodic );
      // do we continue?
      if ( hb_itemGetL( &HB_VM_STACK.Return ) &&
         (Socket->timelimit == -1 || iTimeElapsed < Socket->timelimit ))
      {
         goto about_wait;
      }
   }

   if( iLen == -2 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Timeout" );
      Socket->count = 0;
      iLen = -1;
   }
   else if( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -2, "Connection closed" );
      Socket->count = 0;
   }
   else if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      Socket->count = 0;
   }
   else
   {
      Socket->count = iLen;
   }
   hb_retni( iLen );
}


/***********************************************************
* Generic utility(?) functions
************************************************************/

HB_FUNC( INETCRLF )
{
   hb_retc( "\r\n" );
}


#endif
