/*
* $Id: inet.c,v 1.25 2003/01/04 13:14:09 jonnymind Exp $
*/

/*
* xHarbour Project source code:
* The internet protocol / TCP support
*
* Copyright 2002 Giancarlo Niccolai [gian@niccolai.ws]
*                Ron Pinkas [Ron@RonPinkas.com]
* www - http://www.xharbour.org
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
* As a special exception, xHarbour license gives permission for
* additional uses of the text contained in its release of xHarbour.
*
* The exception is that, if you link the xHarbour libraries with other
* files to produce an executable, this does not by itself cause the
* resulting executable to be covered by the GNU General Public License.
* Your use of that executable is in no way restricted on account of
* linking the xHarbour library code into it.
*
* This exception does not however invalidate any other reasons why
* the executable file might be covered by the GNU General Public License.
*
* This exception applies only to the code released with this xHarbour
* explicit exception.  If you add/copy code from other sources,
* as the General Public License permits, the above exception does
* not apply to the code that you add in this way.  To avoid misleading
* anyone as to the status of such modified files, you must delete
* this exception notice from them.
*
* If you write modifications of your own for xHarbour, it is your choice
* whether to permit this exception to apply to your modifications.
* If you do not wish that, delete this exception notice.
*
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "inet.h"


#ifndef HB_NO_DEFAULT_INET
static int s_iSessions = 0;

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
      #endif
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

HB_FUNC( INETSERVER )
{
   int iPort, iPort1;
   int iOpt = 1;
   HB_SOCKET_STRUCT *Socket;
   int iListen;

   /* Parameter error checking */
   if( ! ISNUM( 1 ) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSERVER", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   HB_SOCKET_INIT( Socket );

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
      hb_retclenAdoptRaw( (char *) Socket, sizeof( HB_SOCKET_STRUCT ) );
      return;
   }

   /* Reusable socket; under unix, do not wait it is unused */
   setsockopt( Socket->com, SOL_SOCKET, SO_REUSEADDR, (const char *) &iOpt, sizeof( iOpt ));

   iPort  = hb_parni( 1 );
   iPort1 = (iPort & 0xff00) >> 8 | (iPort & 0x00ff) << 8;

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port = iPort1;
   Socket->remote.sin_addr.s_addr = INADDR_ANY;

   if ( ISNUM( 2 ) )
   {
      iListen = hb_parni( 2 );
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

   hb_retclenAdoptRaw( (char *) Socket, sizeof( HB_SOCKET_STRUCT ) );
}


#ifdef HB_THREAD_SUPPORT
HB_GARBAGE_FUNC( acceptBlockingDestructor )
{
   HB_THREAD_CONTEXT *pContext = (HB_THREAD_CONTEXT *) Cargo;
   HB_INET_CARGO *inetCargo = (HB_INET_CARGO *) ( pContext->Cargo );

   if( inetCargo )
   {
      //HB_CRITICAL_INET_UNLOCK( inetCargo->Socket->Mutex );

      if( inetCargo->NewSocket )
      {
         //TraceLogPointer( NULL, "free %p\n", inetCargo->NewSocket );
         hb_xfree( (void *) ( inetCargo->NewSocket ) );
      }

      //TraceLogPointer( NULL, "free %p\n", inetCargo );
      hb_xfree( inetCargo );

      pContext->Cargo       = NULL;
      pContext->pDestructor = NULL;
   }
}
#endif

HB_FUNC( INETACCEPT )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );
   HB_SOCKET_STRUCT *Socket, *NewSocket;

   #if defined(HB_OS_WIN_32)
      int Len;
   #else
      unsigned int Len;
   #endif

   Len = sizeof( struct sockaddr_in );

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETACCEPT", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   /*
   * Because accept() IS a blocking function, the only way to stop is with StopThread().
   * But, if StopThread() is used, NewSocket will NOT be returned to caller, and
   * UNRELEASED memory will be rported by xHarbour.
   */

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_INIT( NewSocket );

   //HB_CRITICAL_INET_LOCK( NewSocket->Mutex );

   #define HB_PROTECT_BLOCKING
   #if defined(HB_PROTECT_BLOCKING) && defined(HB_THREAD_SUPPORT)
      if( hb_ht_context )
      {
         HB_INET_CARGO *inetCargo = (HB_INET_CARGO *) hb_xgrab( sizeof( HB_INET_CARGO ) );

         inetCargo->Socket    = Socket;
         inetCargo->NewSocket = NewSocket;

         hb_threadGetCurrentContext()->Cargo = (void *) inetCargo;
         hb_threadGetCurrentContext()->pDestructor = acceptBlockingDestructor;
      }
   #endif

   HB_SOCKET_ZERO_ERROR( Socket );

   NewSocket->com = accept( Socket->com, (struct sockaddr *) &NewSocket->remote, &Len );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

  #if defined(HB_PROTECT_BLOCKING) && defined(HB_THREAD_SUPPORT)
      if( hb_ht_context )
      {
         HB_THREAD_CONTEXT *pContext = hb_threadGetCurrentContext();

         if( pContext )
         {
            HB_INET_CARGO *inetCargo = (HB_INET_CARGO *) ( pContext->Cargo );

            if( inetCargo )
            {
               hb_xfree( inetCargo );
               inetCargo->Cargo = NULL;

               pContext->pDestructor = NULL;
            }
         }
      }
   #endif

   if( NewSocket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( NewSocket );
      NewSocket->com = 0;
   }

   //HB_CRITICAL_INET_UNLOCK( NewSocket->Mutex );

   hb_retclenAdoptRaw( (char *) NewSocket, sizeof( HB_SOCKET_STRUCT ) );
}

HB_FUNC( INETRECV )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_BYREF ); // hb_param() calls hb_itemUnref()!

   char *Buffer;
   int iLen;
   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL || pBuffer == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETRECV", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   if( ISNIL( 3 ) )
   {
      iLen = pBuffer->item.asString.length;
   }
   else
   {
      iLen = hb_parni( 3 );

      if( (signed int) pBuffer->item.asString.length < iLen )
      {
         /* Should we issue a runtime error? */
         HB_SOCKET_SET_ERROR2( Socket, -1, "Passed buffer is smaller than specified requested length!" );
         hb_retni( -1 );
         return;
      }
   }

   Buffer = pBuffer->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   iLen = recv( Socket->com, Buffer, iLen, MSG_NOSIGNAL | MSG_WAITALL );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

   hb_retni( iLen );

   if ( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Data stream closed" )
   }
   else if ( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }
}

HB_FUNC( INETRECVALL )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_BYREF ); // hb_param() calls hb_itemUnref()!

   char *Buffer;
   int iLen, iMax, iReceived, iBufferLen;
   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL || pBuffer == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETRECVALL", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   if( ISNIL( 3 ) )
   {
      iMax = pBuffer->item.asString.length;
   }
   else
   {
      iMax = hb_parni( 3 );

      if( (int) ( pBuffer->item.asString.length ) < iMax )
      {
         /* Should we issue a runtime error? */
         HB_SOCKET_SET_ERROR2( Socket, -1, "Passed buffer is smaller than specified requested length!" );
         hb_retni( -1 );
         return;
      }
   }

   Buffer = pBuffer->item.asString.value;

   iReceived = 0;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   do
   {
      iBufferLen = HB_SENDRECV_BUFFER_SIZE > iMax-iReceived ? iMax-iReceived : HB_SENDRECV_BUFFER_SIZE;

      iLen = recv( Socket->com, Buffer + iReceived, iBufferLen, MSG_NOSIGNAL );

      if( iLen > 0 )
      {
            iReceived += iLen;
      }
   }
   while( iReceived < iMax && iLen > 0 );

   Socket->count = iReceived;

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

   if ( iLen == 0 )
   {
      HB_SOCKET_SET_ERROR2( Socket, -1, "Data stream closed" )
   }

   if ( iLen >= 0 )
   {
      hb_retni( iReceived );
   }
   else
   {
      HB_SOCKET_SET_ERROR( Socket );

      hb_retni( -1 );
   }
}

HB_FUNC( INETRECVLINE )
{
   PHB_ITEM pSocket     = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pResult     = hb_param( 2, HB_IT_BYREF );
   PHB_ITEM pMaxSize    = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pBufferSize = hb_param( 4, HB_IT_NUMERIC );

   char cChar;
   char *Buffer;
   int iAllocated, iBufferSize, iMax;
   int iLen;
   int iPos = 0;

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETRECVLINE", 1, pArgs );
      hb_itemRelease( pArgs );
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

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

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

      iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL );

      if( iLen > 0 )
      {
         if ( cChar == '\n' && iPos && Buffer[ iPos - 1 ] == '\r' )
         {
            break;
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
         HB_SOCKET_SET_ERROR2( Socket, -1, "Data stream closed" )
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
         HB_SOCKET_SET_ERROR2( Socket, -1, "Buffer overrun" );

         if( pResult )
         {
            hb_itemPutNL( pResult, -2 );
         }

         hb_xfree( (void *) Buffer );
         hb_retc( NULL );
      }
   }

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETDATAREADY )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );
   int iLen;
   HB_SOCKET_STRUCT *Socket;

   #if defined( HB_OS_WIN_32 )
      fd_set rfds;
      struct timeval tv = {0,0};
      int nRetval;
   #else
      char cChar;
      char *Buffer;
   #endif

   if( pSocket == NULL || ( hb_pcount() == 2 && ! ISNUM(2)) )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDATAREADY", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   #if defined( HB_OS_WIN_32 )
      /* Watch our socket. */
      FD_ZERO(&rfds);
      FD_SET(Socket->com, &rfds);
      nRetval = select(1, &rfds, NULL, NULL, &tv);
      /* Don't rely on the value of tv now! */

      if ( nRetval )
      {
         iLen = 1;
      }
      else
      {
         iLen = 0;
      }
   #else
      if( hb_pcount() == 2 )
      {
         iLen = hb_parni(2);
         Buffer = (char *) hb_xgrab( iLen );
         iLen = recv( Socket->com, Buffer, iLen, MSG_NOSIGNAL | MSG_PEEK | MSG_DONTWAIT );
         hb_xfree( (void *) Buffer );
      }
      else
      {
         iLen = recv( Socket->com, &cChar, 1, MSG_NOSIGNAL | MSG_PEEK | MSG_DONTWAIT );
      }
   #endif

   if( iLen < 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }

   hb_retni( iLen );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETSEND )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING ), pBuffer = hb_param( 2, HB_IT_STRING );

   char *Buffer;
   int iLen;
   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL || pBuffer == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSEND", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   Buffer = pBuffer->item.asString.value;

   if ( ISNUM( 3 ) )
   {
      iLen = hb_parni( 3 );
   }
   else
   {
      iLen = pBuffer->item.asString.length;
   }

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   Socket->count = send( Socket->com, Buffer, iLen, MSG_NOSIGNAL );

   if( Socket->count <= 0 )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }

   hb_retni( Socket->count );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETSENDALL )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING ), pBuffer = hb_param( 2, HB_IT_STRING );

   char *Buffer;
   int iLen, iSent, iSend, iBufferLen;
   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL || pBuffer == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETSEND", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;
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

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   while( iSent < iSend )
   {
      iBufferLen = HB_SENDRECV_BUFFER_SIZE > iSend - iSent ? iSend - iSent : HB_SENDRECV_BUFFER_SIZE;

      iLen = send( Socket->com, Buffer + iSent, iBufferLen, MSG_NOSIGNAL );

      if( iLen > 0 )
      {
         iSent += iLen;
      }
      else
      {
         HB_SOCKET_SET_ERROR( Socket );
         break;
      }
   }

   Socket->count = iSent;

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

   if ( iLen > 0 )
   {
      hb_retni( iSent );
   }
   else
   {
      hb_retni( -1 );
   }
}

HB_FUNC( INETCLOSE )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );
   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCLOSE", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   if( Socket->com )
   {
      #if defined( HB_OS_WIN_32 )
         shutdown( Socket->com, SD_BOTH );
      #else
         shutdown( Socket->com, SHUT_RDWR );
      #endif

      hb_retni( HB_INET_CLOSE( Socket->com ) );

      Socket->com = 0;
   }
   else
   {
      hb_retni( -1 );
   }

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETDESTROY )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETDESTROY", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   if ( Socket->com > 0 )
   {
      #if defined( HB_OS_WIN_32 )
          shutdown( Socket->com, SD_BOTH );
      #else
          shutdown( Socket->com, SHUT_RDWR );
      #endif

      HB_INET_CLOSE( Socket->com );
   }

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

   HB_SOCKET_FREE( Socket );

   hb_ret();
}

HB_FUNC( INETERRORCODE )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETERRORCODE", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   hb_retni( Socket->errorCode );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETERRORDESC )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETERRORDESC", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   hb_retc( Socket->errorDesc );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETCLEARERROR )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETERRORDESC", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   HB_SOCKET_ZERO_ERROR( Socket );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETCOUNT )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCOUNT", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   hb_retni( Socket->count );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );
}

HB_FUNC( INETADDRESS )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;
   char addr[24];

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETADDRESS", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   //HB_CRITICAL_INET_LOCK( Socket->Mutex );

   sprintf( addr, "%d.%d.%d.%d",
      (int) (Socket->remote.sin_addr.s_addr & 0x000000ff),
      (int) (( Socket->remote.sin_addr.s_addr & 0x0000ff00)>>8),
      (int) (( Socket->remote.sin_addr.s_addr & 0x00ff0000) >>16),
      (int) (Socket->remote.sin_addr.s_addr >> 24)
   );

   //HB_CRITICAL_INET_UNLOCK( Socket->Mutex );

   hb_retc( addr );
}

HB_FUNC( INETPORT )
{
   PHB_ITEM pSocket = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;
   int iPort;

   if( pSocket == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETPORT", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   Socket = (HB_SOCKET_STRUCT *) pSocket->item.asString.value;

   iPort = (Socket->remote.sin_port & 0xff00) >>8 | (Socket->remote.sin_port & 0x00ff) << 8;

   hb_retni( iPort );
}

HB_FUNC( INETCONNECT )
{
   PHB_ITEM pHost = hb_param( 1, HB_IT_STRING );

   HB_SOCKET_STRUCT *Socket;
   int iPort, iPort1;
   struct hostent *Host;
   int iOpt = 1;

   if( pHost == NULL )
   {
      PHB_ITEM pArgs = hb_arrayFromParams( HB_VM_STACK.pBase );
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, "INETCONNECT", 1, pArgs );
      hb_itemRelease( pArgs );
      return;
   }

   HB_SOCKET_INIT( Socket );

   /* Creates comm socket */
   #if defined(HB_OS_WIN_32)
      Socket->com = socket( AF_INET, SOCK_STREAM, 0);
   #else
      Socket->com = socket( PF_INET, SOCK_STREAM, 0);
   #endif

   setsockopt( Socket->com, SOL_SOCKET, SO_KEEPALIVE, (const char *) &iOpt , sizeof( iOpt ));

   if( Socket->com == -1 )
   {
      HB_SOCKET_SET_ERROR( Socket );
      hb_retclenAdoptRaw( (char *) Socket, sizeof( HB_SOCKET_STRUCT ) );
      return;
   }

   Host = gethostbyname( pHost->item.asString.value );

   if( Host == NULL )
   {
      #if defined(HB_OS_WIN_32)
         HB_SOCKET_SET_ERROR2( Socket, WSAGetLastError() , "Generic error in GetHostByName()" );
         WSASetLastError( 0 );
      #else
         HB_SOCKET_SET_ERROR2( Socket, h_errno, (char *) hstrerror( h_errno ) );
      #endif

      hb_retclenAdoptRaw( (char *) Socket, sizeof( HB_SOCKET_STRUCT ) );

      return;
   }

   iPort = hb_parni( 2 );

   iPort1 = ( iPort & 0xff00) >>8 | ( iPort & 0x00ff ) << 8;

   Socket->remote.sin_family = AF_INET;
   Socket->remote.sin_port= iPort1;
   Socket->remote.sin_addr.s_addr =
      ((unsigned char ) Host->h_addr_list[0][0]) |
      ( (unsigned int) ( (unsigned char ) Host->h_addr_list[0][1] ) ) << 8 |
      ( (unsigned int) ( (unsigned char ) Host->h_addr_list[0][2] ) ) << 16 |
      ( (unsigned int) ( (unsigned char ) Host->h_addr_list[0][3] ) ) << 24;

   if( connect( Socket->com, (struct sockaddr *) &Socket->remote, sizeof(Socket->remote) ) )
   {
      HB_SOCKET_SET_ERROR( Socket );
   }

   hb_retclenAdoptRaw( (char *) Socket, sizeof( HB_SOCKET_STRUCT ) );
}

HB_FUNC( INETCRLF )
{
   hb_retc( "\r\n" );
}

#endif
