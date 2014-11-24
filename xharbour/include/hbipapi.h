/*  $Id$  */

/*
 * Harbour Project source code:
 * Header file for Leto RDD and Server
 *
 * Copyright 2008 Alexander S. Kresin <alex / at / belacy.belgorod.su>
 * www - http://www.harbour-project.org
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

#if defined( HB_OS_WIN ) && !defined( WIN32 )
   #define WIN32
#endif
#if !defined( unix ) && ( defined( __LINUX__ ) || defined( HB_OS_LINUX ) )
   #define unix
#endif

#if defined( __WATCOMC__ ) || defined( __LCC__ )
   #define _declspec( dllexport ) __declspec( dllexport )
#endif

#define HB_SENDRECV_BUFFER_SIZE         32767
#if defined( HB_OS_WIN )
   #define HB_SOCKET_T SOCKET
   #undef _WINSOCKAPI_
   #include <winsock2.h>
   #include "cinterface.h"
   #include <windows.h>   
#else
   #define HB_SOCKET_T int
#endif

extern int hb_ipDataReady( HB_SOCKET_T hSocket, int timeout );
extern int hb_ipRecv( HB_SOCKET_T hSocket, char *Buffer, int iMaxLen );
extern int hb_ipSend( HB_SOCKET_T hSocket, char *Buffer, int iSend, int timeout );
extern HB_SOCKET_T hb_ipConnect( const char * szHost, int iPort, int timeout );
extern HB_SOCKET_T hb_ipServer( int iPort, const char * szAddress, int iListen );
extern HB_SOCKET_T hb_ipAccept( HB_SOCKET_T hSocket, int timeout, char * szAddr, long int * lPort );
extern void   hb_ipInit( void );
extern void   hb_ipCleanup( void );
extern int    hb_iperrorcode( void );
extern char * hb_ipErrorDesc( void );
extern void   hb_ipclose( HB_SOCKET_T hSocket );
extern void   hb_ip_rfd_zero( void);
extern void   hb_ip_rfd_clr( HB_SOCKET_T hSocket );
extern void   hb_ip_rfd_set( HB_SOCKET_T hSocket );
extern BOOL   hb_ip_rfd_isset( HB_SOCKET_T hSocket );
extern int    hb_ip_rfd_select( int iTimeOut );
