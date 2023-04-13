/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Remote Procedure Call code
 * Client class
 *
 * Copyright 2003 Giancarlo Niccolai <giancarlo@niccolai.ws>
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

#include "hbrpc.ch"
#include "common.ch"

PROCEDURE HB_TRPCCLIENT_ASYNC( self )

#ifdef HB_THREAD_SUPPORT

   IF ::lAsyncMode
      hb_mutexLock( ::mtxBusy )
      ::thTCPAccept := StartThread( Self, "TCPAccept" )
      hb_mutexUnlock( ::mtxBusy )
   ELSE
      ::TCPAccept()
   ENDIF
#else
   ::TCPAccept()
#endif

   RETURN

PROCEDURE HB_TRPCCLIENT_STOPCALL( self )

#ifdef HB_THREAD_SUPPORT

   hb_mutexLock( ::mtxBusy )
   IF IsValidThread( ::thTCPAccept )
      KillThread( ::thTCPAccept )
      ::thTCPAccept := NIL
      ::nStatus := RPC_STATUS_LOGGED
      hb_mutexUnlock( ::mtxBusy )
      ::OnFunctionReturn( NIL )
   ELSE
      hb_mutexUnlock( ::mtxBusy )
   ENDIF
#else
   ::nStatus := RPC_STATUS_LOGGED
   ::OnFunctionReturn( NIL )
#endif

   RETURN

FUNCTION HB_TRPCCLIENT_CALL( self )

#ifdef HB_THREAD_SUPPORT

   hb_mutexLock( ::mtxBusy )
// already active or not already connected
   IF IsValidThread( ::thTcpAccept ) .OR. ::skTCP == NIL .OR. ::nStatus < RPC_STATUS_LOGGED
      hb_mutexUnlock( ::mtxBusy )
      RETURN NIL
   ENDIF
   hb_mutexUnlock( ::mtxBusy )
#else
   IF ::skTCP == NIL .OR. ::nStatus < RPC_STATUS_LOGGED
      RETURN NIL
   ENDIF
#endif

   RETURN NIL

PROCEDURE HB_TRPCCLIENT_STOPSCAN( self )

#ifdef HB_THREAD_SUPPORT

   hb_mutexLock( ::mtxBusy )
   IF IsValidThread( ::thUDPAccept )
      KillThread( ::thUDPAccept )
      ::thUDPAccept := NIL
      hb_mutexUnlock( ::mtxBusy )
      ::OnScanComplete()
   ELSE
      hb_mutexUnlock( ::mtxBusy )
   ENDIF
#else
   ::OnScanComplete()
#endif

   RETURN

PROCEDURE HB_TRPCCLIENT_STARTSCAN( self )

#ifdef HB_THREAD_SUPPORT

   IF ::lAsyncMode
      hb_mutexLock( ::mtxBusy )
      ::thUdpAccept := StartThread( Self, "UDPAccept" )
      hb_mutexUnlock( ::mtxBusy )
   ELSE
      ::UDPAccept()
   ENDIF
#else
   ::UDPAccept()
#endif

   RETURN

PROCEDURE HB_TRPCCLIENT_DESTROY( self )

#ifdef HB_THREAD_SUPPORT

   IF IsValidThread( ::thUdpAccept )
      KillThread( ::thUdpAccept )
      ::thUdpAccept := NIL
   ENDIF
   IF IsValidThread( ::thTcpAccept )
      KillThread( ::thTcpAccept )
      ::thTcpAccept := NIL
   ENDIF
#else
   HB_SYMBOL_UNUSED( self )
#endif

   RETURN

