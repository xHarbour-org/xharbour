/*
* $Id$
*/

/*
* xHarbour Project source code:
* Versatile logging system - Logger sending log message to e-mail
*
* Copyright 2003 Giancarlo Niccolai [gian@niccolai.ws]
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

PROCEDURE HB_LOGOPEN( oLog )

#ifdef HB_THREAD_SUPPORT

   oLog:mtxBusy := hb_mutexCreate()
   oLog:nThread := StartThread( oLog, "AcceptCon" )
#else
// If we have not threads, we have to sync accept incoming connection
// when we log a message
   InetSetTimeout( oLog:skIn, 50 )
#endif

   RETURN

PROCEDURE HB_LOGACCEPTCON( oLog )

   LOCAL sk

#ifdef HB_THREAD_SUPPORT

   InetSetTimeout( oLog:skIn, 250 )
   DO WHILE .NOT. oLog:bTerminate
      sk := InetAccept( oLog:skIn )
      // A gentle termination request, or an error
      IF sk != NIL
         hb_mutexLock( oLog:mtxBusy )
         AAdd( oLog:aListeners, sk )
         hb_mutexUnlock( oLog:mtxBusy )
      ENDIF
   ENDDO
#else
   sk := InetAccept( oLog:skIn )
// A gentle termination request, or an error
   IF sk != NIL
      AAdd( oLog:aListeners, sk )
      IF oLog:bOnConnect <> NIL
         Eval( oLog:bOnConnect, sk )
      ENDIF
   ENDIF
#endif

   RETURN

PROCEDURE HB_LOGBROADCASTMESSAGE( oLog )

#ifdef HB_THREAD_SUPPORT

// be sure thread is not busy now
   hb_mutexLock( oLog:mtxBusy )
#else
   oLog:AcceptCon()
#endif

   RETURN
