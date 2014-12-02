/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * TIMETOSEC(), SECTOTIME(), MILLISEC()
 *
 * Copyright 2003 Piero Vincenzo Lupano <pierovincenzo1956@supereva.it>
 * Copyright 2003 Przemyslaw Czerpak <druzus@acn.waw.pl>
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

FUNCTION TIMETOSEC( cTime )

   LOCAL nSec, nLen, nVal

   nSec := Seconds()

/* NOTE: In CA-Clipper Tools, timetosec() is limited to HH:MM:SS:hh (hundredth).
         In xHarbour, timetosec() is extended to HH:MM:SS:ttt (thousandth).
 */
   IF HB_ISSTRING( cTime )

      nSec := 0
      nLen := Len( cTime )

      IF Timevalid( cTime )

         IF nLen >= 2       // HH
            nVal := Val( SubStr( cTime,1,2 ) )  // hours
            nSec += nVal * 3600
         ENDIF

         IF nLen >= 5       // HH:MM
            nVal := Val( SubStr( cTime,4,2 ) )  // minutes
            nSec += nVal * 60
         ENDIF

         IF nLen >= 8       // HH:MM:SS
            nVal := Val( SubStr( cTime,7,2 ) )  // seconds
            nSec += nVal
         ENDIF

         IF nLen == 11      // HH:MM:SS:hh
            nVal := Val( SubStr( cTime,10,2 ) ) // hundredth
            nSec += nVal / 100
         ELSEIF nLen == 12  // HH:MM:SS:ttt
            nVal := Val( SubStr( cTime,10,3 ) ) // thousandth
            nSec += nVal / 1000
         ENDIF

      ELSE
         nSec := - 1  // Clipper compliant.
      ENDIF

   ENDIF

   RETURN Round( nSec, iif( nSec - Int(nSec ) > 0, iif(nLen == 12, 3, 2 ), 0 ) )

FUNCTION SECTOTIME( nSec, lHundr, lThous )

   LOCAL i, h, n

   n := iif( !HB_ISNUMERIC( nSec ), Seconds(), nSec )
   h := ""
   IF HB_ISLOGICAL( lHundr ) .AND. lHundr
      h := StrZero( ( nSec * 100 ) % 100, 2 )
   ENDIF
/* NOTE: In CA-Clipper Tools, sectotime() is limited to HH:MM:SS:hh (hundredth).
         In xHarbour, sectotime() is extended to HH:MM:SS:ttt (thousandth).
 */
   IF HB_ISLOGICAL( lThous ) .AND. lThous
      h := StrZero( ( nSec * 1000 ) % 1000, 3 )
   ENDIF
   n := Int( n % 86400 )
   FOR i := 1 TO 3
      h := StrZero( n % 60, 2 ) + iif( Len( h ) == 0, "", ":" ) + h
      n := Int( n / 60 )
   NEXT

   RETURN h

FUNCTION MILLISEC( nDelay )

   SECONDSSLEEP( nDelay / 1000 )

   RETURN ""
