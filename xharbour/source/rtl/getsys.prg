/*
 * $Id: getsys.prg,v 1.10 2003/01/27 03:57:24 walito Exp $
 */

/*
 * Harbour Project source code:
 * GET system module (default)
 *
 * Copyright 1999-2001 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik
 *    Support for CA-Clipper 5.3 Getsystem
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "hbsetup.ch"
#include "setcurs.ch"

#ifdef HB_COMPAT_C53
FUNCTION ReadModal( GetList, nPos,;
                    oMenu, nMsgRow, nMsgLeft, nMsgRight, cMsgColor )

   LOCAL oGetMsg, cColor
#else
FUNCTION ReadModal( GetList, nPos )
#endif

   LOCAL oGetList, oSaveGetList

   IF Empty( GetList )
      SetPos( MaxRow() - 1, 0 )
      RETURN .F.
   ENDIF

   oGetList := HBGetList():New( GetList )
   oGetList:cReadProcName := ProcName( 1 )
   oGetList:nReadProcLine := ProcLine( 1 )
   #ifdef HB_COMPAT_C53
   oGetList:nSaveCursor   := SetCursor(SC_NONE)
   #endif

   oSaveGetList := __GetListActive( )
   __GetListSetActive( oGetList )
   __GetListLast( oGetList )

#ifdef HB_COMPAT_C53

   IF ISNUMBER( nPos )
      oGetList:nPos := oGetList:Settle( nPos, TRUE )
   ELSE
      oGetList:nPos := oGetList:Settle(    0, TRUE )
   ENDIF

   oGetMsg := GetMssgLine():new( nMsgRow, nMsgLeft, nMsgRight, cMsgColor )

   if oGetMsg:Flag
      cColor := setColor( oGetMsg:Color )
      @ oGetMsg:row, oGetMsg:left CLEAR TO oGetMsg:row, oGetMsg:right
      setColor( cColor )
      oGetMsg:saveScreen()
   endif

   oGetList:nNextGet := 0
   oGetList:nHitCode := 0
   oGetList:nMenuID  := 0

#else

   IF ! ( ISNUMBER( nPos ) .AND. nPos > 0 )
      oGetList:nPos := oGetList:Settle( 0 )
   ENDIF

#endif

   WHILE oGetList:nPos != 0

      oGetList:oGet := oGetList:aGetList[ oGetList:nPos ]
      oGetList:PostActiveGet()

/*
#ifdef HB_COMPAT_C53
      if oGetMsg:Flag
         oGet := oGetList:aGetList[ oGetList:nPos ]
         oGetMsg:Show( oGet )
      endif
#endif
*/

      IF ISBLOCK( oGetList:oGet:Reader )

#ifdef HB_COMPAT_C53
         Eval( oGetList:oGet:Reader, oGetList:oGet ,oGetlist, oMenu, oGetMsg )
      ELSE
         oGetList:Reader( oMenu, oGetMsg )
      ENDIF

      oGetList:nPos := oGetList:Settle( , FALSE )
#else
         Eval( oGetList:oGet:Reader, oGetList:oGet )
      ELSE
         oGetList:Reader()
      ENDIF

      oGetList:nPos := oGetList:Settle()
#endif

   ENDDO

/*
#ifdef HB_COMPAT_C53
   if oGetMsg:Flag
      oGetMsg:restScreen()
   endif
#endif
*/

   __GetListSetActive( oSaveGetList )

   SetPos( MaxRow() - 1, 0 )

#ifdef HB_COMPAT_C53
   SetCursor(oGetList:nSaveCursor)
#endif

   RETURN oGetList:lUpdated

PROCEDURE GetReader( oGet )
   oGet:Reader()

   RETURN

FUNCTION GetActive( oGet )

   STATIC oDefaultGet

   LOCAL oGetList := __GetListActive()

   IF oGetList == NIL
      IF PCount() >= 1
         oDefaultGet := oGet
      ENDIF

      RETURN oDefaultGet
   ELSE
      IF PCount() >= 1
         RETURN oGetList:GetActive( oGet )
      ELSE
         RETURN oGetList:GetActive()
      ENDIF
   ENDIF

RETURN NIL

PROCEDURE GetDoSetKey( keyBlock, oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      oGetList:GetDoSetKey( keyBlock )
   ENDIF

   RETURN

PROCEDURE GetApplyKey( oGet, nKey )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      oGetList:GetApplyKey( nKey )
   ENDIF

   RETURN

FUNCTION GetPreValidate( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GetPreValidate()
   ENDIF

   RETURN .F.

FUNCTION GetPostValidate( oGet )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GetPostValidate()
   ENDIF

   RETURN .F.

FUNCTION ReadExit( lExit )
   RETURN IF( ISLOGICAL( lExit ), Set( _SET_EXIT, lExit ), Set( _SET_EXIT ) )

FUNCTION ReadInsert( lInsert )
   RETURN IF( ISLOGICAL( lInsert ), Set( _SET_INSERT, lInsert ), Set( _SET_INSERT ) )

FUNCTION ReadUpdated( lUpdated )
/*   LOCAL oGetList := __GetListActive() */
   LOCAL oGetList := __GetListLast()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:ReadUpdated( lUpdated )
      ELSE
         RETURN oGetList:ReadUpdated()
      ENDIF
   ENDIF

   RETURN .F.

FUNCTION Updated()
/*   LOCAL oGetList := __GetListActive() */
   LOCAL oGetList := __GetListLast()

   IF oGetList != NIL
      RETURN oGetList:lUpdated
   ENDIF

   RETURN .F.

FUNCTION ReadKill( lKill )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:KillRead( lKill )
      ELSE
         RETURN oGetList:KillRead()
      ENDIF
   ENDIF

   RETURN .F.

PROCEDURE __KillRead()
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      oGetList:KillRead( .T. )
   ENDIF

   RETURN

PROCEDURE __SetFormat( bFormat )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF ISBLOCK( bFormat )
         oGetList:SetFormat( bFormat, TRUE )
      ELSE
         oGetList:SetFormat( , TRUE )
      ENDIF
   ENDIF

   RETURN

FUNCTION ReadFormat( bFormat )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF PCount() >= 1
         RETURN oGetList:SetFormat( bFormat, TRUE )
      ELSE
         RETURN oGetList:SetFormat( , FALSE )
      ENDIF
   ENDIF

   RETURN NIL

#define SCORE_ROW       0
#define SCORE_COL       60

#define _GET_RANGE_FROM 10
#define _GET_RANGE_TO   11

FUNCTION RangeCheck( oGet, xDummy, xLow, xHigh )
   LOCAL xValue
   LOCAL cMessage
   LOCAL nOldRow, nOldCol

   IF !oGet:changed
      RETURN .T.
   ENDIF

   xValue := oGet:varGet()

   IF xValue >= xLow .AND. xValue <= xHigh
      RETURN .T.
   ENDIF

   IF Set( _SET_SCOREBOARD )

      cMessage := Left( NationMsg( _GET_RANGE_FROM ) + LTrim( Transform( xLow, "" ) ) + ;
                        NationMsg( _GET_RANGE_TO ) + LTrim( Transform( xHigh, "" ) ), MaxCol() )

      HBConsoleLock()
      nOldRow := Row()
      nOldCol := Col()

      DispOutAt( SCORE_ROW, Min( 60, MaxCol() - Len( cMessage ) ), cMessage )
      SetPos( nOldRow, nOldCol )
      HBConsoleUnlock()

      DO WHILE NextKey() == 0
      ENDDO

      HBConsoleLock()
      DispOutAt( SCORE_ROW, Min( 60, MaxCol() - Len( cMessage ) ), Space( Len( cMessage ) ) )
      SetPos( nOldRow, nOldCol )
      HBConsoleUnlock()

   ENDIF

   RETURN .F.

#ifdef HB_COMPAT_C53

PROCEDURE GUIReader( oGet, oGetlist, oMenu, oGetMsg )

   oGetlist:GuiReader( oGet, oMenu, oGetMsg )

   RETURN

PROCEDURE GuiApplyKey( oGet, nKey, oMenu, oGetMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      oGetList:GUIApplyKey( oGet:control, nKey, oMenu, oGetMsg )
   ENDIF

   RETURN

FUNCTION GuiGetPreValidate( oGet, oGui, oGetMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GetPreValidate( oGui, oGetMsg )
   ENDIF

   RETURN .F.

FUNCTION GuiGetPostValidate( oGet, oGui, oGetMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF

      RETURN oGetList:GuiGetPostValidate( oGui, oGetMsg )
   ENDIF

   RETURN .F.


PROCEDURE TBReader( oGet, oGetList, oMenu, oGetMsg )

   oGetList:TBReader( oGet, oMenu, oGetMsg )

   RETURN

PROCEDURE TBApplyKey( oGet, oTB, GetList, nKey,  oGetMsg )
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      IF oGet != NIL
         oGetList:oGet := oGet
      ENDIF
      oGetList:Tbapplykey( oGet, oTB, nKey, oGetMsg )
   ENDIF
   RETURN

FUNCTION HitTest( aGetList, MouseRow, MouseCol, aMsg ) // Removed STATIC
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      RETURN oGetlist:Hittest( aGetList, MouseRow, MouseCol, aMsg ) // Removed STATIC
   ENDIF
   RETURN 0

/***
*
*  Accelerator( <aGetList>, <nKey>, <aMsg> ) --> 0
*
*  Identify the Accelerator key
*
***/
FUNCTION Accelerator( aGetList, nKey, aMsg ) // Removed STATIC
   LOCAL oGetList := __GetListActive()

   IF oGetList != NIL
      RETURN oGetlist:Accelerator( aGetList, nKey, aMsg ) // Removed STATIC
   ENDIF
   RETURN 0

#endif

