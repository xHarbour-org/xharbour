/*
 * $Id: objfunc.prg,v 1.2 2002/10/04 18:24:25 ronpinkas Exp $
 */

/*
 * Harbour Project source code:
 * Dynamic Object management and misc. Object related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
 * Copyright 2002 Ron Pinkas <ron@ronpinkas.com>
 *    __objGetMsgList
 *    __objGetValueList
 *
 * Copyright 2000 Jf. Lefebvre <jfl@mafact.com> and Ra. Cuylen <rac@mafact.com>
 *    __objDerivedFrom
 *
 * New Param for Method :ClassSel() to allow it to return only ClassData array
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "common.ch"
#include "error.ch"
#include "hboo.ch"

FUNCTION __objHasData( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

RETURN __objHasMsg( oObject, cSymbol ) .AND. __objHasMsg( oObject, "_" + cSymbol )

FUNCTION __objHasMethod( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

RETURN __objHasMsg( oObject, cSymbol ) .AND. ! __objHasMsg( oObject, "_" + cSymbol )

// nCLassType can be 0, 1 or 2 see hbOO.ch
//#define HB_MSGLISTALL   0
//#define HB_MSGLISTCLASS 1
//#define HB_MSGLISTPURE  2

FUNCTION __objGetMsgList( oObject, lData, nRange, nScope )

   LOCAL aMessages
   LOCAL aReturn
   LOCAL nFirstProperty, cMsg

   IF ValType( oObject ) != 'O'
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName() )
   ENDIF

   IF ValType( lData ) != 'L'
      lData := .T.
   ENDIF

   // nRange is already defaulted in ClassSel in classes.c

   aMessages := ASort( oObject:ClassSel( nRange, nScope ) )
   aReturn   := {}

   nFirstProperty := aScan( aMessages, { | cElement | cElement[1] == '_' } )

   FOR EACH cMsg IN aMessages
      IF cMsg[1] = '_'
         EXIT
      ENDIF

      IF ( AScan( aMessages, { | cElement | cElement == "_" + cMsg }, nFirstProperty ) != 0 ) == lData
         AAdd( aReturn, cMsg )
      ENDIF
   NEXT

RETURN aReturn

FUNCTION __objGetMethodList( oObject, nScope )

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

RETURN __objGetMsgList( oObject, .F., HB_MSGLISTALL, nScope )

FUNCTION __objGetValueList( oObject, aExcept, nScope )

   LOCAL aVars
   LOCAL aReturn
   LOCAL cVar

   IF ValType( oObject ) != 'O'
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF ValType( aExcept ) != 'A'
      aExcept := {}
   ENDIF

   aVars   := __objGetMsgList( oObject, .T., HB_MSGLISTALL, nScope )
   aReturn := {}

   FOR EACH cVar IN aVars
      IF AScan( aExcept, { | cElement | cElement == cVar } ) == 0
         AAdd( aReturn, { cVar, __objSendMsg( oObject, cVar ) } )
      ENDIF
   NEXT

RETURN aReturn

FUNCTION __ObjSetValueList( oObject, aData )

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSE
      aEval( aData, { | aItem | __objSendMsg( oObject, "_" + aItem[ HB_OO_DATA_SYMBOL ], aItem[ HB_OO_DATA_VALUE ] ) } )
   ENDIF

RETURN oObject

FUNCTION __objAddMethod( oObject, cSymbol, nFuncPtr )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISNUMBER( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, nFuncPtr, HB_OO_MSG_METHOD, NIL, 1 )
   ENDIF

RETURN oObject

FUNCTION __objAddInline( oObject, cSymbol, bInline )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol )
      __clsAddMsg( oObject:ClassH, cSymbol, bInline, HB_OO_MSG_INLINE, NIL, 1 )
   ENDIF

RETURN oObject

FUNCTION __objAddData( oObject, cSymbol, nScope )

   LOCAL nSeq, hClass

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF !__objHasMsg( oObject, cSymbol ) .AND. !__objHasMsg( oObject, "_" + cSymbol )
      hClass := oObject:ClassH
      nSeq   := __cls_IncData( hClass )         // Allocate new Seq#

      IF nScope == NIL
         nScope := HB_OO_CLSTP_EXPORTED
      ENDIF

      __clsAddMsg( hClass,       cSymbol, nSeq, HB_OO_MSG_DATA, NIL, nScope )
      __clsAddMsg( hClass, "_" + cSymbol, nSeq, HB_OO_MSG_DATA, NIL, nScope )
   ENDIF

RETURN oObject

FUNCTION __objModMethod( oObject, cSymbol, nFuncPtr )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISNUMBER( nFuncPtr )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, nFuncPtr )
   ENDIF

RETURN oObject

FUNCTION __objModInline( oObject, cSymbol, bInline )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol ) .OR. !ISBLOCK( bInline )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsModMsg( oObject:ClassH, cSymbol, bInline )
   ENDIF

RETURN oObject

FUNCTION __objDelMethod( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasMethod( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
   ENDIF

RETURN oObject

FUNCTION __objDelInline( oObject, cSymbol )

RETURN __objDelMethod( oObject, cSymbol )              // Same story

FUNCTION __objDelData( oObject, cSymbol )

   IF !ISOBJECT( oObject ) .OR. !ISCHARACTER( cSymbol )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ELSEIF __objHasData( oObject, cSymbol )
      __clsDelMsg( oObject:ClassH, cSymbol )
      __clsDelMsg( oObject:ClassH, "_" + cSymbol )
      __cls_DecData( oObject:ClassH )         // Decrease wData
   ENDIF

RETURN oObject

FUNCTION __objDerivedFrom( oObject, xSuper )
   LOCAL cClassName

   IF !ISOBJECT( oObject )
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

   IF ISOBJECT( xSuper )
      cClassName := xSuper:ClassName()
   ELSEIF ISCHARACTER( xSuper )
      cClassName := Upper( xSuper )
   ELSE
      __errRT_BASE( EG_ARG, 3101, NIL, ProcName( 0 ) )
   ENDIF

RETURN __clsParent( oObject:ClassH, cClassName )
