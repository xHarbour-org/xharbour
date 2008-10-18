/*
 * $Id: tobject.prg,v 1.27 2007/05/08 13:36:40 andresreyesh Exp $
 */

/*
 * Harbour Project source code:
 * Base Object from wich all object finally inherit
 *
 * Copyright 2000 JfL&RaC <jfl@mafact.com>, <rac@mafact.com>
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
 * Copyright 2000 J. Lefebvre <jfl@mafact.com> & RA. Cuylen <rac@mafact.com>
 *    1.40 07/13/2000 JFL&RAC
 *    Now supporting of New and Init method as Class(y) use it
 *    So oMyObj:new(Var1, Var2) will call oMyObj:Init(Var1, Var2)
 *    Currently limited to 20 params
 *
 *    1.41 07/18/2000 JFL&RAC
 *    Improving class(y) compatibility
 *    adding messages :error() and ::MsgNotFound()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_CLS_NOTOBJECT

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"

//----------------------------------------------------------------------------//
/* Work Like Class(y)) */

CLASS HBObject

   MESSAGE New          IS Init
   METHOD Init          INLINE   Self

   // methods to return string representation of object.  asExpStr is
   // dynamically mapped to asString so subclasses don't need to define both
   // unless asExpStr result differs from asString, as in the scalar classes.

   METHOD asString      INLINE   ::ClassName + " Object"
   METHOD asExpStr      INLINE   ::asString()

   METHOD basicSize     INLINE   Len( Self )

   /* This Three Not Needed C version from classes.c are probably better in term of speed */
   /*
   METHOD ClassName     INLINE __ObjGetClsName( Self )
   METHOD ClassH        INLINE __ClassH( Self )
   METHOD ClassSel      INLINE __ClassSel( Self:ClassH )
   */

   METHOD Copy
   METHOD deepCopy

   METHOD isEqual( o )  INLINE Self == o

   METHOD isKindOf

   // ScalarObject class overrides this
   METHOD isScalar      INLINE (Self), .F.

   METHOD Eval          VIRTUAL
   METHOD Exec          VIRTUAL

   METHOD Error

   ERROR HANDLER ErrorHandler

   METHOD MsgNotFound

   /* Begin xBase++ */
   MESSAGE isDerivedFrom   METHOD isKindOf


ENDCLASS

//----------------------------------------------------------------------------//
/*
    copy()

    RETURN a copy of the receiver which shares the receiver's instance
    variables.
*/

METHOD Copy() CLASS HBObject
   LOCAL NewSelf := __clsInst( ::ClassH )
   LOCAL xItem

   FOR EACH xItem IN Self
      NewSelf[ HB_EnumIndex() ] := Self[ HB_EnumIndex() ]
   NEXT

RETURN NewSelf

//----------------------------------------------------------------------------//
/*
    deepCopy()

    RETURN a copy of the receiver with shallow copies of each instance
*/

METHOD DeepCopy() CLASS HBObject
RETURN __objClone( self )

//----------------------------------------------------------------------------//

METHOD IsKindOf( o ) CLASS HBObject
   RETURN __ObjDerivedFrom( Self, o )

//----------------------------------------------------------------------------//

METHOD Error( cDesc, cClassName, cMsg, nSubCode, aArgs ) CLASS HBObject

   LOCAL nGenCode   := EG_NOMETHOD

   DEFAULT nSubCode TO 1004

   IF nSubCode == 1005
     nGenCode := EG_NOVARMETHOD
   ENDIF

   RETURN EVAL( ErrorBlock(), ErrorNew( "BASE", nGenCode, nSubCode, cClassName + ":" + cMsg, cDesc,;
                   aArgs, ProcFile(3), ProcName(3), ProcLine(3) ) )

//----------------------------------------------------------------------------//

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbstack.h"
#include "hbvm.h"

HB_FUNC_STATIC( HBOBJECT_ERRORHANDLER )
{
   PHB_ITEM pBase    = hb_stackBaseItem();
   PHB_DYNS pDynSym  = hb_dynsymGetCase( "MSGNOTFOUND" );
   USHORT   uiPCount = hb_pcount(), i;

   hb_vmPushSymbol( pDynSym->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmPushString( pBase->item.asSymbol.value->szName, strlen(pBase->item.asSymbol.value->szName) );

   for( i = 1; i <= uiPCount; i++ )
   {
      hb_vmPush( hb_stackItemFromBase( i ) );
   }

   hb_vmSend( uiPCount + 1 );
}

#pragma ENDDUMP

//----------------------------------------------------------------------------//

METHOD MsgNotFound( cMsg ) CLASS HBObject
   RETURN ::Error( "Message not found", __OBJGETCLSNAME( Self ), cMsg, if(substr(cMsg,1,1)=="_",1005,1004) )


FUNCTION TAssociativeArray( ... )
   LOCAL hHash

   IF PCount() == 0
      TraceLog( "Warning! TAssociativeArray() has been superceded by Hash(). Please correct your sources." )
      hHash := Hash()
      HSetAACompatibility( hHash, .T. )
      RETURN hHash
   ENDIF

RETURN Throw( ErrorNew( "TObject", 0, 1001, ProcName(), "No longer supported, please use Hash().", HB_aParams() ) )

#if 0

FUNCTION TAssociativeArray( aInit, lCase )

   LOCAL hClass
   LOCAL aMember, nSeq := 1
   LOCAL aKeys := {}
   LOCAL lCaseSensitive := .T.

   IF ValType( lCase ) == "L"
      lCaseSensitive := lCase
   ENDIF

   // Intentionally creating NEW Class for every instance - Don't change!
   IF ValType( aInit ) == 'A'
      hClass := __clsNew( "TASSOCIATIVEARRAY", Len( aInit ), 1 )

      aKeys := Array(Len( aInit ))

      FOR EACH aMember IN aInit
         __clsAddMsg( hClass, aMember[1], nSeq++, HB_OO_MSG_PROPERTY, aMember[2], HB_OO_CLSTP_EXPORTED, .T., .T. )
//         aAdd( aKeys, aMember[1] )
         aKeys[HB_EnumIndex()] := aMember[1]
      NEXT
   ELSE
      hClass := __clsNew( "TASSOCIATIVEARRAY", 0, 1 )
   ENDIF

   __clsAddMsg( hClass, "lCaseSensitive", {|Self, lNew, lOld| lOld := lCaseSensitive, IIF( lNew <> NIL, lCaseSensitive := lNew, ), lOld }, HB_OO_MSG_INLINE )
   // Intentionally using DEATCHED Local.
   __clsAddMsg( hClass, "Keys"     , {|Self, cKey | IIF( cKey == NIL, , aAdd( aKeys, cKey ) ), aKeys }, HB_OO_MSG_INLINE )

   __clsAddMsg( hClass, "SendKey"  , @TAssociativeArray_SendKey(), HB_OO_MSG_METHOD )
   __clsAddMsg( hClass, "GetKeyPos", @TAssociativeArray_GetKeyPos(), HB_OO_MSG_METHOD )
   __clsAddMsg( hClass, "GetKey"   , @TAssociativeArray_GetKey(), HB_OO_MSG_METHOD )
   __clsAddMsg( hClass, "__OnError", @TAssociativeArray_OnError(), HB_OO_MSG_ONERROR )

RETURN __clsInst( hClass )

STATIC FUNCTION TAssociativeArray_OnError( xParam )
    LOCAL Self := QSelf()
    LOCAL cMsg, cProperty
    LOCAL hClass, nSeq
    LOCAL aUpperKeys := {}
    LOCAL lCaseSensitive := ::lCaseSensitive

    cMsg := __GetMessage()
    //TraceLog( "OnError: cMsg, xParam", cMsg, xParam )
    //TraceLog( "lCaseSensitive", lCaseSensitive )
    IF !lCaseSensitive
       cMsg := Upper( cMsg )
    ENDIF

    IF cMsg[1] == '_'
       hClass    := ::ClassH
       nSeq      := __cls_IncData( hClass )

       cProperty := SubStr( cMsg, 2 )

       __clsAddMsg( hClass, cProperty, nSeq, HB_OO_MSG_PROPERTY, NIL, HB_OO_CLSTP_EXPORTED, .T., .T. )
       __ObjSendMsgCase( Self, cMsg, xParam )

       ::Keys( cProperty )
       //TraceLog( "OnError - Added MSG: cMsg, cProperty, xParam", cMsg, cProperty, xParam )
    ELSE
       IF cMsg IN ::Keys
          //TraceLog( "OnError - Query MSG: cMsg, xParam", cMsg, xParam )
          RETURN __ObjSendMsgCase( Self, cMsg )
       ELSE
          Eval( ErrorBlock(), ErrorNew( "TAssociativeArray", 1001, cMsg, "Message Not found.", HB_aParams() ) )
       ENDIF
    ENDIF

RETURN NIL

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * set/get a key within an associative array
 *
*/
STATIC FUNCTION TAssociativeArray_SendKey( cKey, xParam )
    LOCAL Self := QSelf()
    LOCAL cMsg, cProperty
    LOCAL hClass, nSeq  //, aKeys
    LOCAL xRet

    //cKey := Upper( cKey )
    //aKeys := __ObjSendMsg( Self, "Keys" )
    //TraceLog( "PCOUNT(), cKey, xParam", PCOUNT(), cKey, xParam )

    IF !( cKey IN ::Keys )
       hClass    := ::ClassH
       nSeq      := __cls_IncData( hClass )

       __clsAddMsg( hClass, cKey, nSeq, HB_OO_MSG_PROPERTY, NIL, HB_OO_CLSTP_EXPORTED, .T., .T. )
       ::Keys( cKey )
    ENDIF
    IF PCount() > 1
       xRet := __ObjSendMsgCase( Self, "_" + cKey, xParam )
    ELSE
       xRet := __ObjSendMsgCase( Self, cKey )
    ENDIF

RETURN xRet

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * get a key position within an associative array
 *
*/
STATIC FUNCTION TAssociativeArray_GetKeyPos( cKey )
RETURN aScan( QSelf():Keys, cKey, .T. )

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * get a key value or NIL within an associative array
 *
*/
STATIC FUNCTION TAssociativeArray_GetKey( cKey, lCaseSensitive )
    LOCAL Self := QSelf()
    LOCAL aKeys, xRet, cMsg, nPos
    LOCAL lGlobalCase

    IF !( ValType( lCaseSensitive ) == "L" )
       lGlobalCase    := __SetAssociativeCaseSensitive()
       IF lGlobalCase <> NIL
          lCaseSensitive := lGlobalCase
       ELSE
          lCaseSensitive := ::lCaseSensitive
       ENDIF
    ENDIF

    aKeys := __ObjSendMsg( Self, "Keys" )

    IF !lCaseSensitive
       cMsg := Upper( cKey )
       IF (nPos := aScan( aKeys, {|e| Upper( e ) == cMsg } )) > 0
          xRet := __ObjSendMsgCase( Self, aKeys[ nPos ] )
       ENDIF
    ELSE
       IF ( cKey IN aKeys )
          xRet := __ObjSendMsgCase( Self, cKey )
       ENDIF
    ENDIF

RETURN xRet

/*
 * (C) 2003 - Francesco Saverio Giudice
 *
 * set case sensitive for associative arrays
 *
*/
FUNCTION __SetAssociativeCaseSensitive( lNew )
  STATIC s_lCase // Can be: TRUE or FALSE to force Case Sensitive or NIL to leave every associative to do itself
  LOCAL lOld := s_lCase

  IF PCount() == 1 .AND. ;
     ( ValType( lNew ) == "L" .OR. lNew == NIL )
     s_lCase := lNew
  ENDIF
RETURN lOld

#endif


FUNCTION HashEntry()

   STATIC hClass

   IF ValType( hClass ) == "N"
      RETURN __clsInst( hClass )
   ENDIF

   hClass := __clsNew( "HASHENTRY", 3, 2 )

   __clsAddMsg( hClass, "HPARENT",         1, HB_OO_MSG_PROPERTY, NIL, HB_OO_CLSTP_READONLY, .T., .T. )
   __clsAddMsg( hClass, "KEY",             2, HB_OO_MSG_PROPERTY, NIL, HB_OO_CLSTP_READONLY, .T., .T. )
   __clsAddMsg( hClass, "HASHENTRY_VALUE", 3, HB_OO_MSG_PROPERTY, NIL, HB_OO_CLSTP_HIDDEN,  .T., .T. )

   __clsAddMsg( hClass, "VALUE" , @HashEntry_GetValue(), HB_OO_MSG_METHOD )
   __clsAddMsg( hClass, "_VALUE", @HashEntry_SetValue(), HB_OO_MSG_METHOD )

RETURN __clsInst( hClass )

STATIC FUNCTION HashEntry_GetValue()
RETURN QSelf():HashEntry_Value

STATIC FUNCTION HashEntry_SetValue( xVal )
RETURN QSelf():HashEntry_Value := QSelf():hParent[ QSelf():Key ] := xVal



/*
 * (C) 2003 - Antonio Carlos Pantaglione
 *            toninho@fwi.com.br
 *
 * This procedure is used to create structures using the
 * command STRUCTURE / MEMBER / ENDSTRUCTURE.
 * See also hbstruct.ch
 *
*/

procedure HashAddMember( aName, cType, uInit, oObj )

   local cName

   if !( cType == nil )

      switch Upper( Left( cType, 1 ) )

         case "S" // STRING

              if uInit == nil
                 uInit := ""
              endif

              exit

         case "N" // NUMERIC

              if uInit == nil
                 uInit := 0
              endif

              exit

         case "L" // LOGICAL

              if uInit == nil
                 uInit := .f.
              endif

              exit

         case "D" // DATE or DATETIME

              if uInit == nil
                 uInit := IIf( cType == "DATE", CtoD( "" ), {^0/0/0} )
              endif

              exit

         case "C" // CODEBLOCK

              if uInit == nil
                 uInit := { || nil }
              endif

              exit

         case "A" // ARRAY

              if uInit == nil
                 uInit := {}
              endif

              exit

         case "O" // OBJECT
              exit

         case "H" // HASH

              if uInit == nil
                 uInit := Hash()
              endif

              exit

      end switch

   endif

   for each cName in aName
       oObj[ cName ] := uInit
   next

return


