/*
 * $Id: tobject.prg,v 1.10 2003/10/27 15:06:14 toninhofwi Exp $
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

/* WARNING: Can not use the preprocessor      */
/* otherwise it will auto inherit from itself */

#include "common.ch"
#include "hboo.ch"
#include "error.ch"

FUNCTION HBObject()
   STATIC s_oClass
   LOCAL nScope := HB_OO_CLSTP_EXPORTED
   Local oInstance

   IF s_oClass == NIL

      s_oClass := HBClass():New( "HBObject",  )

      /* Those Five worked fine but their C version from classes.c are probably better in term of speed */
      /*s_oClass:AddInline( "CLASSNAME"      , {| Self | __OBJGETCLSNAME( Self )     }, nScope ) */
      /*s_oClass:AddInline( "CLASSH"         , {| Self | __CLASSH( Self )            }, nScope ) */
      /*s_oClass:AddInline( "CLASSSEL"       , {| Self | __CLASSSEL( Self:CLASSH() ) }, nScope ) */
      /*s_oClass:AddInline( "EVAL"           , {| Self | __EVAL( Self )             }, nScope ) */

      /* xBase++ */
      s_oClass:AddInline( "ISDERIVEDFROM"  , {| Self, xPar1 | __ObjDerivedFrom( Self, xPar1 ) }, nScope )
      /* Class(y) */
      s_oClass:AddInline( "ISKINDOF"       , {| Self, xPar1 | __ObjDerivedFrom( Self, xPar1 ) }, nScope )

      s_oClass:AddMethod( "NEW"  , @HBObject_New()  , nScope )
      s_oClass:AddMethod( "INIT" , @HBObject_Init() , nScope )

      s_oClass:AddMethod( "ERROR", @HBObject_Error() , nScope )

      s_oClass:SetOnError( @HBObject_DftonError() )

      s_oClass:AddInline( "MSGNOTFOUND" , {| Self, cMsg | ::Error( "Message not found", __OBJGETCLSNAME( Self ), cMsg, iif(substr(cMsg,1,1)=="_",1005,1004) ) }, nScope )

      /*s_oClass:AddMultiData(,,nScope,{"CLASS"}, .F. )*/

      /*s_oClass:AddInline( "ADDMETHOD" , { | Self, cMeth, pFunc, nScopeMeth                 |  __clsAddMsg( __CLASSH( Self ) , cMeth , pFunc ,HB_OO_MSG_METHOD , NIL, iif(nScopeMeth==NIL,1,nScopeMeth) ) }, nScope )                                */
      /*s_oClass:AddInline( "ADDVAR"    , { | Self, cVAR, nScopeMeth, uiData , hClass  |  __clsAddMsg( hClass:=__CLASSH( Self ) ,     cVar , uidata := __CLS_INCDATA(hClass) , HB_OO_MSG_DATA, NIL  , iif(nScopeMeth==NIL,1,nScopeMeth) )  , ;        */
      /*                                                                               __clsAddMsg( hClass                   , "_"+cVar , uiData                          , HB_OO_MSG_DATA, NIL  , iif(nScopeMeth==NIL,1,nScopeMeth) ) }, nScope )    */

      /* Those one exist within Class(y), so we will probably try to implement it               */

      /*s_oClass:AddInline( "asString"       , {| Self | ::class:name + " object"   }, nScope ) */
      /*s_oClass:AddInline( "asExpStr"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "basicSize"      , {| Self | Len( Self )                }, nScope ) */
      /*s_oClass:AddInline( "become"         , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "isEqual"        , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "isScalar"       , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "copy"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "deepCopy"       , {| Self |                            }, nScope ) */

      /*s_oClass:AddInline( "deferred"       , {| Self |                            }, nScope ) */

      /*s_oClass:AddInline( "exec"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "error           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "hash"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "null"           , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "size"           , {| Self | Len( Self )                }, nScope ) */

      /* Those three are already treated within Classes.c */
      /*s_oClass:AddInline( "protectErr"     , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "hiddenErr"      , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "readOnlyErr"    , {| Self |                            }, nScope ) */

      /* No idea when those two could occur !!? */
      /*s_oClass:AddInline( "wrongClass"     , {| Self |                            }, nScope ) */
      /*s_oClass:AddInline( "badMethod"      , {| Self |                            }, nScope ) */

      /* this one exist within VO and seem to be Auto Called when object ran out of scope */
      /*s_oClass:AddInline( "Axit"           , {| Self |  }, nScope ) */

      s_oClass:Create()

   ENDIF

   oInstance := s_oClass:Instance()
   /*oInstance:class := s_oClass*/

   RETURN oInstance


/* Currently limited to 20 param */
/* Will be re-written in C later to avoid this */

static function HBObject_New(xPar0, xPar1, xPar2, xPar3, xPar4, xPar5, xPar6, xPar7, xPar8, xPar9, ;
                            xPar10,xPar11,xPar12,xPar13,xPar14,xPar15,xPar16,xPar17,xPar18,xPar19 )

return QSelf():Init(xPar0, xPar1, xPar2, xPar3, xPar4, xPar5, xPar6, xPar7, xPar8, xPar9, ;
                 xPar10,xPar11,xPar12,xPar13,xPar14,xPar15,xPar16,xPar17,xPar18,xPar19 )

static function HBObject_Init()
return QSelf()

static function HBObject_Dftonerror(xPar0, xPar1, xPar2, xPar3, xPar4, xPar5, xPar6, xPar7, xPar8, xPar9, ;
                                   xPar10,xPar11,xPar12,xPar13,xPar14,xPar15,xPar16,xPar17,xPar18,xPar19 )
return QSelf():MSGNOTFOUND( __GetMessage(), xPar0, xPar1, xPar2, xPar3, xPar4, xPar5, xPar6, xPar7, xPar8, xPar9, ;
                                            xPar10,xPar11,xPar12,xPar13,xPar14,xPar15,xPar16,xPar17,xPar18,xPar19 )

static function HBObject_Error( cDesc, cClass, cMsg, nCode )

   DEFAULT nCode TO 1004

   IF nCode == 1005
      RETURN __errRT_SBASE( EG_NOVARMETHOD, 1005, cDesc, cClass + ":" + cMsg )
   ENDIF

RETURN __errRT_SBASE( EG_NOMETHOD, nCode, cDesc, cClass + ":" + cMsg )

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

      FOR EACH aMember IN aInit
         __clsAddMsg( hClass, aMember[1], nSeq++, HB_OO_MSG_PROPERTY, aMember[2], HB_OO_CLSTP_EXPORTED, .T., .T. )
         aAdd( aKeys, aMember[1] )
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
    LOCAL hClass, nSeq, aKeys
    LOCAL xRet

    //cKey := Upper( cKey )
    aKeys := __ObjSendMsg( Self, "Keys" )
    //TraceLog( "PCOUNT(), cKey, xParam", PCOUNT(), cKey, xParam )

    IF !( cKey IN aKeys )
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
    LOCAL Self := QSelf()
    LOCAL aKeys, nPos := 0

    aKeys := __ObjSendMsg( Self, "Keys" )

    IF !( cKey IN aKeys )
       nPos := aScan( aKeys, {|e| e == cKey } )
    ENDIF

RETURN nPos

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
       nPos := aScan( aKeys, {|e| Upper( e ) == cMsg } )
       IF nPos > 0
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


/*

  This procedure is used to create structures using the
  command STRUCTURE / MEMBER / ENDSTRUCTURE

  Toninho@fwi.com.br

*/
procedure TAssociativeArrayMember( aName, cType, uInit, oObj )

   local x := 0

   local y := Len( aName )

   if !( cType == nil )

      cType = Upper( Left( cType, 1 ) )

      switch cType

         case "S"

              if uInit == nil
                 uInit  = ""
              endif

              exit

         case "N"

              if uInit == nil
                 uInit  = 0
              endif

              exit

         case "L"

              if uInit == nil
                 uInit  = .f.
              endif

              exit

         case "D"

              if uInit == nil
                 uInit  = CtoD( "" )
              endif

              exit

         case "C"

              if uInit == nil
                 uInit  = { || nil }
              endif

              exit

         case "A"

              if uInit == nil
                 uInit  = {}
              endif

              exit

      end switch

   endif

   for x := 1 to y
       oObj[ Upper( aName[ x ] ) ] = uInit
   next

return
