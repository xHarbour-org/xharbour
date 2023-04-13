/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base Class for internal handling of class creation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 *    Multiple inheritance
 *    Support shared class DATA
 *    scoping (hidden, protected, readOnly)
 *    Use of __cls_param function to allow multiple superclass declaration
 *    Suppress of SetType and SetInit not more nedded
 *    Delegation and forwarding
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    Support for inheritance
 *    Support for default DATA values
 *
 * See doc/license.txt for licensing terms.
 *
 */

   /* Harbour Class HBClass to build classes */

#include "common.ch"
#include "hboo.ch"
#include "divert.ch"

   REQUEST HBObject
   REQUEST __CLSISACTIVE
   REQUEST __CLSACTIVE

FUNCTION HBClass()

   STATIC s_hClass /* NOTE: Automatically default to NIL */

   IF s_hClass == NIL
      s_hClass := __clsNew( "HBCLASS", 14, 23 )
      __ClsSetModule( s_hClass )

      __clsAddMsg( s_hClass, "New"                  , @New()                  , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Create"               , @Create()               , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddData"              , @AddData()              , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMultiData"         , @AddMultiData()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClassData"         , @AddClassData()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMultiClsData"      , @AddMultiClsData()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddInline"            , @AddInline()            , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddMethod"            , @AddMethod()            , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddClsMethod"         , @AddClsMethod()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddVirtual"           , @AddVirtual()           , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddDelegate"          , @AddDelegate()          , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "ModInline"            , @ModInline()            , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "ModMethod"            , @ModMethod()            , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "ModClsMethod"         , @ModClsMethod()         , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Instance"             , @Instance()             , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetOnError"           , @SetOnError()           , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "SetDestructor"        , @SetDestructor()        , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "ConstructorCall"      , @ConstructorCall()      , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "DivertConstructorCall", @DivertConstructorCall(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Refresh"              , @Refresh()              , HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "AddFriends"           , @AddFriends()           , HB_OO_MSG_METHOD )

      __clsAddMsg( s_hClass, "cSuper"         , {| Self | iif( ::acSuper == NIL .OR. Len( ::acSuper ) == 0, NIL, ::acSuper[ 1 ] ) }, HB_OO_MSG_INLINE )
      __clsAddMsg( s_hClass, "_cSuper"        , {| Self, xVal | iif( ::acSuper == NIL .OR. Len( ::acSuper ) == 0, ( ::acSuper := { xVal } ), ::acSuper[ 1 ] := xVal ), xVal }, HB_OO_MSG_INLINE )

      __clsAddMsg( s_hClass, "hClass"         ,  1, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "cName"          ,  2, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aDatas"         ,  3, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aMethods"       ,  4, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aClsDatas"      ,  5, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aClsMethods"    ,  6, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aInlines"       ,  7, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aVirtuals"      ,  8, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aDelegates"     ,  9, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "acSuper"        , 10, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "nOnError"       , 11, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "nDestructor"    , 12, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "lFirst"         , 13, HB_OO_MSG_PROPERTY )
      __clsAddMsg( s_hClass, "aFriends"       , 14, HB_OO_MSG_PROPERTY )

   ENDIF

   RETURN __clsInst( s_hClass )

//----------------------------------------------------------------------------//
// xSuper is used here as the new preprocessor file (HBCLASS.CH) send here
// always an array (if no superclass, this will be an empty one)
// In case of direct class creation (without the help of preprocessor) xSuper can be
// either NIL or contain the name of the superclass.

STATIC FUNCTION New( cClassName, xSuper )

   LOCAL Self := QSelf()
   LOCAL cSuper

   IF ISARRAY( xSuper ) .AND. Len( xSuper ) >= 1
      ::acSuper := xSuper
   ELSEIF ISCHARACTER( xSuper ) .AND. ! Empty( xSuper )
      ::acSuper := { xSuper }
   ELSE
      ::acSuper := {}
   ENDIF

   ::cName       := Upper( cClassName )

   ::aDatas      := {}
   ::aMethods    := {}
   ::aClsDatas   := {}
   ::aClsMethods := {}
   ::aInlines    := {}
   ::aVirtuals   := {}
   ::aDelegates  := {}
   ::lFirst      := .T.
   ::aFriends    := {}

   FOR EACH cSuper IN ::acSuper
      IF ! HB_ISSTRING( cSuper ) .AND. ! HB_ISNUMERIC( cSuper )
         ASize( ::acSuper, HB_EnumIndex() - 1 )
         EXIT
      ENDIF
   NEXT

   RETURN Self

//----------------------------------------------------------------------------//

STATIC PROCEDURE CREATE()

   LOCAL Self := QSelf()

//LOCAL n
   LOCAL nLen := Len( ::acSuper )
   LOCAL nLenDatas := Len( ::aDatas ) //Datas local to the class !!
   LOCAL nDataBegin := 0
   LOCAL nClassBegin := 0
   LOCAL hClass
   LOCAL ahSuper := Array( nLen )
   LOCAL nExtraMsgs := Len( ::aMethods ) +  ( 2 * Len( ::aClsDatas ) ) + Len( ::aInlines ) + Len( ::aVirtuals )
   LOCAL cDato
   LOCAL hSuper
   LOCAL nClassModule := 1
   LOCAL xFriend

// This code allows HBClass itself to be inherited
   WHILE Self == HB_QSelf( nClassModule )
      nClassModule++
   ENDDO

   IF nLen == 0
      //Maybe this class is a super class, and the oop engine store
      //in the first element a real self, when call a super method
      hClass := __clsNew( ::cName, nLenDatas + 1, nExtraMsgs, , nClassModule )
      nDataBegin := 1
   ELSE                                         // Multi inheritance
      FOR EACH cDato IN ::acSuper

         IF HB_ISNUMERIC( cDato )
            ahSuper[ HB_EnumIndex() ]   := cDato
            ::acSuper[ HB_EnumIndex() ] := __className( cDato )
         ELSE
            hSuper := __ClsGetHandleFromName( cDato )
            IF hSuper == 0
               ahSuper[ HB_EnumIndex() ] := __clsInstSuper( Upper( cDato ) )
            ELSE
               ahSuper[ HB_EnumIndex() ] := hSuper
            ENDIF
         ENDIF

         IF ahSuper[ HB_EnumIndex() ] == 0
            Throw( ErrorNew( "TClass", 0, 1003, ProcName(), "Could not locate super: " , cDato ) )
         ENDIF
      NEXT

      hClass := __clsNew( ::cName, nLenDatas , nExtraMsgs, ahSuper, nClassModule )

      FOR EACH cDato IN ahSuper
         nDataBegin   += __cls_CntData( cDato )        // Get offset for new Datas
         nClassBegin  += __cls_CntClsData( cDato )     // Get offset for new ClassData
      NEXT

      //__clsAddMsg( hClass, Upper( ::acSuper[ 1 ] ), 1, HB_OO_MSG_SUPER, ahSuper[ 1 ], HB_OO_CLSTP_CLASS + 1 )
      // nData begin stay here the same so as, SUPER and __SUPER will share the same pointer to super object with the first one.
      __clsAddMsg( hClass, "SUPER"                , 1, HB_OO_MSG_SUPER, ahSuper[ 1 ], 1 )
      __clsAddMsg( hClass, "__SUPER"              , 1, HB_OO_MSG_SUPER, ahSuper[ 1 ], 1 )

      //FOR n := 2 TO nLen
      //   __clsAddMsg( hClass, Upper( ::acSuper[ n ] ), HB_EnumIndex(), HB_OO_MSG_SUPER, ahSuper[ n ], HB_OO_CLSTP_CLASS + 1 )
      //NEXT
   ENDIF
//   __clsAddMsg( hClass, Upper( ::cName ), 0, HB_OO_MSG_SUPER, hClass, 1 )

   ::hClass := hClass

// We will work here on the MetaClass object to add the Class Method
// as needed
   FOR EACH cDato IN ::aClsMethods
      IF !( __objHasMsg( Self, cDato[ HB_OO_MTHD_SYMBOL ] ) )
         __clsAddMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_PFUNCTION ], HB_OO_MSG_METHOD, NIL, cDato[ HB_OO_MTHD_SCOPE ] )
      ENDIF
   NEXT

   FOR EACH cDato IN ::aDatas
      __clsAddMsg( hClass, cDato[ HB_OO_DATA_SYMBOL ]       , HB_EnumIndex() + nDataBegin, ;
         HB_OO_MSG_PROPERTY, cDato[ HB_OO_DATA_VALUE ], cDato[ HB_OO_DATA_SCOPE ], ;
         cDato[ HB_OO_DATA_PERSISTENT ] )
   NEXT

   FOR EACH cDato IN ::aMethods
      __clsAddMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_PFUNCTION ], HB_OO_MSG_METHOD, NIL, cDato[ HB_OO_MTHD_SCOPE ], ;
         cDato[ HB_OO_MTHD_PERSISTENT ] )
   NEXT

   FOR EACH cDato IN ::aClsDatas
      __clsAddMsg( hClass, cDato[ HB_OO_CLSD_SYMBOL ]      , HB_EnumIndex() + nClassBegin, ;
         HB_OO_MSG_CLASSPROPERTY, cDato[ HB_OO_CLSD_VALUE ], cDato[ HB_OO_CLSD_SCOPE ] )
   NEXT

   FOR EACH cDato IN ::aInlines
      __clsAddMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_PFUNCTION ], ;
         HB_OO_MSG_INLINE, NIL, cDato[ HB_OO_MTHD_SCOPE ], ;
         cDato[ HB_OO_MTHD_PERSISTENT ] )
   NEXT

   FOR EACH cDato IN ::aVirtuals
      __clsAddMsg( hClass, cDato, HB_EnumIndex(), HB_OO_MSG_VIRTUAL )
   NEXT

   FOR EACH cDato IN ::aDelegates
      __clsAddMsg( ::hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_DELEGNAME ], HB_OO_MSG_DELEGATE, cDato[ HB_OO_MTHD_DELEGOBJ ], cDato[ HB_OO_MTHD_SCOPE ], ;
         cDato[ HB_OO_MTHD_PERSISTENT ] )
   NEXT

   FOR EACH xFriend IN ::aFriends
      __clsAddFriend( ::hClass, xFriend )
   NEXT

   IF ::nOnError != NIL
      __clsAddMsg( hClass, "__OnError", ::nOnError, HB_OO_MSG_ONERROR )
   ENDIF

   IF ::nDestructor != NIL
      __clsAddMsg( hClass, "__Destructor", ::nDestructor, HB_OO_MSG_DESTRUCTOR )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE Refresh()

   LOCAL Self := QSelf()
   LOCAL nLen := Len( ::acSuper )
   LOCAL ahSuper := Array( nLen )
   LOCAL cDato, hClass := ::hClass

   FOR EACH cDato IN ::acSuper
      ahSuper[ HB_EnumIndex() ] := __clsInstSuper( Upper( cDato ) )
   NEXT

   FOR EACH cDato IN ::aClsMethods
      IF __clsHasMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ] )
         __clsModMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_PFUNCTION ] )
      ENDIF
   NEXT

   FOR EACH cDato IN ::aMethods
      IF __clsHasMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ] )
         __clsModMsg( hClass, cDato[ HB_OO_MTHD_SYMBOL ], cDato[ HB_OO_MTHD_PFUNCTION ] )
      ENDIF
   NEXT

   IF ::nOnError != NIL
      __clsModMsg( hClass, "__OnError", ::nOnError )
   ENDIF

   IF ::nDestructor != NIL
      __clsModMsg( hClass, "__Destructor", ::nDestructor )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC FUNCTION Instance( )

   LOCAL Self := QSelf()

   RETURN __clsInst( ::hClass )

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddData( cData, xInit, cType, nScope, lNoinit, lPersistent )

   LOCAL Self := QSelf()

   IF lNoInit == NIL; lNoInit := .F. ; ENDIF
   IF lPersistent == nil; lpersistent := .F. ; ENDIF

// Default Init for Logical and numeric
   IF ! lNoInit .AND. cType != NIL .AND. xInit == NIL
      IF Upper( Left( cType, 1 ) ) == "L"
         xInit := .F.
      ELSEIF Upper( Left( cType, 1 ) ) IN "NI"   /* Numeric Int */
         xInit := 0
      ENDIF
   ENDIF

   AAdd( ::aDatas, { cData, xInit, cType, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMultiData( cType, xInit, nScope, aData, lNoInit, lPersistent )

   LOCAL Self := QSelf()
   LOCAL cData

   FOR EACH cData IN aData
      IF ! ISCHARACTER( cData )
         ASize( aData, HB_EnumIndex() - 1 )
         EXIT
      ENDIF
      //      i++
   NEXT


   FOR EACH cData IN aData
      ::AddData( cData, xInit, cType, nScope, lNoInit, lPersistent )
   NEXT

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddClassData( cData, xInit, cType, nScope, lNoInit )

   LOCAL Self := QSelf()

   IF lNoInit == NIL;lNoInit := .F. ;ENDIF

// Default Init for Logical and numeric
   IF ! lNoInit .AND. cType != NIL .AND. xInit == NIL
      IF Upper( Left( cType, 1 ) ) == "L"
         xInit := .F.
      ELSEIF Upper( Left( cType, 1 ) ) IN "NI"  /* Numeric Int */
         xInit := 0
      ENDIF
   ENDIF

   AAdd( ::aClsDatas, { cData, xInit, cType, nScope } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMultiClsData( cType, xInit, nScope, aData, lNoInit )

   LOCAL Self := QSelf()
   LOCAL cData

   FOR EACH cData IN aData
      IF ! ISCHARACTER( cData )
         ASize( aData, HB_EnumIndex() - 1 )
         EXIT
      ENDIF
      //      i++
   NEXT


   FOR EACH cData IN aData
      ::AddClassData( cData, xInit, cType, nScope, lNoInit )
   NEXT

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddInline( cMethod, bCode, nScope, lPersistent )

   LOCAL Self := QSelf()

   AAdd( ::aInlines, { cMethod, bCode, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddMethod( cMethod, nFuncPtr, nScope, lPersistent )

   LOCAL Self := QSelf()

   AAdd( ::aMethods, { cMethod, nFuncPtr, nScope, lPersistent } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddClsMethod( cMethod, nFuncPtr, nScope )

   LOCAL Self := QSelf()

   AAdd( ::aClsMethods, { cMethod, nFuncPtr, nScope } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddVirtual( cMethod )

   LOCAL Self := QSelf()

   AAdd( ::aVirtuals, cMethod )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddDelegate( cMethod, cDelegate, cObject, nScope, lPersistent )

   LOCAL Self := QSelf()

   AAdd( ::aDelegates, { cMethod, cDelegate, nScope, lPersistent, cObject } )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE AddFriends( ... )

   LOCAL Self := QSelf()

   aMerge( ::aFriends, hb_AParams() )

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE ModInline( cMethod, bCode, nScope, lPersistent )

   LOCAL Self := QSelf(), nAt

   IF ( nAt := AScan( ::aInlines, {|a| a[1] == cMethod } ) ) > 0
      ::aInlines[ nAt ] := { cMethod, bCode, nScope, lPersistent }
   ELSE
      AAdd( ::aInlines, { cMethod, bCode, nScope, lPersistent } )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE ModMethod( cMethod, nFuncPtr, nScope, lPersistent )

   LOCAL Self := QSelf(), nAt

   IF ( nAt := AScan( ::aMethods, {|a| a[1] == cMethod } ) ) > 0
      ::aMethods[ nAt ] := { cMethod, nFuncPtr, nScope, lPersistent }
   ELSE
      AAdd( ::aMethods, { cMethod, nFuncPtr, nScope, lPersistent } )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE ModClsMethod( cMethod, nFuncPtr, nScope )

   LOCAL Self := QSelf(), nAt

   IF ( nAt := AScan( ::aClsMethods, {|a| a[1] == cMethod } ) ) > 0
      ::aClsMethods[ nAt ] := { cMethod, nFuncPtr, nScope }
   ELSE
      AAdd( ::aClsMethods, { cMethod, nFuncPtr, nScope } )
   ENDIF

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE SetOnError( nFuncPtr )

   LOCAL Self := QSelf()

   ::nOnError := nFuncPtr

   RETURN

//----------------------------------------------------------------------------//

STATIC PROCEDURE SetDestructor( nFuncPtr )

   LOCAL Self := QSelf()

   ::nDestructor := nFuncPtr

   RETURN

//----------------------------------------------------------------------------//
/*
 * (C) 2002 - Francesco Saverio Giudice
 *
 * Used to autoinitialize a class
 *
 * FSG 2003/11/05 - Fixed with right check of class constructor method
*/

STATIC FUNCTION ConstructorCall( oClass, aParams )

   LOCAL Self := QSelf()
   LOCAL aConstrMethods
   LOCAL lOldScope, nPos

   IF __SetClassAutoInit() .AND. Len( aParams ) > 0
      // Set class scoping off
      lOldScope := __SetClassScope( .F. )

      // Get method full list but limited to those with class type as constructor
      aConstrMethods  := __objGetMsgFullList( oClass, .F. , HB_MSGLISTALL, HB_OO_CLSTP_CTOR )

      // Search the constructor which is not derived from a parent class
      //aEval( aConstrMethods, {|aMth| TraceLog( "aScan",  aMth[HB_OO_DATA_SYMBOL], aMth[HB_OO_DATA_SCOPE], ;
      //                                         hb_BitAnd( aMth[HB_OO_DATA_SCOPE], HB_OO_CLSTP_SUPER ) ) } )
      nPos := AScan( aConstrMethods, {|aMth| hb_bitAnd( aMth[HB_OO_DATA_SCOPE], HB_OO_CLSTP_SUPER ) == 0 } )

      // Revert class scoping
      __SetClassScope( lOldScope )

      IF nPos > 0
         // Exec method - i have found the constructor in this class
         //TraceLog( "Search this class constructor:", aConstrMethods[ nPos ][HB_OO_DATA_SYMBOL] )
         RETURN hb_ExecFromArray( oClass, aConstrMethods[ nPos ][ HB_OO_DATA_SYMBOL ], aParams )
      ELSE
         // Get LAST constructor from parent (NOTE: this can be a default and faster way,
         // but i prefer check rightly before)
         IF !Empty( aConstrMethods )
            //TraceLog( "Search parent class constructor:", aTail( aConstrMethods )[HB_OO_DATA_SYMBOL] )
            RETURN hb_ExecFromArray( oClass, aConstrMethods[-1][ HB_OO_DATA_SYMBOL ], aParams )
         ELSE
            //TraceLog( "Call new default constructor:", "NEW" )
            // If i have no constructor i call NEW method that is defined is HBOBJECT class
            //HB_ExecFromArray( oClass, "NEW", aParams )
            Alert( "Warning! Class function '" + oClass:ClassName + "' called with arguments, but no constructor found." )
         ENDIF
      ENDIF

   ENDIF

   RETURN Self

#pragma -w2

STATIC PROCEDURE DivertConstructorCall( ... )

// From DIVERT parent!
   LOCAL oClassInstance
   LOCAL nScope
// End

   LOCAL aConstrMethods
   LOCAL lOldScope, nPos

   ( nScope )

   IF __SetClassAutoInit() //.AND. PCount() > 0
      // Set class scoping off
      lOldScope := __SetClassScope( .F. )

      // Get method full list but limited to those with class type as constructor
      aConstrMethods  := __objGetMsgFullList( oClassInstance, .F. , HB_MSGLISTALL, HB_OO_CLSTP_CTOR )

      // Search the constructor which is not derived from a parent class
      //aEval( aConstrMethods, {|aMth| TraceLog( "aScan",  aMth[HB_OO_DATA_SYMBOL], aMth[HB_OO_DATA_SCOPE], ;
      //                                         hb_BitAnd( aMth[HB_OO_DATA_SCOPE], HB_OO_CLSTP_SUPER ) ) } )
      nPos := AScan( aConstrMethods, {|aMth| hb_bitAnd( aMth[HB_OO_DATA_SCOPE], HB_OO_CLSTP_SUPER ) == 0 } )

      // Revert class scoping
      __SetClassScope( lOldScope )

      IF nPos > 0
         // Exec method - i have found the constructor in this class
         //TraceLog( "Search this class constructor:", aConstrMethods[ nPos ][HB_OO_DATA_SYMBOL] )
         //RETURN HB_ExecFromArray( oClass, aConstrMethods[ nPos ][ HB_OO_DATA_SYMBOL ], aParams )
         DIVERT TO aConstrMethods[ nPos ][ HB_OO_DATA_SYMBOL_PTR ] OF oClassInstance FLAGS DIVERT_RESET_LOCALS
      ELSE
         // Get LAST constructor from parent (NOTE: this can be a default and faster way,
         // but i prefer check rightly before)
         IF !Empty( aConstrMethods )
            //TraceLog( "Search parent class constructor:", aTail( aConstrMethods )[HB_OO_DATA_SYMBOL] )
            //RETURN HB_ExecFromArray( oClass, aConstrMethods[-1][ HB_OO_DATA_SYMBOL ], aParams )
            DIVERT TO aConstrMethods[-1][ HB_OO_DATA_SYMBOL_PTR ] OF oClassInstance FLAGS DIVERT_RESET_LOCALS
         ELSE
            //TraceLog( "Call new default constructor:", "NEW" )
            // If i have no constructor i call NEW method that is defined is HBOBJECT class
            //HB_ExecFromArray( oClass, "NEW", aParams )
            Alert( "Warning! Class function '" + oClassInstance:ClassName + "' called with arguments, but no constructor found." )
         ENDIF
      ENDIF
   ENDIF

   RETURN
