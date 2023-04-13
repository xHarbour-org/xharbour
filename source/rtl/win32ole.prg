/*
 * $Id$
 */

/*
 * Copyright 2002  José F. Giménez (JFG) - <jfgimenez@wanadoo.es>
 *                 Ron Pinkas            - <ron@ronpinkas.com>
 *
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
 * As a special exception, the xHarbour Project gives permission for
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
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "common.ch"

#ifndef __PLATFORM__Windows

  Function CreateObject()
  Return NIL

  FUNCTION GetActiveObject( cString )
    HB_SYMBOL_UNUSED( cString )
  Return NIL

#else

#define HB_CLS_NOTOBJECT

#include "hbclass.ch"
#include "error.ch"
#include "vt.ch"
#include "oleerr.ch"

#define OLE_DEFAULT_ARG OleDefaultArg()

EXTERNAL HB_OLEINIT
//----------------------------------------------------------------------------//
FUNCTION CreateObject( cString, cLicense )

RETURN TOleAuto():New( cString,, cLicense )

//----------------------------------------------------------------------------//
FUNCTION GetActiveObject( cString )

RETURN TOleAuto():GetActiveObject( cString )

//----------------------------------------------------------------------------//
FUNCTION OleDefaultArg()

RETURN VTWrapper( VT_ERROR, DISP_E_PARAMNOTFOUND )

//----------------------------------------------------------------------------//
CLASS VTWrapper
   DATA vt
   DATA Value

   METHOD New( vt, xVal ) CONSTRUCTOR
ENDCLASS

//----------------------------------------------------------------------------//
METHOD New( vt, xVal ) CLASS VTWrapper

   ::vt := vt
   ::Value := xVal

   //TraceLog( vt, ::vt, xVal, ::Value )

RETURN Self

//----------------------------------------------------------------------------//
CLASS VTArrayWrapper FROM VTWrapper

   METHOD AsArray( nIndex, xValue ) OPERATOR "[]"
   METHOD Enumerate( nEnumOp, nIndex ) OPERATOR "FOR EACH"

ENDCLASS

//----------------------------------------------------------------------------//
METHOD AsArray( nIndex, xValue ) CLASS VTArrayWrapper

RETURN IIF( PCount() == 1, ::Value[nIndex], ::Value[nIndex] := xValue )

//----------------------------------------------------------------------------//
METHOD Enumerate( nEnumOp, nIndex ) CLASS VTarrayWrapper

   HB_SYMBOL_UNUSED( nIndex )

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         RETURN ::Value
      CASE FOREACH_ENUMERATE
         // Can never happen!
         EXIT
      CASE FOREACH_END
         // Can never happen!
         EXIT
   ENDSWITCH

 RETURN Self

//----------------------------------------------------------------------------//
CLASS TOleAuto

   DATA hObj
   DATA cClassName
   DATA pOleEnumerator

   METHOD New( uObj, cClass ) CONSTRUCTOR
   METHOD GetActiveObject( cClass ) CONSTRUCTOR

   METHOD Invoke()
   MESSAGE CallMethod  METHOD Invoke()

   METHOD Set()
   MESSAGE SetProperty METHOD Set()

   METHOD Get()
   MESSAGE GetProperty METHOD Get()

   METHOD OleValue()
   METHOD _OleValue( xSetValue )

   METHOD OleNewEnumerator()

   METHOD OleCollection( xIndex, xValue ) OPERATOR "[]"

   METHOD OleValuePlus( xArg )            OPERATOR "+"
   METHOD OleValueMinus( xArg )           OPERATOR "-"
   METHOD OleValueMultiply( xArg )        OPERATOR "*"
   METHOD OleValueDivide( xArg )          OPERATOR "/"
   METHOD OleValueModulus( xArg )         OPERATOR "%"
   METHOD OleValueInc()                   OPERATOR "++"
   METHOD OleValueDec()                   OPERATOR "--"
   METHOD OleValuePower( xArg )           OPERATOR "^"

   METHOD OleValueEqual( xArg )           OPERATOR "="
   METHOD OleValueExactEqual( xArg )      OPERATOR "=="
   METHOD OleValueNotEqual( xArg )        OPERATOR "!="

   METHOD OleEnumerate( nEnumOp, nIndex ) OPERATOR "FOR EACH"

   ERROR HANDLER OnError()

   DESTRUCTOR Release()

   // Needed to refernce, or hb_dynsymFindName() will fail
   METHOD ForceSymbols() INLINE ::cClassName()

ENDCLASS

//----------------------------------------------------------------------------//
METHOD New( uObj, cClass, cLicense ) CLASS TOleAuto

   LOCAL oErr

   // Hack incase OLE Server already created and New() is attempted as an OLE Method.
   IF ::hObj != NIL
      RETURN HB_ExecFromArray( Self, "_New", HB_aParams() )
   ENDIF

   SWITCH ValType( uObj )
   CASE 'C'
      ::hObj := CreateOleObject( uObj,, cLicense )

      IF OleError() != 0
         IF Ole2TxtError() == "DISP_E_EXCEPTION"
            oErr := ErrorNew()
            oErr:Args          := HB_aParams()
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := OLEExceptionDescription()
            oErr:GenCode       := EG_OLEEXECPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := OLEExceptionSource()

            RETURN Throw( oErr )
         ELSE
            oErr := ErrorNew()
            oErr:Args          := HB_aParams()
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := Ole2TxtError()
            oErr:GenCode       := EG_OLEEXECPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := "TOleAuto"

            RETURN Throw( oErr )
         ENDIF
      ENDIF

      ::cClassName := uObj
      EXIT
   CASE 'N'
      OleAddRef( uObj )
      ::hObj := uObj

      IF HB_ISSTRING( cClass )
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
      EXIT
   CASE 'P'
      uObj := OlePtr2Int( uObj )
      OleAddRef( uObj )
      ::hObj := uObj

      IF HB_ISSTRING( cClass )
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
      EXIT
   DEFAULT
      oErr := ErrorNew()
      oErr:Args          := HB_aParams()
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Invalid argument to constructor!"
      oErr:GenCode       := 0
      oErr:Operation     := ProcName()
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "TOleAuto"

      RETURN Throw( oErr )
   ENDSWITCH

RETURN Self

// Destructor!
//----------------------------------------------------------------------------//
PROCEDURE Release() CLASS TOleAuto

   //TraceLog( ::cClassName, ::hObj )

   IF ! Empty( ::hObj )
      //TraceLog( ::cClassName, ::hObj )
      OleReleaseObject( ::hObj )
      //::hObj := NIL
   ENDIF

RETURN

//----------------------------------------------------------------------------//
METHOD GetActiveObject( cClass ) CLASS TOleAuto

   LOCAL oErr

   IF HB_ISSTRING( cClass )
      ::hObj := GetOleObject( cClass )

      IF OleError() != 0
         IF Ole2TxtError() == "DISP_E_EXCEPTION"
            oErr := ErrorNew()
            oErr:Args          := { cClass }
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := OLEExceptionDescription()
            oErr:GenCode       := EG_OLEEXECPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := OLEExceptionSource()

            RETURN Throw( oErr )
         ELSE
            oErr := ErrorNew()
            oErr:Args          := { cClass }
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := Ole2TxtError()
            oErr:GenCode       := EG_OLEEXECPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := "TOleAuto"

            RETURN Throw( oErr )
         ENDIF
      ENDIF

      ::cClassName := cClass
   ELSE
      OleMessageBox( 0, "Invalid parameter type to constructor TOleAuto():GetActiveObject()!", "OLE Interface", 0 )
      ::hObj := 0
   ENDIF

RETURN Self

//----------------------------------------------------------------------------//
METHOD OleCollection( xIndex, xValue ) CLASS TOleAuto

   LOCAL xRet

   //TraceLog( PCount(), xIndex, xValue )

   IF PCount() == 1
      RETURN ::Item( xIndex )
   ENDIF

   IF HB_ISNUMERIC( xIndex ) .AND. xIndex < 0
      xIndex += ( ::Count + 1 )
   ENDIF

   TRY
      // ASP Collection syntax.
      xRet := ::_Item( xIndex, xValue )
   CATCH
      xRet := ::SetItem( xIndex, xValue )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValuePlus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue + xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '+'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1081
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueMinus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue - xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '-'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1082
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueMultiply( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue * xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '*'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1083
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueDivide( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue / xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '/'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1084
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueModulus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue % xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '%'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueInc() CLASS TOleAuto

   LOCAL oErr

   TRY
      ++::OleValue
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '++'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1086
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN Self

//----------------------------------------------------------------------------//
METHOD OleValueDec() CLASS TOleAuto

   LOCAL oErr

   TRY
      --::OleValue
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '--'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1087
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN Self

//----------------------------------------------------------------------------//
METHOD OleValuePower( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue ^ xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '^'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1088
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue == xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '='
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueExactEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue == xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '=='
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleValueNotEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue != xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := '!='
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//----------------------------------------------------------------------------//
METHOD OleEnumerate( nEnumOp, nIndex ) CLASS TOleAuto

   // LOCAL xRet

   HB_SYMBOL_UNUSED( nIndex )

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         ::pOleEnumerator := ::OleNewEnumerator()
         EXIT

      CASE FOREACH_ENUMERATE
         //xRet := ::Item( nIndex )
         //xRet := ::pOleEnumerator:Next()
         RETURN OleEnumerate_Enumerate( ::pOleEnumerator )
         //EXIT

      CASE FOREACH_END
         OleEnumerate_End( ::pOleEnumerator )
         ::pOleEnumerator := NIL
         EXIT
   END

RETURN Self

#endif  //__PLATFORM__Windows
