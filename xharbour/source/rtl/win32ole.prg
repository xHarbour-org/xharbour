/*
 * $Id: win32ole.prg,v 1.180 2009/10/13 20:36:44 ronpinkas Exp $
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

#ifndef __PLATFORM__Windows

#include "common.ch"

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

//----------------------------------------------------------------------------//

FUNCTION CreateObject( cString, cLicense )

RETURN TOleAuto():New( cString, , cLicense )

//----------------------------------------------------------------------------//
FUNCTION GetActiveObject( cString )

RETURN TOleAuto():GetActiveObject( cString )

//----------------------------------------------------------------------------//
FUNCTION OleDefaultArg()

RETURN VTWrapper( VT_ERROR, DISP_E_PARAMNOTFOUND )

//----------------------------------------------------------------------------//
#pragma BEGINDUMP

   #ifndef CINTERFACE
      #define CINTERFACE 1
   #endif

   #define NONAMELESSUNION

   #include <string.h>

   #include "hbapi.h"
   #include "hbstack.h"
   #include "hbapierr.h"
   #include "hbapiitm.h"
   #include "hbvm.h"
   #include "hbdate.h"
   #include "hboo.ch"
   #include "hbfast.h"

   #include <ctype.h>

   #include <windows.h>
   #include <ole2.h>
   #include <oleauto.h>

   #ifndef __MINGW32__
      // Missing in Mingw V 2.
      //#include <OleDB.h>
   #endif

   #if (defined(__MINGW32__) || defined(__DMC__) || defined(__WATCOMC__))
      #include <ocidl.h>
   #endif

   #include <shlobj.h>

   #ifdef __MINGW32__
      // Missing in oleauto.h
      WINOLEAUTAPI VarR8FromDec(DECIMAL *pdecIn, DOUBLE *pdblOut);
   #endif

   #if ( defined(__DMC__) || defined(__MINGW32__) || ( defined(__WATCOMC__) && !defined(__FORCE_LONG_LONG__) ) )
      #define HB_LONG_LONG_OFF
   #endif

   static void RetValue( void );

   static HRESULT  s_nOleError;
   static HB_ITEM  OleAuto;

   static PHB_DYNS s_pSym_TOleAuto = NULL;
   static PHB_DYNS s_pSym_hObj = NULL;
   static PHB_DYNS s_pSym_New = NULL;
   static PHB_DYNS s_pSym_cClassName = NULL;

   static PHB_DYNS s_pSym_VTWrapper = NULL;
   static PHB_DYNS s_pSym_VTArrayWrapper = NULL;
   static PHB_DYNS s_pSym_vt = NULL;
   static PHB_DYNS s_pSym_Value = NULL;

   static DISPPARAMS s_EmptyDispParams;

   static VARIANTARG RetVal, OleVal;


   #ifdef __WATCOMC__
      HB_EXTERN_BEGIN
      const GUID GUID_NULL = { 0x00, 0x00, 0x00, { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } };
      const IID  IID_IDispatch =
          { 0x00020400, 0x0000, 0x0000, { 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
      const IID  IID_IEnumVARIANT =
          { 0x00020404, 0x0000, 0x0000, { 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
      const IID  IID_IClassFactory2 =
          { 0xb196b28f,0xbab4,0x101a,0xb6,0x9c,0,0xaa,0,0x34,0x1d,0x7 };
      HB_EXTERN_END
   #endif

#pragma ENDDUMP

//----------------------------------------------------------------------------//
// Called by hb_vmInit() by means of hb_vmDoInitOle() ONLY if OLE is linked-in
PROCEDURE HB_OleInit()

   /*
    Using HB_INLINE() instead of HB_FUNC() to force Symbol Table entry, because
    we do NOT want to force static linking!
   */
   HB_INLINE() ;
   {
      if( s_pSym_TOleAuto == NULL )
      {
          s_pSym_TOleAuto       = hb_dynsymFind( "TOLEAUTO" );
          s_pSym_New            = hb_dynsymFind( "NEW" );
          s_pSym_hObj           = hb_dynsymFind( "HOBJ" );
          s_pSym_cClassName     = hb_dynsymFind( "CCLASSNAME" );

          s_pSym_VTWrapper      = hb_dynsymFind( "VTWRAPPER" );
          s_pSym_VTArrayWrapper = hb_dynsymFind( "VTARRAYWRAPPER" );
          s_pSym_vt             = hb_dynsymGetCase( "VT" );
          s_pSym_Value          = hb_dynsymFind( "VALUE" );

          s_EmptyDispParams.rgvarg            = NULL;
          s_EmptyDispParams.cArgs             = 0;
          s_EmptyDispParams.rgdispidNamedArgs = 0;
          s_EmptyDispParams.cNamedArgs        = 0;

          VariantInit( &RetVal );
          VariantInit( &OleVal );
      }
   }

RETURN

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

   (nIndex)

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         RETURN ::Value

      CASE FOREACH_ENUMERATE
         // Can never happen!
         EXIT

      CASE FOREACH_END
         // Can never happen!
         EXIT
   END

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

//--------------------------------------------------------------------
METHOD New( uObj, cClass, cLicense ) CLASS TOleAuto

   LOCAL oErr

   // Hack incase OLE Server already created and New() is attempted as an OLE Method.
   IF ::hObj != NIL
      RETURN HB_ExecFromArray( Self, "_New", HB_aParams() )
   ENDIF

   IF ValType( uObj ) = 'C'
      ::hObj := CreateOleObject( uObj, ,cLicense )

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
   ELSEIF ValType( uObj ) = 'N'
      OleAddRef( uObj )
      ::hObj := uObj

      IF ValType( cClass ) == 'C'
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF

   ELSEIF ValType( uObj ) = 'P'
      uObj := Ptr2Int( uObj )
      OleAddRef( uObj )
      ::hObj := uObj

      IF ValType( cClass ) == 'C'
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
   ELSE
      oErr := ErrorNew()
      oErr:Args          := HB_aParams()
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Invalid argument to contructor!"
      oErr:GenCode       := 0
      oErr:Operation     := ProcName()
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "TOleAuto"

      RETURN Throw( oErr )
   ENDIF

RETURN Self

//--------------------------------------------------------------------
// Destructor!
PROCEDURE Release() CLASS TOleAuto

   //TraceLog( ::cClassName, ::hObj )

   IF ! Empty( ::hObj )
      //TraceLog( ::cClassName, ::hObj )
      OleReleaseObject( ::hObj )
      //::hObj := NIL
   ENDIF

RETURN

//--------------------------------------------------------------------
METHOD GetActiveObject( cClass ) CLASS TOleAuto

   LOCAL oErr

   IF ValType( cClass ) = 'C'
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
      MessageBox( 0, "Invalid parameter type to constructor TOleAuto():GetActiveObject()!", "OLE Interface", 0 )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------
METHOD OleCollection( xIndex, xValue ) CLASS TOleAuto

   LOCAL xRet

   //TraceLog( PCount(), xIndex, xValue )

   IF PCount() == 1
      RETURN ::Item( xIndex )
   ENDIF

   IF ValType( xIndex ) == 'N' .AND. xIndex < 0
      xIndex += ( ::Count + 1 )
   ENDIF

   TRY
      // ASP Collection syntax.
      xRet := ::_Item( xIndex, xValue )
   CATCH
      xRet := ::SetItem( xIndex, xValue )
   END

RETURN xRet

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
METHOD OleValueEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue = xArg
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
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

//--------------------------------------------------------------------
METHOD OleEnumerate( nEnumOp, nIndex ) CLASS TOleAuto

   LOCAL xRet

   (nIndex)

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         ::pOleEnumerator := ::OleNewEnumerator()
         EXIT

      CASE FOREACH_ENUMERATE
         //xRet := ::Item( nIndex )
         //xRet := ::pOleEnumerator:Next()
         xRet := HB_Inline( ::pOleEnumerator ) ;
         {
            IEnumVARIANT *pEnumVariant = (IEnumVARIANT *) hb_parptr(1);
            ULONG *pcElementFetched = NULL;

            if( pEnumVariant->lpVtbl->Next( pEnumVariant, 1, &RetVal, pcElementFetched ) == S_OK )
            {
               RetValue();
            }
            else
            {
               hb_vmRequestBreak( NULL );
            }
         }

         RETURN xRet
         //EXIT

      CASE FOREACH_END
         HB_Inline( ::pOleEnumerator ) ;
         {
            IEnumVARIANT *pEnumVariant = (IEnumVARIANT *) hb_parptr(1);

            pEnumVariant->lpVtbl->Release( pEnumVariant );
         }

         ::pOleEnumerator := NIL
         EXIT
   END

RETURN Self

#pragma BEGINDUMP

  // -----------------------------------------------------------------------
  static EXCEPINFO excep;

  static DISPID lPropPut = DISPID_PROPERTYPUT;
  static UINT uArgErr;
  HB_EXTERN_BEGIN
  HRESULT HB_EXPORT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );
  HB_EXTERN_END
  static PHB_ITEM SafeArrayToArray( SAFEARRAY *parray, UINT iDim, long* rgIndices, VARTYPE vt );

  HB_EXTERN_BEGIN
  //---------------------------------------------------------------------------//
  HB_EXPORT BSTR hb_oleAnsiToSysString( LPCSTR cString )
  {
     int nConvertedLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, NULL, 0 );

     if( nConvertedLen )
     {
        BSTR bstrString = SysAllocStringLen( NULL, nConvertedLen - 1 );

        if( MultiByteToWideChar( CP_ACP, 0, cString, -1, bstrString, nConvertedLen ) )
        {
           return bstrString;
        }
        else
        {
           SysFreeString( bstrString );
        }
     }

     return NULL;
  }

  //---------------------------------------------------------------------------//
  HB_EXPORT LPWSTR hb_oleAnsiToWide( LPSTR cString )
  {
     int nConvertedLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, NULL, 0 );

     if( nConvertedLen )
     {
        LPWSTR wString = (LPWSTR) hb_xgrab( nConvertedLen * 2 );

        if( MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, wString, nConvertedLen ) )
        {
           //printf( "\nAnsi: '%s'\n", cString );
           //printf( "Wide: '%ls' Len: %i : %i\n", wString, wcslen( wString ), nConvertedLen );

           return wString;
        }
        else
        {
           hb_xfree( wString );
        }
     }

     return NULL;
  }

  //---------------------------------------------------------------------------//
  HB_EXPORT LPSTR hb_oleWideToAnsi( LPWSTR wString )
  {
     int nConvertedLen = WideCharToMultiByte( CP_ACP, 0, wString, -1, NULL, 0, NULL, NULL );

     if( nConvertedLen )
     {
        char *cString = (char *) hb_xgrab( nConvertedLen );

        if( WideCharToMultiByte( CP_ACP, 0, wString, -1, cString, nConvertedLen, NULL, NULL ) )
        {
           //printf( "\nWide: '%ls'\n", wString );
           //printf( "Ansi: '%s'\n", cString );

           return cString;
        }
        else
        {
           hb_xfree( cString );
        }
     }

     return NULL;
  }

  //---------------------------------------------------------------------------//
  HB_EXPORT void hb_oleItemToVariant( VARIANT *pVariant, PHB_ITEM pItem )
  {
     BOOL bByRef;
     VARIANT mVariant;
     VARTYPE vt;
     SAFEARRAYBOUND rgsabound;
     void *pSource;// = NULL;
     unsigned long i;
     char *sString;

     if( HB_IS_BYREF( pItem ) )
     {
        pItem = hb_itemUnRef( pItem );
        bByRef = TRUE;
     }
     else
     {
        bByRef = FALSE;
     }

     VariantClear( pVariant );

     switch( pItem->type )
     {
        case HB_IT_NIL:
          //pVariant->n1.n2.vt = VT_EMPTY;
          break;

        case HB_IT_STRING:
        case HB_IT_MEMO:
        {
          ULONG ulLen = hb_itemGetCLen( pItem );

          sString = hb_itemGetCPtr( pItem );

          // Check for hidden signature of SafeArrayToArray().
          if( (int) (pItem->item.asString.allocated - ulLen) >= 5 &&
              sString[ ulLen ] == 0x7A && sString[ ulLen + 1 ] == 0x7B && sString[ ulLen + 2 ] == 0x7C && sString[ ulLen + 3 ] == 0x7D )
          {
             vt = (VARTYPE) sString[ ulLen + 4 ];
             goto ItemToVariant_StringArray;
          }

          if( bByRef )
          {
             hb_itemPutCRawStatic( pItem, (char *) hb_oleAnsiToSysString( sString ), ulLen * 2 + 1 );

             pVariant->n1.n2.vt   = VT_BYREF | VT_BSTR;
             pVariant->n1.n2.n3.pbstrVal = (BSTR *) &( pItem->item.asString.value );
             //wprintf( L"*** BYREF >%s<\n", *pVariant->n1.n2.n3.bstrVal );
          }
          else
          {
             pVariant->n1.n2.vt   = VT_BSTR;
             pVariant->n1.n2.n3.bstrVal = hb_oleAnsiToSysString( sString );
             //wprintf( L"*** >%s<\n", pVariant->n1.n2.n3.bstrVal );
          }
          break;
        }

        case HB_IT_LOGICAL:
          if( bByRef )
          {
             pVariant->n1.n2.vt = VT_BYREF | VT_BOOL;
             pVariant->n1.n2.n3.pboolVal = (short *) &( pItem->item.asLogical.value ) ;
             *pVariant->n1.n2.n3.pboolVal = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
             //pItem->type = HB_IT_LONG;
          }
          else
          {
             pVariant->n1.n2.vt = VT_BOOL;
             pVariant->n1.n2.n3.boolVal = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
          }
          break;

        case HB_IT_INTEGER:
#if HB_INT_MAX == INT16_MAX
           if( bByRef )
           {
              pVariant->n1.n2.vt = VT_BYREF | VT_I2;
              pVariant->n1.n2.n3.piVal = &( pItem->item.asInteger.value ) ;
           }
           else
           {
              pVariant->n1.n2.vt = VT_I2;
              pVariant->n1.n2.n3.iVal = hb_itemGetNI( pItem );
           }
           break;
#else
           if( bByRef )
           {
              pVariant->n1.n2.vt = VT_BYREF | VT_I4;
              pVariant->n1.n2.n3.plVal = (long *) &( pItem->item.asInteger.value ) ;
           }
           else
           {
              pVariant->n1.n2.vt = VT_I4;
              pVariant->n1.n2.n3.lVal = hb_itemGetNL( pItem );
           }
           break;
#endif
        case HB_IT_LONG:
#if HB_LONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
           if( bByRef )
           {
              pVariant->n1.n2.vt = VT_BYREF | VT_I4;
              pVariant->n1.n2.n3.plVal = (long *) &( pItem->item.asLong.value ) ;
           }
           else
           {
              pVariant->n1.n2.vt = VT_I4;
              pVariant->n1.n2.n3.lVal = hb_itemGetNL( pItem );
           }
#else
           if( bByRef )
           {
              pVariant->n1.n2.vt = VT_BYREF | VT_I8;
              pVariant->n1.n2.n3.pllVal = &( pItem->item.asLong.value ) ;
           }
           else
           {
              pVariant->n1.n2.vt = VT_I8;
              pVariant->n1.n2.n3.llVal = hb_itemGetNLL( pItem );
           }
#endif
           break;

        case HB_IT_DOUBLE:
           if( bByRef )
           {
              pVariant->n1.n2.vt = VT_BYREF | VT_R8;
              pVariant->n1.n2.n3.pdblVal = &( pItem->item.asDouble.value ) ;
              pItem->type = HB_IT_DOUBLE;
           }
           else
           {
              pVariant->n1.n2.vt   = VT_R8;
              pVariant->n1.n2.n3.dblVal = hb_itemGetND( pItem );
           }
           break;

        case HB_IT_DATE:
        case HB_IT_DATE | HB_IT_TIMEFLAG:
          if( pItem->item.asDate.value == 0 )
          {
             pVariant->n1.n2.vt = VT_NULL;
          }
          else if( bByRef )
          {
             pItem->item.asDouble.value = hb_itemGetDTD( pItem ) - (double) 2415019;
             pItem->type = HB_IT_DOUBLE;

             pVariant->n1.n2.vt = VT_BYREF | VT_DATE;
             pVariant->n1.n2.n3.pdblVal = &( pItem->item.asDouble.value ) ;
          }
          else
          {
             pVariant->n1.n2.vt = VT_DATE;
             pVariant->n1.n2.n3.dblVal = hb_itemGetDTD( pItem ) - (double) 2415019;
          }
          break;

        case HB_IT_POINTER:
           pVariant->n1.n2.vt = VT_PTR;
           pVariant->n1.n2.n3.byref = hb_itemGetPtr( pItem );
           break;

        case HB_IT_ARRAY:
        {
           if( HB_IS_OBJECT( pItem ) )
           {
              if( hb_clsIsParent( pItem->item.asArray.value->uiClass , "TOLEAUTO" ) )
              {
                 IDispatch *pDisp;// = NULL;

                 hb_vmPushSymbol( s_pSym_hObj->pSymbol );
                 hb_vmPush( pItem );
                 hb_vmSend( 0 );

                 pDisp = (IDispatch *) hb_parnl( -1 );
                 pDisp->lpVtbl->AddRef( pDisp );

                 //TraceLog( NULL, "Dispatch: in: %s(%i)%ld\n", pDisp, __FILE__, __LINE__ );

                 if( bByRef )
                 {
                    pVariant->n1.n2.vt = ( VT_DISPATCH | VT_BYREF );
                    // Hack!!! Using high 4 bytes of the union (llVal)
                    *( (IDispatch **) ( &pVariant->n1.n2.n3.lVal ) + 1 ) = pDisp;
                    pVariant->n1.n2.n3.ppdispVal = (IDispatch **) (&pVariant->n1.n2.n3.lVal ) + 1;
                 }
                 else
                 {
                    pVariant->n1.n2.vt = VT_DISPATCH;
                    pVariant->n1.n2.n3.pdispVal = pDisp;
                 }
              }
              // MUST be before "VTWRAPPER"
              else if( hb_clsIsParent( pItem->item.asArray.value->uiClass , "VTARRAYWRAPPER" ) )
              {
                 // vt := oVTArray:vt
                 hb_vmPushSymbol( s_pSym_vt->pSymbol );
                 hb_vmPush( pItem );
                 hb_vmSend( 0 );

                 vt = (VARTYPE) hb_parnl(-1);

                 // aArray := oVTArray:Value
                 hb_vmPushSymbol( s_pSym_Value->pSymbol );
                 hb_vmPush( pItem );
                 hb_vmSend( 0 );

                 // Intentionally not using hb_itemCopy() or hb_itemForwardValue()
                 pItem = hb_stackReturnItem();

                 if( ( vt == VT_I1 || vt == VT_UI1 ) && HB_IS_STRING( pItem ) )
                 {
                    SAFEARRAY *parray;

                    sString = hb_itemGetCPtr( pItem );

                   ItemToVariant_StringArray:

                    rgsabound.cElements = hb_itemGetCLen( pItem );
                    rgsabound.lLbound = 0;

                    parray = SafeArrayCreate( vt, 1, &rgsabound );

                    if( bByRef )
                    {
                       pVariant->n1.n2.vt = ( VT_ARRAY | VT_BYREF | vt );
                       // Hack!!! Using high 4 bytes of the union (llVal)
                       *( (SAFEARRAY **) ( &pVariant->n1.n2.n3.lVal ) + 1 ) = parray;
                       pVariant->n1.n2.n3.pparray = (SAFEARRAY **) (&pVariant->n1.n2.n3.lVal ) + 1;
                    }
                    else
                    {
                       pVariant->n1.n2.vt = ( VT_ARRAY | vt );
                       pVariant->n1.n2.n3.parray = parray;
                    }

                    for( i = 0; i < rgsabound.cElements; i++ )
                    {
                       SafeArrayPutElement( parray, (LONG *) &i, &( sString[i]) );
                    }

                    break;
                 }

                 VariantInit( &mVariant );
                 pSource = &mVariant.n1.n2.n3.cVal;

                 goto ItemToVariant_ProcessArray;
              }
              else if( hb_clsIsParent( pItem->item.asArray.value->uiClass , "VTWRAPPER" ) )
              {
                 // vt := oVT:vt
                 hb_vmPushSymbol( s_pSym_vt->pSymbol );
                 hb_vmPush( pItem );
                 hb_vmSend( 0 );

                 pVariant->n1.n2.vt = (VARTYPE) hb_parnl(-1);

                 //value := oVT:value
                 hb_vmPushSymbol( s_pSym_Value->pSymbol );
                 hb_vmPush( pItem );
                 hb_vmSend( 0 );

                 switch( pVariant->n1.n2.vt )
                 {
                    case VT_UNKNOWN:
                       pVariant->n1.n2.n3.punkVal = (IUnknown *) hb_parptr( -1 );
                       break;

                    case ( VT_UNKNOWN | VT_BYREF ):
                       // Hack!!! Using high 4 bytes of the union (llVal)
                       *( (IUnknown **) ( &pVariant->n1.n2.n3.lVal ) + 1 ) = (IUnknown *) hb_parptr( -1 );
                       pVariant->n1.n2.n3.ppunkVal = (IUnknown **) (&pVariant->n1.n2.n3.lVal ) + 1;
                       break;

                    case VT_ERROR:
                       pVariant->n1.n2.n3.scode = hb_parni(-1);
                       break;

                    default:
                       TraceLog( NULL, "Unexpected VT type %p in: %s(%i)!\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
                 }

                 break;
              }
              else
              {
                 TraceLog( NULL, "Class: '%s' not suported!\n", hb_objGetClsName( pItem ) );
              }
           }
           else
           {
              unsigned long  i;
              SAFEARRAY *parray;

              vt = VT_VARIANT;
              VariantInit( &mVariant );
              pSource = &mVariant;

            ItemToVariant_ProcessArray:

              rgsabound.cElements = hb_arrayLen( pItem );
              rgsabound.lLbound = 0;

              //TraceLog( NULL, "ItemToVariant() Array len: %i type: %i ByRef: %i in: %s(%i) \n", rgsabound.cElements, vt, bByRef, __FILE__, __LINE__ );

              parray = SafeArrayCreate( vt, 1, &rgsabound );

              if( bByRef )
              {
                 pVariant->n1.n2.vt = ( VT_ARRAY | VT_BYREF | vt );
                 // Hack!!! Using high 4 bytes of the union (llVal)
                 *( (SAFEARRAY **) ( &pVariant->n1.n2.n3.lVal ) + 1 ) = parray;
                 pVariant->n1.n2.n3.pparray = (SAFEARRAY **) (&pVariant->n1.n2.n3.lVal ) + 1;
              }
              else
              {
                 pVariant->n1.n2.vt = ( VT_ARRAY | vt );
                 pVariant->n1.n2.n3.parray = parray;
              }

              for( i = 0; i < rgsabound.cElements; i++ )
              {
                 hb_oleItemToVariant( &mVariant, hb_arrayGetItemPtr( pItem, i + 1 ) );
                 SafeArrayPutElement( parray, (LONG *) &i, pSource );
                 VariantClear( &mVariant );
              }
           }
        }
        break;

        default:
        {
           TraceLog( NULL, "Unexpected type %p in: %s(%i)!\n", pItem->type, __FILE__, __LINE__ );
        }
     }
  }

  HB_EXTERN_END

  //---------------------------------------------------------------------------//
  HB_FUNC( ANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
  {
     char *cString = hb_parc( 1 );

     if( cString )
     {
        LPWSTR wString = hb_oleAnsiToWide( cString );

        if( wString )
        {
           hb_retclenAdoptRaw( (char *) wString, ( wcslen( wString ) + 1 ) * 2 );
           //printf( "Returning: '%ls' Len: %i\n", hb_stackReturnItem()->item.asString.value, hb_stackReturnItem()->item.asString.length );
           return;
        }
     }

     hb_ret();
     return;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( WIDETOANSI )  // ( cWideStr, nLen ) -> cAnsiStr
  {
     LPWSTR wString = (LPWSTR) hb_parc( 1 );

     if( wString )
     {
        char *cString = hb_oleWideToAnsi( wString );

        if( cString )
        {
           hb_retclenAdopt( cString, strlen( cString ) );
           return;
        }
     }

     hb_ret();
     return;
  }


  //---------------------------------------------------------------------------//
  static PHB_ITEM * GetParams( DISPPARAMS *pDispParams, int nOffset )
  {
     VARIANTARG * pArgs = NULL;
     int n, nArgs, nArg;
     //BOOL bByRef;
     PHB_ITEM *aPrgParams = NULL;

     nArgs = hb_pcount() - nOffset;

     if( nArgs > 0 )
     {
        pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * nArgs );
        aPrgParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * nArgs );

        //printf( "Args: %i\n", nArgs );

        for( n = 0; n < nArgs; n++ )
        {
           // Parameters are processed in reversed order.
           nArg = nArgs - n;
           VariantInit( &( pArgs[ n ] ) );

           aPrgParams[ n ] = hb_stackItemFromBase( nArg + nOffset );

           //TraceLog( NULL, "N: %i Arg: %i Type: %i %i ByRef: %i\n", n, nArg, pParam->type, aPrgParams[ n ]->type, bByRef );

           hb_oleItemToVariant( &( pArgs[ n ] ), aPrgParams[ n ] );
        }
     }

     pDispParams->rgvarg            = pArgs;
     pDispParams->cArgs             = nArgs;
     pDispParams->rgdispidNamedArgs = 0;
     pDispParams->cNamedArgs        = 0;

     return aPrgParams;
  }

  //---------------------------------------------------------------------------//
  static void FreeParams( DISPPARAMS *pDispParams, PHB_ITEM *aPrgParams )
  {
     if( pDispParams->cArgs > 0 )
     {
        IDispatch *pDisp = NULL;
        int n; //, nParam;
        char *sString;
        VARIANT *pVariant;
        PHB_ITEM pItem;
        BOOL bByRef;

        for( n = 0; n < ( int ) pDispParams->cArgs; n++ )
        {
           pVariant = &( pDispParams->rgvarg[ n ] );
           pItem = aPrgParams[ n ];

           if( HB_IS_BYREF( pItem ) )
           {
              bByRef = TRUE;
              pItem = hb_itemUnRef( pItem );
           }
           else
           {
              bByRef = FALSE;
           }

           //nParam = pDispParams->cArgs - n;

           //TraceLog( NULL, "*** N: %i, Param: %i Type: %i\n", n, nParam, pVariant->n1.n2.vt);

           if( bByRef )
           {
              switch( pVariant->n1.n2.vt )
              {
                 case VT_BYREF | VT_BSTR:
                   SysFreeString( *pVariant->n1.n2.n3.pbstrVal );
                   sString = hb_oleWideToAnsi( *( pVariant->n1.n2.n3.pbstrVal ) );
                   hb_itemPutCPtr( pItem, sString, strlen( sString ) );
                   break;

                 case VT_BSTR:
                   sString = hb_oleWideToAnsi( pVariant->n1.n2.n3.bstrVal );
                   hb_itemPutCPtr( pItem, sString, strlen( sString ) );
                   break;

                 case VT_BYREF | VT_BOOL:
                   //( pItem )->type = HB_IT_LOGICAL;
                   hb_itemPutL( pItem, *pVariant->n1.n2.n3.pboolVal == VARIANT_FALSE ? FALSE : TRUE );
                   break;

                 case VT_BOOL:
                   hb_itemPutL( pItem, pVariant->n1.n2.n3.boolVal == VARIANT_FALSE ? FALSE : TRUE );
                   break;

                 case ( VT_BYREF | VT_DISPATCH ):
                   if( *pVariant->n1.n2.n3.ppdispVal == NULL )
                   {
                      hb_itemClear( pItem );
                      break;
                   }
                   else
                   {
                      pDisp = *pVariant->n1.n2.n3.ppdispVal;
                   }
                   // Intentionally fall through.

                 case VT_DISPATCH:
                   if( pVariant->n1.n2.vt == VT_DISPATCH )
                   {
                      if( pVariant->n1.n2.n3.pdispVal == NULL )
                      {
                         hb_itemClear( pItem );
                         break;
                      }
                      else
                      {
                         pDisp = pVariant->n1.n2.n3.pdispVal;
                      }
                   }

                   OleAuto.type = HB_IT_NIL;

                   if( s_pSym_TOleAuto )
                   {
                      hb_vmPushSymbol( s_pSym_TOleAuto->pSymbol );
                      hb_vmPushNil();
                      hb_vmDo( 0 );

                      hb_itemForwardValue( &OleAuto, hb_stackReturnItem() );
                   }

                   if( s_pSym_New && OleAuto.type )
                   {
                      // Implemented in :New()
                      //pDisp->lpVtbl->AddRef( pDisp );

                      //TOleAuto():New( nDispatch )
                      hb_vmPushSymbol( s_pSym_New->pSymbol );
                      hb_itemPushForward( &OleAuto );
                      hb_vmPushLong( ( LONG ) pDisp );
                      hb_vmSend( 1 );

                      hb_itemForwardValue( pItem, hb_stackReturnItem() );
                   }
                   break;

                 case VT_BYREF | VT_I2:
                   hb_itemPutNI( pItem, ( int ) *pVariant->n1.n2.n3.piVal );
                   break;

                 case VT_I2:
                   hb_itemPutNI( pItem, ( int ) pVariant->n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_I4:
                   hb_itemPutNL( pItem, ( LONG ) *pVariant->n1.n2.n3.plVal );
                   break;

                 case VT_I4:
                   hb_itemPutNL( pItem, ( LONG ) pVariant->n1.n2.n3.lVal );
                   break;

    #ifndef HB_LONG_LONG_OFF
                 case VT_BYREF | VT_I8:
                   hb_itemPutNLL( pItem, ( LONGLONG ) *pVariant->n1.n2.n3.pllVal );
                   break;
    #endif

    #ifndef HB_LONG_LONG_OFF
                 case VT_I8:
                   hb_itemPutNLL( pItem, ( LONGLONG ) pVariant->n1.n2.n3.llVal );
                   break;
    #endif

                 case VT_BYREF | VT_R8:
                   hb_itemPutND( pItem, *pVariant->n1.n2.n3.pdblVal );
                   break;

                 case VT_R8:
                   hb_itemPutND( pItem, pVariant->n1.n2.n3.dblVal );
                   break;

                 case VT_BYREF | VT_DATE:
                   hb_itemPutDTD( pItem, *pVariant->n1.n2.n3.pdblVal + (double) 2415019 );
                   if( pItem->item.asDate.time )
                      pItem->item.asDate.time++;
                   break;

                 case VT_DATE:
                   hb_itemPutDTD( pItem,  pVariant->n1.n2.n3.dblVal + (double) 2415019 );
                   if( pItem->item.asDate.time )
                      pItem->item.asDate.time++;
                   break;

                 case VT_BYREF | VT_EMPTY:
                 case VT_EMPTY:
                   hb_itemClear( pItem );
                   break;

                 case VT_BYREF | VT_VARIANT:
                   hb_oleItemToVariant( pVariant->n1.n2.n3.pvarVal, pItem );
                   break;

                 default:
                   if( (VARTYPE) ( pVariant->n1.n2.vt & ( VT_BYREF | VT_ARRAY ) ) == (VARTYPE) ( VT_BYREF | VT_ARRAY ) )
                   {
                      VARTYPE vt;
                      PHB_ITEM pArray;
                      UINT iDims = SafeArrayGetDim( *pVariant->n1.n2.n3.pparray );
                      long *rgIndices = (long *) hb_xgrab( sizeof(long) * iDims );

                      vt = pVariant->n1.n2.vt;
                      vt &= ~VT_ARRAY;
                      vt &= ~VT_BYREF;

                      pArray = SafeArrayToArray( *pVariant->n1.n2.n3.pparray, iDims, rgIndices, vt );

                      hb_xfree( (void *) rgIndices );

                      hb_itemForwardValue( pItem, pArray );
                      hb_itemRelease( pArray );
                   }
                   else
                   {
                      TraceLog( NULL, "Unexpected type %p in: %s(%i)!\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
                   }
              }
           }
           else
           {
              if( pVariant->n1.n2.vt & VT_BYREF )
              {
                 TraceLog( NULL, "Unexpected type %p in: %s(%i)!\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
              }
           }

           VariantClear( &(pDispParams->rgvarg[ n ] ) );
        }

        hb_xfree( ( LPVOID ) pDispParams->rgvarg );
        hb_xfree( ( LPVOID ) aPrgParams );
     }
  }

  //---------------------------------------------------------------------------//
  static PHB_ITEM SafeArrayToArray( SAFEARRAY *parray, UINT iDim, long* rgIndices, VARTYPE vt )
  {
     long iFrom, iTo, iLen, i;
     PHB_ITEM pArray = hb_itemNew( NULL );;

     if( parray == NULL )
     {
        hb_arrayNew( pArray, 0 );
        return pArray;
     }

     SafeArrayGetLBound( parray, iDim, &iFrom );
     SafeArrayGetUBound( parray, iDim, &iTo );

     iLen = iTo - iFrom + 1;

     if( iDim > 1 )
     {
        PHB_ITEM pSubArray;

        hb_arrayNew( pArray, iLen );

        for( i = iFrom; i <= iTo; i++ )
        {
           rgIndices[ iDim - 1 ] = i;

           //printf( "   Sub: %i\n", i );

           pSubArray = SafeArrayToArray( parray, iDim - 1, rgIndices, vt );
           hb_arraySetForward( pArray, i - iFrom + 1, pSubArray );
           hb_itemRelease( pSubArray );
        }
     }
     else
     {
        VARIANT mElem;
        void *pTarget;
        char *sArray = NULL;

        VariantInit( &mElem );

        if( vt == VT_VARIANT )
        {
           hb_arrayNew( pArray, iLen );

           pTarget = &mElem;
        }
        else
        {
           if( vt == VT_I1 || vt == VT_UI1 )
           {
              // Ugly hack, but needed to allocate our signature as hidden bytes!
              hb_itemPutCL( pArray, NULL, 0 );
              HB_STRING_ALLOC( pArray, (ULONG)(iLen + 5) );
              pArray->item.asString.length = iLen;

              sArray = hb_itemGetCPtr( pArray );

              sArray[ iLen ]     = 0x7A;
              sArray[ iLen + 1 ] = 0x7B;
              sArray[ iLen + 2 ] = 0x7C;
              sArray[ iLen + 3 ] = 0x7D;
              sArray[ iLen + 4 ] = (char)(vt);

              pTarget = NULL;
           }
           else
           {
              hb_arrayNew( pArray, iLen );

              pTarget = &mElem.n1.n2.n3.cVal;
           }
        }

        for( i = iFrom; i <= iTo; i++ )
        {
           rgIndices[ iDim - 1 ] = i;

           if( vt != VT_VARIANT )
           {
              // Get cleared on VariantClear() - don't place out of loop!
              mElem.n1.n2.vt = vt;

              if( vt == VT_I1 || vt == VT_UI1 )
              {
                 SafeArrayGetElement( parray, rgIndices, &( sArray[ i - iFrom ] ) );

                 continue;
              }
           }

           if( SUCCEEDED( SafeArrayGetElement( parray, rgIndices, pTarget ) ) )
           {
              //TraceLog( NULL, "Type: %p in: %s(%i)\n", mElem.n1.n2.vt, __FILE__, __LINE__ );

              hb_oleVariantToItem( pArray->item.asArray.value->pItems + ( i - iFrom ), &mElem );

              VariantClear( &mElem );
           }
        }
     }

     //TraceLog( NULL, "Return len: %i\n", pArray->item.asArray.value->ulLen );

     // Wrap our array with VTArrayWrapper() class ( aArray := VTArrayWrapper( vt, aArray) )
     if( HB_IS_ARRAY( pArray ) && vt != VT_VARIANT )
     {
        PHB_ITEM pVT = hb_itemPutNL( hb_itemNew( NULL ), (LONG) vt );

        hb_vmPushSymbol( s_pSym_VTArrayWrapper->pSymbol );
        hb_vmPushNil();
        hb_itemPushForward( pVT );
        hb_itemPushForward( pArray );
        hb_vmDo( 2 );

        hb_itemForwardValue( pArray, hb_stackReturnItem() );

        hb_itemRelease( pVT );
     }

     return pArray;
  }

  //---------------------------------------------------------------------------//
  HRESULT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant )
  {
     PHB_ITEM pOleAuto;
     IUnknown  *pUnk   = NULL;
     IDispatch *pDisp  = NULL;
     SAFEARRAY *parray;// = NULL;

     hb_itemClear( pItem );

     // Don't "optimize" (VT_ARRAY | VT_VARIANT) must not match!
     while( pVariant->n1.n2.vt == ( VT_BYREF | VT_VARIANT ) || pVariant->n1.n2.vt == VT_VARIANT || pVariant->n1.n2.vt == VT_BYREF )
     {
        pVariant = pVariant->n1.n2.n3.pvarVal;
     }

     switch( pVariant->n1.n2.vt )
     {
        case VT_BSTR | VT_BYREF:
        case VT_BSTR:
        {
           char *sString;

           if( pVariant->n1.n2.vt & VT_BYREF )
           {
              sString = hb_oleWideToAnsi( *pVariant->n1.n2.n3.pbstrVal );
           }
           else
           {
              sString = hb_oleWideToAnsi( pVariant->n1.n2.n3.bstrVal );
           }

           if( sString )
           {
              hb_itemPutCPtr( pItem, sString, strlen( sString ) );
           }
           else
           {
              hb_itemPutC( pItem, NULL );
           }

           break;
        }

        case VT_BOOL | VT_BYREF:
           hb_itemPutL( pItem, *pVariant->n1.n2.n3.pboolVal == VARIANT_FALSE ? FALSE : TRUE );
           break;

        case VT_BOOL:
           hb_itemPutL( pItem, pVariant->n1.n2.n3.boolVal == VARIANT_FALSE ? FALSE : TRUE );
           break;

        case ( VT_UNKNOWN | VT_BYREF ):
           pUnk = *pVariant->n1.n2.n3.ppunkVal;
           // Intentionally fall through

        case VT_UNKNOWN:
           if( pVariant->n1.n2.vt == VT_UNKNOWN )
           {
              pUnk = pVariant->n1.n2.n3.punkVal;
           }

           if( pUnk )
           {
              // pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) &pDisp );
         // Not sure if this is correct, but by this change GCC now quiet
         // and also the other compilers
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
              pUnk->lpVtbl->QueryInterface( pUnk, (REFIID)  IID_IDispatch, (void **) pDisp );
#else
              pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) pDisp );
#endif
           }
           // Intentionally fall through

        case ( VT_DISPATCH | VT_BYREF ):
           if( pVariant->n1.n2.vt == ( VT_DISPATCH | VT_BYREF ) )
           {
              pDisp = *pVariant->n1.n2.n3.ppdispVal;
           }
           // Intentionally fall through

        case VT_DISPATCH:
           if( pVariant->n1.n2.vt == VT_DISPATCH )
           {
              pDisp = pVariant->n1.n2.n3.pdispVal;
           }

           if( pDisp == NULL )
           {
              if( pUnk )
              {
                 PHB_ITEM pVT = hb_itemPutNL( hb_itemNew( NULL ), (LONG) pVariant->n1.n2.vt );
                 PHB_ITEM pUnknown = hb_itemPutPtr( hb_itemNew( NULL ), (void *) pUnk );

                 hb_vmPushSymbol( s_pSym_VTWrapper->pSymbol );
                 hb_vmPushNil();
                 hb_itemPushForward( pVT );
                 hb_itemPushForward( pUnknown );
                 hb_vmDo( 2 );

                 if( pItem != hb_stackReturnItem() )
                 {
                    hb_itemForwardValue( pItem, hb_stackReturnItem() );
                 }

                 hb_itemRelease( pVT );
                 hb_itemRelease( pUnknown );
              }

              break;
           }

           pOleAuto = hb_itemNew( NULL );

           hb_vmPushSymbol( s_pSym_TOleAuto->pSymbol );
           hb_vmPushNil();
           hb_vmDo( 0 );

           // Safety!
           hb_vmRequestReset();

           hb_itemForwardValue( pOleAuto, hb_stackReturnItem() );

           if( pOleAuto->type )
           {
              //TOleAuto():New( nDispatch )
              hb_vmPushSymbol( s_pSym_New->pSymbol );
              hb_itemPushForward( pOleAuto );
              hb_vmPushLong( ( LONG ) pDisp );
              hb_vmSend( 1 );

              // If retrieved from IUnknown than doubly added!
              if( pVariant->n1.n2.vt == VT_UNKNOWN || pVariant->n1.n2.vt == ( VT_UNKNOWN | VT_BYREF ) )
              {
                 pDisp->lpVtbl->Release( pDisp );
              }

              hb_itemRelease( pOleAuto );

              // Safety!
              hb_vmRequestReset();

              if( pItem != hb_stackReturnItem() )
              {
                 hb_itemForwardValue( pItem, hb_stackReturnItem() );
              }

              //printf( "Dispatch: %ld %ld\n", ( LONG ) pDisp, (LONG) hb_stackReturnItem()->item.asArray.value );
           }
           break;

        case VT_I1 | VT_BYREF:     // Byte
        case VT_UI1 | VT_BYREF:
           hb_itemPutNI( pItem, ( short ) *pVariant->n1.n2.n3.pbVal );
           break;

        case VT_I1:     // Byte
        case VT_UI1:
           hb_itemPutNI( pItem, ( short ) pVariant->n1.n2.n3.bVal );
           break;

        case VT_I2 | VT_BYREF:     // Short (2 bytes)
        case VT_UI2 | VT_BYREF:
           hb_itemPutNI( pItem, ( short ) *pVariant->n1.n2.n3.piVal );
           break;

        case VT_I2:     // Short (2 bytes)
        case VT_UI2:
           hb_itemPutNI( pItem, ( short ) pVariant->n1.n2.n3.iVal );
           break;

        case VT_I4 | VT_BYREF:     // Long (4 bytes)
        case VT_UI4 | VT_BYREF:
        case VT_INT | VT_BYREF:
        case VT_UINT | VT_BYREF:
           hb_itemPutNL( pItem, ( LONG ) *pVariant->n1.n2.n3.plVal );
           break;

        case VT_I4:     // Long (4 bytes)
        case VT_UI4:
        case VT_INT:
        case VT_UINT:
           hb_itemPutNL( pItem, ( LONG ) pVariant->n1.n2.n3.lVal );
           break;

        case VT_R4 | VT_BYREF:     // Single
           hb_itemPutND( pItem, *pVariant->n1.n2.n3.pfltVal );
           break;

        case VT_R4:     // Single
           hb_itemPutND( pItem, pVariant->n1.n2.n3.fltVal );
           break;

        case VT_R8 | VT_BYREF:     // Double
           hb_itemPutND( pItem, *pVariant->n1.n2.n3.pdblVal );
           break;

        case VT_R8:     // Double
           hb_itemPutND( pItem, pVariant->n1.n2.n3.dblVal );
           break;

        case VT_CY | VT_BYREF:     // Currency
        case VT_CY:     // Currency
        {
           double tmp = 0;

           if( pVariant->n1.n2.vt & VT_BYREF )
           {
              VarR8FromCy( *pVariant->n1.n2.n3.pcyVal, &tmp );
           }
           else
           {
              VarR8FromCy( pVariant->n1.n2.n3.cyVal, &tmp );
           }

           hb_itemPutND( pItem, tmp );
           break;
        }

        case VT_DECIMAL | VT_BYREF: // Decimal
        case VT_DECIMAL: // Decimal
        {
           double tmp = 0;

           if( pVariant->n1.n2.vt & VT_BYREF )
           {
              VarR8FromDec( pVariant->n1.n2.n3.pdecVal, &tmp );
           }
           else
           {
              VarR8FromDec( &pVariant->n1.decVal, &tmp );
           }

           hb_itemPutND( pItem, tmp );
           break;
        }

        case VT_DATE | VT_BYREF:
           hb_itemPutDTD( pItem, *pVariant->n1.n2.n3.pdblVal + (double) 2415019 );
           if( pItem->item.asDate.time )
              pItem->item.asDate.time++;
           break;

        case VT_DATE:
           hb_itemPutDTD( pItem,  pVariant->n1.n2.n3.dblVal + (double) 2415019 );
           if( pItem->item.asDate.time )
              pItem->item.asDate.time++;
           break;

        case VT_EMPTY | VT_BYREF:
        case VT_NULL | VT_BYREF:
        case VT_EMPTY:
        case VT_NULL:
           break;

          /*
          case VT_VARIANT:
             hb_oleVariantToItem( pItem, pVariant->n1.n2.n3.pvarVal );
             break;
          */

        case VT_PTR:
           hb_itemPutPtr( pItem, pVariant->n1.n2.n3.byref );
           break;

        default:
           if( pVariant->n1.n2.vt & VT_ARRAY )
           {
              UINT iDims;
              long * rgIndices;
              PHB_ITEM pArray;
              VARTYPE vt;

              if( pVariant->n1.n2.vt & VT_BYREF )
              {
                 parray = *pVariant->n1.n2.n3.pparray;
              }
              else
              {
                 parray = pVariant->n1.n2.n3.parray;
              }

              if( parray )
              {
                 iDims = SafeArrayGetDim( parray );
                 rgIndices = (long *) hb_xgrab( sizeof(long) * iDims );

                 vt = pVariant->n1.n2.vt;
                 vt &= ~VT_ARRAY;
                 vt &= ~VT_BYREF;

                 //TraceLog( NULL, "Type: %p in: %s(%i)\n", vt, __FILE__, __LINE__ );

                 pArray = SafeArrayToArray( parray, iDims, rgIndices, vt );

                 hb_xfree( (void *) rgIndices );

                 hb_itemForwardValue( pItem, pArray );
                 hb_itemRelease( pArray );
              }
              else
              {
                 hb_arrayNew( pItem, 0 );
              }
           }
           else
           {
              TraceLog( NULL, "Unexpected type %p in: %s(%i)!\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
              return E_FAIL;
           }
     }

     //VariantClear( pVariant );

     return S_OK;
  }

  //---------------------------------------------------------------------------//
  static void RetValue( void )
  {
     hb_oleVariantToItem( hb_stackReturnItem(), &RetVal );

     VariantClear( &RetVal );

     return;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLESHOWEXCEPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source, description;

        source = hb_oleWideToAnsi( excep.bstrSource );
        description = hb_oleWideToAnsi( excep.bstrDescription );

        MessageBox( NULL, description, source, MB_ICONHAND );

        hb_xfree( source );
        hb_xfree( description );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLEEXCEPTIONSOURCE )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source;

        source = hb_oleWideToAnsi( excep.bstrSource );
        hb_retcAdopt( source );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLEEXCEPTIONDESCRIPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR description;

        description = hb_oleWideToAnsi( excep.bstrDescription );
        hb_retcAdopt( description );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLEERROR )
  {
     hb_retnl( (LONG) s_nOleError );
  }

  //---------------------------------------------------------------------------//
  static char * Ole2TxtError( void )
  {
     switch( (LONG) s_nOleError )
     {
        case S_OK:
           return "S_OK";

        case CO_E_CLASSSTRING:
           return "CO_E_CLASSSTRING";

        case OLE_E_WRONGCOMPOBJ:
           return "OLE_E_WRONGCOMPOBJ";

        case REGDB_E_CLASSNOTREG:
           return "REGDB_E_CLASSNOTREG";

        case REGDB_E_WRITEREGDB:
           return "REGDB_E_WRITEREGDB";

        case E_FAIL:
           return "E_FAIL";

        case E_OUTOFMEMORY:
           return "E_OUTOFMEMORY";

        case E_NOTIMPL:
           return "E_NOTIMPL";

        case E_INVALIDARG:
           return "E_INVALIDARG";

        case E_UNEXPECTED:
           return "E_UNEXPECTED";

        case DISP_E_UNKNOWNNAME:
           return "DISP_E_UNKNOWNNAME";

        case DISP_E_UNKNOWNLCID:
           return "DISP_E_UNKNOWNLCID";

        case DISP_E_BADPARAMCOUNT:
           return "DISP_E_BADPARAMCOUNT";

        case DISP_E_BADVARTYPE:
           return "DISP_E_BADVARTYPE";

        case DISP_E_EXCEPTION:
           return "DISP_E_EXCEPTION";

        case DISP_E_MEMBERNOTFOUND:
           return "DISP_E_MEMBERNOTFOUND";

        case DISP_E_NONAMEDARGS:
           return "DISP_E_NONAMEDARGS";

        case DISP_E_OVERFLOW:
           return "DISP_E_OVERFLOW";

        case DISP_E_PARAMNOTFOUND:
           return "DISP_E_PARAMNOTFOUND";

        case DISP_E_TYPEMISMATCH:
           return "DISP_E_TYPEMISMATCH";

        case DISP_E_UNKNOWNINTERFACE:
           return "DISP_E_UNKNOWNINTERFACE";

        case DISP_E_PARAMNOTOPTIONAL:
           return "DISP_E_PARAMNOTOPTIONAL";

        case CO_E_SERVER_EXEC_FAILURE:
           return "CO_E_SERVER_EXEC_FAILURE";

        case MK_E_UNAVAILABLE:
           return "MK_E_UNAVAILABLE";

        default:
           TraceLog( NULL, "TOleAuto Error %p\n", s_nOleError );
           return "Unknown error";
     };
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLE2TXTERROR )
  {
     hb_retc( Ole2TxtError() );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( MESSAGEBOX )
  {
     hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ) ) );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( CREATEOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] [, cLicense] )
  {
     BSTR bstrClassID;
     IID ClassID, iid;
     LPIID riid = (LPIID) &IID_IDispatch;
     void *pDisp = NULL; // IDispatch
     /* void *
      * used intentionally to inform compiler that there is no
      * strict-aliasing
      */
     bstrClassID = hb_oleAnsiToSysString( hb_parcx( 1 ) );

     if( hb_parcx( 1 )[ 0 ] == '{' )
     {
        s_nOleError = CLSIDFromString( bstrClassID, (LPCLSID) &ClassID );
     }
     else
     {
        s_nOleError = CLSIDFromProgID( bstrClassID, (LPCLSID) &ClassID );
     }

     SysFreeString( bstrClassID );

     //TraceLog( NULL, "Result: %p\n", s_nOleError );

     if( ISCHAR(2) )
     {
        if( hb_parcx( 2 )[ 0 ] == '{' )
        {
           bstrClassID = hb_oleAnsiToSysString( hb_parc( 2 ) );
           s_nOleError = CLSIDFromString( bstrClassID, &iid );
           SysFreeString( bstrClassID );
        }
        else
        {
           memcpy( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
        }

        riid = &iid;
     }

     if( SUCCEEDED( s_nOleError ) )
     {
        if( ISCHAR(3) )
        {
           IClassFactory2 *pCF;

#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = CoGetClassObject( (REFCLSID)  ClassID, CLSCTX_SERVER, NULL, (REFIID) IID_IClassFactory2, (LPVOID *) &pCF );
#else
           s_nOleError = CoGetClassObject( (REFCLSID) &ClassID, CLSCTX_SERVER, NULL, (REFIID) (LPIID) &IID_IClassFactory2, (LPVOID *) &pCF );
#endif

           if( SUCCEEDED( s_nOleError ) )
           {
              BSTR bstrLic = hb_oleAnsiToSysString( hb_parc(3) );

              s_nOleError = pCF->lpVtbl->CreateInstanceLic( pCF, NULL, NULL, (REFIID) riid, bstrLic, &pDisp );

              SysFreeString( bstrLic );
              pCF->lpVtbl->Release( pCF );
           }
        }
        else
        {
           //TraceLog( NULL, "Class: %i\n", ClassID );
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = CoCreateInstance( (REFCLSID)  ClassID, NULL, CLSCTX_SERVER, (REFIID) riid, &pDisp );
#else
           s_nOleError = CoCreateInstance( (REFCLSID) &ClassID, NULL, CLSCTX_SERVER, (REFIID) riid, &pDisp );
#endif
           //TraceLog( NULL, "Result: %p\n", s_nOleError );
        }
     }

     hb_retnl( ( LONG ) pDisp );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( GETOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
  {
     BSTR bstrClassID;
     IID ClassID, iid;
     LPIID riid = (LPIID) &IID_IDispatch;
     IUnknown *pUnk = NULL;
     void *pDisp = NULL; // IDispatch
     /* void *
      * used intentionally to inform compiler that there is no
      * strict-aliasing
      */

     bstrClassID = hb_oleAnsiToSysString( hb_parcx( 1 ) );

     if( hb_parcx( 1 )[ 0 ] == '{' )
     {
        s_nOleError = CLSIDFromString( bstrClassID, (LPCLSID) &ClassID );
     }
     else
     {
        s_nOleError = CLSIDFromProgID( bstrClassID, (LPCLSID) &ClassID );
     }

     //s_nOleError = ProgIDFromCLSID( &ClassID, &pOleStr );
     //wprintf( L"Result %i ProgID: '%s'\n", s_nOleError, pOleStr );

     SysFreeString( bstrClassID );

     if( hb_pcount() == 2 )
     {
        if( hb_parcx( 2 )[ 0 ] == '{' )
        {
           bstrClassID = hb_oleAnsiToSysString( hb_parcx( 2 ) );
           s_nOleError = CLSIDFromString( bstrClassID, &iid );
           SysFreeString( bstrClassID );
        }
        else
        {
           memcpy( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
        }

        riid = &iid;
     }

     if( SUCCEEDED( s_nOleError ) )
     {
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
        s_nOleError = GetActiveObject( (REFCLSID)  ClassID, NULL, &pUnk );
#else
        s_nOleError = GetActiveObject( (REFCLSID) &ClassID, NULL, &pUnk );
#endif

        if( SUCCEEDED( s_nOleError ) )
        {
           s_nOleError = pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) riid, &pDisp );

           pUnk->lpVtbl->Release( pUnk );

           if( SUCCEEDED( s_nOleError ) )
           {
              hb_retnl( ( LONG ) pDisp );
           }
        }
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLEADDREF ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch *pDisp = ( IDispatch * ) hb_parnl( 1 );

     //TraceLog( NULL, "OleAddRef( %p )\n", pDisp );

     s_nOleError = pDisp->lpVtbl->AddRef( pDisp );

     hb_retnl( s_nOleError );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLERELEASEOBJECT ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch *pDisp = ( IDispatch * ) hb_parnl( 1 );

     //TraceLog( NULL, "OleReleaseObject( %p )\n", pDisp );

     s_nOleError = pDisp->lpVtbl->Release( pDisp );

     hb_retnl( s_nOleError );
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleSetProperty( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     pDispParams->rgdispidNamedArgs = &lPropPut;
     pDispParams->cNamedArgs = 1;

     // 1 Based!!!
     if( ( ISBYREF( 1 ) ) || ISARRAY( 1 ) )
     {
        memset( (LPBYTE) &excep, 0, sizeof( excep ) );

#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                             DispID,
                                             (REFIID) IID_NULL,
                                             LOCALE_SYSTEM_DEFAULT,
                                             DISPATCH_PROPERTYPUTREF,
                                             pDispParams,
                                             NULL,    // No return value
                                             &excep,
                                             &uArgErr );
#else
        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                             DispID,
                                             (REFIID) &IID_NULL,
                                             LOCALE_SYSTEM_DEFAULT,
                                             DISPATCH_PROPERTYPUTREF,
                                             pDispParams,
                                             NULL,    // No return value
                                             &excep,
                                             &uArgErr );
#endif

        if( SUCCEEDED( s_nOleError ) )
        {
           return s_nOleError;
        }
     }

     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_PROPERTYPUT,
                                          pDispParams,
                                          NULL,    // No return value
                                          &excep,
                                          &uArgErr );
#else
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_PROPERTYPUT,
                                          pDispParams,
                                          NULL,    // No return value
                                          &excep,
                                          &uArgErr );
#endif

     pDispParams->rgdispidNamedArgs = NULL;
     pDispParams->cNamedArgs = 0;

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleInvoke( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID)  IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_METHOD,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );
#else
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_METHOD,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );
#endif

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleGetProperty( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_PROPERTYGET,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );
#else
     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_SYSTEM_DEFAULT,
                                          DISPATCH_PROPERTYGET,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );
#endif
     //TraceLog( NULL, "OleGetValue: %p\n", s_nOleError );

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleGetValue( IDispatch *pDisp )
  {
     VariantClear( &RetVal );

     // Try to apply the requested message to the DEFAULT Property of the object if any.
     if( SUCCEEDED( OleGetProperty( pDisp, DISPID_VALUE, &s_EmptyDispParams ) ) &&  ( RetVal.n1.n2.vt == VT_DISPATCH || RetVal.n1.n2.vt == ( VT_DISPATCH | VT_BYREF ) ) )
     {
        VariantCopy( &OleVal, &RetVal );
        VariantClear( &RetVal );

        return s_nOleError;
     }

     return E_FAIL;
  }

  //---------------------------------------------------------------------------//
  static void OleThrowError( void )
  {
     PHB_ITEM pReturn;
     char *sDescription;

     hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );

     if( s_nOleError == DISP_E_EXCEPTION )
     {
        // Intentional to avoid report of memory leak if fatal error.
        char *sTemp = hb_oleWideToAnsi( excep.bstrDescription );
        sDescription = (char *) malloc( strlen( sTemp ) + 1 );
        strcpy( sDescription, sTemp );
        hb_xfree( sTemp );
     }
     else
     {
        sDescription = Ole2TxtError();
     }

     //TraceLog( NULL, "Desc: '%s'\n", sDescription );

     pReturn = hb_errRT_SubstParams( hb_parcx( -1 ), EG_OLEEXECPTION, (ULONG) s_nOleError, sDescription, hb_stackBaseItem()->item.asSymbol.value->szName );

     if( s_nOleError == DISP_E_EXCEPTION )
     {
        free( (void *) sDescription );
     }

     if( pReturn )
     {
        hb_itemRelease( hb_itemReturn( pReturn ) );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO_OLEVALUE )
  {
     if( hb_pcount() == 0 )
     {
        IDispatch *pDisp;

        hb_vmPushSymbol( s_pSym_hObj->pSymbol );
        hb_vmPush( hb_stackSelfItem() );
        hb_vmSend( 0 );

        pDisp = ( IDispatch * ) hb_parnl( -1 );

        VariantClear( &RetVal );

        OleGetProperty( pDisp, DISPID_VALUE, &s_EmptyDispParams );
        //TraceLog( NULL, "GetDefault: %p\n", s_nOleError );

        if( SUCCEEDED( s_nOleError ) )
        {
           RetValue();
        }
        else
        {
           OleThrowError();
        }
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO__OLEVALUE )
  {
     if( hb_pcount() >= 1 )
     {
        IDispatch *pDisp;
        DISPPARAMS DispParams;
        PHB_ITEM *aPrgParams;

        hb_vmPushSymbol( s_pSym_hObj->pSymbol );
        hb_vmPush( hb_stackSelfItem() );
        hb_vmSend( 0 );

        pDisp = ( IDispatch * ) hb_parnl( -1 );

        VariantClear( &RetVal );

        aPrgParams = GetParams( &DispParams, 0 );

        OleSetProperty( pDisp, DISPID_VALUE, &DispParams );
        //TraceLog( NULL, "SetDefault: %p\n", s_nOleError );

        FreeParams( &DispParams, aPrgParams );

        if( SUCCEEDED( s_nOleError ) )
        {
           hb_itemReturn( hb_stackItemFromBase( 1 ) );
        }
        else
        {
           OleThrowError();
        }
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC_STATIC( TOLEAUTO_OLENEWENUMERATOR ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch *pDisp;

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );

     pDisp = ( IDispatch * ) hb_parnl( -1 );

     VariantClear( &RetVal );

     if( SUCCEEDED( OleGetProperty( pDisp, DISPID_NEWENUM, &s_EmptyDispParams ) ) ||
         SUCCEEDED( OleInvoke( pDisp, DISPID_NEWENUM, &s_EmptyDispParams ) ) )
     {
        LPVOID pEnumVariant = NULL; /* IEnumVARIANT */

        if( RetVal.n1.n2.vt == ( VT_UNKNOWN | VT_BYREF ) )
        {
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = (*RetVal.n1.n2.n3.ppunkVal)->lpVtbl->QueryInterface( *RetVal.n1.n2.n3.ppunkVal, (REFIID)  IID_IEnumVARIANT, &pEnumVariant );
#else
           s_nOleError = (*RetVal.n1.n2.n3.ppunkVal)->lpVtbl->QueryInterface( *RetVal.n1.n2.n3.ppunkVal, (REFIID) &IID_IEnumVARIANT, &pEnumVariant );
#endif
        }
        else if( RetVal.n1.n2.vt == VT_UNKNOWN )
        {
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = RetVal.n1.n2.n3.punkVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.punkVal, (REFIID)  IID_IEnumVARIANT, &pEnumVariant );
#else
           s_nOleError = RetVal.n1.n2.n3.punkVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.punkVal, (REFIID) &IID_IEnumVARIANT, &pEnumVariant );
#endif
        }
        else if( RetVal.n1.n2.vt == ( VT_DISPATCH | VT_BYREF ) )
        {
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = (*RetVal.n1.n2.n3.ppdispVal)->lpVtbl->QueryInterface( *RetVal.n1.n2.n3.ppdispVal, (REFIID)  IID_IEnumVARIANT, &pEnumVariant );
#else
           s_nOleError = (*RetVal.n1.n2.n3.ppdispVal)->lpVtbl->QueryInterface( *RetVal.n1.n2.n3.ppdispVal, (REFIID) &IID_IEnumVARIANT, &pEnumVariant );
#endif
        }
        else if( RetVal.n1.n2.vt == VT_DISPATCH )
        {
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
           s_nOleError = RetVal.n1.n2.n3.pdispVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.pdispVal, (REFIID) IID_IEnumVARIANT, &pEnumVariant );
#else
           s_nOleError = RetVal.n1.n2.n3.pdispVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.pdispVal, (REFIID) &IID_IEnumVARIANT, &pEnumVariant );
#endif
        }
        else
        {
           s_nOleError = E_FAIL;
        }

        VariantClear( &RetVal );

        if( SUCCEEDED( s_nOleError ) )
        {
           hb_retptr( pEnumVariant );
        }
        else
        {
           hb_ret();
        }
     }
     else
     {
        OleThrowError();
     }
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleGetID( IDispatch *pDisp, const char *szName, DISPID *pDispID, BOOL *pbSetFirst )
  {
     BSTR bstrMessage;

     if( pbSetFirst )
     {
        *pbSetFirst = FALSE;
     }

     /*
     if( strcmp( szName, "OLEVALUE" ) == 0 || strcmp( szName, "_OLEVALUE" ) == 0 )
     {
        DispID = DISPID_VALUE;
        s_nOleError = S_OK;
     }
     else*/ if( szName[0] == '_' && szName[1] && hb_pcount() >= 1 )
     {
        bstrMessage = hb_oleAnsiToSysString( (char*) szName + 1 );
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID)  IID_NULL, (wchar_t **) &bstrMessage, 1, LOCALE_SYSTEM_DEFAULT, pDispID );
#else
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, LOCALE_SYSTEM_DEFAULT, pDispID );
#endif
        SysFreeString( bstrMessage );
        //TraceLog( NULL, "1. ID of: '%s' -> %i Result: %p\n", hb_stackBaseItem()->item.asSymbol.value->szName + 1, DispID, s_nOleError );

        if( SUCCEEDED( s_nOleError ) )
        {
           if( pbSetFirst )
           {
              *pbSetFirst = TRUE;
           }
        }
     }
     else
     {
        s_nOleError = E_PENDING;
     }

     if( FAILED( s_nOleError ) )
     {
        // Try again without removing the assign prefix (_).
        bstrMessage = hb_oleAnsiToSysString( szName );
#if defined( __cplusplus ) && ( ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1280 ) ) )
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID)  IID_NULL, (wchar_t **) &bstrMessage, 1, 0, pDispID );
#else
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, 0, pDispID );
#endif
        SysFreeString( bstrMessage );
        //TraceLog( NULL, "2. ID of: '%s' -> %i Result: %p\n", szName, *pDispID, s_nOleError );
     }

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO_INVOKE )
  {
     IDispatch *pDisp;
     const char *szName = hb_parc(1);
     DISPID DispID;
     DISPPARAMS DispParams;

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );

     pDisp = ( IDispatch * ) hb_parnl( -1 );

     if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
     {
        PHB_ITEM *aPrgParams = GetParams( &DispParams, 1 );

        if( SUCCEEDED( OleInvoke( pDisp, DispID, &DispParams ) ) )
        {
           RetValue();
        }

        FreeParams( &DispParams, aPrgParams );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO_SET )
  {
     IDispatch *pDisp;
     const char *szName = hb_parc(1);
     DISPID DispID;
     DISPPARAMS DispParams;

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );
     pDisp = ( IDispatch * ) hb_parnl( -1 );

     if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
     {
        PHB_ITEM *aPrgParams = GetParams( &DispParams, 1 );

        if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
        {
           RetValue();
        }

        FreeParams( &DispParams, aPrgParams );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO_GET )
  {
     IDispatch *pDisp;
     const char *szName = hb_parc(1);
     DISPID DispID;
     DISPPARAMS DispParams;

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );
     pDisp = ( IDispatch * ) hb_parnl( -1 );

     if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
     {
        PHB_ITEM *aPrgParams = GetParams( &DispParams, 1 );

        if( SUCCEEDED( OleGetProperty( pDisp, DispID, &DispParams ) ) )
        {
           RetValue();
        }

        FreeParams( &DispParams, aPrgParams );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( TOLEAUTO_ONERROR )
  {
     IDispatch *pDisp;
     DISPID DispID;
     DISPPARAMS DispParams;
     BOOL bSetFirst = FALSE, bTryDefault = TRUE;
     PHB_ITEM *aPrgParams = GetParams( &DispParams, 0 );

     //TraceLog( NULL, "Class: '%s' Message: '%s', Params: %i Arg1: %i\n", hb_objGetClsName( hb_stackSelfItem() ), hb_stackBaseItem()->item.asSymbol.value->szName, hb_pcount(), hb_parinfo(1) );

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );
     pDisp = ( IDispatch * ) hb_parnl( -1 );

    OleGetID :

     if( SUCCEEDED( OleGetID( pDisp, hb_stackBaseItem()->item.asSymbol.value->szName, &DispID, &bSetFirst ) ) )
     {
        VariantClear( &RetVal );

        if( bSetFirst )
        {
           if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
           {
              hb_itemReturn( hb_stackItemFromBase( 1 ) );
           }

           //TraceLog( NULL, "FIRST OleSetProperty %i\n", s_nOleError );
        }
        else
        {
           s_nOleError = E_PENDING;
        }

        if( FAILED( s_nOleError ) )
        {
           if( SUCCEEDED( OleInvoke( pDisp, DispID, &DispParams ) ) )
           {
              RetValue();
           }

           //TraceLog( NULL, "OleInvoke %i\n", s_nOleError );
        }

        if( FAILED( s_nOleError ) )
        {
           if( SUCCEEDED( OleGetProperty( pDisp, DispID, &DispParams ) ) )
           {
              RetValue();
           }

           //TraceLog( NULL, "OleGetProperty(%i) %i\n", DispParams.cArgs, s_nOleError );
        }

        if( FAILED( s_nOleError ) && bSetFirst == FALSE && hb_pcount() >= 1 )
        {
           if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
           {
              hb_itemReturn( hb_stackItemFromBase( 1 ) );
           }

           //TraceLog( NULL, "OleSetProperty %i\n", s_nOleError );
        }
     }

     if( SUCCEEDED( s_nOleError ) )
     {
        //TraceLog( NULL, "Invoke Succeeded!\n" );
        if( HB_IS_OBJECT( hb_stackReturnItem() ) && hb_clsIsParent( hb_stackReturnItem()->item.asArray.value->uiClass , "TOLEAUTO" ) )
        {
           PHB_ITEM pReturn = hb_itemNew( NULL );
           PHB_ITEM pOleClassName = hb_itemNew( NULL );
           char *sOleClassName;
           int iClassNameLen, iMsgNameLen;

           hb_itemForwardValue( pReturn, hb_stackReturnItem() );

           hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
           hb_vmPush( hb_stackSelfItem() );
           hb_vmSend( 0 );

           iClassNameLen = hb_parclen( -1 );
           iMsgNameLen = strlen( hb_stackBaseItem()->item.asSymbol.value->szName );

           sOleClassName = (char *) hb_xgrab( iClassNameLen + 1 + iMsgNameLen + 1 );

           strncpy( sOleClassName, hb_parc( - 1 ), iClassNameLen );
           sOleClassName[ iClassNameLen ] = ':';
           strcpy( sOleClassName + iClassNameLen + 1, hb_stackBaseItem()->item.asSymbol.value->szName );

           //TraceLog( NULL, "Class: '%s'\n", sOleClassName );

           hb_itemPutCPtr( pOleClassName, sOleClassName, iClassNameLen + 1 + iMsgNameLen );

           hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
           hb_vmPush( pReturn );
           hb_itemPushForward( pOleClassName );
           hb_vmSend( 1 );

           hb_itemReturnForward( pReturn );

           hb_itemRelease( pReturn );
           hb_itemRelease( pOleClassName );
        }
     }
     else
     {
        // Try to apply the requested message to the DEFAULT Method of the object if any.
        if( bTryDefault )
        {
           bTryDefault = FALSE;

           if( SUCCEEDED( ( /* s_nOleError = */ OleGetValue( pDisp ) ) ) )
           {
              //TraceLog( NULL, "Try using DISPID_VALUE\n" );
              pDisp = OleVal.n1.n2.n3.pdispVal;
              OleVal.n1.n2.vt = VT_EMPTY;
              goto OleGetID;
           }
           else
           {
              pDisp = NULL;
           }
        }

        //TraceLog( NULL, "Invoke Failed!\n" );
        OleThrowError();
     }

     FreeParams( &DispParams, aPrgParams );

     // We are responsible to release the Default Interface which we retrieved
     if( ( bTryDefault == FALSE ) && pDisp )
     {
        pDisp->lpVtbl->Release( pDisp );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( PTR2INT )  // Ptr2Int( <pPointer> ) -> <nInteger>
  {
     hb_retni( (int) hb_parptr( 1 ) );
  }

#pragma ENDDUMP

#endif
