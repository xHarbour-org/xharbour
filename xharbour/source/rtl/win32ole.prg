/*
 * $Id: win32ole.prg,v 1.98 2005/11/12 20:27:12 ronpinkas Exp $
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
  Function CreateObject()
  Return NIL

  FUNCTION GetActiveObject( cString )
  Return NIL
#else

#include "hbclass.ch"
#include "error.ch"

//----------------------------------------------------------------------------//

FUNCTION CreateObject( cString )

RETURN TOleAuto():New( cString )

//----------------------------------------------------------------------------//
FUNCTION GetActiveObject( cString )

RETURN TOleAuto():GetActiveObject( cString )

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

   static PHB_DYNS s_pSym_TOleAuto;
   static PHB_DYNS s_pSym_hObj;
   static PHB_DYNS s_pSym_New;
   static PHB_DYNS s_pSym_cClassName;

   static BOOL *s_OleRefFlags = NULL;

   static DISPPARAMS s_EmptyDispParams;

   static VARIANTARG RetVal, OleVal;

#pragma ENDDUMP

//----------------------------------------------------------------------------//
INIT PROC HB_OLEINIT

   HB_INLINE()
   {
      s_nOleError = OleInitialize( NULL );

      s_pSym_TOleAuto     = hb_dynsymFindName( "TOLEAUTO" );
      s_pSym_New         = hb_dynsymFindName( "NEW" );
      s_pSym_hObj        = hb_dynsymFindName( "HOBJ" );
      s_pSym_cClassName  = hb_dynsymFindName( "CCLASSNAME" );

      s_EmptyDispParams.rgvarg            = NULL;
      s_EmptyDispParams.cArgs             = 0;
      s_EmptyDispParams.rgdispidNamedArgs = 0;
      s_EmptyDispParams.cNamedArgs        = 0;

      VariantInit( &RetVal );
      VariantInit( &OleVal );
   }
return

EXIT PROC HB_OLEEXIT
   HB_INLINE()
   {
      OleUninitialize();
   }
return

//----------------------------------------------------------------------------//
CLASS TOleAuto

   DATA hObj
   DATA cClassName
   DATA pOleEnumerator

   METHOD New( uObj, cClass ) CONSTRUCTOR
   METHOD GetActiveObject( cClass ) CONSTRUCTOR

   METHOD Invoke()
   MESSAGE Set METHOD Invoke()
   MESSAGE Get METHOD Invoke()

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
METHOD New( uObj, cClass ) CLASS TOleAuto

   LOCAL oErr

   // Hack incase OLE Server already created and New() is attempted as an OLE Method.
   IF ::hObj != NIL
      RETURN HB_ExecFromArray( Self, "_New", HB_aParams() )
   ENDIF

   IF ValType( uObj ) = 'C'
      ::hObj := CreateOleObject( uObj )

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
      ::hObj := uObj

      IF ValType( cClass ) == 'C'
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
   ELSE
      MessageBox( 0, "Invalid parameter type to constructor TOleAuto():New()!", "OLE Interface", 0 )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------
// Destructor!
PROCEDURE Release() CLASS TOleAuto

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
METHOD Invoke( ... ) CLASS TOleAuto

   LOCAL cMethod := HB_aParams()[1]

RETURN HB_ExecFromArray( Self, cMethod, aDel( HB_aParams(), 1, .T. ) )

//--------------------------------------------------------------------
METHOD OleCollection( xIndex, xValue ) CLASS TOleAuto

   LOCAL xRet

   //TraceLog( PCount(), xIndex, xValue )

   IF PCount() == 1
      RETURN ::Item( xIndex )
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
      oErr:Operation     := '+'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1082
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

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
      oErr:Operation     := '%'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

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
      oErr:Operation     := '%'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

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
      oErr:Operation     := '%'
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

METHOD OleEnumerate( nEnumOp, nIndex ) CLASS TOleAuto

   LOCAL xRet

   SWITCH nEnumOp
      CASE FOREACH_BEGIN
         ::pOleEnumerator := ::OleNewEnumerator()
         EXIT

      CASE FOREACH_ENUMERATE
         //xRet := ::Item( nIndex )
         //xRet := ::pOleEnumerator:Next()

         xRet := HB_Inline( ::pOleEnumerator )
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
         HB_Inline( ::pOleEnumerator )
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

  static PHB_ITEM *aPrgParams = NULL;

  static BSTR bstrMessage;
  static DISPID lPropPut = DISPID_PROPERTYPUT;
  static UINT uArgErr;
  static DISPID ValueID = DISPID_VALUE;
  static DISPID NewEnumID = DISPID_NEWENUM;

  HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

  //---------------------------------------------------------------------------//
  static double DateToDbl( LPSTR cDate )
  {
     double nDate;

     nDate = hb_dateEncStr( cDate ) - 0x0024d9abL;

     return ( nDate );
  }

  //---------------------------------------------------------------------------//
  static LPSTR DblToDate( double nDate )
  {
     static char cDate[9] = "00000000";

     hb_dateDecStr( cDate, (LONG) nDate + 0x0024d9abL );

     return ( cDate );
  }

  //---------------------------------------------------------------------------//
  HB_EXPORT BSTR AnsiToSysString( LPSTR cString )
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
  HB_EXPORT LPWSTR AnsiToWide( LPSTR cString )
  {
     int nConvertedLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, NULL, 0 );

     if( nConvertedLen )
     {
        LPWSTR wString = ( BSTR ) hb_xgrab( nConvertedLen * 2 );

        if( MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, wString, nConvertedLen - 1 ) )
        {
           return wString;
        }
        else
        {
           hb_xfree( wString );
        }
     }

     //printf( "\nAnsi: '%s'\n", cString );
     //wprintf( L"\nWide: '%s'\n", wString );
     return NULL;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( ANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
  {
     char *cString = hb_parc( 1 );

     if( cString )
     {
        BSTR wString = AnsiToWide( cString );

        if( wString )
        {
           hb_retclenAdoptRaw( (char *) wString, SysStringLen( wString ) );
           return;
        }
     }

     hb_ret();
     return;
  }

  //---------------------------------------------------------------------------//
  HB_EXPORT LPSTR WideToAnsi( BSTR wString )
  {
     int nConvertedLen = WideCharToMultiByte( CP_ACP, 0, wString, -1, NULL, 0, NULL, NULL );

     if( nConvertedLen )
     {
        char *cString = (char *) hb_xgrab( nConvertedLen );

        if( WideCharToMultiByte( CP_ACP, 0, wString, -1, cString, nConvertedLen, NULL, NULL ) )
        {
           return cString;
        }
        else
        {
           hb_xfree( cString );
        }
     }

     //wprintf( L"\nWide: '%s'\n", wString );
     //printf( "\nAnsi: '%s'\n", cString );

     return NULL;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( WIDETOANSI )  // ( cWideStr, nLen ) -> cAnsiStr
  {
     BSTR wString = ( BSTR ) hb_parc( 1 );

     if( wString )
     {
        char *cString = WideToAnsi( wString );

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
  static void GetParams( DISPPARAMS *pDispParams )
  {
     VARIANTARG * pArgs = NULL;
     PHB_ITEM uParam;
     int n, nArgs, nArg;
     BOOL bByRef;

     nArgs = hb_pcount();

     if( nArgs > 0 )
     {
        pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * nArgs );
        aPrgParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * nArgs );

        // 1 Based!!!
        s_OleRefFlags = (BOOL *) hb_xgrab( ( nArgs + 1 ) * sizeof( BOOL ) );

        //printf( "Args: %i\n", nArgs );

        for( n = 0; n < nArgs; n++ )
        {
           // Parameters are processed in reversed order.
           nArg = nArgs - n;

           VariantInit( &( pArgs[ n ] ) );

           uParam = hb_param( nArg, HB_IT_ANY );

           bByRef = HB_IS_BYREF( hb_stackItemFromBase( nArg ) );

           // 1 Based!!!
           s_OleRefFlags[ nArg ] = bByRef;

           //TraceLog( NULL, "N: %i Arg: %i Type: %i %i ByRef: %i\n", n, nArg, hb_stackItemFromBase( nArg  )->type, uParam->type, bByRef );

           aPrgParams[ n ] = uParam;

           switch( uParam->type )
           {
              case HB_IT_NIL:
                //pArgs[ n ].n1.n2.vt   = VT_EMPTY;
                break;

              case HB_IT_STRING:
              case HB_IT_MEMO:
                if( bByRef )
                {
                   hb_itemPutCRawStatic( uParam, (char *) AnsiToSysString( hb_parc( nArg ) ), uParam->item.asString.length * 2 + 1 );

                   pArgs[ n ].n1.n2.vt   = VT_BYREF | VT_BSTR;
                   pArgs[ n ].n1.n2.n3.pbstrVal = (BSTR *) &( uParam->item.asString.value );
                   //wprintf( L"*** BYREF >%s<\n", *pArgs[ n ].n1.n2.n3.bstrVal );
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_BSTR;
                   pArgs[ n ].n1.n2.n3.bstrVal = AnsiToSysString( hb_parc( nArg ) );
                   //wprintf( L"*** >%s<\n", pArgs[ n ].n1.n2.n3.bstrVal );
                }
                break;

              case HB_IT_LOGICAL:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_BOOL;
                   pArgs[ n ].n1.n2.n3.pboolVal = (short *) &( uParam->item.asLogical.value ) ;
                   uParam->type = HB_IT_LONG;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_BOOL;
                   pArgs[ n ].n1.n2.n3.boolVal = hb_parl( nArg ) ? VARIANT_TRUE : VARIANT_FALSE;
                }
                break;

              case HB_IT_INTEGER:
#if HB_INT_MAX == INT16_MAX
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I2;
                   pArgs[ n ].n1.n2.n3.piVal = &( uParam->item.asInteger.value ) ;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I2;
                   pArgs[ n ].n1.n2.n3.iVal = hb_parni( nArg );
                }
                break;
#else
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I4;
                   pArgs[ n ].n1.n2.n3.plVal = (long *) &( uParam->item.asInteger.value ) ;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I4;
                   pArgs[ n ].n1.n2.n3.lVal = hb_parnl( nArg );
                }
                break;
#endif
              case HB_IT_LONG:
#if HB_LONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I4;
                   pArgs[ n ].n1.n2.n3.plVal = (long *) &( uParam->item.asLong.value ) ;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I4;
                   pArgs[ n ].n1.n2.n3.lVal = hb_parnl( nArg );
                }
#else
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I8;
                   pArgs[ n ].n1.n2.n3.pllVal = &( uParam->item.asLong.value ) ;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I8;
                   pArgs[ n ].n1.n2.n3.llVal = hb_parnll( nArg );
                }
#endif
                break;

              case HB_IT_DOUBLE:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_R8;
                   pArgs[ n ].n1.n2.n3.pdblVal = &( uParam->item.asDouble.value ) ;
                   uParam->type = HB_IT_DOUBLE;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_R8;
                   pArgs[ n ].n1.n2.n3.dblVal = hb_parnd( nArg );
                }
                break;

              case HB_IT_DATE:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_DATE;
                   uParam->item.asDouble.value = DateToDbl( hb_pards( nArg ) );
                   pArgs[ n ].n1.n2.n3.pdblVal = &( uParam->item.asDouble.value ) ;
                   uParam->type = HB_IT_DOUBLE;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_DATE;
                   pArgs[ n ].n1.n2.n3.dblVal = DateToDbl( hb_pards( nArg ) );
                }
                break;

              case HB_IT_ARRAY:
              {
                 //pArgs[ n ].n1.n2.vt = VT_EMPTY;

                 if( ! HB_IS_OBJECT( uParam ) )
                 {
                    SAFEARRAYBOUND rgsabound;
                    PHB_ITEM       elem;
                    long           count;
                    long           i;

                    count = hb_arrayLen( uParam );

                    rgsabound.cElements = count;
                    rgsabound.lLbound = 0;
                    pArgs[ n ].n1.n2.vt        = VT_ARRAY | VT_VARIANT;
                    pArgs[ n ].n1.n2.n3.parray = SafeArrayCreate( VT_VARIANT, 1, &rgsabound );

                    for( i = 0; i < count; i++ )
                    {
                       elem = hb_arrayGetItemPtr( uParam, i+1 );

                       if( strcmp( hb_objGetClsName( elem ), "TOLEAUTO" ) == 0 )
                       {
                          VARIANT mVariant;

                          VariantInit( &mVariant );

                          hb_vmPushSymbol( s_pSym_hObj->pSymbol );
                          hb_vmPush( elem );
                          hb_vmSend( 0 );

                          mVariant.n1.n2.vt = VT_DISPATCH;
                          mVariant.n1.n2.n3.pdispVal = ( IDispatch * ) hb_parnl( -1 );
                          SafeArrayPutElement( pArgs[ n ].n1.n2.n3.parray, &i, &mVariant );
                       }
                    }
                 }
                 else
                 {
                    if( hb_clsIsParent( uParam->item.asArray.value->uiClass , "TOLEAUTO" ) )
                    {
                       hb_vmPushSymbol( s_pSym_hObj->pSymbol );
                       hb_vmPush( uParam );
                       hb_vmSend( 0 );
                       //TraceLog( NULL, "\n#%i Dispatch: %ld\n", n, hb_parnl( -1 ) );
                       pArgs[ n ].n1.n2.vt = VT_DISPATCH;
                       pArgs[ n ].n1.n2.n3.pdispVal = ( IDispatch * ) hb_parnl( -1 );
                       //printf( "\nDispatch: %p\n", pArgs[ n ].n1.n2.n3.pdispVal );

                    }
                    else
                    {
                       TraceLog( NULL, "Class: '%s' not suported!\n", hb_objGetClsName( uParam ) );
                    }
                 }
              }
              break;
           }
        }
     }

     pDispParams->rgvarg            = pArgs;
     pDispParams->cArgs             = nArgs;
     pDispParams->rgdispidNamedArgs = 0;
     pDispParams->cNamedArgs        = 0;
  }

  //---------------------------------------------------------------------------//
  static void FreeParams( DISPPARAMS *pDispParams )
  {
     int n, nParam;
     char *sString;

     if( pDispParams->cArgs > 0 )
     {
        for( n = 0; n < ( int ) pDispParams->cArgs; n++ )
        {
           nParam = pDispParams->cArgs - n;

           //TraceLog( NULL, "*** N: %i, Param: %i Type: %i\n", n, nParam, pDispParams->rgvarg[ n ].n1.n2.vt );

           // 1 Based!!!
           if( s_OleRefFlags[ nParam ]  )
           {
              switch( pDispParams->rgvarg[ n ].n1.n2.vt )
              {
                 case VT_BYREF | VT_BSTR:
                   //printf( "String\n" );
                   sString = WideToAnsi( *( pDispParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );

                   SysFreeString( *( pDispParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );

                   hb_itemPutCPtr( aPrgParams[ n ], sString, strlen( sString ) );
                   break;

                 // Already using the PHB_ITEM allocated value
                 /*
                 case VT_BYREF | VT_BOOL:
                   //printf( "Logical\n" );
                   ( aPrgParams[ n ] )->type = HB_IT_LOGICAL;
                   ( aPrgParams[ n ] )->item.asLogical.value = pDispParams->rgvarg[ n ].n1.n2.n3.boolVal ;
                   break;
                 */

                 case VT_DISPATCH:
                 case VT_BYREF | VT_DISPATCH:
                   //TraceLog( NULL, "Dispatch %p\n", pDispParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                   if( pDispParams->rgvarg[ n ].n1.n2.n3.pdispVal == NULL )
                   {
                      hb_itemClear( aPrgParams[ n ] );
                      break;
                   }

                   OleAuto.type = HB_IT_NIL;

                   if( s_pSym_TOleAuto )
                   {
                      hb_vmPushSymbol( s_pSym_TOleAuto->pSymbol );
                      hb_vmPushNil();
                      hb_vmDo( 0 );

                      hb_itemForwardValue( &OleAuto, &(HB_VM_STACK.Return) );
                   }

                   if( s_pSym_New && OleAuto.type )
                   {
                      //TOleAuto():New( nDispatch )
                      hb_vmPushSymbol( s_pSym_New->pSymbol );
                      hb_itemPushForward( &OleAuto );
                      hb_vmPushLong( ( LONG ) pDispParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                      hb_vmSend( 1 );

                      hb_itemForwardValue( aPrgParams[ n ], &(HB_VM_STACK.Return) );
                   }
                   // Can't CLEAR this Variant
                   continue;

                 /*
                 case VT_BYREF | VT_I2:
                   //printf( "Int %i\n", pDispParams->rgvarg[ n ].n1.n2.n3.iVal );
                   hb_itemPutNI( aPrgParams[ n ], ( int ) pDispParams->rgvarg[ n ].n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_I4:
                   //printf( "Long %ld\n", pDispParams->rgvarg[ n ].n1.n2.n3.lVal );
                   hb_itemPutNL( aPrgParams[ n ], ( LONG ) pDispParams->rgvarg[ n ].n1.n2.n3.lVal );
                   break;

#ifndef HB_LONG_LONG_OFF
                 case VT_BYREF | VT_I8:
                   //printf( "Long %Ld\n", pDispParams->rgvarg[ n ].n1.n2.n3.llVal );
                   hb_itemPutNLL( aPrgParams[ n ], ( LONGLONG ) pDispParams->rgvarg[ n ].n1.n2.n3.llVal );
                   break;
#endif

                 case VT_BYREF | VT_R8:
                   //printf( "Double\n" );
                   hb_itemPutND( aPrgParams[ n ],  pDispParams->rgvarg[ n ].n1.n2.n3.dblVal );
                   break;
                 */

                 case VT_BYREF | VT_DATE:
                   //printf( "Date\n" );
                   hb_itemPutDS( aPrgParams[ n ], DblToDate( *( pDispParams->rgvarg[ n ].n1.n2.n3.pdblVal ) ) );
                   break;

                 /*
                 case VT_BYREF | VT_EMPTY:
                   //printf( "Nil\n" );
                   hb_itemClear( aPrgParams[ n ] );
                   break;
                 */

                 default:
                   TraceLog( NULL, "*** Unexpected Type: %i***\n", pDispParams->rgvarg[ n ].n1.n2.vt );
              }
           }
           else
           {
              switch( pDispParams->rgvarg[ n ].n1.n2.vt )
              {
                 case VT_BSTR:
                   break;

                 case VT_DISPATCH:
                   //TraceLog( NULL, "***NOT REF*** Dispatch %p\n", pDispParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                   // Can'r CLEAR this Variant.
                   continue;

                 //case VT_ARRAY | VT_VARIANT:
                 //  SafeArrayDestroy( pDispParams->rgvarg[ n ].n1.n2.n3.parray );
              }
           }

           VariantClear( &(pDispParams->rgvarg[ n ] ) );
        }

        hb_xfree( ( LPVOID ) pDispParams->rgvarg );

        hb_xfree( (void *) s_OleRefFlags );
        s_OleRefFlags = NULL;

        hb_xfree( ( LPVOID ) aPrgParams );
        aPrgParams = NULL;
     }
  }

  PHB_ITEM SafeArrayToArray( VARIANT *psa, UINT iDim, long* rgIndices )
  {
     long iFrom, iTo, i;
     PHB_ITEM pArray;

     SafeArrayGetLBound( psa->n1.n2.n3.parray, iDim, &iFrom );
     SafeArrayGetUBound( psa->n1.n2.n3.parray, iDim, &iTo );

     pArray = hb_itemNew( NULL );
     hb_arrayNew( pArray, iTo - iFrom + 1 );

     //printf( "Dim: %i\n", iDim );

     for( i = iFrom; i <= iTo; i++ )
     {
        rgIndices[ iDim - 1 ] = i;

        if( iDim > 1 )
        {
           PHB_ITEM pSubArray;

           //printf( "   Sub: %i\n", i );

           pSubArray = SafeArrayToArray( psa, iDim - 1, rgIndices );

           hb_arraySetForward( pArray, i - iFrom + 1, pSubArray );

           hb_itemRelease( pSubArray );
        }
        else
        {
           VARIANT  mElem;

           VariantInit( &mElem );

           //printf( "   Get: %i\n", i );

           if( SafeArrayGetElement( psa->n1.n2.n3.parray, rgIndices, &mElem ) == S_OK )
           {
              VariantToItem( pArray->item.asArray.value->pItems + ( i - iFrom ), &mElem );
              //printf( "   Type: %i\n", ( pArray->item.asArray.value->pItems + ( i - iFrom ) )->type );
           }
        }
     }

     //printf( "Return len: %i\n", pArray->item.asArray.value->ulLen );

     return pArray;
  }

  HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant )
  {
     PHB_ITEM pOleAuto;
     IDispatch *pDispatch = NULL;

     hb_itemClear( pItem );

     // Don't "optimize" (VT_ARRAY | VT_VARIANT) must not match!
     while( pVariant->n1.n2.vt == VT_BYREF || pVariant->n1.n2.vt == VT_VARIANT )
     {
        pVariant = pVariant->n1.n2.n3.pvarVal;
     }

     switch( pVariant->n1.n2.vt )
     {
        case VT_BSTR:
        {
           char *sString = WideToAnsi( pVariant->n1.n2.n3.bstrVal );

           hb_itemPutCPtr( pItem, sString, strlen( sString ) );

           break;
        }

        case VT_BOOL:
           hb_itemPutL( pItem, pVariant->n1.n2.n3.boolVal == VARIANT_TRUE ? TRUE : FALSE );
           break;

        case VT_UNKNOWN:
           pVariant->n1.n2.n3.punkVal->lpVtbl->QueryInterface( pVariant->n1.n2.n3.punkVal, &IID_IDispatch, &pDispatch );
           // Intentionally fall through

        case VT_DISPATCH:

           if( pVariant->n1.n2.vt == VT_DISPATCH )
           {
              pDispatch = pVariant->n1.n2.n3.pdispVal;
           }

           if( pDispatch == NULL )
           {
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
              hb_vmPushLong( ( LONG ) pDispatch );
              hb_vmSend( 1 );

              hb_itemRelease( pOleAuto );

              // Safety!
              hb_vmRequestReset();

              if( pItem != hb_stackReturnItem() )
              {
                 hb_itemForwardValue( pItem, hb_stackReturnItem() );
              }

              //printf( "Dispatch: %ld %ld\n", ( LONG ) pDispatch, (LONG) hb_stackReturnItem()->item.asArray.value );

              // If retrieved from IUnknown than allready added!
              if( pVariant->n1.n2.vt == VT_DISPATCH )
              {
                 pDispatch->lpVtbl->AddRef( pDispatch );
              }
           }
           break;

        case VT_I1:     // Byte
        case VT_UI1:
           hb_itemPutNI( pItem, ( short ) pVariant->n1.n2.n3.bVal );
           break;

        case VT_I2:     // Short (2 bytes)
        case VT_UI2:
           hb_itemPutNI( pItem, ( short ) pVariant->n1.n2.n3.iVal );
           break;

        case VT_I4:     // Long (4 bytes)
        case VT_UI4:
        case VT_INT:
        case VT_UINT:
           hb_itemPutNL( pItem, ( LONG ) pVariant->n1.n2.n3.lVal );
           break;

        case VT_R4:     // Single
           hb_itemPutND( pItem, pVariant->n1.n2.n3.fltVal );
           break;

        case VT_R8:     // Double
           hb_itemPutND( pItem, pVariant->n1.n2.n3.dblVal );
           break;

        case VT_CY:     // Currency
        {
           double tmp = 0;

           VarR8FromCy( pVariant->n1.n2.n3.cyVal, &tmp );
           hb_itemPutND( pItem, tmp );
           break;
        }

        case VT_DECIMAL: // Decimal
        {
           double tmp = 0;

           VarR8FromDec( &pVariant->n1.decVal, &tmp );
           hb_itemPutND( pItem, tmp );
           break;
        }

        case VT_DATE:
           hb_itemPutDS( pItem, DblToDate( pVariant->n1.n2.n3.dblVal ) );
           break;

        case VT_EMPTY:
        case VT_NULL:
           break;

        /*
        case VT_VARIANT:
           VariantToItem( pItem, pVariant->n1.n2.n3.pvarVal );
           break;
        */

        case VT_ARRAY | VT_VARIANT:
        {
           UINT iDims = SafeArrayGetDim( pVariant->n1.n2.n3.parray );
           long * rgIndices = (long *) hb_xgrab( sizeof(long) * iDims );
           PHB_ITEM pArray = SafeArrayToArray( pVariant, iDims, rgIndices );

           hb_xfree( (void *) rgIndices );

           hb_itemForwardValue( pItem, pArray );
           hb_itemRelease( pArray );

           break;
        }

        default:
          TraceLog( NULL, "Unexpected type %i!\n", pVariant->n1.n2.vt );
          return E_FAIL;
     }

     VariantClear( pVariant );

     return S_OK;
  }

  //---------------------------------------------------------------------------//
  static void RetValue( void )
  {
     VariantToItem( hb_stackReturnItem(), &RetVal );

     return;
  }

  //---------------------------------------------------------------------------//
  HB_FUNC( OLESHOWEXCEPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source, description;

        source = WideToAnsi( excep.bstrSource );
        description = WideToAnsi( excep.bstrDescription );

        MessageBox( NULL, description, source, MB_ICONHAND );

        hb_xfree( source );
        hb_xfree( description );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( OLEEXCEPTIONSOURCE )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR source;

        source = WideToAnsi( excep.bstrSource );
        hb_retcAdopt( source );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( OLEEXCEPTIONDESCRIPTION )
  {
     if( (LONG) s_nOleError == DISP_E_EXCEPTION )
     {
        LPSTR description;

        description = WideToAnsi( excep.bstrDescription );
        hb_retcAdopt( description );
     }
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( OLEERROR )
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
  HB_FUNC_STATIC( CREATEOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
  {
     BSTR bstrClassID;
     IID ClassID, iid;
     LPIID riid = (LPIID) &IID_IDispatch;
     void *pDisp = NULL; // IDispatch
     /* void *
      * used intentionally to inform compiler that there is no
      * strict-aliasing
      */
     bstrClassID = AnsiToSysString( hb_parcx( 1 ) );

     if( hb_parcx( 1 )[ 0 ] == '{' )
     {
        s_nOleError = CLSIDFromString( bstrClassID, (LPCLSID) &ClassID );
     }
     else
     {
        s_nOleError = CLSIDFromProgID( bstrClassID, (LPCLSID) &ClassID );
     }

     SysFreeString( bstrClassID );

     //TraceLog( NULL, "Result: %i\n", s_nOleError );

     if( hb_pcount() == 2 )
     {
        if( hb_parcx( 2 )[ 0 ] == '{' )
        {
           bstrClassID = AnsiToSysString( hb_parcx( 2 ) );
           s_nOleError = CLSIDFromString( bstrClassID, &iid );
           SysFreeString( bstrClassID );
        }
        else
        {
           memcpy( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
        }

        riid = &iid;
     }

     if( s_nOleError == S_OK )
     {
        //TraceLog( NULL, "Class: %i\n", ClassID );
        s_nOleError = CoCreateInstance( (REFCLSID) &ClassID, NULL, CLSCTX_SERVER, (REFIID) riid, &pDisp );
        //TraceLog( NULL, "Result: %i\n", s_nOleError );
     }

     hb_retnl( ( LONG ) pDisp );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( GETOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
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

     bstrClassID = AnsiToSysString( hb_parcx( 1 ) );

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
           bstrClassID = AnsiToSysString( hb_parcx( 2 ) );
           s_nOleError = CLSIDFromString( bstrClassID, &iid );
           SysFreeString( bstrClassID );
        }
        else
        {
           memcpy( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
        }

        riid = &iid;
     }

     if( s_nOleError == S_OK )
     {
        s_nOleError = GetActiveObject( (REFCLSID) &ClassID, NULL, &pUnk );

        if( s_nOleError == S_OK )
        {
           s_nOleError = pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) riid, &pDisp );
        }
     }

     hb_retnl( ( LONG ) pDisp );
  }

  //---------------------------------------------------------------------------//
  HB_FUNC_STATIC( OLERELEASEOBJECT ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch *pDisp = ( IDispatch * ) hb_parnl( 1 );

     s_nOleError = pDisp->lpVtbl->Release( pDisp );
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleSetProperty( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     // 1 Based!!!
     if( ( s_OleRefFlags && s_OleRefFlags[ 1 ] ) || hb_param( 1, HB_IT_ARRAY ) )
     {
        memset( (LPBYTE) &excep, 0, sizeof( excep ) );

        s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                             DispID,
                                             (REFIID) &IID_NULL,
                                             LOCALE_USER_DEFAULT,
                                             DISPATCH_PROPERTYPUTREF,
                                             pDispParams,
                                             NULL,    // No return value
                                             &excep,
                                             &uArgErr );

        if( s_nOleError == S_OK )
        {
           return S_OK;
        }
     }

     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_USER_DEFAULT,
                                          DISPATCH_PROPERTYPUT,
                                          pDispParams,
                                          NULL,    // No return value
                                          &excep,
                                          &uArgErr );

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleInvoke( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_USER_DEFAULT,
                                          DISPATCH_METHOD,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleGetProperty( IDispatch *pDisp, DISPID DispID, DISPPARAMS *pDispParams )
  {
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     s_nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                          DispID,
                                          (REFIID) &IID_NULL,
                                          LOCALE_USER_DEFAULT,
                                          DISPATCH_PROPERTYGET,
                                          pDispParams,
                                          &RetVal,
                                          &excep,
                                          &uArgErr );

     return s_nOleError;
  }

  //---------------------------------------------------------------------------//
  static HRESULT OleGetValue( IDispatch *pDisp )
  {
     VariantClear( &RetVal );

     if( OleInvoke( pDisp, ValueID, &s_EmptyDispParams ) == S_OK && RetVal.n1.n2.vt == VT_DISPATCH )
     {
        VariantCopy( &OleVal, &RetVal );
        VariantClear( &RetVal );

        return S_OK;
     }
     else
     {
        // Try to apply the requested message to the DEFAULT Property of the object if any.
        if( OleGetProperty( pDisp, ValueID, &s_EmptyDispParams ) == S_OK && RetVal.n1.n2.vt == VT_DISPATCH )
        {
           VariantCopy( &OleVal, &RetVal );
           VariantClear( &RetVal );

           return S_OK;
        }
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
        char *sTemp = WideToAnsi( excep.bstrDescription );
        sDescription = (char *) malloc( strlen( sTemp ) + 1 );
        strcpy( sDescription, sTemp );
        hb_xfree( sTemp );
     }
     else
     {
        sDescription = Ole2TxtError();
     }

     //TraceLog( NULL, "Desc: '%s'\n", sDescription );

     pReturn = hb_errRT_SubstParams( hb_parcx( -1 ), EG_OLEEXECPTION, (ULONG) s_nOleError, sDescription, ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName );

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

        OleGetProperty( pDisp, ValueID, &s_EmptyDispParams );

        if( s_nOleError == S_OK )
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

        hb_vmPushSymbol( s_pSym_hObj->pSymbol );
        hb_vmPush( hb_stackSelfItem() );
        hb_vmSend( 0 );

        pDisp = ( IDispatch * ) hb_parnl( -1 );

        VariantClear( &RetVal );

        GetParams( &DispParams );

        DispParams.rgdispidNamedArgs = &lPropPut;
        DispParams.cNamedArgs = 1;

        OleSetProperty( pDisp, ValueID, &DispParams );

        FreeParams( &DispParams );

        if( s_nOleError == S_OK )
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

     if( OleGetProperty( pDisp, NewEnumID, &s_EmptyDispParams ) == S_OK || OleInvoke( pDisp, NewEnumID, &s_EmptyDispParams ) == S_OK )
     {
        LPVOID pEnumVariant = NULL; /* IEnumVARIANT */

        if( RetVal.n1.n2.vt == VT_UNKNOWN )
        {
           s_nOleError = RetVal.n1.n2.n3.punkVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.punkVal, &IID_IEnumVARIANT, &pEnumVariant );
        }
        else if( RetVal.n1.n2.vt == VT_DISPATCH )
        {
           s_nOleError = RetVal.n1.n2.n3.pdispVal->lpVtbl->QueryInterface( RetVal.n1.n2.n3.pdispVal, &IID_IEnumVARIANT, &pEnumVariant );
        }
        else
        {
           s_nOleError = E_FAIL;
        }

        if( s_nOleError == S_OK )
        {
           /*
              Intentionally using Init instead of Clear, because we keep refernce to
              this unknown, as per below.
            */
           VariantInit( &RetVal );

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
  HB_FUNC_STATIC( TOLEAUTO_ONERROR )
  {
     IDispatch *pDisp;
     DISPID DispID;
     DISPPARAMS DispParams;
     BOOL bSetFirst = FALSE;

     //TraceLog( NULL, "Class: '%s' Message: '%s', Params: %i Arg1: %i\n", hb_objGetClsName( hb_stackSelfItem() ), ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, hb_pcount(), hb_parinfo(1) );

     hb_vmPushSymbol( s_pSym_hObj->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );

     pDisp = ( IDispatch * ) hb_parnl( -1 );

    OleGetID :

     /*
     if( strcmp( ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, "OLEVALUE" ) == 0 || strcmp( ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, "_OLEVALUE" ) == 0 )
     {
        DispID = ValueID;
        s_nOleError = S_OK;
     }
     else*/ if( ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName[0] == '_' && ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName[1] && hb_pcount() >= 1 )
     {
        bstrMessage = AnsiToSysString( ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName + 1 );
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, LOCALE_USER_DEFAULT, &DispID );
        SysFreeString( bstrMessage );
        //TraceLog( NULL, "1. ID of: '%s' -> %i Result: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName + 1, DispID, s_nOleError );

        if( s_nOleError == S_OK )
        {
           bSetFirst = TRUE;
        }
     }
     else
     {
        s_nOleError = E_PENDING;
     }

     if( s_nOleError != S_OK )
     {
        // Try again without removing the assign prefix (_).
        bstrMessage = AnsiToSysString( ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName );
        s_nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, (REFIID) &IID_NULL, (wchar_t **) &bstrMessage, 1, 0, &DispID );
        SysFreeString( bstrMessage );
        //TraceLog( NULL, "2. ID of: '%s' -> %i Result: %i\n", ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName, DispID, s_nOleError );
     }

     if( s_nOleError == S_OK )
     {
        GetParams( &DispParams );

        VariantClear( &RetVal );

        if( bSetFirst )
        {
           DispParams.rgdispidNamedArgs = &lPropPut;
           DispParams.cNamedArgs = 1;

           OleSetProperty( pDisp, DispID, &DispParams );
           //TraceLog( NULL, "OleSetProperty %i\n", s_nOleError );

           if( s_nOleError == S_OK )
           {
              hb_itemReturn( hb_stackItemFromBase( 1 ) );
           }
           else
           {
              DispParams.rgdispidNamedArgs = NULL;
              DispParams.cNamedArgs = 0;
           }
        }

        if( bSetFirst == FALSE || s_nOleError != S_OK )
        {
           OleInvoke( pDisp, DispID, &DispParams );
           //TraceLog( NULL, "OleInvoke %i\n", s_nOleError );

           if( s_nOleError == S_OK )
           {
              RetValue();
           }
        }

        // Collections are properties that do require arguments!
        if( s_nOleError != S_OK /* && hb_pcount() == 0 */ )
        {
           OleGetProperty( pDisp, DispID, &DispParams );
           //TraceLog( NULL, "OleGetProperty %i\n", s_nOleError );

           if( s_nOleError == S_OK )
           {
              RetValue();
           }
        }

        if( s_nOleError != S_OK && hb_pcount() >= 1 )
        {
           DispParams.rgdispidNamedArgs = &lPropPut;
           DispParams.cNamedArgs = 1;

           OleSetProperty( pDisp, DispID, &DispParams );
           //TraceLog( NULL, "OleSetProperty %i\n", s_nOleError );

           if( s_nOleError == S_OK )
           {
              hb_itemReturn( hb_stackItemFromBase( 1 ) );
           }
        }

        FreeParams( &DispParams );
     }

     if( s_nOleError == S_OK )
     {
        //TraceLog( NULL, "Invoke Succeeded!\n" );

        if( HB_IS_OBJECT( &HB_VM_STACK.Return ) )
        {
           HB_ITEM Return;
           HB_ITEM OleClassName;
           char sOleClassName[ 256 ];

           Return.type = HB_IT_NIL;
           hb_itemForwardValue( &Return, &HB_VM_STACK.Return );

           hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
           hb_vmPush( hb_stackSelfItem() );
           hb_vmSend( 0 );

           strncpy( sOleClassName, hb_parc( - 1 ), hb_parclen( -1 ) );
           sOleClassName[ hb_parclen( -1 ) ] = ':';
           strcpy( sOleClassName + hb_parclen( -1 ) + 1, ( *HB_VM_STACK.pBase )->item.asSymbol.value->szName );

           //TraceLog( NULL, "Class: '%s'\n", sOleClassName );

           OleClassName.type = HB_IT_NIL;
           hb_itemPutC( &OleClassName, sOleClassName );

           hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
           hb_vmPush( &Return );
           hb_itemPushForward( &OleClassName );
           hb_vmSend( 1 );

           hb_itemReturnForward( &Return );
        }
     }
     else
     {
        // Try to apply the requested message to the DEFAULT Method of the object if any.
        if( OleGetValue( pDisp ) == S_OK )
        {
           pDisp = OleVal.n1.n2.n3.pdispVal;
           goto OleGetID;
        }

        //TraceLog( NULL, "Invoke Failed!\n" );
        OleThrowError();
     }
  }
#pragma ENDDUMP

#endif
