/*
 * $Id: win32ole.prg,v 1.1 2002/05/02 22:28:58 ronpinkas Exp $
 */

/*
 * Copyright 2002  José F. Giménez (JFG) - <jfgimenez@wanadoo.es>
 *                                         <tecnico.sireinsa@ctv.es>
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

//----------------------------------------------------------------------------//

FUNCTION CreateObject( cString )

RETURN TOleAuto():New( cString )

//----------------------------------------------------------------------------//

#pragma BEGINDUMP

   #include "hbapi.h"
   #include "hbstack.h"
   #include "hbapierr.h"
   #include "hbapiitm.h"
   #include "hbvm.h"
   #include "hbdate.h"
   #include "hboo.ch"
   #include "hbfast.h"

   static char *s_OleRefFlags = NULL;

   HB_FUNC( SETOLEREFFLAGS )
   {
      if( s_OleRefFlags )
      {
         hb_xfree( s_OleRefFlags );
         s_OleRefFlags = NULL;
      }

      if( hb_pcount() )
      {
         s_OleRefFlags = hb_strdup( hb_stackItemFromBase( 1 )->item.asString.value );
      }
   }

#pragma ENDDUMP

//----------------------------------------------------------------------------//

#include "hbclass.ch"

CLASS TOleAuto

   DATA hObj
   DATA cClassName
   DATA bShowException INIT .T.

   METHOD New( uObj, cClass ) CONSTRUCTOR
   METHOD End()

   METHOD Invoke( cMember, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )

   ERROR HANDLER OnError()

ENDCLASS

//--------------------------------------------------------------------

METHOD New( uObj, cClass ) CLASS TOleAuto

   //TraceLog( uObj, cClass )

   IF ValType( uObj ) = 'C'
      ::hObj := CreateOleObject( uObj )
      ::cClassName := uObj
   ELSEIF ValType( uObj ) = 'N'
      ::hObj := uObj
      IF ValType( cClass ) == 'C'
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
   ELSE
      Alert( "Invalid parameter type to constructor TOleAuto():New()!" )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------

METHOD End() CLASS TOleAuto

   ::hObj := NIL

   OLEUninitialize()

RETURN NIL

//--------------------------------------------------------------------

METHOD Invoke( cMethod, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount(), Counter
   LOCAL OleRefFlags := Space( nParams - 1 )

   //TraceLog( cMethod, nParams )

   IF ProcName( 1 ) != "TOLEAUTO:" + cMethod
      IF nParams >= 7
         IF HB_ISBYREF( @uParam6 )
            OleRefFlags[6] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 6
         IF HB_ISBYREF( @uParam5 )
            OleRefFlags[5] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 5
         IF HB_ISBYREF( @uParam4 )
            OleRefFlags[4] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 4
         IF HB_ISBYREF( @uParam3 )
            OleRefFlags[3] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 3
         IF HB_ISBYREF( @uParam2 )
            OleRefFlags[2] = 'Y'
         ENDIF
      ENDIF

      IF nParams >= 2
         IF HB_ISBYREF( @uParam1 )
            OleRefFlags[6] = 'Y'
         ENDIF

         SetOleRefFlags( OleRefFlags )
      ENDIF
   ENDIF

   IF nParams == 7
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      uObj := OLEInvoke( ::hObj, cMethod, @uParam1 )
   ELSEIF nParams == 1
      uObj := OLEInvoke( ::hObj, cMethod )
   ELSE
      Alert( "TOleAuto:Invoke() - Unsupported number of parameter!" )
      RETURN NIL
   ENDIF

   SetOleRefFlags()

   IF OleIsObject()
      RETURN TOleAuto():New( uObj )
   ELSEIF ::bShowException .AND. Ole2TxtError() == "DISP_E_EXCEPTION"
      OLEShowException()
      RETURN Self
   ELSEIF ::bShowException .AND. OleError() != 0
      Alert( "Error! " + ::cClassName + ":" + cMethod + " " + Ole2TxtError() )
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount()

   //TraceLog( cProperty, nParams )

   IF nParams == 7
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      OLESetProperty( ::hObj, cProperty, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      OLESetProperty( ::hObj, cProperty, @uParam1 )
   ELSE
      Alert( "TOleAuto:Set() - Unsupported number of parameter!" )
      RETURN NIL
   ENDIF

   SetOleRefFlags()

   IF ::bShowException .AND. Ole2TxtError() == "DISP_E_EXCEPTION"
      OLEShowException()
   ELSEIF ::bShowException .AND. OleError() != 0
      Alert( "Error! " + ::cClassName + ":" + cProperty + " " + Ole2TxtError() )
   ENDIF

RETURN nil

//--------------------------------------------------------------------

METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj, nParams := PCount()

   //TraceLog( cProperty, nParams )

   IF nParams == 7
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
   ELSEIF nParams == 6
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
   ELSEIF nParams == 5
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3, @uParam4 )
   ELSEIF nParams == 4
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2, @uParam3 )
   ELSEIF nParams == 3
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1, @uParam2 )
   ELSEIF nParams == 2
      uObj := OLEGetProperty( ::hObj, cProperty, @uParam1 )
   ELSEIF nParams == 1
      uObj := OLEGetProperty( ::hObj, cProperty )
   ELSE
      Alert( "TOleAuto:Get() - Unsupported number of parameter!" )
      RETURN NIL
   ENDIF

   IF OleIsObject()
      RETURN TOleAuto():New( uObj )
   ELSEIF ::bShowException .AND. OleError() != 0
      Alert( "Error! " + ::cClassName + ":" + cProperty + " " + Ole2TxtError() )
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD OnError( uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL cMsg := __GetMessage()
   LOCAL bPresetShowException := ::bShowException
   LOCAL uObj
   LOCAL cError
   LOCAL nParams := PCount(), OleRefFlags := Space( nParams )

   //TraceLog( cMsg, nParams )

   IF nParams >= 6
      IF HB_ISBYREF( @uParam6 )
         OleRefFlags[6] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 5
      IF HB_ISBYREF( @uParam5 )
         OleRefFlags[5] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 4
      IF HB_ISBYREF( @uParam4 )
         OleRefFlags[4] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 3
      IF HB_ISBYREF( @uParam3 )
         OleRefFlags[3] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 2
      IF HB_ISBYREF( @uParam2 )
         OleRefFlags[2] = 'Y'
      ENDIF
   ENDIF

   IF nParams >= 1
      IF HB_ISBYREF( @uParam1 )
         OleRefFlags[1] = 'Y'
      ENDIF

      SetOleRefFlags( OleRefFlags )
   ENDIF

   ::bShowException := .F.

   IF LEFT( cMsg, 1 ) == '_'
      cMsg := SubStr( cMsg, 2 )

      IF nParams == 6
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
      ELSEIF nParams == 5
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
      ELSEIF nParams == 4
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
      ELSEIF nParams == 3
         uObj := ::Set( cMsg, @uParam1, @uParam2, @uParam3 )
      ELSEIF nParams == 2
         uObj := ::Set( cMsg, @uParam1, @uParam2 )
      ELSEIF nParams == 1
         uObj := ::Set( cMsg, @uParam1 )
      ELSE
         uObj := ::Set( cMsg )
      ENDIF

      // Reset in ::Set()
      //SetOleRefFlags()
   ELSE
      IF nParams == 6
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
      ELSEIF nParams == 5
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
      ELSEIF nParams == 4
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
      ELSEIF nParams == 3
         uObj := ::Invoke( cMsg, @uParam1, @uParam2, @uParam3 )
      ELSEIF nParams == 2
         uObj := ::Invoke( cMsg, @uParam1, @uParam2 )
      ELSEIF nParams == 1
         uObj := ::Invoke( cMsg, @uParam1 )
      ELSE
         uObj := ::Invoke( cMsg )
      ENDIF

      // Reset in ::Invoke()
      //SetOleRefFlags()

      IF Ole2TxtError() != "S_OK"
         //TraceLog( cMsg )

         IF nParams == 6
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5, @uParam6 )
         ELSEIF nParams == 5
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4, @uParam5 )
         ELSEIF nParams == 4
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3, @uParam4 )
         ELSEIF nParams == 3
            uObj := ::Get( cMsg, @uParam1, @uParam2, @uParam3 )
         ELSEIF nParams == 2
            uObj := ::Get( cMsg, @uParam1, @uParam2 )
         ELSEIF nParams == 1
            uObj := ::Get( cMsg, @uParam1 )
         ELSE
            uObj := ::Get( cMsg )
         ENDIF

        // Reset in ::Get()
        //SetOleRefFlags()
      ENDIF
   ENDIF

   ::bShowException := bPresetShowException

   IF ::bShowException .AND. ( cError := Ole2TxtError() ) != "S_OK"
      Alert( "Error! " + ::cClassName + ":" + cMsg + " " + cError )
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

#pragma BEGINDUMP

  #define __STDC__ 1
  #define CINTERFACE 1

  #ifndef __FLAT__
    #define __FLAT__ 1
  #endif

  #include <Windows.h>
  #include <Ole2.h>

  #include <OleAuto.h>
  #include <OleDB.h>
  #include <ShlObj.h>

  #include <ctype.h>

  #undef  WORD
  #define WORD  unsigned short

  // -----------------------------------------------------------------------

  static far VARIANTARG RetVal;

  static EXCEPINFO excep;

  static HRESULT nOleError = 0;

  static int nInitialized = 0;

  static PHB_ITEM *aPrgParams = NULL;

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
     static char *cDate = "00000000";

     hb_dateDecStr( cDate, (long) nDate + 0x0024d9abL );

     return ( cDate );
  }

  //---------------------------------------------------------------------------//

  static BSTR AnsiToWide( LPSTR cString )
  {
     UINT uLen;
     BSTR wString;

     uLen  = strlen( cString );

     if( uLen )
     {
        wString = ( BSTR ) hb_xgrab( ( uLen + 1 ) * 2 );
     }
     else
     {
       return NULL;
     }

     MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, uLen + 1, wString, uLen + 1 );

     //printf( "\nAnsi: '%s'\n", cString );
     //wprintf( L"\nWide: '%s'\n", wString );

     return ( wString );
  }

  //---------------------------------------------------------------------------//

  static LPSTR WideToAnsi( BSTR wString )
  {
     UINT uLen;
     LPSTR cString = NULL;

     uLen = SysStringLen( wString );

     if( uLen )
     {
        cString = (char *) hb_xgrab( uLen + 1 );
        WideCharToMultiByte( CP_ACP, 0, wString, uLen + 1, cString, uLen + 1, NULL, NULL );
     }

     //wprintf( L"\nWide: '%s'\n", wString );
     //printf( "\nAnsi: '%s'\n", cString );

     return cString;
  }

  //---------------------------------------------------------------------------//

  static void GetParams(DISPPARAMS * dParams)
  {
     VARIANTARG * pArgs = NULL;
     PHB_ITEM uParam;
     int n, nArgs, nArg, nParam;
     BSTR wString;
     BOOL bByRef;

     nArgs = hb_pcount() - 2;

     if( nArgs > 0 )
     {
        pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * nArgs );
        aPrgParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * nArgs );

        //printf( "Args: %i\n", nArgs );

        for( n = 0; n < nArgs; n++ )
        {
           // Pramateres processed in reveresed order.
           nArg = nArgs + 2 - n;
           nParam = nArgs - n;

           bByRef = s_OleRefFlags && s_OleRefFlags[ nParam - 1 ] == 'Y';

           //printf( "N: %i Arg: %i Type: %i ByRef: %i\n", n, nArg, hb_stackItemFromBase( nArg  )->type, bByRef );

           VariantInit( &( pArgs[ n ] ) );

           uParam = hb_param( nArg, HB_IT_ANY );

           aPrgParams[ n ] = uParam;

           switch( uParam->type )
           {
              case HB_IT_NIL:
                pArgs[ n ].n1.n2.vt   = VT_EMPTY;
                break;

              case HB_IT_STRING:
              case HB_IT_MEMO:
                if( bByRef )
                {
                   wString = AnsiToWide( hb_parc( nArg ) );
                   hb_itemReleaseString( uParam );
                   uParam->item.asString.value = (char *) SysAllocString( wString );
                   hb_xfree( wString );
                   pArgs[ n ].n1.n2.vt   = VT_BYREF | VT_BSTR;
                   pArgs[ n ].n1.n2.n3.pbstrVal = (BSTR *) &( uParam->item.asString.value );
                }
                else
                {
                   pArgs[ n ].n1.n2.vt   = VT_BSTR;
                   wString = AnsiToWide( hb_parc( nArg ) );
                   pArgs[ n ].n1.n2.n3.bstrVal = SysAllocString( wString );
                   hb_xfree( wString );
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
                   pArgs[ n ].n1.n2.vt   = VT_BOOL;
                   pArgs[ n ].n1.n2.n3.boolVal = hb_parl( nArg );
                }
                break;

              case HB_IT_INTEGER:
              case HB_IT_LONG:
              case HB_IT_NUMERIC:
                if( bByRef )
                {
                   pArgs[ n ].n1.n2.vt = VT_BYREF | VT_I4;
                   pArgs[ n ].n1.n2.n3.plVal = &( uParam->item.asLong.value ) ;
                   uParam->type = HB_IT_LONG;
                }
                else
                {
                   pArgs[ n ].n1.n2.vt = VT_I4;
                   pArgs[ n ].n1.n2.n3.lVal = hb_parnl( nArg );
                }
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

              case HB_IT_OBJECT:
              {
                 PHB_DYNS pData;

                 pArgs[ n ].n1.n2.vt = VT_EMPTY;

                 if ( hb_stricmp( hb_objGetClsName( uParam ), "TOleAuto" ) == 0 )
                 {
                    pData = hb_dynsymFindName( "hObj" );

                    if( pData )
                    {
                       hb_vmPush( uParam );
                       hb_vmPushSymbol( pData->pSymbol );
                       hb_vmDo( 0 );
                       pArgs[ n ].n1.n2.vt = VT_DISPATCH;
                       pArgs[ n ].n1.n2.n3.pdispVal = ( IDispatch * ) hb_parnl( -1 );
                    }
                 }
              }
              break;
           }
        }
     }

     dParams->rgvarg            = pArgs;
     dParams->cArgs             = nArgs;
     dParams->rgdispidNamedArgs = 0;
     dParams->cNamedArgs        = 0;
  }

  //---------------------------------------------------------------------------//

  static void FreeParams(DISPPARAMS * dParams)
  {
     int n, nParam;
     char *sString;

     if( dParams->cArgs > 0 )
     {
        for( n = 0; n < ( int ) dParams->cArgs; n++ )
        {
           nParam = dParams->cArgs - n;

           //printf( "*** N: %i, Param: %i\n", n, nParam );

           if( s_OleRefFlags && s_OleRefFlags[ nParam - 1 ] == 'Y' )
           {
              switch( dParams->rgvarg[ n ].n1.n2.vt )
              {
                 case VT_BYREF | VT_BSTR:
                   //printf( "String\n" );
                   sString = WideToAnsi( *( dParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );
                   SysFreeString( *( dParams->rgvarg[ n ].n1.n2.n3.pbstrVal ) );

                   // The item should NOT be cleared because we released the value above.
                   aPrgParams[ n ]->type = HB_IT_NIL;

                   hb_itemPutCPtr( aPrgParams[ n ], sString, strlen( sString ) );
                   break;

                 // Already using the PHB_ITEM allocated value
                 /*
                 case VT_BYREF | VT_BOOL:
                   //printf( "Logical\n" );
                   ( aPrgParams[ n ] )->type = HB_IT_LOGICAL;
                   ( aPrgParams[ n ] )->item.asLogical.value = dParams->rgvarg[ n ].n1.n2.n3.boolVal ;
                   break;

                 case VT_BYREF | VT_DISPATCH:
                   //printf( "Dispatch\n" );
                   hb_itemPutNL( aPrgParams[ n ], ( LONG ) dParams->rgvarg[ n ].n1.n2.n3.pdispVal );
                   break;

                 case VT_BYREF | VT_I2:
                   //printf( "Int %i\n", dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   hb_itemPutNI( aPrgParams[ n ], ( int ) dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_I4:
                   //printf( "Long %ld\n", dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   hb_itemPutNL( aPrgParams[ n ], ( LONG ) dParams->rgvarg[ n ].n1.n2.n3.iVal );
                   break;

                 case VT_BYREF | VT_R8:
                   //printf( "Double\n" );
                   hb_itemPutND( aPrgParams[ n ],  dParams->rgvarg[ n ].n1.n2.n3.dblVal );
                   break;
                 */

                 case VT_BYREF | VT_DATE:
                   //printf( "Date\n" );
                   hb_itemPutDS( aPrgParams[ n ], DblToDate( *( dParams->rgvarg[ n ].n1.n2.n3.pdblVal ) ) );
                   break;

                 /*
                 case VT_BYREF | VT_EMPTY:
                   //printf( "Nil\n" );
                   hb_itemClear( aPrgParams[ n ] );
                   break;
                 */

                 default:
                   //printf( "*** Other %i***\n", dParams->rgvarg[ n ].n1.n2.vt );
                   ;
              }
           }

           VariantClear( &(dParams->rgvarg[ n ] ) );
        }

        hb_xfree( ( LPVOID ) dParams->rgvarg );

        if( aPrgParams )
        {
           hb_xfree( ( LPVOID ) aPrgParams );
           aPrgParams = NULL;
        }
     }
  }

  //---------------------------------------------------------------------------//

  static void RetValue( void )
  {
     LPSTR cString;

     switch( RetVal.n1.n2.vt )
     {
        case VT_BSTR:
          cString = WideToAnsi( RetVal.n1.n2.n3.bstrVal );
          hb_retcAdopt( cString );
          break;

        case VT_BOOL:
          hb_retl( RetVal.n1.n2.n3.boolVal );
          break;

        case VT_DISPATCH:
          hb_retnl( ( LONG ) RetVal.n1.n2.n3.pdispVal );
          break;

        case VT_I4:
          hb_retnl( ( LONG ) RetVal.n1.n2.n3.iVal );
          break;

        case VT_R8:
          hb_retnd( RetVal.n1.n2.n3.dblVal );
          break;

        case VT_DATE:
          hb_retds( DblToDate( RetVal.n1.n2.n3.dblVal ) );
          break;

        case VT_EMPTY:
          hb_ret();
          break;

        default:
          if( nOleError == S_OK )
          {
             (LONG) nOleError = -1;
          }

          hb_ret();
          break;
     }

     if( RetVal.n1.n2.vt != VT_DISPATCH )
     {
        VariantClear( &RetVal );
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( CREATEOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
  {
     BSTR wCLSID;
     GUID ClassID, iid;
     /*REFIID*/ struct _GUID *riid = (struct _GUID *) &IID_IDispatch;
     IDispatch * pDisp = NULL;

     nOleError = S_OK;

     if ( nInitialized == 0 )
     {
        nOleError = OleInitialize( NULL );
     }

     if ( (nOleError == S_OK) || (nOleError == (HRESULT) S_FALSE) )
     {
        nInitialized++;

        wCLSID = AnsiToWide( hb_parc( 1 ) );

        if ( hb_parc( 1 )[ 0 ] == '{' )
        {
           nOleError = CLSIDFromString( wCLSID, &ClassID );
        }
        else
        {
           nOleError = CLSIDFromProgID( wCLSID, &ClassID );
        }

        hb_xfree( wCLSID );

        if ( hb_pcount() == 2 )
        {
           if ( hb_parc( 2 )[ 0 ] == '{' )
           {
              wCLSID = AnsiToWide( hb_parc( 2 ) );
              nOleError = CLSIDFromString( wCLSID, &iid );
              hb_xfree( wCLSID );
           }
           else
           {
              memcpy( ( LPVOID ) &iid, hb_parc( 2 ), sizeof( iid ) );
           }

           riid = &iid;
        }

        if ( nOleError == S_OK )
        {
           nOleError = CoCreateInstance( &ClassID, NULL, CLSCTX_SERVER, riid, (void **) &pDisp );
        }
     }

     hb_retnl( ( LONG ) pDisp );

  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLESHOWEXCEPTION )
  {
     if( (LONG) nOleError == DISP_E_EXCEPTION )
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

  HB_FUNC( OLEINVOKE ) // (hOleObject, szMethodName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID;
     DISPPARAMS dParams;
     UINT uArgErr;

     VariantInit( &RetVal );
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     wMember = AnsiToWide( hb_parc( 2 ) );
     nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     if( nOleError == S_OK )
     {
        GetParams( &dParams );
        nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                           lDispID,
                                           &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           DISPATCH_METHOD,
                                           &dParams,
                                           &RetVal,
                                           &excep,
                                           &uArgErr ) ;
        FreeParams( &dParams );
     }

     RetValue();
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLESETPROPERTY ) // (hOleObject, cPropName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID, lPropPut = DISPID_PROPERTYPUT;
     DISPPARAMS dParams;
     UINT uArgErr;

     VariantInit( &RetVal );
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     wMember = AnsiToWide( hb_parc( 2 ) );
     nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     if( nOleError == S_OK )
     {
        GetParams( &dParams );
        dParams.rgdispidNamedArgs = &lPropPut;
        dParams.cNamedArgs = 1;

        nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                           lDispID,
                                           &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           DISPATCH_PROPERTYPUT,
                                           &dParams,
                                           NULL,    // No return value
                                           &excep,
                                           &uArgErr );

        FreeParams( &dParams );
     }

     hb_ret();
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEGETPROPERTY )  // (hOleObject, cPropName, uParams...)
  {
     IDispatch * pDisp = ( IDispatch * ) hb_parnl( 1 );
     BSTR wMember;
     DISPID lDispID;
     DISPPARAMS dParams;
     UINT uArgErr;

     VariantInit( &RetVal );
     memset( (LPBYTE) &excep, 0, sizeof( excep ) );

     wMember = AnsiToWide( hb_parc( 2 ) );
     nOleError = pDisp->lpVtbl->GetIDsOfNames( pDisp, &IID_NULL, (unsigned short **) &wMember, 1, LOCALE_USER_DEFAULT, &lDispID );
     hb_xfree( wMember );

     if( nOleError == S_OK )
     {
        GetParams( &dParams );

        nOleError = pDisp->lpVtbl->Invoke( pDisp,
                                           lDispID,
                                           &IID_NULL,
                                           LOCALE_USER_DEFAULT,
                                           DISPATCH_PROPERTYGET,
                                           &dParams,
                                           &RetVal,
                                           &excep,
                                           &uArgErr );

        FreeParams( &dParams );
     }

     RetValue();
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEQUERYINTERFACE )  // (hOleObject, cIID ) -> ppvObject
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );
     IUnknown * ppvObject = NULL;
     GUID iid;
     BSTR wiid;

     nOleError = S_OK;

     if( hb_parc( 2 )[ 0 ] == '{' )
     {
        wiid = AnsiToWide( hb_parc( 2 ) );
        nOleError = CLSIDFromString( wiid, &iid );
        hb_xfree( wiid );
     }
     else
     {
        memcpy( ( LPVOID ) &iid, hb_parc( 2 ), sizeof( iid ) );
     }

     if( nOleError == S_OK )
     {
        nOleError = pUnk -> lpVtbl -> QueryInterface( pUnk, (const struct _GUID *const) &iid, (void **) &ppvObject );
     }

     hb_retnl( ( LONG ) ppvObject );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEADDREF )  // ( hOleObject )
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );

     hb_retnl( pUnk -> lpVtbl -> AddRef( pUnk ) );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLERELEASE )  // ( hOleObject )
  {
     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );

     hb_retnl( pUnk -> lpVtbl -> Release( pUnk ) );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( COMFUNCTION )  // ( hOleObject, nFunc, uParams... )
  {
     typedef HRESULT ( STDMETHODCALLTYPE * COMFunc ) ( IUnknown * pUnk );

     IUnknown * pUnk = ( IUnknown * ) hb_parnl( 1 );
     COMFunc pFunc;
     COMFunc *vTbl;
     int i, iParams = hb_pcount();
     double doubles[16];
     LPVOID ptros[16];

     vTbl = ( COMFunc * ) &( pUnk -> lpVtbl -> QueryInterface );
     vTbl += hb_parni( 2 ) + 2;
     pFunc = *vTbl;

     for( i = iParams; i > 2; i-- )
     {
        char *sString;
        int iInt;
        double dDouble;

        switch ( ( hb_parinfo( i ) & ~HB_IT_BYREF) )
        {
           case HB_IT_STRING:
           case HB_IT_MEMO:
             sString = hb_parc( i );
             __asm push sString
             break;

           case HB_IT_LOGICAL:
             if ( ISBYREF( i ) )
             {
                ptros[ i ] = (LPVOID) hb_parl( i );
                sString = ( char * ) &ptros[ i ];
                __asm push sString
             }
             else
             {
                iInt = hb_parl( i );
                __asm push iInt
             }
             break;

           case HB_IT_INTEGER:
           case HB_IT_LONG:
             if ( ISBYREF( i ) )
             {
                ptros[ i ] = (LPVOID) hb_parnl( i );
                sString = ( char * ) &ptros[ i ];
                __asm push sString
             }
             else
             {
                iInt = hb_parnl( i );
                __asm push iInt
             }
             break;

           case HB_IT_DOUBLE:
             if ( ISBYREF( i ) )
             {
                doubles[ i ] = hb_parnd( i );
                sString = ( char * ) &doubles[ i ];
                __asm push sString
             }
             else
             {
                dDouble = hb_parl( i );
                __asm push dDouble
             }
             break;

           case HB_IT_DATE:
             if ( ISBYREF( i ) )
             {
                doubles[ i ] = DateToDbl( hb_pards( i ) );
                sString = ( char * ) &doubles[ i ];
                __asm push sString
             }
             else
             {
                dDouble = DateToDbl( hb_pards( i ) );
                __asm push dDouble
             }
             break;

           default:
             sString = ( char * ) NULL;
             __asm push sString
             break;
        }
     }

     nOleError = pFunc( pUnk );

     for ( i = 3; i <= iParams; i++ )
     {
        if ( ISBYREF( i ) )
        {
           switch ( (hb_parinfo( i ) & ~HB_IT_BYREF) )
           {
              case HB_IT_STRING:
              case HB_IT_MEMO:
                hb_storc( (char *) hb_parc( i ), i );
                break;

              case HB_IT_LOGICAL:
                hb_storl( (long) ptros[ i ], i );
                break;

              case HB_IT_INTEGER:
              case HB_IT_LONG:
                hb_stornl( (long) ptros[ i ], i );
                break;

              case HB_IT_DOUBLE:
                hb_stornd( doubles[ i ], i );
                break;

              case HB_IT_DATE:
                hb_stords( DblToDate( doubles[ i ] ), i );
                break;
           }
        }
     }

     hb_ret();
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEERROR )
  {
     hb_retnl( (LONG) nOleError );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEISOBJECT )
  {
     hb_retl( RetVal.n1.n2.vt == VT_DISPATCH );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLEUNINITIALIZE )
  {
     if( nInitialized > 0 )
     {
        nInitialized--;

        if ( nInitialized == 0 )
        {
           OleUninitialize();
        }
     }
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( OLE2TXTERROR )
  {
     switch ( (LONG) nOleError)
     {
        case S_OK:
           hb_retc( "S_OK" );
           break;

        case CO_E_CLASSSTRING:
           hb_retc( "CO_E_CLASSSTRING" );
           break;

        case OLE_E_WRONGCOMPOBJ:
           hb_retc( "OLE_E_WRONGCOMPOBJ" );
           break;

        case REGDB_E_CLASSNOTREG:
           hb_retc( "REGDB_E_CLASSNOTREG" );
           break;

        case REGDB_E_WRITEREGDB:
           hb_retc( "REGDB_E_WRITEREGDB" );
           break;

        case E_OUTOFMEMORY:
           hb_retc( "E_OUTOFMEMORY" );
           break;

        case E_INVALIDARG:
           hb_retc( "E_INVALIDARG" );
           break;

        case E_UNEXPECTED:
           hb_retc( "E_UNEXPECTED" );
           break;

        case DISP_E_UNKNOWNNAME:
           hb_retc( "DISP_E_UNKNOWNNAME" );
           break;

        case DISP_E_UNKNOWNLCID:
           hb_retc( "DISP_E_UNKNOWNLCID" );
           break;

        case DISP_E_BADPARAMCOUNT:
           hb_retc( "DISP_E_BADPARAMCOUNT" );
           break;

        case DISP_E_BADVARTYPE:
           hb_retc( "DISP_E_BADVARTYPE" );
           break;

        case DISP_E_EXCEPTION:
           hb_retc( "DISP_E_EXCEPTION" );
           break;

        case DISP_E_MEMBERNOTFOUND:
           hb_retc( "DISP_E_MEMBERNOTFOUND" );
           break;

        case DISP_E_NONAMEDARGS:
           hb_retc( "DISP_E_NONAMEDARGS" );
           break;

        case DISP_E_OVERFLOW:
           hb_retc( "DISP_E_OVERFLOW" );
           break;

        case DISP_E_PARAMNOTFOUND:
           hb_retc( "DISP_E_PARAMNOTFOUND" );
           break;

        case DISP_E_TYPEMISMATCH:
           hb_retc( "DISP_E_TYPEMISMATCH" );
           break;

        case DISP_E_UNKNOWNINTERFACE:
           hb_retc( "DISP_E_UNKNOWNINTERFACE" );
           break;

        case DISP_E_PARAMNOTOPTIONAL:
           hb_retc( "DISP_E_PARAMNOTOPTIONAL" );
           break;

        default:
           hb_retc( "Unknown error" );
           break;
     };
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( ANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
  {
     WORD wLen;
     LPSTR cOut;

     wLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, hb_parc( 1 ), -1, 0, 0 );
     cOut = ( char * ) hb_xgrab( wLen * 2 );
     MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, hb_parc( 1 ), -1, ( LPWSTR ) cOut, wLen );

     hb_retclen( cOut, wLen * 2 - 1 );
     hb_xfree( cOut );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( WIDETOANSI )  // ( cWideStr, nLen ) -> cAnsiStr
  {
     WORD wLen;
     LPWSTR cWideStr;
     LPSTR cOut = NULL;

     cWideStr = ( LPWSTR ) hb_parc( 1 );
     wLen = WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, cWideStr, -1, cOut, 0, NULL, NULL );
     cOut = ( char * ) hb_xgrab( wLen );
     WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, cWideStr, -1, cOut, wLen, NULL, NULL );

     hb_retc( cOut );
     hb_xfree( cOut );
  }

  //---------------------------------------------------------------------------//

  HB_FUNC( MESSAGEBOX )
  {
      hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parc( 2 ), hb_parc( 3 ), hb_parni( 4 ) ) );
  }

  //---------------------------------------------------------------------------//

#pragma ENDDUMP
