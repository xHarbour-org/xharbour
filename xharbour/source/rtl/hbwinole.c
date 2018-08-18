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

//---------------------------------------------------------------------------//
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

#if defined( HB_OS_WIN )

#include <ctype.h>

#include <windows.h>
#include <ole2.h>
#include <oleauto.h>

#if defined( __POCC__ )
   #include "wchar.h"
#endif

#ifndef __MINGW32__
// Missing in Mingw V 2.
//#include <OleDB.h>
#endif

#if ( defined( __MINGW32__ ) || defined( __DMC__ ) || defined( __WATCOMC__ ) )
   #include <ocidl.h>
#endif

#include <shlobj.h>

#ifdef __MINGW32__
// Missing in oleauto.h
WINOLEAUTAPI VarR8FromDec( DECIMAL * pdecIn, DOUBLE * pdblOut );
#endif

#if ( defined( __DMC__ ) || defined( __MINGW32__ ) || ( defined( __WATCOMC__ ) && ! defined( __FORCE_LONG_LONG__ ) ) )
   #define HB_LONG_LONG_OFF
#endif

static void RetValue( void );

static HRESULT    s_nOleError;
static HB_ITEM    OleAuto;

static PHB_DYNS s_pSym_TOleAuto       = NULL;
static PHB_DYNS s_pSym_hObj           = NULL;
static PHB_DYNS s_pSym_New            = NULL;
static PHB_DYNS s_pSym_cClassName     = NULL;

static PHB_DYNS s_pSym_VTWrapper      = NULL;
static PHB_DYNS s_pSym_VTArrayWrapper = NULL;
static PHB_DYNS s_pSym_vt             = NULL;
static PHB_DYNS s_pSym_Value          = NULL;

static DISPPARAMS s_EmptyDispParams;

static VARIANTARG RetVal, OleVal;

#if defined( __WATCOMC__ )
HB_EXTERN_BEGIN
const GUID  GUID_NULL                     = { 0x00000000, 0x0000, 0x0000, { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } };
//const IID   IID_IUnknown                  = { 0x00000000, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
//const IID   IID_IOleObject                = { 0x00000112, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
const IID   IID_IDispatch                 = { 0x00020400, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
const IID   IID_IEnumVARIANT              = { 0x00020404, 0x0000, 0x0000, { 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46 } };
//const IID   IID_IPicture                  = { 0x7bf80980, 0xbf32, 0x101a, { 0x8b, 0xbb, 0x00, 0xaa, 0x00, 0x30, 0x0c, 0xab } };
const IID   IID_IClassFactory2            = { 0xb196b28f, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
//const IID   IID_IProvideClassInfo         = { 0xb196b283, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
//const IID   IID_IProvideClassInfo2        = { 0xa6bc3ac0, 0xdbaa, 0x11ce, { 0x9d, 0xe3, 0x00, 0xaa, 0x00, 0x4b, 0xb8, 0x51 } };
//const IID   IID_IConnectionPointContainer = { 0xb196b284, 0xbab4, 0x101a, { 0xb6, 0x9c, 0x00, 0xaa, 0x00, 0x34, 0x1d, 0x07 } };
HB_EXTERN_END
#endif

static EXCEPINFO excep = { 0 };

static DISPID  lPropPut = DISPID_PROPERTYPUT;
static UINT    uArgErr;

HB_EXTERN_BEGIN
HB_EXPORT HRESULT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT * pVariant );
HB_EXPORT BSTR    hb_oleAnsiToSysString( const char * cString );
HB_EXPORT LPWSTR  hb_oleAnsiToWide( const char * cString );
HB_EXPORT char *  hb_oleWideToAnsi( LPWSTR wString );
HB_EXPORT void    hb_oleItemToVariant( VARIANT * pVariant, PHB_ITEM pItem );
HB_EXTERN_END

static PHB_ITEM SafeArrayToArray( SAFEARRAY * parray, UINT iDim, long * rgIndices, VARTYPE vt );

#if defined( __cplusplus ) && ( ( defined( __WATCOMC__ ) && ( __WATCOMC__ >= 1280 ) ) )
   #define HBOLE_IID( p )  p
#else
   #define HBOLE_IID( p )  &p
#endif

#define HB_V_VT(X)         ((X).n1.n2.vt)
#define PHB_V_VT(X)        ((X)->n1.n2.vt)
#define HB_V_UNION(X, Y)   ((X).n1.n2.n3.Y)
#define PHB_V_UNION(X, Y)  ((X)->n1.n2.n3.Y)
#define HB_VTBL( X )       (X)->lpVtbl


//---------------------------------------------------------------------------//
HB_FUNC( HB_OLEINIT )
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

//---------------------------------------------------------------------------//
HB_FUNC( OLEENUMERATE_ENUMERATE )
{
   IEnumVARIANT * pEnumVariant     = ( IEnumVARIANT * ) hb_parptr( 1 );
   ULONG        * pcElementFetched = NULL;

   if( HB_VTBL( pEnumVariant )->Next( pEnumVariant, 1, &RetVal, pcElementFetched ) == S_OK )
   {
      RetValue();
   }
   else
   {
      hb_vmRequestBreak( NULL );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEENUMERATE_END )
{
   IEnumVARIANT * pEnumVariant = ( IEnumVARIANT * ) hb_parptr( 1 );

   HB_VTBL( pEnumVariant )->Release( pEnumVariant );
}

//---------------------------------------------------------------------------//
HB_EXPORT BSTR hb_oleAnsiToSysString( const char * cString )
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
HB_EXPORT LPWSTR hb_oleAnsiToWide( const char * cString )
{
   int nConvertedLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, cString, -1, NULL, 0 );

   if( nConvertedLen )
   {
      LPWSTR wString = ( LPWSTR ) hb_xgrab( nConvertedLen * 2 );

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
HB_EXPORT char * hb_oleWideToAnsi( LPWSTR wString )
{
   int nConvertedLen = WideCharToMultiByte( CP_ACP, 0, wString, -1, NULL, 0, NULL, NULL );

   if( nConvertedLen )
   {
      char * cString = ( char * ) hb_xgrab( nConvertedLen );

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
HB_EXPORT void hb_oleItemToVariant( VARIANT * pVariant, PHB_ITEM pItem )
{
   BOOL           bByRef;
   VARIANT        mVariant;
   VARTYPE        vt;
   SAFEARRAYBOUND rgsabound;
   void           * pSource; // = NULL;
   unsigned long  i;
   char           * sString;

   if( HB_IS_BYREF( pItem ) )
   {
      pItem  = hb_itemUnRef( pItem );
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
         //PHB_V_VT( pVariant ) = VT_EMPTY;
         break;

      case HB_IT_STRING:
      case HB_IT_MEMO:
      {
         HB_SIZE ulLen = hb_itemGetCLen( pItem );

         sString = hb_itemGetCPtr( pItem );

         // Check for hidden signature of SafeArrayToArray()
         if( ( int ) ( pItem->item.asString.allocated - ulLen ) >= 5 &&
             sString[ ulLen ] == 0x7A && sString[ ulLen + 1 ] == 0x7B &&
             sString[ ulLen + 2 ] == 0x7C && sString[ ulLen + 3 ] == 0x7D )
         {
            vt = ( VARTYPE ) sString[ ulLen + 4 ];
            goto ItemToVariant_StringArray;
         }

         if( bByRef )
         {
            hb_itemPutCRawStatic( pItem, ( const char * ) hb_oleAnsiToSysString( sString ), ulLen * 2 + 1 );

            PHB_V_VT( pVariant )              = VT_BYREF | VT_BSTR;
            PHB_V_UNION( pVariant, pbstrVal ) = ( BSTR * ) &( pItem->item.asString.value );
            //wprintf( L"*** BYREF >%s<\n", *PHB_V_UNION( pVariant, bstrVal ) );
         }
         else
         {
            PHB_V_VT( pVariant )             = VT_BSTR;
            PHB_V_UNION( pVariant, bstrVal ) = hb_oleAnsiToSysString( sString );
            //wprintf( L"*** >%s<\n", PHB_V_UNION( pVariant, bstrVal ) );
         }
         break;
      }

      case HB_IT_LOGICAL:
         if( bByRef )
         {
            PHB_V_VT( pVariant )               = VT_BYREF | VT_BOOL;
            PHB_V_UNION( pVariant, pboolVal )  = ( short * ) &( pItem->item.asLogical.value );
            *PHB_V_UNION( pVariant, pboolVal ) = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
            //pItem->type = HB_IT_LONG;
         }
         else
         {
            PHB_V_VT( pVariant )             = VT_BOOL;
            PHB_V_UNION( pVariant, boolVal ) = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
         }
         break;

      case HB_IT_INTEGER:
#if HB_INT_MAX == INT16_MAX
         if( bByRef )
         {
            PHB_V_VT( pVariant )           = VT_BYREF | VT_I2;
            PHB_V_UNION( pVariant, piVal ) = &( pItem->item.asInteger.value );
         }
         else
         {
            PHB_V_VT( pVariant )          = VT_I2;
            PHB_V_UNION( pVariant, iVal ) = hb_itemGetNI( pItem );
         }
         break;
#else
         if( bByRef )
         {
            PHB_V_VT( pVariant )           = VT_BYREF | VT_I4;
            PHB_V_UNION( pVariant, plVal ) = ( long * ) &( pItem->item.asInteger.value );
         }
         else
         {
            PHB_V_VT( pVariant )          = VT_I4;
            PHB_V_UNION( pVariant, lVal ) = hb_itemGetNL( pItem );
         }
         break;
#endif
      case HB_IT_LONG:
#if HB_LONG_MAX == INT32_MAX || defined( HB_LONG_LONG_OFF )
         if( bByRef )
         {
            PHB_V_VT( pVariant )           = VT_BYREF | VT_I4;
            PHB_V_UNION( pVariant, plVal ) = ( long * ) &( pItem->item.asLong.value );
         }
         else
         {
            PHB_V_VT( pVariant )          = VT_I4;
            PHB_V_UNION( pVariant, lVal ) = hb_itemGetNL( pItem );
         }
#else
         if( bByRef )
         {
            PHB_V_VT( pVariant )            = VT_BYREF | VT_I8;
            PHB_V_UNION( pVariant, pllVal ) = &( pItem->item.asLong.value );
         }
         else
         {
            PHB_V_VT( pVariant )           = VT_I8;
            PHB_V_UNION( pVariant, llVal ) = hb_itemGetNLL( pItem );
         }
#endif
         break;

      case HB_IT_DOUBLE:
         if( bByRef )
         {
            PHB_V_VT( pVariant )             = VT_BYREF | VT_R8;
            PHB_V_UNION( pVariant, pdblVal ) = &( pItem->item.asDouble.value );
            pItem->type                      = HB_IT_DOUBLE;
         }
         else
         {
            PHB_V_VT( pVariant )            = VT_R8;
            PHB_V_UNION( pVariant, dblVal ) = hb_itemGetND( pItem );
         }
         break;

      case HB_IT_DATE:
         if( pItem->item.asDate.value == 0 )
         {
            PHB_V_VT( pVariant ) = VT_NULL;
         }
         else if( bByRef )
         {
            pItem->item.asDouble.value = hb_itemGetDTD( pItem ) - ( double ) 2415019;
            pItem->type                = HB_IT_DOUBLE;

            PHB_V_VT( pVariant )             = VT_BYREF | VT_DATE;
            PHB_V_UNION( pVariant, pdblVal ) = &( pItem->item.asDouble.value );
         }
         else
         {
            PHB_V_VT( pVariant )            = VT_DATE;
            PHB_V_UNION( pVariant, dblVal ) = hb_itemGetDTD( pItem ) - ( double ) 2415019;
         }
         break;

      case HB_IT_TIMEFLAG:
         if( pItem->item.asDate.value == 0 )
         {
            PHB_V_VT( pVariant ) = VT_NULL;
         }
         else if( bByRef )
         {
            pItem->item.asDouble.value = ( double ) pItem->item.asDate.value +
                                         ( double ) pItem->item.asDate.time / HB_MILLISECS_PER_DAY -
                                         ( double ) 2415019;
            pItem->type                = HB_IT_DOUBLE;

            PHB_V_VT( pVariant )             = VT_BYREF | VT_DATE;
            PHB_V_UNION( pVariant, pdblVal ) = &( pItem->item.asDouble.value );
         }
         else
         {
            PHB_V_VT( pVariant )            = VT_DATE;
            PHB_V_UNION( pVariant, dblVal ) = ( double ) pItem->item.asDate.value +
                                              ( double ) pItem->item.asDate.time / HB_MILLISECS_PER_DAY -
                                              ( double ) 2415019;
         }
         break;

      case HB_IT_POINTER:
         PHB_V_VT( pVariant )           = VT_PTR;
         PHB_V_UNION( pVariant, byref ) = hb_itemGetPtr( pItem );
         break;

      case HB_IT_ARRAY:
         if( HB_IS_OBJECT( pItem ) )
         {
            if( hb_clsIsParent( pItem->item.asArray.value->uiClass, "TOLEAUTO" ) )
            {
               IDispatch * pDisp; // = NULL;

               hb_vmPushSymbol( s_pSym_hObj->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );
               pDisp = ( IDispatch * ) hb_parns( -1 );

               HB_VTBL( pDisp )->AddRef( pDisp );
               //TraceLog( NULL, "TOleAuto: Dispatch: in: %s(%i)%ld\n", pDisp, __FILE__, __LINE__ );

               if( bByRef )
               {
                  PHB_V_VT( pVariant ) = ( VT_DISPATCH | VT_BYREF );
                  // Hack!!! Using high 4 bytes of the union (llVal)
                  *( ( IDispatch ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1 ) = pDisp;
                  PHB_V_UNION( pVariant, ppdispVal ) = ( IDispatch ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1;
               }
               else
               {
                  PHB_V_VT( pVariant )              = VT_DISPATCH;
                  PHB_V_UNION( pVariant, pdispVal ) = pDisp;
               }
            }
            // MUST be before "VTWRAPPER"
            else if( hb_clsIsParent( pItem->item.asArray.value->uiClass, "VTARRAYWRAPPER" ) )
            {
               // vt := oVTArray:vt
               hb_vmPushSymbol( s_pSym_vt->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               vt = ( VARTYPE ) hb_parnl( -1 );

               // aArray := oVTArray:Value
               hb_vmPushSymbol( s_pSym_Value->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               // Intentionally not using hb_itemCopy() or hb_itemForwardValue()
               pItem = hb_stackReturnItem();

               if( ( vt == VT_I1 || vt == VT_UI1 ) && HB_IS_STRING( pItem ) )
               {
                  SAFEARRAY * parray;

                  sString = hb_itemGetCPtr( pItem );

 ItemToVariant_StringArray:

                  rgsabound.cElements = ( ULONG ) hb_itemGetCLen( pItem );
                  rgsabound.lLbound   = 0;

                  parray = SafeArrayCreate( vt, 1, &rgsabound );

                  if( bByRef )
                  {
                     PHB_V_VT( pVariant ) = ( VT_ARRAY | VT_BYREF | vt );
                     // Hack!!! Using high 4 bytes of the union (llVal)
                     *( ( SAFEARRAY ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1 ) = parray;
                     PHB_V_UNION( pVariant, pparray ) = ( SAFEARRAY ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1;
                  }
                  else
                  {
                     PHB_V_VT( pVariant )            = ( VT_ARRAY | vt );
                     PHB_V_UNION( pVariant, parray ) = parray;
                  }

                  for( i = 0; i < rgsabound.cElements; i++ )
                  {
                     SafeArrayPutElement( parray, ( LONG * ) &i, &( sString[ i ] ) );
                  }

                  break;
               }

               VariantInit( &mVariant );
               pSource = &HB_V_UNION( mVariant, cVal );

               goto ItemToVariant_ProcessArray;
            }
            else if( hb_clsIsParent( pItem->item.asArray.value->uiClass, "VTWRAPPER" ) )
            {
               // vt := oVT:vt
               hb_vmPushSymbol( s_pSym_vt->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               PHB_V_VT( pVariant ) = ( VARTYPE ) hb_parnl( -1 );

               //value := oVT:value
               hb_vmPushSymbol( s_pSym_Value->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               switch( PHB_V_VT( pVariant ) )
               {
                  case VT_UNKNOWN:
                     PHB_V_UNION( pVariant, punkVal ) = ( IUnknown * ) hb_parptr( -1 );
                     break;

                  case ( VT_UNKNOWN | VT_BYREF ):
                     // Hack!!! Using high 4 bytes of the union (llVal)
                     *( ( IUnknown ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1 ) = ( IUnknown * ) hb_parptr( -1 );
                     PHB_V_UNION( pVariant, ppunkVal ) = ( IUnknown ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1;
                     break;

                  case VT_ERROR:
                     PHB_V_UNION( pVariant, scode ) = hb_parni( -1 );
                     break;

                  default:
                     TraceLog( NULL, "TOleAuto: Unexpected VT type %p in: %s(%i)!\n", PHB_V_VT( pVariant ), __FILE__, __LINE__ );
               }

               break;
            }
            else
            {
               TraceLog( NULL, "TOleAuto: Class: '%s' not suported!\n", hb_objGetClsName( pItem ) );
            }
         }
         else
         {
            unsigned long i2;
            SAFEARRAY     * parray;

            vt = VT_VARIANT;
            VariantInit( &mVariant );
            pSource = &mVariant;

 ItemToVariant_ProcessArray:

            rgsabound.cElements = ( ULONG ) hb_arrayLen( pItem );
            rgsabound.lLbound   = 0;

            //TraceLog( NULL, "TOleAuto: ItemToVariant() Array len: %i type: %i ByRef: %i in: %s(%i) \n",
            //          rgsabound.cElements, vt, bByRef, __FILE__, __LINE__ );

            parray = SafeArrayCreate( vt, 1, &rgsabound );

            if( bByRef )
            {
               PHB_V_VT( pVariant ) = ( VT_ARRAY | VT_BYREF | vt );
               // Hack!!! Using high 4 bytes of the union (llVal)
               *( ( SAFEARRAY ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1 ) = parray;
               PHB_V_UNION( pVariant, pparray ) = ( SAFEARRAY ** ) ( &PHB_V_UNION( pVariant, lVal ) ) + 1;
            }
            else
            {
               PHB_V_VT( pVariant )            = ( VT_ARRAY | vt );
               PHB_V_UNION( pVariant, parray ) = parray;
            }

            for( i2 = 0; i2 < rgsabound.cElements; i2++ )
            {
               hb_oleItemToVariant( &mVariant, hb_arrayGetItemPtr( pItem, i2 + 1 ) );
               SafeArrayPutElement( parray, ( LONG * ) &i2, pSource );
               VariantClear( &mVariant );
            }
         }
         break;

      default:
         TraceLog( NULL, "TOleAuto: Unexpected type %p in: %s(%i)!\n", pItem->type, __FILE__, __LINE__ );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEANSITOWIDE )  // ( cAnsiStr ) -> cWideStr
{
   const char * cString = hb_parc( 1 );

   if( cString )
   {
      LPWSTR wString = hb_oleAnsiToWide( cString );

      if( wString )
      {
         hb_retclenAdoptRaw( ( char * ) wString, ( wcslen( wString ) + 1 ) * 2 );
         //printf( "Returning: '%ls' Len: %i\n", hb_stackReturnItem()->item.asString.value, hb_stackReturnItem()->item.asString.length );
         return;
      }
   }

   hb_ret();
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEWIDETOANSI )  // ( cWideStr, nLen ) -> cAnsiStr
{
   LPWSTR wString = ( LPWSTR ) hb_parc( 1 );

   if( wString )
   {
      char * cString = hb_oleWideToAnsi( wString );

      if( cString )
      {
         hb_retclenAdopt( cString, strlen( cString ) );
         return;
      }
   }

   hb_ret();
}

//---------------------------------------------------------------------------//
static PHB_ITEM * GetParams( DISPPARAMS * pDispParams, int nOffset )
{
   VARIANTARG * pArgs = NULL;
   int        n, nArgs, nArg;
   PHB_ITEM   * aPrgParams = NULL;

   nArgs = hb_pcount() - nOffset;

   if( nArgs > 0 )
   {
      pArgs      = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * nArgs );
      aPrgParams = ( PHB_ITEM * ) hb_xgrab( sizeof( PHB_ITEM ) * nArgs );

      //printf( "Args: %i\n", nArgs );

      for( n = 0; n < nArgs; n++ )
      {
         // Parameters are processed in reversed order.
         nArg = nArgs - n;
         VariantInit( &( pArgs[ n ] ) );

         aPrgParams[ n ] = hb_stackItemFromBase( nArg + nOffset );

         //TraceLog( NULL, "TOleAuto: N: %i Arg: %i Type: %i %i\n", n, nArg, pParam->type, aPrgParams[ n ]->type );

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
static void FreeParams( DISPPARAMS * pDispParams, PHB_ITEM * aPrgParams )
{
   if( pDispParams->cArgs > 0 )
   {
      IDispatch * pDisp = NULL;
      int       n; //, nParam;
      char      * sString;
      VARIANT   * pVariant;
      PHB_ITEM  pItem;
      BOOL      bByRef;

      for( n = 0; n < ( int ) pDispParams->cArgs; n++ )
      {
         pVariant = &( pDispParams->rgvarg[ n ] );
         pItem    = aPrgParams[ n ];

         if( HB_IS_BYREF( pItem ) )
         {
            bByRef = TRUE;
            pItem  = hb_itemUnRef( pItem );
         }
         else
         {
            bByRef = FALSE;
         }

         //nParam = pDispParams->cArgs - n;

         //TraceLog( NULL, "TOleAuto: *** N: %i, Param: %i Type: %i\n", n, nParam, PHB_V_VT( pVariant ) );

         if( bByRef )
         {
            switch( PHB_V_VT( pVariant ) )
            {
               case VT_BYREF | VT_BSTR:
                  SysFreeString( *PHB_V_UNION( pVariant, pbstrVal ) );
                  sString = hb_oleWideToAnsi( *PHB_V_UNION( pVariant, pbstrVal ) );
                  hb_itemPutCPtr( pItem, sString, strlen( sString ) );
                  break;

               case VT_BSTR:
                  sString = hb_oleWideToAnsi( PHB_V_UNION( pVariant, bstrVal ) );
                  hb_itemPutCPtr( pItem, sString, strlen( sString ) );
                  break;

               case VT_BYREF | VT_BOOL:
                  //( pItem )->type = HB_IT_LOGICAL;
                  hb_itemPutL( pItem, *PHB_V_UNION( pVariant, pboolVal ) == VARIANT_FALSE ? FALSE : TRUE );
                  break;

               case VT_BOOL:
                  hb_itemPutL( pItem, PHB_V_UNION( pVariant, boolVal ) == VARIANT_FALSE ? FALSE : TRUE );
                  break;

               case ( VT_BYREF | VT_DISPATCH ):
                  if( *PHB_V_UNION( pVariant, ppdispVal ) == NULL )
                  {
                     hb_itemClear( pItem );
                     break;
                  }
                  else
                  {
                     pDisp = *PHB_V_UNION( pVariant, ppdispVal );
                  }
               // Intentionally fall through.

               case VT_DISPATCH:
                  if( PHB_V_VT( pVariant ) == VT_DISPATCH )
                  {
                     if( PHB_V_UNION( pVariant, pdispVal ) == NULL )
                     {
                        hb_itemClear( pItem );
                        break;
                     }
                     else
                     {
                        pDisp = PHB_V_UNION( pVariant, pdispVal );
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
                     //HB_VTBL( pDisp )->AddRef( pDisp );

                     //TOleAuto():New( nDispatch )
                     hb_vmPushSymbol( s_pSym_New->pSymbol );
                     hb_itemPushForward( &OleAuto );
                     // hb_vmPushLong( ( LONG ) pDisp );
                     hb_vmPushSize( ( HB_ISIZ ) pDisp );
                     hb_vmSend( 1 );

                     hb_itemForwardValue( pItem, hb_stackReturnItem() );
                  }
                  break;

               case VT_BYREF | VT_I2:
                  hb_itemPutNI( pItem, ( int ) *PHB_V_UNION( pVariant, piVal ) );
                  break;

               case VT_I2:
                  hb_itemPutNI( pItem, ( int ) PHB_V_UNION( pVariant, iVal ) );
                  break;

               case VT_BYREF | VT_I4:
                  hb_itemPutNL( pItem, ( LONG ) *PHB_V_UNION( pVariant, plVal ) );
                  break;

               case VT_I4:
                  hb_itemPutNL( pItem, ( LONG ) PHB_V_UNION( pVariant, lVal ) );
                  break;

#ifndef HB_LONG_LONG_OFF
               case VT_BYREF | VT_I8:
                  hb_itemPutNLL( pItem, ( LONGLONG ) *PHB_V_UNION( pVariant, pllVal ) );
                  break;

               case VT_I8:
                  hb_itemPutNLL( pItem, ( LONGLONG ) PHB_V_UNION( pVariant, llVal ) );
                  break;
#endif
               case VT_BYREF | VT_R8:
                  hb_itemPutND( pItem, *PHB_V_UNION( pVariant, pdblVal ) );
                  break;

               case VT_R8:
                  hb_itemPutND( pItem, PHB_V_UNION( pVariant, dblVal ) );
                  break;

               case VT_BYREF | VT_DATE:
                  hb_itemPutDTD( pItem, *PHB_V_UNION( pVariant, pdblVal ) + ( double ) 2415019 );
                  if( pItem->item.asDate.time )
                     pItem->item.asDate.time++;
                  break;

               case VT_DATE:
                  hb_itemPutDTD( pItem, PHB_V_UNION( pVariant, dblVal ) + ( double ) 2415019 );
                  if( pItem->item.asDate.time )
                     pItem->item.asDate.time++;
                  break;

               case VT_BYREF | VT_EMPTY:
               case VT_EMPTY:
                  hb_itemClear( pItem );
                  break;

               case VT_BYREF | VT_VARIANT:
                  hb_oleItemToVariant( PHB_V_UNION( pVariant, pvarVal ), pItem );
                  break;

               default:
                  if( ( VARTYPE ) ( PHB_V_VT( pVariant ) & ( VT_BYREF | VT_ARRAY ) ) == ( VARTYPE ) ( VT_BYREF | VT_ARRAY ) )
                  {
                     VARTYPE  vt;
                     PHB_ITEM pArray;
                     UINT     iDims       = SafeArrayGetDim( *PHB_V_UNION( pVariant, pparray ) );
                     long     * rgIndices = ( long * ) hb_xgrab( sizeof( long ) * iDims );

                     vt  = PHB_V_VT( pVariant );
                     vt &= ~(VT_ARRAY | VT_BYREF);

                     pArray = SafeArrayToArray( *PHB_V_UNION( pVariant, pparray ), iDims, rgIndices, vt );

                     hb_xfree( ( void * ) rgIndices );

                     hb_itemForwardValue( pItem, pArray );
                     hb_itemRelease( pArray );
                  }
                  else
                  {
                     TraceLog( NULL, "TOleAuto: Unexpected type %p in: %s(%i)!\n", PHB_V_VT( pVariant ), __FILE__, __LINE__ );
                  }
            }
         }
         else
         {
            if( PHB_V_VT( pVariant ) & VT_BYREF )
            {
               TraceLog( NULL, "TOleAuto: Unexpected type %p in: %s(%i)!\n", PHB_V_VT( pVariant ), __FILE__, __LINE__ );
            }
         }

         VariantClear( &( pDispParams->rgvarg[ n ] ) );
      }

      hb_xfree( ( LPVOID ) pDispParams->rgvarg );
      hb_xfree( ( LPVOID ) aPrgParams );
   }
}

//---------------------------------------------------------------------------//
static PHB_ITEM SafeArrayToArray( SAFEARRAY * parray, UINT iDim, long * rgIndices, VARTYPE vt )
{
   long     iFrom, iTo, iLen, i;
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
      void    * pTarget;
      char    * sArray = NULL;

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
            HB_STRING_ALLOC( pArray, ( HB_SIZE ) ( iLen + 5 ) );
            pArray->item.asString.length  = iLen;

            sArray = hb_itemGetCPtr( pArray );

            sArray[ iLen     ] = 0x7A;
            sArray[ iLen + 1 ] = 0x7B;
            sArray[ iLen + 2 ] = 0x7C;
            sArray[ iLen + 3 ] = 0x7D;
            sArray[ iLen + 4 ] = ( char ) ( vt );

            pTarget = NULL;
         }
         else
         {
            hb_arrayNew( pArray, iLen );

            pTarget = &HB_V_UNION( mElem, cVal );
         }
      }

      for( i = iFrom; i <= iTo; i++ )
      {
         rgIndices[ iDim - 1 ] = i;

         if( vt != VT_VARIANT )
         {
            // Get cleared on VariantClear() - don't place out of loop!
            HB_V_VT( mElem ) = vt;

            if( vt == VT_I1 || vt == VT_UI1 )
            {
               SafeArrayGetElement( parray, rgIndices, &( sArray[ i - iFrom ] ) );

               continue;
            }
         }

         if( SUCCEEDED( SafeArrayGetElement( parray, rgIndices, pTarget ) ) )
         {
            //TraceLog( NULL, "TOleAuto: Type: %p in: %s(%i)\n", HB_V_VT( mElem ), __FILE__, __LINE__ );

            hb_oleVariantToItem( pArray->item.asArray.value->pItems + ( i - iFrom ), &mElem );

            VariantClear( &mElem );
         }
      }
   }

   //TraceLog( NULL, "TOleAuto: Return len: %i\n", pArray->item.asArray.value->ulLen );

   // Wrap our array with VTArrayWrapper() class ( aArray := VTArrayWrapper( vt, aArray) )
   if( HB_IS_ARRAY( pArray ) && vt != VT_VARIANT )
   {
      PHB_ITEM pVT = hb_itemPutNL( hb_itemNew( NULL ), ( LONG ) vt );

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
HRESULT hb_oleVariantToItem( PHB_ITEM pItem, VARIANT * pVariant )
{
   PHB_ITEM  pOleAuto;
   IUnknown  * pUnk  = NULL;
   IDispatch * pDisp = NULL;
   VARTYPE   vt;
   BOOL      bByRef;

   hb_itemClear( pItem );

   // Don't "optimize" (VT_ARRAY | VT_VARIANT) must not match!
   while( PHB_V_VT( pVariant ) == ( VT_BYREF | VT_VARIANT ) ||
          PHB_V_VT( pVariant ) == VT_VARIANT ||
          PHB_V_VT( pVariant ) == VT_BYREF )
   {
      pVariant = PHB_V_UNION( pVariant, pvarVal );
   }

   vt     = PHB_V_VT( pVariant ) & ~VT_BYREF;
   bByRef = PHB_V_VT( pVariant ) & VT_BYREF;

   switch( vt )
   {
      case VT_BSTR:
      {
         char * sString;

         sString = hb_oleWideToAnsi( bByRef ? *PHB_V_UNION( pVariant, pbstrVal ) :
                                              PHB_V_UNION( pVariant, bstrVal ) );
         if( sString )
            hb_itemPutCPtr( pItem, sString, strlen( sString ) );
         else
            hb_itemPutC( pItem, NULL );
         break;
      }

      case VT_BOOL:
         hb_itemPutL( pItem, ( bByRef ? *PHB_V_UNION( pVariant, pboolVal ) :
                                        PHB_V_UNION( pVariant, boolVal ) ) ==
                             VARIANT_FALSE ? FALSE : TRUE );
         break;

      case VT_UNKNOWN:
         pUnk = bByRef ? *PHB_V_UNION( pVariant, ppunkVal ) :
                         PHB_V_UNION( pVariant, punkVal );
         if( pUnk )
         {
            // HB_VTBL( pUnk )->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) &pDisp );
            // Not sure if this is correct, but by this change GCC now quiet
            // and also the other compilers
            HB_VTBL( pUnk )->QueryInterface( pUnk, ( REFIID ) HBOLE_IID( IID_IDispatch ), ( void ** ) pDisp );
         }
         // Intentionally fall through

      case VT_DISPATCH:
         if( vt == VT_DISPATCH )
         {
            pDisp = bByRef ? *PHB_V_UNION( pVariant, ppdispVal ) :
                             PHB_V_UNION( pVariant, pdispVal );
         }

         if( pDisp == NULL )
         {
            if( pUnk )
            {
               PHB_ITEM pVT      = hb_itemPutNL( hb_itemNew( NULL ), ( LONG ) PHB_V_VT( pVariant ) );
               PHB_ITEM pUnknown = hb_itemPutPtr( hb_itemNew( NULL ), ( void * ) pUnk );

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
            // TOleAuto():New( nDispatch )
            hb_vmPushSymbol( s_pSym_New->pSymbol );
            hb_itemPushForward( pOleAuto );
            // hb_vmPushLong( ( LONG ) pDisp );
            hb_vmPushSize( ( HB_ISIZ ) pDisp );
            hb_vmSend( 1 );

            // If retrieved from IUnknown than doubly added!
            if( vt == VT_UNKNOWN )
               HB_VTBL( pDisp )->Release( pDisp );

            hb_itemRelease( pOleAuto );

            // Safety!
            hb_vmRequestReset();

            if( pItem != hb_stackReturnItem() )
               hb_itemForwardValue( pItem, hb_stackReturnItem() );

            //printf( "Dispatch: %ld %ld\n", ( LONG ) pDisp, (LONG) hb_stackReturnItem()->item.asArray.value );
         }
         break;

      case VT_I1:                   // Byte
      case VT_UI1:
         hb_itemPutNI( pItem, ( short ) ( bByRef ? *PHB_V_UNION( pVariant, pbVal ) :
                                                   PHB_V_UNION( pVariant, bVal ) ) );
         break;

      case VT_I2:                   // Short (2 bytes)
      case VT_UI2:
         hb_itemPutNI( pItem, ( short ) ( bByRef ? *PHB_V_UNION( pVariant, piVal ) :
                                                   PHB_V_UNION( pVariant, iVal ) ) );
         break;

      case VT_I4:                   // Long (4 bytes)
      case VT_UI4:
      case VT_INT:
      case VT_UINT:
         hb_itemPutNL( pItem, ( LONG ) ( bByRef ? *PHB_V_UNION( pVariant, plVal ) :
                                                  PHB_V_UNION( pVariant, lVal ) ) );
         break;

      case VT_R4:                   // Single
         hb_itemPutND( pItem, bByRef ? *PHB_V_UNION( pVariant, pfltVal ) :
                                       PHB_V_UNION( pVariant, fltVal ) );
         break;

      case VT_R8:                   // Double
         hb_itemPutND( pItem, bByRef ? *PHB_V_UNION( pVariant, pdblVal ) :
                                       PHB_V_UNION( pVariant, dblVal ) );
         break;

      case VT_CY:                   // Currency
      {
         double tmp = 0;

         VarR8FromCy( bByRef ? *PHB_V_UNION( pVariant, pcyVal ) :
                               PHB_V_UNION( pVariant, cyVal ), &tmp );
         hb_itemPutND( pItem, tmp );
         break;
      }

      case VT_DECIMAL:              // Decimal
      {
         double tmp = 0;

         VarR8FromDec( bByRef ? PHB_V_UNION( pVariant, pdecVal ) :
                                &pVariant->n1.decVal, &tmp );
         hb_itemPutND( pItem, tmp );
         break;
      }

      case VT_DATE:                 // Date
         hb_itemPutDTD( pItem, bByRef ? *PHB_V_UNION( pVariant, pdblVal ) :
                                        PHB_V_UNION( pVariant, dblVal ) + ( double ) 2415019 );
         if( pItem->item.asDate.time )
            pItem->item.asDate.time++;
         break;

      case VT_EMPTY:
      case VT_NULL:
         break;

      /*
         case VT_VARIANT:
         hb_oleVariantToItem( pItem, PHB_V_UNION( pVariant, pvarVal ) );
         break;
       */

      case VT_PTR:
         hb_itemPutPtr( pItem, PHB_V_UNION( pVariant, byref ) );
         break;

      default:
         if( vt & VT_ARRAY )
         {
            UINT      iDims;
            long      * rgIndices;
            PHB_ITEM  pArray;
            SAFEARRAY * psa;

            psa = bByRef ? *PHB_V_UNION( pVariant, pparray ) :
                           PHB_V_UNION( pVariant, parray );

            if( psa )
            {
               iDims     = SafeArrayGetDim( psa );
               rgIndices = ( long * ) hb_xgrab( sizeof( long ) * iDims );

               vt &= ~VT_ARRAY;

               //TraceLog( NULL, "TOleAuto: Type: %p in: %s(%i)\n", vt, __FILE__, __LINE__ );

               pArray = SafeArrayToArray( psa, iDims, rgIndices, vt );

               hb_xfree( ( void * ) rgIndices );

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
            TraceLog( NULL, "TOleAuto: Unexpected type %p in: %s(%i)!\n", vt, __FILE__, __LINE__ );
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
}

//---------------------------------------------------------------------------//
HB_FUNC( OLESHOWEXCEPTION )
{
   if( ( LONG ) s_nOleError == DISP_E_EXCEPTION )
   {
      char * source, * description;

      source      = hb_oleWideToAnsi( excep.bstrSource );
      description = hb_oleWideToAnsi( excep.bstrDescription );

      MessageBox( NULL, description, source, MB_ICONHAND );

      hb_xfree( source );
      hb_xfree( description );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEEXCEPTIONSOURCE )
{
   if( ( LONG ) s_nOleError == DISP_E_EXCEPTION )
   {
      char * source = hb_oleWideToAnsi( excep.bstrSource );

      hb_retcAdopt( source );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEEXCEPTIONDESCRIPTION )
{
   if( ( LONG ) s_nOleError == DISP_E_EXCEPTION )
   {
      char * description = hb_oleWideToAnsi( excep.bstrDescription );

      hb_retcAdopt( description );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEERROR )
{
   hb_retnl( ( LONG ) s_nOleError );
}

//---------------------------------------------------------------------------//
static void ReleaseExcepInfo( void )
{
   if( excep.bstrSource != NULL )
      SysFreeString( excep.bstrSource );

   if( excep.bstrDescription != NULL )
      SysFreeString( excep.bstrDescription );

   if( excep.bstrHelpFile != NULL )
      SysFreeString( excep.bstrHelpFile );

   memset( ( LPBYTE ) &excep, 0, sizeof( excep ) );
}

//---------------------------------------------------------------------------//
static char * Ole2TxtError( void )
{
   switch( ( LONG ) s_nOleError )
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
         TraceLog( NULL, "TOleAuto: Error %p\n", s_nOleError );
         return "Unknown error";
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLE2TXTERROR )
{
   hb_retc( Ole2TxtError() );
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEMESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) hb_parns( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ) ) );
}

//---------------------------------------------------------------------------//
HB_FUNC( CREATEOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] [, cLicense] )
{
   BSTR  bstrClassID;
   IID   ClassID, iid;
   LPIID riid  = ( LPIID ) &IID_IDispatch;
   void  * pDisp = NULL; // IDispatch

   /* void *
    * used intentionally to inform compiler that there is no
    * strict-aliasing
    */
   bstrClassID = hb_oleAnsiToSysString( hb_parcx( 1 ) );

   if( hb_parcx( 1 )[ 0 ] == '{' )
   {
      s_nOleError = CLSIDFromString( bstrClassID, ( LPCLSID ) &ClassID );
   }
   else
   {
      s_nOleError = CLSIDFromProgID( bstrClassID, ( LPCLSID ) &ClassID );
   }

   SysFreeString( bstrClassID );

   //TraceLog( NULL, "TOleAuto: Result: %p\n", s_nOleError );

   if( ISCHAR( 2 ) )
   {
      if( hb_parcx( 2 )[ 0 ] == '{' )
      {
         bstrClassID = hb_oleAnsiToSysString( hb_parc( 2 ) );
         s_nOleError = CLSIDFromString( bstrClassID, &iid );
         SysFreeString( bstrClassID );
      }
      else
      {
         HB_MEMCPY( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
      }

      riid = &iid;
   }

   if( SUCCEEDED( s_nOleError ) )
   {
      if( ISCHAR( 3 ) )
      {
         IClassFactory2 * pCF;
         s_nOleError = CoGetClassObject( ( REFCLSID ) &ClassID, CLSCTX_SERVER, NULL, ( REFIID ) ( LPIID ) HBOLE_IID( IID_IClassFactory2 ), ( LPVOID * ) &pCF );

         if( SUCCEEDED( s_nOleError ) )
         {
            BSTR bstrLic = hb_oleAnsiToSysString( hb_parc( 3 ) );

            s_nOleError = HB_VTBL( pCF )->CreateInstanceLic( pCF, NULL, NULL, ( REFIID ) riid, bstrLic, &pDisp );

            SysFreeString( bstrLic );
            HB_VTBL( pCF )->Release( pCF );
         }
      }
      else
      {
         //TraceLog( NULL, "TOleAuto: Class: %i\n", ClassID );
         s_nOleError = CoCreateInstance( ( REFCLSID ) HBOLE_IID( ClassID ), NULL, CLSCTX_SERVER, ( REFIID ) riid, &pDisp );
         //TraceLog( NULL, "TOleAuto: Result: %p\n", s_nOleError );
      }
   }

   // hb_retnl( ( LONG ) pDisp );
   hb_retns( ( HB_ISIZ ) pDisp );
}

//---------------------------------------------------------------------------//
static HRESULT GetObjectCLSID( LPSTR lpszOle, LPCLSID rclsid, LPSTR * lpszRet )
{
   HRESULT hr = REGDB_E_CLASSNOTREG;
   LPWSTR  lpwz;

   lpwz = hb_oleAnsiToWide( lpszOle );

   if( lpwz )
   {
      if( lpszOle[ 0 ] == '{' )
         hr = CLSIDFromString( lpwz, rclsid );	/* NOERROR == 0 */
      else
         hr = CLSIDFromProgID( lpwz, rclsid );	/* S_OK == 0 */

      hb_xfree( lpwz );

      if( hr == 0 )
      {
         if( lpszOle[ 0 ] == '{' )
            hr = ProgIDFromCLSID( rclsid, &lpwz );  /* S_OK */
         else
            hr = StringFromCLSID( rclsid, &lpwz );  /* S_OK */

         if( hr == S_OK && lpwz )
         {
            *lpszRet = hb_oleWideToAnsi( lpwz );

            CoTaskMemFree( lpwz );
         }
      }
   }

   return hr;
}

//---------------------------------------------------------------------------//
HB_FUNC( ISOLEOBJECTREGISTERED ) // ( cOleName|cCLSID [, @cCLSID|@cOleName] )
{
   HRESULT hr;
   LPSTR   szOLE = (LPSTR) hb_parcx( 1 );
   LPSTR   szRET = NULL;
   CLSID   clsid;

   hr = GetObjectCLSID( szOLE, &clsid, &szRET );

   if( hr == 0 )
   {
      if( ISBYREF( 2 ) )
         hb_storclenAdopt( szRET, strlen( szRET ), 2 );
      else
         hb_xfree( szRET );
   }

   hb_retl( hr == 0 );
}

//---------------------------------------------------------------------------//
HB_FUNC( ISOLEOBJECTACTIVE ) // ( cOleName|cCLSID [, @cCLSID|@cOleName] )
{
   HRESULT hr;
   LPSTR   szOLE = (LPSTR) hb_parcx( 1 );
   LPSTR   szRET = NULL;
   CLSID   clsid;

   hr = GetObjectCLSID( szOLE, &clsid, &szRET );

   if( hr == 0 )
   {
      IUnknown * pUnk;

      hr = GetActiveObject( &clsid, NULL, &pUnk );

      if( ISBYREF( 2 ) )
         hb_storclenAdopt( szRET, strlen( szRET ), 2 );
      else
         hb_xfree( szRET );
   }

   hb_retl( hr == 0 );
}

//---------------------------------------------------------------------------//
HB_FUNC( GETOLEOBJECT ) // ( cOleName | cCLSID  [, cIID ] )
{
   BSTR     bstrClassID;
   IID      ClassID, iid;
   LPIID    riid    = ( LPIID ) &IID_IDispatch;
   IUnknown * pUnk  = NULL;
   void     * pDisp = NULL; // IDispatch

   /* void *
    * used intentionally to inform compiler that there is no
    * strict-aliasing
    */

   bstrClassID = hb_oleAnsiToSysString( hb_parcx( 1 ) );

   if( hb_parcx( 1 )[ 0 ] == '{' )
   {
      s_nOleError = CLSIDFromString( bstrClassID, ( LPCLSID ) &ClassID );
   }
   else
   {
      s_nOleError = CLSIDFromProgID( bstrClassID, ( LPCLSID ) &ClassID );
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
         HB_MEMCPY( ( LPVOID ) &iid, hb_parcx( 2 ), sizeof( iid ) );
      }

      riid = &iid;
   }

   if( SUCCEEDED( s_nOleError ) )
   {
      s_nOleError = GetActiveObject( ( REFCLSID ) HBOLE_IID( ClassID ), NULL, &pUnk );

      if( SUCCEEDED( s_nOleError ) )
      {
         s_nOleError = HB_VTBL( pUnk )->QueryInterface( pUnk, ( REFIID ) riid, &pDisp );

         HB_VTBL( pUnk )->Release( pUnk );

         if( SUCCEEDED( s_nOleError ) )
         {
            //hb_retnl( ( LONG ) pDisp );
            hb_retns( ( HB_ISIZ ) pDisp );
         }
      }
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEADDREF ) // (hOleObject, szMethodName, uParams...)
{
   IDispatch * pDisp = ( IDispatch * ) hb_parns( 1 );

   //TraceLog( NULL, "TOleAuto: OleAddRef( %p )\n", pDisp );

   s_nOleError = HB_VTBL( pDisp )->AddRef( pDisp );

   hb_retnl( s_nOleError );
}

//---------------------------------------------------------------------------//
HB_FUNC( OLERELEASEOBJECT ) // (hOleObject, szMethodName, uParams...)
{
   IDispatch * pDisp = ( IDispatch * ) hb_parns( 1 );

   //TraceLog( NULL, "TOleAuto: OleReleaseObject( %p )\n", pDisp );

   s_nOleError = HB_VTBL( pDisp )->Release( pDisp );

   hb_retnl( s_nOleError );
}

//---------------------------------------------------------------------------//
static HRESULT OleSetProperty( IDispatch * pDisp, DISPID DispID, DISPPARAMS * pDispParams )
{
   pDispParams->rgdispidNamedArgs = &lPropPut;
   pDispParams->cNamedArgs        = 1;

   // 1 Based!!!
   if( ( ISBYREF( 1 ) ) || ISARRAY( 1 ) )
   {
      ReleaseExcepInfo();

      s_nOleError = HB_VTBL( pDisp )->Invoke( pDisp,
                                              DispID,
                                              ( REFIID ) HBOLE_IID( IID_NULL ),
                                              LOCALE_SYSTEM_DEFAULT,
                                              DISPATCH_PROPERTYPUTREF,
                                              pDispParams,
                                              NULL,    // No return value
                                              &excep,
                                              &uArgErr );
      if( SUCCEEDED( s_nOleError ) )
      {
         return s_nOleError;
      }
   }

   ReleaseExcepInfo();

   s_nOleError = HB_VTBL( pDisp )->Invoke( pDisp,
                                           DispID,
                                           ( REFIID ) HBOLE_IID( IID_NULL ),
                                           LOCALE_SYSTEM_DEFAULT,
                                           DISPATCH_PROPERTYPUT,
                                           pDispParams,
                                           NULL,    // No return value
                                           &excep,
                                           &uArgErr );
   pDispParams->rgdispidNamedArgs = NULL;
   pDispParams->cNamedArgs        = 0;

   return s_nOleError;
}

//---------------------------------------------------------------------------//
static HRESULT OleInvoke( IDispatch * pDisp, DISPID DispID, DISPPARAMS * pDispParams )
{
   ReleaseExcepInfo();

   s_nOleError = HB_VTBL( pDisp )->Invoke( pDisp,
                                           DispID,
                                           ( REFIID ) HBOLE_IID( IID_NULL ),
                                           LOCALE_SYSTEM_DEFAULT,
                                           DISPATCH_METHOD,
                                           pDispParams,
                                           &RetVal,
                                           &excep,
                                           &uArgErr );
   return s_nOleError;
}

//---------------------------------------------------------------------------//
static HRESULT OleGetProperty( IDispatch * pDisp, DISPID DispID, DISPPARAMS * pDispParams )
{
   ReleaseExcepInfo();

   s_nOleError = HB_VTBL( pDisp )->Invoke( pDisp,
                                           DispID,
                                           ( REFIID ) HBOLE_IID( IID_NULL ),
                                           LOCALE_SYSTEM_DEFAULT,
                                           DISPATCH_PROPERTYGET,
                                           pDispParams,
                                           &RetVal,
                                           &excep,
                                           &uArgErr );
   //TraceLog( NULL, "TOleAuto: OleGetValue: %p\n", s_nOleError );

   return s_nOleError;
}

//---------------------------------------------------------------------------//
static HRESULT OleGetValue( IDispatch * pDisp )
{
   VariantClear( &RetVal );

   // Try to apply the requested message to the DEFAULT Property of the object if any.
   if( SUCCEEDED( OleGetProperty( pDisp, DISPID_VALUE, &s_EmptyDispParams ) ) &&
       ( HB_V_VT( RetVal ) == VT_DISPATCH || HB_V_VT( RetVal ) == ( VT_DISPATCH | VT_BYREF ) ) )
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
   char     * sDescription;

   hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );

   if( s_nOleError == DISP_E_EXCEPTION )
   {
      // Intentional to avoid report of memory leak if fatal error.
      char * sTemp = hb_oleWideToAnsi( excep.bstrDescription );
      sDescription = ( char * ) malloc( strlen( sTemp ) + 1 );
      hb_xstrcpy( sDescription, sTemp, 0 );
      hb_xfree( sTemp );
   }
   else
   {
      sDescription = Ole2TxtError();
   }

   //TraceLog( NULL, "TOleAuto: Desc: '%s'\n", sDescription );

   pReturn = hb_errRT_SubstParams( hb_parcx( -1 ), EG_OLEEXECPTION, ( HB_ERRCODE ) s_nOleError,
                                   sDescription, hb_stackBaseItem()->item.asSymbol.value->szName );

   if( s_nOleError == DISP_E_EXCEPTION )
   {
      free( ( void * ) sDescription );
   }

   if( pReturn )
   {
      hb_itemRelease( hb_itemReturn( pReturn ) );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( TOLEAUTO_OLEVALUE )
{
   if( hb_pcount() == 0 )
   {
      IDispatch * pDisp;

      hb_vmPushSymbol( s_pSym_hObj->pSymbol );
      hb_vmPush( hb_stackSelfItem() );
      hb_vmSend( 0 );
      pDisp = ( IDispatch * ) hb_parns( -1 );

      VariantClear( &RetVal );

      OleGetProperty( pDisp, DISPID_VALUE, &s_EmptyDispParams );
      //TraceLog( NULL, "TOleAuto: GetDefault: %p\n", s_nOleError );

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
HB_FUNC( TOLEAUTO__OLEVALUE )
{
   if( hb_pcount() >= 1 )
   {
      IDispatch  * pDisp;
      DISPPARAMS DispParams;
      PHB_ITEM   * aPrgParams;

      hb_vmPushSymbol( s_pSym_hObj->pSymbol );
      hb_vmPush( hb_stackSelfItem() );
      hb_vmSend( 0 );
      pDisp = ( IDispatch * ) hb_parns( -1 );

      VariantClear( &RetVal );

      aPrgParams = GetParams( &DispParams, 0 );

      OleSetProperty( pDisp, DISPID_VALUE, &DispParams );
      //TraceLog( NULL, "TOleAuto: SetDefault: %p\n", s_nOleError );

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
HB_FUNC( TOLEAUTO_OLENEWENUMERATOR ) // (hOleObject, szMethodName, uParams...)
{
   IDispatch * pDisp;

   hb_vmPushSymbol( s_pSym_hObj->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch * ) hb_parns( -1 );

   VariantClear( &RetVal );

   if( SUCCEEDED( OleGetProperty( pDisp, DISPID_NEWENUM, &s_EmptyDispParams ) ) ||
       SUCCEEDED( OleInvoke( pDisp, DISPID_NEWENUM, &s_EmptyDispParams ) ) )
   {
      LPVOID pEnumVariant = NULL; /* IEnumVARIANT */

      if( HB_V_VT( RetVal ) == ( VT_UNKNOWN | VT_BYREF ) )
      {
         s_nOleError = ( *HB_V_UNION( RetVal, ppunkVal ) )->lpVtbl->QueryInterface( *HB_V_UNION( RetVal, ppunkVal ), ( REFIID ) HBOLE_IID( IID_IEnumVARIANT ), &pEnumVariant );
      }
      else if( HB_V_VT( RetVal ) == VT_UNKNOWN )
      {
         s_nOleError = HB_V_UNION( RetVal, punkVal )->lpVtbl->QueryInterface( HB_V_UNION( RetVal, punkVal ), ( REFIID ) HBOLE_IID( IID_IEnumVARIANT ), &pEnumVariant );
      }
      else if( HB_V_VT( RetVal ) == ( VT_DISPATCH | VT_BYREF ) )
      {
         s_nOleError = ( *HB_V_UNION( RetVal, ppdispVal ) )->lpVtbl->QueryInterface( *HB_V_UNION( RetVal, ppdispVal ), ( REFIID ) HBOLE_IID( IID_IEnumVARIANT ), &pEnumVariant );
      }
      else if( HB_V_VT( RetVal ) == VT_DISPATCH )
      {
         s_nOleError = HB_V_UNION( RetVal, pdispVal )->lpVtbl->QueryInterface( HB_V_UNION( RetVal, pdispVal ), ( REFIID ) HBOLE_IID( IID_IEnumVARIANT ), &pEnumVariant );
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
static HRESULT OleGetID( IDispatch * pDisp, const char * szName, DISPID * pDispID, BOOL * pbSetFirst )
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
      else*/if( szName[ 0 ] == '_' && szName[ 1 ] && hb_pcount() >= 1 )
   {
      bstrMessage = hb_oleAnsiToSysString( ( const char * ) szName + 1 );
      s_nOleError = HB_VTBL( pDisp )->GetIDsOfNames( pDisp, ( REFIID ) HBOLE_IID( IID_NULL ), ( wchar_t ** ) &bstrMessage, 1, LOCALE_SYSTEM_DEFAULT, pDispID );
      SysFreeString( bstrMessage );
      //TraceLog( NULL, "TOleAuto: 1. ID of: '%s' -> %i Result: %p\n", hb_stackBaseItem()->item.asSymbol.value->szName + 1, DispID, s_nOleError );

      if( SUCCEEDED( s_nOleError ) )
      {
         if( pbSetFirst )
            *pbSetFirst = TRUE;
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
      s_nOleError = HB_VTBL( pDisp )->GetIDsOfNames( pDisp, ( REFIID ) HBOLE_IID( IID_NULL ), ( wchar_t ** ) &bstrMessage, 1, 0, pDispID );
      SysFreeString( bstrMessage );
      //TraceLog( NULL, "TOleAuto: 2. ID of: '%s' -> %i Result: %p\n", szName, *pDispID, s_nOleError );
   }

   return s_nOleError;
}

//---------------------------------------------------------------------------//
HB_FUNC( TOLEAUTO_INVOKE )
{
   IDispatch  * pDisp;
   const char * szName = hb_parc( 1 );
   DISPID     DispID;
   DISPPARAMS DispParams;

   hb_vmPushSymbol( s_pSym_hObj->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch * ) hb_parns( -1 );

   if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
   {
      PHB_ITEM * aPrgParams = GetParams( &DispParams, 1 );

      if( SUCCEEDED( OleInvoke( pDisp, DispID, &DispParams ) ) )
      {
         RetValue();
      }

      FreeParams( &DispParams, aPrgParams );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( TOLEAUTO_SET )
{
   IDispatch  * pDisp;
   const char * szName = hb_parc( 1 );
   DISPID     DispID;
   DISPPARAMS DispParams;

   hb_vmPushSymbol( s_pSym_hObj->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch * ) hb_parns( -1 );

   if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
   {
      PHB_ITEM * aPrgParams = GetParams( &DispParams, 1 );

      if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
      {
         RetValue();
      }

      FreeParams( &DispParams, aPrgParams );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( TOLEAUTO_GET )
{
   IDispatch  * pDisp;
   const char * szName = hb_parc( 1 );
   DISPID     DispID;
   DISPPARAMS DispParams;

   hb_vmPushSymbol( s_pSym_hObj->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch * ) hb_parns( -1 );

   if( szName && SUCCEEDED( OleGetID( pDisp, szName, &DispID, NULL ) ) )
   {
      PHB_ITEM * aPrgParams = GetParams( &DispParams, 1 );

      if( SUCCEEDED( OleGetProperty( pDisp, DispID, &DispParams ) ) )
      {
         RetValue();
      }

      FreeParams( &DispParams, aPrgParams );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( TOLEAUTO_ONERROR )
{
   IDispatch  * pDisp;
   DISPID     DispID;
   DISPPARAMS DispParams;
   BOOL       bSetFirst = FALSE, bTryDefault = TRUE;
   PHB_ITEM   * aPrgParams = GetParams( &DispParams, 0 );

   //TraceLog( NULL, "TOleAuto: Class: '%s' Message: '%s', Params: %i Arg1: %i\n",
   //          hb_objGetClsName( hb_stackSelfItem() ), hb_stackBaseItem()->item.asSymbol.value->szName, hb_pcount(), hb_parinfo(1) );

   hb_vmPushSymbol( s_pSym_hObj->pSymbol );
   hb_vmPush( hb_stackSelfItem() );
   hb_vmSend( 0 );
   pDisp = ( IDispatch * ) hb_parns( -1 );

 OleGetID:

   if( SUCCEEDED( OleGetID( pDisp, hb_stackBaseItem()->item.asSymbol.value->szName, &DispID, &bSetFirst ) ) )
   {
      VariantClear( &RetVal );

      if( bSetFirst )
      {
         if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
         {
            hb_itemReturn( hb_stackItemFromBase( 1 ) );
         }

         //TraceLog( NULL, "TOleAuto: FIRST OleSetProperty %i\n", s_nOleError );
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

         //TraceLog( NULL, "TOleAuto: OleInvoke %i\n", s_nOleError );
      }

      if( FAILED( s_nOleError ) )
      {
         if( SUCCEEDED( OleGetProperty( pDisp, DispID, &DispParams ) ) )
         {
            RetValue();
         }

         //TraceLog( NULL, "TOleAuto: OleGetProperty(%i) %i\n", DispParams.cArgs, s_nOleError );
      }

      if( FAILED( s_nOleError ) && bSetFirst == FALSE && hb_pcount() >= 1 )
      {
         if( SUCCEEDED( OleSetProperty( pDisp, DispID, &DispParams ) ) )
         {
            hb_itemReturn( hb_stackItemFromBase( 1 ) );
         }

         //TraceLog( NULL, "TOleAuto: OleSetProperty %i\n", s_nOleError );
      }
   }

   if( SUCCEEDED( s_nOleError ) )
   {
      //TraceLog( NULL, "TOleAuto: Invoke Succeeded!\n" );
      if( HB_IS_OBJECT( hb_stackReturnItem() ) && hb_clsIsParent( hb_stackReturnItem()->item.asArray.value->uiClass, "TOLEAUTO" ) )
      {
         PHB_ITEM pReturn       = hb_itemNew( NULL );
         PHB_ITEM pOleClassName = hb_itemNew( NULL );
         char     * sOleClassName;
         HB_SIZE  iClassNameLen, iMsgNameLen;

         hb_itemForwardValue( pReturn, hb_stackReturnItem() );

         hb_vmPushSymbol( s_pSym_cClassName->pSymbol );
         hb_vmPush( hb_stackSelfItem() );
         hb_vmSend( 0 );

         iClassNameLen = hb_parclen( -1 );
         iMsgNameLen   = strlen( hb_stackBaseItem()->item.asSymbol.value->szName );

         sOleClassName = ( char * ) hb_xgrab( iClassNameLen + 1 + iMsgNameLen + 1 );

         hb_strncpy( sOleClassName, hb_parc( -1 ), iClassNameLen );
         sOleClassName[ iClassNameLen ] = ':';
         hb_xstrcpy( sOleClassName + iClassNameLen + 1, hb_stackBaseItem()->item.asSymbol.value->szName, 0 );

         //TraceLog( NULL, "TOleAuto: Class: '%s'\n", sOleClassName );

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
            //TraceLog( NULL, "TOleAuto: Try using DISPID_VALUE\n" );
            pDisp             = HB_V_UNION( OleVal, pdispVal );
            HB_V_VT( OleVal ) = VT_EMPTY;
            goto OleGetID;
         }
         else
         {
            pDisp = NULL;
         }
      }

      //TraceLog( NULL, "TOleAuto: Invoke Failed!\n" );
      OleThrowError();
   }

   FreeParams( &DispParams, aPrgParams );

   // We are responsible to release the Default Interface which we retrieved
   if( ( bTryDefault == FALSE ) && pDisp )
   {
      HB_VTBL( pDisp )->Release( pDisp );
   }
}

//---------------------------------------------------------------------------//
HB_FUNC( OLEPTR2INT )  // OlePtr2Int( <pPointer> ) -> <nInteger>
{
#if defined( __MINGW32__ ) && defined( HB_OS_WIN_64 )
   hb_retni( ( int ) ( HB_LONG ) hb_parptr( 1 ) );
#elif defined( _MSC_VER ) && defined( HB_OS_WIN_64 )
   hb_retni( ( int ) ( HB_LONG ) hb_parptr( 1 ) );
#else
   hb_retni( ( int ) hb_parptr( 1 ) );
#endif
}

#endif  //defined( HB_OS_WIN )
