/*
  (c) copyright xHarbour.com Inc. http://www.xHarbour.com
  Author: Ron Pinkas Ron@xHarbour.com

  This source file is an intellectual property of xHarbour.com Inc.
  You may NOT forward or share this file under any conditions!
*/

#pragma comment( lib, "ole32.lib" )
#pragma comment( lib, "oleAut32.lib" )
#pragma comment( lib, "AdvApi32.lib" )

#define CINTERFACE 1
#define NONAMELESSUNION

#define HB_NO_DEFAULT_API_MACROS
//#define HB_NO_DEFAULT_STACK_MACROS

#define _HB_API_INTERNAL_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbinit.h"
#include "hbvm.h"
#include "hbvmpub.h"
#include "hbdate.h"
#include "hbfast.h"
#include "hashapi.h"
#include "hbstack.h"
#include "hbapierr.h"

#include <windows.h>
#include <ole2.h>
#include <olectl.h>
#include <winreg.h>
#include <ocidl.h>

#include <tchar.h>
#include <stdio.h>

#include <assert.h>

#include "oleserver.h"

#define DLLEXP //__declspec( dllexport )

#ifdef AX
  #include "ActiveScriptTrace.h"

  #define DllGetClassObject OleServerGetClassObject
#else
  #ifdef _MSC_VER
     #ifndef NODLLMAIN
        #pragma comment( linker, "-export:DllRegisterServer=_DllRegisterServer@0,@1" )
        #pragma comment( linker, "-export:DllUnregisterServer=_DllUnregisterServer@0,@2" )
     #endif

     #pragma comment( linker, "-export:DllCanUnloadNow=_DllCanUnloadNow@0,@3" )
     #pragma comment( linker, "-export:DllGetClassObject=_DllGetClassObject@12,@4" )
  #endif

  extern BOOL IsExportedSymbol( PHB_DYNS pDyn );
  extern PHB_SYMB OleServerSymbols( USHORT *puiSymbols );
#endif

DLLEXP  STDAPI DllGetClassObject( REFCLSID ClassiD, REFIID riid, void **ppvObj );

IDispatch *OleWrap( PHB_ITEM pObject );

static HRESULT VTObjectToItem( PHB_ITEM pItem, VARIANT *pVariant, const char* sGUID );

HB_EXTERN_BEGIN
HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant );
HRESULT ItemToVariant( VARIANT *pVariant, PHB_ITEM pItem );
HB_EXTERN_END

static int s_iLock = 0;

static BOOL s_bOleWrap = FALSE;

static PHB_DYNS s_pSym_TOleAuto        = NULL;
static PHB_DYNS s_pSym_hObj            = NULL;
static PHB_DYNS s_pSym_New             = NULL;
//static PHB_DYNS s_pSym_InvokeEvent     = NULL;
static PHB_DYNS s_pSym_VTWrapper       = NULL;
static PHB_DYNS s_pSym_VTArrayWrapper  = NULL;
static PHB_DYNS s_pSym_vt              = NULL;
static PHB_DYNS s_pSym_Value           = NULL;

#if 0
   static PHB_DYNS s_pSym_QueryInterface  = NULL;
#endif

#ifndef AX
  #ifndef NODLLMAIN
     static PHB_SYMB s_pSym_DllMain        = NULL;
     static PHB_SYMB s_pSym_CreateInstance = NULL;
  #endif
#endif

HB_ITEM NilItem;

HINSTANCE g_hDll = 0;

typedef struct tagIClassFactoryObject
{
  struct IClassFactoryVtbl *lpVtbl;
  int m_iRef;
} IClassFactoryObject;

static struct IClassFactoryVtbl s_ClassFactoryVtbl =
 {
    ClassFactoryQueryInterface,
    ClassFactoryAddRef,
    ClassFactoryRelease,
    ClassFactoryCreateInstance,
    ClassFactoryLockServer
 };

static struct IDispatchVtbl s_DispatchVtbl =
{
    DispatchObjectQueryInterface,
    DispatchObjectAddRef,
    DispatchObjectRelease,
    DispatchObjectGetTypeInfoCount,
    DispatchObjectGetTypeInfo,
    DispatchObjectGetIDsOfNames,
    DispatchObjectInvoke,
};

#if 0
static void cdecl __PushPointer( void *Push );
void (__stdcall *PushPointer)(void *) = (void (__stdcall *)(void *)) __PushPointer;

static void cdecl __PushLongLong( LONGLONG Push );
void (__stdcall *PushLongLong)(LONGLONG) = (void (__stdcall *)(LONGLONG)) __PushLongLong;

static void cdecl __PushLong( long Push );
void (__stdcall *PushLong)(long) = (void (__stdcall *)(long)) __PushLong;

static void cdecl __PushInt( int Push );
void (__stdcall *PushInt)(int) = (void (__stdcall *)(int)) __PushInt;

static void cdecl __PushChar( char Push );
void (__stdcall *PushChar)(char) = (void (__stdcall *)(char)) __PushChar;

static void cdecl __PushDouble( double Push );
void (__stdcall *PushDouble)(double) = (void (__stdcall *)(double)) __PushDouble;

static void cdecl __PushFloat( float Push );
void (__stdcall *PushFloat)(float) = (void (__stdcall *)(float)) __PushFloat;

static void cdecl __PushCY( CY Push );
void (__stdcall *PushCY)(CY) = (void (__stdcall *)(CY)) __PushCY;

static void cdecl __PushDec( DECIMAL Push );
void (__stdcall *PushDec)(DECIMAL) = (void (__stdcall *)(DECIMAL)) __PushDec;
#endif

static int s_iObjects = 0;

static HRESULT  s_nOleError;

// Dummy.
const char *g_RegTable[6][3];

#ifdef _DEBUG
    #define OLESERVER_DEBUG_BUFFER_LEN 8192
    static char s_sBuffer[ OLESERVER_DEBUG_BUFFER_LEN ];

    void OutputDebugValues( const char *sFormat, ... )
    {
        va_list ap;

        s_sBuffer[0] = '\0';

        va_start( ap, sFormat );

        #ifdef __XCC__
           vsnprintf( s_sBuffer, OLESERVER_DEBUG_BUFFER_LEN, sFormat, ap );
        #else
           _vsnprintf( s_sBuffer, OLESERVER_DEBUG_BUFFER_LEN, sFormat, ap );
        #endif

        va_end( ap );

        OutputDebugString( s_sBuffer );
    }
#else
   #ifndef OutputDebugValues
      #define OutputDebugValues {}(void)
   #endif
#endif

static HRESULT STDMETHODCALLTYPE DispatchObjectQueryInterface( IDispatch *This, REFIID riid, void **ppvObj )
{
   #if 0
     BSTR bstrIID;

     StringFromIID( riid, &bstrIID );
     TraceLog( NULL, "riid: %ls\n", bstrIID );
     CoTaskMemFree( bstrIID );
     bstrIID = NULL;

     StringFromIID( &(( (IDispatchObject *) This )->m_InterfaceIID), &bstrIID );
     TraceLog( NULL, "m_InterfaceIID: %ls\n", bstrIID );
     CoTaskMemFree( bstrIID );
     bstrIID = NULL;
   #endif

   OutputDebugValues( "DispatchObject->QueryInterface( %p, %p, %p)\n", This, riid, ppvObj );

   #if 0
      OutputDebugValues( "riid %i %i %i %.*s\n", riid->Data1, riid->Data2, riid->Data3, 8, riid->Data4 );
      OutputDebugValues( "IID_Unknown   %i %i %i %.*s\n", IID_IUnknown.Data1, IID_IUnknown.Data2, IID_IUnknown.Data3, 8, IID_IUnknown.Data4 );
      OutputDebugValues( "IID_IDispatch %i %i %i %.*s\n", IID_IDispatch.Data1, IID_IDispatch.Data2, IID_IDispatch.Data3, 8, IID_IDispatch.Data4 );
   #endif

   if( ppvObj == NULL )
   {
      OutputDebugString( "***Unknown Interface->E_INVALIDARG\n" );
      return E_INVALIDARG;
   }

   // Safety.
   *ppvObj = NULL;

   if( IsEqualIID( riid, (&IID_IUnknown) ) == FALSE && IsEqualIID( riid, (&IID_IDispatch) ) == FALSE && IsEqualIID( riid, &(( (IDispatchObject *) This )->m_InterfaceIID) ) == FALSE )
   {
      BSTR bstrIID = NULL;

      StringFromCLSID( riid, &bstrIID );

      #ifndef _DEBUG
         OutputDebugString( "***Unknown Interface\n" );
      #else
         OutputDebugValues( "***Unknown Interface >>>%ls<<<\n", bstrIID );
      #endif

      #if 0
        pIID = hb_itemNew( NULL );
        hb_itemPutCPtr( pIID, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );

        CoTaskMemFree( bstrIID );
        bstrIID = NULL;

        if( ( (IDispatchObject *) This )->m_pxObject && HB_IS_OBJECT( ( (IDispatchObject *) This )->m_pxObject ) && hb_objHasMsg( ( (IDispatchObject *) This )->m_pxObject, "QUERYINTERFACE" ) )
        {
           OutputDebugString( "***Defering to PRG Server\n" );

           hb_vmPushSymbol( s_pSym_QueryInterface->pSymbol );
           hb_vmPush( ( (IDispatchObject *) This )->m_pxObject );
           hb_itemPushForward( pIID );
           hb_vmSend(1);

           if( HB_IS_OBJECT( hb_stackReturnItem() ) )
           {
              if( hb_stackReturnItem()->item.asArray.value == ( (IDispatchObject *) This )->m_pxObject->item.asArray.value )
              {
                 OutputDebugString( "*** PRG server returned Self\n" );

                 *ppvObj = (void *) This;
                 ((LPUNKNOWN) *ppvObj)->lpVtbl->AddRef( (LPUNKNOWN) *ppvObj );
              }
              else
              {
                 OutputDebugValues( "*** PRG server returned %p\n", hb_stackReturnItem()->item.asArray.value );

                 // Allready AddRef()ed
                 *ppvObj = (void *) OleWrap( hb_stackReturnItem() );
              }
           }
        }
        else if( IsExportedSymbol( s_pSym_QueryInterface ) )
        {
           OutputDebugString( "***Defering to PRG QueryInterface()\n" );

           hb_vmPushSymbol( s_pSym_QueryInterface->pSymbol );
           hb_vmPushNil();
           hb_itemPushForward( pIID );
           hb_vmDo(1);

           if( HB_IS_OBJECT( hb_stackReturnItem() ) )
           {
              OutputDebugValues( "*** PRG returned %p\n", hb_stackReturnItem()->item.asArray.value );

              // Allready AddRef()ed
              *ppvObj = (void *) OleWrap( hb_stackReturnItem() );
           }
        }
        else
        {
           OutputDebugString( "***Still unresoved QueryInterface()\n" );
        }

        // Safety!
        hb_vmRequestReset();
        hb_itemClear( hb_stackReturnItem() );

        hb_itemRelease( pIID );
        pIID = NULL;
      #endif

      if( *ppvObj )
      {
         OutputDebugValues( "*** QueryInterface() returns %p\n", *ppvObj );
         return S_OK;
      }

      OutputDebugString( "***E_NOINTERFACE!!!\n" );
      return E_NOINTERFACE;
   }

   *ppvObj = (void *) This;
   ((LPUNKNOWN) *ppvObj)->lpVtbl->AddRef( (LPUNKNOWN) *ppvObj );

   return S_OK;
}

static ULONG STDMETHODCALLTYPE DispatchObjectAddRef( IDispatch *This )
{
   OutputDebugValues( "DispatchObject->AddRef(%i)\n", ( (IDispatchObject *) This )->m_iRef );

   return ++( ( (IDispatchObject *) This )->m_iRef );
}

static void DispatchObjectFree( IDispatch *This )
{
   OutputDebugValues( "~~~DispatchObject->Free\n" );

   if( ( (IDispatchObject *) This )->m_pxObject )
   {
      hb_itemRelease( ( (IDispatchObject *) This )->m_pxObject );
      ( (IDispatchObject *) This )->m_pxObject = NULL;
   }

   if( ( (IDispatchObject *) This )->m_pxHashMembers )
   {
      hb_itemRelease( ( (IDispatchObject *) This )->m_pxHashMembers );
      ( (IDispatchObject *) This )->m_pxHashMembers = NULL;
   }

   if( ( (IDispatchObject *) This )->m_pxHashHandlers )
   {
      hb_itemRelease( ( (IDispatchObject *) This )->m_pxHashHandlers );
      ( (IDispatchObject *) This )->m_pxHashHandlers = NULL;
   }

   free( (void *) This );

   s_iObjects--;
}

static ULONG STDMETHODCALLTYPE DispatchObjectRelease( IDispatch *This )
{
    OutputDebugValues( "DispatchObjectRelease->Release(%i)\n", ( (IDispatchObject *) This )->m_iRef );

    if( --( ( ( (IDispatchObject*) This )->m_iRef ) ) == 0 )
    {
        DispatchObjectFree( This );
        return 0;
    }

    return ( (IDispatchObject *) This )->m_iRef;
}

static HRESULT STDMETHODCALLTYPE DispatchObjectGetTypeInfoCount( IDispatch *This, UINT *iTInfo )
{
    OutputDebugValues( "DispatchObject->GetTypeInfo( %p )\n", This );

    if( iTInfo == NULL )
    {
        return E_INVALIDARG;
    }

    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE DispatchObjectGetTypeInfo( IDispatch *This, UINT iTInfo, LCID lcid, ITypeInfo **ppTInfo )
{
    OutputDebugValues( "DispatchObject->GetTypeInfo( %p )\n", This );

    if( ppTInfo == NULL )
    {
        return E_INVALIDARG;
    }

    *ppTInfo = NULL;

    return E_NOTIMPL;
}

static HRESULT STDMETHODCALLTYPE DispatchObjectGetIDsOfNames( IDispatch *This, REFIID riid, LPOLESTR *pNames, UINT iNames, LCID lcid, DISPID *pDispID )
{
   HRESULT hr = S_OK;
   UINT i;
   HB_ITEM Name, DynSym;

   OutputDebugValues( "DispatchObject->GetIDsOfNames(%p)\n", This );

   if( ! IsEqualIID( riid, &IID_NULL ) )
   {
      return E_INVALIDARG;
   }

   Name.type   = HB_IT_NIL;
   DynSym.type = HB_IT_NIL;

   for( i = 0; i < iNames; i++ )
   {
      LPCOLESTR curName = pNames[i];
      char sName[ HB_SYMBOL_NAME_LEN ];
      PHB_DYNS pDyn;
      int iLen;
      ULONG ulPos;

      iLen = WideCharToMultiByte( CP_ACP, WC_COMPOSITECHECK, curName, -1, sName, HB_SYMBOL_NAME_LEN, NULL, NULL);

      OutputDebugValues( "****** Searching: '%s'\n", sName );

      hb_itemPutCL( &Name, sName, iLen - 1 );

      // Odd Event trigerring calls GetIDsOfNames() instead of using event ID directly!
      if( ( (IDispatchObject *) This )->m_pxHashHandlers )
      {
         // For such case we stored hMembers hash generated in ::Connectevents()
         if( hb_hashScan( ( (IDispatchObject *) This )->m_pxHashMembers, &Name, &ulPos ) )
         {
            // The KEY is the Event:Name and the value is the Event:MemberID
            PHB_ITEM pID = hb_hashGetValueAt( ( (IDispatchObject *) This )->m_pxHashMembers, ulPos );

            if( HB_IS_NUMBER( pID ) )
            {
               pDispID[i] = (ULONG) hb_itemGetNL( pID );
               continue;
            }
            else
            {
               OutputDebugString( "*** OOPS! NON Numeric Event:MemberID\n" );
            }
         }
         else
         {
            OutputDebugString( "*** OOPS! Missing memeber in hMembers\n" );
         }
      }

      if( hb_hashScan( ( (IDispatchObject *) This )->m_pxHashMembers, &Name, &ulPos ) )
      {
         hb_itemClear( &Name );

         pDispID[i] = ulPos;
         OutputDebugValues( "Pre-Existing.\n" );
         continue;
      }

      if( ( (IDispatchObject *) This )->m_pxObject && HB_IS_OBJECT( ( (IDispatchObject *) This )->m_pxObject ) )
      {
         PHB_ITEM pxObject = ( (IDispatchObject *) This )->m_pxObject;

         OutputDebugValues( "xHarbour Object %p\n", pxObject->item.asArray.value );

         if( hb_objHasMsg( pxObject, sName ) )
         {
            OutputDebugValues( "Found Message %s\n", sName );

            pDyn = hb_dynsymGet( sName );
            hb_itemPutPtr( &DynSym, (void *) pDyn );
            hb_hashAdd( ( (IDispatchObject *) This )->m_pxHashMembers, ++( ( (IDispatchObject *) This )->m_uiMembers ), &Name, &DynSym );

            //Give the DISPID back to the caller
            pDispID[i] = ( (IDispatchObject *) This )->m_uiMembers;

            OutputDebugValues( "OBJECT Method.\n" );
            hb_itemClear( &Name );
         }
         else
         {
            hr = DISP_E_MEMBERNOTFOUND;
         }

         continue;
      }

      pDyn = hb_dynsymFindName( sName );

      if( pDyn )
      {
         if( pDyn && pDyn->hMemvar )
         {
            hb_itemPutPtr( &DynSym, (void *) pDyn );
            hb_hashAdd( ( (IDispatchObject *) This )->m_pxHashMembers, ++( ( (IDispatchObject *) This )->m_uiMembers ), &Name, &DynSym );

            //Give the DISPID back to the caller
            pDispID[i] = ( (IDispatchObject *) This )->m_uiMembers;

            OutputDebugValues( "MEMVAR.\n" );
            hb_itemClear( &Name );
            continue;
         }
         else if( pDyn->pSymbol->value.pFunPtr )
         {
         #if ( ! defined( EXPORT_RTL ) ) && ! defined( AX )
            if( IsExportedSymbol( pDyn ) )
         #endif
            {
               hb_itemPutPtr( &DynSym, (void *) pDyn );
               hb_hashAdd( ( (IDispatchObject *) This )->m_pxHashMembers, ++( ( (IDispatchObject *) This )->m_uiMembers ), &Name, &DynSym );

               //Give the DISPID back to the caller
               pDispID[i] = ( (IDispatchObject *) This )->m_uiMembers;

               OutputDebugValues( "FUNCTION %s Given ID# %i\n", sName, ( (IDispatchObject *) This )->m_uiMembers );
               hb_itemClear( &Name );
               continue;
            }
         #if( ! defined( EXPORT_RTL ) ) && ! defined( AX )
            else
            {
               OutputDebugValues( "NON Module Function! %p\n", pDyn->pSymbol );
            }
         #endif
         }
      }

      hb_itemClear( &Name );

      pDispID[i] = 0;
      OutputDebugValues( "NOT-FOUND!(%s)\n", sName );

      //We couldn't find the name, so set an error code.
      hr = DISP_E_UNKNOWNNAME;
   }

   return hr;
}

static PHB_ITEM PushDispParams( DISPPARAMS *pDispParams )
{
   PHB_ITEM paParams = hb_itemNew( NULL );
   int i;

   hb_arrayNew( paParams, pDispParams->cArgs );

   for( i = pDispParams->cArgs - 1; i >= 0; i-- )
   {
      //VTObjectToItem( paParams->item.asArray.value->pItems + i, &pDispParams->rgvarg[i], "" );
      VariantToItem( paParams->item.asArray.value->pItems + i, &pDispParams->rgvarg[i] );

      hb_stackPush();

      if( pDispParams->rgvarg[i].n1.n2.vt & VT_BYREF )
      {
         hb_arrayGetByRef( paParams, i + 1, hb_stackItemFromTop( -1 ) );
      }
      else
      {
         hb_arrayGet( paParams, i + 1, hb_stackItemFromTop( -1 ) );
      }
   }

   return paParams;
}

static HRESULT ItemToByRefVariant( VARIANT *pVariant, PHB_ITEM pItem )
{
   VARIANT mVariant;
   VARTYPE vt;
   SAFEARRAYBOUND rgsabound;
   void *pSource;// = NULL;
   unsigned long i;
   char *sString;
   IDispatch *pDisp;// = NULL;

   if( pVariant == NULL )
   {
      OutputDebugString( "****************** NULL Variant ************************\n" );
      return E_INVALIDARG;
   }

   if( ( pVariant->n1.n2.vt & VT_BYREF ) == 0 )
   {
      OutputDebugString( "****************** NON VT_BYREF Variant ****************\n" );
      return E_INVALIDARG;
   }

   switch( pVariant->n1.n2.vt )
   {
      case VT_BYREF | VT_BSTR:
         SysFreeString( *pVariant->n1.n2.n3.pbstrVal );
         *pVariant->n1.n2.n3.pbstrVal = hb_oleAnsiToSysString( hb_itemGetCPtr( pItem ) );
         break;

      case VT_BYREF | VT_BOOL:
         *pVariant->n1.n2.n3.pboolVal = hb_itemGetL( pItem ) ? VARIANT_TRUE : VARIANT_FALSE;
         break;

      case VT_BYREF | VT_UI2:
         *pVariant->n1.n2.n3.puiVal = (USHORT) hb_itemGetNI( pItem );
         break;

      case VT_BYREF | VT_I2:
         *pVariant->n1.n2.n3.piVal = (SHORT) hb_itemGetNI( pItem );
         break;

      case VT_BYREF | VT_UI4:
         *pVariant->n1.n2.n3.pulVal = (ULONG) hb_itemGetNL( pItem );
         break;

      case VT_BYREF | VT_I4:
         *pVariant->n1.n2.n3.plVal = (LONG) hb_itemGetNL( pItem );
         break;

      case VT_BYREF | VT_UI8:
         *pVariant->n1.n2.n3.pullVal = (ULONGLONG) hb_itemGetNLL( pItem );;
         break;

      case VT_BYREF | VT_I8:
         *pVariant->n1.n2.n3.pllVal = (LONGLONG) hb_itemGetNLL( pItem );;
         break;

      case VT_BYREF | VT_R8:
         *pVariant->n1.n2.n3.pdblVal =  (DOUBLE) hb_itemGetND( pItem );
         break;

      case VT_BYREF | VT_DATE:
         *pVariant->n1.n2.n3.pdblVal =  hb_itemGetDTD( pItem ) - (double) 2415019;
         break;

      case VT_BYREF | VT_VARIANT:
         ItemToVariant( pVariant->n1.n2.n3.pvarVal, pItem );
         break;

      case VT_BYREF | VT_DISPATCH:

         if( pVariant->n1.n2.n3.ppdispVal )
         {
            if( HB_IS_ARRAY( pItem ) && hb_clsIsParent( pItem->item.asArray.value->uiClass , "TOLEAUTO" ) )
            {
               hb_vmPushSymbol( s_pSym_hObj->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               // AddRef() logic below!
               pDisp = (IDispatch *) hb_parnl( -1 );

               // Safety!
               hb_vmRequestReset();
               hb_itemClear( hb_stackReturnItem() ); // NOTE was COMMENTED!!!
            }
            else
            {
               pDisp = NULL;
            }

            if( *pVariant->n1.n2.n3.ppdispVal != pDisp )
            {
               if( pDisp )
               {
                  pDisp->lpVtbl->AddRef( pDisp );
               }

               if( *pVariant->n1.n2.n3.ppdispVal )
               {
                  ( *pVariant->n1.n2.n3.ppdispVal )->lpVtbl->Release( ( *pVariant->n1.n2.n3.ppdispVal ) );
               }

               *pVariant->n1.n2.n3.ppdispVal = pDisp;
            }
         }
         break;

      default:
         if( (VARTYPE) ( pVariant->n1.n2.vt & ( VT_BYREF | VT_ARRAY ) ) == (VARTYPE) ( VT_BYREF | VT_ARRAY ) )
         {
            if( hb_clsIsParent( pItem->item.asArray.value->uiClass , "VTARRAYWRAPPER" ) )
            {
               // vt := oVTArray:vt
               hb_vmPushSymbol( s_pSym_vt->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               vt = (VARTYPE) hb_parnl(-1);

               // Safety!
               hb_vmRequestReset();
               hb_itemClear( hb_stackReturnItem() );

               if( ( pVariant->n1.n2.vt & vt ) != vt )
               {
                 TraceLog( NULL, "VT_ARRAY vt mismatch type %p/%p in: %s(%i)!\n", vt, pVariant->n1.n2.vt, __FILE__, __LINE__ );
                 return E_FAIL;
               }

               // aArray := oVTArray:Value
               hb_vmPushSymbol( s_pSym_Value->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               // Safety!
               hb_vmRequestReset();
               //hb_itemClear( hb_stackReturnItem() ); Do NOT uncomment, see pItem assignment below!!!

               // Intentionally not using hb_itemCopy() or hb_itemForwardValue()
               pItem = hb_stackReturnItem();

               if( ( vt == VT_I1 || vt == VT_UI1 ) && HB_IS_STRING( pItem ) )
               {
                  sString = hb_itemGetCPtr( pItem );

                  rgsabound.cElements = hb_itemGetCLen( pItem );
                  rgsabound.lLbound = 0;

                  //pVariant->n1.n2.vt = ( VT_ARRAY | VT_BYREF | vt );
                  *pVariant->n1.n2.n3.pparray = SafeArrayCreate( vt, 1, &rgsabound );

                  for( i = 0; i < rgsabound.cElements; i++ )
                  {
                     SafeArrayPutElement( *pVariant->n1.n2.n3.pparray, (LONG *) &i, &( sString[i]) );
                  }

                  break;
               }

               VariantInit( &mVariant );
               pSource = &mVariant.n1.n2.n3.cVal;

               goto ItemToByRefVariant_ProcessByRefArray;
            }
            else
            {
               unsigned long  i;

               vt = VT_VARIANT;
               VariantInit( &mVariant );
               pSource = &mVariant;

             ItemToByRefVariant_ProcessByRefArray:

               if( ! HB_IS_ARRAY( pItem ) )
               {
                  TraceLog( NULL, "Unexpected type (wanted HB_IT_ARRAY) %p in: %s(%i)!\n", pItem->type, __FILE__, __LINE__ );
               }

               rgsabound.cElements = hb_arrayLen( pItem );
               rgsabound.lLbound = 0;

               //TraceLog( NULL, "Array len: %i type: %i in: %s(%i) \n", rgsabound.cElements, vt, __FILE__, __LINE__ );

               //pVariant->n1.n2.vt = ( VT_ARRAY | VT_BYREF | vt );
               *pVariant->n1.n2.n3.pparray = SafeArrayCreate( vt, 1, &rgsabound );

               for( i = 0; i < rgsabound.cElements; i++ )
               {
                  ItemToVariant( &mVariant, hb_arrayGetItemPtr( pItem, i + 1 ) );
                  SafeArrayPutElement( *pVariant->n1.n2.n3.pparray, (LONG *) &i, pSource );
                  VariantClear( &mVariant );
               }
            }
            break;
         }
         else
         {
            TraceLog( NULL, "Unexpected type %p in: %s(%i)\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
            return E_FAIL;
         }
   }

   return S_OK;
}

static void ProjectParamsToDispParams( DISPPARAMS *pDispParams, PHB_ITEM paParams )
{
   int i;

   for( i = pDispParams->cArgs - 1; i >= 0; i-- )
   {
      if( pDispParams->rgvarg[i].n1.n2.vt & VT_BYREF )
      {
         //TraceLog( NULL, "Project type: %p\n", pDispParams->rgvarg[i].n1.n2.vt );
         ItemToByRefVariant( &pDispParams->rgvarg[i], paParams->item.asArray.value->pItems + i );
      }
   }

   hb_itemRelease( paParams );
}

static HRESULT STDMETHODCALLTYPE DispatchObjectInvoke( IDispatch *This, DISPID DispID, REFIID riid, LCID lcid, WORD wFlags, DISPPARAMS *pDispParams, VARIANT *pResult, EXCEPINFO *pExcepInfo, UINT *puArgErr )
{
   HB_ITEM Member;
   PHB_ITEM pxObject;
   BOOL bMethod;
   HRESULT hr = S_OK;

   OutputDebugValues( "%p:DispatchObject->Invoke(%i) with %i Arguments\n", This, DispID, pDispParams ? pDispParams->cArgs : 0 );

   //DebugBreak();

   if( ! IsEqualIID( riid, &IID_NULL ) )
   {
      OutputDebugValues( "*** E_INVALIDARG\n", DispID );
      return E_INVALIDARG;
   }

   Member.type = HB_IT_NIL;

   // OleWrapFunction() support!
   if( DispID == 0 )
   {
      DispID = 1;
   }

   if( ( (IDispatchObject *) This )->m_pxHashHandlers )
   {
      BOOL bGeneric = FALSE;
      ULONG ulPos = 0;
      char *szEvent;
      #ifdef DYNSYM_SORT_BUG
         char *szSymbol;
      #else
          PHB_DYNS pClonedDynSym;
      #endif

      HB_STACK_STATE sStackState;
      PHB_ITEM pHandlerArray, pHandler;
      PHB_ITEM paParams = NULL;
      PHB_SYMB pSym;
      HB_ITEM_NEW( xDispID );

      OutputDebugValues( "Events Interface!\n" );

      hb_itemPutNL( &xDispID, (LONG) DispID );

      if( hb_hashScan( ( (IDispatchObject *) This )->m_pxHashHandlers, &xDispID, &ulPos ) )
      {
        GetHandler :

         pHandlerArray = hb_hashGetValueAt( ( (IDispatchObject *) This )->m_pxHashHandlers, ulPos );

         if( bGeneric )
         {
            OutputDebugValues( "Process GENERIC Handler\n" );
         }
         else
         {
            szEvent = hb_arrayGetCPtr( pHandlerArray, 1 );
            OutputDebugValues( "Process Event: %s\n", szEvent );
         }

         pHandler = hb_arrayGetItemPtr( pHandlerArray, 2 );

         // Safety!
         hb_vmRequestReset();
         hb_itemClear( hb_stackReturnItem() );

         switch( pHandler->type )
         {
            case HB_IT_NIL :
               OutputDebugValues( "NO Handler...\n" );
               break;

            case HB_IT_BLOCK :
               OutputDebugValues( "Block Handler...\n" );

               hb_vmPushEvalSym();
               hb_vmPush( pHandler );

               paParams = PushDispParams( pDispParams );

               hb_vmSend( pDispParams->cArgs );
               OutputDebugValues( "Done!\n" );

               ProjectParamsToDispParams( pDispParams, paParams );

               break;

            case HB_IT_POINTER :
               OutputDebugValues( "Function Handler...\n" );

               //TraceLog( NULL, "Args: %i\n", pDispParams->cArgs );

               pSym = (PHB_SYMB) hb_itemGetPtr( pHandler );

               #ifdef DYNSYM_SORT_BUG
                  // Save!
                  szSymbol = pSym->szName;

                  // Switch the symbol.
                  pSym->szName = szEvent;
               #else
                  pClonedDynSym = hb_dynsymGetCase( szEvent );

                  pClonedDynSym->pSymbol->scope.value   = pSym->scope.value;
                  pClonedDynSym->pSymbol->value.pFunPtr = pSym->value.pFunPtr;
                  pClonedDynSym->pModuleSymbols = pSym->pDynSym->pModuleSymbols;
               #endif

               #ifdef DYNSYM_SORT_BUG
                  hb_vmPushSymbol( pSym );
               #else
                  hb_vmPushSymbol( pClonedDynSym->pSymbol );
               #endif

               hb_vmPush( ( (IDispatchObject *) This )->m_pxObject );

               paParams = PushDispParams( pDispParams );

               //hb_vmDo( pDispParams->cArgs );
               hb_stackNewFrame( &sStackState, pDispParams->cArgs );

               pSym->value.pFunPtr();
               OutputDebugValues( "Done!\n" );

               hb_stackOldFrame( &sStackState );

               #ifdef DYNSYM_SORT_BUG
                  // Restore
                  pSym->szName = szSymbol;
               #else
                  pClonedDynSym->pSymbol->scope.value   = 0;
                  pClonedDynSym->pSymbol->value.pFunPtr = NULL;
                  pClonedDynSym->pModuleSymbols = NULL;
               #endif

               ProjectParamsToDispParams( pDispParams, paParams );

               if( pResult )
               {
                  ItemToVariant( pResult, hb_stackReturnItem() );
               }

               hb_itemClear( hb_stackReturnItem() ); //NOTE: Was commented!!!

               break;

            default :
               TraceLog( NULL, "Unexpected case in: %s(%i)\n", __FILE__, __LINE__ );
         }

         if( ( hb_vmRequestQuery() /* == HB_QUIT_REQUESTED || hb_vmRequestQuery() == HB_BREAK_REQUESTED */ ) )
         {
             OutputDebugValues( "*** QUIT or BREAK requested!!!\n" );

             // Safety!
             hb_vmRequestReset();

             //return DISP_E_MEMBERNOTFOUND;
             return S_OK;
         }

         // Safety!
         //hb_vmRequestReset();
      }
      else
      {
         OutputDebugValues( "Missing Handler %i in: %s(%i)\n", DispID, __FILE__, __LINE__ );
         return DISP_E_MEMBERNOTFOUND;
      }

      if( bGeneric )
      {
         OutputDebugValues( "End Generic\n" );
      }
      else
      {
         if( ( (IDispatchObject *) This )->m_ulGenericPos )
         {
            bGeneric = TRUE;
            ulPos = ( (IDispatchObject *) This )->m_ulGenericPos;
            goto GetHandler;
         }
         else
         {
            OutputDebugValues( "No GENERIC Handler\n" );
         }
      }

      OutputDebugValues( "End Event\n" );
      return S_OK;
   }

   OutputDebugValues( "Normal Interface!\n" );

   if( hb_hashGet( ( (IDispatchObject *) This )->m_pxHashMembers, DispID, &Member ) )
   {
      PHB_DYNS pDynSym = (PHB_DYNS) hb_itemGetPtr( &Member );

      //If we're getting a property, it must BE a property and pResult cannot be NULL
      if( wFlags & DISPATCH_PROPERTYGET )
      {
         OutputDebugValues( "PROPERTYGET\n" );

         if( pDynSym->hMemvar )
         {
            if( pResult != NULL )
            {
                PHB_ITEM pValue = hb_itemNew( NULL );
                HB_ITEM_PTR pMemvar = hb_memvarGetValueByHandle( pDynSym->hMemvar );

                OutputDebugValues( "GET Memvar\n" );

                if( HB_IS_BYREF( pMemvar ) )
                {
                   hb_itemCopy( pValue, hb_itemUnRef( pMemvar ) );
                }
                else
                {
                   hb_itemCopy( pValue, pMemvar );
                }

                ItemToVariant( pResult, pValue );

                hb_itemRelease( pValue );
                pValue = NULL;
            }
            else
            {
               return DISP_E_BADVARTYPE;
            }
         }
         else
         {
            goto TryMethod;
         }
      }
      else if( wFlags & DISPATCH_PROPERTYPUT )
      {
         OutputDebugValues( "PROPERTYPUT\n" );

         if( pDynSym->hMemvar )
         {
            if( pDispParams->cArgs == 1 )
            {
                PHB_ITEM pValue = hb_itemNew( NULL );
                HB_ITEM_PTR pMemvar = hb_memvarGetValueByHandle( pDynSym->hMemvar );

                VTObjectToItem( pValue, &pDispParams->rgvarg[0], "" );

                if( HB_IS_BYREF( pMemvar ) )
                {
                   hb_itemForwardValue( hb_itemUnRef( pMemvar ), pValue );
                }
                else
                {
                   hb_itemForwardValue( pMemvar, pValue );
                }

                hb_itemRelease( pValue );
                pValue = NULL;
            }
            else
            {
               return DISP_E_BADPARAMCOUNT;
            }
         }
         else
         {
            //return E_INVALIDARG;
            goto TryMethod;
         }
      }
      else if( wFlags & DISPATCH_METHOD )
      {
         TryMethod:

         if( ( (IDispatchObject *) This )->m_pxObject && HB_IS_OBJECT( ( (IDispatchObject *) This )->m_pxObject ) )
         {
            pxObject = ( (IDispatchObject *) This )->m_pxObject;

            bMethod = TRUE;

            OutputDebugValues( "Class: %i Items: %p, Length %i\n", pxObject->item.asArray.value->uiClass, pxObject->item.asArray.value->pItems, pxObject->item.asArray.value->ulLen );
         }
         else
         {
            bMethod = FALSE;
            pxObject = &NilItem;
         }

         if( pDynSym->pSymbol->value.pFunPtr || bMethod )
         {
            PHB_ITEM paParams;

            OutputDebugValues( "METHOD: %s Params: %i\n", pDynSym->pSymbol->szName, pDispParams->cArgs );

            hb_vmPushSymbol( pDynSym->pSymbol );

            hb_vmPush( pxObject );

            paParams = PushDispParams( pDispParams );

            OutputDebugValues( "%s -> DO it...\n", pDynSym->pSymbol->szName );

            if( bMethod )
            {
               hb_vmSend( pDispParams->cArgs );
            }
            else
            {
               hb_vmDo( pDispParams->cArgs );
            }

            ProjectParamsToDispParams( pDispParams, paParams);

            if( ( hb_vmRequestQuery() == HB_QUIT_REQUESTED || hb_vmRequestQuery() == HB_BREAK_REQUESTED ) )
            {
                OutputDebugValues( "*** QUIT or BREAK requested!!! Request: %i, Type: %i\n", hb_vmRequestQuery(), hb_stackReturnItem()->type );

                //RaiseException( hb_stackReturnItem() );
                if( ErrorToException( hb_stackReturnItem(), pExcepInfo, 0, pDynSym->pSymbol->szName ) == S_OK )
                {
                    // Safety!
                    hb_vmRequestReset();
                    hb_itemClear( hb_stackReturnItem() ); // NOTE was COMMENTED!!!
                    return DISP_E_EXCEPTION;
                }

                // Safety!
                hb_vmRequestReset();
                hb_itemClear( hb_stackReturnItem() ); // NOTE was COMMENTED!!!
                return E_FAIL;
            }

            if( HB_IS_STRING( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asString.allocated )
            {
               OutputDebugValues( "*********** Return: %s Owners: %i\n", hb_stackReturnItem()->item.asString.value, *( hb_stackReturnItem()->item.asString.pulHolders ) );
            }
            else if( HB_IS_OBJECT( hb_stackReturnItem() ) )
            {
               OutputDebugValues( "*********** Owners: %i\n", hb_stackReturnItem()->item.asArray.value->ulHolders );
            }
            else
            {
               OutputDebugValues( "*********** Type: %i\n", hb_stackReturnItem()->type );
            }

            if( pResult )
            {
               ItemToVariant( pResult, hb_stackReturnItem() );
            }

            // Safety!
            hb_vmRequestReset();
            hb_itemClear( hb_stackReturnItem() ); // NOTE: was COMMENTED!!!

            OutputDebugValues( "Done!\n" );
         }
         else
         {
            return E_INVALIDARG;
         }
      }
      else
      {
         OutputDebugValues( "Invalid Flag\n" );
         return E_INVALIDARG;
      }
   }
   else
   {
      OutputDebugValues( "Invalid ID\n" );
      return E_INVALIDARG;
   }

   return S_OK;;
}

static HRESULT STDMETHODCALLTYPE ClassFactoryQueryInterface( IClassFactory *This, REFIID riid, void **ppvObj )
{
    //OutputDebugValues( "ClassFactoryQueryInterface( %p, %p, %p)\n", This, riid, ppvObj );

    #if 0
       OutputDebugValues( "riid %i %i %i %.*s\n", riid->Data1, riid->Data2, riid->Data3, 8, riid->Data4 );

       OutputDebugValues( "IID_Unknown   %i %i %i %.*s\n", IID_IUnknown.Data1, IID_IUnknown.Data2, IID_IUnknown.Data3, 8, IID_IUnknown.Data4 );
       OutputDebugValues( "IID_IClassFactory %i %i %i %.*s\n", IID_IClassFactory.Data1, IID_IClassFactory.Data2, IID_IClassFactory.Data3, 8, IID_IClassFactory.Data4 );
    #endif

    if( ppvObj == NULL )
    {
        return E_INVALIDARG;
    }

    if( IsEqualIID( riid, (&IID_IUnknown) ) == FALSE && IsEqualIID( riid, (&IID_IClassFactory) ) == FALSE )
    {
       OutputDebugString( "*** ClassFactory->Unknown Interface\n" );

       *ppvObj = NULL;
       return E_NOINTERFACE;
    }

    *ppvObj = (void *) This;

    ((LPUNKNOWN) *ppvObj)->lpVtbl->AddRef( (LPUNKNOWN) *ppvObj );

    return S_OK;
}

static ULONG STDMETHODCALLTYPE ClassFactoryAddRef( IClassFactory *This )
{
   //OutputDebugString( "ClassFactoryAddRef(%i)\n", ( (IClassFactoryObject *) This )->m_iRef );

   return ++( ( (IClassFactoryObject *) This )->m_iRef );
}

static ULONG STDMETHODCALLTYPE ClassFactoryRelease( IClassFactory *This )
{
   int iRef = --( ( (IClassFactoryObject*) This )->m_iRef );

   //OutputDebugString( "ClassFactoryRelease\n" );

   if( iRef == 0 ) //TODO: Review!!!
   {
       //OutputDebugString( "~~~Free\n" );
       free( (void *) This );
       return 0;
   }

   return iRef;
}

static HRESULT STDMETHODCALLTYPE ClassFactoryCreateInstance( IClassFactory *This, IUnknown * pUnkOuter, REFIID riid,  void ** ppvObj )
{
   IDispatchObject *NewObject = ( IDispatchObject *) malloc( sizeof( IDispatchObject ) );
   HRESULT hr;
   PHB_ITEM pxObject = NULL, pxHashMembers = hb_itemNew( NULL );

   //OutputDebugString( "ClassFactory->CreateInstance\n" );

  #ifndef AX
   #ifndef NODLLMAIN
    if( s_bOleWrap == FALSE )
    {
        if( s_pSym_CreateInstance )
        {
           OutputDebugValues( "Calling PRG CreateInstance\n" );

           hb_vmPushSymbol( s_pSym_CreateInstance );
           hb_vmPushNil();
           hb_vmDo(0);

           if( HB_IS_OBJECT( hb_stackReturnItem() ) )
           {
              pxObject = hb_itemNew( hb_stackReturnItem() );
           }

           // Safety!
           hb_vmRequestReset();
           hb_itemClear( hb_stackReturnItem() ); // NOTE: was COMMENTED!!!
        }
        else
        {
           OutputDebugValues( "***Missing PRG CreateInstance!!!\n" );
        }
    }
   #endif
  #endif

    hb_hashNew( pxHashMembers );
    hb_hashSetCaseMatch( pxHashMembers, (BOOL) FALSE );

    NewObject->lpVtbl = &s_DispatchVtbl;
    NewObject->pUnkOuter = pUnkOuter;
    NewObject->m_iRef = 0;
    NewObject->m_pxObject = pxObject;
    NewObject->m_uiMembers = 0;
    NewObject->m_pxHashMembers = pxHashMembers;
    NewObject->m_pxHashHandlers = NULL;

    memcpy( &( NewObject->m_InterfaceIID ), &IID_NULL, sizeof( IID ) );

    OutputDebugValues( "Returning: %p %p %p %p\n", ppvObj, NewObject, pxObject, pUnkOuter );

    hr = NewObject->lpVtbl->QueryInterface( (IDispatch *) NewObject, riid, ppvObj );

    s_iObjects++;

    if( FAILED( hr ) )
    {
       DispatchObjectFree( (IDispatch *) NewObject );
    }

    return hr;
}

static HRESULT STDMETHODCALLTYPE ClassFactoryLockServer( IClassFactory *This, BOOL fLock )
{
   if( fLock )
   {
      s_iLock++;
   }
   else
   {
      s_iLock--;
   }

   return S_OK;
}

static PHB_ITEM NameFromDispatch( IDispatch *pDisp )
{
   PHB_ITEM pClassName = NULL;
   ITypeInfo *pITypeInfo = NULL;

   if( SUCCEEDED( pDisp->lpVtbl->GetTypeInfo( pDisp, 0, LOCALE_SYSTEM_DEFAULT, &pITypeInfo ) ) )
   {
      TYPEATTR *pTypeAttr = NULL;

      if( SUCCEEDED( pITypeInfo->lpVtbl->GetTypeAttr( pITypeInfo, &pTypeAttr ) ) )
      {
         BSTR bstrProgID = NULL, bstrIID = NULL;
         char *sClassName = NULL;

         ProgIDFromCLSID( &pTypeAttr->guid, &bstrProgID );

         if( bstrProgID )
         {
            sClassName = hb_oleWideToAnsi( bstrProgID );

            CoTaskMemFree( bstrProgID );
            bstrProgID = NULL;
         }
         else
         {
            StringFromCLSID( &pTypeAttr->guid, &bstrIID );

            if( bstrIID )
            {
               sClassName = hb_oleWideToAnsi( bstrIID );

               CoTaskMemFree( bstrIID );
               bstrIID = NULL;
            }
         }

         if( sClassName )
         {
            pClassName = hb_itemNew( NULL );
            hb_itemPutCPtr( pClassName, sClassName, strlen( sClassName ) );
         }

         pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
         pTypeAttr = NULL;
      }

      pITypeInfo->lpVtbl->Release( pITypeInfo );
      pITypeInfo = NULL;
   }
   else
   {
      TraceLog( NULL, "Failed to load TypeInfo in: %s(%i)\n", __FILE__, __LINE__ );
   }

   return pClassName;
}

static void ParamsToDispParams( PHB_ITEM paParams, DISPPARAMS *pDispParams, unsigned int uiStart, unsigned int uiParams )
{
   VARIANTARG *pArgs = NULL;
   unsigned int uiParam, uiArgs, uiArg;

   uiArgs = uiParams - uiStart + 1;

   if( uiArgs )
   {
      pArgs = ( VARIANTARG * ) hb_xgrab( sizeof( VARIANTARG ) * uiArgs );

      for( uiParam = uiStart; uiParam <= uiParams; uiParam++ )
      {
         uiArg = uiParams - uiParam;

         //OutputDebugValues( "Param: %i Arg: %i Type: %i\n", uiParam, uiArg, hb_arrayGetItemPtr( paParams, uiParam )->type );

         ItemToVariant( &( pArgs[ uiArg ] ), hb_arrayGetItemPtr( paParams, uiParam ) );
      }
   }

   //OutputDebugValues( "Args: %i\n", uiArgs );

   pDispParams->rgvarg            = pArgs;
   pDispParams->cArgs             = uiArgs;
   pDispParams->rgdispidNamedArgs = 0;
   pDispParams->cNamedArgs        = 0;
}

IDispatch *OleWrapEvents( PHB_ITEM pObject, REFIID pInterfaceIID, PHB_ITEM pHashHandlers, PHB_ITEM pHashMembers )
{
   IClassFactory *pClassFactory = NULL;
   IDispatch *pDispatch = NULL;
   ULONG ulPos = 0;
   HB_ITEM_NEW( GenericID );

   DllGetClassObject( 0, &IID_IClassFactory, (void **) &pClassFactory );

   s_bOleWrap = TRUE;
   pClassFactory->lpVtbl->CreateInstance( pClassFactory, NULL, &IID_IDispatch, (void **) &pDispatch );
   s_bOleWrap = FALSE;

   pClassFactory->lpVtbl->Release( pClassFactory );
   pClassFactory = NULL;

   ( (IDispatchObject *) pDispatch )->m_pxObject = hb_itemNew( pObject );
   memcpy( &( ( (IDispatchObject *) pDispatch )->m_InterfaceIID), pInterfaceIID, sizeof( IID ) );
   ( (IDispatchObject *) pDispatch )->m_pxHashHandlers = hb_itemNew( pHashHandlers );

   if( ( (IDispatchObject *) pDispatch )->m_pxHashMembers )
   {
      hb_itemRelease( ( (IDispatchObject *) pDispatch )->m_pxHashMembers );
   }
   ( (IDispatchObject *) pDispatch )->m_pxHashMembers = hb_itemNew( pHashMembers );

   hb_itemPutCL( &GenericID, "*", 1 );

   if( hb_hashScan( ( (IDispatchObject *) pDispatch )->m_pxHashHandlers, &GenericID, &ulPos ) )
   {
      ( (IDispatchObject *) pDispatch )->m_ulGenericPos = ulPos;
   }
   else
   {
      ( (IDispatchObject *) pDispatch )->m_ulGenericPos = 0;
   }

   return pDispatch;
}

IDispatch *OleWrapFunction( const char *sName )
{
   PHB_DYNS pDyn = hb_dynsymFindName( sName );

   if( pDyn )
   {
      if( pDyn->pSymbol->value.pFunPtr )
      {
        #if ( ! defined( EXPORT_RTL ) ) && ! defined( AX )
         if( IsExportedSymbol( pDyn ) )
        #endif
         {
            IDispatch *This = OleWrap( NULL );
            PHB_ITEM pName = hb_itemNew( NULL ), pDynSym = hb_itemNew( NULL );

            hb_itemPutC( pName, sName );
            hb_itemPutPtr( pDynSym, (void *) pDyn );

            hb_hashAdd( ( (IDispatchObject *) This )->m_pxHashMembers, 1, pName, pDynSym );

            hb_itemRelease( pName );
            pName = NULL;

            hb_itemRelease( pDynSym );
            pDynSym = NULL;

			return This;
         }
        #if ( ! defined( EXPORT_RTL ) ) && ! defined( AX )
         else
         {
            OutputDebugValues( "NON Module Function! %p\n", pDyn->pSymbol );
         }
        #endif
      }
   }
   else
   {
      OutputDebugString( "!!! Dynamic symbol not found: " );
      OutputDebugString( sName );
      OutputDebugString( "\n" );
   }

   return NULL;
}

HB_FUNC( __OLECONNECTINTERFACE )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_OBJECT );
   IDispatch *pDisp = (IDispatch *) hb_parnl(2), *pSink;
   REFIID  pInterfaceIID = (REFIID) hb_parc(3);
   PHB_ITEM pHashHandlers = hb_param( 4, HB_IT_HASH );
   PHB_ITEM pHashMembers = hb_param( 5, HB_IT_HASH );

   IConnectionPointContainer *pConnectionPointContainer = NULL;
   IConnectionPoint *pConnectionPoint;
   DWORD Cookie;

  #ifdef _DEBUG
    #if 0
       BSTR bstrIID;

       StringFromIID( pInterfaceIID, &bstrIID );
       OutputDebugValues( "IID: %S\n", bstrIID );
       CoTaskMemFree( bstrIID );
    #endif
  #endif

   s_nOleError = pDisp->lpVtbl->QueryInterface( pDisp, &IID_IConnectionPointContainer, (void**) &pConnectionPointContainer );

   if( FAILED( s_nOleError ) )
   {
      TraceLog( NULL, "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "IID_IConnectionPointContainer", "__OLECONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   s_nOleError = pConnectionPointContainer->lpVtbl->FindConnectionPoint( pConnectionPointContainer, pInterfaceIID, &pConnectionPoint );

   pConnectionPointContainer->lpVtbl->Release( pConnectionPointContainer );
   pConnectionPointContainer = NULL;

   if( FAILED( s_nOleError ) )
   {
      TraceLog( NULL, "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "FindConnectionPoint", "__OLECONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      hb_ret();
      return;
   }

   pSink = OleWrapEvents( pSelf, pInterfaceIID, pHashHandlers, pHashMembers );
   s_nOleError = pConnectionPoint->lpVtbl->Advise( pConnectionPoint, (IUnknown*) pSink, &Cookie );

   pSink->lpVtbl->Release( pSink );
   pSink = NULL;

   pConnectionPoint->lpVtbl->Release( pConnectionPoint );
   pConnectionPoint = NULL;

   //pSink->lpVtbl->Release( pSink );

   if( FAILED( s_nOleError ) )
   {
      TraceLog( NULL, "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "Advise", "__OLECONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      hb_ret();
      return;
   }

   hb_retnl( Cookie );
}

HB_FUNC( __OLEDISCONNECTINTERFACE )
{
   IDispatch *pDisp = (IDispatch *) hb_parnl(1);
   REFIID  pInterfaceIID = (REFIID) hb_parc(2);
   DWORD Cookie = (DWORD) hb_parnl(3);

   IConnectionPointContainer *pConnectionPointContainer = NULL;
   IConnectionPoint *pConnectionPoint = NULL;

   #ifdef _DEBUG
     #if 0
        BSTR bstrIID;

        StringFromIID( pInterfaceIID, &bstrIID );
        OutputDebugValues( "IID: %S\n", bstrIID );
        CoTaskMemFree( bstrIID );
     #endif
   #endif

   s_nOleError = pDisp->lpVtbl->QueryInterface( pDisp, &IID_IConnectionPointContainer, (void**) &pConnectionPointContainer );

   if( FAILED( s_nOleError ) )
   {
      OutputDebugValues( "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "QueryInterface failed", "__OLEDISCONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      hb_ret();
      return;
   }

   s_nOleError = pConnectionPointContainer->lpVtbl->FindConnectionPoint( pConnectionPointContainer, pInterfaceIID, &pConnectionPoint );

   pConnectionPointContainer->lpVtbl->Release( pConnectionPointContainer );
   pConnectionPointContainer = NULL;

   if( FAILED( s_nOleError ) )
   {
      OutputDebugValues( "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "Release failed", "__OLEDISCONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      hb_ret();
      return;
   }

   s_nOleError = pConnectionPoint->lpVtbl->Unadvise( pConnectionPoint, Cookie );

   pConnectionPoint->lpVtbl->Release( pConnectionPoint );
   pConnectionPoint = NULL;

   if( FAILED( s_nOleError ) )
   {
      OutputDebugValues( "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );
      hb_errRT_BASE( EG_ARG, 6001, "Invoke failed", "__OLEDISCONNECTINTERFACE", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      hb_ret();
      return;
   }
}

static void FreeDispParams( PHB_ITEM paParams, DISPPARAMS *pDispParams, unsigned int uiStart )
{
   VARIANTARG *pArgs = pDispParams->rgvarg;
   unsigned int uiParam, uiArg, uiParams = hb_arrayLen( paParams );
   PHB_ITEM pItem;

   for( uiParam = uiStart; uiParam <= uiParams; uiParam++ )
   {
      uiArg = uiParams - uiParam;

      pItem = hb_arrayGetItemPtr( paParams, uiParam );

      //OutputDebugValues( "uiArg: %i Param: %i Type: %i VT: %i\n", uiArg, uiParam, pItem->type, pArgs[ uiArg ].n1.n2.vt );

      if( HB_IS_BYREF( pItem ) )
      {
         VariantToItem( hb_itemUnRef( pItem ), &( pArgs[ uiArg ] ) );
      }
      else
      {
         if( pArgs[ uiArg ].n1.n2.vt & VT_BYREF )
         {
            OutputDebugValues( "Param: %i was not BYREF, but DispParam type: %i is a BYREF\n", uiParam, pArgs[ uiArg ].n1.n2.vt );
            TraceLog( NULL, "Param: %i was not BYREF, but DispParam type: %i is a BYREF\n", uiParam, pArgs[ uiArg ].n1.n2.vt );
         }
      }

      VariantClear( &( pArgs[ uiArg ] ) );
   }

   if( pDispParams->rgvarg )
   {
      hb_xfree( (void *) pDispParams->rgvarg );
   }
}

HB_FUNC( __OLEINVOKEDISPATCH )
{
   IDispatch *pDisp = (IDispatch *) hb_parnl(1);
   DISPID  dispIdMember = (DISPID) hb_parnl(2);
   WORD wFlags = (WORD) hb_parnl(3);
   const char *sGUID = hb_parcx( 4 );
   PHB_ITEM paParams = hb_itemNew( hb_param( 5, HB_IT_ARRAY ) );

   DISPPARAMS DispParams;
   DISPID PropPut = DISPID_PROPERTYPUT;
   VARIANT RetVal;
   EXCEPINFO Exception;
   unsigned int uiArgErr = 0;

   #ifdef _DEBUG
      #if 0
         char *sName;
         PHB_ITEM pSelf = hb_itemNew( hb_arrayGetItemPtr( paParams, 1 ) );

         hb_objSendMsg( pSelf, "CLASSNAME", 0 );
         sName = hb_parc(-1);

         hb_itemRelease( pSelf );
         OutputDebugValues( "Class: %s\n", sName );
      #endif

      OutputDebugValues( "__OLEINVOKEDISPATCH: Disp: %p, Id: %i, Flags: %i Return GUID: %s Params: %i\n", pDisp, dispIdMember, wFlags, sGUID, hb_arrayLen( paParams ) - 1 );
   #endif

   if( pDisp == NULL )
   {
      hb_errRT_BASE( EG_ARG, 6001, "NULL IDispatch", "__OLEINVOKEDISPATCH", 3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }

   //TraceLog( NULL, "SetParams\n" );
   ParamsToDispParams( paParams, &DispParams, 2, hb_arrayLen( paParams ) );
   //TraceLog( NULL, "SetParams DONE!\n" );

   VariantInit( &RetVal );
   memset( (LPBYTE) &Exception, 0, sizeof( Exception ) );

   if( wFlags & ( INVOKE_PROPERTYPUT | INVOKE_PROPERTYPUTREF ) )
   {
     InvokeProperty:

      DispParams.rgdispidNamedArgs = &PropPut;
      DispParams.cNamedArgs        = 1;

      //TraceLog( NULL, "PROPERTYPUT, Args: %i Member: %i Flags: %i VT: %i\n", DispParams.cArgs, dispIdMember, wFlags, DispParams.rgvarg[ 0 ].n1.n2.vt );

      s_nOleError = pDisp->lpVtbl->Invoke( pDisp, dispIdMember, (REFIID) &IID_NULL, LOCALE_SYSTEM_DEFAULT, wFlags, &DispParams, NULL, &Exception, &uiArgErr );

      //OutputDebugValues( "Result: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );

      hb_itemReturn( hb_arrayGetItemPtr( paParams, 2 ) );
   }
   else
   {
     //InvokeMethod:

      s_nOleError = pDisp->lpVtbl->Invoke( pDisp, dispIdMember, (REFIID) &IID_NULL, LOCALE_SYSTEM_DEFAULT, wFlags, &DispParams, &RetVal, &Exception, &uiArgErr );

      //OutputDebugValues( "Result: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );

      if( FAILED( s_nOleError ) )
      {
         if( ( wFlags == INVOKE_PROPERTYGET ) && DispParams.cArgs )
         {
            OutputDebugValues( "Recover using INVOKE_PROPERTYPUT" );

            wFlags = INVOKE_PROPERTYPUT;
            goto InvokeProperty;
         }
      }

      VTObjectToItem( hb_stackReturnItem(), &RetVal, sGUID );

      VariantClear( &RetVal );
   }

   FreeDispParams( paParams, &DispParams, 2 );
   hb_itemRelease( paParams );

   if( FAILED( s_nOleError ) )
   {
      TraceLog( NULL, "Error: %p in: %s(%i)\n", s_nOleError, __FILE__, __LINE__ );

      if( s_nOleError == DISP_E_EXCEPTION )
      {
         char *sSource, *sDesc;

         TraceLog( NULL, "Exception: %p,%p %p %s -> %s\n", Exception.wCode, Exception.scode, Exception.pfnDeferredFillIn, sSource = hb_oleWideToAnsi( Exception.bstrSource ), sDesc = hb_oleWideToAnsi( Exception.bstrDescription ) );

         if( sSource )
         {
            hb_xfree( (void *) sSource );
         }

         if( sDesc )
         {
            hb_xfree( (void *) sDesc );
         }
      }

      hb_errRT_BASE( EG_ARG, 6001, "Invoke failed", "__OLEINVOKEDISPATCH", 5, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ), hb_paramError( 5 ) );
      hb_ret();
   }
}

#if 0

static void cdecl __PushPointer( void *Push )
{
   (Push);
}

static void cdecl __PushLongLong( LONGLONG Push )
{
   (Push);
}

static void cdecl __PushLong( long Push )
{
   (Push);
}

static void cdecl __PushInt( int Push )
{
   (Push);
}

static void cdecl __PushChar( char Push )
{
   (Push);
}

static void cdecl __PushDouble( double Push )
{
   (Push);
}

static void cdecl __PushFloat( float Push )
{
   (Push);
}

static void cdecl __PushCY( CY Push )
{
   (Push);
}

static void cdecl __PushDec( DECIMAL Push )
{
   (Push);
}

void PushVTFromItem( PHB_ITEM pItem, VARTYPE vt )
{
   DECIMAL Decimal;
   CY      Currency;
   VARIANT Variant;

   switch( vt )
   {
      case VT_EMPTY:
      case VT_NULL:
      case VT_VOID:
        PushPointer( NULL );
        break;

      case VT_I1:
      case VT_UI1:
        PushChar( (char) hb_itemGetNL( pItem ) );
        break;

      case VT_I2:
      case VT_UI2:
        PushShort( (short int) hb_itemGetNL( pItem ) );
        break;

      case VT_INT:
      case VT_UINT:
        PushInt( (int) hb_itemGetNL( pItem ) );
        break;

      case VT_I4:
      case VT_UI4:
      case VT_HRESULT:
        PushLong( (long) hb_itemGetNL( pItem ) );
        break;

      case VT_I8:
      case VT_UI8:
        PushLongLong( (LONGLONG) hb_itemGetNLL( pItem ) );
        break;

      case VT_R4:
        PushFloat( (float) hb_itemGetND( pItem ) );
        break;

      case VT_R8:
        PushFloat( (double) hb_itemGetND( pItem ) );
        break;

      case VT_CY:
        VarCyFromR8( hb_itemGetND( pItem ), &Currency );
        PushCY( Currency );
        break;

      case VT_DECIMAL:
        VarDecFromR8( hb_itemGetND( pItem ), &Decimal );
        PushDec( Decimal );
        break;

      case VT_DATE:
        PushDouble( hb_itemGetDTD( pItem ) - (double) 2415019 );
        break;

      case VT_BSTR:
        PushPointer( (void *) hb_oleAnsiToSysString( hb_itemGetCPtr( pItem ) ) );
        break;

      case VT_DISPATCH:
        PushPointer( (void *) oleGetDispatch( pItem ) );
        break;

      case VT_ERROR:
        PushPointer( NULL );
        break;

      case VT_BOOL:
        PushLong( hb_itemGetL( pItem ) );
        break;

      case VT_VARIANT:
        ItemToVariant( pItem, &Variant );
        PushPointer( (void *) &Variant );
        break;

      case VT_UNKNOWN:
        PushPointer( (void *) oleGetUnknown( pItem ) );
        break;

      case VT_PTR:
        PushPointer( (void *) hb_itemGetNL( pItem ) );
        break;

      case VT_SAFEARRAY:
        PushPointer( NULL );
        break;

      case VT_CARRAY:
        PushPointer( NULL );
        break;

      case VT_USERDEFINED:
        PushPointer( NULL );
        break;

      case VT_LPSTR:
        PushPointer( (void *) hb_itemGetCPtr( pItem ) );
        break;

      case VT_LPWSTR:
        PushPointer( (void *) hb_oleAnsiToSysString( hb_itemGetCPtr( pItem ) ) );
        break;

      case VT_RECORD:
        PushPointer( NULL );
        break;

      case VT_FILETIME:
        PushPointer( NULL );
        break;

      case VT_BLOB:
        PushPointer( NULL );
        break;

      case VT_STREAM:
        PushPointer( NULL );
        break;

      case VT_STORAGE:
        PushPointer( NULL );
        break;

      case VT_STREAMED_OBJECT:
        PushPointer( NULL );
        break;

      case VT_STORED_OBJECT:
        PushPointer( NULL );
        break;

      case VT_BLOB_OBJECT:
        PushPointer( NULL );
        break;

      case VT_CF:
        PushPointer( NULL );
        break;

      case VT_CLSID:
        PushPointer( NULL );
        break;

      //case VT_VERSIONED_STREAM:
      case VT_BSTR_BLOB:
        PushPointer( NULL );
        break;

      case VT_VECTOR:
        PushPointer( NULL );
        break;

      case VT_ARRAY:
        PushPointer( NULL );
        break;

      //case VT_BYREF:

      //case VT_RESERVED:

      //case VT_ILLEGAL:
      //case VT_ILLEGALMASKED:
      //case VT_TYPEMASK:

      default:
         hb_errRT_BASE( EG_ARG, 5020, "Unexpected VT", "PushVTFromItem", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( __OLEINVOKEVTBL )
{
   typedef HRESULT ( STDMETHODCALLTYPE * VtblFunc )( IUnknown * );

   IUnknown *pUnk = ( IUnknown * ) hb_parnl( 1 );
   VtblFunc pFunc;
   VtblFunc *vTbl;
   int i;
   double adDoubles[16];
   void *apPointers[16];

   vTbl = ( VtblFunc * ) (void *)( &( pUnk->lpVtbl->QueryInterface ) ) + hb_parnl(2);
   pFunc = *vTbl;

   for( i = hb_pcount(); i > 2; i-- )
   {
      switch( ( hb_parinfo( i ) & ~HB_IT_BYREF ) )
      {
         case HB_IT_STRING:
         case HB_IT_MEMO:
            PushPointer( (void *) hb_parc( i ) );
            break;

         case HB_IT_LOGICAL:
            if( ISBYREF( i ) )
            {
               apPointers[ i ] = (void *) hb_parl( i );
               PushPointer( &apPointers[ i ] );
            }
            else
            {
              PushLong( hb_parl( i ) );
            }
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
            if( ISBYREF( i ) )
            {
               apPointers[ i ] = (void *) hb_parnl( i );
               PushPointer( (void *) &apPointers[ i ] );
            }
            else
            {
               PushLong( hb_parnl( i ) );
            }
            break;

         case HB_IT_DOUBLE:
            if( ISBYREF( i ) )
            {
               adDoubles[ i ] = hb_parnd( i );
               PushPointer( (void *) &adDoubles[ i ] );
            }
            else
            {
               PushDouble( hb_parnd( i ) );
            }
            break;

         case HB_IT_DATE:
            if( ISBYREF( i ) )
            {
               adDoubles[ i ] = (double) ( hb_pardl( i ) - 2415019 ) ;
               PushPointer( &adDoubles[ i ] );
            }
            else
            {
               PushDouble( (double) ( hb_pardl( i ) - 2415019 ) );
            }
            break;

         default:
            PushPointer( NULL );
            break;
      }
   }

   s_nOleError = pFunc( pUnk );

   for( i = 3; i <= hb_pcount(); i++ )
   {
      if( ISBYREF( i ) )
      {
         switch( ( hb_parinfo( i ) & ~HB_IT_BYREF ) )
         {
            case HB_IT_STRING:
            case HB_IT_MEMO:
               hb_storc( (char *) hb_parc( i ), i );
               break;

            case HB_IT_LOGICAL:
               hb_storl( (long) apPointers[ i ], i );
               break;

            case HB_IT_INTEGER:
            case HB_IT_LONG:
               hb_stornl( (long) apPointers[ i ], i );
               break;

            case HB_IT_DOUBLE:
               hb_stornd( adDoubles[ i ], i );
               break;

            case HB_IT_DATE:
               hb_stordl( (long)( adDoubles[ i ] ) + 2415019, i );
               break;
         }
      }
   }

   hb_ret();
}
#endif

void DoEvents( void )
{
   MSG msg;

   //if( GetInputState() )
   {
      while( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) )
      {
         TranslateMessage( &msg );
         DispatchMessage( &msg );
      }
   }
}

HB_FUNC( DOEVENTS )
{
   DoEvents();
}

BOOL IsExportedSymbol( PHB_DYNS pDyn )
{
   return TRUE;
}

PHB_SYMB OleServerSymbols( USHORT *puiSymbols )
{
   return NULL;
}

HRESULT ItemToVariant( VARIANT *pVariant, PHB_ITEM pItem )
{
   BOOL bByRef;
   VARIANT mVariant;
   VARTYPE vt;
   SAFEARRAYBOUND rgsabound;
   void *pSource;// = NULL;
   unsigned long i;
   char *sString;// = NULL;

   if( pVariant == NULL )
   {
      OutputDebugString( "****************** NULL Variant ************************\n" );
      return E_INVALIDARG;
   }

   if( HB_IS_BYREF( pItem ) )
   {
      pItem = hb_itemUnRef( pItem );
      bByRef = TRUE;
   }
   else
   {
      bByRef = FALSE;
   }

   // NOTE! This function shoul never be called with a valid, uncleared Variant!!!
   VariantInit( pVariant );

   switch( pItem->type )
   {
      case HB_IT_NIL:
        //pVariant->n1.n2.vt   = VT_EMPTY;
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
           //printf( L"*** BYREF >%ls<\n", *pVariant->n1.n2.n3.bstrVal );
        }
        else
        {
           pVariant->n1.n2.vt   = VT_BSTR;
           pVariant->n1.n2.n3.bstrVal = hb_oleAnsiToSysString( sString );
           //printf( L"*** >%ls<\n", pVariant->n1.n2.n3.bstrVal );
        }
        break;
      }

      case HB_IT_LOGICAL:
        if( bByRef )
        {
           pVariant->n1.n2.vt = VT_BYREF | VT_BOOL;
           pVariant->n1.n2.n3.pboolVal = (short *) &( pItem->item.asLogical.value );
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
        pVariant->n1.n2.vt   = VT_PTR;
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

               // Safety!
               hb_vmRequestReset();
               hb_itemClear( hb_stackReturnItem() );

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

               // Safety!
               hb_vmRequestReset();
               hb_itemClear( hb_stackReturnItem() );

               // aArray := oVTArray:Value
               hb_vmPushSymbol( s_pSym_Value->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               // Safety!
               hb_vmRequestReset();
               //hb_itemClear( hb_stackReturnItem() ); do NOT uncomment, see pItem assignment below!!!

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

               // Safety!
               hb_vmRequestReset();
               hb_itemClear( hb_stackReturnItem() );

               //value := oVT:value
               hb_vmPushSymbol( s_pSym_Value->pSymbol );
               hb_vmPush( pItem );
               hb_vmSend( 0 );

               // Safety!
               hb_vmRequestReset();

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
                    pVariant->n1.n2.n3.scode = hb_parni(1);
                    break;

                  default:
                     TraceLog( NULL, "Unexpected VT type %p in: %s(%i)!\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
               }

               hb_itemClear( hb_stackReturnItem() );

               break;
            }
            else
            {
               OutputDebugValues( "Wrap Class: '%s'\n", hb_objGetClsName( pItem ) );
               pVariant->n1.n2.vt = VT_DISPATCH;
               pVariant->n1.n2.n3.pdispVal = OleWrap( pItem );
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
               ItemToVariant( &mVariant, hb_arrayGetItemPtr( pItem, i + 1 ) );
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

   return S_OK;
}

static PHB_ITEM SafeArrayToArray( SAFEARRAY *parray, UINT iDim, long* rgIndices, VARTYPE vt )
{
   long iFrom, iTo, iLen, i;
   PHB_ITEM pArray = hb_itemNew( NULL );;

   if( parray == NULL )
   {
      TraceLog( NULL, "NULL prray in: %s(%i)\n", __FILE__, __LINE__ );

      hb_arrayNew( pArray, 0 );
      return pArray;
   }

   SafeArrayGetLBound( parray, iDim, &iFrom );
   SafeArrayGetUBound( parray, iDim, &iTo );

   iLen = iTo - iFrom + 1;

   //TraceLog( NULL, "Dim: %i From: %i To: %i Len: %i in: %s(%i)\n", iDim, iFrom, iTo, iLen, __FILE__, __LINE__ );

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
         pSubArray = NULL;
      }
   }
   else
   {
      VARIANT mElem;
      void *pTarget;
      char *sArray;

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

            VTObjectToItem( pArray->item.asArray.value->pItems + ( i - iFrom ), &mElem, "" );

            VariantClear( &mElem );
         }
      }
   }

   //TraceLog( NULL, "Return len: %i in: %s(%i)\n", pArray->item.asArray.value->ulLen, __FILE__, __LINE__ );

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

      // Safety!
      hb_vmRequestReset();
      hb_itemClear( hb_stackReturnItem() );

      hb_itemRelease( pVT );
      pVT = NULL;
   }

   return pArray;
}

HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant )
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
            pUnk->lpVtbl->QueryInterface( pUnk, (REFIID) &IID_IDispatch, (void **) &pDisp );
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

               // Safety!
               hb_vmRequestReset();

               if( pItem != hb_stackReturnItem() )
               {
                  hb_itemForwardValue( pItem, hb_stackReturnItem() );
               }

               hb_itemRelease( pVT );
               pVT = NULL;

               hb_itemRelease( pUnknown );
               pUnknown = NULL;
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

            // Safety!
            hb_vmRequestReset();

            // If retrieved from IUnknown than doubly added!
            if( pVariant->n1.n2.vt == VT_UNKNOWN || pVariant->n1.n2.vt == ( VT_UNKNOWN | VT_BYREF ) )
            {
               pDisp->lpVtbl->Release( pDisp );
            }

            hb_itemRelease( pOleAuto );
            pOleAuto = NULL;

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
           VariantToItem( pItem, pVariant->n1.n2.n3.pvarVal );
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
               rgIndices = NULL;

               hb_itemForwardValue( pItem, pArray );

               hb_itemRelease( pArray );
               pArray = NULL;
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

static HRESULT VTObjectToItem( PHB_ITEM pItem, VARIANT *pVariant, const char * sGUID )
{
   IDispatch *pDisp = NULL;

   if( sGUID == NULL || sGUID[0] == '\0' )
   {
      return VariantToItem( pItem, pVariant );
   }

   hb_itemClear( pItem );

   if( pVariant->n1.n2.vt == VT_DISPATCH && pVariant->n1.n2.n3.pdispVal )
   {
      pDisp = pVariant->n1.n2.n3.pdispVal;
      //pDisp->lpVtbl->AddRef( pDisp );
   }
   else if( pVariant->n1.n2.vt == ( VT_DISPATCH | VT_BYREF ) && pVariant->n1.n2.n3.ppdispVal && *pVariant->n1.n2.n3.ppdispVal )
   {
      pDisp = *pVariant->n1.n2.n3.ppdispVal;
      //pDisp->lpVtbl->AddRef( pDisp );
   }
   else if( pVariant->n1.n2.vt == VT_UNKNOWN && pVariant->n1.n2.n3.punkVal )
   {
      pVariant->n1.n2.n3.punkVal->lpVtbl->QueryInterface( pVariant->n1.n2.n3.punkVal, &IID_IDispatch, (void **) &pDisp );

      if( pDisp == NULL )
      {
         pVariant->n1.n2.n3.punkVal->lpVtbl->AddRef( pVariant->n1.n2.n3.punkVal );
         hb_itemPutPtr( pItem, (void *) pVariant->n1.n2.n3.punkVal );
         return S_OK;
      }
      else
      {
         // pDisp->AddRef() was performed by QueryInterface!
      }
   }
   else if( pVariant->n1.n2.vt == ( VT_UNKNOWN | VT_BYREF ) && pVariant->n1.n2.n3.ppunkVal && *pVariant->n1.n2.n3.ppunkVal )
   {
      (*pVariant->n1.n2.n3.ppunkVal)->lpVtbl->QueryInterface( *pVariant->n1.n2.n3.ppunkVal, &IID_IDispatch, (void **) &pDisp );

      if( pDisp == NULL )
      {
         (*pVariant->n1.n2.n3.ppunkVal)->lpVtbl->AddRef( *pVariant->n1.n2.n3.ppunkVal );
         hb_itemPutPtr( pItem, (void *) pVariant->n1.n2.n3.punkVal );
         return S_OK;
      }
      else
      {
         // pDisp->AddRef() was performed by QueryInterface!
      }
   }
   else if( pVariant->n1.n2.vt == VT_PTR && pVariant->n1.n2.n3.byref )
   {
      hb_itemPutPtr( pItem, (void *) pVariant->n1.n2.n3.byref );
      return S_OK;
   }
   else
   {
      #ifdef _DEBUG
         TraceLog( NULL, "Non Object, type: %p in: %s(%i)\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
      #endif

      return VariantToItem( pItem, pVariant );
   }

   if( pDisp )
   {
      PHB_ITEM pClassName = NULL;

      if( sGUID[0] )
      {
         pClassName = hb_itemNew( hb_param( 4, HB_IT_ANY ) );
      }
      else
      {
         pClassName = NameFromDispatch( pDisp );
      }

      if( pClassName )
      {
         PHB_ITEM pInstance, pTemp = hb_itemNew( NULL );

         // AddRef() will be performed in Wrapdispatch()!
         hb_itemPutNL( pTemp, (LONG) pDisp );
         pInstance = hb_itemDoC( "WRAPDISPATCH", 2, pClassName, pTemp );

         hb_itemForwardValue( pItem, pInstance );

         hb_itemRelease( pClassName );
         pClassName = NULL;

         hb_itemRelease( pTemp );
         pTemp = NULL;

         hb_itemRelease( pInstance );
         pInstance = NULL;

         if( HB_IS_NIL( pItem ) )
         {
            pDisp->lpVtbl->Release( pDisp );
            pDisp = NULL;

            return VariantToItem( pItem, pVariant );
         }
      }
      else
      {
         TraceLog( NULL, "Failed to locate class in: %s(%i)\n", __FILE__, __LINE__ );

         // If retrieved from IUnknown than allready added!
         if( pVariant->n1.n2.vt == VT_UNKNOWN || pVariant->n1.n2.vt == ( VT_UNKNOWN | VT_BYREF ) && pDisp )
         {
            pDisp->lpVtbl->Release( pDisp );
            pDisp = NULL;
         }

         return VariantToItem( pItem, pVariant );
      }
   }
   else
   {
      TraceLog( NULL, "Empty Object type: %p in: %s(%i)\n", pVariant->n1.n2.vt, __FILE__, __LINE__ );
   }

   return S_OK;
}

HRESULT STDMETHODCALLTYPE ErrorToException( PHB_ITEM pError, EXCEPINFO *pException, SCODE scode, const char *sSource )
{
   OutputDebugValues( "ErrorToException( %p, %p, %i, %s )!!!\n", pError, pException, scode, sSource );

   if( pError && strcmp( hb_objGetClsName( pError ), "ERROR" ) == 0 && pException )
   {
      PHB_ITEM pErrorMessage;

      memset( pException, 0, sizeof( EXCEPINFO ) );

      OutputDebugValues( "Raising EXCEPTION!!! Source: '%s'\n", sSource );

      #ifdef AX
         pErrorMessage = hb_itemDoC( "PP_ErrorMessage", 1, pError );
      #else
         pErrorMessage = hb_itemDoC( "ErrorMessage", 1, pError );
      #endif

      // Safety!
      hb_vmRequestReset();

      pException->bstrDescription = hb_oleAnsiToSysString( hb_parcx( -1 ) );

      hb_itemRelease( pErrorMessage );
      pErrorMessage = NULL;

      if( sSource )
      {
         pException->bstrSource = hb_oleAnsiToSysString( sSource );
      }

      if( scode )
      {
         pException->scode = scode;
      }
      else
      {
         pException->scode = E_FAIL;
      }

      return S_OK;
   }

   OutputDebugValues( "ErrorToException( %p, %p, %i, %s ) E_INVALIDARG!!!\n", pError, pException, scode, sSource );

   return E_INVALIDARG;
}

IDispatch *OleWrap( PHB_ITEM pObject )
{
   IClassFactory *pClassFactory = NULL;
   IDispatch *pDispatch = NULL;

   DllGetClassObject( 0, &IID_IClassFactory, (void **) &pClassFactory );

   s_bOleWrap = TRUE;
   pClassFactory->lpVtbl->CreateInstance( pClassFactory, NULL, &IID_IDispatch, (void **) &pDispatch );
   s_bOleWrap = FALSE;

   pClassFactory->lpVtbl->Release( pClassFactory );
   pClassFactory = NULL;

   #ifdef AX
     if( pObject )
	  {
         ( (IDispatchObject *) pDispatch )->m_pxObject = hb_itemNew( pObject );
	  }
	  else
	  {
         ( (IDispatchObject *) pDispatch )->m_pxObject = NULL;
	  }
   #else
      ( (IDispatchObject *) pDispatch )->m_pxObject = hb_itemNew( pObject );
   #endif

   return pDispatch;
}

DLLEXP STDAPI DllGetClassObject( REFCLSID ClassiD, REFIID riid, void **ppvObj )
{
    IClassFactoryObject *NewFactory = ( IClassFactoryObject *) malloc( sizeof( IClassFactoryObject ) );

    //OutputDebugString( "DLLGetClassObject\n" );

    NewFactory->lpVtbl = &s_ClassFactoryVtbl;

    NewFactory->m_iRef = 0;

    *ppvObj = (void *) NewFactory;

    //OutputDebugValues( "Returning: %p %p\n", ppvObj, NewFactory );

    ((LPUNKNOWN) *ppvObj )->lpVtbl->AddRef( ((LPUNKNOWN) *ppvObj ) );

    return NOERROR;
}

#ifndef AX

  PHB_SYMB OleServerFindSymbol( char * sSymbol )
  {
     USHORT uiSymbols = 0, ui;
     PHB_SYMB pSymbols = OleServerSymbols( &uiSymbols );

     OutputDebugValues( "OleServerFindSymbol(%s) symbols: %p %i'\n", sSymbol, pSymbols, uiSymbols );

     for( ui = 0; ui < uiSymbols; ui++ )
     {
        PHB_SYMB pSymbol = pSymbols + ui;

        //OutputDebugValues( "#%i '%s' ? '%s'\n", ui, sSymbol, pSymbol->szName );

        if( strcmp( pSymbol->szName, sSymbol ) == 0 )
        {
           return pSymbol;
        }
     }

     return NULL;
  }

  void OleServerInit( void )
  {
     USHORT uiSymbols = 0, ui;
     PHB_SYMB pSymbols = OleServerSymbols( &uiSymbols );

     // First process STATIC Var initializations.
     for( ui = 0; ui < uiSymbols; ui++ )
     {
        PHB_SYMB pSymbol = pSymbols + ui;
        HB_SYMBOLSCOPE scope = pSymbol->scope.value & HB_FS_INITEXIT;

        if( scope == HB_FS_INITEXIT )
        {
           hb_vmPushSymbol( pSymbol );
           hb_vmPushNil();
           hb_vmDo( 0 );

           // Safety!
           hb_vmRequestReset();
           hb_itemClear( hb_stackReturnItem() );
        }
     }

     // Run INIT Procedures.
     for( ui = 0; ui < uiSymbols; ui++ )
     {
        PHB_SYMB pSymbol = pSymbols + ui;
        HB_SYMBOLSCOPE scope = pSymbol->scope.value & HB_FS_INITEXIT;

        if( scope == HB_FS_INIT )
        {
           hb_vmPushSymbol( pSymbol );
           hb_vmPushNil();
           hb_vmDo( 0 );

           // Safety!
           hb_vmRequestReset();
           hb_itemClear( hb_stackReturnItem() );
        }
     }
  }

  DLLEXP STDAPI DllCanUnloadNow( void )
  {
      OutputDebugValues( "DllCanUnloadNow() -> %i %i\n",  s_iLock, s_iObjects );

      if( s_iLock > 0 || s_iObjects )
      {
         return S_FALSE;
      }

      return S_OK;
  }

#ifndef NODLLMAIN
  PSYMBOLS hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiModuleSymbols, const char *szModule, int iPCodeVer, PHB_ITEM *pGlobals ) /* module symbols initialization */
  {
     return hb_vmProcessPrgDllSymbols( pSymbols, uiModuleSymbols, szModule, iPCodeVer, pGlobals );
  }

  DLLEXP BOOL WINAPI DllMain( HINSTANCE hInstance, DWORD fdwReason, LPVOID lpvReserved )
  {
  	 static BOOL s_bQuit = FALSE;

     OutputDebugValues( "DllMain\n" );

     switch( fdwReason )
     {
        case DLL_PROCESS_ATTACH:
        {
           OutputDebugValues( "Attach...\n" );

           DisableThreadLibraryCalls( hInstance );

           //OutputDebugString( "Disabled Threads\n" );

           g_hDll = hInstance;

           if( hb_dynsymFind( "__EVAL" ) == NULL )
           {
           	  s_bQuit = TRUE;
              hb_vmInit( FALSE );  /* Don't execute first linked symbol */
              OutputDebugValues( "Initialised VM\n" );
           }
           else
           {
              OutputDebugValues( "Initialised, using SHARED VM\n" );
              OleServerInit();
           }

           NilItem.type = HB_IT_NIL;

           s_pSym_TOleAuto       = hb_dynsymFind( "TOLEAUTO" );
           s_pSym_New            = hb_dynsymFind( "NEW" );
           s_pSym_hObj           = hb_dynsymFind( "HOBJ" );
           //s_pSym_InvokeEvent    = hb_dynsymGetCase( "INVOKEEVENT" );

           s_pSym_VTWrapper      = hb_dynsymFind( "VTWRAPPER" );
           s_pSym_VTArrayWrapper = hb_dynsymFind( "VTARRAYWRAPPER" );
           s_pSym_vt             = hb_dynsymGetCase( "VT" );
           s_pSym_Value          = hb_dynsymFind( "VALUE" );

           s_pSym_CreateInstance = OleServerFindSymbol( "CREATEINSTANCE" );
           s_pSym_DllMain        = OleServerFindSymbol( "DLLMAIN" );

           #if 0
              s_pSym_QueryInterface = hb_dynsymGetCase( "QUERYINTERFACE" );
           #endif

           OutputDebugValues( "Initialized Symbols\n" );

           if( s_pSym_DllMain )
           {
              OutputDebugValues( "Do DllMain\n" );

              hb_vmPushSymbol( s_pSym_DllMain );
              hb_vmPushNil();
              hb_vmPushLong( (LONG) hInstance );
              hb_vmPushLong( (LONG) fdwReason );
              hb_vmDo(2);

              OutputDebugValues( "Done DllMain\n" );

              // Safety!
              hb_vmRequestReset();
              hb_itemClear( hb_stackReturnItem() );
           }

           break;
        }

        case DLL_PROCESS_DETACH:
           OutputDebugValues( "Detach\n" );

           if( s_pSym_DllMain )
           {
              hb_vmPushSymbol( s_pSym_DllMain );
              hb_vmPushNil();
              hb_vmPushLong( (LONG) hInstance );
              hb_vmPushLong( (LONG) fdwReason );
              hb_vmDo(2);

              // Safety!
              hb_vmRequestReset();
              hb_itemClear( hb_stackReturnItem() ); // NOTE: was commented!!!
           }

           // *** !!! REVIEW: How to have count of shared VMs???
           if( s_bQuit )
           {
              OutputDebugValues( "Quit\n" );

              //assert( 0 );

              __try
              {
                 hb_vmQuit();
              }
              __except( EXCEPTION_EXECUTE_HANDLER )
              {
                 OutputDebugString( "EXCEPTION Caught!\n" );
              }
           }

           OutputDebugValues( "Should unload now..\n" );
           break;

        default:
           if( s_pSym_DllMain )
           {
              hb_vmPushSymbol( s_pSym_DllMain );
              hb_vmPushNil();
              hb_vmPushLong( (LONG) fdwReason );
              hb_vmDo(1);

              // Safety!
              hb_vmRequestReset();
              hb_itemClear( hb_stackReturnItem() );
           }

           OutputDebugString( "Default case in DllMain!\n" );
     }

     return TRUE;
  }

  DLLEXP STDAPI DllUnregisterServer( void )
  {
     HRESULT hr = S_OK;
     int i, nEntries = sizeof(g_RegTable)/sizeof(*g_RegTable);

     for( i = nEntries - 1; i >= 0; i-- )
     {
        const char * pszKeyName = g_RegTable[i][0];
        long err = RegDeleteKeyA( HKEY_CLASSES_ROOT, pszKeyName );

        if( err != ERROR_SUCCESS )
        {
           hr = S_FALSE;
        }
     }

     return hr;
  }

  DLLEXP STDAPI DllRegisterServer( void )
  {
     HRESULT hr = S_OK;
     int i, nEntries = sizeof( g_RegTable ) / sizeof( *g_RegTable );
     char szFileName[MAX_PATH];
     HKEY hkey;
     long err;

     GetModuleFileName( g_hDll, szFileName, MAX_PATH );

     OutputDebugValues( "Module: '%s'", szFileName );

     //register entries from the table
     for( i = 0; SUCCEEDED(hr) && i < nEntries; i++ )
     {
        const char *pszName = g_RegTable[i][0];
        const char *pszValueName = g_RegTable[i][1];
        const char *pszValue = g_RegTable[i][2];

        //Map rogue values to module file name
        if( pszValue == (const char*) -1 )
        {
           pszValue = szFileName;
        }

        //Create the key
        err = RegCreateKeyA( HKEY_CLASSES_ROOT, pszName, &hkey);

        //Set the value
        if( err == ERROR_SUCCESS )
        {
           err = RegSetValueExA( hkey, pszValueName, 0, REG_SZ, (const BYTE*) pszValue, ( strlen( pszValue ) + 1 ) );
           RegCloseKey( hkey );
        }

        //if cannot add key or value, back out and fail
        if( err != ERROR_SUCCESS )
        {
           OutputDebugValues( "RegSvr Failed: '%s' '%s' '%s'", pszName, pszValueName, pszValue );
           DllUnregisterServer();
           hr = SELFREG_E_CLASS;
        }
     }

     return hr;
  }

#endif

#endif

static void xHB_OleServerInit( void * cargo )
{
   if( g_hDll == 0 )
   {
      OutputDebugValues( "Init: %s(%i)\n", __FILE__, __LINE__ );

      s_pSym_TOleAuto       = hb_dynsymFind( "TOLEAUTO" );
      s_pSym_New            = hb_dynsymFind( "NEW" );
      s_pSym_hObj           = hb_dynsymFind( "HOBJ" );
      //s_pSym_InvokeEvent    = hb_dynsymGetCase( "INVOKEEVENT" );

      s_pSym_VTWrapper      = hb_dynsymFind( "VTWRAPPER" );
      s_pSym_VTArrayWrapper = hb_dynsymFind( "VTARRAYWRAPPER" );
      s_pSym_vt             = hb_dynsymGetCase( "VT" );
      s_pSym_Value          = hb_dynsymFind( "VALUE" );

      #if 0
         s_pSym_QueryInterface = hb_dynsymGetCase( "QUERYINTERFACE" );
      #endif
   }
}

HB_FUNC( NAMEFROMDISPATCH )
{
   PHB_ITEM pClassName = NameFromDispatch( (IDispatch *) hb_parnl(1) );

   if( pClassName )
   {
      hb_itemRelease( hb_itemReturnForward( pClassName ) );
      pClassName = NULL;
   }
}

HB_FUNC( GETREF )
{
   IDispatch *pDispatch = OleWrapFunction( hb_parcx( 1 ) );

   //TraceLog( NULL, "Dispatch: %p for %s\n", pDispatch, hb_parcx(1) );

   if( pDispatch )
   {
      PHB_ITEM pOleAuto = hb_itemNew( NULL );

      hb_vmPushSymbol( s_pSym_TOleAuto->pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      hb_itemForwardValue( pOleAuto, hb_stackReturnItem() );

      // Safety!
      hb_vmRequestReset();
      hb_itemClear( hb_stackReturnItem() );

      if( pOleAuto->type )
      {
         //TOleAuto():New( nDispatch )
         hb_vmPushSymbol( s_pSym_New->pSymbol );
         hb_itemPushForward( pOleAuto );
         hb_vmPushLong( ( LONG ) pDispatch );
         hb_vmSend( 1 );

         hb_itemRelease( pOleAuto );
         pOleAuto = NULL;

         // Safety!
         hb_vmRequestReset();
         hb_itemClear( hb_stackReturnItem() );

         // AddRefed in :New( hObj )
         pDispatch->lpVtbl->Release( pDispatch );
         pDispatch = NULL;
         return;
      }
   }

   hb_ret();
}

//-------------------------------------------------------------------------------------------------
HB_FUNC( OUTPUTDEBUGSTRING )
{
   OutputDebugString( hb_parc(1) );
}

HB_CALL_ON_STARTUP_BEGIN( _xHB_OleServer_init_ )
   hb_vmAtInit( xHB_OleServerInit, NULL );
HB_CALL_ON_STARTUP_END( _xHB_OleServer_init_ )

#if defined(HB_PRAGMA_STARTUP)
#  pragma startup _xHB_OleServer_init_
#elif defined(HB_MSC_STARTUP)
#  if _MSC_VER >= 1010
#     pragma data_seg( ".CRT$XIY" )
#     pragma comment( linker, "/Merge:.CRT=.data" )
#  else
#     pragma data_seg( "XIY" )
#  endif
   static HB_$INITSYM hb_vm_auto_xHB_OleServer_init_ = _xHB_OleServer_init_;
#  pragma data_seg()
#endif
