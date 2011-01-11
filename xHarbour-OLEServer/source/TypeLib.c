#pragma comment( lib, "ole32.lib" )
#pragma comment( lib, "oleAut32.lib" )
#pragma comment( lib, "AdvApi32.lib" )

//#define _DEBUG
//#undef _DEBUG

//#define UNICODE
//#define _UNICODE

#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#define NONAMELESSUNION

#include <tchar.h>
#include <stdio.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbvmpub.h"

#include <windows.h>
#include <ole2.h>
#include <winreg.h>
#include <ocidl.h>


#if defined( _MSC_VER ) && ! defined( __POCC__ )
   #define snprintf _snprintf
#endif

#if defined( _DEBUG )
    static char sBuffer[8192];

    void OutputDebugValues( const char *sFormat, ... )
    {
        va_list ap;

        sBuffer[0] = '\0';

        va_start( ap, sFormat );

        #ifdef __XCC__
           vsnprintf( sBuffer, 8192, sFormat, ap );
        #else
           _vsnprintf( sBuffer, 8192, sFormat, ap );
        #endif

        va_end( ap );

        OutputDebugString( sBuffer );
    }
#else
   #define OutputDebugValues {}(void)
#endif

HB_EXTERN_BEGIN

   extern HB_EXPORT BSTR hb_oleAnsiToSysString( const char *cString );
   extern HB_EXPORT LPSTR hb_oleWideToAnsi( BSTR wString );
   extern HB_ITEM G_SCANMEMBERID, G_MEMBERID, G_SCANINTERFACEGUID, G_INTERFACEGUID;

HB_EXTERN_END

#define CASE_STRING( x ) case x: return #x;
#define FLAG_STRING( x ) if( typeflags & x ){ strcat( s, #x " " ); }

LPTYPELIB TypeLibFromID( BSTR bstrID, IID *pClassID );
LPTYPELIB TypeLibFromInstance( IDispatch *pDisp, IUnknown *pUnk, BSTR *pbstrProgID, IID *pClassID );
HRESULT Unknown_GetTypeInfo( IUnknown *pUnk, ITypeInfo** ppTypeInfo );

PHB_ITEM ProcessTypeLib( LPTYPELIB pITypeLib, IID *pClassID, BSTR bstrObject );
void ProcessTypeInfo( LPTYPEINFO pITypeInfo, PHB_ITEM pTypeLib, IID *pClassID, BSTR bstrObject );
void ProcessInterface( LPTYPEINFO pITypeInfo, int iImplTypeFlags, PHB_ITEM pObject, PHB_ITEM pTypeLib );
PHB_ITEM ProcessType( LPTYPEINFO pITypeInfo, LPTYPEATTR pTypeAttr, int iImplTypeFlags, PHB_ITEM pTypeLib );
PHB_ITEM ProcessVar( ITypeInfo * pITypeInfo, VARDESC *pVarDesc, char **psDesc, PHB_ITEM pTypeLib );

char * GetTypeFlagName( unsigned short typeflags );
const char * GetTypeKindName( TYPEKIND typekind );
const char * GetInvokeKindName( INVOKEKIND invkind );
const char * GetVTName( VARTYPE vt );
char * GetTypeDesc( LPTYPEINFO pITypeInfo, TYPEDESC *ptdesc, PHB_ITEM pType, PHB_ITEM pObject, PHB_ITEM pTypeLib );

HRESULT RegEnumKeyFind( HKEY hKey, BSTR bstrFind, BSTR bstrValue, LONG *lLen );

static PHB_DYNS s_pSym_TypeLib           = NULL;
static PHB_DYNS s_pSym_ObjectTypeInfo    = NULL;
static PHB_DYNS s_pSym_InterfaceTypeInfo = NULL;
static PHB_DYNS s_pSym_PropertyTypeInfo  = NULL;
static PHB_DYNS s_pSym_MethodTypeInfo    = NULL;
static PHB_DYNS s_pSym_ArgumentTypeInfo  = NULL;
static PHB_DYNS s_pSym_EnumTypeInfo      = NULL;
static PHB_DYNS s_pSym_ConstantTypeInfo  = NULL;

static PHB_DYNS s_pSym_Name              = NULL;
static PHB_DYNS s_pSym_ProgID            = NULL;
static PHB_DYNS s_pSym_Flags             = NULL;
static PHB_DYNS s_pSym_HelpFile          = NULL;
static PHB_DYNS s_pSym_HelpString        = NULL;
static PHB_DYNS s_pSym_MemberID          = NULL;
static PHB_DYNS s_pSym_ReadOnly          = NULL;
static PHB_DYNS s_pSym_ByRef             = NULL;
static PHB_DYNS s_pSym_Optional          = NULL;
static PHB_DYNS s_pSym_VtblOffset        = NULL;
static PHB_DYNS s_pSym_TypeDesc          = NULL;
static PHB_DYNS s_pSym_VT                = NULL;
static PHB_DYNS s_pSym_Interface         = NULL;
static PHB_DYNS s_pSym_GUID              = NULL;
static PHB_DYNS s_pSym_IID               = NULL;
static PHB_DYNS s_pSym_MajorVer          = NULL;
static PHB_DYNS s_pSym_MinorVer          = NULL;

static PHB_DYNS s_pSym_Objects           = NULL;
static PHB_DYNS s_pSym_Interfaces        = NULL;
static PHB_DYNS s_pSym_Enumerations      = NULL;
static PHB_DYNS s_pSym_Constants         = NULL;
static PHB_DYNS s_pSym_Value             = NULL;
static PHB_DYNS s_pSym_Events            = NULL;
static PHB_DYNS s_pSym_Methods           = NULL;
static PHB_DYNS s_pSym_Arguments         = NULL;
static PHB_DYNS s_pSym_Properties        = NULL;

HB_FUNC( LOADTYPELIB )
{
   BSTR bstrID = NULL;
   LPTYPELIB pITypeLib = NULL;
   IID ClassID;
   PHB_ITEM pServer;
   BSTR bstrObject = NULL;

   if( s_pSym_TypeLib == NULL )
   {
      s_pSym_TypeLib           = hb_dynsymFind( "TYPELIB" );
      s_pSym_ObjectTypeInfo    = hb_dynsymFind( "OBJECTTYPEINFO" );
      s_pSym_InterfaceTypeInfo = hb_dynsymFind( "INTERFACETYPEINFO" );
      s_pSym_PropertyTypeInfo  = hb_dynsymFind( "PROPERTYTYPEINFO" );
      s_pSym_MethodTypeInfo    = hb_dynsymFind( "METHODTYPEINFO" );
      s_pSym_ArgumentTypeInfo  = hb_dynsymFind( "ARGUMENTTYPEINFO" );
      s_pSym_EnumTypeInfo      = hb_dynsymFind( "ENUMTYPEINFO" );
      s_pSym_ConstantTypeInfo  = hb_dynsymFind( "CONSTANTTYPEINFO" );

      s_pSym_Name              = hb_dynsymGetCase( "_NAME" );
      s_pSym_ProgID            = hb_dynsymGetCase( "_PROGID" );
      s_pSym_Flags             = hb_dynsymGetCase( "FLAGS" );
      s_pSym_HelpFile          = hb_dynsymGetCase( "_HELPFILE" );
      s_pSym_HelpString        = hb_dynsymGetCase( "_HELPSTRING" );
      s_pSym_MemberID          = hb_dynsymGetCase( "_MEMBERID" );
      s_pSym_ReadOnly          = hb_dynsymGetCase( "_READONLY" );
      s_pSym_ByRef             = hb_dynsymGetCase( "_BYREF" );
      s_pSym_Optional          = hb_dynsymGetCase( "_OPTIONAL" );
      s_pSym_VtblOffset        = hb_dynsymGetCase( "_VTBLOFFSET" );
      s_pSym_TypeDesc          = hb_dynsymGetCase( "_TYPEDESC" );
      s_pSym_VT                = hb_dynsymGetCase( "_VT" );
      s_pSym_Interface         = hb_dynsymGetCase( "_INTERFACE" );
      s_pSym_GUID              = hb_dynsymGetCase( "_GUID" );
      s_pSym_IID               = hb_dynsymGetCase( "_IID" );
      s_pSym_MajorVer          = hb_dynsymGetCase( "_MAJORVER" );
      s_pSym_MinorVer          = hb_dynsymGetCase( "_MINORVER" );

      s_pSym_Arguments         = hb_dynsymGetCase( "_ARGUMENTS" );
      s_pSym_Value             = hb_dynsymGetCase( "_VALUE" );

      s_pSym_Objects           = hb_dynsymGetCase( "OBJECTS" );
      s_pSym_Interfaces        = hb_dynsymGetCase( "INTERFACES" );
      s_pSym_Enumerations      = hb_dynsymGetCase( "ENUMERATIONS" );
      s_pSym_Constants         = hb_dynsymGetCase( "CONSTANTS" );
      s_pSym_Events            = hb_dynsymGetCase( "EVENTS" );
      s_pSym_Methods           = hb_dynsymGetCase( "METHODS" );
      s_pSym_Properties        = hb_dynsymGetCase( "PROPERTIES" );
   }

   pServer = hb_param( 1, HB_IT_ANY );

   //TraceLog( NULL, "Type: %i\n", pServer->type );

   if( HB_IS_STRING( pServer ) )
   {
      bstrID = hb_oleAnsiToSysString( hb_itemGetCPtr( pServer ) );

      pITypeLib = TypeLibFromID( bstrID, &ClassID );
   }
   else if( HB_IS_NUMBER( pServer ) )
   {
      pITypeLib = TypeLibFromInstance( (IDispatch *) hb_itemGetNL( pServer ), NULL, &bstrID, &ClassID );

      if( hb_param( 2, HB_IT_STRING ) )
      {
         bstrObject = hb_oleAnsiToSysString( hb_parcx(2) );
      }
      else
      {
         if( bstrID && hb_parl(2) == FALSE )
         {
            bstrObject = SysAllocString( bstrID );
         }
      }

      //OutputDebugValues( "%ls, %ls\n", bstrID, bstrObject );

      if( bstrID )
      {
         hb_storclenAdopt( hb_oleWideToAnsi( bstrID ), wcslen( bstrID ), 3 );

         CoTaskMemFree( bstrID );
         bstrID = NULL;
      }
      else
      {
         TraceLog( NULL, "Unexpected failure in line: %i Module: '%s'\n", __LINE__, __FILE__ );
      }
   }

   if( pITypeLib )
   {
      //TraceLog( NULL, "%f\n", hb_dateSeconds() );
      hb_itemRelease( hb_itemReturnForward( ProcessTypeLib( pITypeLib, bstrObject == NULL && hb_parl(2) == FALSE ? &ClassID : NULL, bstrObject ) ) );
      //TraceLog( NULL, "%f\n", hb_dateSeconds() );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 5001, "Could not load TypeLib", "LoadTypeLib", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }

   SysFreeString( bstrID );
   SysFreeString( bstrObject );
}

LPTYPELIB TypeLibFromInstance( IDispatch *pDisp, IUnknown *pUnk, BSTR *pbstrProgID, IID *pClassID )
{
   LPTYPELIB pITypeLib = NULL;
   ITypeInfo* pITypeInfo = NULL;
   BOOL bReleaseDisp = FALSE, bReleaseUnk = FALSE;
   HRESULT hr;

   if( pUnk && pDisp == NULL )
   {
      pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, (void **) &pDisp );
      bReleaseUnk = TRUE;
   }
   else if( pDisp && pUnk == NULL )
   {
      pDisp->lpVtbl->QueryInterface( pDisp, &IID_IUnknown, (void **) &pUnk );
      bReleaseDisp = TRUE;
   }

   if( pDisp )
   {
      hr = pDisp->lpVtbl->GetTypeInfo( pDisp, 0, LOCALE_SYSTEM_DEFAULT, &pITypeInfo );

      if( FAILED( hr ) || pITypeInfo == NULL )
      {
         TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
      }
   }

   if( pITypeInfo == NULL && pUnk )
   {
      Unknown_GetTypeInfo( pUnk, &pITypeInfo );
   }

   //TraceLog( NULL, "TypeInfo: %p\n", pITypeInfo );

   if( pITypeInfo )
   {
      unsigned int uiIndex;
      TYPEATTR *pTypeAttr = NULL;

      if( pbstrProgID )
      {
         hr = pITypeInfo->lpVtbl->GetTypeAttr( pITypeInfo, &pTypeAttr );

         if( SUCCEEDED( hr ) )
         {
            //TraceLog( NULL, "Kind: %i\n", pTypeAttr->typekind );
            if( pClassID )
            {
               memcpy( pClassID, &pTypeAttr->guid, sizeof(IID) ) ;
            }

            // bstrProgID is an OUT argument.
            *pbstrProgID = (BSTR) NULL;
            hr = ProgIDFromCLSID( &pTypeAttr->guid, pbstrProgID );

            if( FAILED( hr ) )
            {
               *pbstrProgID = (BSTR) NULL;
               hr = StringFromCLSID( &pTypeAttr->guid, pbstrProgID );

               if( FAILED( hr ) )
               {
                  TraceLog( NULL, "Unexpected failure %p in line: %i Module: '%s'\n", hr, __LINE__, __FILE__ );
               }
            }

            pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
         }
         else
         {
            TraceLog( NULL, "Unexpected failure %p in line: %i Module: '%s'\n", hr, __LINE__, __FILE__ );
         }
      }
      else
      {
         TraceLog( NULL, "Missing pbstrProgID in line: %i Module: '%s'\n", __LINE__, __FILE__ );
      }

      if( SUCCEEDED( pITypeInfo->lpVtbl->GetContainingTypeLib( pITypeInfo, &pITypeLib, &uiIndex ) ) )
      {
         TLIBATTR *pTLibAttr = NULL;

         //OutputDebugValues( "%i\n", uiIndex );

         /*
          * For some very ODD reason, TypeLib loaded from FILE, is MUCH faster then when provided by the control
          * so we are going to load new copy of the typelib, by means of LoadRegTypeLib()
          */
         if( SUCCEEDED( pITypeLib->lpVtbl->GetLibAttr( pITypeLib, &pTLibAttr ) ) )
         {
            LPTYPELIB pITypeLibFromReg = NULL;

            hr = LoadRegTypeLib( &pTLibAttr->guid, pTLibAttr->wMajorVerNum, pTLibAttr->wMinorVerNum, LOCALE_SYSTEM_DEFAULT, &pITypeLibFromReg );

            pITypeLib->lpVtbl->ReleaseTLibAttr( pITypeLib, pTLibAttr );

            if( SUCCEEDED( hr ) )
            {
               pITypeLib->lpVtbl->Release( pITypeLib );
               pITypeLib = pITypeLibFromReg;

               #ifdef _DEBUG
                  OutputDebugString( "Defered to typelib from reg.\n" );
               #endif
            }
         }
      }
      else
      {
         TraceLog( NULL, "Unexpected failure in line: %i Module: '%s'\n", __LINE__, __FILE__ );
      }

      pITypeInfo->lpVtbl->Release( pITypeInfo );
   }

   if( bReleaseUnk )
   {
      pUnk->lpVtbl->Release( pUnk );
   }

   if( bReleaseDisp )
   {
      pDisp->lpVtbl->Release( pDisp );
   }

   return pITypeLib;
}

LPTYPELIB TypeLibFromID( BSTR bstrID, IID *pClassID )
{
   LPTYPELIB pITypeLib = NULL;
   ITypeInfo* pITypeInfo = NULL;
   IUnknown *pUnk = NULL;
   IDispatch *pDisp = NULL;
   BSTR bstrClassID = NULL;
   HRESULT hr;
   HKEY hCLSID, hClass, hTypeLib;
   LONG lRet, lLen;
   IID ClassID;
   OLECHAR bstrTlb[ MAX_PATH + 1 ];

   hr = LoadTypeLibEx( bstrID, REGKIND_NONE, &pITypeLib );

   if( SUCCEEDED( hr ) )
   {
      return pITypeLib;
   }

   if( bstrID[0] != L'{' )
   {
      hr = CLSIDFromProgID( bstrID, &ClassID );

      if( FAILED( hr ) )
      {
         //hb_errRT_BASE( EG_ARG, 5002, "Could not retrieve CLSID", "TypeLibFromID", 1, hb_paramError( 1 ) );
         OutputDebugString( "5002\n" );
         return NULL;
      }
   }
   else
   {
      hr = CLSIDFromString( bstrID, &ClassID );

      if( FAILED( hr ) )
      {
         //hb_errRT_BASE( EG_ARG, 5003, "Could not retrieve literal CLSID", "TypeLibFromID", 1, hb_paramError( 1 ) );
         OutputDebugString( "5003a\n" );
         return NULL;
      }
   }

   if( pClassID )
   {
      memcpy( pClassID, &ClassID, sizeof(IID) ) ;
   }

   // scan registry
   lRet = RegOpenKeyEx( HKEY_CLASSES_ROOT, _T( "CLSID" ), 0, KEY_READ, &hCLSID );

   if( lRet != ERROR_SUCCESS )
   {
      //hb_errRT_BASE( EG_ARG, 5004, "Could not retrieve CLSID registry", "TypeLibFromID", 1, hb_paramError( 1 ) );
      OutputDebugString( "5004\n" );

      return NULL;
   }

   if( bstrID[0] != L'{' )
   {
      hr = StringFromCLSID( &ClassID, &bstrClassID );

      if( FAILED( hr ) )
      {
         //hb_errRT_BASE( EG_ARG, 5003, "Could not retrieve literal CLSID", "TypeLibFromID", 1, hb_paramError( 1 ) );
         OutputDebugString( "5003\n" );
         return NULL;
      }
   }
   else
   {
      bstrClassID = bstrID;
   }

   lRet = RegOpenKeyExW( hCLSID, bstrClassID, 0, KEY_READ, &hClass );

   if( bstrClassID != bstrID )
   {
      CoTaskMemFree( (void *) bstrClassID );
   }

   if( lRet != ERROR_SUCCESS )
   {
      //hb_errRT_BASE( EG_ARG, 5005, "Could not retrieve CLSID registry entry", "TypeLibFromID", 1, hb_paramError( 1 ) );
      OutputDebugString( "5005\n" );

      RegCloseKey( hCLSID );
      return NULL;
   }

   lRet = RegOpenKeyEx( hClass, _T( "TypeLib" ), 0, KEY_READ, &hTypeLib );

   if( lRet != ERROR_SUCCESS )
   {
      if( ( lRet = RegOpenKeyEx( hClass, _T( "LocalServer32" ), 0, KEY_READ, &hTypeLib ) ) != ERROR_SUCCESS )
      {
         if( ( lRet = RegOpenKeyEx( hClass, _T( "InprocServer32" ), 0, KEY_READ, &hTypeLib ) ) != ERROR_SUCCESS )
         {
            //hb_errRT_BASE( EG_ARG, 5006, "Could not retrieve InprocServer32 registry entry", "TypeLibFromID", 1, hb_paramError( 1 ) );
            OutputDebugString( "5006\n" );

            RegCloseKey( hClass );
            RegCloseKey( hCLSID );
            return NULL;
         }
      }
   }

   lLen = sizeof( bstrTlb );
   lRet = RegQueryValueW( hTypeLib, NULL, bstrTlb, &lLen );

   RegCloseKey( hTypeLib );
   RegCloseKey( hClass );
   RegCloseKey( hCLSID );

   if( lRet != ERROR_SUCCESS )
   {
      //hb_errRT_BASE( EG_ARG, 5007, "Could not retrieve registry value", "TypeLibFromID", 1, hb_paramError( 1 ) );
      OutputDebugString( "5007\n" );
      return NULL;
   }

   // Maybe a GUID
   if( bstrTlb[0] == L'{' )
   {
      HKEY hTlb;
      DWORD dwIndex = 0;

      lRet = RegOpenKeyEx( HKEY_CLASSES_ROOT, _T( "TypeLib" ), 0, KEY_READ, &hTypeLib );

      if( lRet != ERROR_SUCCESS )
      {
         //hb_errRT_BASE( EG_ARG, 5008, "Could not retrieve TypeLib registry", "TypeLibFromID", 1, hb_paramError( 1 ) );
         OutputDebugString( "5008\n" );
         return NULL;
      }

      lRet = RegOpenKeyExW( hTypeLib, bstrTlb, 0, KEY_ENUMERATE_SUB_KEYS, &hTlb );

      if( lRet != ERROR_SUCCESS )
      {
         //hb_errRT_BASE( EG_ARG, 5009, "Could not retrieve TypeLib registry entry", "TypeLibFromID", 1, hb_paramError( 1 ) );
         OutputDebugString( "5009\n" );
         RegCloseKey( hTypeLib );
         return NULL;
      }

      lLen = sizeof( bstrTlb );
      hr = RegEnumKeyFind( hTlb, L"win32", bstrTlb, &lLen );

      RegCloseKey( hTlb );
      RegCloseKey( hTypeLib );

      if( SUCCEEDED( hr ) )
      {
         //printf( "TypeLib: %ls\n", bstrTlb );
         hr = LoadTypeLibEx( bstrTlb, REGKIND_NONE, &pITypeLib );

         if( SUCCEEDED( hr ) )
         {
            return pITypeLib;
         }
      }
   }
   else // Or the executable name (possibly followed by switches such as /Automation)
   {
      BSTR bstrSwitch = wcschr( bstrTlb, L'/' );

      if( bstrSwitch )
      {
         bstrTlb[ bstrSwitch - bstrTlb ] = 0;
      }

      OutputDebugValues( "%s\n", bstrTlb );

      hr = LoadTypeLibEx( bstrTlb, REGKIND_NONE, &pITypeLib );

      if( SUCCEEDED( hr ) )
      {
         return pITypeLib;
      }
   }

   OutputDebugString( "Last resort - Instanciate\n" );

   // Let's instanciate and request the control itself to provide it's type info.
   if( SUCCEEDED( GetActiveObject( (REFCLSID) &ClassID, NULL, &pUnk ) ) ||
       SUCCEEDED( CoCreateInstance( (REFCLSID) &ClassID, NULL, CLSCTX_SERVER, &IID_IDispatch, (void **) &pDisp ) ) )
   {
      pITypeLib = TypeLibFromInstance( pDisp, pUnk, NULL, pClassID );

      if( pUnk )
      {
         pUnk->lpVtbl->Release( pUnk );
      }

      if( pDisp )
      {
         pDisp->lpVtbl->Release( pDisp );
      }
   }

   return pITypeLib;
}

HRESULT RegEnumKeyFind( HKEY hKey, BSTR bstrFind, BSTR bstrValue, LONG *lLen )
{
   DWORD dwIndex = 0;
   HKEY hSubKey;

   while( RegEnumKeyW( hKey, dwIndex++, bstrValue, MAX_PATH ) != ERROR_NO_MORE_ITEMS )
   {
      if( wcscmp( bstrValue, bstrFind  ) == 0 )
      {
         if( RegOpenKeyExW( hKey, bstrValue, 0, KEY_READ, &hSubKey ) == ERROR_SUCCESS )
         {
            LONG lRet = RegQueryValueW( hSubKey, NULL, bstrValue, lLen );

            RegCloseKey( hSubKey );

            if( lRet == ERROR_SUCCESS )
            {
               return S_OK;
            }
            else
            {
               return E_FAIL;
            }
         }
      }
      else
      {
         if( RegOpenKeyExW( hKey, bstrValue, 0, KEY_ENUMERATE_SUB_KEYS, &hSubKey ) == ERROR_SUCCESS )
         {
             HRESULT hr = RegEnumKeyFind( hSubKey, bstrFind, bstrValue, lLen );

             RegCloseKey( hSubKey );

             return hr;
         }
      }
   }

   return E_FAIL;
}

PHB_ITEM ProcessTypeLib( LPTYPELIB pITypeLib, IID *pClassID, BSTR bstrObject )
{
   TLIBATTR *pTLibAttr = NULL;
   UINT i, iCount;
   PHB_ITEM pTypeLib;
   PHB_ITEM pTemp = hb_itemNew( NULL );
   BSTR bstrTypeLibName = NULL, bstrDoc = NULL, bstrHelpFile = NULL;

   //pTypeLib = hb_itemDoC( "TYPELIB", 0 );
   hb_vmPushSymbol( s_pSym_TypeLib->pSymbol );
   hb_vmPushNil();
   hb_vmDo(0);
   pTypeLib = hb_itemNew( hb_stackReturnItem() );

   if( SUCCEEDED( pITypeLib->lpVtbl->GetDocumentation( pITypeLib, MEMBERID_NIL, &bstrTypeLibName, &bstrDoc, NULL, &bstrHelpFile ) ) )
   {
      if( bstrTypeLibName )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrTypeLibName ), wcslen( bstrTypeLibName ) );

         SysFreeString( bstrTypeLibName );

         //hb_objSendMsg( pObject, "_NAME", 1, pTemp );
         hb_vmPushSymbol( s_pSym_Name->pSymbol );
         hb_vmPush( pTypeLib );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrDoc )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );

         SysFreeString( bstrDoc );

         //hb_objSendMsg( pObject, "_HELPSTRING", 1, pTemp );
         hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
         hb_vmPush( pTypeLib );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrHelpFile )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrHelpFile ), wcslen( bstrHelpFile ) );

         SysFreeString( bstrHelpFile );

         //hb_objSendMsg( pObject, "_HELPFILE", 1, pTemp );
         hb_vmPushSymbol( s_pSym_HelpFile->pSymbol );
         hb_vmPush( pTypeLib );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }
   }

   if( SUCCEEDED( pITypeLib->lpVtbl->GetLibAttr( pITypeLib, &pTLibAttr ) ) )
   {
      BSTR bstrIID = NULL;

      StringFromCLSID( &pTLibAttr->guid, &bstrIID );

      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );

      CoTaskMemFree( bstrIID );

      //hb_objSendMsg( pObject, "_GUID", 1, pTemp );
      hb_vmPushSymbol( s_pSym_GUID->pSymbol );
      hb_vmPush( pTypeLib );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      hb_itemPutNL( pTemp, pTLibAttr->wMajorVerNum );
      //hb_objSendMsg( pObject, "_MAJORVER", 1, pTemp );
      hb_vmPushSymbol( s_pSym_GUID->pSymbol );
      hb_vmPush( pTypeLib );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      hb_itemPutNL( pTemp, pTLibAttr->wMinorVerNum );
      //hb_objSendMsg( pObject, "_MINORVER", 1, pTemp );
      hb_vmPushSymbol( s_pSym_GUID->pSymbol );
      hb_vmPush( pTypeLib );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      pITypeLib->lpVtbl->ReleaseTLibAttr( pITypeLib, pTLibAttr );
   }

   hb_itemRelease( pTemp );
   pTemp = NULL;

   iCount = pITypeLib->lpVtbl->GetTypeInfoCount( pITypeLib );

   for( i = 0; i < iCount; i++ )
   {
      LPTYPEINFO pITypeInfo = NULL;

      HRESULT hr = pITypeLib->lpVtbl->GetTypeInfo( pITypeLib, i, &pITypeInfo );

      if( SUCCEEDED( hr ) )
      {
         //OutputDebugValues( "%i\n", i );
         ProcessTypeInfo( pITypeInfo, pTypeLib, pClassID, bstrObject );

         pITypeInfo->lpVtbl->Release( pITypeInfo );
         pITypeInfo = NULL;
      }
      else
      {
         OutputDebugString( "Failed to retrieve typeinfo\n" );
      }
   }

   pITypeLib->lpVtbl->Release( pITypeLib );

   return pTypeLib;
}

void ProcessTypeInfo( LPTYPEINFO pITypeInfo, PHB_ITEM pTypeLib, IID* pClassID, BSTR bstrObject )
{
   HRESULT hr;
   TYPEATTR *pTypeAttr = NULL;

   hr = pITypeInfo->lpVtbl->GetTypeAttr( pITypeInfo, &pTypeAttr );

   if( FAILED( hr ) )
   {
      hb_errRT_BASE( EG_ARG, 5010, "Could not retrieve TypAttr", "ProcessTypeInfo", 1, hb_paramError( 1 ) );
      return;
   }

   if( pTypeAttr->typekind == TKIND_COCLASS )
   {
      unsigned short i;
      int iImplTypeFlags = 0;
      BSTR bstrProgID = NULL;
      char *sTypeFlags = NULL;
      BSTR bstrTypeInfoName = NULL, bstrDoc = NULL, bstrIID = NULL, bstrHelpFile = NULL;
      HREFTYPE href;
      ITypeInfo *pInterfaceTypeInfo = NULL;
      PHB_ITEM pObject = NULL;
      PHB_ITEM pTemp = NULL;

      if( pClassID && ! IsEqualGUID( pClassID, &pTypeAttr->guid ) )
      {
         OutputDebugValues( "Skip COClass\n" );

         pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
         return;
      }

      hr = pITypeInfo->lpVtbl->GetDocumentation( pITypeInfo, MEMBERID_NIL, &bstrTypeInfoName, &bstrDoc, NULL, &bstrHelpFile );

      if( FAILED( hr ) )
      {
         hb_errRT_BASE( EG_ARG, 5011, "Could not retrieve Documentation", "ProcessTypeInfo", 1, hb_paramError( 1 ) );

         pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );

         SysFreeString( bstrTypeInfoName );
         SysFreeString( bstrDoc );
         SysFreeString( bstrHelpFile );

         return;
      }

      ProgIDFromCLSID( &pTypeAttr->guid, &bstrProgID );
      StringFromCLSID( &pTypeAttr->guid, &bstrIID );

      if( bstrObject )
      {
         if( ( bstrTypeInfoName == NULL || _wcsicmp( bstrObject, bstrTypeInfoName ) ) &&
             ( bstrProgID == NULL || _wcsnicmp( bstrObject, bstrProgID, wcslen( bstrObject ) ) ) &&
             ( bstrIID == NULL || _wcsicmp( bstrObject, bstrIID ) ) )
         {
            OutputDebugValues( "Skip: %ls, %ls, %ls - Want: %ls\n", bstrTypeInfoName, bstrProgID, bstrIID, bstrObject );

            pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );

            SysFreeString( bstrTypeInfoName );
            SysFreeString( bstrDoc );
            SysFreeString( bstrHelpFile );

            CoTaskMemFree( bstrProgID );
            CoTaskMemFree( bstrIID );

            return;
         }
      }

      sTypeFlags = GetTypeFlagName( pTypeAttr->wTypeFlags );
      OutputDebugValues( "Process OBJECT: '%ls' ProgID: '%ls' GUID: %ls Flags: '%s'\n", bstrTypeInfoName, bstrProgID, bstrIID, sTypeFlags );
      hb_xfree( sTypeFlags );
      sTypeFlags = NULL;

      //pObject = hb_itemDoC( "OBJECTTYPEINFO", 0 );
      hb_vmPushSymbol( s_pSym_ObjectTypeInfo->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
      pObject = hb_itemNew( hb_stackReturnItem() );

      pTemp = hb_itemNew( NULL );

      if( bstrTypeInfoName )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrTypeInfoName ), wcslen( bstrTypeInfoName ) );
         //hb_objSendMsg( pObject, "_NAME", 1, pTemp );
         hb_vmPushSymbol( s_pSym_Name->pSymbol );
         hb_vmPush( pObject );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrProgID )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrProgID ), wcslen( bstrProgID ) );
         //hb_objSendMsg( pObject, "_PROGID", 1, pTemp );
         hb_vmPushSymbol( s_pSym_ProgID->pSymbol );
         hb_vmPush( pObject );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrIID )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );
         //hb_objSendMsg( pObject, "_GUID", 1, pTemp );
         hb_vmPushSymbol( s_pSym_GUID->pSymbol );
         hb_vmPush( pObject );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrDoc )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );
         //hb_objSendMsg( pObject, "_HELPSTRING", 1, pTemp );
         hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
         hb_vmPush( pObject );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrHelpFile )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrHelpFile ), wcslen( bstrHelpFile ) );
         //hb_objSendMsg( pObject, "_HELPFILE", 1, pTemp );
         hb_vmPushSymbol( s_pSym_HelpFile->pSymbol );
         hb_vmPush( pObject );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      hb_itemPutNL( pTemp, pTypeAttr->wTypeFlags );
      //hb_objSendMsg( pObject, "FLAGS", 1, pTemp );
      hb_vmPushSymbol( s_pSym_Flags->pSymbol );
      hb_vmPush( pObject );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      //hb_objSendMsg( pTypeLib, "OBJECTS", 0 );
      hb_vmPushSymbol( s_pSym_Objects->pSymbol );
      hb_vmPush( pTypeLib );
      hb_vmSend(0);

      hb_arrayAdd( hb_stackReturnItem(), pObject );

      hb_itemRelease( pTemp );
      pTemp = NULL;

      CoTaskMemFree( bstrProgID );
      CoTaskMemFree( bstrIID );

      SysFreeString( bstrTypeInfoName );
      SysFreeString( bstrDoc );
      SysFreeString( bstrHelpFile );

      for( i = 0; i < pTypeAttr->cImplTypes; i++ )
      {
         hr = pITypeInfo->lpVtbl->GetImplTypeFlags( pITypeInfo, i, &iImplTypeFlags );

         if( FAILED( hr ) )
         {
             hb_errRT_BASE( EG_ARG, 5011, "Could not retrieve TypeFlags", "ProcessTypeInfo", 1, hb_paramError( 1 ) );
             continue;
         }

         hr = pITypeInfo->lpVtbl->GetRefTypeOfImplType( pITypeInfo, i, &href );
         //OutputDebugValues( "hRef: %i\n", href );

         if( FAILED( hr ) )
         {
             hb_errRT_BASE( EG_ARG, 5012, "Could not retrieve RefType", "ProcessTypeInfo", 1, hb_paramError( 1 ) );
             continue;
         }

         hr = pITypeInfo->lpVtbl->GetRefTypeInfo( pITypeInfo, href, &pInterfaceTypeInfo );

         if( FAILED( hr ) )
         {
             hb_errRT_BASE( EG_ARG, 5013, "Could not retrieve TypeInfo", "ProcessTypeInfo", 1, hb_paramError( 1 ) );
             continue;
         }

         ProcessInterface( pInterfaceTypeInfo, iImplTypeFlags, pObject, pTypeLib );
      }

      hb_itemRelease( pObject );
      pObject = NULL;
   }
   else
   {
      PHB_ITEM pType;

      //printf( "Kind: %s\n", GetTypeKindName( pTypeAttr->typekind ) );

      switch( pTypeAttr->typekind )
      {
          case TKIND_ENUM:
             pType = ProcessType( pITypeInfo, pTypeAttr, 0, pTypeLib );

             //hb_objSendMsg( pTypeLib, "ENUMERATIONS", 0 );
             hb_vmPushSymbol( s_pSym_Enumerations->pSymbol );
             hb_vmPush( pTypeLib );
             hb_vmSend(0);

             hb_arrayAddForward( hb_stackReturnItem(), pType );
             hb_itemRelease( pType );
             pType = NULL;
             break;

          case TKIND_RECORD:
          case TKIND_ALIAS:
          case TKIND_UNION:
          case TKIND_MODULE:
          case TKIND_INTERFACE:
             // TODO!!!
             //TraceLog( NULL, "TODO TKIND: %i\n", pTypeAttr->typekind );
             break;

          case TKIND_DISPATCH:
             // Process through Types used by the CoClass Interfaces!
             //pITypeInfo->lpVtbl->AddRef( pITypeInfo );
             //ProcessInterface( pITypeInfo, 0, pTypeLib, pTypeLib );
             break;

          //case TKIND_COCLASS:
              // Retrived through the CoClass!
              // break;

          default:
             hb_errRT_BASE( EG_ARG, 5014, "Unknown Type", "ProcessTypeInfo", 1, hb_paramError( 1 ) );
      }
   }

   pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
}

void ProcessInterface( LPTYPEINFO pITypeInfo, int iImplTypeFlags, PHB_ITEM pObject, PHB_ITEM pTypeLib )
{
   FUNCDESC * pFuncDesc;
   BSTR bstrInterfaceName = NULL, bstrFuncName = NULL, bstrDoc = NULL;
   HRESULT hr;
   unsigned int i, iFirst;
   TYPEATTR *pTypeAttr = NULL;
   BSTR bstrIID = NULL, bstrProgID = NULL;
   PHB_ITEM pTypeLibInterfaces = NULL, pObjectInterfaces = NULL;
   PHB_ITEM pInterface = NULL, pMethod = NULL;
   PHB_ITEM pTemp = NULL;
   ULONG ulPos;
   PHB_ITEM pProperties = NULL, pMethods = NULL, pEvents = NULL;

   hr = pITypeInfo->lpVtbl->GetTypeAttr( pITypeInfo, &pTypeAttr );

   if( FAILED( hr ) )
   {
      hb_errRT_BASE( EG_ARG, 5015, "Could not retrieve TypeAttr", "ProcessInterface", 1, hb_paramError( 1 ) );

      pITypeInfo->lpVtbl->Release( pITypeInfo );
      return;
   }

   StringFromCLSID( &pTypeAttr->guid, &bstrIID );

   hb_itemPutCPtr( &G_INTERFACEGUID, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );

   //hb_objSendMsg( pTypeLib, "INTERFACES", 0 );
   hb_vmPushSymbol( s_pSym_Interfaces->pSymbol );
   hb_vmPush( pTypeLib );
   hb_vmSend(0);

   pTypeLibInterfaces = hb_itemNew( hb_stackReturnItem() );

   //hb_objSendMsg( pObject, "INTERFACES", 0 );
   hb_vmPushSymbol( s_pSym_Interfaces->pSymbol );
   hb_vmPush( pObject );
   hb_vmSend(0);

   pObjectInterfaces = hb_itemNew( hb_stackReturnItem() );
   pTemp = hb_itemNew( NULL );

   if( ( ulPos = hb_arrayScan( pTypeLibInterfaces, &G_SCANINTERFACEGUID, NULL, NULL, TRUE, FALSE ) ) != 0 )
   {
      OutputDebugValues( "Allready in TypeLib: %ls\n", bstrIID );

      if( hb_arrayScan( pObjectInterfaces, &G_SCANINTERFACEGUID, NULL, NULL, TRUE, FALSE ) == 0 )
      {
         OutputDebugValues( "Recycling: %ls\n", bstrIID );

         pInterface = hb_itemNew( NULL );
         hb_arrayGet( pTypeLibInterfaces, ulPos, pInterface );

         hb_vmPushSymbol( s_pSym_Flags->pSymbol );
         hb_vmPush( pInterface );
         hb_vmSend(0);

         if( hb_parnl(-1) != iImplTypeFlags )
         {
            PHB_ITEM pClonedInterface = hb_arrayClone2( pInterface, NULL );

            OutputDebugValues( "Cloning Interface\n" );

            hb_itemRelease( pInterface );
            pInterface = pClonedInterface;

            hb_itemPutNL( pTemp, iImplTypeFlags );//pTypeAttr->wTypeFlags );
            //hb_objSendMsg( pInterface, "FLAGS", 1, pTemp );
            hb_vmPushSymbol( s_pSym_Flags->pSymbol );
            hb_vmPush( pInterface );
            hb_itemPushForward( pTemp );
            hb_vmSend(1);
         }

         hb_arrayAddForward( pObjectInterfaces, pInterface );
      }
      else
      {
         OutputDebugValues( "Allready processed: %ls\n", bstrIID );
      }

      hb_itemRelease( pInterface );
      hb_itemRelease( pTypeLibInterfaces );
      hb_itemRelease( pObjectInterfaces );
      hb_itemRelease( pTemp );

      pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
      pITypeInfo->lpVtbl->Release( pITypeInfo );
      return;
   }

   pITypeInfo->lpVtbl->GetDocumentation( pITypeInfo, MEMBERID_NIL, &bstrInterfaceName, &bstrDoc, NULL, NULL );

   if( pTypeAttr->cFuncs == 0 && pTypeAttr->cVars == 0 )
   {
      ITypeLib *pITypeLib = NULL;
      #if 0
         unsigned int uiIndex;
      #endif

      #ifdef _DEBUG
         OutputDebugValues( "Empty Interface: %ls", bstrInterfaceName );
      #endif

      #if 0
        if( SUCCEEDED( pITypeInfo->lpVtbl->GetContainingTypeLib( pITypeInfo, &pITypeLib, &uiIndex ) ) )
        {
           IID guid;

           memcpy( &guid, &pTypeAttr->guid, sizeof( IID ) );

           pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
           pITypeInfo->lpVtbl->Release( pITypeInfo );

           hr = pITypeLib->lpVtbl->GetTypeInfoOfGuid( pITypeLib, &guid, &pITypeInfo );
           pITypeLib->lpVtbl->Release( pITypeLib );

           if( FAILED( hr ) )
           {
              hb_errRT_BASE( EG_ARG, 5015, "Could not retrieve global GUID", "ProcessInterface", 1, hb_paramError( 1 ) );

              hb_itemRelease( pTypeLibInterfaces );
              hb_itemRelease( pObjectInterfaces );
              hb_itemRelease( pTemp );

              return;
           }

           hr = pITypeInfo->lpVtbl->GetTypeAttr( pITypeInfo, &pTypeAttr );

           if( FAILED( hr ) )
           {
              hb_errRT_BASE( EG_ARG, 5015, "Could not retrieve TypeAttr", "ProcessInterface", 1, hb_paramError( 1 ) );

              pITypeInfo->lpVtbl->Release( pITypeInfo );

              hb_itemRelease( pTypeLibInterfaces );
              hb_itemRelease( pObjectInterfaces );
              hb_itemRelease( pTemp );

              return;
           }
         #ifdef _DEBUG
           else
           {
              OutputDebugString( "Retrived GLOBAL\n" );
           }
         #endif
        }
      #endif

      hb_itemRelease( pTypeLibInterfaces );
      hb_itemRelease( pObjectInterfaces );
      hb_itemRelease( pTemp );

      return;
   }

   OutputDebugValues( "Processing: %ls GUID: %ls\n", bstrInterfaceName, bstrIID );

   //pInterface = hb_itemDoC( "INTERFACETYPEINFO", 0 );
   hb_vmPushSymbol( s_pSym_InterfaceTypeInfo->pSymbol );
   hb_vmPushNil();
   hb_vmDo(0);
   pInterface = hb_itemNew( hb_stackReturnItem() );

   if( bstrIID )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );

      //hb_objSendMsg( pInterface, "_GUID", 1, pTemp );
      hb_vmPushSymbol( s_pSym_GUID->pSymbol );
      hb_vmPush( pInterface );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   ProgIDFromCLSID( &pTypeAttr->guid, &bstrProgID );

   if( bstrProgID )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrProgID ), wcslen( bstrProgID ) );

      CoTaskMemFree( (void *) bstrProgID );

      //hb_objSendMsg( pInterface, "_PROGID", 1, pTemp );
      hb_vmPushSymbol( s_pSym_ProgID->pSymbol );
      hb_vmPush( pInterface );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   hb_arrayAdd( pTypeLibInterfaces, pInterface );

   hb_arrayAdd( pObjectInterfaces, pInterface );

   if( bstrInterfaceName )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrInterfaceName ), wcslen( bstrInterfaceName ) );
      //hb_objSendMsg( pInterface, "_NAME", 1, pTemp );
      hb_vmPushSymbol( s_pSym_Name->pSymbol );
      hb_vmPush( pInterface );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   if( bstrDoc )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );
      //hb_objSendMsg( pInterface, "_HELPSTRING", 1, pTemp );
      hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
      hb_vmPush( pInterface );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   hb_itemPutNL( pTemp, iImplTypeFlags );//pTypeAttr->wTypeFlags );
   //hb_objSendMsg( pInterface, "FLAGS", 1, pTemp );
   hb_vmPushSymbol( s_pSym_Flags->pSymbol );
   hb_vmPush( pInterface );
   hb_itemPushForward( pTemp );
   hb_vmSend(1);

   hb_itemPutCL( pTemp, (char *) &( pTypeAttr->guid ), sizeof( pTypeAttr->guid ) );
   //hb_objSendMsg( pInterface, "_IID", 1, pTemp );
   hb_vmPushSymbol( s_pSym_IID->pSymbol );
   hb_vmPush( pInterface );
   hb_itemPushForward( pTemp );
   hb_vmSend(1);

   CoTaskMemFree( (void *) bstrIID );

   SysFreeString( bstrInterfaceName );
   SysFreeString( bstrDoc );

   //hb_objSendMsg( pInterface, "PROPERTIES", 0 );
   hb_vmPushSymbol( s_pSym_Properties->pSymbol );
   hb_vmPush( pInterface );
   hb_vmSend(0);

   pProperties = hb_itemNew( hb_stackReturnItem() );

   for( i = 0; i < pTypeAttr->cVars; i++ )
   {
      VARDESC *pVarDesc;
      PHB_ITEM pProperty;

      hr = pITypeInfo->lpVtbl->GetVarDesc( pITypeInfo, i, &pVarDesc );

      if( FAILED( hr ) )
      {
         hb_errRT_BASE( EG_ARG, 5016, "Could not retrieve VarDesc", "ProcessInterface", 1, hb_paramError( 1 ) );
         continue;
      }

      pProperty = ProcessVar( pITypeInfo, pVarDesc, NULL, pTypeLib );
      hb_arrayAddForward( pProperties, pProperty );
      hb_itemRelease( pProperty );
   }

   iFirst = 0;

   #ifdef SKIP_DISP_EVENTS
      if( iImplTypeFlags & IMPLTYPEFLAG_FSOURCE )
      {
         iFirst = 7;
      }
   #endif

   //hb_objSendMsg( pInterface, "METHODS", 0 );
   hb_vmPushSymbol( s_pSym_Methods->pSymbol );
   hb_vmPush( pInterface );
   hb_vmSend(0);

   pMethods = hb_itemNew( hb_stackReturnItem() );

   //hb_objSendMsg( pInterface, "EVENTS", 0 );
   hb_vmPushSymbol( s_pSym_Events->pSymbol );
   hb_vmPush( pInterface );
   hb_vmSend(0);

   pEvents = hb_itemNew( hb_stackReturnItem() );

   for( i = iFirst; i < pTypeAttr->cFuncs; i++ )
   {
      BSTR *paNames = NULL;
      char *sTypeDesc = NULL;
      unsigned int cNames;

      hr = pITypeInfo->lpVtbl->GetFuncDesc( pITypeInfo, i, &pFuncDesc );

      if( FAILED( hr ) )
      {
         hb_errRT_BASE( EG_ARG, 5016, "Could not retrieve FuncDesc", "ProcessInterface", 1, hb_paramError( 1 ) );
         continue;
      }

      hr = pITypeInfo->lpVtbl->GetDocumentation( pITypeInfo, pFuncDesc->memid, &bstrFuncName, &bstrDoc, NULL, NULL );

      if( FAILED( hr ) )
      {
         hb_errRT_BASE( EG_ARG, 5017, "Could not retrieve Documentation", "ProcessInterface", 1, hb_paramError( 1 ) );
         pITypeInfo->lpVtbl->ReleaseFuncDesc( pITypeInfo, pFuncDesc );
         continue;
      }

      //printf( "         %ls[%i/%i](%s):%s -> %ls\n", bstrFuncName, pFuncDesc->memid, pFuncDesc->oVft, GetInvokeKindName( pFuncDesc->invkind ), sTypeDesc, bstrDoc );

      if( pFuncDesc->invkind & ( INVOKE_PROPERTYGET | INVOKE_PROPERTYPUT | INVOKE_PROPERTYPUTREF ) )
      {
         //pMethod = hb_itemDoC( "PROPERTYTYPEINFO", 0 );
         hb_vmPushSymbol( s_pSym_PropertyTypeInfo->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
         pMethod = hb_itemNew( hb_stackReturnItem() );
      }
      else
      {
         //pMethod = hb_itemDoC( "METHODTYPEINFO", 0 );
         hb_vmPushSymbol( s_pSym_MethodTypeInfo->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
         pMethod = hb_itemNew( hb_stackReturnItem() );
      }

      sTypeDesc = GetTypeDesc( pITypeInfo, &( pFuncDesc->elemdescFunc.tdesc ), pMethod, pObject, pTypeLib );

      if( bstrFuncName )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrFuncName ), wcslen( bstrFuncName ) );
         //hb_objSendMsg( pMethod, "_NAME", 1, pTemp );
         hb_vmPushSymbol( s_pSym_Name->pSymbol );
         hb_vmPush( pMethod );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( bstrDoc )
      {
         hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );
         //hb_objSendMsg( pMethod, "_HELPSTRING", 1, pTemp );
         hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
         hb_vmPush( pMethod );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      if( sTypeDesc )
      {
         hb_itemPutCPtr( pTemp, sTypeDesc, strlen( sTypeDesc ) );
         sTypeDesc = NULL;

         //hb_objSendMsg( pMethod, "_TYPEDESC", 1, pTemp );
         hb_vmPushSymbol( s_pSym_TypeDesc->pSymbol );
         hb_vmPush( pMethod );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);
      }

      hb_itemPutNL( pTemp, pFuncDesc->memid );
      //hb_objSendMsg( pMethod, "_MEMBERID", 1, pTemp );
      hb_vmPushSymbol( s_pSym_MemberID->pSymbol );
      hb_vmPush( pMethod );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      hb_itemPutNL( pTemp, pFuncDesc->oVft );
      //hb_objSendMsg( pMethod, "_VTBLOFFSET", 1, pTemp );
      hb_vmPushSymbol( s_pSym_VtblOffset->pSymbol );
      hb_vmPush( pMethod );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      hb_itemPutNL( pTemp, pFuncDesc->elemdescFunc.tdesc.vt );
      //hb_objSendMsg( pMethod, "_VT", 1, pTemp );
      hb_vmPushSymbol( s_pSym_VT->pSymbol );
      hb_vmPush( pMethod );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);

      if( iImplTypeFlags & IMPLTYPEFLAG_FSOURCE )
      {
         hb_arrayAdd( pEvents, pMethod );
      }
      else
      {
         if( pFuncDesc->invkind & INVOKE_FUNC )
         {
            hb_arrayAdd( pMethods, pMethod );
         }
         else
         {
            ULONG ulProperty;

            hb_itemPutNL( &G_MEMBERID, pFuncDesc->memid );

            if( ( ulProperty = hb_arrayScan( pProperties, &G_SCANMEMBERID, NULL, NULL, TRUE, FALSE ) ) == 0 )
            {
               if( pFuncDesc->invkind & ( INVOKE_PROPERTYPUT | INVOKE_PROPERTYPUTREF ) )
               {
                  //hb_objSendMsg( pMethod, "_READONLY", 1, pTemp );
                  hb_vmPushSymbol( s_pSym_ReadOnly->pSymbol );
                  hb_vmPush( pMethod );
                  hb_vmPushLogical( FALSE );
                  hb_vmSend(1);

                  if( pFuncDesc->invkind & INVOKE_PROPERTYPUTREF )
                  {
                     //hb_objSendMsg( pMethod, "_BYREF", 1, pTemp );
                     hb_vmPushSymbol( s_pSym_ByRef->pSymbol );
                     hb_vmPush( pMethod );
                     hb_vmPushLogical( TRUE );
                     hb_vmSend(1);
                  }
               }

               hb_arrayAdd( pProperties, pMethod );
            }
            else
            {
               if( pFuncDesc->invkind & ( INVOKE_PROPERTYPUT | INVOKE_PROPERTYPUTREF ) )
               {
                  //hb_objSendMsg( pMethod, "_READONLY", 1, pTemp );
                  hb_vmPushSymbol( s_pSym_ReadOnly->pSymbol );
                  hb_vmPush( pMethod );
                  hb_vmPushLogical( FALSE );
                  hb_vmSend(1);

                  if( pFuncDesc->invkind & INVOKE_PROPERTYPUTREF )
                  {
                     //hb_objSendMsg( pMethod, "_BYREF", 1, pTemp );
                     hb_vmPushSymbol( s_pSym_ByRef->pSymbol );
                     hb_vmPush( pMethod );
                     hb_vmPushLogical( TRUE );
                     hb_vmSend(1);
                  }

                  hb_arraySet( pProperties, ulProperty, pMethod );
               }
            }
         }
      }

      paNames = (BSTR *) hb_xgrab( sizeof( BSTR ) * ( pFuncDesc->cParams + 1 ) );
      memset( paNames, 0, sizeof( BSTR ) * ( pFuncDesc->cParams + 1 ) );

      hr = pITypeInfo->lpVtbl->GetNames( pITypeInfo, pFuncDesc->memid, paNames, pFuncDesc->cParams + 1, &cNames );

      SysFreeString( paNames[0] );

      if( sTypeDesc )
      {
         hb_xfree( sTypeDesc );
         sTypeDesc = NULL;
      }

      if( SUCCEEDED( hr ) && cNames && pFuncDesc->cParams )
      {
         unsigned short iParam;
         PHB_ITEM pArgs, pArg;

         if( cNames < pFuncDesc->cParams + 1 )
         {
            for( iParam = 0; iParam < pFuncDesc->cParams; iParam++ )
            {
               if( iParam + 1 >= cNames )
               {
                  paNames[ 1 + iParam ] = SysAllocString( L"RHS" );
                  cNames++;
               }
            }
         }

         pArgs = hb_itemNew( NULL );
         hb_arrayNew( pArgs, pFuncDesc->cParams );

         for( iParam = 0; iParam < pFuncDesc->cParams; iParam++ )
         {
            if( iParam >= cNames )
            {
               hb_errRT_BASE( EG_ARG, 5018, "Could not retrieve Param", "ProcessInterface", 1, hb_paramError( 1 ) );
               break;
            }

            //pArg = hb_itemDoC( "ARGUMENTTYPEINFO", 0 );
            hb_vmPushSymbol( s_pSym_ArgumentTypeInfo->pSymbol );
            hb_vmPushNil();
            hb_vmDo(0);
            pArg = hb_itemNew( hb_stackReturnItem() );

            sTypeDesc = GetTypeDesc( pITypeInfo, &( pFuncDesc->lprgelemdescParam[iParam].tdesc ), pArg, pObject, pTypeLib );

            if( paNames[ 1 + iParam ] )
            {
               hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( paNames[ 1 + iParam ] ), wcslen( paNames[ 1 + iParam ] ) );
               //hb_objSendMsg( pArg, "_NAME", 1, pTemp );
               hb_vmPushSymbol( s_pSym_Name->pSymbol );
               hb_vmPush( pArg );
               hb_itemPushForward( pTemp );
               hb_vmSend(1);
            }

            if( sTypeDesc )
            {
               hb_itemPutCPtr( pTemp, sTypeDesc, strlen( sTypeDesc ) );
               sTypeDesc = NULL;

               //hb_objSendMsg( pArg, "_TYPEDESC", 1, pTemp );
               hb_vmPushSymbol( s_pSym_TypeDesc->pSymbol );
               hb_vmPush( pArg );
               hb_itemPushForward( pTemp );
               hb_vmSend(1);
            }

            hb_itemPutNL( pTemp, pFuncDesc->lprgelemdescParam[iParam].tdesc.vt );
            //hb_objSendMsg( pArg, "_VT", 1, pTemp );
            hb_vmPushSymbol( s_pSym_VT->pSymbol );
            hb_vmPush( pArg );
            hb_itemPushForward( pTemp );
            hb_vmSend(1);

            //hb_objSendMsg( pArg, "_BYREF", 1, pTemp );
            hb_vmPushSymbol( s_pSym_ByRef->pSymbol );
            hb_vmPush( pArg );
            hb_vmPushLogical( pFuncDesc->lprgelemdescParam[iParam].paramdesc.wParamFlags & PARAMFLAG_FOUT );
            hb_vmSend(1);

            //hb_objSendMsg( pArg, "_OPTIONAL", 1, pTemp );
            hb_vmPushSymbol( s_pSym_Optional->pSymbol );
            hb_vmPush( pArg );
            hb_vmPushLogical( pFuncDesc->lprgelemdescParam[iParam].paramdesc.wParamFlags & PARAMFLAG_FOPT );
            hb_vmSend(1);

            hb_itemPutNL( pTemp, pFuncDesc->lprgelemdescParam[iParam].paramdesc.wParamFlags );
            //hb_objSendMsg( pArg, "FLAGS", 1, pTemp );
            hb_vmPushSymbol( s_pSym_Flags->pSymbol );
            hb_vmPush( pArg );
            hb_itemPushForward( pTemp );
            hb_vmSend(1);

            hb_arraySetForward( pArgs, iParam + 1, pArg );
            hb_itemRelease( pArg );

            SysFreeString( paNames[ 1 + iParam ] );

            if( sTypeDesc )
            {
               hb_xfree( sTypeDesc );
               sTypeDesc = NULL;
            }
         }

         //_tprintf( _T("\n") );

         //hb_objSendMsg( pMethod, "_ARGUMENTS", 1, pArgs );
         hb_vmPushSymbol( s_pSym_Arguments->pSymbol );
         hb_vmPush( pMethod );
         hb_itemPushForward( pArgs );
         hb_vmSend(1);
         hb_itemRelease( pArgs );
      }

      hb_itemRelease( pMethod );

      hb_xfree( (void *) paNames );
      paNames = NULL;

      pITypeInfo->lpVtbl->ReleaseFuncDesc( pITypeInfo, pFuncDesc );

      SysFreeString( bstrFuncName );
      SysFreeString( bstrDoc );
   }

   hb_itemRelease( pTemp );
   hb_itemRelease( pInterface );
   hb_itemRelease( pTypeLibInterfaces );
   hb_itemRelease( pObjectInterfaces );

   hb_itemRelease( pProperties );
   hb_itemRelease( pMethods );
   hb_itemRelease( pEvents );

   pITypeInfo->lpVtbl->ReleaseTypeAttr( pITypeInfo, pTypeAttr );
   pITypeInfo->lpVtbl->Release( pITypeInfo );

   //_tprintf( _T("\n") );
}

PHB_ITEM ProcessType( LPTYPEINFO pITypeInfo, LPTYPEATTR pTypeAttr, int iImplTypeFlags, PHB_ITEM pTypeLib )
{
   BSTR bstrTypeInfoName = NULL, bstrDoc = NULL;
   VARDESC * pVarDesc;
   HRESULT hr;
   PHB_ITEM pType;
   PHB_ITEM pTemp;

   hr = pITypeInfo->lpVtbl->GetDocumentation( pITypeInfo, MEMBERID_NIL, &bstrTypeInfoName, &bstrDoc, NULL, NULL );

   switch( pTypeAttr->typekind )
   {
      case TKIND_ENUM:
      {
         //printf( "Enumeration: %ls -> %ls\n", bstrTypeInfoName, bstrDoc );

         //pType = hb_itemDoC( "ENUMTYPEINFO", 0 );
         hb_vmPushSymbol( s_pSym_EnumTypeInfo->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
         pType = hb_itemNew( hb_stackReturnItem() );

         pTemp = hb_itemNew( NULL );

         if( bstrTypeInfoName )
         {
            hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrTypeInfoName ), wcslen( bstrTypeInfoName ) );
            //hb_objSendMsg( pType, "_NAME", 1, pTemp );
            hb_vmPushSymbol( s_pSym_Name->pSymbol );
            hb_vmPush( pType );
            hb_itemPushForward( pTemp );
            hb_vmSend(1);
         }

         if( bstrDoc )
         {
            hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );
            //hb_objSendMsg( pType, "_HELPSTRING", 1, pTemp );
            hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
            hb_vmPush( pType );
            hb_itemPushForward( pTemp );
            hb_vmSend(1);
         }

         hb_itemRelease( pTemp );

         if( pTypeAttr->cVars )
         {
            unsigned int i;
            PHB_ITEM pConstants = hb_itemNew( NULL ), pConstant;

            //hb_objSendMsg( pType, "CONSTANTS", 0 );
            hb_vmPushSymbol( s_pSym_Constants->pSymbol );
            hb_vmPush( pType );
            hb_vmSend(0);

            hb_itemForwardValue( pConstants, hb_stackReturnItem() );
            hb_arraySize( pConstants, pTypeAttr->cVars );

            //_tprintf( _T("     Variables:\n") );

            for( i = 0; i < pTypeAttr->cVars; i++ )
            {
               if( SUCCEEDED( ( hr = pITypeInfo->lpVtbl->GetVarDesc( pITypeInfo, i, &pVarDesc ) ) ) )
               {
                  pConstant = ProcessVar( pITypeInfo, pVarDesc, NULL, pTypeLib );
                  hb_arraySetForward( pConstants, i + 1, pConstant );
                  hb_itemRelease( pConstant );
               }
               else
               {
                  char sDebug[MAX_PATH + 128];

                  snprintf( sDebug, MAX_PATH + 128, "Error: %i %s(%i)\n", hr, __FILE__, __LINE__ );
                  OutputDebugString( sDebug );
               }
            }

            //_tprintf( _T("\n") );

            hb_itemRelease( pConstants );
         }

         break;
      }

      default:
      {
         const char *sTypeName = GetTypeKindName( pTypeAttr->typekind );

         TraceLog( NULL, "Unexpected Kind: %s in %s(%i)\n", sTypeName, __FILE__, __LINE__ );
         hb_errRT_BASE( EG_ARG, 5019, "Unexpected Kind", sTypeName, 1, hb_paramError( 1 ) );

         pType = hb_itemNew( NULL );
      }
   }

   return pType;
}

PHB_ITEM ProcessVar( ITypeInfo * pITypeInfo, VARDESC *pVarDesc, char **psDesc, PHB_ITEM pTypeLib )
{
   BSTR bstrVarName = NULL, bstrDoc = NULL;
   char *sTypeDesc = NULL;
   char s[512];
   PHB_ITEM pVar = hb_itemNew( NULL ), pTemp;

   if( pVarDesc == NULL )
   {
      char sDebug[MAX_PATH + 128];

      snprintf( sDebug, MAX_PATH + 128, "NULL VarDesc! in %s(%i)\n", __FILE__, __LINE__ );
      OutputDebugString( sDebug );

      return pVar;
   }

   pITypeInfo->lpVtbl->GetDocumentation( pITypeInfo, pVarDesc->memid, &bstrVarName, &bstrDoc, NULL, NULL );

   /*
    VAR_PERINSTANCE = 0,
    VAR_STATIC = VAR_PERINSTANCE+1,
    VAR_CONST = VAR_STATIC+1,
    VAR_DISPATCH = VAR_CONST+1
   */

   pTemp = hb_itemNew( NULL );

   switch( pVarDesc->varkind )
   {
      case VAR_CONST:
      {
         long lConstant = pVarDesc->lpvarValue->n1.n2.n3.lVal;
         const char *sVTName = GetVTName( pVarDesc->lpvarValue->n1.n2.vt );

         snprintf( s, 512, "         %ls = %i (%s)-> %ls\n", bstrVarName, lConstant, sVTName, bstrDoc );

         //pVar = hb_itemDoC( "CONSTANTTYPEINFO", 0 );
         hb_vmPushSymbol( s_pSym_ConstantTypeInfo->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
         hb_itemCopy( pVar, hb_stackReturnItem() );

         hb_itemPutNL( pTemp, pVarDesc->lpvarValue->n1.n2.vt );
         //hb_objSendMsg( pVar, "_VT", 1, pTemp );
         hb_vmPushSymbol( s_pSym_VT->pSymbol );
         hb_vmPush( pVar );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);

         hb_itemPutNL( pTemp, lConstant );
         //hb_objSendMsg( pVar, "VALUE", 1, pTemp );
         hb_vmPushSymbol( s_pSym_Value->pSymbol );
         hb_vmPush( pVar );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);

         hb_itemPutCStatic( pTemp, sVTName );

         //hb_objSendMsg( pVar, "_TYPEDESC", 1, pTemp );
         hb_vmPushSymbol( s_pSym_TypeDesc->pSymbol );
         hb_vmPush( pVar );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);

         break;
      }

      default:
      {
         //pVar = hb_itemDoC( "PROPERTYTYPEINFO", 0 );
         hb_vmPushSymbol( s_pSym_PropertyTypeInfo->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
         hb_itemCopy( pVar, hb_stackReturnItem() );

         hb_itemPutNL( pTemp, pVarDesc->elemdescVar.tdesc.vt );
         //hb_objSendMsg( pVar, "_VT", 1, pTemp );
         hb_vmPushSymbol( s_pSym_VT->pSymbol );
         hb_vmPush( pVar );
         hb_itemPushForward( pTemp );
         hb_vmSend(1);

         sTypeDesc = GetTypeDesc( pITypeInfo, &( pVarDesc->elemdescVar.tdesc ), NULL, NULL, pTypeLib );
         snprintf( s, 512, "*** %ls (%s)", bstrVarName, sTypeDesc );
      }
   }

   pITypeInfo->lpVtbl->ReleaseVarDesc( pITypeInfo, pVarDesc );

   if( bstrVarName )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrVarName ), wcslen( bstrVarName ) );
      //hb_objSendMsg( pVar, "_NAME", 1, pTemp );
      hb_vmPushSymbol( s_pSym_Name->pSymbol );
      hb_vmPush( pVar );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   if( bstrDoc )
   {
      hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrDoc ), wcslen( bstrDoc ) );
      //hb_objSendMsg( pVar, "_HELPSTRING", 1, pTemp );
      hb_vmPushSymbol( s_pSym_HelpString->pSymbol );
      hb_vmPush( pVar );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   if( sTypeDesc )
   {
      hb_itemPutCPtr( pTemp, sTypeDesc, strlen( sTypeDesc ) );
      sTypeDesc = NULL;

      //hb_objSendMsg( pVar, "_TYPEDESC", 1, pTemp );
      hb_vmPushSymbol( s_pSym_TypeDesc->pSymbol );
      hb_vmPush( pVar );
      hb_itemPushForward( pTemp );
      hb_vmSend(1);
   }

   SysFreeString( bstrVarName );
   SysFreeString( bstrDoc );
   //bstrTypeDesc is a static string!

   if( psDesc )
   {
      *psDesc = hb_strdup( s );
   }

   hb_itemRelease( pTemp );

   return pVar;
}

char * GetTypeFlagName( unsigned short typeflags )
{
   char *s = (char *) hb_xgrab( 512 );

   strcpy( s, "" );

   FLAG_STRING( TYPEFLAG_FAPPOBJECT )
   FLAG_STRING( TYPEFLAG_FCANCREATE )
   FLAG_STRING( TYPEFLAG_FLICENSED )
   FLAG_STRING( TYPEFLAG_FPREDECLID )
   FLAG_STRING( TYPEFLAG_FHIDDEN )
   FLAG_STRING( TYPEFLAG_FCONTROL )
   FLAG_STRING( TYPEFLAG_FDUAL )
   FLAG_STRING( TYPEFLAG_FNONEXTENSIBLE )
   FLAG_STRING( TYPEFLAG_FOLEAUTOMATION )
   FLAG_STRING( TYPEFLAG_FRESTRICTED )
   FLAG_STRING( TYPEFLAG_FAGGREGATABLE )
   FLAG_STRING( TYPEFLAG_FREPLACEABLE )
   FLAG_STRING( TYPEFLAG_FDISPATCHABLE )
   FLAG_STRING( TYPEFLAG_FREVERSEBIND )
   FLAG_STRING( TYPEFLAG_FPROXY )

   return s;
}

const char *GetTypeKindName( TYPEKIND typekind )
{
   switch( typekind )
   {
       CASE_STRING( TKIND_ENUM )
       CASE_STRING( TKIND_RECORD )
       CASE_STRING( TKIND_MODULE )
       CASE_STRING( TKIND_INTERFACE )
       CASE_STRING( TKIND_DISPATCH )
       CASE_STRING( TKIND_COCLASS )
       CASE_STRING( TKIND_ALIAS )
       CASE_STRING( TKIND_UNION )
       default:
         hb_errRT_BASE( EG_ARG, 5019, "Unexpected Kind", "GetTypeKindName", 1, hb_paramError( 1 ) );
   }

   return "<unknown>";
}

const char * GetInvokeKindName( INVOKEKIND invkind )
{
   switch( invkind )
   {
      CASE_STRING( INVOKE_FUNC )
      CASE_STRING( INVOKE_PROPERTYGET )
      CASE_STRING( INVOKE_PROPERTYPUT )
      CASE_STRING( INVOKE_PROPERTYPUTREF )
   }

   return "<unknown>";
}

const char * GetVTName( VARTYPE vt )
{
   switch( vt )
   {
        CASE_STRING( VT_EMPTY )
        CASE_STRING( VT_NULL )
        CASE_STRING( VT_I2 )
        CASE_STRING( VT_I4 )
        CASE_STRING( VT_R4 )
        CASE_STRING( VT_R8 )
        CASE_STRING( VT_CY )
        CASE_STRING( VT_DATE )
        CASE_STRING( VT_BSTR )
        CASE_STRING( VT_DISPATCH )
        CASE_STRING( VT_ERROR )
        CASE_STRING( VT_BOOL )
        CASE_STRING( VT_VARIANT )
        CASE_STRING( VT_UNKNOWN )
        CASE_STRING( VT_DECIMAL )
        CASE_STRING( VT_I1 )
        CASE_STRING( VT_UI1 )
        CASE_STRING( VT_UI2 )
        CASE_STRING( VT_UI4 )
        CASE_STRING( VT_I8 )
        CASE_STRING( VT_UI8 )
        CASE_STRING( VT_INT )
        CASE_STRING( VT_UINT )
        CASE_STRING( VT_VOID )
        CASE_STRING( VT_HRESULT )
        CASE_STRING( VT_PTR )
        CASE_STRING( VT_SAFEARRAY )
        CASE_STRING( VT_CARRAY )
        CASE_STRING( VT_USERDEFINED )
        CASE_STRING( VT_LPSTR )
        CASE_STRING( VT_LPWSTR )
        CASE_STRING( VT_RECORD )
        CASE_STRING( VT_FILETIME )
        CASE_STRING( VT_BLOB )
        CASE_STRING( VT_STREAM )
        CASE_STRING( VT_STORAGE )
        CASE_STRING( VT_STREAMED_OBJECT )
        CASE_STRING( VT_STORED_OBJECT )
        CASE_STRING( VT_BLOB_OBJECT )
        CASE_STRING( VT_CF )
        CASE_STRING( VT_CLSID )
        //CASE_STRING( VT_VERSIONED_STREAM )
        CASE_STRING( VT_BSTR_BLOB )
        CASE_STRING( VT_VECTOR )
        CASE_STRING( VT_ARRAY )
        CASE_STRING( VT_BYREF )
        CASE_STRING( VT_RESERVED )
        CASE_STRING( VT_ILLEGAL )
        //CASE_STRING( VT_ILLEGALMASKED )
        //CASE_STRING( VT_TYPEMASK )
        default:
           hb_errRT_BASE( EG_ARG, 5020, "Unexpected VT", "GetVTName", 1, hb_paramError( 1 ) );
           return "Unknown VT";
   }
}

char * GetTypeDesc( LPTYPEINFO pITypeInfo, TYPEDESC *ptdesc, PHB_ITEM pType, PHB_ITEM pObject, PHB_ITEM pTypeLib )
{
   char *s2;
   HRESULT hr;
   LPTYPEINFO pUserDefinedInfo;
   LPTYPEATTR pTypeAttr;
   VARDESC *pVarDesc = NULL;
   unsigned int i;
   char s[1024];

   strcpy( s, "DEFAULT VT!!!" );

   switch( ptdesc->vt )
   {
      case VT_PTR:
        //OutputDebugString( "Pointer!\n" );
        s2 = GetTypeDesc( pITypeInfo, ptdesc->lptdesc, pType, pObject, pTypeLib );
        //OutputDebugString( "->>>" );OutputDebugStringW( s );OutputDebugStringW( s2 );OutputDebugString( "<<<-\n" );
        snprintf( s, 1024, "*%s", s2 );
        hb_xfree( s2 );
        s2 = NULL;
        return hb_strdup( s );

      case VT_SAFEARRAY:
        s2 = GetTypeDesc( pITypeInfo, &( ptdesc->lpadesc->tdescElem ), pType, pObject, pTypeLib );
        snprintf( s, 1024, "%s SAFEARRAY [ ", s2 );
        hb_xfree( s2 );
        s2 = NULL;

#if 1
        for( i = 0; i < ptdesc->lpadesc->cDims; i++ )
        {
           char s64[64];

           snprintf( s64, 64, "%i...%i,", ptdesc->lpadesc->rgbounds[i].lLbound, ptdesc->lpadesc->rgbounds[i].lLbound + ptdesc->lpadesc->rgbounds[i].cElements );
           //SysReAllocString( &s, wcslen( s ) + 64 );
           strcat( s, s64 );

           if( i > 2 )
           {
              OutputDebugValues( "Dims: %i\n", ptdesc->lpadesc->cDims );
              break;
           }
        }
#endif

        s[ strlen( s ) - 1 ] = ']';

        return hb_strdup( s );

      case VT_CARRAY:
        s2 = GetTypeDesc( pITypeInfo, &( ptdesc->lpadesc->tdescElem ), pType, pObject, pTypeLib );
        snprintf( s, 1024, "%s[", s2 );
        hb_xfree( s2 );
        s2 = NULL;

        for( i = 0; i < ptdesc->lpadesc->cDims; i++ )
        {
           char s64[64];

           snprintf( s64, 64, "%i...%i", ptdesc->lpadesc->rgbounds[i].lLbound, ptdesc->lpadesc->rgbounds[i].lLbound + ptdesc->lpadesc->rgbounds[i].cElements );
           strcat( s, s64 );
        }

        strcat( s, "]" );

        return hb_strdup( s );

      case VT_USERDEFINED:
        hr = pITypeInfo->lpVtbl->GetRefTypeInfo( pITypeInfo, ptdesc->hreftype, &pUserDefinedInfo );

        strcpy( s, "VT_USERDEFINED: " );

        if( SUCCEEDED( hr ) )
        {
           if( SUCCEEDED( pUserDefinedInfo->lpVtbl->GetTypeAttr( pUserDefinedInfo, &pTypeAttr ) ) )
           {
              BSTR bstrIID = NULL, bstrInterfaceName = NULL, bstrDesc = NULL;
              int iImplTypeFlags = 0;

              switch( pTypeAttr->typekind )
              {
                 case TKIND_ENUM:
                    if( SUCCEEDED( pUserDefinedInfo->lpVtbl->GetDocumentation( pUserDefinedInfo, MEMBERID_NIL, &bstrDesc, NULL, NULL, NULL ) ) )
                    {
                       snprintf( s, 512, "Enumeration %ls", bstrDesc );
                       SysFreeString( bstrDesc );
                       bstrDesc = NULL;
                    }
                    else
                    {
                       strcat( s, "Enumeration <***>" );
                    }

                    return hb_strdup( s );

                 case TKIND_RECORD:
                    strcpy( s, "struct {" );

                    for( i = 0; i < pTypeAttr->cVars; i++ )
                    {
                       char *sMember = NULL;

                       pUserDefinedInfo->lpVtbl->GetVarDesc( pUserDefinedInfo, i, &pVarDesc );
                       hb_itemRelease( ProcessVar( pUserDefinedInfo, pVarDesc, &sMember, pTypeLib ) );
                       strcat( s, sMember );
                       hb_xfree( sMember );
                       sMember = NULL;
                    }

                    s[ strlen( s ) - 2 ] = ' ';
                    s[ strlen( s ) - 1 ] = '}';
                    break;

                 case TKIND_UNION:
                    strcpy( s, "union {" );

                    for( i = 0; i < pTypeAttr->cVars; i++ )
                    {
                       char *sMember = NULL;

                       pUserDefinedInfo->lpVtbl->GetVarDesc( pUserDefinedInfo, i, &pVarDesc );
                       hb_itemRelease( ProcessVar( pUserDefinedInfo, pVarDesc, &sMember, pTypeLib ) );
                       strcat( s, sMember );
                       hb_xfree( sMember );
                       sMember = NULL;
                    }

                    s[ strlen( s ) - 2 ] = ' ';
                    s[ strlen( s ) - 1 ] = '}';
                    break;

                 case TKIND_COCLASS:
                 case TKIND_DISPATCH:
                    hr = pUserDefinedInfo->lpVtbl->GetTypeAttr( pUserDefinedInfo, &pTypeAttr );

                    if( FAILED( hr ) )
                    {
                       hb_errRT_BASE( EG_ARG, 5015, "Could not retrieve TypeAttr", "GetTypeDesc", 1, hb_paramError( 1 ) );
                       pUserDefinedInfo->lpVtbl->Release( pUserDefinedInfo );
                       return NULL;
                    }

                    StringFromCLSID( &pTypeAttr->guid, &bstrIID );

                    if( bstrIID && pType )
                    {
                       PHB_ITEM pTemp = hb_itemNew( NULL );

                       hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrIID ), wcslen( bstrIID ) );
                       //hb_objSendMsg( pType, "_GUID", 1, pTemp );
                       hb_vmPushSymbol( s_pSym_GUID->pSymbol );
                       hb_vmPush( pType );
                       hb_itemPushForward( pTemp );
                       hb_vmSend(1);

                       hb_itemRelease( pTemp );
                    }

                    pUserDefinedInfo->lpVtbl->GetDocumentation( pUserDefinedInfo, MEMBERID_NIL, &bstrInterfaceName, NULL, NULL, NULL );

                    if( bstrInterfaceName && pType )
                    {
                       PHB_ITEM pTemp = hb_itemNew( NULL );

                       hb_itemPutCPtr( pTemp, hb_oleWideToAnsi( bstrInterfaceName ), wcslen( bstrInterfaceName ) );
                       //hb_objSendMsg( pType, "_INTERFACE", 1, pTemp );
                       hb_vmPushSymbol( s_pSym_Interface->pSymbol );
                       hb_vmPush( pType );
                       hb_itemPushForward( pTemp );
                       hb_vmSend(1);

                       hb_itemRelease( pTemp );

                       snprintf( s, 1024, "%ls", bstrInterfaceName );
                    }

                    //pUserDefinedInfo->lpVtbl->GetImplTypeFlags( pUserDefinedInfo, MEMBERID_NIL, &iImplTypeFlags );

                    if( pObject )
                    {
                       pUserDefinedInfo->lpVtbl->AddRef( pUserDefinedInfo );
                       ProcessInterface( pUserDefinedInfo, iImplTypeFlags, pObject, pTypeLib );
                    }
                    break;

                 default:
                   snprintf( s + strlen( s ), 1024 - strlen( s) , "%s", GetTypeKindName( pTypeAttr->typekind ) );
              }

              pUserDefinedInfo->lpVtbl->ReleaseTypeAttr( pUserDefinedInfo, pTypeAttr );
           }

           pUserDefinedInfo->lpVtbl->Release( pUserDefinedInfo );
        }

      default:
      {
         const char *sVTName = GetVTName( ptdesc->vt );

         //OutputDebugValues( "VT Default case: %s\n", sVTName );

         snprintf( s, 1024, "%s", sVTName );
      }
   }

   return hb_strdup( s );
}

HRESULT Unknown_GetTypeInfo( IUnknown *pUnk, ITypeInfo **ppTypeInfo )
{
   HRESULT hr;
   IProvideClassInfo2 *pClassInfo2 = NULL;
   IProvideClassInfo *pClassInfo = NULL;

   *ppTypeInfo = NULL;

   #if 0
     IProvideMultipleClassInfo *pMultipleClassInfo = NULL;

     hr = pUnk->lpVtbl->QueryInterface( pUnk, &IID_IProvideMultipleClassInfo, (void**) &pMultipleClassInfo );

     if( FAILED( hr ) || pMultipleClassInfo == NULL )
     {
        TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
     }
   #endif

   hr = pUnk->lpVtbl->QueryInterface( pUnk, &IID_IProvideClassInfo2, (void**) &pClassInfo2 );

   if( FAILED( hr ) || pClassInfo2 == NULL )
   {
      TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
   }
   else
   {
      hr = pClassInfo2->lpVtbl->GetClassInfo( pClassInfo2, ppTypeInfo );
      pClassInfo2->lpVtbl->Release( pClassInfo2 );

      if( FAILED( hr ) || ppTypeInfo == NULL )
      {
         TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
      }
      else
      {
         return hr;
      }
   }

   hr = pUnk->lpVtbl->QueryInterface( pUnk, &IID_IProvideClassInfo, (void**) &pClassInfo );

   if( FAILED( hr ) || pClassInfo == NULL )
   {
      TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
      return hr;
   }

   hr = pClassInfo->lpVtbl->GetClassInfo( pClassInfo, ppTypeInfo );
   pClassInfo->lpVtbl->Release( pClassInfo );

   if( FAILED( hr ) || ppTypeInfo == NULL )
   {
      TraceLog( NULL, "Result: %i in: %s(%i)\n", hr, __FILE__, __LINE__ );
   }

   return hr;
}
