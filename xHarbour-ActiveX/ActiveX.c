#ifndef CINTERFACE
   #define CINTERFACE 1
#endif

#ifdef __XCC__
  //#pragma comment( lib, "ole.lib" )
  //#pragma comment( lib, "oleserver.lib" )
#endif

#if 0
   #define _DEBUG
#else
   #undef _DEBUG
#endif

#include "hbapi.h"
#include "hbapierr.h"

#include <windows.h>

#include <olectl.h>
#include <ole2.h>
#include <ocidl.h>

HB_EXTERN_BEGIN
   extern HB_EXPORT LPWSTR hb_oleAnsiToWide( const char *cString );
HB_EXTERN_END

#if defined( _DEBUG )
   void OutputDebugValues( const char *sFormat, ... )
   {
       char sBuffer[ 1024 ];
       va_list ap;

       va_start( ap, sFormat );

       vsprintf( sBuffer, sFormat, ap );

       OutputDebugString( sBuffer );

       va_end( ap );
   }
#else
   #define OutputDebugValues {}(void)
#endif

#ifndef USE_ATLAX

#define HIMETRIC_PER_INCH   2540
#define MAP_PIX_TO_LOGHIM( x, ppli )  ( (long) MulDiv( HIMETRIC_PER_INCH, ( x ), ( ppli ) ) )
#define MAP_LOGHIM_TO_PIX( x, ppli )  ( (long) MulDiv( ( ppli ), ( x ), HIMETRIC_PER_INCH ) )

// IStorage
HRESULT STDMETHODCALLTYPE IStorage_QueryInterface( IStorage *This, REFIID riid, void **ppvObject );
ULONG   STDMETHODCALLTYPE IStorage_AddRef( IStorage *This );
ULONG   STDMETHODCALLTYPE IStorage_Release( IStorage *This );
HRESULT STDMETHODCALLTYPE IStorage_CreateStream( IStorage *This, const WCHAR *pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStream **ppstm );
HRESULT STDMETHODCALLTYPE IStorage_OpenStream( IStorage *This, const WCHAR * pwcsName, void *reserved1, DWORD grfMode, DWORD reserved2, IStream **ppstm );
HRESULT STDMETHODCALLTYPE IStorage_CreateStorage( IStorage *This, const WCHAR *pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStorage **ppstg );
HRESULT STDMETHODCALLTYPE IStorage_OpenStorage( IStorage *This, const WCHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage **ppstg );
HRESULT STDMETHODCALLTYPE IStorage_CopyTo( IStorage *This, DWORD ciidExclude, IID const *rgiidExclude, SNB snbExclude,IStorage *pstgDest );
HRESULT STDMETHODCALLTYPE IStorage_MoveElementTo( IStorage *This, const OLECHAR *pwcsName,IStorage * pstgDest, const OLECHAR *pwcsNewName, DWORD grfFlags );
HRESULT STDMETHODCALLTYPE IStorage_Commit( IStorage *This, DWORD grfCommitFlags );
HRESULT STDMETHODCALLTYPE IStorage_Revert( IStorage *This );
HRESULT STDMETHODCALLTYPE IStorage_EnumElements( IStorage *This, DWORD reserved1, void * reserved2, DWORD reserved3, IEnumSTATSTG ** ppenum );
HRESULT STDMETHODCALLTYPE IStorage_DestroyElement( IStorage *This, const OLECHAR *pwcsName );
HRESULT STDMETHODCALLTYPE IStorage_RenameElement( IStorage *This, const WCHAR *pwcsOldName, const WCHAR *pwcsNewName );
HRESULT STDMETHODCALLTYPE IStorage_SetElementTimes( IStorage *This, const WCHAR *pwcsName, FILETIME const *pctime, FILETIME const *patime, FILETIME const *pmtime );
HRESULT STDMETHODCALLTYPE IStorage_SetClass( IStorage *This, REFCLSID clsid );
HRESULT STDMETHODCALLTYPE IStorage_SetStateBits( IStorage *This, DWORD grfStateBits, DWORD grfMask );
HRESULT STDMETHODCALLTYPE IStorage_Stat( IStorage *This, STATSTG * pstatstg, DWORD grfStatFlag );

static IStorageVtbl s_IStorageVtbl =
{
   IStorage_QueryInterface,
   IStorage_AddRef,
   IStorage_Release,
   IStorage_CreateStream,
   IStorage_OpenStream,
   IStorage_CreateStorage,
   IStorage_OpenStorage,
   IStorage_CopyTo,
   IStorage_MoveElementTo,
   IStorage_Commit,
   IStorage_Revert,
   IStorage_EnumElements,
   IStorage_DestroyElement,
   IStorage_RenameElement,
   IStorage_SetElementTimes,
   IStorage_SetClass,
   IStorage_SetStateBits,
   IStorage_Stat
};

typedef struct tagCStorage
{
   struct IStorageVtbl *lpVtbl;
   int m_iRef;
} CStorage;

// IOleInPlaceFrame
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_QueryInterface( IOleInPlaceFrame *This, REFIID riid, void **ppvObject );
ULONG   STDMETHODCALLTYPE IOleInPlaceFrame_AddRef( IOleInPlaceFrame *This );
ULONG   STDMETHODCALLTYPE IOleInPlaceFrame_Release( IOleInPlaceFrame *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_GetWindow( IOleInPlaceFrame *This, HWND * lphwnd );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_ContextSensitiveHelp( IOleInPlaceFrame *This, BOOL fEnterMode );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_GetBorder( IOleInPlaceFrame *This, LPRECT lprectBorder );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_RequestBorderSpace( IOleInPlaceFrame *This, LPCBORDERWIDTHS pborderwidths );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetBorderSpace( IOleInPlaceFrame *This, LPCBORDERWIDTHS pborderwidths );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetActiveObject( IOleInPlaceFrame *This, IOleInPlaceActiveObject *pActiveObject, LPCOLESTR pszObjName );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_InsertMenus( IOleInPlaceFrame *This, HMENU hmenuShared, LPOLEMENUGROUPWIDTHS lpMenuWidths );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetMenu( IOleInPlaceFrame *This, HMENU hmenuShared, HOLEMENU holemenu, HWND hwndActiveObject );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_RemoveMenus( IOleInPlaceFrame *This, HMENU hmenuShared );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetStatusText( IOleInPlaceFrame *This, LPCOLESTR pszStatusText );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_EnableModeless( IOleInPlaceFrame *This, BOOL fEnable );
HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_TranslateAccelerator( IOleInPlaceFrame *This, LPMSG lpmsg, WORD wID );

static IOleInPlaceFrameVtbl s_IOleInPlaceFrameVtbl =
{
   IOleInPlaceFrame_QueryInterface,
   IOleInPlaceFrame_AddRef,
   IOleInPlaceFrame_Release,
   IOleInPlaceFrame_GetWindow,
   IOleInPlaceFrame_ContextSensitiveHelp,
   IOleInPlaceFrame_GetBorder,
   IOleInPlaceFrame_RequestBorderSpace,
   IOleInPlaceFrame_SetBorderSpace,
   IOleInPlaceFrame_SetActiveObject,
   IOleInPlaceFrame_InsertMenus,
   IOleInPlaceFrame_SetMenu,
   IOleInPlaceFrame_RemoveMenus,
   IOleInPlaceFrame_SetStatusText,
   IOleInPlaceFrame_EnableModeless,
   IOleInPlaceFrame_TranslateAccelerator
};

typedef struct tagCOleInPlaceFrame
{
  struct IOleInPlaceFrameVtbl *lpVtbl;
  int m_iRef;
} COleInPlaceFrame;

// IOleClientSite
HRESULT STDMETHODCALLTYPE IOleClientSite_QueryInterface( IOleClientSite *This, REFIID riid, void ** ppvObject );
ULONG   STDMETHODCALLTYPE IOleClientSite_AddRef( IOleClientSite *This );
ULONG   STDMETHODCALLTYPE IOleClientSite_Release( IOleClientSite *This );
HRESULT STDMETHODCALLTYPE IOleClientSite_SaveObject( IOleClientSite *This );
HRESULT STDMETHODCALLTYPE IOleClientSite_GetMoniker( IOleClientSite *This, DWORD dwAssign, DWORD dwWhichMoniker, IMoniker ** ppmk );
HRESULT STDMETHODCALLTYPE IOleClientSite_GetContainer( IOleClientSite *This, LPOLECONTAINER * ppContainer );
HRESULT STDMETHODCALLTYPE IOleClientSite_ShowObject( IOleClientSite *This );
HRESULT STDMETHODCALLTYPE IOleClientSite_OnShowWindow( IOleClientSite *This, BOOL fShow );
HRESULT STDMETHODCALLTYPE IOleClientSite_RequestNewObjectLayout( IOleClientSite *This );

static IOleClientSiteVtbl s_IOleClientSiteVtbl =
{
   IOleClientSite_QueryInterface,
   IOleClientSite_AddRef,
   IOleClientSite_Release,
   IOleClientSite_SaveObject,
   IOleClientSite_GetMoniker,
   IOleClientSite_GetContainer,
   IOleClientSite_ShowObject,
   IOleClientSite_OnShowWindow,
   IOleClientSite_RequestNewObjectLayout
};

typedef struct tagCOleClientSite
{
  struct IOleClientSiteVtbl *lpVtbl;
  int m_iRef;
} COleClientSite;

// IOleInPlaceSiteEx
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_QueryInterface( IOleInPlaceSiteEx *This, REFIID riid, void **ppvObject );
ULONG   STDMETHODCALLTYPE IOleInPlaceSiteEx_AddRef( IOleInPlaceSiteEx *This );
ULONG   STDMETHODCALLTYPE IOleInPlaceSiteEx_Release( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_GetWindow( IOleInPlaceSiteEx *This, HWND * lphwnd );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_ContextSensitiveHelp( IOleInPlaceSiteEx *This, BOOL fEnterMode );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_CanInPlaceActivate( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceActivate( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnUIActivate( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_GetWindowContext( IOleInPlaceSiteEx *This, LPOLEINPLACEFRAME * lplpFrame,LPOLEINPLACEUIWINDOW * lplpDoc,LPRECT lprcPosRect,LPRECT lprcClipRect,LPOLEINPLACEFRAMEINFO lpFrameInfo );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_Scroll( IOleInPlaceSiteEx *This, SIZE scrollExtent );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnUIDeactivate( IOleInPlaceSiteEx *This, BOOL fUndoable );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceDeactivate( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_DiscardUndoState( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_DeactivateAndUndo( IOleInPlaceSiteEx *This );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnPosRectChange( IOleInPlaceSiteEx *This, LPCRECT lprcPosRect );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceActivateEx( IOleInPlaceSiteEx *This, BOOL * pfNoRedraw, DWORD dwFlags );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceDeactivateEx( IOleInPlaceSiteEx *This, BOOL fNoRedraw );
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_RequestUIActivate( IOleInPlaceSiteEx *This );

static IOleInPlaceSiteExVtbl s_IOleInPlaceSiteExVtbl =
{
   IOleInPlaceSiteEx_QueryInterface,
   IOleInPlaceSiteEx_AddRef,
   IOleInPlaceSiteEx_Release,
   IOleInPlaceSiteEx_GetWindow,
   IOleInPlaceSiteEx_ContextSensitiveHelp,
   IOleInPlaceSiteEx_CanInPlaceActivate,
   IOleInPlaceSiteEx_OnInPlaceActivate,
   IOleInPlaceSiteEx_OnUIActivate,
   IOleInPlaceSiteEx_GetWindowContext,
   IOleInPlaceSiteEx_Scroll,
   IOleInPlaceSiteEx_OnUIDeactivate,
   IOleInPlaceSiteEx_OnInPlaceDeactivate,
   IOleInPlaceSiteEx_DiscardUndoState,
   IOleInPlaceSiteEx_DeactivateAndUndo,
   IOleInPlaceSiteEx_OnPosRectChange,
   IOleInPlaceSiteEx_OnInPlaceActivateEx,
   IOleInPlaceSiteEx_OnInPlaceDeactivateEx,
   IOleInPlaceSiteEx_RequestUIActivate
};

typedef struct tagCOleInPlaceSiteEx
{
  struct IOleInPlaceSiteExVtbl *lpVtbl;
  int m_iRef;
} COleInPlaceSiteEx;

HRESULT STDMETHODCALLTYPE IAdviseSinkEx_QueryInterface( IAdviseSinkEx * This, REFIID riid, void **ppvObject );
ULONG   STDMETHODCALLTYPE IAdviseSinkEx_AddRef( IAdviseSinkEx * This );
ULONG   STDMETHODCALLTYPE IAdviseSinkEx_Release( IAdviseSinkEx * This );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnDataChange( IAdviseSinkEx * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnViewChange( IAdviseSinkEx * This, DWORD dwAspect, LONG lindex );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnRename( IAdviseSinkEx * This, IMoniker * pmk );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnSave( IAdviseSinkEx * This );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnClose( IAdviseSinkEx * This );
void    STDMETHODCALLTYPE IAdviseSinkEx_OnViewStatusChange( IAdviseSinkEx * This, DWORD dwViewStatus );

static IAdviseSinkExVtbl s_IAdviseSinkExVtbl =
{
    IAdviseSinkEx_QueryInterface,
    IAdviseSinkEx_AddRef,
    IAdviseSinkEx_Release,
    IAdviseSinkEx_OnDataChange,
    IAdviseSinkEx_OnViewChange,
    IAdviseSinkEx_OnRename,
    IAdviseSinkEx_OnSave,
    IAdviseSinkEx_OnClose,
    IAdviseSinkEx_OnViewStatusChange,
};

typedef struct tagCAdviseSink
{
  struct IAdviseSinkExVtbl *lpVtbl;
  int m_iRef;
} CAdviseSink;

typedef struct tagCOleSite
{
   COleClientSite    m_OleClientSite;      // My IOleClientSite object. Must be first.
   COleInPlaceSiteEx m_OleInPlaceSiteEx;   // My IOleInPlaceSite object. A convenient place to put it.
   COleInPlaceFrame  m_OleInPlaceFrame;
   CAdviseSink       m_AdviseSink;
   CStorage          m_Storage;

   IStorage          *m_pObjStorage;

   IOleObject              *m_pOleObject;
   IOleInPlaceObject       *m_pInPlaceObject;
   IOleInPlaceActiveObject *m_pActiveObject;

   HWND              m_hWnd;
   HWND              m_hFrameWnd;

   HWND              m_hWndIPObj;
   HWND              m_hWndUIActiveObj;

   HMENU             m_hMenu;
   HMENU             m_hFileMenu;
   HMENU             m_hViewMenu;
   HMENU             m_hWindowMenu;

   SIZEL             m_sizel;

   DWORD             m_dwDrawAspect;

   BOOL              m_fInPlaceActive;

   BORDERWIDTHS     m_Borders;

   HRESULT( STDMETHODCALLTYPE * GetObjRect ) ( struct tagCOleSite *This, RECT *pRect );
} COleSite;

#if 0
static void XformSizeInPixelsToHimetric( HDC hDC, SIZEL *pSizeInPix, SIZEL *pSizeInHiMetric )
{
   int cxPPI = GetDeviceCaps( hDC, LOGPIXELSX );
   int cyPPI = GetDeviceCaps( hDC, LOGPIXELSY );

   pSizeInHiMetric->cx = MAP_PIX_TO_LOGHIM( (int) pSizeInPix->cx, cxPPI );
   pSizeInHiMetric->cy = MAP_PIX_TO_LOGHIM( (int) pSizeInPix->cy, cyPPI );
}
#endif

static void XformSizeInHimetricToPixels( HDC hDC, SIZEL *pSizeInHiMetric, SIZEL *pSizeInPix )
{
   int cxPPI = GetDeviceCaps( hDC, LOGPIXELSX );
   int cyPPI = GetDeviceCaps( hDC, LOGPIXELSY );

   pSizeInPix->cx = MAP_LOGHIM_TO_PIX( (int) pSizeInHiMetric->cx, cxPPI );
   pSizeInPix->cy = MAP_LOGHIM_TO_PIX( (int) pSizeInHiMetric->cy, cyPPI );
}

HRESULT STDMETHODCALLTYPE COleSite_GetObjRect( struct tagCOleSite *This, RECT *pRect )
{
   SIZEL SizeInPix;
   HDC hDC = GetDC( This->m_hWnd );

   XformSizeInHimetricToPixels( hDC, &This->m_sizel, &SizeInPix );

   ReleaseDC( This->m_hWnd, hDC );

   // convert it to pixels
   pRect->left = pRect->top = 0;

   pRect->right = SizeInPix.cx;
   pRect->bottom = SizeInPix.cy;

   return S_OK;
}

#define OLESITE_FROM_CLIENTSITE( p )   ( ( COleSite * ) ( p ) )
#define OLESITE_FROM_INPLACESITE( p )  ( ( COleSite * ) ( ( (char *) ( p ) ) - sizeof( COleClientSite ) ) )
#define OLESITE_FROM_INPLACEFRAME( p ) ( ( COleSite * ) ( ( (char *) ( p ) ) - sizeof( COleClientSite ) - sizeof( COleInPlaceSiteEx ) ) )
#define OLESITE_FROM_ADVISESINK( p )   ( ( COleSite * ) ( ( (char *) ( p ) ) - sizeof( COleClientSite ) - sizeof( COleInPlaceSiteEx ) - sizeof( COleInPlaceFrame ) ) )

HRESULT STDMETHODCALLTYPE IAdviseSinkEx_QueryInterface( IAdviseSinkEx * This, REFIID riid, void **ppvObject )
{
   OutputDebugValues( "REDIRECT IAdviseSinkEx IID_*** %p\n", riid );

   return IOleClientSite_QueryInterface( (IOleClientSite *) OLESITE_FROM_ADVISESINK( This ), riid, ppvObject );
}

ULONG STDMETHODCALLTYPE IAdviseSinkEx_AddRef( IAdviseSinkEx * This )
{
   OutputDebugValues( "AdviseSink AddRef: %p\n", This );

   return 1;
}

ULONG STDMETHODCALLTYPE IAdviseSinkEx_Release( IAdviseSinkEx * This )
{
   OutputDebugValues( "AdviseSink Release: %p\n", This );

   return 1;
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnDataChange( IAdviseSinkEx * This, FORMATETC * pFormatetc, STGMEDIUM * pStgmed )
{
   OutputDebugValues( "AdviseSink OnDataChange: %p, %p, %p\n", This, pFormatetc, pStgmed );
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnViewChange( IAdviseSinkEx * This, DWORD dwAspect, LONG lindex )
{
   OutputDebugValues( "AdviseSink OnViewChange: %p, %i, %i\n", This, dwAspect, lindex );
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnRename( IAdviseSinkEx * This, IMoniker * pmk )
{
   OutputDebugValues( "AdviseSink OnRename: %p, %p\n", This, pmk );
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnSave( IAdviseSinkEx * This )
{
   OutputDebugValues( "AdviseSink OnSave: %p\n", This );
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnClose( IAdviseSinkEx * This )
{
   OutputDebugValues( "AdviseSink OnClose: %p\n", This );
}

void STDMETHODCALLTYPE IAdviseSinkEx_OnViewStatusChange( IAdviseSinkEx * This, DWORD dwViewStatus )
{
   OutputDebugValues( "AdviseSink OnStatusChange: %p, %i\n", This, dwViewStatus );
}

// IStorage
HRESULT STDMETHODCALLTYPE IStorage_QueryInterface( IStorage *This, REFIID riid, void **ppvObject )
{
   OutputDebugValues( "IStorage QueryInterface: %p, %p %p\n", This, riid, ppvObject );

   return E_NOINTERFACE;
}

ULONG STDMETHODCALLTYPE IStorage_AddRef( IStorage *This )
{
   OutputDebugValues( "*** IStorage_AddRef %p\n", This );

   return E_NOINTERFACE;
}

ULONG STDMETHODCALLTYPE IStorage_Release( IStorage *This )
{
   OutputDebugValues( "*** IStorage_Release %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_CreateStream( IStorage *This, const WCHAR *pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStream **ppstm )
{
   OutputDebugValues( "*** IStorage_CreateStream %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_OpenStream( IStorage *This, const WCHAR * pwcsName, void *reserved1, DWORD grfMode, DWORD reserved2, IStream **ppstm )
{
   OutputDebugValues( "*** IStorage_OpenStream %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_CreateStorage( IStorage *This, const WCHAR *pwcsName, DWORD grfMode, DWORD reserved1, DWORD reserved2, IStorage **ppstg )
{
   OutputDebugValues( "*** IStorage_CreateStorage %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_OpenStorage( IStorage *This, const WCHAR * pwcsName, IStorage * pstgPriority, DWORD grfMode, SNB snbExclude, DWORD reserved, IStorage **ppstg )
{
   OutputDebugValues( "*** IStorage_OpenStorage %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_CopyTo( IStorage *This, DWORD ciidExclude, IID const *rgiidExclude, SNB snbExclude,IStorage *pstgDest )
{
   OutputDebugValues( "*** IStorage_CopyTo %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_MoveElementTo( IStorage *This, const OLECHAR *pwcsName,IStorage * pstgDest, const OLECHAR *pwcsNewName, DWORD grfFlags )
{
   OutputDebugValues( "*** IStorage_MoveElementTo %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_Commit( IStorage *This, DWORD grfCommitFlags )
{
   OutputDebugValues( "*** IStorage_Commit %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_Revert( IStorage *This )
{
   OutputDebugValues( "*** IStorage_Revert %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_EnumElements( IStorage *This, DWORD reserved1, void * reserved2, DWORD reserved3, IEnumSTATSTG ** ppenum )
{
   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_DestroyElement( IStorage *This, const OLECHAR *pwcsName )
{
   OutputDebugValues( "*** IStorage_DestroyElement %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_RenameElement( IStorage *This, const WCHAR *pwcsOldName, const WCHAR *pwcsNewName )
{
   OutputDebugValues( "*** IStorage_RenameElement %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_SetElementTimes( IStorage *This, const WCHAR *pwcsName, FILETIME const *pctime, FILETIME const *patime, FILETIME const *pmtime )
{
   OutputDebugValues( "*** IStorage_SetElementTimes %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_SetClass( IStorage *This, REFCLSID clsid )
{
   OutputDebugValues( "*** IStorage_SetClass %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_SetStateBits( IStorage *This, DWORD grfStateBits, DWORD grfMask )
{
   OutputDebugValues( "*** IStorage_SetStateBits %p\n", This );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IStorage_Stat( IStorage *This, STATSTG * pstatstg, DWORD grfStatFlag )
{
   OutputDebugValues( "*** IStorage_Stat %p\n", This );

   return E_NOINTERFACE;
}

// IOleClientSite
HRESULT STDMETHODCALLTYPE IOleClientSite_QueryInterface( IOleClientSite *This, REFIID riid, void **ppvObject )
{
   OutputDebugValues( "QueryInterface %p\n", riid );

   if( ppvObject == NULL )
   {
      return E_INVALIDARG;
   }

   if( ! memcmp( riid, &IID_IUnknown, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IUnknown\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleClientSite;
   }
   else if( ! memcmp( riid, &IID_IDispatch, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IDispatch\n" );

      *ppvObject = NULL;
      return E_NOINTERFACE;
   }
   else if( ! memcmp( riid, &IID_IOleClientSite, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleClientSite\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleClientSite;
   }
   else if( ! memcmp( riid, &IID_IOleInPlaceSite, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleInPlaceSite\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleInPlaceSiteEx;
   }
   else if( ! memcmp( riid, &IID_IOleInPlaceSiteEx, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleInPlaceSiteEx\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleInPlaceSiteEx;
   }
   else if( ! memcmp( riid, &IID_IOleInPlaceFrame, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleInPlaceFrame\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleInPlaceFrame;
   }
   else if( ! memcmp( riid, &IID_IOleWindow, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleWindow\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleInPlaceFrame;
   }
   else if( ! memcmp( riid, &IID_IOleInPlaceUIWindow, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IOleInPlaceUIWindow\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_OleInPlaceFrame;
   }
   else if( ! memcmp( riid, &IID_IAdviseSink, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IAdviseSink\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_AdviseSink;
   }
   else if( ! memcmp( riid, &IID_IAdviseSinkEx, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IAdviseSinkEx\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_AdviseSink;
   }
   else if( ! memcmp( riid, &IID_IStorage, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IStorage\n" );

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage;
   }
   else if( ! memcmp( riid, &IID_IPersist, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IPersist\n" );

      if( OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage == (IStorage *) &OLESITE_FROM_CLIENTSITE( This )->m_Storage )
      {
         *ppvObject = NULL;
         return E_NOINTERFACE;
      }

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage;
   }
   else if( ! memcmp( riid, &IID_IPersistStorage, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IPersistStorage\n" );

      if( OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage == (IStorage *) &OLESITE_FROM_CLIENTSITE( This )->m_Storage )
      {
         *ppvObject = NULL;
         return E_NOINTERFACE;
      }

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage;
   }
   else if( ! memcmp( riid, &IID_IPersistFile, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IPersistFile\n" );

      if( OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage == (IStorage *) &OLESITE_FROM_CLIENTSITE( This )->m_Storage )
      {
         *ppvObject = NULL;
         return E_NOINTERFACE;
      }

      *ppvObject = &OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage;
   }
   else if( ! memcmp( riid, &IID_IProvideClassInfo, sizeof( GUID ) ) )
   {
      OutputDebugValues( "IID_IProvideClassInfo\n" );

      *ppvObject = NULL;
      return E_NOINTERFACE;
   }
   else
   {
      OutputDebugValues( "Unknown IID_*** %p\n", riid );

      *ppvObject = NULL;
      return E_NOINTERFACE;
   }

   return S_OK;
}

ULONG STDMETHODCALLTYPE IOleClientSite_AddRef( IOleClientSite *This )
{
   return( 1 );
}

ULONG STDMETHODCALLTYPE IOleClientSite_Release( IOleClientSite *This )
{
   return( 1 );
}

HRESULT STDMETHODCALLTYPE IOleClientSite_SaveObject( IOleClientSite *This )
{
   LPPERSISTSTORAGE lpPS = NULL;
   HRESULT RetCode = E_FAIL;

   OutputDebugValues( "SaveObject\n" );

   // get a pointer to IPersistStorage
   if( SUCCEEDED( OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage->lpVtbl->QueryInterface( OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage, &IID_IPersistStorage, ( LPVOID * ) &lpPS ) ) )
   {
      RetCode = OleSave( lpPS, OLESITE_FROM_CLIENTSITE( This )->m_pObjStorage, TRUE );
      lpPS->lpVtbl->SaveCompleted( lpPS, NULL );
      lpPS->lpVtbl->Release( lpPS );
   }

   return RetCode;
}

HRESULT STDMETHODCALLTYPE IOleClientSite_GetMoniker( IOleClientSite *This, DWORD dwAssign, DWORD dwWhichMoniker, IMoniker ** ppmk )
{
   OutputDebugValues( "GetMoniker\n" );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IOleClientSite_GetContainer( IOleClientSite *This, LPOLECONTAINER * ppContainer )
{
   OutputDebugValues( "GetContainer\n" );

   *ppContainer = NULL;

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IOleClientSite_ShowObject( IOleClientSite *This )
{
   OutputDebugValues( "ShowObject\n" );

   return( NOERROR );
}

HRESULT STDMETHODCALLTYPE IOleClientSite_OnShowWindow( IOleClientSite *This, BOOL fShow )
{
   OutputDebugValues( "OnshowWindow\n" );

   //m_pSite->m_fObjectOpen = fShow;
   InvalidateRect( OLESITE_FROM_CLIENTSITE( This )->m_hWnd, NULL, TRUE );

   // if object window is closing, then bring container window to top
   if( ! fShow )
   {
      BringWindowToTop( OLESITE_FROM_CLIENTSITE( This )->m_hWnd );
      SetFocus( OLESITE_FROM_CLIENTSITE( This )->m_hWnd );
   }

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleClientSite_RequestNewObjectLayout( IOleClientSite *This )
{
   OutputDebugValues( "RequestNewObjectLayout\n" );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_QueryInterface( IOleInPlaceSiteEx *This, REFIID riid, void **ppvObject )
{
   OutputDebugValues( "REDIRECT Inplace IID_*** %p\n", riid );

   return IOleClientSite_QueryInterface( (IOleClientSite *) OLESITE_FROM_CLIENTSITE( This ), riid, ppvObject );
}

ULONG STDMETHODCALLTYPE IOleInPlaceSiteEx_AddRef( IOleInPlaceSiteEx *This )
{
   return( 1 );
}

ULONG STDMETHODCALLTYPE IOleInPlaceSiteEx_Release( IOleInPlaceSiteEx *This )
{
   return( 1 );
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_GetWindow( IOleInPlaceSiteEx *This, HWND * lphwnd )
{
   OutputDebugValues( "GetWindow\n" );

   *lphwnd = OLESITE_FROM_INPLACESITE( This )->m_hWnd;

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_ContextSensitiveHelp( IOleInPlaceSiteEx *This, BOOL fEnterMode )
{
   OutputDebugValues( "Help: %i\n", fEnterMode );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_CanInPlaceActivate( IOleInPlaceSiteEx *This )
{
   OutputDebugValues( "CanInPlaceActivate\n" );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceActivate( IOleInPlaceSiteEx *This )
{
   OutputDebugValues( "OnInPlaceActivate\n" );

   if( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject )
   {
      OutputDebugValues( "*** Already Active!!!\n" );

      OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject->lpVtbl->Release( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject );
      OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject = NULL;
   }

   return OLESITE_FROM_INPLACESITE( This )->m_pOleObject->lpVtbl->QueryInterface( OLESITE_FROM_INPLACESITE( This )->m_pOleObject, &IID_IOleInPlaceObject, (LPVOID FAR *) &OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject );
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnUIActivate( IOleInPlaceSiteEx *This )
{
   OutputDebugValues( "OnUIActivate\n" );

   if( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject == NULL )
   {
      OutputDebugValues( "*** NO InPlaceObject recorded!!!\n" );

      OLESITE_FROM_INPLACESITE( This )->m_pOleObject->lpVtbl->QueryInterface( OLESITE_FROM_INPLACESITE( This )->m_pOleObject, &IID_IOleInPlaceObject, (LPVOID FAR *) &OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject );
   }

   OLESITE_FROM_INPLACESITE( This )->m_fInPlaceActive = TRUE;

   return OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject->lpVtbl->GetWindow( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject, &OLESITE_FROM_INPLACESITE( This )->m_hWndIPObj );
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_GetWindowContext( IOleInPlaceSiteEx *This, LPOLEINPLACEFRAME * lplpFrame, LPOLEINPLACEUIWINDOW * lplpDoc, LPRECT lprcPosRect, LPRECT lprcClipRect, LPOLEINPLACEFRAMEINFO lpFrameInfo )
{
   OutputDebugValues( "GetWindowContext\n" );

   *lplpFrame = ( LPOLEINPLACEFRAME ) &OLESITE_FROM_INPLACESITE( This )->m_OleInPlaceFrame;

   if( OLESITE_FROM_INPLACESITE( This )->m_hFrameWnd )
   {
      OutputDebugValues( "Frame Context %i\n", OLESITE_FROM_INPLACESITE( This )->m_hFrameWnd );

      lpFrameInfo->hwndFrame = OLESITE_FROM_INPLACESITE( This )->m_hFrameWnd;
   }
   else
   {
      OutputDebugValues( "Document Context %i\n", OLESITE_FROM_INPLACESITE( This )->m_hWnd );

      *lplpDoc = ( LPOLEINPLACEUIWINDOW ) NULL;
      lpFrameInfo->hwndFrame = OLESITE_FROM_INPLACESITE( This )->m_hWnd;
   }

   // Fill in some other info for the control
   lpFrameInfo->fMDIApp = FALSE;
   lpFrameInfo->haccel = 0;
   lpFrameInfo->cAccelEntries = 0;

   GetClientRect( OLESITE_FROM_INPLACESITE( This )->m_hWnd, lprcPosRect );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_Scroll( IOleInPlaceSiteEx *This, SIZE scrollExtent )
{
   OutputDebugValues( "Scroll\n" );

   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnUIDeactivate( IOleInPlaceSiteEx *This, BOOL fUndoable )
{
   OutputDebugValues( "OnUIDeactivate\n" );

   OLESITE_FROM_INPLACESITE( This )->m_fInPlaceActive = FALSE;
   OLESITE_FROM_INPLACESITE( This )->m_hWndIPObj = 0;

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceDeactivate( IOleInPlaceSiteEx *This )
{
   OutputDebugValues( "OnInPlaceDeactivate\n" );

   if( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject )
   {
      OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject->lpVtbl->Release( OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject );
      OLESITE_FROM_INPLACESITE( This )->m_pInPlaceObject = NULL;
      OLESITE_FROM_INPLACESITE( This )->m_hWndIPObj = 0;
   }
   else
   {
      OutputDebugValues( "*** NO InPlaceObject recorded!!!\n" );
   }

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_DiscardUndoState( IOleInPlaceSiteEx *This )
{
   return E_NOINTERFACE;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_DeactivateAndUndo( IOleInPlaceSiteEx *This )
{
   return E_NOINTERFACE;
}

// Called when the position of the control object is changed.
HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnPosRectChange( IOleInPlaceSiteEx *This, LPCRECT lprcPosRect )
{
   RECT rect;
   COleSite  *pOleSite = OLESITE_FROM_INPLACESITE( This );

   OutputDebugValues( "OnPosRectChanged\n" );

   pOleSite->m_pOleObject->lpVtbl->GetExtent( pOleSite->m_pOleObject, DVASPECT_CONTENT, &pOleSite->m_sizel );

   GetClientRect( pOleSite->m_hWnd, &rect);

   #if 1
     rect.left   += pOleSite->m_Borders.left;
     rect.top    += pOleSite->m_Borders.top;
     rect.right  -= pOleSite->m_Borders.right;
     rect.bottom += pOleSite->m_Borders.bottom;
   #endif

   // tell the object its new size
   pOleSite->m_pInPlaceObject->lpVtbl->SetObjectRects( pOleSite->m_pInPlaceObject, lprcPosRect, &rect );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceActivateEx( IOleInPlaceSiteEx *This, BOOL * pfNoRedraw, DWORD dwFlags )
{
   OutputDebugValues( "OnInPlaceActivateEx\n" );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_OnInPlaceDeactivateEx( IOleInPlaceSiteEx *This, BOOL fNoRedraw )
{
   OutputDebugValues( "OnInPlaceDeactivateEx\n" );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceSiteEx_RequestUIActivate( IOleInPlaceSiteEx *This )
{
   OutputDebugValues( "RequestUIActivate\n" );

   return S_OK;
}

////////////////////////////////////// My IOleInPlaceFrame functions  /////////////////////////////////////////

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_QueryInterface( IOleInPlaceFrame *This, REFIID riid, void **ppvObject )
{
   OutputDebugValues( "REDIRECT InPlaceFrame IID_*** %p\n", riid );

   return IOleClientSite_QueryInterface( (IOleClientSite *) OLESITE_FROM_INPLACEFRAME( This ), riid, ppvObject );
}

ULONG STDMETHODCALLTYPE IOleInPlaceFrame_AddRef( IOleInPlaceFrame *This )
{
   return( 1 );
}

ULONG STDMETHODCALLTYPE IOleInPlaceFrame_Release( IOleInPlaceFrame *This )
{
   return( 1 );
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_GetWindow( IOleInPlaceFrame *This, HWND *lphwnd )
{
   OutputDebugValues( "GetWindow %i\n", OLESITE_FROM_INPLACEFRAME( This )->m_hWnd );

   *lphwnd = OLESITE_FROM_INPLACEFRAME( This )->m_hWnd;
   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_ContextSensitiveHelp( IOleInPlaceFrame *This, BOOL fEnterMode )
{
   OutputDebugValues( "Frame Help: %i\n", fEnterMode );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_GetBorder( IOleInPlaceFrame *This, LPRECT lprectBorder )
{
   //RECT rect;

   OutputDebugValues( "GetBorder\n" );

   //GetClientRect( OLESITE_FROM_INPLACEFRAME( This )->m_hWnd, &rect );
   GetClientRect( OLESITE_FROM_INPLACEFRAME( This )->m_hWnd, lprectBorder );

   //CopyRect( lprectBorder, &rect );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_RequestBorderSpace( IOleInPlaceFrame *This, LPCBORDERWIDTHS pborderwidths )
{
   OutputDebugValues( "RequestBorderSpace\n" );

   return NOERROR;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetBorderSpace( IOleInPlaceFrame *This, LPCBORDERWIDTHS pborderwidths )
{
   OutputDebugValues( "SetBorderSpace\n" );

   if( pborderwidths == NULL )
   {
      OutputDebugValues( "NULL Borders\n" );

      //TODO
      #if 0
         m_pApp->AddFrameLevelTools();
      #endif
   }
   else
   {
      RECT rect;
      COleSite  *pOleSite = OLESITE_FROM_INPLACEFRAME( This );

      OutputDebugValues( "Borders: %i, %i, %i, %i\n", pborderwidths->left, pborderwidths->top, pborderwidths->right, pborderwidths->bottom );

      pOleSite->m_Borders = *pborderwidths;

      #if 1
        GetClientRect( pOleSite->m_hWnd, &rect );

        #if 1
          rect.left   += pOleSite->m_Borders.left;
          rect.top    += pOleSite->m_Borders.top;
          rect.right  -= pOleSite->m_Borders.right;
          rect.bottom += pOleSite->m_Borders.bottom;
        #endif

        // tell the object its new size
        pOleSite->m_pInPlaceObject->lpVtbl->SetObjectRects( pOleSite->m_pInPlaceObject, &rect, &rect );
      #endif
   }

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetActiveObject( IOleInPlaceFrame *This, IOleInPlaceActiveObject *pActiveObject, LPCOLESTR pszObjName )
{
   OutputDebugValues( "ActiveObject: %p\n", pActiveObject );

   if( pActiveObject )
   {
      pActiveObject->lpVtbl->AddRef( pActiveObject );

      pActiveObject->lpVtbl->GetWindow( pActiveObject, &OLESITE_FROM_INPLACEFRAME( This )->m_hWndUIActiveObj );

      if( OLESITE_FROM_INPLACEFRAME( This )->m_hWndUIActiveObj )
      {
         SendMessage( OLESITE_FROM_INPLACEFRAME( This )->m_hWndUIActiveObj, WM_QUERYNEWPALETTE, 0, 0L );
      }
   }
   else
   {
      if( OLESITE_FROM_INPLACEFRAME( This )->m_pActiveObject )
      {
         OLESITE_FROM_INPLACEFRAME( This )->m_pActiveObject->lpVtbl->Release( OLESITE_FROM_INPLACEFRAME( This )->m_pActiveObject );
      }

      OLESITE_FROM_INPLACEFRAME( This )->m_hWndUIActiveObj = 0;
   }

   OLESITE_FROM_INPLACEFRAME( This )->m_pActiveObject = pActiveObject;

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_InsertMenus( IOleInPlaceFrame *This, HMENU hmenuShared, LPOLEMENUGROUPWIDTHS lpMenuWidths )
{
   OutputDebugValues( "InsertMenus: %i\n", hmenuShared );

   if( OLESITE_FROM_INPLACEFRAME( This )->m_hFileMenu )
   {
      AppendMenu( hmenuShared, MF_BYPOSITION | MF_POPUP, (UINT) OLESITE_FROM_INPLACEFRAME( This )->m_hFileMenu,   "&File" );
      lpMenuWidths->width[0] = 1;
   }

   if( OLESITE_FROM_INPLACEFRAME( This )->m_hViewMenu )
   {
      AppendMenu( hmenuShared, MF_BYPOSITION | MF_POPUP, (UINT) OLESITE_FROM_INPLACEFRAME( This )->m_hViewMenu,   "&View" );
      lpMenuWidths->width[2] = 1;
   }

   if( OLESITE_FROM_INPLACEFRAME( This )->m_hWindowMenu )
   {
      AppendMenu( hmenuShared, MF_BYPOSITION | MF_POPUP, (UINT) OLESITE_FROM_INPLACEFRAME( This )->m_hWindowMenu, "&Window" );
      lpMenuWidths->width[4] = 1;
   }

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetMenu( IOleInPlaceFrame *This, HMENU hmenuShared, HOLEMENU holemenu, HWND hwndActiveObject )
{
   HOLEMENU hMenu = OLESITE_FROM_INPLACEFRAME( This )->m_hMenu;

   OutputDebugValues( "SetMenu: %i %i\n", hmenuShared, holemenu );

   if( hmenuShared )
   {
      hMenu = hmenuShared;
   }
   else if( holemenu )
   {
      hMenu = holemenu;
   }

   SetMenu( OLESITE_FROM_INPLACEFRAME( This )->m_hWnd, (HMENU) hMenu ); //REVIEW HMENU CAST!!!

   OleSetMenuDescriptor( holemenu, OLESITE_FROM_INPLACEFRAME( This )->m_hWnd, hwndActiveObject, This, OLESITE_FROM_INPLACEFRAME( This )->m_pActiveObject );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_RemoveMenus( IOleInPlaceFrame *This, HMENU hmenuShared )
{
   OutputDebugValues( "RemoveMenus: %i\n", hmenuShared );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_SetStatusText( IOleInPlaceFrame *This, LPCOLESTR pszStatusText )
{
   OutputDebugValues( "SetStatusText: %p\n", pszStatusText );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_EnableModeless( IOleInPlaceFrame *This, BOOL fEnable )
{
   OutputDebugValues( "EnableModless\n" );

   return S_OK;
}

HRESULT STDMETHODCALLTYPE IOleInPlaceFrame_TranslateAccelerator( IOleInPlaceFrame *This, LPMSG lpmsg, WORD wID )
{
   OutputDebugValues( "TranslateAccelerators\n" );

   return S_OK;
}

COleSite *EmbedObject( HWND hWnd, const char *sClass, long iVerb, HWND hFrame )
{
   IOleObject *pOleObject = NULL;
   COleSite  *pOleSite = NULL;
   int iLen = strlen( sClass ) + 1;
   IID ClassID;
   BSTR wCLSID = hb_oleAnsiToWide( sClass );

   //OutputDebugValues( "EmbedObject '%s'\n", sClass );

   if( FAILED( CLSIDFromProgID( wCLSID, ( LPCLSID ) &ClassID ) ) )
   {
      OutputDebugString( "Invalid program ID\n" );
      hb_xfree( (void *) wCLSID );
      return 0;
   }

   hb_xfree( (void *) wCLSID );

   if( ( pOleSite = ( COleSite * ) GlobalAlloc( GMEM_FIXED, sizeof( COleSite ) ) ) == NULL )
   {
      OutputDebugString( "Allocation failed\n" );
      return( 0 );
   }

   memset( (void *) pOleSite, '\0', sizeof( COleSite ) );

   pOleSite->GetObjRect = COleSite_GetObjRect;

   pOleSite->m_OleClientSite.lpVtbl = &s_IOleClientSiteVtbl;

   pOleSite->m_OleInPlaceSiteEx.lpVtbl = &s_IOleInPlaceSiteExVtbl;

   pOleSite->m_OleInPlaceFrame.lpVtbl = &s_IOleInPlaceFrameVtbl;

   pOleSite->m_AdviseSink.lpVtbl = &s_IAdviseSinkExVtbl;

   pOleSite->m_Storage.lpVtbl = &s_IStorageVtbl;

   pOleSite->m_hWnd = hWnd;
   pOleSite->m_hFrameWnd = hFrame;

   pOleSite->m_pActiveObject = NULL;
   pOleSite->m_pInPlaceObject = NULL;

   pOleSite->m_hWndUIActiveObj = 0;
   pOleSite->m_hWndIPObj = 0;

   pOleSite->m_hMenu = GetMenu( hWnd );

   if( pOleSite->m_hMenu )
   {
      pOleSite->m_hFileMenu = GetSubMenu( pOleSite->m_hMenu, 0 );
   }

   pOleSite->m_dwDrawAspect = DVASPECT_CONTENT;

   pOleSite->m_fInPlaceActive = FALSE;

   if( StgCreateDocfile( NULL, STGM_READWRITE | STGM_TRANSACTED | STGM_SHARE_EXCLUSIVE, 0, &pOleSite->m_pObjStorage ) )
   {
      TraceLog( NULL, "Failed to create Storage\n" );

      pOleSite->m_pObjStorage = (IStorage *) &pOleSite->m_Storage;
   }

   if( SUCCEEDED( OleCreate( &ClassID, &IID_IOleObject, OLERENDER_DRAW, 0, ( IOleClientSite * ) pOleSite, pOleSite->m_pObjStorage, ( void** ) &pOleObject ) ) )
   {
      LPVIEWOBJECT2 pViewObject2 = NULL;
      RECT rect;

      pOleSite->m_pOleObject = pOleObject;

      pOleObject->lpVtbl->QueryInterface( pOleObject, &IID_IViewObject2, (void **) &pViewObject2 );
      pViewObject2->lpVtbl->SetAdvise( pViewObject2, pOleSite->m_dwDrawAspect, ADVF_PRIMEFIRST, (IAdviseSink *) &pOleSite->m_AdviseSink );

      // get the initial size of the object
      pViewObject2->lpVtbl->GetExtent( pViewObject2, pOleSite->m_dwDrawAspect, -1 /*lindex*/, NULL /*ptd*/, &pOleSite->m_sizel );

      pOleSite->GetObjRect( pOleSite, &rect );
      pViewObject2->lpVtbl->Release( pViewObject2 );

      pOleObject->lpVtbl->SetHostNames( pOleObject, L"My Host Name", 0 );

      // Let object know that it is embedded in an OLE container.
      if( SUCCEEDED( OleSetContainedObject( ( struct IUnknown * ) pOleObject, TRUE ) ) )
      {
         OutputDebugValues( "OleObject: %p ClientSite %p hWnd: %i\n", pOleObject, pOleSite, hWnd );

         if( SUCCEEDED( pOleObject->lpVtbl->DoVerb( pOleObject, iVerb, NULL, ( IOleClientSite * ) &pOleSite->m_OleClientSite, -1, hWnd, &rect ) ) )
         {
            OutputDebugValues( "*** Creation completed '%s\n", sClass );

            return pOleSite;
         }
      }

      // Something went wrong!
      //UnEmbedObject( hWnd );
      OutputDebugString( "Unexpected condition\n" );
      return( NULL );
   }

   GlobalFree( pOleSite );

   return( NULL );
}

HB_FUNC( __CREATEACTIVEX )
{
   long iVerb;
   COleSite  *pOleSite;

   if( ISNUM( 4 ) )
   {
      iVerb = hb_parnl( 4 );
   }
   else
   {
      iVerb = OLEIVERB_UIACTIVATE;//OLEIVERB_INPLACEACTIVATE;
   }

   pOleSite = EmbedObject( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), iVerb, ( HWND ) hb_parnl( 5 ) );

   if( pOleSite )
   {
      hb_retnl( (long) (void *) pOleSite );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 7001, "EmbedObject failed!", "__CREATEACTIVEX", 5, hb_paramError(1), hb_paramError(2), hb_paramError(3), hb_paramError(4), hb_paramError(5) );
   }
}

HB_FUNC( ACTIVEX_SHOW )
{
   COleSite *pOleSite = (COleSite *) hb_parnl( 1 );
   RECT rect;

   if( SUCCEEDED( pOleSite->m_pOleObject->lpVtbl->GetClientSite( pOleSite->m_pOleObject, ( IOleClientSite ** ) &pOleSite ) ) )
   {
      GetClientRect( pOleSite->m_hWnd, &rect );

      //OutputDebugValues( "ActiveX_Show() - pOleObject: %p ClientSite %p hWnd: %i\n", pOleSite->m_pOleObject, pOleSite, pOleSite->m_hWnd );

      hb_retnl( pOleSite->m_pOleObject->lpVtbl->DoVerb( pOleSite->m_pOleObject, OLEIVERB_SHOW, NULL, ( IOleClientSite * ) pOleSite, -1, pOleSite->m_hWnd, &rect ) );
   }
}

HB_FUNC( ACTIVEX_GETDISPATCH )
{
   COleSite *pOleSite = (COleSite *) hb_parnl( 1 );
   IDispatch  *pDispatch = NULL;

   if( SUCCEEDED( pOleSite && pOleSite->m_pOleObject->lpVtbl->QueryInterface( pOleSite->m_pOleObject, &IID_IDispatch, ( void** ) &pDispatch ) ) )
   {
      hb_retnl( (long) (void *) pDispatch );
   }
}

HB_FUNC( ACTIVEX_PROPERTIES )
{
   COleSite  *pOleSite = (COleSite *) hb_parnl( 1 );
   RECT rect;

   if( SUCCEEDED( pOleSite->m_pOleObject->lpVtbl->GetClientSite( pOleSite->m_pOleObject, ( IOleClientSite ** ) &pOleSite ) ) )
   {
      GetClientRect( pOleSite->m_hWnd, &rect );

      hb_retnl( pOleSite->m_pOleObject->lpVtbl->DoVerb( pOleSite->m_pOleObject, OLEIVERB_PROPERTIES, NULL, ( IOleClientSite * ) pOleSite, -1, pOleSite->m_hWnd, &rect ) );
   }
}

HB_FUNC( ACTIVEX_RESIZE )
{
   COleSite  *pOleSite = (COleSite *) hb_parnl( 1 );
   RECT rect;

   #if 0
      HDC hDC = GetDC( pOleSite->m_hWnd );
      SIZEL SizeInPix, SizeInHiMetric;

      GetClientRect( pOleSite->m_hWnd, &rect );

      SizeInPix.cx = rect.right;
      SizeInPix.cy = rect.bottom;

      XformSizeInPixelsToHimetric( hDC, &SizeInPix, &SizeInHiMetric );

      ReleaseDC( pOleSite->m_hWnd, hDC );

      pOleSite->m_pOleObject->lpVtbl->SetExtent( pOleSite->m_pOleObject, pOleSite->m_dwDrawAspect, &SizeInHiMetric );
   #else
      GetClientRect( pOleSite->m_hWnd, &rect );

      if( pOleSite->m_fInPlaceActive )
      {
         pOleSite->m_pActiveObject->lpVtbl->ResizeBorder( pOleSite->m_pActiveObject, &rect, (IOleInPlaceUIWindow *) &pOleSite->m_OleInPlaceFrame, TRUE );
      }

      GetClientRect( pOleSite->m_hWnd, &rect );

      #if 1
        rect.left   += pOleSite->m_Borders.left;
        rect.top    += pOleSite->m_Borders.top;
        rect.right  -= pOleSite->m_Borders.right;
        rect.bottom += pOleSite->m_Borders.bottom;
      #endif

      if( pOleSite->m_pInPlaceObject )
      {
         pOleSite->m_pInPlaceObject->lpVtbl->SetObjectRects( pOleSite->m_pInPlaceObject, &rect, &rect );
      }
      else
      {
         IOleInPlaceObject  *pOleInPlaceObject = NULL;

         if( SUCCEEDED( pOleSite->m_pOleObject->lpVtbl->QueryInterface( pOleSite->m_pOleObject, &IID_IOleInPlaceObject, ( void** ) &pOleInPlaceObject ) ) )
         {
            pOleInPlaceObject->lpVtbl->SetObjectRects( pOleInPlaceObject, &rect, &rect );
            pOleInPlaceObject->lpVtbl->Release( pOleInPlaceObject );
         }
      }
   #endif
}

HB_FUNC( ACTIVEX_CLOSE )
{
   COleSite  *pOleSite = (COleSite *) hb_parnl(1);
   IDispatch  *pDispatch = (IDispatch *) hb_parnl( 2 );
   LPVIEWOBJECT pViewObject = NULL;

   OutputDebugValues( "Site: %p, Disp: %p\n", pOleSite, pDispatch );

   if( pOleSite == NULL || pOleSite->m_pOleObject == NULL )
   {
      TraceLog( NULL, "ACTIVEX_CLOSE(), Invalid Arguments %p, %p!!!\n", pOleSite, pDispatch );
   }

   // TODO: Do we not need to worry about #if 0 segment?
   #if 0
      if( pOleSite->m_pInPlaceObject )
      {
         if( pOleSite->m_fInPlaceActive )
         {
            pOleSite->m_pInPlaceObject->lpVtbl->UIDeactivate( pOleSite->m_pInPlaceObject );
            pOleSite->m_pInPlaceObject->lpVtbl->InPlaceDeactivate( pOleSite->m_pInPlaceObject );
         }
         else if( pOleSite->m_pInPlaceObject )
         {
            pOleSite->m_pInPlaceObject->lpVtbl->Release( pOleSite->m_pInPlaceObject );
         }
      }
   #elif 0
      if( pOleSite->m_fInPlaceActive )
      {
         LPOLEINPLACEOBJECT pObject = NULL;

         if( SUCCEEDED( pOleSite->m_pOleObject->lpVtbl->QueryInterface( pOleSite->m_pOleObject, &IID_IOleInPlaceObject, (void **) &pObject ) ) )
         {
            pObject->lpVtbl->UIDeactivate( pObject );
            pObject->lpVtbl->InPlaceDeactivate( pObject );

            pObject->lpVtbl->Release( pObject );
         }
      }
   #endif

   pOleSite->m_pOleObject->lpVtbl->Close( pOleSite->m_pOleObject, hb_parnl(3) );

   pOleSite->m_pOleObject->lpVtbl->QueryInterface( pOleSite->m_pOleObject, &IID_IViewObject, (void **) &pViewObject );

   if( pViewObject )
   {
      pViewObject->lpVtbl->SetAdvise( pViewObject, pOleSite->m_dwDrawAspect, 0, NULL );
      pViewObject->lpVtbl->Release( pViewObject );
   }

   pDispatch->lpVtbl->Release( pDispatch );

   pOleSite->m_pOleObject->lpVtbl->Release( pOleSite->m_pOleObject );
   pOleSite->m_pOleObject = NULL;

   if( pOleSite->m_pObjStorage && pOleSite->m_pObjStorage != (IStorage *) &pOleSite->m_Storage )
   {
      pOleSite->m_pObjStorage->lpVtbl->Release( pOleSite->m_pObjStorage );
   }

   GlobalFree( (void *) pOleSite );

   OutputDebugValues( "*** Closed & Freed\n" );
}

#else

typedef HRESULT (CALLBACK *ATLAXWININIT)( void );
typedef HRESULT (CALLBACK *ATLAXCREATECONTROL)( LPCOLESTR, HWND, IStream*, IUnknown** );
typedef HRESULT (CALLBACK *ATLAXGETCONTROL)(HWND, IUnknown**);
typedef HRESULT (CALLBACK *ATLAXGETHOST)(HWND, IUnknown**);

static HMODULE hAtl = 0;

static ATLAXWININIT       pAtlAxWinInit       = NULL;
static ATLAXCREATECONTROL pAtlAxCreateControl = NULL;
static ATLAXGETCONTROL    pAtlAxGetControl    = NULL;
static ATLAXGETHOST       pAtlAxGetHost       = NULL;

#ifdef INPLACE_WINDOW
IUnknown *EmbedObject( HWND hWnd, char *sClass, long iVerb )
#else
IUnknown *EmbedObject( HWND hWnd, char *sClass, long iVerb, HWND *phControlWnd )
#endif
{
   IStream *pStream        = NULL;
   IUnknown *pUnkContainer = NULL;

  #ifdef INPLACE_WINDOW
   BSTR wClass             = hb_oleAnsiToWide( sClass );
  #else
   RECT Rect;
  #endif

   if( pAtlAxWinInit == NULL )
   {
      hAtl = LoadLibrary( "atl.dll" );

      pAtlAxWinInit       = (ATLAXWININIT)       GetProcAddress( hAtl, "AtlAxWinInit" );
      pAtlAxCreateControl = (ATLAXCREATECONTROL) GetProcAddress( hAtl, "AtlAxCreateControl" );
      pAtlAxGetControl    = (ATLAXGETCONTROL)    GetProcAddress( hAtl, "AtlAxGetControl" );
      pAtlAxGetHost       = (ATLAXGETHOST)       GetProcAddress( hAtl, "AtlAxGetHost" );

      pAtlAxWinInit();
   }

  #ifdef INPLACE_WINDOW
   pAtlAxCreateControl( wClass, hWnd, pStream, &pUnkContainer );

   hb_xfree( (void *) wClass );
  #else
   GetClientRect( hWnd, &Rect );
   *phControlWnd = CreateWindowEx( WS_EX_CONTROLPARENT, "AtlAxWin", sClass, WS_CHILD | WS_VISIBLE, 0, 0, Rect.right, Rect.bottom, hWnd, NULL, GetModuleHandle( NULL ), NULL );
   pAtlAxGetHost( *phControlWnd, &pUnkContainer );
  #endif

   return pUnkContainer;
}

HB_FUNC( __CREATEACTIVEX )
{
   long iVerb;
   IUnknown  *pOleSite;

  #ifdef INPLACE_WINDOW
  #else
   HWND hControlWnd = 0;
  #endif

   if( ISNUM( 3 ) )
   {
      iVerb = hb_parnl( 3 );
   }
   else
   {
      iVerb = OLEIVERB_UIACTIVATE;//OLEIVERB_INPLACEACTIVATE;
   }

  #ifdef INPLACE_WINDOW
   pOleSite = EmbedObject( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), iVerb );
  #else
   pOleSite = EmbedObject( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), iVerb, &hControlWnd );

   hb_stornl( (LONG) hControlWnd, 4 );
  #endif

   if( pOleSite )
   {
      hb_retnl( (long) (void *) pOleSite );
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 7001, "EmbedObject failed!", "__CREATEACTIVEX", 3, hb_paramError(1), hb_paramError(2), hb_paramError(3) );
   }
}

HB_FUNC( ACTIVEX_GETDISPATCH )
{
   HWND hWnd = (HWND) hb_parnl( 1 );
   IUnknown *pUnknown = NULL;

   if( SUCCEEDED( pAtlAxGetControl( hWnd, &pUnknown ) ) )
   {
      IDispatch  *pDispatch = NULL;

      if( SUCCEEDED( pUnknown->lpVtbl->QueryInterface( pUnknown, &IID_IDispatch, ( void** ) &pDispatch ) ) )
      {
         hb_retnl( (long) (void *) pDispatch );
      }

      pUnknown->lpVtbl->Release( pUnknown );
   }
}

HB_FUNC( ACTIVEX_GETOLEOBJECT )
{
   HWND hWnd = (HWND) hb_parnl( 1 );
   IUnknown *pUnknown = NULL;

   if( SUCCEEDED( pAtlAxGetControl( hWnd, &pUnknown ) ) )
   {
      IOleObject *pOleObject = NULL;

      if( SUCCEEDED( pUnknown->lpVtbl->QueryInterface( pUnknown, &IID_IOleObject, ( void** ) &pOleObject ) ) )
      {
         hb_retnl( (long) (void *) pOleObject );
      }

      pUnknown->lpVtbl->Release( pUnknown );
   }
}

HB_FUNC( ACTIVEX_PROPERTIES )
{
   HWND hWnd = (HWND) hb_parnl( 1 );
   IUnknown *pControlUnknown = NULL, *pHostUnknown = NULL;

   if( SUCCEEDED( pAtlAxGetControl( hWnd, &pControlUnknown ) ) && SUCCEEDED( pAtlAxGetHost( hWnd, &pHostUnknown ) ) )
   {
      IOleObject     *pOleObject = NULL;
      IOleClientSite *pOleSite   = NULL;

      if( SUCCEEDED( pControlUnknown->lpVtbl->QueryInterface( pControlUnknown, &IID_IOleObject, ( void** ) &pOleObject ) ) && SUCCEEDED( pHostUnknown->lpVtbl->QueryInterface( pHostUnknown, &IID_IOleClientSite, ( void** ) &pOleSite ) ) )
      {
         RECT rect;

         GetClientRect( hWnd, &rect );

         hb_retnl( pOleObject->lpVtbl->DoVerb( pOleObject, OLEIVERB_PROPERTIES, NULL, pOleSite, -1, hWnd, &rect ) );
      }

      if( pOleObject )
      {
         pOleObject->lpVtbl->Release( pOleObject );
      }

      if( pOleSite )
      {
         pOleSite->lpVtbl->Release( pOleSite );
      }
   }

   if( pControlUnknown )
   {
      pControlUnknown->lpVtbl->Release( pControlUnknown );
   }

   if( pHostUnknown )
   {
      pHostUnknown->lpVtbl->Release( pHostUnknown );
   }
}

#ifdef INPLACE_WINDOW
#else
HB_FUNC( ACTIVEX_RESIZE )
{
   HWND hWnd = (HWND) hb_parnl( 1 );
   HWND hControlWnd = (HWND) hb_parnl( 2 );
   RECT Rect;

   GetClientRect( hWnd, &Rect );

   ///TraceLog( NULL, "hWnd: %i, hControlwWnd: %i Bottom: %i, Right: %i\n", hWnd, hControlWnd, Rect.bottom, Rect.right );
   MoveWindow( hControlWnd, 0, 0, Rect.right, Rect.bottom, TRUE );
}
#endif

HB_FUNC( ACTIVEX_CLOSE )
{
   IUnknown  *pOleSite  = (IUnknown *)  hb_parnl(1);
   IDispatch *pDispatch = (IDispatch *) hb_parnl( 2 );

   pDispatch->lpVtbl->Release( pDispatch );
   pOleSite->lpVtbl->Release( pOleSite );

   #ifdef INPLACE_WINDOW
   #else
     DestroyWindow( (HWND) hb_parnl(4) );
   #endif
}
#endif
