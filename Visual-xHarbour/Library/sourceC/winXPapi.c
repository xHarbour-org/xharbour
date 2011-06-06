/*
 * $Id$
 */
#define HB_OS_WIN_32_USED
#define TMT_FONT     210


#include <windows.h>
#include <winuser.h>
#include "item.api"
#include "hbapi.h"
#include "uxtheme.h"

HB_EXTERN_BEGIN
   extern HB_EXPORT LPSTR hb_oleWideToAnsi( BSTR wString );
   extern HB_EXPORT LPWSTR hb_oleAnsiToWide( const char *cString );
HB_EXTERN_END

//#if defined( __XCC__ )
typedef struct _DTTOPTS {
    DWORD dwSize;
    DWORD dwFlags;
    COLORREF crText;
    COLORREF crBorder;
    COLORREF crShadow;
    int iTextShadowType;
    POINT ptShadowOffset;
    int iBorderSize;
    int iFontPropId;
    int iColorPropId;
    int iStateId;
    BOOL fApplyOverlay;
    int iGlowSize;
    DWORD pfnDrawTextCallback;
    LPARAM lParam;
} DTTOPTS, *PDTTOPTS;
//#endif

BOOL Array2Rect(PHB_ITEM aRect, RECT *rc );
void Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect );
void Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize );

typedef HTHEME   (WINAPI* fnOpenThemeData)( HWND hwnd, LPCWSTR pszClassList);
typedef HRESULT  (WINAPI* fnCloseThemeData)(HTHEME hTheme);
typedef HRESULT  (WINAPI* fnDrawThemeBackground)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT *pRect, const RECT *pClipRect);
typedef HRESULT  (WINAPI* fnGetThemeBackgroundContentRect)( HTHEME hTheme, HDC hdc,    int iPartId, int iStateId, const RECT *pBoundingRect, RECT *pContentRect);

typedef HRESULT  (WINAPI* fnGetThemeBackgroundExtent)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT *pContentRect, RECT *pExtentRect);

typedef HRESULT  (WINAPI* fnDrawThemeText)  ( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, LPCWSTR pszText, int iCharCount, DWORD dwTextFlags, DWORD dwTextFlags2, const RECT *pRect);
typedef HRESULT  (WINAPI* fnDrawThemeTextEx)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, LPCWSTR pszText, int iCharCount, DWORD dwTextFlags, LPRECT pRect, const DTTOPTS *pOptions);

typedef HRESULT  (WINAPI* fnHitTestThemeBackground)( HTHEME hTheme, HDC hdc, int iPartId,    int iStateId, DWORD dwOptions, const RECT *pRect, HRGN hrgn,    POINT ptTest, WORD *pwHitTestCode);
typedef BOOL     (WINAPI* fnIsAppThemed)(void);
typedef COLORREF (WINAPI* fnGetThemeSysColor)(HTHEME hTheme, int iColorId);
typedef HRESULT  (WINAPI* fnGetThemeSysFont)(HTHEME hTheme, int iFontId, LOGFONTW *plf);
typedef HRESULT  (WINAPI* fnDrawThemeIcon)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT *pRect, HIMAGELIST himl, int iImageIndex);
typedef HRESULT  (WINAPI* fnGetThemeTextExtent)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, LPCWSTR pszText, int iCharCount, DWORD dwTextFlags, const RECT *pRect, RECT* pExtent);
typedef HRESULT  (WINAPI* fnDrawThemeParentBackground)( HWND hwnd, HDC hdc, RECT* prc);
typedef HRESULT  (WINAPI* fnDrawThemeEdge)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, const RECT *pDestRect, UINT uEdge, UINT uFlags, RECT *pContentRect);
typedef HRESULT  (WINAPI* fnGetThemeRect)( HTHEME hTheme, int iPartId, int iStateId, int iPropId, RECT *pRect);
typedef HRESULT  (WINAPI* fnGetThemePartSize)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, RECT *prc, enum THEMESIZE eSize, SIZE *psz);
typedef void     (WINAPI* fnSetThemeAppProperties)( DWORD dwFlags);
typedef DWORD    (WINAPI* fnGetThemeAppProperties)(void);
typedef HTHEME   (WINAPI* fnGetWindowTheme)( HWND hWnd);
typedef BOOL     (WINAPI* fnIsThemeActive)(void);
typedef HRESULT  (WINAPI* fnSetWindowTheme)( HWND hwnd, LPCWSTR pszSubAppName, LPCWSTR pszSubIdList );
typedef HRESULT  (WINAPI* fnEnableThemeDialogTexture)( HWND hwnd, DWORD dwFlags );
typedef HRESULT  (WINAPI* fnGetThemeColor)( HTHEME hTheme, int iPartId, int iStateId, int iPropId, COLORREF *pColor );
typedef HRESULT  (WINAPI* fnGetThemeMetric)( HTHEME hTheme, HDC hDC, int iPartId, int iStateId, int iPropId, int *piVal );
typedef HRESULT  (WINAPI* fnGetThemeInt)( HTHEME hTheme, int iPartId, int iStateId, int iPropId, int *piVal );
typedef HRESULT  (WINAPI* fnGetCurrentThemeName)( LPWSTR pszThemeFileName, int dwMaxNameChars, LPWSTR pszColorBuff, int cchMaxColorChars, LPWSTR pszSizeBuff, int cchMaxSizeChars );
typedef HRESULT  (WINAPI* fnGetThemeDocumentationProperty)( LPCWSTR pszThemeName, LPCWSTR pszPropertyName, LPWSTR pszValueBuff, int cchMaxValChars );
typedef HRESULT  (WINAPI* fnGetThemeFont)( HTHEME hTheme, HDC hdc, int iPartId, int iStateId, int iPropId, LOGFONT *pFont );

static HINSTANCE hUxTheme;

BOOL Array2Rect(PHB_ITEM aRect, RECT *rc )
{
   if (HB_IS_ARRAY(aRect) && hb_arrayLen(aRect) == 4) {
      rc->left   = hb_arrayGetNL(aRect,1);
      rc->top    = hb_arrayGetNL(aRect,2);
      rc->right  = hb_arrayGetNL(aRect,3);
      rc->bottom = hb_arrayGetNL(aRect,4);
      return TRUE ;
   }

   return FALSE;
}

void  Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc->left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc->top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc->right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc->bottom));
   hb_itemRelease(element);
}

void Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);

}

//-------------------------------------------------------------------------------------------
void InitUxTheme( void )
{
   if( hUxTheme == NULL )
   {
       hUxTheme = (HINSTANCE ) LoadLibraryEx( "uxtheme.dll", NULL, 0 );
       hb_retnl( (LONG) hUxTheme );
   }
}

void EndUxTheme( void )
{
   if( hUxTheme != NULL )
   {
      FreeLibrary( hUxTheme );
   }
}

HB_FUNC( INITUXTHEME )
{
   InitUxTheme();
}

HB_FUNC( ENDUXTHEME )
{
   EndUxTheme();
}

HB_FUNC( ISTHEMEACTIVE )
{
   BOOL bRet = (BOOL) FALSE ;

   if( hUxTheme == NULL )
   {
       hUxTheme = (HINSTANCE) LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnIsThemeActive pfn = (fnIsThemeActive) GetProcAddress( hUxTheme, "IsThemeActive") ;

       if( pfn )
       {
           bRet = (BOOL) pfn();
       }
   }

   hb_retl( bRet );
}

//-------------------------------------------------------------------------------------------

HB_FUNC( GETTHEMESYSFONT )
{
   HFONT hFont = NULL;
   HRESULT hRet;

   if( hUxTheme == NULL )
   {
       hUxTheme = (HINSTANCE) LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeSysFont pfn = (fnGetThemeSysFont) GetProcAddress( hUxTheme, "GetThemeSysFont") ;

       if( pfn )
       {
           LOGFONTW pFont;

           hRet = pfn( (HTHEME) hb_parnl(1), hb_parni(2), &pFont );

           if( hRet == S_OK )
           {
              hFont = CreateFontIndirectW( &pFont );
           }
       }
   }

   if( hFont )
   {
      hb_retnl( (long) hFont );
   }
}

//-------------------------------------------------------------------------------------------

HB_FUNC( GETTHEMEFONT )
{
   HFONT hFont = NULL;
   HRESULT hRet;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeFont pfn = (fnGetThemeFont) GetProcAddress( hUxTheme, "GetThemeFont") ;

       if( pfn )
       {
           LPVOID Buffer;
           LOGFONT pFont;
           OutputDebugString( "1" );
           hRet = pfn( (HTHEME) hb_parnl(1), ISNIL(2) ? NULL : (HDC) hb_parnl(2), hb_parni(3), hb_parni(4), TMT_FONT, &pFont );

//FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, hRet, 0, (LPTSTR) &Buffer, 0, NULL );
//OutputDebugString( (LPCSTR) Buffer );

           if( hRet == S_OK )
           {
              hFont = CreateFontIndirect( &pFont );
           }
       }
   }

   if( hFont )
   {
      hb_retnl( (long) hFont );
   }
}

//-------------------------------------------------------------------------------------------

HB_FUNC( GETTHEMECOLOR )
{
   COLORREF pColor = 0;
   HRESULT nRet;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeColor pfn = (fnGetThemeColor) GetProcAddress( hUxTheme, "GetThemeColor") ;
       if( pfn )
       {
           nRet = pfn( (HTHEME) hb_parnl(1), hb_parni(2), hb_parni(3), hb_parni(4), &pColor );
       }
   }

   if( nRet == S_OK )
   {
      hb_retnl( (LONG) pColor );
   }
}

//----------------------------------------------------------------

HB_FUNC( OPENTHEMEDATA )
{
   HTHEME nRet;
   HWND hWnd = (HWND) hb_parnl(1);

   LPCWSTR pszClassList = (LPCWSTR) hb_parcx(2);

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnOpenThemeData pfn = (fnOpenThemeData) GetProcAddress( hUxTheme, "OpenThemeData") ;

       if( pfn )
       {
           nRet = (HTHEME) pfn( hWnd, pszClassList );
       }
   }

   if( nRet != NULL )
   {
      hb_retnl( (LONG) nRet );
   }
}

//-------------------------------------------------------------------------------------------

HB_FUNC( CLOSETHEMEDATA )
{
   HTHEME nRet;
   HTHEME hTheme   = (HTHEME) hb_parnl(1);

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnCloseThemeData pfn = (fnCloseThemeData) GetProcAddress( hUxTheme, "CloseThemeData") ;

       if( pfn )
       {
           nRet = (HTHEME) pfn( hTheme );
       }
   }

   if( nRet != NULL )
   {
      hb_retnl( (LONG) nRet );
   }
}

//-------------------------------------------------------------------------------------------

HB_FUNC( DRAWTHEMEBACKGROUND )
{
   HRESULT nRet;

   HTHEME hTheme   = (HTHEME) hb_parnl(1);
   HDC    hDC      = (HDC) hb_parnl(2);
   int    iPartId  = hb_parni(3);
   int    iStateId = hb_parni(4);

   RECT pRect;

   Array2Rect( hb_param(5,HB_IT_ARRAY), &pRect );
   //Array2Rect( hb_param(6,HB_IT_ARRAY), &pClipRect );

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnDrawThemeBackground pfn = (fnDrawThemeBackground) GetProcAddress( hUxTheme, "DrawThemeBackground") ;

       if( pfn )
       {
           nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, &pRect, NULL );
       }
   }

   hb_retl( (nRet==S_OK) );
}


// DrawThemeTextEx( hTheme, hdc, nPartId, nStateId, cText nFlags, aRect, pOptions )
HB_FUNC( DRAWTHEMETEXTEX )
{
   HRESULT nRet;

   HTHEME hTheme   = (HTHEME) hb_parnl(1);
   HDC    hDC      = (HDC) hb_parnl(2);
   int    iPartId  = hb_parni(3);
   int    iStateId = hb_parni(4);

   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
      fnDrawThemeTextEx pfn = (fnDrawThemeTextEx) GetProcAddress( hUxTheme, "DrawThemeTextEx") ;

      if( pfn )
      {
         RECT pRect;
         DTTOPTS *pOptions;
         LPCWSTR wText;

         Array2Rect( hb_param(7,HB_IT_ARRAY), &pRect );
         pOptions = (DTTOPTS*) hb_param( 8, HB_IT_STRING )->item.asString.value;
         wText = hb_oleAnsiToWide( hb_parc(5) );
         nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, wText, -1, (DWORD) hb_parnl(6), &pRect, NULL );
         hb_xfree( (void *) wText );
      }
   }

   hb_retl( (nRet==S_OK) );
}


// DrawThemeText( hTheme, hdc, nPartId, nStateId, cText nFlags, aRect )
HB_FUNC( DRAWTHEMETEXT )
{
   HRESULT nRet;

   HTHEME hTheme   = (HTHEME) hb_parnl(1);
   HDC    hDC      = (HDC) hb_parnl(2);
   int    iPartId  = hb_parni(3);
   int    iStateId = hb_parni(4);

   RECT pRect;

   Array2Rect( hb_param(7,HB_IT_ARRAY), &pRect );

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnDrawThemeText pfn = (fnDrawThemeText) GetProcAddress( hUxTheme, "DrawThemeText") ;

       if( pfn )
       {
           LPCWSTR wText = hb_oleAnsiToWide( hb_parc(5) );
           nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, wText , -1, (DWORD) hb_parnl(6), 0, &pRect );
           hb_xfree( (void *) wText );
       }
   }

   hb_retl( (nRet==S_OK) );
}

HB_FUNC( GETTHEMEMETRIC )
{
   int nRet = 0;

   HTHEME hTheme   = (HTHEME) hb_parnl(1);
   HDC    hDC      = (HDC) hb_parnl(2);
   int    iPartId  = hb_parni(3);
   int    iStateId = hb_parni(4);
   int    iPropId  = hb_parni(5);
   int    piVal;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeMetric pfn = (fnGetThemeMetric) GetProcAddress( hUxTheme, "GetThemeMetric") ;

       if( pfn )
       {
           pfn( hTheme, hDC, iPartId, iStateId, iPropId, &piVal );
           nRet = piVal;
       }
   }

   hb_retnl( nRet );
}

HB_FUNC( GETTHEMEINT )
{

   HTHEME hTheme   = (HTHEME) hb_parnl(1);
   int    iPartId  = hb_parni(2);
   int    iStateId = hb_parni(3);
   int    iPropId  = hb_parni(4);
   int    iVal;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeInt pfn = (fnGetThemeInt) GetProcAddress( hUxTheme, "GetThemeInt" );

       if( pfn )
       {
           pfn( hTheme, iPartId, iStateId, iPropId, &iVal );
       }
   }

   hb_retni( iVal );
}


HB_FUNC( GETTHEMERECT )
{
   HRESULT nRet;
   HTHEME hTheme = (HTHEME) hb_parnl(1);
   int iPartId  = hb_parni(2);
   int iStateId = hb_parni(3);
   int iPropId  = hb_parni(4);
   PHB_ITEM pArray = hb_param( 5, HB_IT_ARRAY );
   RECT Rect = { 0 };

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeRect pfn = (fnGetThemeRect) GetProcAddress( hUxTheme, "GetThemeRect" ) ;

       if( pfn )
       {
           nRet = (HRESULT) pfn( hTheme, iPartId, iStateId, iPropId, &Rect );
           Rect2ArrayEx( &Rect, pArray );
       }
   }

   hb_retl( nRet == S_OK );
}

HB_FUNC( GETTHEMEPARTSIZE )
{
   HRESULT nRet;
   HTHEME hTheme = (HTHEME) hb_parnl(1);
   HDC    hDC    = (HDC) hb_parnl(2);
   int iPartId   = hb_parni(3);
   int iStateId  = hb_parni(4);
   SIZE pSize;

   PHB_ITEM pArray = hb_param( 7, HB_IT_ARRAY );

   if( hUxTheme == NULL )
   {
      hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
      fnGetThemePartSize pfn = (fnGetThemePartSize) GetProcAddress( hUxTheme, "GetThemePartSize") ;

      if( pfn )
      {
         RECT pRect;

         if( ISARRAY(5) )
         {
            Array2Rect( hb_param( 5, HB_IT_ARRAY ), &pRect );
         }

         nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, ( ISARRAY(5) ? &pRect : NULL ), (enum THEMESIZE) hb_parni(6), &pSize );
         Size2ArrayEx( &pSize, pArray );
      }
   }

   hb_retl( (nRet==S_OK) );
}


HB_FUNC( DRAWTHEMEPARENTBACKGROUND )
{
   HRESULT nRet;
   HWND   hWnd     = (HWND) hb_parnl(1);
   HDC    hDC      = (HDC) hb_parnl(2);
   RECT pRect;

   if (ISARRAY( 7 ) )
   {
      Array2Rect( hb_param(3,HB_IT_ARRAY), &pRect );
   }

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnDrawThemeParentBackground pfn = (fnDrawThemeParentBackground) GetProcAddress( hUxTheme, "DrawThemeParentBackground") ;

       if( pfn )
       {
           nRet = (HRESULT) pfn( hWnd, hDC, &pRect );
       }
   }

   hb_retl( (nRet==S_OK) );
}

HB_FUNC( GETCURRENTTHEMENAME )
{
   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
      fnGetCurrentThemeName pfn = (fnGetCurrentThemeName) GetProcAddress( hUxTheme, "GetCurrentThemeName" );
      if( pfn )
      {
         HRESULT hRet;
         WCHAR currentTheme[MAX_PATH];
         WCHAR currentColor[MAX_PATH];
         WCHAR currentSize[MAX_PATH];
         //hRet = (HRESULT) pfn( currentTheme, sizeof(currentTheme) / sizeof(WCHAR), currentColor, sizeof(currentColor) / sizeof(WCHAR), currentSize, sizeof(currentSize) / sizeof(WCHAR) );

         hRet = (HRESULT) pfn( currentTheme, 256, currentColor, 256, currentSize, 256 );
         if( hRet == S_OK )
         {
            hb_storclenAdopt( hb_oleWideToAnsi( currentTheme ), wcslen( currentTheme ), 1 );
            hb_storclenAdopt( hb_oleWideToAnsi( currentColor ), wcslen( currentColor ), 2 );
            hb_storclenAdopt( hb_oleWideToAnsi( currentSize  ), wcslen( currentSize  ), 3 );
         }
         else
         {
            //LPVOID Buffer;
            //FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, hRet, 0, (LPTSTR) &Buffer, 0, NULL );
            //OutputDebugString( (LPCSTR) Buffer );
            hb_retni( (UINT) hRet );
         }
      }
   }
}

HB_FUNC( GETTHEMEDOCUMENTATIONPROPERTY )
{
   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeDocumentationProperty pfn = (fnGetThemeDocumentationProperty) GetProcAddress( hUxTheme, "GetThemeDocumentationProperty" );

       if( pfn )
       {
          HRESULT hRet;
          WCHAR ValueBuffer[MAX_PATH];
          hRet = (HRESULT) pfn( (LPCWSTR) hb_parcx(1), (LPCWSTR) hb_parcx(2), ValueBuffer, MAX_PATH );

          if( hRet == S_OK )
          {
             hb_retclenAdopt( hb_oleWideToAnsi( ValueBuffer ), wcslen( ValueBuffer ) );
          }
       }
    }
 }


HB_FUNC( GETTHEMEBACKGROUNDCONTENTRECT )
{
   HRESULT nRet;
   HTHEME hTheme = (HTHEME) hb_parnl(1);
   HDC hDC = (HDC) hb_parnl(2);
   int iPartId  = hb_parni(3);
   int iStateId = hb_parni(4);
   PHB_ITEM pBound = hb_param( 5, HB_IT_ARRAY );
   PHB_ITEM pCont  = hb_param( 6, HB_IT_ARRAY );

   RECT pBoundingRect;
   RECT pContentRect;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeBackgroundContentRect pfn = (fnGetThemeBackgroundContentRect) GetProcAddress( hUxTheme, "GetThemeBackgroundContentRect") ;

       if( pfn )
       {
           Array2Rect( pBound, &pBoundingRect );
           nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, &pBoundingRect, &pContentRect );
           Rect2ArrayEx( &pContentRect, pCont );
       }
   }

   hb_retl( (nRet==S_OK) );
}

HB_FUNC( GETTHEMEBACKGROUNDEXTENT )
{
   HRESULT nRet;
   HTHEME hTheme = (HTHEME) hb_parnl(1);
   HDC hDC = (HDC) hb_parnl(2);
   int iPartId  = hb_parni(3);
   int iStateId = hb_parni(4);
   PHB_ITEM aContentRect = hb_param( 5, HB_IT_ARRAY );
   PHB_ITEM aExtentRect  = hb_param( 6, HB_IT_ARRAY );

   RECT pContentRect;
   RECT pExtentRect;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetThemeBackgroundExtent pfn = (fnGetThemeBackgroundExtent) GetProcAddress( hUxTheme, "GetThemeBackgroundExtent") ;

       if( pfn )
       {
           Array2Rect( aContentRect, &pContentRect );
           nRet = (HRESULT) pfn( hTheme, hDC, iPartId, iStateId, &pContentRect, &pExtentRect );
           Rect2ArrayEx( &pExtentRect, aExtentRect );
       }
   }

   hb_retl( (nRet==S_OK) );
}

//-------------------------------------------------------------------------------------------

HB_FUNC( SETWINDOWTHEME )
{
   HRESULT nRet;

   HWND hWnd = (HWND) hb_parnl(1);
   LPCWSTR pszSubAppName = (LPCWSTR) hb_oleAnsiToWide( hb_parc(2) );
   LPCWSTR pszSubIdList  = (LPCWSTR) hb_oleAnsiToWide( hb_parc(3) );

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnSetWindowTheme pfn = (fnSetWindowTheme) GetProcAddress( hUxTheme, "SetWindowTheme") ;

       if( pfn )
       {
          nRet = (HRESULT) pfn( hWnd, pszSubAppName, pszSubIdList );
       }
   }

   hb_retl( (nRet==S_OK) );
}

//-------------------------------------------------------------------------------------------

HB_FUNC( GETWINDOWTHEME )
{
   HRESULT nRet = 0;

   HWND hWnd = (HWND) hb_parnl(1);

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnGetWindowTheme pfn = (fnGetWindowTheme) GetProcAddress( hUxTheme, "GetWindowTheme") ;

       if( pfn )
       {
          nRet = (HRESULT) pfn( hWnd );
       }
   }

   hb_retnl( nRet );
}

//-------------------------------------------------------------------------------------------

HB_FUNC( SETTHEMEAPPPROPERTIES )
{
   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnSetThemeAppProperties pfn = (fnSetThemeAppProperties) GetProcAddress( hUxTheme, "SetThemeAppProperties") ;

       if( pfn )
       {
          pfn( (DWORD) hb_parnl(1) );
       }
   }
}


HB_FUNC( GETTHEMESYSCOLOR )
{
   COLORREF nRet = 0;

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
      fnGetThemeSysColor pfn = (fnGetThemeSysColor) GetProcAddress( hUxTheme, "GetThemeSysColor") ;

      if( pfn )
      {
         nRet = (COLORREF) pfn( (HTHEME) hb_parnl(1), hb_parni(2) );
      }
   }

   hb_retnl( (LONG) nRet );
}

//-------------------------------------------------------------------------------------------

HB_FUNC( ENABLETHEMEDIALOGTEXTURE )
{
   HRESULT nRet;

   HWND hWnd = (HWND) hb_parnl(1);
   DWORD flags = hb_parnl(2);

   if( hUxTheme == NULL )
   {
       hUxTheme = LoadLibraryEx( "uxtheme.dll", NULL, 0 );
   }

   if( hUxTheme )
   {
       fnEnableThemeDialogTexture pfn = (fnEnableThemeDialogTexture) GetProcAddress( hUxTheme, "EnableThemeDialogTexture") ;

       if( pfn )
       {
           nRet = (HRESULT) pfn( hWnd, flags );
       }
   }

   hb_retl( (nRet==S_OK) );
}

//FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, hRet, NULL, (LPTSTR) &Buffer, NULL, NULL );
//OutputDebugString( Buffer );
