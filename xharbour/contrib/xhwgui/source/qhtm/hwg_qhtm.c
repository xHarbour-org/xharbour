/*
 * QHTM wrappers for Harbour/HwGUI
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "qhtm.h"

extern  BOOL WINAPI QHTM_Initialize( HINSTANCE hInst );
extern  int WINAPI QHTM_MessageBox(HWND hwnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType );

typedef BOOL (WINAPI *QHTM_INITIALIZE)( HINSTANCE hInst );
typedef int (WINAPI *QHTM_MESSAGEBOX)(HWND hwnd, LPCTSTR lpText, LPCTSTR lpCaption, UINT uType );
typedef QHTMCONTEXT (WINAPI *QHTM_PRINTCREATECONTEXT)( UINT uZoomLevel );
typedef BOOL (WINAPI *QHTM_ENABLECOOLTIPS)( void );
typedef BOOL (WINAPI *QHTM_SETHTMLBUTTON)( HWND hwndButton );
typedef BOOL (WINAPI *QHTM_PRINTSETTEXT)( QHTMCONTEXT ctx, LPCTSTR pcszText );
typedef BOOL (WINAPI *QHTM_PRINTSETTEXTFILE)( QHTMCONTEXT ctx, LPCTSTR pcszText );
typedef BOOL (WINAPI *QHTM_PRINTSETTEXTRESOURCE)( QHTMCONTEXT ctx, HINSTANCE hInst, LPCTSTR pcszName );
typedef BOOL (WINAPI *QHTM_PRINTLAYOUT)( QHTMCONTEXT ctx, HDC dc, LPCRECT pRect, LPINT nPages );
typedef BOOL (WINAPI *QHTM_PRINTPAGE)( QHTMCONTEXT ctx, HDC hDC, UINT nPage, LPCRECT prDest );
typedef void (WINAPI *QHTM_PRINTDESTROYCONTEXT)( QHTMCONTEXT );

static HINSTANCE  hQhtmDll = NULL;

BOOL qhtmInit( char* cLibname )
{
   if( !hQhtmDll )
   {
      if( !cLibname )
         cLibname = "qhtm.dll";
      hQhtmDll = LoadLibrary( (LPCTSTR)cLibname );
      if( hQhtmDll )
      {
         QHTM_INITIALIZE pFunc = (QHTM_INITIALIZE) GetProcAddress( hQhtmDll,"QHTM_Initialize" );
         if( pFunc )
            return ( pFunc( GetModuleHandle( NULL ) ) )? 1:0;
      }
      else
      {
         MessageBox( GetActiveWindow(), "Library not loaded", cLibname, MB_OK | MB_ICONSTOP );
         return 0;
      }
   }
   return 1;
}

HB_FUNC( QHTM_INIT )
{
   char* cLibname = ( hb_pcount() < 1 )? NULL:hb_parc( 1 );
   hb_retl( qhtmInit( cLibname ) );
}

HB_FUNC( QHTM_END )
{
   if( hQhtmDll )
   {
      FreeLibrary( hQhtmDll );
      hQhtmDll = NULL;
   }
}

/*
   CreateQHTM( hParentWindow, nID, nStyle, x1, y1, nWidth, nHeight )
*/
HB_FUNC( CREATEQHTM )
{
   if( qhtmInit(NULL) )
   {
      HWND handle = CreateWindow( 
                       "QHTM_Window_Class_001",     /* predefined class  */
                       NULL,                        /* no window title   */
                       WS_CHILD | WS_VISIBLE | hb_parnl(3),    /* style  */
                       hb_parni(4), hb_parni(5),    /* x, y       */
                       hb_parni(6), hb_parni(7),    /* nWidth, nHeight */
                       (HWND) hb_parnl(1),           /* parent window    */ 
                       (HMENU) hb_parni(2),          /* control ID  */ 
                       GetModuleHandle( NULL ), 
                       NULL);

      hb_retnl( (LONG) handle );
   }
   else
      hb_retnl( 0 );
}

HB_FUNC( QHTM_GETNOTIFY )
{
   LPNMQHTM pnm = (LPNMQHTM) hb_parnl(1);

   hb_retc( (char*)pnm->pcszLinkText );
}

HB_FUNC( QHTM_SETRETURNVALUE )
{
   LPNMQHTM pnm = (LPNMQHTM) hb_parnl(1);
   pnm->resReturnValue = hb_parl(2);
}

void CALLBACK FormCallback( HWND hWndQHTM, LPQHTMFORMSubmit pFormSubmit, LPARAM lParam )
{
   PHB_DYNS pSymTest;
   PHB_ITEM aMetr = hb_itemArrayNew( pFormSubmit->uFieldCount );
   PHB_ITEM atemp, temp;
   int i;

   for( i=0;i<(int)pFormSubmit->uFieldCount;i++ )
   {
      atemp = hb_itemArrayNew( 2 );
      temp = hb_itemPutC( NULL, (char*)((pFormSubmit->parrFields+i)->pcszName) );
      hb_itemArrayPut( atemp, 1, temp );
      hb_itemRelease( temp );
      temp = hb_itemPutC( NULL, (char*)((pFormSubmit->parrFields+i)->pcszValue) );
      hb_itemArrayPut( atemp, 2, temp );
      hb_itemRelease( temp );

      hb_itemArrayPut( aMetr, i+1, atemp );
      hb_itemRelease( atemp );
   }

   HB_SYMBOL_UNUSED( lParam );

   if( ( pSymTest = hb_dynsymFind( "QHTMFORMPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWndQHTM );
      hb_vmPushString( (char*) pFormSubmit->pcszMethod,strlen(pFormSubmit->pcszMethod) );
      hb_vmPushString( (char*) pFormSubmit->pcszAction,strlen(pFormSubmit->pcszAction) );
      if( pFormSubmit->pcszName )
         hb_vmPushString( (char*) pFormSubmit->pcszName,strlen(pFormSubmit->pcszName) );
      else
         hb_vmPushNil();
      hb_vmPush( aMetr );
      hb_vmDo( 5 );
   }
   hb_itemRelease( aMetr );
}

// Wrappers to QHTM Functions

HB_FUNC( QHTM_MESSAGE )
{
   if( qhtmInit(NULL) )
   {
      char* cTitle = ( hb_pcount() < 2 )? "":hb_parc( 2 );
      UINT uType = ( hb_pcount() < 3 )? MB_OK:(UINT)hb_parni( 3 );
      QHTM_MESSAGEBOX pFunc = (QHTM_MESSAGEBOX) GetProcAddress( hQhtmDll,"QHTM_MessageBox" );

      if( pFunc )
         pFunc( GetActiveWindow(), hb_parc(1), cTitle, uType );
   }
}

HB_FUNC( QHTM_LOADFILE )
{
   if( qhtmInit(NULL) )
   {
      hb_retl( SendMessage( (HWND) hb_parnl(1), QHTM_LOAD_FROM_FILE, 0, (LPARAM)hb_parc(2) ) );
   }
}

HB_FUNC( QHTM_LOADRES )
{
   if( qhtmInit(NULL) )
   {
      hb_retl( SendMessage( (HWND) hb_parnl(1), QHTM_LOAD_FROM_RESOURCE,
                  (WPARAM)GetModuleHandle( NULL ), (LPARAM)hb_parc(2) ) );
   }
}

HB_FUNC( QHTM_ADDHTML )
{
   if( qhtmInit(NULL) )
   {
      SendMessage( (HWND) hb_parnl(1), QHTM_ADD_HTML, 0, (LPARAM)hb_parc(2) );
   }
}

HB_FUNC( QHTM_GETTITLE )
{
   if( qhtmInit(NULL) )
   {
      char szBuffer[ 256 ];
      SendMessage( (HWND) hb_parnl(1), QHTM_GET_HTML_TITLE, 256, (LPARAM)szBuffer );
      hb_retc( szBuffer );
   }
}

HB_FUNC( QHTM_GETSIZE )
{
   if( qhtmInit(NULL) )
   {
      SIZE size;

      if( SendMessage( (HWND) hb_parnl(1), QHTM_GET_DRAWN_SIZE, 0, (LPARAM)&size ) )
      {
          PHB_ITEM aMetr = hb_itemArrayNew( 2 );
          PHB_ITEM temp;

          temp = hb_itemPutNL( NULL, size.cx );
          hb_itemArrayPut( aMetr, 1, temp );
          hb_itemRelease( temp );

          temp = hb_itemPutNL( NULL, size.cy );
          hb_itemArrayPut( aMetr, 2, temp );
          hb_itemRelease( temp );

          hb_itemReturn( aMetr );
          hb_itemRelease( aMetr );
      }
      else
         hb_ret();
   }
}

HB_FUNC( QHTM_FORMCALLBACK )
{
   if( qhtmInit(NULL) )
   {
      hb_retl( SendMessage( (HWND) hb_parnl(1), QHTM_SET_OPTION, (WPARAM)QHTM_OPT_SET_FORM_SUBMIT_CALLBACK, (LPARAM)FormCallback ) );
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_ENABLECOOLTIPS )
{
   if( qhtmInit(NULL) )
   {
      QHTM_ENABLECOOLTIPS pFunc = (QHTM_ENABLECOOLTIPS) GetProcAddress( hQhtmDll,"QHTM_EnableCooltips" );
      if( pFunc )
         pFunc();
      else
         hb_retl(0);
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_SETHTMLBUTTON )
{
   if( qhtmInit(NULL) )
   {
      QHTM_SETHTMLBUTTON pFunc = (QHTM_SETHTMLBUTTON) GetProcAddress( hQhtmDll,"QHTM_SetHTMLButton" );
      if( pFunc )
         hb_retl( pFunc( (HWND) hb_parnl(1) ) );
      else
         hb_retl(0);
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_PRINTCREATECONTEXT )
{
   if( qhtmInit(NULL) )
   {
      QHTM_PRINTCREATECONTEXT pFunc = (QHTM_PRINTCREATECONTEXT) GetProcAddress( hQhtmDll,"QHTM_PrintCreateContext" );
      hb_retnl( (LONG) pFunc( (hb_pcount()==0)? 1:(UINT)hb_parni(1) ) );
   }
   else
      hb_retnl(0);
}

HB_FUNC( QHTM_PRINTSETTEXT )
{
   if( qhtmInit(NULL) )
   {
      QHTM_PRINTSETTEXT pFunc = (QHTM_PRINTSETTEXT) GetProcAddress( hQhtmDll,"QHTM_PrintSetText" );
      hb_retl( pFunc( (QHTMCONTEXT)hb_parnl(1),hb_parc(2) ) );
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_PRINTSETTEXTFILE )
{
   if( qhtmInit(NULL) )
   {
      QHTM_PRINTSETTEXTFILE pFunc = (QHTM_PRINTSETTEXTFILE) GetProcAddress( hQhtmDll,"QHTM_PrintSetTextFile" );
      hb_retl( pFunc( (QHTMCONTEXT)hb_parnl(1),hb_parc(2) ) );
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_PRINTSETTEXTRESOURCE )
{
   if( qhtmInit(NULL) )
   {
      QHTM_PRINTSETTEXTRESOURCE pFunc = (QHTM_PRINTSETTEXTRESOURCE) GetProcAddress( hQhtmDll,"QHTM_PrintSetTextResource" );
      hb_retl( pFunc( (QHTMCONTEXT)hb_parnl(1),GetModuleHandle( NULL ),hb_parc(2) ) );
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_PRINTLAYOUT )
{
   if( qhtmInit(NULL) )
   {
      HDC hDC = (HDC)hb_parnl(1);
      QHTMCONTEXT qhtmCtx = (QHTMCONTEXT) hb_parnl(2);
      RECT rcPage;
      int nNumberOfPages;
      QHTM_PRINTLAYOUT pFunc = (QHTM_PRINTLAYOUT) GetProcAddress( hQhtmDll,"QHTM_PrintLayout" );

      rcPage.left = rcPage.top = 0;
      rcPage.right = GetDeviceCaps( hDC, HORZRES );
      rcPage.bottom = GetDeviceCaps( hDC, VERTRES );

      pFunc( qhtmCtx, hDC, &rcPage, &nNumberOfPages );
      hb_retni( nNumberOfPages );
   }
   else
      hb_retnl(0);
}

HB_FUNC( QHTM_PRINTPAGE )
{
   if( qhtmInit(NULL) )
   {
      HDC hDC = (HDC)hb_parnl(1);
      QHTMCONTEXT qhtmCtx = (QHTMCONTEXT)hb_parnl(2);
      RECT rcPage;
      QHTM_PRINTPAGE pFunc = (QHTM_PRINTPAGE) GetProcAddress( hQhtmDll,"QHTM_PrintPage" );

      rcPage.left = rcPage.top = 0;
      rcPage.right = GetDeviceCaps( hDC, HORZRES );
      rcPage.bottom = GetDeviceCaps( hDC, VERTRES );

      hb_retl( pFunc( qhtmCtx, hDC, hb_parni(3)-1, &rcPage ) );
   }
   else
      hb_retl(0);
}

HB_FUNC( QHTM_PRINTDESTROYCONTEXT )
{
   if( qhtmInit(NULL) )
   {
      QHTM_PRINTDESTROYCONTEXT pFunc = (QHTM_PRINTDESTROYCONTEXT) GetProcAddress( hQhtmDll,"QHTM_PrintDestroyContext" );
      pFunc( (QHTMCONTEXT) hb_parnl(1) );
   }
}
