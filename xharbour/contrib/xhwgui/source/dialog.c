/*
 *$Id: dialog.c,v 1.3 2003/12/01 12:41:31 lculik Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * C level dialog boxes functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
// #define OEMRESOURCE
#include <windows.h>
#if defined(__MINGW32__)
   #include <prsht.h>
#endif
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "item.api"
#include "guilib.h"

LRESULT WINAPI ModalDlgProc( HWND, UINT, WPARAM, LPARAM );
LRESULT CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM) ;
LRESULT CALLBACK PSPProc (HWND, UINT, WPARAM, LPARAM);
extern PHB_ITEM GetObjectVar( PHB_ITEM pObject, char* varname );
extern void SetObjectVar( PHB_ITEM pObject, char* varname, PHB_ITEM pValue );
extern HMODULE hModule ;

HWND aDialogs[ 20 ];
int iDialogs = 0;

HB_FUNC ( HWG_DIALOGBOX )
{
   char *cIdDlg;
   PHB_ITEM pObj = hb_param( 2, HB_IT_OBJECT );

   cIdDlg = hb_itemGetCPtr( GetObjectVar( pObj, "TITLE" ) );

   DialogBox( hModule, cIdDlg, (HWND) hb_parnl( 1 ), (DLGPROC) ModalDlgProc );
}

/*  Creates modeless dialog
    CreateDialog( hParentWindow, aDialog )
*/
HB_FUNC ( HWG_CREATEDIALOG )
{
   HWND hDlg;
   PHB_ITEM pObj = hb_param( 2, HB_IT_OBJECT );
   char *cIdDlg = hb_itemGetCPtr( GetObjectVar( pObj, "TITLE" ) );

   hDlg = CreateDialog( hModule, cIdDlg, (HWND) hb_parnl( 1 ), 
      (DLGPROC) DlgProc ); 

   ShowWindow( hDlg, SW_SHOW);
   hb_retnl( (LONG) hDlg );
}

HB_FUNC ( HWG_ENDDIALOG )
{
   EndDialog( (HWND) hb_parnl( 1 ), TRUE );
}

HB_FUNC ( GETDLGITEM )
{
   HWND hWnd = GetDlgItem(
                 (HWND) hb_parnl( 1 ),	// handle of dialog box
                 hb_parni( 2 )	        // identifier of control
               );
   hb_retnl( (LONG) hWnd );
}

HB_FUNC ( GETDLGCTRLID )
{
   hb_retni( GetDlgCtrlID( (HWND) hb_parnl( 1 ) ) );
}

HB_FUNC ( SETDLGITEMTEXT )
{
    SetDlgItemText(
       (HWND) hb_parnl( 1 ),	// handle of dialog box
       hb_parni( 2 ),	        // identifier of control
       (LPCTSTR) hb_parc( 3 ) 	// text to set
    );
}

HB_FUNC ( SETDLGITEMINT )
{
    SetDlgItemInt(
       (HWND) hb_parnl( 1 ),	// handle of dialog box
       hb_parni( 2 ),	        // identifier of control
       (UINT) hb_parni( 3 ), 	// text to set
       ( hb_pcount()<4 || ISNIL(4) || !hb_parl(4) )? 0:1
    );
}

HB_FUNC ( GETDLGITEMTEXT )
{
   USHORT iLen = hb_parni( 3 );
   char *cText = (char*) hb_xgrab( iLen+1 );

   GetDlgItemText(
       (HWND) hb_parnl( 1 ),	// handle of dialog box
    hb_parni( 2 ),          	// identifier of control
    (LPTSTR) cText,       	// address of buffer for text
    iLen                   	// maximum size of string
   );	
   hb_retc( cText );
   hb_xfree( cText );
}

HB_FUNC ( GETEDITTEXT )
{
   HWND   hDlg = (HWND) hb_parnl( 1 );
   int    id = hb_parni( 2 );
   USHORT iLen = (USHORT)SendMessage( GetDlgItem( hDlg,id ), WM_GETTEXTLENGTH, 0, 0 );
   char *cText = (char*) hb_xgrab( iLen+2 );

   GetDlgItemText(
       hDlg,	// handle of dialog box
    id,        	// identifier of control
    (LPTSTR) cText,       	// address of buffer for text
    iLen+1                   	// maximum size of string
   );	
   hb_retc( cText );
   hb_xfree( cText );
}

HB_FUNC ( CHECKDLGBUTTON )
{
    CheckDlgButton(
       (HWND) hb_parnl( 1 ),	// handle of dialog box
       hb_parni( 2 ),	        // identifier of control
       ( hb_parl( 3 ) )? BST_CHECKED:BST_UNCHECKED 	// value to set
    );
}

HB_FUNC ( CHECKRADIOBUTTON )
{
    CheckRadioButton(
       (HWND) hb_parnl( 1 ),	// handle of dialog box
       hb_parni( 2 ),	        // identifier of first radio button in group
       hb_parni( 3 ),	        // identifier of last radio button in group
       hb_parni( 4 )	        // identifier of radio button to select
    );
}

HB_FUNC ( ISDLGBUTTONCHECKED )
{
  UINT nRes = IsDlgButtonChecked(
                  (HWND) hb_parnl( 1 ),       // handle of dialog box
                   hb_parni( 2 )               // button identifier
              );
  if( nRes == BST_CHECKED )
     hb_retl( TRUE );
  else
     hb_retl( FALSE );
}

HB_FUNC ( COMBOADDSTRING )
{
   char *cString = hb_parc( 2 );
   SendMessage( (HWND) hb_parnl( 1 ), CB_ADDSTRING, 0, (LPARAM) cString );
}

HB_FUNC ( COMBOSETSTRING )
{
   SendMessage( (HWND) hb_parnl( 1 ), CB_SETCURSEL, (WPARAM) hb_parni(2)-1, 0);
}

HB_FUNC ( GETNOTIFYCODE )
{
   hb_retnl( (LONG) (((NMHDR *) hb_parnl(1))->code) );
}

LPWORD lpwAlign ( LPWORD lpIn)
{
  ULONG ul;

  ul = (ULONG) lpIn;
  ul +=3;
  ul >>=2;
  ul <<=2;
  return (LPWORD) ul;
}

int nCopyAnsiToWideChar ( LPWORD lpWCStr, LPCSTR lpAnsiIn )
{
   int CodePage = GetACP();
   LPWSTR pszDst;
   int nDstLen = MultiByteToWideChar( CodePage, 0, lpAnsiIn, -1, NULL, 0 );
   int i;

   pszDst = ( LPWSTR ) hb_xgrab( nDstLen*2 );

   MultiByteToWideChar( CodePage, 0, lpAnsiIn, -1, pszDst, nDstLen );

   for( i=0;i<nDstLen;i++ )
      *( lpWCStr+i ) = *( pszDst+i );

   hb_xfree( pszDst );      
   return nDstLen;
/*
  int nChar = 0;

  do {
    *lpWCStr++ = (WORD) *lpAnsiIn;
    nChar++;
  } while (*lpAnsiIn++);

  return nChar;
*/
}

LPDLGTEMPLATE CreateDlgTemplate( PHB_ITEM pObj, int x1, int y1, int dwidth, int dheight, LONG lStyle )
{
   PWORD p, pdlgtemplate;
   PHB_ITEM pControls, pControl, temp;
   LONG baseUnit = GetDialogBaseUnits();
   int baseunitX = LOWORD( baseUnit ), baseunitY = HIWORD( baseUnit );
   int nchar, nControls, i;
   long lTemplateSize = 36;
   LONG lExtStyle;

   x1 = (x1 * 4) / baseunitX;
   dwidth = (dwidth * 4) / baseunitX;
   y1 = (y1 * 8) / baseunitY;
   dheight = (dheight * 8) / baseunitY;

   pControls = hb_itemNew( GetObjectVar( pObj, "ACONTROLS" ) );
   nControls = pControls->item.asArray.value->ulLen;

   temp = GetObjectVar( pObj, "TITLE" );
   if( hb_itemType( temp ) == HB_IT_STRING )
      lTemplateSize += temp->item.asString.length * 2;
   else
      lTemplateSize += 2;

   for( i=0;i<nControls;i++ )
   {
      lTemplateSize += 36;
      pControl = (PHB_ITEM) (pControls->item.asArray.value->pItems + i);
      temp =  GetObjectVar( pControl, "WINCLASS" );
      lTemplateSize += temp->item.asString.length * 2;
      temp =  GetObjectVar( pControl, "TITLE" );
      if( hb_itemType( temp ) == HB_IT_STRING )
         lTemplateSize += temp->item.asString.length * 2;
      else
         lTemplateSize += 2;
   }

   pdlgtemplate = (PWORD) LocalAlloc (LPTR,lTemplateSize );
   p = pdlgtemplate;
   *p++ = 1;          // DlgVer
   *p++ = 0xFFFF;     // Signature
   *p++ = 0;          // LOWORD HelpID
   *p++ = 0;          // HIWORD HelpID
   *p++ = 0;          // LOWORD (lExtendedStyle)
   *p++ = 0;          // HIWORD (lExtendedStyle)
   *p++ = LOWORD (lStyle);
   *p++ = HIWORD (lStyle);
   *p++ = nControls;  // NumberOfItems
   *p++ = x1;         // x
   *p++ = y1;         // y
   *p++ = dwidth;     // cx
   *p++ = dheight;    // cy
   *p++ = 0;          // Menu
   *p++ = 0;          // Class

   // Copy the title of the dialog box.

   temp = GetObjectVar( pObj, "TITLE" );
   if( hb_itemType( temp ) == HB_IT_STRING )
   {
      nchar = nCopyAnsiToWideChar (p, TEXT(hb_itemGetCPtr( temp )));
      p += nchar;
   }
   else
      *p++ = 0; 

   /* {
      char res[20];
      sprintf( res,"nControls: %d",nControls );
      MessageBox( GetActiveWindow(), res, "", MB_OK | MB_ICONINFORMATION );
   } */

   for( i=0;i<nControls;i++ )
   {
      pControl = (PHB_ITEM) (pControls->item.asArray.value->pItems + i);
      temp = hb_itemPutNI( NULL, -1 );
      SetObjectVar( pControl, "_HANDLE", temp );
      hb_itemRelease( temp );
      
      p = lpwAlign (p);

      lStyle = hb_itemGetNL( GetObjectVar( pControl, "STYLE" ) );
      lExtStyle = hb_itemGetNL( GetObjectVar( pControl, "EXTSTYLE" ) );
      x1 = ( hb_itemGetNI( GetObjectVar( pControl, "NLEFT" ) ) * 4) / baseunitX;
      dwidth = ( hb_itemGetNI( GetObjectVar( pControl, "NWIDTH" ) ) * 4) / baseunitX;
      y1 = ( hb_itemGetNI( GetObjectVar( pControl, "NTOP" ) ) * 8) / baseunitY;
      dheight = ( hb_itemGetNI( GetObjectVar( pControl, "NHEIGHT" ) ) * 8) / baseunitY;
      
      *p++ = 0;          // LOWORD (lHelpID)
      *p++ = 0;          // HIWORD (lHelpID)
      *p++ = LOWORD (lExtStyle);          // LOWORD (lExtendedStyle)
      *p++ = HIWORD (lExtStyle);          // HIWORD (lExtendedStyle)
      *p++ = LOWORD (lStyle);
      *p++ = HIWORD (lStyle);
      *p++ = x1;         // x
      *p++ = y1;         // y
      *p++ = dwidth;     // cx
      *p++ = dheight;    // cy
      *p++ = hb_itemGetNI( GetObjectVar( pControl, "ID" ) );       // LOWORD (Control ID)
      *p++ = 0;      // HOWORD (Control ID)

      // class name
      nchar = nCopyAnsiToWideChar (p, TEXT(hb_itemGetCPtr( GetObjectVar( pControl, "WINCLASS" ) )));
      p += nchar;

      // Caption
      temp = GetObjectVar( pControl, "TITLE" );
      if( hb_itemType( temp ) == HB_IT_STRING )
         nchar = nCopyAnsiToWideChar (p, TEXT(hb_itemGetCPtr( temp )));
      else
         nchar = nCopyAnsiToWideChar (p, TEXT(""));
      p += nchar;

      *p++ = 0;  // Advance pointer over nExtraStuff WORD.
   }
   *p = 0;  // Number of bytes of extra data.

   hb_itemRelease( pControls );

   return (LPDLGTEMPLATE) pdlgtemplate;

}

HB_FUNC ( CREATEDLGTEMPLATE )
{
   hb_retnl( (LONG) CreateDlgTemplate( hb_param( 1, HB_IT_OBJECT ), hb_parni(2),
                         hb_parni(3), hb_parni(4), hb_parni(5), hb_parnl(6) ) );
}

HB_FUNC ( RELEASEDLGTEMPLATE )
{
   LocalFree( LocalHandle( (LPDLGTEMPLATE) hb_parnl(1) ) );
}

/*
 *  _CreatePropertySheetPage( aDlg, x1, y1, nWidth, nHeight, nStyle ) --> hPage
 */
HB_FUNC ( _CREATEPROPERTYSHEETPAGE )
{
   PROPSHEETPAGE psp;
   PHB_ITEM pObj = hb_param( 1, HB_IT_OBJECT ), temp;
   char *cTitle;
   LPDLGTEMPLATE pdlgtemplate;

   memset( (void*) &psp, 0, sizeof( PROPSHEETPAGE ) );

   psp.dwSize = sizeof(PROPSHEETPAGE);
   psp.hInstance = (HINSTANCE) NULL;
   psp.pszTitle = NULL;
   psp.pfnDlgProc = (DLGPROC) PSPProc;
   psp.lParam = 0;
   psp.pfnCallback = NULL;
   psp.pcRefParent = 0;
#if !defined(__BORLANDC__)
   psp.hIcon = 0;
#else
   psp.DUMMYUNIONNAME2.hIcon = 0;
#endif

   if( hb_itemGetNI( GetObjectVar( pObj, "TYPE" ) ) == WND_DLG_RESOURCE )
   {
      psp.dwFlags = 0;
      temp = GetObjectVar( pObj, "TITLE" );
      if( hb_itemType( temp ) == HB_IT_STRING )
         cTitle = hb_itemGetCPtr( temp );
      else
         cTitle = NULL;
#if !defined(__BORLANDC__)
      psp.pszTemplate = cTitle;
#else
      psp.DUMMYUNIONNAME.pszTemplate = cTitle;
#endif
   }
   else
   {
      pdlgtemplate = (LPDLGTEMPLATE) hb_parnl(2);

      psp.dwFlags = PSP_DLGINDIRECT;
#if !defined(__BORLANDC__)
      psp.pResource = pdlgtemplate;
#else
      psp.DUMMYUNIONNAME.pResource = pdlgtemplate;
#endif
   }

   hb_retnl( (LONG) CreatePropertySheetPage( &psp ) );
   // if( pdlgtemplate )
   //   LocalFree (LocalHandle (pdlgtemplate));
}

/*
 * _PropertySheet( hWndParent, aPageHandles, nPageHandles, cTitle, 
 *                [ lModeless ], [ lNoApply ], [ lWizard ] ) --> hPropertySheet
 */
HB_FUNC ( _PROPERTYSHEET )
{
   PHB_ITEM pArr = hb_param( 2, HB_IT_ARRAY );
   int nPages = hb_parni(3), i;
   HPROPSHEETPAGE psp[10];
   PROPSHEETHEADER psh;
   DWORD dwFlags = (hb_pcount()<5||ISNIL(5)||!hb_parl(5))? 0:PSH_MODELESS;

   if( hb_pcount()>5 && !ISNIL(6) && hb_parl(6) )
      dwFlags |= PSH_NOAPPLYNOW;
   if( hb_pcount()>6 && !ISNIL(7) && hb_parl(7) )
      dwFlags |= PSH_WIZARD;
   for( i=0;i<nPages;i++ )
      psp[i] = (HPROPSHEETPAGE) hb_itemGetNL( pArr->item.asArray.value->pItems + i );

   psh.dwSize = sizeof(PROPSHEETHEADER);
   psh.dwFlags = dwFlags;
   psh.hwndParent = (HWND) hb_parnl(1);
   psh.hInstance = (HINSTANCE) NULL;
#if !defined(__BORLANDC__)
   psh.pszIcon = NULL;
#else
   psh.DUMMYUNIONNAME.pszIcon = NULL;
#endif
   psh.pszCaption = (LPSTR) hb_parc(4);
   psh.nPages = nPages;
#if !defined(__BORLANDC__)
   psh.nStartPage = 0;
   psh.phpage = psp;
#else
   psh.DUMMYUNIONNAME2.nStartPage = 0;
   psh.DUMMYUNIONNAME3.phpage = psp;
#endif
   psh.pfnCallback = NULL;

   hb_retnl( (LONG) PropertySheet(&psh) );
}

/* Hwg_CreateDlgIndirect( hParentWnd, pArray, x1, y1, nWidth, nHeight, nStyle )
*/

HB_FUNC( HWG_CREATEDLGINDIRECT )
{
   LPDLGTEMPLATE pdlgtemplate;
   PHB_ITEM pObj = hb_param( 2, HB_IT_OBJECT );

   if( hb_pcount()>7 && !ISNIL(8) )
      pdlgtemplate = (LPDLGTEMPLATE) hb_parnl(8);
   else
   {
      LONG lStyle = ( ( hb_pcount()>6 && !ISNIL(7) )? hb_parnl(7):WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU | WS_SIZEBOX ); // | DS_SETFONT;

      pdlgtemplate = CreateDlgTemplate( pObj, hb_parni(3), hb_parni(4),
                          hb_parni(5), hb_parni(6), lStyle );
   }

   CreateDialogIndirect( hModule, pdlgtemplate,
                      (HWND) hb_parnl(1), (DLGPROC) DlgProc );

   if( hb_pcount()<8 || ISNIL(8) )
      LocalFree( LocalHandle( pdlgtemplate ) );
}

/* Hwg_DlgBoxIndirect( hParentWnd, pArray, x1, y1, nWidth, nHeight, nStyle )
*/

HB_FUNC( HWG_DLGBOXINDIRECT )
{
   LPDLGTEMPLATE pdlgtemplate;
   PHB_ITEM pObj;
   LONG lStyle = ( ( hb_pcount()>6 && !ISNIL(7) )? hb_parnl(7):WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU ); // | DS_SETFONT;
   int x1 = hb_parni(3), y1 = hb_parni(4), dwidth = hb_parni(5), dheight = hb_parni(6);

   pObj = hb_param( 2, HB_IT_OBJECT );
   pdlgtemplate = CreateDlgTemplate( pObj, x1, y1, dwidth, dheight, lStyle );

   DialogBoxIndirect( hModule, pdlgtemplate,
                      (HWND) hb_parnl(1), (DLGPROC) ModalDlgProc );
   LocalFree (LocalHandle (pdlgtemplate));
}

HB_FUNC( DIALOGBASEUNITS )
{
   hb_retnl( GetDialogBaseUnits() );
}

LRESULT CALLBACK ModalDlgProc( HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
   PHB_DYNS pSymTest;
   long int res;

   if( ( pSymTest = hb_dynsymFind( "DEFMODALDLGPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmPushLong( (LONG ) hDlg );    /* pushes parameters on to the hvm stack */
      hb_vmPushLong( (LONG ) uMsg );
      hb_vmPushLong( (LONG) wParam );
      hb_vmPushLong( (LONG) lParam );
      hb_vmDo( 4 );  /* where iArgCount is the number of pushed parameters */
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return FALSE;
      else
         return res;
    }
    else
       return FALSE;
}

LRESULT CALLBACK DlgProc( HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
   PHB_DYNS pSymTest;
   long int res;
   int i;

   if( uMsg == WM_INITDIALOG )
      aDialogs[ iDialogs++ ] = hDlg;
   else if( uMsg == WM_DESTROY )
   {
      for( i=0;i<iDialogs;i++ )
         if( aDialogs[ i ] == hDlg )  break;
      iDialogs --;
      for( ;i<iDialogs;i++ )
         aDialogs[ i ] = aDialogs[ i+1 ];
   }

   if( ( pSymTest = hb_dynsymFind( "DEFDLGPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmPushLong( (LONG ) hDlg );    /* pushes parameters on to the hvm stack */
      hb_vmPushLong( (LONG ) uMsg );
      hb_vmPushLong( (LONG) wParam );
      hb_vmPushLong( (LONG) lParam );
      hb_vmDo( 4 );  /* where iArgCount is the number of pushed parameters */
      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
      if( res == -1 )
         return FALSE;
      else
         return res;
    }
    else
       return FALSE;
}

LRESULT CALLBACK PSPProc( HWND hDlg, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
   PHB_DYNS pSymTest;
   int i;

   if( uMsg == WM_INITDIALOG )
      aDialogs[ iDialogs++ ] = hDlg;
   else if( uMsg == WM_DESTROY )
   {
      for( i=0;i<iDialogs;i++ )
         if( aDialogs[ i ] == hDlg )  break;
      iDialogs --;
      for( ;i<iDialogs;i++ )
         aDialogs[ i ] = aDialogs[ i+1 ];
   }

   if( ( pSymTest = hb_dynsymFind( "DEFPSPPROC" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmPushLong( (LONG ) hDlg );    /* pushes parameters on to the hvm stack */
      hb_vmPushLong( (LONG ) uMsg );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );  /* where iArgCount is the number of pushed parameters */
      return hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
    }
    else
       return FALSE;
}
