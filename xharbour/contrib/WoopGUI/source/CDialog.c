/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*
*/
/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level dialog boxes functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <shlobj.h>

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

#include "woopgui.h"

LPWORD lpwAlign ( LPWORD lpIn );
int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn);

LRESULT WINAPI WG_ModalDlgProc( HWND, UINT, WPARAM, LPARAM );
BOOL CALLBACK WG_DlgProc(HWND, UINT, WPARAM, LPARAM);
PHB_ITEM pArray;
extern BOOL WG_DebugTrace( char* cMsg );

HB_FUNC ( WG_DIALOGBOX )
{
//   PHB_DYNS pSymTest;
   char *cIdDlg;

   pArray = hb_param( 1, HB_IT_ARRAY );
   cIdDlg = hb_itemGetCPtr ( pArray->item.asArray.value->pItems + 0 );
//   if( ( pSymTest = hb_dynsymFind( "INITRESCONTROLS" ) ) != NULL )
//   {
//      hb_vmPushSymbol( pSymTest->pSymbol );
//      hb_vmPushNil();
//      hb_vmPush( pArray );
//      hb_vmDo( 1 );
//   }
   hb_retni( DialogBox( GetModuleHandle( NULL ), cIdDlg, GetActiveWindow(), (DLGPROC) WG_ModalDlgProc ) );
}

//HB_FUNC ( ENDDIALOG )
//{
//   EndDialog( (HWND) hb_parnl( 1 ), TRUE );
//}
//
//HB_FUNC ( GETDLGITEM )
//{
//   HWND hWnd = GetDlgItem(
//                 (HWND) hb_parnl( 1 ),  // handle of dialog box
//                 hb_parni( 2 )         // identifier of control
//               );
//   hb_retnl( (LONG) hWnd );
//}
//
//HB_FUNC ( GETDLGCTRLID )
//{
//   hb_retni( GetDlgCtrlID( (HWND) hb_parnl( 1 ) ) );
//}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetDlgItemInt( IN HWND hDlg, IN int nIDDlgItem, IN UINT uValue, IN BOOL bSigned);


//HB_FUNC( SETDLGITEMINT )
//{
//   hb_retl( SetDlgItemInt( (HWND) hb_parnl( 1 ),
//                           hb_parni( 2 )       ,
//                           (UINT) hb_parni( 3 ),
//                           hb_parl( 4 )
//                         ) ) ;
//}


//-----------------------------------------------------------------------------

//HB_FUNC ( SETDLGITEMTEXT )
//{
//    SetDlgItemText(
//       (HWND) hb_parnl( 1 ),   // handle of dialog box
//       hb_parni( 2 ),         // identifier of control
//       (LPCTSTR) hb_parcx( 3 )  // text to set
//    );
//}
//
//HB_FUNC ( GETDLGITEMTEXT )
//{
//   USHORT iLen = hb_parni( 3 );
//   char *cText = (char*) hb_xgrab( iLen+1 );
//
//   GetDlgItemText(
//       (HWND) hb_parnl( 1 ),   // handle of dialog box
//    hb_parni( 2 ),             // identifier of control
//    (LPTSTR) cText,         // address of buffer for text
//    iLen                    // maximum size of string
//   );
//   hb_retc( cText );
//   hb_xfree( cText );
//}
//
HB_FUNC ( GETEDITTEXT )
{
   HWND   hDlg = (HWND) hb_parnl( 1 );
   int    id = hb_parni( 2 );
   ULONG iLen = SendMessage( GetDlgItem( hDlg,id ), WM_GETTEXTLENGTH, 0, 0 );
   char *cText = (char*) hb_xgrab( iLen+2 );

   GetDlgItemText(
       hDlg,   // handle of dialog box
    id,           // identifier of control
    (LPTSTR) cText,        // address of buffer for text
    iLen+1                    // maximum size of string
   );
   hb_retc( cText );
   hb_xfree( cText );
}

//HB_FUNC ( CHECKDLGBUTTON )
//{
//    CheckDlgButton(
//       (HWND) hb_parnl( 1 ),   // handle of dialog box
//       hb_parni( 2 ),         // identifier of control
//       ( hb_parl( 3 ) )? BST_CHECKED:BST_UNCHECKED  // value to set
//    );
//}
//
//HB_FUNC ( CHECKRADIOBUTTON )
//{
//    CheckRadioButton(
//       (HWND) hb_parnl( 1 ),   // handle of dialog box
//       hb_parni( 2 ),         // identifier of first radio button in group
//       hb_parni( 3 ),         // identifier of last radio button in group
//       hb_parni( 4 )          // identifier of radio button to select
//    );
//}
//
//HB_FUNC ( ISDLGBUTTONCHECKED )
//{
//  UINT nRes = IsDlgButtonChecked(
//                  (HWND) hb_parnl( 1 ),       // handle of dialog box
//                   hb_parni( 2 )               // button identifier
//              );
//  if( nRes == BST_CHECKED )
//     hb_retl( TRUE );
//  else
//     hb_retl( FALSE );
//}
//
HB_FUNC ( COMBOADDSTRING )
{
   char *cString = hb_parcx( 2 );
   SendMessage( (HWND) hb_parnl( 1 ), CB_ADDSTRING, 0, (LPARAM) cString );
}

HB_FUNC ( COMBOSETSTRING )
{
   SendMessage( (HWND) hb_parnl( 1 ), CB_SETCURSEL, (WPARAM) hb_parni(2)-1, 0);
}

/*
LPWORD lpwAlign ( LPWORD lpIn)
{
  ULONG ul;

  ul = (ULONG) lpIn;
  ul +=3;
  ul >>=2;
  ul <<=2;
  return (LPWORD) ul;
}

int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
  int nChar = 0;

  //WG_warning(lpWCStr, "WG_SetObjectDataLong - Type");

  do {
    *lpWCStr++ = (WORD) *lpAnsiIn;
    nChar++;
  } while (*lpAnsiIn++);

  return nChar;
}
*/

/* WG_CreateDialogIndirect( pDialog, pArrItems )
*/


int WG_GetCharWidth( void )
{
    LONG nBaseUnit = GetDialogBaseUnits();
    return LOWORD( nBaseUnit );
}

int WG_GetCharHeight( void )
{
    LONG nBaseUnit = GetDialogBaseUnits();
    return HIWORD( nBaseUnit );
}

int WG_Pixel2DialogX( int nPixel )
{
    return MulDiv( nPixel, 4, WG_GetCharWidth() );
}

int WG_Pixel2DialogY( int nPixel )
{
    return MulDiv( nPixel, 8, WG_GetCharHeight() );
}

int WG_Dialog2PixelX( int nDialog )
{
    return MulDiv( nDialog, WG_GetCharWidth(), 4 );
}

int WG_Dialog2PixelY( int nDialog )
{
    return MulDiv( nDialog, WG_GetCharHeight(), 8 );
}


HB_FUNC ( WG_GETCHARWIDTH )
{
   hb_retni( WG_GetCharWidth() ) ;
}

HB_FUNC ( WG_GETCHARHEIGHT )
{
   hb_retni( WG_GetCharHeight() ) ;
}

HB_FUNC ( WG_PIXEL2DIALOGX )
{
   hb_retni( WG_Pixel2DialogX( hb_parni(1) ) ) ;
}

HB_FUNC ( WG_PIXEL2DIALOGY )
{
   hb_retni( WG_Pixel2DialogY( hb_parni(1) ) ) ;
}

HB_FUNC ( WG_DIALOG2PIXELX )
{
   hb_retni( WG_Dialog2PixelX( hb_parni(1) ) ) ;
}

HB_FUNC ( WG_DIALOG2PIXELY )
{
   hb_retni( WG_Dialog2PixelY( hb_parni(1) ) ) ;
}

LPDLGTEMPLATE WG_CreateDialogTemplate( PHB_ITEM pObj )
{
   WORD *p, *pDlgTemplate;
   PHB_ITEM paoItems, pItem;
   int nchar, nItems, i;
   int lPixel;

   pDlgTemplate = p = (PWORD) LocalAlloc (LPTR, 65534)  ; // 64k allow to build up to 255 items on the dialog

   lPixel = WG_GetObjectDataLogical( pObj, "LPIXEL" );

   paoItems = hb_itemNew( WG_GetObjectData( pObj, "AOCHILDS" ) );
   nItems = paoItems->item.asArray.value->ulLen;

   // Start with dialog template
   *p++ = ( WORD) WG_GetObjectDataLong( pObj, "NDLGVER" );              // 1      - dlgVer
   *p++ = ( WORD) WG_GetObjectDataLong( pObj, "NSIGNATURE" );           // 0xFFFF - signature
   *p++ = LOWORD( WG_GetObjectDataLong( pObj, "NHELPID" ) );    //        - LOWORD helpID
   *p++ = HIWORD( WG_GetObjectDataLong( pObj, "NHELPID" ) );    //        - HIWORD helpID
   *p++ = LOWORD( WG_GetObjectDataLong( pObj, "NEXSTYLE" ) );   //        - LOWORD ExStyle
   *p++ = HIWORD( WG_GetObjectDataLong( pObj, "NEXSTYLE" ) );   //        - HIWORD ExStyle
   *p++ = LOWORD( WG_GetObjectDataLong( pObj, "NSTYLE" ) );     //        - LOWORD Style
   *p++ = HIWORD( WG_GetObjectDataLong( pObj, "NSTYLE" ) );     //        - HIWORD Style
   *p++ = ( WORD) WG_GetObjectDataLong( pObj, "NDLGITEMS" );            //        - cDlgItems
   if (lPixel)
   {
      *p++ = WG_Pixel2DialogX( WG_GetObjectDataInteger( pObj, "NLEFT" ) ); //    - x
      *p++ = WG_Pixel2DialogY( WG_GetObjectDataInteger( pObj, "NTOP" ) ); //    - y
      *p++ = WG_Pixel2DialogX( WG_GetObjectDataInteger( pObj, "NWIDTH" ) );  // cx
      *p++ = WG_Pixel2DialogY( WG_GetObjectDataInteger( pObj, "NHEIGHT" ) ); // cy
   }
   else
   {
      *p++ = WG_GetObjectDataInteger( pObj, "NLEFT" ); //    - x
      *p++ = WG_GetObjectDataInteger( pObj, "NTOP" ); //    - y
      *p++ = WG_GetObjectDataInteger( pObj, "NWIDTH" );  // cx
      *p++ = WG_GetObjectDataInteger( pObj, "NHEIGHT" ); // cy
   }
   *p++ = (WORD) WG_GetObjectDataLong( pObj, "NMENU" );                //        - menu
   *p++ = (WORD) WG_GetObjectDataLong( pObj, "NCLASSNAME" );           //        - windowClass

   // Get the title
   nchar = nCopyAnsiToWideChar( p, ( char *) WG_GetObjectDataString( pObj, "CNAME" ) );
   p += nchar;
   //*p++ = 0;

   // FSG - Here must be putted font definition

   // Then add control templates
   for( i=0;i<nItems;i++ )
   {
      pItem = (PHB_ITEM) (paoItems->item.asArray.value->pItems + i);

      lPixel = WG_GetObjectDataLogical( pItem, "LPIXEL" );

      WG_SetObjectDataLong( pItem, "NHANDLE", -1 );
      //WG_SetObjectDataLong( pItem, "NHANDLE", WG_GetObjectDataLong( pItem, "NID" ) );

      p = lpwAlign( p );

      *p++ = LOWORD( WG_GetObjectDataLong( pItem, "NHELPID" ) ); // LOWORD (lHelpID)
      *p++ = HIWORD( WG_GetObjectDataLong( pItem, "NHELPID" ) ); // HIWORD (lHelpID)
      *p++ = LOWORD( WG_GetObjectDataLong( pItem, "NEXSTYLE" ) ); // LOWORD (lExtendedStyle)
      *p++ = HIWORD( WG_GetObjectDataLong( pItem, "NEXSTYLE" ) ); // HIWORD (lExtendedStyle)
      *p++ = LOWORD( WG_GetObjectDataLong( pItem, "NSTYLE" ) );
      *p++ = HIWORD( WG_GetObjectDataLong( pItem, "NSTYLE" ) );
      if (lPixel)
      {
         *p++ = WG_Pixel2DialogX( WG_GetObjectDataInteger( pItem, "NLEFT" ) ); // x
         *p++ = WG_Pixel2DialogY( WG_GetObjectDataInteger( pItem, "NTOP" ) ); // y
         *p++ = WG_Pixel2DialogX( WG_GetObjectDataInteger( pItem, "NWIDTH" ) ); // cx
         *p++ = WG_Pixel2DialogY( WG_GetObjectDataInteger( pItem, "NHEIGHT" ) ); // cy
      }
      else
      {
         *p++ = WG_GetObjectDataInteger( pItem, "NLEFT" ); // x
         *p++ = WG_GetObjectDataInteger( pItem, "NTOP" ); // y
         *p++ = WG_GetObjectDataInteger( pItem, "NWIDTH" ); // cx
         *p++ = WG_GetObjectDataInteger( pItem, "NHEIGHT" ); // cy
      }
      *p++ = LOWORD( WG_GetObjectDataLong( pItem, "NID" ) );       // LOWORD (Control ID)
      *p++ = HIWORD( WG_GetObjectDataLong( pItem, "NID" ) ); // HOWORD (Control ID)

      // classname
      nchar = nCopyAnsiToWideChar( p, WG_GetObjectDataString( pItem, "CCLASSNAME" ) );
      p += nchar;

      // Caption
      nchar = nCopyAnsiToWideChar( p, WG_GetObjectDataString( pItem, "CNAME" ) );
      p += nchar;

      *p++ = 0;  // Advance pointer over nExtraStuff WORD.

      //hb_itemRelease( pItem );
   }
   *p = 0;  // Number of bytes of extra data.

   hb_itemRelease( paoItems );

   return (LPDLGTEMPLATE) pDlgTemplate;

}

HB_FUNC ( WG_CREATEDIALOGINDIRECT )
{
   LPDLGTEMPLATE pDlgTemplate;
   PHB_ITEM pObj;
   HWND hwnd, hparentwnd;

   pObj = hb_param( 1, HB_IT_OBJECT );   // Dialog Object

   pDlgTemplate = WG_CreateDialogTemplate( pObj );

   //DialogBoxIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgItemTemplate,
   //                   (HWND) hb_parnl(1), (DLGPROC) ModalDlgProc );


   // DialogBoxIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgItemTemplate,
   //                    (HWND) hb_parnl(1), (DLGPROC) WG_DlgProc );

   // pParent = WG_GetObjectData( pObj, "OPARENT" );
   // hparentwnd = (HWND) WG_GetObjectDataLong( pParent, "NHANDLE" );
   // hb_itemRelease( pParent );

   hparentwnd = (HWND) WG_GetObjectDataLong( pObj, "NPARENT" );

   WG_SetObjectDataLogical( pObj, "LINITIALIZE", TRUE );

   hwnd = CreateDialogIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgTemplate,
                       (HWND) hparentwnd, (DLGPROC) WG_DlgProc );

   hb_retnl( (long) hwnd );

   // sprintf( cres,"%d,%d",res,GetLastError() );
   // MessageBox( GetActiveWindow(), cres, "DialogBoxIndirect", MB_OK | MB_ICONINFORMATION );
   LocalFree( LocalHandle( pDlgTemplate ) );
}

HB_FUNC ( WG_DIALOGBOXINDIRECT )
{
   LPDLGTEMPLATE pDlgTemplate;
   PHB_ITEM pObj;
   HWND hparentwnd;

   pObj = hb_param( 1, HB_IT_OBJECT );   // Dialog Object

   pDlgTemplate = WG_CreateDialogTemplate( pObj );

   hparentwnd = (HWND) WG_GetObjectDataLong( pObj, "NPARENT" );

   WG_SetObjectDataLogical( pObj, "LINITIALIZE", TRUE );

   hb_retni( DialogBoxIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgTemplate,
                               (HWND) hparentwnd, (DLGPROC) WG_ModalDlgProc ) );

   //DialogBoxIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgItemTemplate,
   //                   (HWND) hb_parnl(1), (DLGPROC) ModalDlgProc );


   // DialogBoxIndirect( GetModuleHandle( NULL ), (LPDLGTEMPLATE) pDlgItemTemplate,
   //                    (HWND) hb_parnl(1), (DLGPROC) WG_DlgProc );

   // pParent = WG_GetObjectData( pObj, "OPARENT" );
   // hparentwnd = (HWND) WG_GetObjectDataLong( pParent, "NHANDLE" );
   // hb_itemRelease( pParent );

   // sprintf( cres,"%d,%d",res,GetLastError() );
   // MessageBox( GetActiveWindow(), cres, "DialogBoxIndirect", MB_OK | MB_ICONINFORMATION );
   LocalFree( LocalHandle( pDlgTemplate ) );
}


LRESULT CALLBACK WG_ModalDlgProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_DYNS pSymTest;
   long int res;

   if( ( pSymTest = hb_dynsymFind( "WG_DEFDLGEVENTS" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmPushLong( ( LONG ) hWnd );
      hb_vmPushLong( message );
      hb_vmPushLong( wParam );
      hb_vmPushLong( lParam );
      hb_vmDo( 4 );  /* where iArgCount is the number of pushed parameters */

      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );

      //sprintf( szBuffer, "from C WG_ModalDlgProc(): hWnd = %lu, nMsg = %lu, res = %i", hWnd, message, res );
      //WG_DebugTrace( szBuffer );

      return res;
    }
    else
       return FALSE;
}

BOOL CALLBACK WG_DlgProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   PHB_DYNS pSymTest;
   long int res;

   if( ( pSymTest = hb_dynsymFind( "WG_DEFDLGEVENTS" ) ) != NULL )
   {
      hb_vmPushSymbol( pSymTest->pSymbol );
      hb_vmPushNil();
      hb_vmPushLong( ( LONG ) hWnd );
      hb_vmPushLong( message );
      hb_vmPushLong( wParam );
      hb_vmPushLong( lParam );
      hb_vmDo( 4 );

      res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );

      //sprintf( szBuffer, "from C WG_DlgProc(): hWnd = %lu, nMsg = %lu, res = %i", hWnd, message, res );
      //WG_DebugTrace( szBuffer );

      return res;
   }
    else
       return FALSE;
}

