/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level common dialogs functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

extern PHB_ITEM GetObjectVar( PHB_ITEM pObject, char* varname );

HB_FUNC ( SELECTFONT )
{

   CHOOSEFONT cf;
   LOGFONT lf;
   HFONT hfont;
   PHB_ITEM aMetr = hb_itemArrayNew( 9 );
   PHB_ITEM pObj = ( ISNIL(1) )? NULL:hb_param( 1, HB_IT_OBJECT ), temp;

    /* Initialize members of the CHOOSEFONT structure. */
    if( pObj )
    {
       memset( &lf, 0, sizeof(LOGFONT) );
       temp = GetObjectVar( pObj, "NAME" );
       strcpy( lf.lfFaceName, hb_itemGetCPtr( temp ) );
       temp = GetObjectVar( pObj, "WIDTH" );
       lf.lfWidth = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "HEIGHT" );
       lf.lfHeight = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "WEIGHT" );
       lf.lfWeight = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "CHARSET" );
       lf.lfCharSet = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "ITALIC" );
       lf.lfItalic = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "UNDERLINE" );
       lf.lfUnderline = hb_itemGetNI( temp );
       temp = GetObjectVar( pObj, "STRIKEOUT" );
       lf.lfStrikeOut = hb_itemGetNI( temp );
    }

    cf.lStructSize = sizeof(CHOOSEFONT);
    cf.hwndOwner = (HWND)NULL;
    cf.hDC = (HDC)NULL;
    cf.lpLogFont = &lf;
    cf.iPointSize = 0;
    cf.Flags = CF_SCREENFONTS | ( (pObj)? CF_INITTOLOGFONTSTRUCT:0 );
    cf.rgbColors = RGB(0,0,0);
    cf.lCustData = 0L;
    cf.lpfnHook = (LPCFHOOKPROC)NULL;
    cf.lpTemplateName = (LPSTR)NULL;

    cf.hInstance = (HINSTANCE) NULL;
    cf.lpszStyle = (LPSTR)NULL;
    cf.nFontType = SCREEN_FONTTYPE;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;

    /* Display the CHOOSEFONT common-dialog box. */

    if( !ChooseFont(&cf) )
    {
       /*
       temp = hb_itemPutNL( NULL, 0 );
       hb_itemArrayPut( aMetr, 1, temp );
       hb_itemRelease( temp );
       hb_itemReturn( aMetr );
       */
       hb_itemRelease( aMetr );
       hb_ret();
       return;
    }

    /* Create a logical font based on the user's   */
    /* selection and return a handle identifying   */
    /* that font.                                  */

    hfont = CreateFontIndirect(cf.lpLogFont);

   temp = hb_itemPutNL( NULL, (LONG) hfont );
   hb_itemArrayPut( aMetr, 1, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutC( NULL, lf.lfFaceName );
   hb_itemArrayPut( aMetr, 2, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfWidth );
   hb_itemArrayPut( aMetr, 3, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfHeight );
   hb_itemArrayPut( aMetr, 4, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNL( NULL, lf.lfWeight );
   hb_itemArrayPut( aMetr, 5, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfCharSet );
   hb_itemArrayPut( aMetr, 6, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfItalic );
   hb_itemArrayPut( aMetr, 7, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfUnderline );
   hb_itemArrayPut( aMetr, 8, temp );
   hb_itemRelease( temp );

   temp = hb_itemPutNI( NULL, lf.lfStrikeOut );
   hb_itemArrayPut( aMetr, 9, temp );
   hb_itemRelease( temp );

   hb_itemReturn( aMetr );
   hb_itemRelease( aMetr );

}

HB_FUNC ( SELECTFILE )
{
   OPENFILENAME ofn;
   char buffer[512];
   char *strFilter, *str1 = hb_parc( 1 ), *str2 = hb_parc( 2 );
   char *initDir = ( hb_pcount()>2 && ISCHAR(3) )? hb_parc(3):NULL;
   char *cTitle = ( hb_pcount()>3 && ISCHAR(4) )? hb_parc(4):NULL;


   strFilter = (char*) hb_xgrab( strlen(str1) + strlen(str2) + 4 );
   if( strFilter == NULL )
   {
      hb_retc( "NULL" );
      return;
   }
   memset( strFilter, 0, strlen(str1) + strlen(str2) + 4 );
   strcpy( strFilter, str1 );
   strcpy( strFilter+strlen(str1)+1, str2 );

   memset( (void*) &ofn, 0, sizeof( OPENFILENAME ) );
   ofn.lStructSize = sizeof(ofn);
   ofn.hwndOwner = GetActiveWindow();
   ofn.lpstrFilter = strFilter;
   ofn.lpstrFile = buffer;
   buffer[0] = 0;
   ofn.nMaxFile = 512;
   ofn.lpstrInitialDir = initDir;
   ofn.lpstrTitle = cTitle;
   ofn.Flags = OFN_FILEMUSTEXIST|OFN_EXPLORER;

   if( GetOpenFileName( &ofn ) )
      hb_retc( ofn.lpstrFile );
   else
      hb_retc( "" );
   hb_xfree( strFilter );
}

HB_FUNC ( SAVEFILE )
{
   OPENFILENAME ofn;
   char buffer[512];
   char *strFilter, *str1 = hb_parc( 2 ), *str2 = hb_parc( 3 );
   char *initDir = ( hb_pcount()>3 && ISCHAR(4) )? hb_parc(4):NULL;
   char *cTitle = ( hb_pcount()>4 && ISCHAR(5) )? hb_parc(5):NULL;


   strFilter = (char*) hb_xgrab( strlen(str1) + strlen(str2) + 4 );
   if( strFilter == NULL )
   {
      hb_retc( "NULL" );
      return;
   }
   memset( strFilter, 0, strlen(str1) + strlen(str2) + 4 );
   strcpy( strFilter, str1 );
   strcpy( strFilter+strlen(str1)+1, str2 );

   strcpy( buffer, hb_parc( 1 ) );

   memset( (void*) &ofn, 0, sizeof( OPENFILENAME ) );
   ofn.lStructSize = sizeof(ofn);
   ofn.hwndOwner = GetActiveWindow();
   ofn.lpstrFilter = strFilter;
   ofn.lpstrFile = buffer;
   ofn.nMaxFile = 512;
   ofn.lpstrInitialDir = initDir;
   ofn.lpstrTitle = cTitle;
   ofn.Flags = OFN_FILEMUSTEXIST|OFN_EXPLORER;

   if( GetSaveFileName( &ofn ) )
      hb_retc( ofn.lpstrFile );
   else
      hb_retc( "" );
   hb_xfree( strFilter );
}

HB_FUNC ( PRINTSETUP )
{
   PRINTDLG pd;

   memset( (void*) &pd, 0, sizeof( PRINTDLG ) );

   pd.lStructSize = sizeof(PRINTDLG); 
   // pd.hDevNames = (HANDLE) NULL; 
   pd.Flags = PD_RETURNDC; 
   pd.hwndOwner = GetActiveWindow();
   // pd.hDC = (HDC) NULL; 
   pd.nFromPage = 1; 
   pd.nToPage = 1; 
   // pd.nMinPage = 0; 
   // pd.nMaxPage = 0; 
   pd.nCopies = 1; 
   // pd.hInstance = (HANDLE) NULL; 
   // pd.lCustData = 0L; 
   // pd.lpfnPrintHook = (LPPRINTHOOKPROC) NULL; 
   // pd.lpfnSetupHook = (LPSETUPHOOKPROC) NULL; 
   // pd.lpPrintTemplateName = (LPSTR) NULL; 
   // pd.lpSetupTemplateName = (LPSTR)  NULL; 
   // pd.hPrintTemplate = (HANDLE) NULL; 
   // pd.hSetupTemplate = (HANDLE) NULL; 
    
   if( PrintDlg(&pd) )
      hb_retnl( (LONG) pd.hDC );
   else
      hb_retnl( 0 );  
}

HB_FUNC ( HWG_CHOOSECOLOR )
{
   CHOOSECOLOR cc;
   COLORREF rgb[16];
   DWORD nStyle = ( ISLOG(2) && hb_parl(2) )? CC_FULLOPEN:0;

   memset( (void*) &cc, 0, sizeof( CHOOSECOLOR ) );

   cc.lStructSize = sizeof(CHOOSECOLOR); 
   cc.hwndOwner   = GetActiveWindow();
   cc.lpCustColors = rgb;
   if( ISNUM(1) )
   {
      cc.rgbResult = (COLORREF)hb_parnl(1);
      nStyle |= CC_RGBINIT;
   }
   cc.Flags = nStyle;

   if( ChooseColor( &cc ) )
      hb_retnl( (LONG) cc.rgbResult );
   else
      hb_ret();
}


unsigned long Get_SerialNumber(char* RootPathName)
{
   unsigned long SerialNumber;

   GetVolumeInformation(RootPathName, NULL, 0, &SerialNumber,
                        NULL, NULL, NULL, 0);
   return SerialNumber;
}

HB_FUNC( HDGETSERIAL)
{
   hb_retnl( Get_SerialNumber(hb_parc(1)) );
}
