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
*/
/*------------------------------------------------------------------------------
* Low Level C Generic Routines
*------------------------------------------------------------------------------*/
#define _WIN32_WINNT 0x0400
#define WINVER 0x0400
#define _WIN32_IE 0x0501

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

#include "woopgui.h"

static LOGFONT mylf;

HB_FUNC ( WG_REGISTERFONT )
{
    mylf.lfHeight            =           hb_parni(1);   // height of font
    mylf.lfWidth             =           hb_parni(2);   // average character width
    mylf.lfEscapement        =           hb_parni(3);   // angle of escapement
    mylf.lfOrientation       =           hb_parni(4);   // base-line orientation angle
    mylf.lfWeight            =           hb_parni(5);   // font weight
    mylf.lfItalic            =           hb_parl(6);    // italic attribute option
    mylf.lfUnderline         =           hb_parl(7);    // underline attribute option
    mylf.lfStrikeOut         =           hb_parl(8);    // strikeout attribute option
    mylf.lfCharSet           =  (BYTE)   hb_parnl(9);   // character set identifier
    mylf.lfOutPrecision      =  (BYTE)   hb_parnl(10);  // output precision
    mylf.lfClipPrecision     =  (BYTE)   hb_parnl(11);  // clipping precision
    mylf.lfQuality           =  (BYTE)   hb_parnl(12);  // output quality
    mylf.lfPitchAndFamily    =  (BYTE)   hb_parnl(13);  // pitch and family
    strcpy( mylf.lfFaceName, hb_parcx(14) );   // typeface name

}


HB_FUNC ( CREATEFONT )
{
    int   nHeight            =           hb_parni(1);   // height of font
    int   nWidth             =           hb_parni(2);   // average character width
    int   nEscapement        =           hb_parni(3);   // angle of escapement
    int   nOrientation       =           hb_parni(4);   // base-line orientation angle
    int   fnWeight           =           hb_parni(5);   // font weight
    DWORD fdwItalic          =           hb_parl(6);    // italic attribute option
    DWORD fdwUnderline       =           hb_parl(7);    // underline attribute option
    DWORD fdwStrikeOut       =           hb_parl(8);    // strikeout attribute option
    DWORD fdwCharSet         =           hb_parnl(9);   // character set identifier
    DWORD fdwOutputPrecision =           hb_parnl(10);  // output precision
    DWORD fdwClipPrecision   =           hb_parnl(11);  // clipping precision
    DWORD fdwQuality         =           hb_parnl(12);  // output quality
    DWORD fdwPitchAndFamily  =           hb_parnl(13);  // pitch and family
    CHAR  *lpszFace          =           hb_parcx(14);   // typeface name

    hb_retnl( (long) CreateFont( nHeight, nWidth, nEscapement, nOrientation, fnWeight,
                                 fdwItalic, fdwUnderline, fdwStrikeOut, fdwCharSet,
                                 fdwOutputPrecision, fdwClipPrecision, fdwQuality,
                                 fdwPitchAndFamily, lpszFace ) );

}


HB_FUNC ( WG_CHOOSEFONT )
{
    CHOOSEFONT cf;
    LOGFONT lf;
    HFONT hfont;

    PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );

    // Initialize members of the CHOOSEFONT structure.

    cf.lStructSize = sizeof(CHOOSEFONT);
    cf.hwndOwner = (HWND)NULL;
    cf.hDC = (HDC)NULL;
    cf.lpLogFont = &lf;
    cf.iPointSize = 0;
    cf.Flags = CF_SCREENFONTS;
    cf.rgbColors = RGB(0,0,0);
    cf.lCustData = 0L;
    cf.lpfnHook = (LPCFHOOKPROC)NULL;
    cf.lpTemplateName = (LPSTR)NULL;
    cf.hInstance = (HINSTANCE) NULL;
    cf.lpszStyle = (LPSTR)NULL;
    cf.nFontType = SCREEN_FONTTYPE;
    cf.nSizeMin = 0;
    cf.nSizeMax = 0;

    // Display the CHOOSEFONT common-dialog box.

    if ( ChooseFont(&cf) )
    {
       // Create a logical font based on the user's
       // selection and return a handle identifying
       // that font.
       hfont = CreateFontIndirect(cf.lpLogFont);

       //MessageBox( NULL, "Fin qua sono arrivato", "Error", MB_OK | MB_ICONINFORMATION );

       if ( hb_pcount()>=1 )
       {

         // Fill TFont object
         WG_SetObjectDataLong( pObject, "NHANDLE", (long) hfont );                // nHandle
         WG_SetObjectDataLong( pObject, "NHEIGHT", lf.lfHeight );                 // nHeight
         WG_SetObjectDataLong( pObject, "NWIDTH", lf.lfWidth );                   // nWidth
         WG_SetObjectDataLong( pObject, "NESCAPEMENT", lf.lfEscapement );         // nEscapement
         WG_SetObjectDataLong( pObject, "NORIENTATION", lf.lfOrientation );       // nOrientation
         WG_SetObjectDataLong( pObject, "NWEIGHT", lf.lfWeight );                 // nWeight
         WG_SetObjectDataLogical( pObject, "LITALIC", lf.lfItalic );              // lItalic
         WG_SetObjectDataLogical( pObject, "LUNDERLINE", lf.lfUnderline );        // lUnderline
         WG_SetObjectDataLogical( pObject, "LSTRIKEOUT", lf.lfStrikeOut );        // lStrikeOut
         WG_SetObjectDataLong( pObject, "NCHARSET", lf.lfCharSet );               // nCharSet
         WG_SetObjectDataLong( pObject, "NOUTPUTPRECISION", lf.lfOutPrecision );  // nOutputPrecision
         WG_SetObjectDataLong( pObject, "NCLIPPRECISION", lf.lfClipPrecision );   // nClipPrecision
         WG_SetObjectDataLong( pObject, "NQUALITY", lf.lfQuality );               // nQuality
         WG_SetObjectDataLong( pObject, "NPITCHANDFAMILY", lf.lfPitchAndFamily ); // nPitchAndFamily
         WG_SetObjectDataString( pObject, "CFACE", lf.lfFaceName );               // cFace

       }
    }
    else
       hfont = 0;

    hb_retnl( (long) hfont );
}

HB_FUNC ( WG_CREATEFONTINDIRECT )
{
    LOGFONT lf;
    HFONT hfont;

    PHB_ITEM pObject  = hb_param( 1, HB_IT_OBJECT );

    memcpy(&lf,&mylf,sizeof(LOGFONT));

    hfont = CreateFontIndirect(&mylf);

       //MessageBox( NULL, "Fin qua sono arrivato", "Error", MB_OK | MB_ICONINFORMATION );

       if ( hb_pcount()>=1 )
       {

         // Fill TFont object with real values
         WG_SetObjectDataLong( pObject, "NHANDLE", (long) hfont );                // nHandle
         WG_SetObjectDataLong( pObject, "NHEIGHT", lf.lfHeight );                 // nHeight
         WG_SetObjectDataLong( pObject, "NWIDTH", lf.lfWidth );                   // nWidth
         WG_SetObjectDataLong( pObject, "NESCAPEMENT", lf.lfEscapement );         // nEscapement
         WG_SetObjectDataLong( pObject, "NORIENTATION", lf.lfOrientation );       // nOrientation
         WG_SetObjectDataLong( pObject, "NWEIGHT", lf.lfWeight );                 // nWeight
         WG_SetObjectDataLogical( pObject, "LITALIC", lf.lfItalic );              // lItalic
         WG_SetObjectDataLogical( pObject, "LUNDERLINE", lf.lfUnderline );        // lUnderline
         WG_SetObjectDataLogical( pObject, "LSTRIKEOUT", lf.lfStrikeOut );        // lStrikeOut
         WG_SetObjectDataLong( pObject, "NCHARSET", lf.lfCharSet );               // nCharSet
         WG_SetObjectDataLong( pObject, "NOUTPUTPRECISION", lf.lfOutPrecision );  // nOutputPrecision
         WG_SetObjectDataLong( pObject, "NCLIPPRECISION", lf.lfClipPrecision );   // nClipPrecision
         WG_SetObjectDataLong( pObject, "NQUALITY", lf.lfQuality );               // nQuality
         WG_SetObjectDataLong( pObject, "NPITCHANDFAMILY", lf.lfPitchAndFamily ); // nPitchAndFamily
         WG_SetObjectDataString( pObject, "CFACE", lf.lfFaceName );               // cFace

       }

    hb_retnl( (long) hfont );

}


HB_FUNC ( CONVERTPOINTSTOLU )
{
  HDC  hDC;
  int  nPointSize = hb_parni(1); // Font size in points
  long lfHeight;

  hDC = GetDC ( HWND_DESKTOP ) ;

  lfHeight = -MulDiv( nPointSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);

  ReleaseDC ( HWND_DESKTOP, hDC ) ;

  hb_retnl( lfHeight );

}
