
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
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "item.api"

void WG_error(LPCTSTR cError, LPCTSTR cWhere);
void WG_warning(LPCTSTR cError, LPCTSTR cWhere);

PHB_ITEM WG_GetObjectData( PHB_ITEM pObj, char* cData);
double WG_GetObjectDataDouble( PHB_ITEM pObj, char* cData );
int WG_GetObjectDataInteger( PHB_ITEM pObj, char* cData );
int WG_GetObjectDataLogical( PHB_ITEM pObj, char* cData );
long WG_GetObjectDataLong( PHB_ITEM pObj, char* cData );
char * WG_GetObjectDataString( PHB_ITEM pObj, char* cData );
char * WG_GetObjectDataStringPtr( PHB_ITEM pObj, char* cData );

void WG_SetObjectDataDate( PHB_ITEM pObj, char* cData, char * szDate );
void WG_SetObjectDataDouble( PHB_ITEM pObj, char* cData, double dNew );
void WG_SetObjectDataInteger( PHB_ITEM pObj, char* cData, int iNew );
void WG_SetObjectDataLogical( PHB_ITEM pObj, char* cData, BOOL lNew );
void WG_SetObjectDataLong( PHB_ITEM pObj, char* cData, long nNew );
void WG_SetObjectDataString( PHB_ITEM pObj, char* cData, char *cNew );
void WG_SetObjectData( PHB_ITEM pObj, char* cData, PHB_ITEM pNew );



char * hb_itemVal2Str( PHB_ITEM pItem )
{
   PHB_ITEM pResult;
   char * buffer;
   ULONG ulLen;
   BOOL bFreeReq;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemVal2Str(%p)", pItem));

   buffer = hb_itemString( pItem, &ulLen, &bFreeReq );
   return buffer;
   //if( bFreeReq )
   //   hb_xfree( buffer );
}


// Funzione per la visualizzazione dell'errore
void WG_error(LPCTSTR cError, LPCTSTR cWhere)
{
    LPVOID lpMsgBuf;
    char   cErrorMsg[200];
    DWORD dw = GetLastError();
    static PHB_DYNS Dyns = 0 ;
    long int res;

    FormatMessage(
                   FORMAT_MESSAGE_ALLOCATE_BUFFER |
                   FORMAT_MESSAGE_FROM_SYSTEM |
                   FORMAT_MESSAGE_IGNORE_INSERTS,
                   NULL,
                   dw,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                   (LPTSTR) &lpMsgBuf,
                   0,
                   NULL
                 );

    sprintf(cErrorMsg, "Error: %s \nPosition %s.\n Error code: %u.\n %s\n",
        cError, cWhere, dw, (LPCTSTR)lpMsgBuf);

    // Display the string.
    MessageBox( NULL, (LPCTSTR)cErrorMsg, "Error", MB_OK | MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree( lpMsgBuf );

    if ( ! Dyns )
    {
        Dyns = hb_dynsymFind( "WG_GENMYERROR" );
    }

    hb_vmPushSymbol( Dyns->pSymbol );
    hb_vmPushNil();
    hb_vmPushString( cErrorMsg, strlen(cErrorMsg) );
    hb_vmDo( 1 );

    return;

}

// Funzione per la visualizzazione dell'errore
void WG_warning(LPCTSTR cError, LPCTSTR cWhere)
{
    LPVOID lpMsgBuf;
    char   cErrorMsg[200];
    DWORD dw = GetLastError();

    FormatMessage(
                   FORMAT_MESSAGE_ALLOCATE_BUFFER |
                   FORMAT_MESSAGE_FROM_SYSTEM |
                   FORMAT_MESSAGE_IGNORE_INSERTS,
                   NULL,
                   dw,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                   (LPTSTR) &lpMsgBuf,
                   0,
                   NULL
                 );

    sprintf(cErrorMsg, "Error: %s \nPosition %s.\n Error code: %u.\n %s\n",
        cError, cWhere, dw, (LPCTSTR)lpMsgBuf);

    // Display the string.
    MessageBox( NULL, (LPCTSTR)cErrorMsg, "Warning", MB_OK | MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree( lpMsgBuf );

}

double WG_GetObjectDataDouble( PHB_ITEM pObj, char* cData )
{
   return hb_itemGetND( WG_GetObjectData( pObj, cData ) );
}

int WG_GetObjectDataInteger( PHB_ITEM pObj, char* cData )
{
   return hb_itemGetNI( WG_GetObjectData( pObj, cData ) );
}

int WG_GetObjectDataLogical( PHB_ITEM pObj, char* cData )
{
   return TEXT( hb_itemGetL( WG_GetObjectData( pObj, cData ) ) );
}

long WG_GetObjectDataLong( PHB_ITEM pObj, char* cData )
{
   return hb_itemGetNL( WG_GetObjectData( pObj, cData ) );
}

char * WG_GetObjectDataString( PHB_ITEM pObj, char* cData )
{
   PHB_ITEM pItem;
   char * cText;

   //pItem = WG_GetObjectData( pObj, cData );
   cText = TEXT( hb_itemGetCPtr( WG_GetObjectData( pObj, cData ) ));

   //WG_warning(cData, "WG_GetObjectDataString - cData");
   //WG_warning(cText, "WG_GetObjectDataString - Value cText");
   //WG_warning(hb_itemValToStr( pItem ), "WG_GetObjectDataString - String");
   //WG_warning(hb_itemVal2Str( pItem ), "WG_GetObjectDataString - String");

   //if ( hb_itemType( pItem ) == HB_IT_STRING )
   //if ( hb_itemTypeStr( pItem ) == "C" )
   //{
   //   // cText = TEXT( hb_itemGetCPtr( pItem ));
   //   cText = TEXT( (LPSTR) hb_itemGetCPtr( pItem ));
   //   WG_warning(cText, "WG_GetObjectDataString - cText");
   //}
   //else
   //{
   //   cText = TEXT( "" );
   //   WG_warning(cText, "WG_GetObjectDataString - no cText");
   //}
   //hb_itemRelease( pItem );

   return cText;
}

char * WG_GetObjectDataStringPtr( PHB_ITEM pObj, char* cData )
{
   return hb_itemGetCPtr( WG_GetObjectData( pObj, cData ) );
}

PHB_ITEM WG_GetObjectData( PHB_ITEM pObj, char* cData )
{
   PHB_DYNS pDyns;
   char  cMsg[200];

   pDyns = hb_dynsymFind( cData );

   if( pDyns )
   {
      hb_vmPushSymbol( pDyns->pSymbol );
      hb_vmPush( pObj );
      hb_vmSend( 0 );
      return &hb_stack.Return;
   }
   else
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "GetObjectData");
     return 0;
   }
}

void WG_SetObjectDataDate( PHB_ITEM pObj, char* cData, char * szDate )
{
   PHB_ITEM xValue = hb_itemPutDS( NULL, szDate );
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectDataDouble( PHB_ITEM pObj, char* cData, double dNew )
{
   PHB_ITEM xValue = hb_itemPutND( NULL, dNew );
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectDataInteger( PHB_ITEM pObj, char* cData, int iNew )
{
   PHB_ITEM xValue = hb_itemPutNI( NULL, iNew );
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectDataLogical( PHB_ITEM pObj, char* cData, BOOL lNew )
{
   PHB_ITEM xValue = hb_itemPutL( NULL, lNew );
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectDataLong( PHB_ITEM pObj, char* cData, long nNew )
{
   PHB_ITEM xValue = hb_itemPutNL( NULL, nNew );
   //WG_warning(hb_itemTypeStr( xValue ), "WG_SetObjectDataLong - Type");
   //WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectDataLong - pObj Type");
   //WG_warning(hb_itemVal2Str( xValue ), "WG_SetObjectDataLong - Value");
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectDataString( PHB_ITEM pObj, char* cData, char *cNew )
{
   PHB_ITEM xValue = hb_itemPutCL( NULL, cNew, strlen(cNew) );
   WG_SetObjectData( pObj, cData, xValue );
   //hb_itemRelease( xValue );

}

void WG_SetObjectData( PHB_ITEM pObj, char* cData, PHB_ITEM xValue )
{
   PHB_DYNS pDyns;
   char  cMsg[200];
   char cSetData[ HB_SYMBOL_NAME_LEN + 1 ];

   //WG_warning("Passato da qui", "WG_SetObjectData");

   strcpy( cSetData, "_" );
   strcat( cSetData, cData );

   pDyns = hb_dynsymFind( cSetData );

   if( pDyns )
   {

      // WG_warning(hb_itemTypeStr( pObj ), "WG_SetObjectData - pObj tipo");
      // WG_warning(hb_itemTypeStr( xValue ), "WG_SetObjectData - tipo");
      // WG_warning(hb_itemVal2Str( xValue ), "WG_SetObjectData - value");

      hb_vmPushSymbol( pDyns->pSymbol );
      hb_vmPush( pObj );
      hb_vmPush( xValue );
      hb_vmSend( 1 );

      hb_itemRelease( xValue );
   }
   else
   {
     sprintf( cMsg, "Data %s not found.", cData );
     WG_error(cMsg, "SetObjectData");
   }

   //hb_xfree( cSetData );

}


/*
   Ron Pinkas example:

   // Same as oObj:Data
   hb_vmPushSymbol( hb_dynsymFind( "DATA" )->pSymbol )
   hb_vmPush( pObj  )
   hb_vmSend( 0 )

   Result is in &hb_stack.Return

   If you need to set a property you do like this:

   // Same as oObj:Data := xNew
   hb_vmPushSymbol( hb_dynsymFind( "_DATA" )->pSymbol ) // NOTE the _
   hb_vmPush( pObj  )
   hb_vmPush( pNew ) // The new value.
   hb_vmSend( 1 ) // 1 parameter.

   Ron
*/

const char *WG_DecodeMessageName(int message)
{

    static char s_szBuf[128];

    switch ( message )
    {
        case 0x0000: return "WM_NULL";
        case 0x0001: return "WM_CREATE";
        case 0x0002: return "WM_DESTROY";
        case 0x0003: return "WM_MOVE";
        case 0x0005: return "WM_SIZE";
        case 0x0006: return "WM_ACTIVATE";
        case 0x0007: return "WM_SETFOCUS";
        case 0x0008: return "WM_KILLFOCUS";
        case 0x000A: return "WM_ENABLE";
        case 0x000B: return "WM_SETREDRAW";
        case 0x000C: return "WM_SETTEXT";
        case 0x000D: return "WM_GETTEXT";
        case 0x000E: return "WM_GETTEXTLENGTH";
        case 0x000F: return "WM_PAINT";
        case 0x0010: return "WM_CLOSE";
        case 0x0011: return "WM_QUERYENDSESSION";
        case 0x0012: return "WM_QUIT";
        case 0x0013: return "WM_QUERYOPEN";
        case 0x0014: return "WM_ERASEBKGND";
        case 0x0015: return "WM_SYSCOLORCHANGE";
        case 0x0016: return "WM_ENDSESSION";
        case 0x0017: return "WM_SYSTEMERROR";
        case 0x0018: return "WM_SHOWWINDOW";
        case 0x0019: return "WM_CTLCOLOR";
        case 0x001A: return "WM_WININICHANGE";
        case 0x001B: return "WM_DEVMODECHANGE";
        case 0x001C: return "WM_ACTIVATEAPP";
        case 0x001D: return "WM_FONTCHANGE";
        case 0x001E: return "WM_TIMECHANGE";
        case 0x001F: return "WM_CANCELMODE";
        case 0x0020: return "WM_SETCURSOR";
        case 0x0021: return "WM_MOUSEACTIVATE";
        case 0x0022: return "WM_CHILDACTIVATE";
        case 0x0023: return "WM_QUEUESYNC";
        case 0x0024: return "WM_GETMINMAXINFO";
        case 0x0026: return "WM_PAINTICON";
        case 0x0027: return "WM_ICONERASEBKGND";
        case 0x0028: return "WM_NEXTDLGCTL";
        case 0x002A: return "WM_SPOOLERSTATUS";
        case 0x002B: return "WM_DRAWITEM";
        case 0x002C: return "WM_MEASUREITEM";
        case 0x002D: return "WM_DELETEITEM";
        case 0x002E: return "WM_VKEYTOITEM";
        case 0x002F: return "WM_CHARTOITEM";
        case 0x0030: return "WM_SETFONT";
        case 0x0031: return "WM_GETFONT";
        case 0x0037: return "WM_QUERYDRAGICON";
        case 0x0039: return "WM_COMPAREITEM";
        case 0x0041: return "WM_COMPACTING";
        case 0x0044: return "WM_COMMNOTIFY";
        case 0x0046: return "WM_WINDOWPOSCHANGING";
        case 0x0047: return "WM_WINDOWPOSCHANGED";
        case 0x0048: return "WM_POWER";

#ifdef  __WIN32__
        case 0x004A: return "WM_COPYDATA";
        case 0x004B: return "WM_CANCELJOURNAL";
        case 0x004E: return "WM_NOTIFY";
        case 0x0050: return "WM_INPUTLANGCHANGEREQUEST";
        case 0x0051: return "WM_INPUTLANGCHANGE";
        case 0x0052: return "WM_TCARD";
        case 0x0053: return "WM_HELP";
        case 0x0054: return "WM_USERCHANGED";
        case 0x0055: return "WM_NOTIFYFORMAT";
        case 0x007B: return "WM_CONTEXTMENU";
        case 0x007C: return "WM_STYLECHANGING";
        case 0x007D: return "WM_STYLECHANGED";
        case 0x007E: return "WM_DISPLAYCHANGE";
        case 0x007F: return "WM_GETICON";
        case 0x0080: return "WM_SETICON";
#endif  //WIN32

        case 0x0081: return "WM_NCCREATE";
        case 0x0082: return "WM_NCDESTROY";
        case 0x0083: return "WM_NCCALCSIZE";
        case 0x0084: return "WM_NCHITTEST";
        case 0x0085: return "WM_NCPAINT";
        case 0x0086: return "WM_NCACTIVATE";
        case 0x0087: return "WM_GETDLGCODE";
        case 0x00A0: return "WM_NCMOUSEMOVE";
        case 0x00A1: return "WM_NCLBUTTONDOWN";
        case 0x00A2: return "WM_NCLBUTTONUP";
        case 0x00A3: return "WM_NCLBUTTONDBLCLK";
        case 0x00A4: return "WM_NCRBUTTONDOWN";
        case 0x00A5: return "WM_NCRBUTTONUP";
        case 0x00A6: return "WM_NCRBUTTONDBLCLK";
        case 0x00A7: return "WM_NCMBUTTONDOWN";
        case 0x00A8: return "WM_NCMBUTTONUP";
        case 0x00A9: return "WM_NCMBUTTONDBLCLK";

        // Edit Control Messages
        case 0x00B0: return "EM_GETSEL";
        case 0x00B1: return "EM_SETSEL";
        case 0x00B2: return "EM_GETRECT";
        case 0x00B3: return "EM_SETRECT";
        case 0x00B4: return "EM_SETRECTNP";
        case 0x00B5: return "EM_SCROLL";
        case 0x00B6: return "EM_LINESCROLL";
        case 0x00B7: return "EM_SCROLLCARET";
        case 0x00B8: return "EM_GETMODIFY";
        case 0x00B9: return "EM_SETMODIFY";
        case 0x00BA: return "EM_GETLINECOUNT";
        case 0x00BB: return "EM_LINEINDEX";
        case 0x00BC: return "EM_SETHANDLE";
        case 0x00BD: return "EM_GETHANDLE";
        case 0x00BE: return "EM_GETTHUMB";
        case 0x00C1: return "EM_LINELENGTH";
        case 0x00C2: return "EM_REPLACESEL";
        case 0x00C4: return "EM_GETLINE";
        case 0x00C5: return "EM_LIMITTEXT";
        case 0x00C6: return "EM_CANUNDO";
        case 0x00C7: return "EM_UNDO";
        case 0x00C8: return "EM_FMTLINES";
        case 0x00C9: return "EM_LINEFROMCHAR";
        case 0x00CB: return "EM_SETTABSTOPS";
        case 0x00CC: return "EM_SETPASSWORDCHAR";
        case 0x00CD: return "EM_EMPTYUNDOBUFFER";
        case 0x00CE: return "EM_GETFIRSTVISIBLELINE";
        case 0x00CF: return "EM_SETREADONLY";
        case 0x00D0: return "EM_SETWORDBREAKPROC";
        case 0x00D1: return "EM_GETWORDBREAKPROC";
        case 0x00D2: return "EM_GETPASSWORDCHAR";

        //#if(WINVER >= 0x0400)
        case 0x00D3: return "EM_SETMARGINS";
        case 0x00D4: return "EM_GETMARGINS";
        //#define EM_SETLIMITTEXT         EM_LIMITTEXT   /* ;win40 Name change */
        case 0x00D5: return "EM_GETLIMITTEXT";
        case 0x00D6: return "EM_POSFROMCHAR";
        case 0x00D7: return "EM_CHARFROMPOS";
        //#endif /* WINVER >= 0x0400 */

        //#if(WINVER >= 0x0500)
        case 0x00D8: return "EM_SETIMESTATUS";
        case 0x00D9: return "EM_GETIMESTATUS";
        //#endif /* WINVER >= 0x0500 */

        case 0x0100: return "WM_KEYDOWN";
        case 0x0101: return "WM_KEYUP";
        case 0x0102: return "WM_CHAR";
        case 0x0103: return "WM_DEADCHAR";
        case 0x0104: return "WM_SYSKEYDOWN";
        case 0x0105: return "WM_SYSKEYUP";
        case 0x0106: return "WM_SYSCHAR";
        case 0x0107: return "WM_SYSDEADCHAR";
        case 0x0108: return "WM_KEYLAST";

#ifdef  __WIN32__
        case 0x010D: return "WM_IME_STARTCOMPOSITION";
        case 0x010E: return "WM_IME_ENDCOMPOSITION";
        case 0x010F: return "WM_IME_COMPOSITION";
#endif  //WIN32

        case 0x0110: return "WM_INITDIALOG";
        case 0x0111: return "WM_COMMAND";
        case 0x0112: return "WM_SYSCOMMAND";
        case 0x0113: return "WM_TIMER";
        case 0x0114: return "WM_HSCROLL";
        case 0x0115: return "WM_VSCROLL";
        case 0x0116: return "WM_INITMENU";
        case 0x0117: return "WM_INITMENUPOPUP";
        case 0x011F: return "WM_MENUSELECT";
        case 0x0120: return "WM_MENUCHAR";
        case 0x0121: return "WM_ENTERIDLE";

        // #if(WINVER >= 0x0500)
        // #ifndef _WIN32_WCE
        case 0x0122: return "WM_MENURBUTTONUP";
        case 0x0123: return "WM_MENUDRAG";
        case 0x0124: return "WM_MENUGETOBJECT";
        case 0x0125: return "WM_UNINITMENUPOPUP";
        case 0x0126: return "WM_MENUCOMMAND";

        // #ifndef _WIN32_WCE
        // #if(_WIN32_WINNT >= 0x0500)
        case 0x0127: return "WM_CHANGEUISTATE";
        case 0x0128: return "WM_UPDATEUISTATE";
        case 0x0129: return "WM_QUERYUISTATE";

        case 0x0132: return "WM_CTLCOLORMSGBOX";
        case 0x0133: return "WM_CTLCOLOREDIT";
        case 0x0134: return "WM_CTLCOLORLISTBOX";
        case 0x0135: return "WM_CTLCOLORBTN";
        case 0x0136: return "WM_CTLCOLORDLG";
        case 0x0137: return "WM_CTLCOLORSCROLLBAR";
        case 0x0138: return "WM_CTLCOLORSTATIC";

        case 0x0200: return "WM_MOUSEMOVE";
        case 0x0201: return "WM_LBUTTONDOWN";
        case 0x0202: return "WM_LBUTTONUP";
        case 0x0203: return "WM_LBUTTONDBLCLK";
        case 0x0204: return "WM_RBUTTONDOWN";
        case 0x0205: return "WM_RBUTTONUP";
        case 0x0206: return "WM_RBUTTONDBLCLK";
        case 0x0207: return "WM_MBUTTONDOWN";
        case 0x0208: return "WM_MBUTTONUP";
        case 0x0209: return "WM_MBUTTONDBLCLK";
        case 0x02A0: return "WM_NCMOUSEHOVER";
        case 0x02A2: return "WM_NCMOUSELEAVE";
        case 0x0210: return "WM_PARENTNOTIFY";
        case 0x0211: return "WM_ENTERMENULOOP";
        case 0x0212: return "WM_EXITMENULOOP";

#ifdef  __WIN32__
        case 0x0213: return "WM_NEXTMENU";
        case 0x0214: return "WM_SIZING";
        case 0x0215: return "WM_CAPTURECHANGED";
        case 0x0216: return "WM_MOVING";
        case 0x0218: return "WM_POWERBROADCAST";
        case 0x0219: return "WM_DEVICECHANGE";
#endif  //WIN32

        case 0x0220: return "WM_MDICREATE";
        case 0x0221: return "WM_MDIDESTROY";
        case 0x0222: return "WM_MDIACTIVATE";
        case 0x0223: return "WM_MDIRESTORE";
        case 0x0224: return "WM_MDINEXT";
        case 0x0225: return "WM_MDIMAXIMIZE";
        case 0x0226: return "WM_MDITILE";
        case 0x0227: return "WM_MDICASCADE";
        case 0x0228: return "WM_MDIICONARRANGE";
        case 0x0229: return "WM_MDIGETACTIVE";
        case 0x0230: return "WM_MDISETMENU";
        case 0x0233: return "WM_DROPFILES";

#ifdef  __WIN32__
        case 0x0281: return "WM_IME_SETCONTEXT";
        case 0x0282: return "WM_IME_NOTIFY";
        case 0x0283: return "WM_IME_CONTROL";
        case 0x0284: return "WM_IME_COMPOSITIONFULL";
        case 0x0285: return "WM_IME_SELECT";
        case 0x0286: return "WM_IME_CHAR";
        case 0x0290: return "WM_IME_KEYDOWN";
        case 0x0291: return "WM_IME_KEYUP";
#endif  //WIN32

        case 0x0300: return "WM_CUT";
        case 0x0301: return "WM_COPY";
        case 0x0302: return "WM_PASTE";
        case 0x0303: return "WM_CLEAR";
        case 0x0304: return "WM_UNDO";
        case 0x0305: return "WM_RENDERFORMAT";
        case 0x0306: return "WM_RENDERALLFORMATS";
        case 0x0307: return "WM_DESTROYCLIPBOARD";
        case 0x0308: return "WM_DRAWCLIPBOARD";
        case 0x0309: return "WM_PAINTCLIPBOARD";
        case 0x030A: return "WM_VSCROLLCLIPBOARD";
        case 0x030B: return "WM_SIZECLIPBOARD";
        case 0x030C: return "WM_ASKCBFORMATNAME";
        case 0x030D: return "WM_CHANGECBCHAIN";
        case 0x030E: return "WM_HSCROLLCLIPBOARD";
        case 0x030F: return "WM_QUERYNEWPALETTE";
        case 0x0310: return "WM_PALETTEISCHANGING";
        case 0x0311: return "WM_PALETTECHANGED";
        case 0x0312: return "WM_HOTKEY";
        case 0x0317: return "WM_PRINT";
        case 0x0318: return "WM_PRINTCLIENT";

        case 0x0400: return "WM_USER";

#ifdef __WIN32__
        // common controls messages - although they're not strictly speaking
        // standard, it's nice to decode them nevertheless

        // listview
        case 0x1000 + 0: return "LVM_GETBKCOLOR";
        case 0x1000 + 1: return "LVM_SETBKCOLOR";
        case 0x1000 + 2: return "LVM_GETIMAGELIST";
        case 0x1000 + 3: return "LVM_SETIMAGELIST";
        case 0x1000 + 4: return "LVM_GETITEMCOUNT";
        case 0x1000 + 5: return "LVM_GETITEMA";
        case 0x1000 + 75: return "LVM_GETITEMW";
        case 0x1000 + 6: return "LVM_SETITEMA";
        case 0x1000 + 76: return "LVM_SETITEMW";
        case 0x1000 + 7: return "LVM_INSERTITEMA";
        case 0x1000 + 77: return "LVM_INSERTITEMW";
        case 0x1000 + 8: return "LVM_DELETEITEM";
        case 0x1000 + 9: return "LVM_DELETEALLITEMS";
        case 0x1000 + 10: return "LVM_GETCALLBACKMASK";
        case 0x1000 + 11: return "LVM_SETCALLBACKMASK";
        case 0x1000 + 12: return "LVM_GETNEXTITEM";
        case 0x1000 + 13: return "LVM_FINDITEMA";
        case 0x1000 + 83: return "LVM_FINDITEMW";
        case 0x1000 + 14: return "LVM_GETITEMRECT";
        case 0x1000 + 15: return "LVM_SETITEMPOSITION";
        case 0x1000 + 16: return "LVM_GETITEMPOSITION";
        case 0x1000 + 17: return "LVM_GETSTRINGWIDTHA";
        case 0x1000 + 87: return "LVM_GETSTRINGWIDTHW";
        case 0x1000 + 18: return "LVM_HITTEST";
        case 0x1000 + 19: return "LVM_ENSUREVISIBLE";
        case 0x1000 + 20: return "LVM_SCROLL";
        case 0x1000 + 21: return "LVM_REDRAWITEMS";
        case 0x1000 + 22: return "LVM_ARRANGE";
        case 0x1000 + 23: return "LVM_EDITLABELA";
        case 0x1000 + 118: return "LVM_EDITLABELW";
        case 0x1000 + 24: return "LVM_GETEDITCONTROL";
        case 0x1000 + 25: return "LVM_GETCOLUMNA";
        case 0x1000 + 95: return "LVM_GETCOLUMNW";
        case 0x1000 + 26: return "LVM_SETCOLUMNA";
        case 0x1000 + 96: return "LVM_SETCOLUMNW";
        case 0x1000 + 27: return "LVM_INSERTCOLUMNA";
        case 0x1000 + 97: return "LVM_INSERTCOLUMNW";
        case 0x1000 + 28: return "LVM_DELETECOLUMN";
        case 0x1000 + 29: return "LVM_GETCOLUMNWIDTH";
        case 0x1000 + 30: return "LVM_SETCOLUMNWIDTH";
        case 0x1000 + 31: return "LVM_GETHEADER";
        case 0x1000 + 33: return "LVM_CREATEDRAGIMAGE";
        case 0x1000 + 34: return "LVM_GETVIEWRECT";
        case 0x1000 + 35: return "LVM_GETTEXTCOLOR";
        case 0x1000 + 36: return "LVM_SETTEXTCOLOR";
        case 0x1000 + 37: return "LVM_GETTEXTBKCOLOR";
        case 0x1000 + 38: return "LVM_SETTEXTBKCOLOR";
        case 0x1000 + 39: return "LVM_GETTOPINDEX";
        case 0x1000 + 40: return "LVM_GETCOUNTPERPAGE";
        case 0x1000 + 41: return "LVM_GETORIGIN";
        case 0x1000 + 42: return "LVM_UPDATE";
        case 0x1000 + 43: return "LVM_SETITEMSTATE";
        case 0x1000 + 44: return "LVM_GETITEMSTATE";
        case 0x1000 + 45: return "LVM_GETITEMTEXTA";
        case 0x1000 + 115: return "LVM_GETITEMTEXTW";
        case 0x1000 + 46: return "LVM_SETITEMTEXTA";
        case 0x1000 + 116: return "LVM_SETITEMTEXTW";
        case 0x1000 + 47: return "LVM_SETITEMCOUNT";
        case 0x1000 + 48: return "LVM_SORTITEMS";
        case 0x1000 + 49: return "LVM_SETITEMPOSITION32";
        case 0x1000 + 50: return "LVM_GETSELECTEDCOUNT";
        case 0x1000 + 51: return "LVM_GETITEMSPACING";
        case 0x1000 + 52: return "LVM_GETISEARCHSTRINGA";
        case 0x1000 + 117: return "LVM_GETISEARCHSTRINGW";
        case 0x1000 + 53: return "LVM_SETICONSPACING";
        case 0x1000 + 54: return "LVM_SETEXTENDEDLISTVIEWSTYLE";
        case 0x1000 + 55: return "LVM_GETEXTENDEDLISTVIEWSTYLE";
        case 0x1000 + 56: return "LVM_GETSUBITEMRECT";
        case 0x1000 + 57: return "LVM_SUBITEMHITTEST";
        case 0x1000 + 58: return "LVM_SETCOLUMNORDERARRAY";
        case 0x1000 + 59: return "LVM_GETCOLUMNORDERARRAY";
        case 0x1000 + 60: return "LVM_SETHOTITEM";
        case 0x1000 + 61: return "LVM_GETHOTITEM";
        case 0x1000 + 62: return "LVM_SETHOTCURSOR";
        case 0x1000 + 63: return "LVM_GETHOTCURSOR";
        case 0x1000 + 64: return "LVM_APPROXIMATEVIEWRECT";
        case 0x1000 + 65: return "LVM_SETWORKAREA";

        // tree view
        case 0x1100 + 0: return "TVM_INSERTITEMA";
        case 0x1100 + 50: return "TVM_INSERTITEMW";
        case 0x1100 + 1: return "TVM_DELETEITEM";
        case 0x1100 + 2: return "TVM_EXPAND";
        case 0x1100 + 4: return "TVM_GETITEMRECT";
        case 0x1100 + 5: return "TVM_GETCOUNT";
        case 0x1100 + 6: return "TVM_GETINDENT";
        case 0x1100 + 7: return "TVM_SETINDENT";
        case 0x1100 + 8: return "TVM_GETIMAGELIST";
        case 0x1100 + 9: return "TVM_SETIMAGELIST";
        case 0x1100 + 10: return "TVM_GETNEXTITEM";
        case 0x1100 + 11: return "TVM_SELECTITEM";
        case 0x1100 + 12: return "TVM_GETITEMA";
        case 0x1100 + 62: return "TVM_GETITEMW";
        case 0x1100 + 13: return "TVM_SETITEMA";
        case 0x1100 + 63: return "TVM_SETITEMW";
        case 0x1100 + 14: return "TVM_EDITLABELA";
        case 0x1100 + 65: return "TVM_EDITLABELW";
        case 0x1100 + 15: return "TVM_GETEDITCONTROL";
        case 0x1100 + 16: return "TVM_GETVISIBLECOUNT";
        case 0x1100 + 17: return "TVM_HITTEST";
        case 0x1100 + 18: return "TVM_CREATEDRAGIMAGE";
        case 0x1100 + 19: return "TVM_SORTCHILDREN";
        case 0x1100 + 20: return "TVM_ENSUREVISIBLE";
        case 0x1100 + 21: return "TVM_SORTCHILDRENCB";
        case 0x1100 + 22: return "TVM_ENDEDITLABELNOW";
        case 0x1100 + 23: return "TVM_GETISEARCHSTRINGA";
        case 0x1100 + 64: return "TVM_GETISEARCHSTRINGW";
        case 0x1100 + 24: return "TVM_SETTOOLTIPS";
        case 0x1100 + 25: return "TVM_GETTOOLTIPS";

        // header
        case 0x1200 + 0: return "HDM_GETITEMCOUNT";
        case 0x1200 + 1: return "HDM_INSERTITEMA";
        case 0x1200 + 10: return "HDM_INSERTITEMW";
        case 0x1200 + 2: return "HDM_DELETEITEM";
        case 0x1200 + 3: return "HDM_GETITEMA";
        case 0x1200 + 11: return "HDM_GETITEMW";
        case 0x1200 + 4: return "HDM_SETITEMA";
        case 0x1200 + 12: return "HDM_SETITEMW";
        case 0x1200 + 5: return "HDM_LAYOUT";
        case 0x1200 + 6: return "HDM_HITTEST";
        case 0x1200 + 7: return "HDM_GETITEMRECT";
        case 0x1200 + 8: return "HDM_SETIMAGELIST";
        case 0x1200 + 9: return "HDM_GETIMAGELIST";
        case 0x1200 + 15: return "HDM_ORDERTOINDEX";
        case 0x1200 + 16: return "HDM_CREATEDRAGIMAGE";
        case 0x1200 + 17: return "HDM_GETORDERARRAY";
        case 0x1200 + 18: return "HDM_SETORDERARRAY";
        case 0x1200 + 19: return "HDM_SETHOTDIVIDER";

        // tab control
        case 0x1300 + 2: return "TCM_GETIMAGELIST";
        case 0x1300 + 3: return "TCM_SETIMAGELIST";
        case 0x1300 + 4: return "TCM_GETITEMCOUNT";
        case 0x1300 + 5: return "TCM_GETITEMA";
        case 0x1300 + 60: return "TCM_GETITEMW";
        case 0x1300 + 6: return "TCM_SETITEMA";
        case 0x1300 + 61: return "TCM_SETITEMW";
        case 0x1300 + 7: return "TCM_INSERTITEMA";
        case 0x1300 + 62: return "TCM_INSERTITEMW";
        case 0x1300 + 8: return "TCM_DELETEITEM";
        case 0x1300 + 9: return "TCM_DELETEALLITEMS";
        case 0x1300 + 10: return "TCM_GETITEMRECT";
        case 0x1300 + 11: return "TCM_GETCURSEL";
        case 0x1300 + 12: return "TCM_SETCURSEL";
        case 0x1300 + 13: return "TCM_HITTEST";
        case 0x1300 + 14: return "TCM_SETITEMEXTRA";
        case 0x1300 + 40: return "TCM_ADJUSTRECT";
        case 0x1300 + 41: return "TCM_SETITEMSIZE";
        case 0x1300 + 42: return "TCM_REMOVEIMAGE";
        case 0x1300 + 43: return "TCM_SETPADDING";
        case 0x1300 + 44: return "TCM_GETROWCOUNT";
        case 0x1300 + 45: return "TCM_GETTOOLTIPS";
        case 0x1300 + 46: return "TCM_SETTOOLTIPS";
        case 0x1300 + 47: return "TCM_GETCURFOCUS";
        case 0x1300 + 48: return "TCM_SETCURFOCUS";
        case 0x1300 + 49: return "TCM_SETMINTABWIDTH";
        case 0x1300 + 50: return "TCM_DESELECTALL";

        // toolbar
        case WM_USER+1: return "TB_ENABLEBUTTON";
        case WM_USER+2: return "TB_CHECKBUTTON";
        case WM_USER+3: return "TB_PRESSBUTTON";
        case WM_USER+4: return "TB_HIDEBUTTON";
        case WM_USER+5: return "TB_INDETERMINATE";
        case WM_USER+9: return "TB_ISBUTTONENABLED";
        case WM_USER+10: return "TB_ISBUTTONCHECKED";
        case WM_USER+11: return "TB_ISBUTTONPRESSED";
        case WM_USER+12: return "TB_ISBUTTONHIDDEN";
        case WM_USER+13: return "TB_ISBUTTONINDETERMINATE";
        case WM_USER+17: return "TB_SETSTATE";
        case WM_USER+18: return "TB_GETSTATE";
        case WM_USER+19: return "TB_ADDBITMAP";
        case WM_USER+20: return "TB_ADDBUTTONS";
        case WM_USER+21: return "TB_INSERTBUTTON";
        case WM_USER+22: return "TB_DELETEBUTTON";
        case WM_USER+23: return "TB_GETBUTTON";
        case WM_USER+24: return "TB_BUTTONCOUNT";
        case WM_USER+25: return "TB_COMMANDTOINDEX";
        case WM_USER+26: return "TB_SAVERESTOREA";
        case WM_USER+76: return "TB_SAVERESTOREW";
        case WM_USER+27: return "TB_CUSTOMIZE";
        case WM_USER+28: return "TB_ADDSTRINGA";
        case WM_USER+77: return "TB_ADDSTRINGW";
        case WM_USER+29: return "TB_GETITEMRECT";
        case WM_USER+30: return "TB_BUTTONSTRUCTSIZE";
        case WM_USER+31: return "TB_SETBUTTONSIZE";
        case WM_USER+32: return "TB_SETBITMAPSIZE";
        case WM_USER+33: return "TB_AUTOSIZE";
        case WM_USER+35: return "TB_GETTOOLTIPS";
        case WM_USER+36: return "TB_SETTOOLTIPS";
        case WM_USER+37: return "TB_SETPARENT";
        case WM_USER+39: return "TB_SETROWS";
        case WM_USER+40: return "TB_GETROWS";
        case WM_USER+42: return "TB_SETCMDID";
        case WM_USER+43: return "TB_CHANGEBITMAP";
        case WM_USER+44: return "TB_GETBITMAP";
        case WM_USER+45: return "TB_GETBUTTONTEXTA";
        case WM_USER+75: return "TB_GETBUTTONTEXTW";
        case WM_USER+46: return "TB_REPLACEBITMAP";
        case WM_USER+47: return "TB_SETINDENT";
        case WM_USER+48: return "TB_SETIMAGELIST";
        case WM_USER+49: return "TB_GETIMAGELIST";
        case WM_USER+50: return "TB_LOADIMAGES";
        case WM_USER+51: return "TB_GETRECT";
        case WM_USER+52: return "TB_SETHOTIMAGELIST";
        case WM_USER+53: return "TB_GETHOTIMAGELIST";
        case WM_USER+54: return "TB_SETDISABLEDIMAGELIST";
        case WM_USER+55: return "TB_GETDISABLEDIMAGELIST";
        case WM_USER+56: return "TB_SETSTYLE";
        case WM_USER+57: return "TB_GETSTYLE";
        case WM_USER+58: return "TB_GETBUTTONSIZE";
        case WM_USER+59: return "TB_SETBUTTONWIDTH";
        case WM_USER+60: return "TB_SETMAXTEXTROWS";
        case WM_USER+61: return "TB_GETTEXTROWS";
        case WM_USER+41: return "TB_GETBITMAPFLAGS";

#endif //WIN32

        default:
            sprintf(s_szBuf, "<unknown msg = %d>", message);
            return s_szBuf;
    }
}

HB_FUNC ( WG_DECODEMESSAGENAME )
{
    int nMessage = (int) hb_parni(1);
    hb_retc( WG_DecodeMessageName( nMessage ) );
}

//HB_FUNC ( WG_CONTROLPAINT )
//{
//    HWND hwnd = (HWND) hb_parnl( 1 );
//    PAINTSTRUCT ps;
//    HDC hdc;
//
//    hdc = BeginPaint(hwnd, &ps);
//    if (hdc) {
//        char sz[64];
//        RECT rc;
//        HFONT hfOld;
//        COLORREF crTextOld, crBkOld;
//        int cch;
//
//        if (IsRectEmpty(&ps.rcPaint)) {
//            /*
//             *  Nothing to do.  Don't wake up either.
//             */
//        } else {
//            hfOld = SelectObject(hdc, GetStockObject(ANSI_FIXED_FONT));
//            crTextOld = SetTextColor(hdc, hb_parnl( 2 ) ); //GetSysColor(COLOR_WINDOWTEXT));
//            crBkOld = SetBkColor(hdc, hb_parnl( 3 ) ); //GetSysColor(COLOR_WINDOW));
//
//            GetClientRect(hwnd, &rc);
//            //cch = wsprintf(sz, "%u", GetTickCount());
//            ExtTextOut(hdc, 0, 0, ETO_OPAQUE, &rc, sz, cch, NULL);
//
//            SetBkColor(hdc, crBkOld);
//            SetTextColor(hdc, crTextOld);
//            SelectObject(hdc, hfOld);
//
//        }
//
//        EndPaint(hwnd, &ps);
//    }
//}
//
//HB_FUNC( WG_DRAWITEM )
//{   HWND hwnd;
//    HDC hdc;
//    HBRUSH brush;
//    LPDRAWITEMSTRUCT lpdis;
//    TEXTMETRIC tm;
//    RECT rcBitmap;
//    char tchBuffer[MAX_PATH];
//    int y;
//
//    lpdis = (LPDRAWITEMSTRUCT) hb_parnl(1);;
//    hwnd  = (HWND) hb_parnl (2);
//    hdc   = (HDC) lpdis->hDC;
//
//    switch (lpdis->itemAction) {
//        case ODA_SELECT:
//        case ODA_DRAWENTIRE:
//            rcBitmap.left = lpdis->rcItem.left;
//            rcBitmap.top = lpdis->rcItem.top;
//            rcBitmap.right = lpdis->rcItem.right;
//            rcBitmap.bottom = lpdis->rcItem.bottom;
//
//            if ((hb_pcount() > 2) && (!ISNIL(3)))
//            { brush = CreateSolidBrush( hb_parni(3) );
//              FillRect(hdc, &rcBitmap, brush);
//              DeleteObject(brush);
//            }
//            else
//            { brush = (HBRUSH)( COLOR_BTNFACE + 1 );
//              FillRect(hdc, &rcBitmap, brush);
//            }
//
//            GetWindowText(hwnd, tchBuffer, MAX_PATH);
//            GetTextMetrics(hdc, &tm);
//
//            y = (lpdis->rcItem.bottom + lpdis->rcItem.top - tm.tmHeight) / 2;
//
//            if ((hb_pcount() > 3) && (!ISNIL(4)))
//              SetTextColor(hdc, hb_parni(4) );
//
//            SetBkMode(hdc, TRANSPARENT);
//            TextOut(hdc, 0, y, tchBuffer, GetWindowTextLength(hwnd));
//            break;
//
//        case ODA_FOCUS:
//            break;
//    }
//}                                                                            