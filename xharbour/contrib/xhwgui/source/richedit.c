/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level richedit control functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define _WIN32_IE    0x0400
#define OEMRESOURCE
#include <windows.h>
#if defined(__MINGW32__)
   #include <prsht.h>
#endif
#include <commctrl.h>
#include <richedit.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"

LRESULT APIENTRY RichSubclassProc( HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam );

static HINSTANCE hRichEd = 0;
static WNDPROC wpOrigRichProc;

HB_FUNC( CREATERICHEDIT )
{
   HWND hCtrl;

   if( !hRichEd )
      hRichEd = LoadLibrary( "riched32.dll" );

   hCtrl = CreateWindowEx( 
                 0	,     /* extended style    */
                 "RichEdit",  /* predefined class  */
                 NULL,        /* title   */
                 WS_CHILD | WS_VISIBLE | hb_parnl(3), /* style  */
                 hb_parni(4), hb_parni(5),            /* x, y   */
                 hb_parni(6), hb_parni(7),      /* nWidth, nHeight */
                 (HWND) hb_parnl(1),           /* parent window    */ 
                 (HMENU) hb_parni(2),               /* control ID  */
                 GetModuleHandle( NULL ), 
                 NULL);

   if( hb_pcount() > 7 )
      SendMessage( hCtrl, WM_SETTEXT, 0, (LPARAM) hb_parc(8) );  

   hb_retnl( (LONG) hCtrl );

}

/*
 * re_SetCharFormat( hCtrl, n1, n2, nColor, cName, nHeight, lBold, lItalic, lUnderline )
 */
HB_FUNC ( RE_SETCHARFORMAT )
{
   HWND hCtrl = (HWND) hb_parnl(1);
   CHARRANGE chrOld, chrNew;
   CHARFORMAT cf;
   PHB_ITEM pArr;

   SendMessage( hCtrl, EM_EXGETSEL, 0, (LPARAM) &chrOld );
   SendMessage( hCtrl, EM_HIDESELECTION, 1, 0 );

   if( ISARRAY(2) )
   {
      ULONG i;
      PHB_ITEM pArr1;
      pArr = hb_param( 2, HB_IT_ARRAY );
      for( i=0; i<pArr->item.asArray.value->ulLen; i++ )
      {
         pArr1 = pArr->item.asArray.value->pItems + i;
         chrNew.cpMin = hb_itemGetNL( pArr1->item.asArray.value->pItems )-1;
         chrNew.cpMax = hb_itemGetNL( pArr1->item.asArray.value->pItems + 1 )-1;
         SendMessage( hCtrl, EM_EXSETSEL, 0, (LPARAM) &chrNew );

         memset( &cf, 0, sizeof(CHARFORMAT) );
         cf.cbSize = sizeof(CHARFORMAT);
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 2) )->type != HB_IT_NIL )
         {
            cf.crTextColor = (COLORREF) hb_itemGetNL( pArr1->item.asArray.value->pItems + 2 );
            cf.dwMask |= CFM_COLOR;
         }
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 3) )->type != HB_IT_NIL )
         {
            strcpy( cf.szFaceName, hb_itemGetCPtr( pArr1->item.asArray.value->pItems + 3 ) );
            cf.dwMask |= CFM_FACE;
         }
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 4) )->type != HB_IT_NIL )
         {
            cf.yHeight = hb_itemGetNL( pArr1->item.asArray.value->pItems + 4 );
            cf.dwMask |= CFM_SIZE;
         }
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 5) )->type != HB_IT_NIL && hb_itemGetL( pArr1->item.asArray.value->pItems + 5 ) )
         {
            cf.dwEffects |= CFE_BOLD;
            cf.dwMask |= CFM_BOLD;
         }
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 6) )->type != HB_IT_NIL && hb_itemGetL( pArr1->item.asArray.value->pItems + 6 ) )
         {
            cf.dwEffects |= CFE_ITALIC;
            cf.dwMask |= CFM_ITALIC;
         }
         if( ( (PHB_ITEM)(pArr1->item.asArray.value->pItems + 7) )->type != HB_IT_NIL && hb_itemGetL( pArr1->item.asArray.value->pItems + 7 ) )
         {
            cf.dwEffects |= CFE_UNDERLINE;
            cf.dwMask |= CFM_UNDERLINE;
         }
         SendMessage( hCtrl, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf );
      }
   }
   else
   {
      /*   Set new selection   */
      chrNew.cpMin = hb_parnl(2)-1;
      chrNew.cpMax = hb_parnl(3)-1;
      SendMessage( hCtrl, EM_EXSETSEL, 0, (LPARAM) &chrNew );

      memset( &cf, 0, sizeof(CHARFORMAT) );
      cf.cbSize = sizeof(CHARFORMAT);

      if( !ISNIL(4) )
      {
         cf.crTextColor = (COLORREF) hb_parnl(4);
         cf.dwMask |= CFM_COLOR;
      }
      if( !ISNIL(5) )
      {
         strcpy( cf.szFaceName, hb_parc(5) );
         cf.dwMask |= CFM_FACE;
      }
      if( !ISNIL(6) )
      {
         cf.yHeight = hb_parnl(6);
         cf.dwMask |= CFM_SIZE;
      }
      if( !ISNIL(7) )
      {
         cf.dwEffects |= (hb_parl(7))? CFE_BOLD:0;
         cf.dwMask |= CFM_BOLD;
      }
      if( !ISNIL(8) )
      {
         cf.dwEffects |= (hb_parl(8))? CFE_ITALIC:0;
         cf.dwMask |= CFM_ITALIC;
      }
      if( !ISNIL(9) )
      {
         cf.dwEffects |= (hb_parl(9))? CFE_UNDERLINE:0;
         cf.dwMask |= CFM_UNDERLINE;
      }

      SendMessage( hCtrl, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM) &cf );
   }

   /*   Restore selection   */
   SendMessage( hCtrl, EM_EXSETSEL, 0, (LPARAM) &chrOld );
   SendMessage( hCtrl, EM_HIDESELECTION, 0, 0 );

}

/*
 * re_SetDefault( hCtrl, nColor, cName, nHeight, nCharset )
 */
HB_FUNC ( RE_SETDEFAULT )
{
   HWND hCtrl = (HWND) hb_parnl(1);
   CHARFORMAT cf;

   memset( &cf, 0, sizeof(CHARFORMAT) );
   cf.cbSize = sizeof(CHARFORMAT);

   if( !ISNIL(2) )
   {
      cf.crTextColor = (COLORREF) hb_parnl(2);
      cf.dwMask |= CFM_COLOR;
   }
   if( !ISNIL(3) )
   {
      strcpy( cf.szFaceName, hb_parc(3) );
      cf.dwMask |= CFM_FACE;
   }
   if( !ISNIL(4) )
   {
      cf.yHeight = hb_parnl(4);
      cf.dwMask |= CFM_SIZE;
   }
   if( !ISNIL(5) )
   {
      cf.bCharSet = hb_parnl(5);
      cf.dwMask |= CFM_CHARSET;
   }

   SendMessage( hCtrl, EM_SETCHARFORMAT, SCF_ALL, (LPARAM) &cf );

}

/*
 * re_CharFromPos( hEdit, xPos, yPos ) --> nPos
 */
HB_FUNC ( RE_CHARFROMPOS )
{
   HWND hCtrl = (HWND) hb_parnl(1);
   int x = hb_parni( 2 );
   int y = hb_parni( 3 );
   ULONG ul;
   POINTL pp;

   pp.x = x;
   pp.y = y;
   ul = SendMessage( hCtrl, EM_CHARFROMPOS, 0, (LPARAM)&pp );
   x = (int) ( ul & 0xFFFF );
   y = (int) ( ( ul >> 16 ) & 0xFFFF );
   ul = SendMessage( hCtrl, EM_LINEINDEX, (WPARAM) y, 0 );
   hb_retnl( ul + x );
}

/*
 * re_GetTextRange( hEdit, n1, n2 )
 */
HB_FUNC ( RE_GETTEXTRANGE )
{
   HWND hCtrl = (HWND) hb_parnl(1);
   TEXTRANGE tr;
   ULONG ul;

   tr.chrg.cpMin = hb_parnl(2)-1;
   tr.chrg.cpMax = hb_parnl(3)-1;

   tr.lpstrText = (LPSTR) hb_xgrab( tr.chrg.cpMax-tr.chrg.cpMin+2 );
   ul = SendMessage( hCtrl, EM_GETTEXTRANGE, 0, (LPARAM)&tr );
   hb_retclen( tr.lpstrText, ul );
   hb_xfree( tr.lpstrText );

}

HB_FUNC ( HWG_INITRICHPROC )
{
   wpOrigRichProc = (WNDPROC) SetWindowLong( (HWND) hb_parnl(1),
                                 GWL_WNDPROC, (LONG) RichSubclassProc );
}


LRESULT APIENTRY RichSubclassProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
   if( msg == WM_KEYUP || msg == WM_KEYDOWN || msg == WM_CHAR ||
      msg == WM_LBUTTONDBLCLK || msg == WM_MOUSEMOVE )
   {
      long int res;
      PHB_DYNS pSymTest;

      if( ( pSymTest = hb_dynsymFind( "DEFRICHPROC" ) ) != NULL )
      {
         hb_vmPushSymbol( pSymTest->pSymbol );
         hb_vmPushNil();
         hb_vmPushLong( (LONG ) hWnd );
         hb_vmPushLong( (LONG ) msg );
         hb_vmPushLong( (LONG ) wParam );
         hb_vmPushLong( (LONG ) lParam );
         hb_vmDo( 4 );
         res = hb_itemGetNL( (PHB_ITEM) &hb_stack.Return );
         if( res == -1 )
            return CallWindowProc( wpOrigRichProc, hWnd, msg, wParam, lParam );
         else
            return res;
      }
   }
   return CallWindowProc( wpOrigRichProc, hWnd, msg, wParam, lParam );
}
