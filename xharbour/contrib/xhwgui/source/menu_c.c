/*
 * HWGUI - Harbour Win32 GUI library source code:
 * C level menu functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#define HB_OS_WIN_32_USED

#define _WIN32_WINNT 0x0400
#define OEMRESOURCE
#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

extern HWND aWindows[];

/*
 *  CreateMenu() --> hMenu
 */
HB_FUNC ( HWG__CREATEMENU )
{
   HMENU hMenu = CreateMenu();
   hb_retnl( (LONG) hMenu );
}

HB_FUNC ( HWG__CREATEPOPUPMENU )
{
   HMENU hMenu = CreatePopupMenu();
   hb_retnl( (LONG) hMenu );
}

/*
 *  AddMenuItem( hMenu,cCaption,nPos,fByPosition,nId,fState,lSubMenu ) --> lResult
 */

HB_FUNC( HWG__ADDMENUITEM )
{
   UINT uFlags = MF_BYPOSITION | ( ( ISNIL(6) || hb_parl(6) )? 0:MFS_DISABLED );
   LPCTSTR lpNewItem = NULL;

   if( ISCHAR( 2 ) )
   {
      lpNewItem	 = (LPCTSTR) hb_parc(2);
      uFlags |= MF_STRING;
   }
   else
      uFlags |= MF_SEPARATOR;

   if( hb_parl(7) )
   {
      HMENU hSubMenu = CreateMenu();

      uFlags |= MF_POPUP;
      InsertMenu( ( HMENU ) hb_parnl(1), hb_parni(3),
       uFlags,	// menu item flags
       (UINT)hSubMenu,	// menu item identifier or handle of drop-down menu or submenu 
       lpNewItem	// menu item content
      );
      hb_retnl( (LONG) hSubMenu );
   }
   else
   {
      InsertMenu( ( HMENU ) hb_parnl(1), hb_parni(3),
       uFlags,	// menu item flags
       hb_parni( 5 ),	// menu item identifier or handle of drop-down menu or submenu 
       lpNewItem	// menu item content
      );
      hb_retnl(0);
   }
}

/*
HB_FUNC( HWG__ADDMENUITEM )
{

   MENUITEMINFO mii;
   BOOL fByPosition = ( ISNIL(4) )? 0:(BOOL) hb_parl(4);

   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask = MIIM_TYPE | MIIM_STATE | MIIM_ID;
   mii.fState = ( ISNIL(6) || hb_parl( 6 ) )? 0:MFS_DISABLED;
   mii.wID = hb_parni( 5 );
   if( ISCHAR( 2 ) )
   {
      mii.dwTypeData = hb_parc( 2 );
      mii.cch = strlen( mii.dwTypeData );
      mii.fType = MFT_STRING;
   }
   else
      mii.fType = MFT_SEPARATOR;

   hb_retl( InsertMenuItem( ( HMENU ) hb_parnl( 1 ),
     hb_parni( 3 ), fByPosition, &mii
   ) );
}
*/

/*
 *  CreateSubMenu( hMenu, nMenuId ) --> hSubMenu
 */
HB_FUNC( HWG__CREATESUBMENU )
{

   MENUITEMINFO mii;
   HMENU hSubMenu = CreateMenu();

   mii.cbSize = sizeof( MENUITEMINFO );
   mii.fMask = MIIM_SUBMENU;
   mii.hSubMenu = hSubMenu;

   if( SetMenuItemInfo( ( HMENU ) hb_parnl( 1 ), hb_parni( 2 ), 0, &mii ) )
      hb_retnl( (LONG) hSubMenu );
   else
      hb_retnl( 0 );
}

/*
 *  SetMenu( hWnd, hMenu ) --> lResult
 */
HB_FUNC( HWG__SETMENU )
{
   hb_retl( SetMenu( ( HWND ) hb_parnl( 1 ), ( HMENU ) hb_parnl( 2 ) ) );
}

HB_FUNC ( CHECKMENUITEM )
{
   HMENU hMenu = GetMenu( ( hb_pcount()>0 && !ISNIL(1) )? (HWND)hb_parnl(1):aWindows[0] );
   UINT  uCheck = ( hb_pcount() < 3 || !ISLOG( 3 ) || hb_parl( 3 ) )? MF_CHECKED:MF_UNCHECKED;

   if( !hMenu )
      MessageBox( GetActiveWindow(), "", "No Menu!", MB_OK | MB_ICONINFORMATION );
   else
   {
      CheckMenuItem(
         hMenu,	                // handle to menu 
         hb_parni( 2 ),         // menu item to check or uncheck
         MF_BYCOMMAND | uCheck  // menu item flags 
      );
   }
}

HB_FUNC ( ISCHECKEDMENUITEM )
{
   HMENU hMenu = GetMenu( ( hb_pcount()>0 && !ISNIL(1) )? (HWND)hb_parnl(1):aWindows[0] );
   UINT  uCheck;

   if( !hMenu )
      hb_retl( 0 );
   else
   {
      uCheck = GetMenuState(
         hMenu,	                // handle to menu 
         hb_parni( 2 ),         // menu item to check or uncheck
         MF_BYCOMMAND           // menu item flags 
      );
      hb_retl( uCheck & MF_CHECKED );
   }
}

HB_FUNC ( ENABLEMENUITEM )
{
   // HMENU hMenu = GetMenu( ( hb_pcount()>0 && !ISNIL(1) )? (HWND)hb_parnl(1):aWindows[0] );
   HMENU hMenu = ( hb_pcount()>0 && !ISNIL(1) )? (HMENU)hb_parnl(1) : GetMenu( aWindows[0] );
   UINT  uEnable = ( hb_pcount() < 3 || !ISLOG( 3 ) || hb_parl( 3 ) )? MF_ENABLED:MF_GRAYED;
   UINT  uFlag = ( hb_pcount() < 4 || !ISLOG( 4 ) || hb_parl( 4 ) )? MF_BYCOMMAND:MF_BYPOSITION;

   if( !hMenu )
   {
      MessageBox( GetActiveWindow(), "", "No Menu!", MB_OK | MB_ICONINFORMATION );
      hb_retnl( -1 );
   }
   else
   {
      hb_retnl( (LONG) EnableMenuItem(
         hMenu,	                // handle to menu 
         hb_parni( 2 ),         // menu item to check or uncheck
         uFlag | uEnable // menu item flags 
      ) );
   }
}

HB_FUNC ( ISENABLEDMENUITEM )
{
   HMENU hMenu = GetMenu( ( hb_pcount()>0 && !ISNIL(1) )? (HWND)hb_parnl(1):aWindows[0] );
   UINT  uCheck;
   UINT  uFlag = ( hb_pcount() < 3 || !ISLOG( 3 ) || hb_parl( 3 ) )? MF_BYCOMMAND:MF_BYPOSITION;

   if( !hMenu )
      hb_retl( 0 );
   else
   {
      uCheck = GetMenuState(
         hMenu,	                // handle to menu 
         hb_parni( 3 ),         // menu item to check or uncheck
         uFlag           // menu item flags 
      );
      hb_retl( !( uCheck & MF_GRAYED ) );
   }
}

HB_FUNC ( HWG_TRACKMENU )
{
    hb_retl( TrackPopupMenu(
                  (HMENU) hb_parnl(1),  // handle of shortcut menu
                  TPM_RIGHTALIGN,       // screen-position and mouse-button flags
                  hb_parni(2),          // horizontal position, in screen coordinates
                  hb_parni(3),          // vertical position, in screen coordinates
                  0,                    // reserved, must be zero
                  (HWND) hb_parnl(4),   // handle of owner window
                  NULL
    ) );
}

HB_FUNC( HWG_DESTROYMENU )
{
   hb_retl( DestroyMenu( (HMENU) hb_parnl(1) ) );
}

/*
 * CreateAcceleratorTable( _aAccel )
 */
HB_FUNC( CREATEACCELERATORTABLE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY ), pSubArr;
   LPACCEL lpaccl;
   int cEntries = (int) pArray->item.asArray.value->ulLen;
   int i;
   HACCEL h;

   lpaccl = (LPACCEL) hb_xgrab( sizeof(ACCEL)*cEntries );

   for( i=0; i<cEntries; i++ )
   {
      pSubArr = pArray->item.asArray.value->pItems + i;
      lpaccl[i].fVirt = (BYTE) hb_itemGetNL( pSubArr->item.asArray.value->pItems ) | FNOINVERT | FVIRTKEY;
      lpaccl[i].key = (WORD) hb_itemGetNL( pSubArr->item.asArray.value->pItems + 1 );
      lpaccl[i].cmd = (WORD) hb_itemGetNL( pSubArr->item.asArray.value->pItems + 2 );
   }
   h = CreateAcceleratorTable( lpaccl,cEntries );

   hb_xfree( lpaccl );
   hb_retnl( (LONG) h );
}

/*
 * DestroyAcceleratorTable( hAccel )
 */
HB_FUNC( DESTROYACCELERATORTABLE )
{
   hb_retl( DestroyAcceleratorTable( (HACCEL) hb_parnl(1) ) );
}
