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
* Low Level C Menu Routines
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


// The ModifyMenu function changes an existing menu item. This function is used to specify the
// content, appearance, and behavior of the menu item.
//
HB_FUNC( MODIFYMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
  UINT uFlags            = (UINT)  hb_parni(3);        // options
  UINT_PTR uIDNewItem    = (UINT_PTR) hb_parni(4);     // identifier, menu, or submenu
  LPCTSTR lpNewItem      = (LPCTSTR)  hb_parcx(5);      // menu item content

  hb_retl( (BOOL) ModifyMenu( hMenu, uPosition, uFlags, uIDNewItem, lpNewItem ) );
}

// The RemoveMenu function deletes a menu item or detaches a submenu from the specified menu.
// If the menu item opens a drop-down menu or submenu, RemoveMenu does not destroy the menu
// or its handle, allowing the menu to be reused. Before this function is called, the GetSubMenu
// function should retrieve a handle to the drop-down menu or submenu.
//
//HB_FUNC( REMOVEMENU )
//{
//  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
//  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
//  UINT uFlags            = (UINT)  hb_parni(3);        // options
//
//  hb_retl( (BOOL) RemoveMenu( hMenu, uPosition, uFlags ) );
//}

VOID APIENTRY DisplayContextMenu(HWND hwnd, POINT pt, HMENU hmenu)
{
    //HMENU hmenu;            // top-level menu
    HMENU hmenuTrackPopup;  // shortcut menu

    // Load the menu resource.

    //if ((hmenu = LoadMenu(hinst, "ShortcutExample")) == NULL)
    //    return;

    // TrackPopupMenu cannot display the menu bar so get
    // a handle to the first shortcut menu.

    hmenuTrackPopup = hmenu; //GetSubMenu(hmenu, 0);

    // Display the shortcut menu. Track the right mouse
    // button.

    TrackPopupMenu(hmenuTrackPopup,
            TPM_LEFTALIGN | TPM_RIGHTBUTTON,
            pt.x, pt.y, 0, hwnd, NULL);

    // Destroy the menu.

    //DestroyMenu(hmenu);
}


BOOL WINAPI OnContextMenu(HWND hwnd, int x, int y, HMENU hmenu)
{
    RECT rc;                    // client area of window
    POINT pt;                   // location of mouse click

    pt.x = x;
    pt.y = y;
    // Get the bounding rectangle of the client area.

    GetClientRect(hwnd, &rc);

    // Convert the mouse position to client coordinates.

    ScreenToClient(hwnd, &pt);

    // If the position is in the client area, display a
    // shortcut menu.

    if (PtInRect(&rc, pt))
    {
        ClientToScreen(hwnd, &pt);
        DisplayContextMenu(hwnd, pt, hmenu);
        return TRUE;
    }

    // Return FALSE if no menu is displayed.

    return FALSE;
}


HB_FUNC( WG_ONCONTEXTMENU )
{
   HWND  hwnd  = (HWND)  hb_parnl( 1 );  // Window handle
   int   x     =         hb_parni( 2 );  // x position
   int   y     =         hb_parni( 3 );  // y position
   HMENU hmenu = (HMENU) hb_parnl( 4 );  // Menu handle

   hb_retl( (BOOL) OnContextMenu( hwnd, x, y, hmenu) );
}
