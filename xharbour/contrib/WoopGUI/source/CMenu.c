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

// The AppendMenu function appends a new item to the end of the specified menu bar, drop-down menu,
// submenu, or shortcut menu. You can use this function to specify the content, appearance, and
// behavior of the menu item.
//
HB_FUNC( APPENDMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uFlags            = (UINT)  hb_parni(2);        // options
  UINT_PTR uIDNewItem    = (UINT_PTR) hb_parni(3);     // identifier, menu, or submenu
  LPCTSTR lpNewItem      = (LPCTSTR)  hb_parc(4);      // menu item content

  hb_retl( (BOOL) AppendMenu( hMenu, uFlags, uIDNewItem, lpNewItem ) );
}

// The CheckMenuItem function sets the state of the specified menu item's check-mark attribute to
// either selected or clear.
//
HB_FUNC( CHECKMENUITEM )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT  uIDCheckItem     = (UINT)  hb_parni(2);        // menu item to check or uncheck
  UINT  uCheck           = (UINT)  hb_parni(3);        // menu item options

  hb_retnl( (DWORD) CheckMenuItem( hMenu, uIDCheckItem, uCheck ) );
}

// The CheckMenuRadioItem function checks a specified menu item and makes it a radio item.
// At the same time, the function clears all other menu items in the associated group and
// clears the radio-item type flag for those items.
//
HB_FUNC( CHECKMENURADIOITEM )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT  idFirst          = (UINT)  hb_parni(2);        // identifier or position of first item
  UINT  idLast           = (UINT)  hb_parni(3);        // identifier or position of last item
  UINT  idCheck          = (UINT)  hb_parni(4);        // identifier or position of menu item
  UINT  uFlags           = (UINT)  hb_parni(5);        // function options

  hb_retl( (BOOL) CheckMenuRadioItem( hMenu, idFirst, idLast, idCheck, uFlags ) );
}

// The CreateMenu function creates a menu. The menu is initially empty, but it can be filled with
// menu items by using the InsertMenuItem, AppendMenu, and InsertMenu functions.
//
HB_FUNC( CREATEMENU )
{
  hb_retnl( (long) CreateMenu() );
}

// The CreatePopupMenu function creates a drop-down menu, submenu, or shortcut menu.
// The menu is initially empty. You can use the InsertMenu function to insert menu items and
// the AppendMenu function to append menu items.
//
HB_FUNC( CREATEPOPUPMENU )
{
  hb_retnl( (long) CreatePopupMenu() );
}

// The DeleteMenu function deletes an item from the specified menu. If the menu item opens a menu
// or submenu, this function destroys the handle to the menu or submenu and frees the memory
// used by the menu or submenu.
//
HB_FUNC( DELETEMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
  UINT uFlags            = (UINT)  hb_parni(3);        // options

  hb_retl( (BOOL) DeleteMenu( hMenu, uPosition, uFlags ) );
}

//The DestroyMenu function destroys the specified menu and frees any memory that the menu occupies.
//
HB_FUNC( DESTROYMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu

  hb_retl( (BOOL) DestroyMenu( hMenu ) );
}

//The DrawMenuBar function redraws the menu bar of the specified window. If the menu bar changes
//after the system has created the window, this function must be called to draw the changed
//menu bar.
//
HB_FUNC( DRAWMENUBAR )
{
  HWND  hWnd             = (HWND)  hb_parnl(1);        // handle to window

  hb_retl( (BOOL) DrawMenuBar( hWnd ) );
}

// The EnableMenuItem function enables, disables, or grays the specified menu item
//
HB_FUNC( ENABLEMENUITEM )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT  uIDEnableItem    = (UINT)  hb_parni(2);        // menu item to update
  UINT  uEnable          = (UINT)  hb_parni(3);        // options

  hb_retl( (BOOL) EnableMenuItem ( hMenu, uIDEnableItem, uEnable ) );
}

/*  WINVER >= 500
// The EndMenu function ends the calling thread's active menu.
//
HB_FUNC( ENDMENU )
{
  hb_retl( (BOOL) EndMenu() );
}
*/


// The GetMenuState function return the state of the specified menu item
//
HB_FUNC( GETMENUSTATE )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT  uIDItem          = (UINT)  hb_parni(2);        // menu item to update
  UINT  uFlags           = (UINT)  hb_parni(3);        // options

  hb_retnl( (long) GetMenuState ( hMenu, uIDItem, uFlags ) );
}

HB_FUNC ( GETMENUITEMID )
{
   hb_retni( GetMenuItemID(
                            (HMENU) hb_parnl( 1 ),  // handle to menu
                            (int) hb_parni( 2 )     // position of menu item
                          ) ) ;
}

// The GetSystemMenu function allows the application to access the window menu (also known as
// the system menu or the control menu) for copying and modifying.
//
HB_FUNC( GETSYSTEMMENU )
{
  HWND  hWnd             = (HWND)  hb_parnl(1);        // handle to window
  BOOL  bRevert          = hb_parl(2);                 // reset option

  hb_retnl( (long) GetSystemMenu( hWnd, bRevert ) );
}

// The HiliteMenuItem function highlights or removes the highlighting from an item in a menu bar.
//
HB_FUNC( HILITEMENUITEM )
{
  HWND  hWnd             = (HWND)  hb_parnl(1);        // handle to window
  HMENU hMenu            = (HMENU) hb_parnl(2);        // handle to menu
  UINT  uItemHilite      = (UINT)  hb_parni(3);        // menu item
  UINT  uHilite          = (UINT)  hb_parni(4);        // highlight options

  hb_retl( (BOOL) HiliteMenuItem ( hWnd, hMenu, uItemHilite, uHilite ) );
}

// The InsertMenu function inserts a new menu item into a menu, moving other items down the menu.
//
HB_FUNC( INSERTMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
  UINT uFlags            = (UINT)  hb_parni(3);        // options
  UINT_PTR uIDNewItem    = (UINT_PTR) hb_parni(4);     // identifier, menu, or submenu
  LPCTSTR lpNewItem      = (LPCTSTR)  hb_parc(5);      // menu item content

  hb_retl( (BOOL) InsertMenu( hMenu, uPosition, uFlags, uIDNewItem, lpNewItem ) );
}

// The IsMenu function determines whether a handle is a menu handle.
//
HB_FUNC( ISMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu

  hb_retl( (BOOL) IsMenu( hMenu ) );
}

// The ModifyMenu function changes an existing menu item. This function is used to specify the
// content, appearance, and behavior of the menu item.
//
HB_FUNC( MODIFYMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
  UINT uFlags            = (UINT)  hb_parni(3);        // options
  UINT_PTR uIDNewItem    = (UINT_PTR) hb_parni(4);     // identifier, menu, or submenu
  LPCTSTR lpNewItem      = (LPCTSTR)  hb_parc(5);      // menu item content

  hb_retl( (BOOL) ModifyMenu( hMenu, uPosition, uFlags, uIDNewItem, lpNewItem ) );
}

// The RemoveMenu function deletes a menu item or detaches a submenu from the specified menu.
// If the menu item opens a drop-down menu or submenu, RemoveMenu does not destroy the menu
// or its handle, allowing the menu to be reused. Before this function is called, the GetSubMenu
// function should retrieve a handle to the drop-down menu or submenu.
//
HB_FUNC( REMOVEMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to menu
  UINT uPosition         = (UINT)  hb_parni(2);        // item that new item precedes
  UINT uFlags            = (UINT)  hb_parni(3);        // options

  hb_retl( (BOOL) RemoveMenu( hMenu, uPosition, uFlags ) );
}

// The SetMenu function assigns a new menu to the specified window.
//
HB_FUNC( SETMENU )
{
  HWND  hWnd             = (HWND)  hb_parnl(1);        // handle to window
  HMENU hMenu            = (HMENU) hb_parnl(2);        // handle to menu

  hb_retl( (BOOL) SetMenu( hWnd, hMenu ) );
}

// The TrackPopupMenu function displays a shortcut menu at the specified location and tracks
// the selection of items on the menu. The shortcut menu can appear anywhere on the screen
//
HB_FUNC( TRACKPOPUPMENU )
{
  HMENU hMenu            = (HMENU) hb_parnl(1);        // handle to shortcut menu
  UINT  uFlags           = (UINT)  hb_parni(2);        // options
  int   x                =         hb_parni(3);        // horizontal position
  int   y                =         hb_parni(4);        // vertical position
  int   nReserved        = 0;                          // reserved, must be zero
  HWND  hWnd             = (HWND)  hb_parnl(5);        // handle to owner window
  CONST RECT *prcRect;                                 // ignored

  hb_retl( (BOOL) TrackPopupMenu( hMenu, uFlags, x, y, nReserved, hWnd, NULL ) );
}

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