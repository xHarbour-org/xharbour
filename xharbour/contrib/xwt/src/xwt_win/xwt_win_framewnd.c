/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_win_framewnd.c,v 1.3 2003/10/13 11:54:08 jonnymind Exp $

   MS-Windows interface - Frame window
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_win.h>


/* Destroy given frame window */
         
void xwt_win_free_wnd( void *wnd )
{
   PXWT_WIN_DATA self = (PXWT_WIN_DATA) wnd;
   CloseHandle( self->hMain );
   
   if ( self->hMainWidget != NULL ) 
   {
      DestroyWindow( self->hMainWidget );
      self->hMainWidget = NULL;
   }
   
   if ( self->hStatusBar != NULL ) 
   {
      DestroyWindow( self->hStatusBar );
      self->hStatusBar = NULL;
   }

   if ( self->pMenu != NULL ) 
   {
      PHB_BASEARRAY pBar = self->pMenu;
      ULONG ulPos;
      
      /*
      for ( ulPos = 0; ulPos < pBar->ulLen; ulPos++ )
      {
         PHB_ITEM pMenuItem = pBar->pItems + ulPos;
         hb_objSendMsg( pMenuItem, "DESTROY",0 );
         
      }*/

      hb_gcUnlock( self->pMenu );
      self->pMenu = NULL;
   }
   
   hb_xfree( self );
}

static HMENU xwt_win_createMenuFromArray( PHB_ITEM pMenuArray )
{
   MENUITEMINFO miInfo;
   MENUINFO mInfo;
   PHB_BASEARRAY pBar = pMenuArray->item.asArray.value;
   ULONG ulPos;
   HMENU hMenu;
   
   hMenu= CreateMenu();

   // manage menu-level style
   mInfo.cbSize = sizeof( MENUINFO );
   mInfo.fMask = MIM_STYLE;
   mInfo.dwStyle = MNS_CHECKORBMP | MNS_MODELESS;
   mInfo.cyMax = 0;
   // TODO: manage context help
   mInfo.dwContextHelpID = 0;
   SetMenuInfo( hMenu, &mInfo );

   /* Generic menuitem settings */
   miInfo.cbSize = sizeof(MENUITEMINFO);
  
   for ( ulPos = 0; ulPos < pBar->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBar->pItems + ulPos;
      PXWT_WIN_MENUDATA menuData;
      PXWT_WIDGET widget;

      hb_objSendMsg( pMenuItem, "ORAWWIDGET",0 );
      widget = (PXWT_WIDGET) HB_VM_STACK.Return.item.asPointer.value;
      menuData = widget->widget_data;
      
      miInfo.fMask = MIIM_DATA | MIIM_TYPE ;
      miInfo.fType = MFT_STRING;
      
      //gets the string;
      miInfo.cch = strlen( menuData->szLabel );
      miInfo.dwTypeData = menuData->szLabel;
      
      // set backreference to this menu item
      miInfo.dwItemData = (ULONG_PTR) widget;
      miInfo.hbmpItem = NULL;
      
      // is it a menu item?
      hb_objSendMsg( pMenuItem, "GETTYPE",0 );
      if (  hb_itemGetNI( &HB_VM_STACK.Return ) == XWT_TYPE_MENU )
      {
         miInfo.fMask |= MIIM_SUBMENU;
         hb_objSendMsg( pMenuItem, "ACHILDREN",0 );
         miInfo.hSubMenu = xwt_win_createMenuFromArray( &HB_VM_STACK.Return );
      }
      else {
         //gets the ID
         miInfo.fMask |= MIIM_ID;
         hb_objSendMsg( pMenuItem, "NID",0 );
         miInfo.wID = hb_itemGetNI( &HB_VM_STACK.Return );
      }
      
      InsertMenuItem( hMenu, ulPos, TRUE, &miInfo );
   }   
   
   return hMenu;
}



void xwt_win_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray )
{
   PXWT_WIN_DATA wnd = (PXWT_WIN_DATA) xwtData->widget_data;
   HMENU hMenu, hOldMenu;
   PXWT_WIN_DATA frame;

   /* The menu array must survive function boundaries */      
   hb_gcLock( pMenuArray );

   /* we must compose the menu by scanning the PHB_ITEM widget hyerarcy */
   frame = (PXWT_WIN_DATA) xwtData->widget_data;
   
   hMenu = xwt_win_createMenuFromArray( pMenuArray );
   
   hOldMenu = GetMenu( frame->hMain );
   SetMenu( frame->hMain, hMenu );
   if ( hOldMenu != NULL && hOldMenu != NULL)
   {
      DestroyMenu( hOldMenu );
   }
   frame->pMenu = pMenuArray->item.asArray.value;   
   
   DrawMenuBar( wnd->hMain );
}

void xwt_win_resetMenuBar( PXWT_WIDGET xwtData )
{
   PXWT_WIN_DATA self = (PXWT_WIN_DATA) xwtData->widget_data;
   
   if ( self->pMenu != NULL ) 
   {
      PHB_BASEARRAY pBar = self->pMenu;
      ULONG ulPos;
      
      for ( ulPos = 0; ulPos < pBar->ulLen; ulPos++ )
      {
         PHB_ITEM pMenuItem = pBar->pItems + ulPos;
         hb_objSendMsg( pMenuItem, "DESTROY",0 );
      }

      hb_gcUnlock( self->pMenu );
      self->pMenu = NULL;
   }
}

static PHB_ITEM xwt_win_findMenuItem( PHB_BASEARRAY pBar, UINT uiId )
{
   PHB_ITEM pRet;
   ULONG ulPos;
   
   for ( ulPos = 0; ulPos < pBar->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBar->pItems + ulPos;
      
      hb_objSendMsg( pMenuItem, "GETTYPE",0 );
      if (  hb_itemGetNI( &HB_VM_STACK.Return ) == XWT_TYPE_MENUITEM )
      {   
         hb_objSendMsg( pMenuItem, "NID",0 );
         if( uiId == (unsigned)  hb_itemGetNI( &HB_VM_STACK.Return ) )
         {
            return pMenuItem;
         }
      }
      else 
      {
         hb_objSendMsg( pMenuItem, "ACHILDREN",0 );
         pRet = xwt_win_findMenuItem( HB_VM_STACK.Return.item.asArray.value, uiId );
         if ( pRet != NULL )
         {
            return pRet;
         }
      }
   }
   
   return NULL;
}
  


LRESULT CALLBACK xwt_gtk_framewndproc(
    HWND hwnd,
    UINT uMsg,
    WPARAM wParam,
    LPARAM lParam
)
{ 
   XWT_WIN_MAKESELF( hwnd );
   
   switch( uMsg ) {
      case WM_CREATE: 
      break;
         
      case WM_COMMAND:
         if ( HIWORD( wParam ) == 0 )
         {
            PHB_BASEARRAY pMenu = (( PXWT_WIN_DATA) _wSelf->widget_data )->pMenu;
            PHB_ITEM pMenuItem;
            
            pMenuItem = xwt_win_findMenuItem( pMenu, LOWORD( wParam ) );

            if ( pMenuItem != NULL )
            {            
               xwt_rise_event( pMenuItem, XWT_E_CLICKED, 0 ); 
            }
         }
      return 0;
      
      case WM_CLOSE:
         if ( ! xwt_rise_event( &Self, XWT_E_DESTROY_REQ, 0 ) )
         {
            // This will 1: rise destroyed event, 2: call widget destructor, 
            //   3: rise destruction signals/events in all the childs
            hb_objSendMsg( &Self, "DESTROY", 0 );
         }
      return 0;
   
      //todo: rise XWT events
   }
   
   // event managed
   return DefWindowProc( hwnd, uMsg, wParam, lParam );
}      

/* Create the window */
   
BOOL xwt_win_createFrameWindow( PXWT_WIDGET xwtData )
{
   PXWT_WIN_DATA data;
   HWND hWnd;
   
   hWnd =  CreateWindow( 
    XWT_WIN_FRMCLSNAME,
    "", // no name now
    WS_SYSMENU | WS_OVERLAPPEDWINDOW ,  // not visible now
    0,  // no position now
    0,
    0,
    0,
    NULL,   // no parent
    NULL, // no menu
    (HINSTANCE) hb_hInstance,
    NULL // no win parameter
   );

   if ( hWnd == NULL ) 
      return FALSE;

   data = (PXWT_WIN_DATA) hb_xgrab( sizeof( XWT_WIN_DATA ) );
   data->xwt_widget = xwtData;
   data->hMain = hWnd;
   data->pMenu = NULL;
   data->hMainWidget = NULL;
   data->hStatusBar = NULL;
   
   /* BackReferences our XWT widget */
   SetWindowLong( hWnd, GWL_USERDATA, (LONG) xwtData );
      
   /* Forward reference the window into XWT_WIDGET */
   xwtData->widget_data = (void *) data;
   xwtData->destructor = xwt_win_free_wnd;
   xwtData->get_main_widget = xwt_win_get_topwidget_neuter;
   xwtData->get_top_widget = xwt_win_get_topwidget_neuter;

   return TRUE;
}

