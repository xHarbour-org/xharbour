/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_win_framewnd.c,v 1.2 2003/10/10 13:33:53 paultucker Exp $

   MS-Windows interface - Frame window
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_win.h>

HB_EXPORT extern HANDLE hb_hInstance;
HB_EXPORT extern HANDLE hb_hPrevInstance;
HB_EXPORT extern int    hb_iCmdShow;

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
         

BOOL xwt_win_createFrameWindow( PXWT_WIDGET xwtData )
{
   PXWT_WIN_DATA data;
   HWND hWnd;
   
   /* Create the window */
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
   
   /* BackReferences our XWT widget */
   SetWindowLong( hWnd, GWL_USERDATA, (LONG) xwtData );
      
   /* Forward reference the window into XWT_WIDGET */
   xwtData->widget_data = (void *) data;
   xwtData->destructor = xwt_win_free_wnd;
   xwtData->get_main_widget = xwt_win_get_topwidget_neuter;
   xwtData->get_top_widget = xwt_win_get_topwidget_neuter;

   return TRUE;
}

