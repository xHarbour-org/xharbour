
/*
   XWT_WIN - xHarbour Windowing Toolkit/ MS-Windows interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.c,v 1.23 2003/09/04 20:26:24 xthefull Exp $

   Global declarations, common functions

*/

#include <hbapi.h>
#include <xwt_win.h>
#include <xwt_api.h>

HB_EXPORT extern HANDLE hb_hInstance;
HB_EXPORT extern HANDLE hb_hPrevInstance;
HB_EXPORT extern int    hb_iCmdShow;


BOOL xwt_drv_set_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   HWND hWnd = ( HWND ) wWidget->get_top_widget( wWidget );

   switch( prop->type ) {
      case XWT_PROP_TEXT:
         // todo: check widget
         SetWindowText( hWnd, prop->value.text );
      return TRUE;
      
      case XWT_PROP_SIZE:
         // todo: check widget
         SetWindowPos( 
            hWnd,
            NULL,  
            0, 0,
            prop->value.size.width, prop->value.size.height,
            SWP_NOMOVE | SWP_NOZORDER 
         );
      return TRUE;
      
      case XWT_PROP_POSITION:
         // todo: check widget
         SetWindowPos( 
            hWnd, 
            NULL,
            prop->value.position.x, prop->value.position.y,
            0, 0,
            SWP_NOSIZE | SWP_NOZORDER 
         );
      return TRUE; 
      
      case XWT_PROP_VISIBILITY:
         // common for all widgets
         switch( prop->value.number )
         {
            case XWT_VIS_HIDDEN:
               ShowWindow( hWnd, SW_HIDE );
            return TRUE;

            case XWT_VIS_NORMAL:
               ShowWindow( hWnd, SW_SHOW );
            return TRUE;
         }

         // Only for window widgets
         if (
            wWidget->type != XWT_TYPE_FRAME ||
            wWidget->type != XWT_TYPE_WINDOW
         )
         {
            return FALSE;
         }
         switch( prop->value.number )
         {
            case XWT_VIS_MAXIMIZED_H:
            case XWT_VIS_MAXIMIZED_V:
            case XWT_VIS_MAXIMIZED:
               ShowWindow( hWnd, SW_MAXIMIZE );
            return TRUE;
            
            case XWT_VIS_MINIMIZED:
               ShowWindow( hWnd, SW_MINIMIZE );
            return TRUE;
         }

      return FALSE;

      
   }
   
   return FALSE;
}


BOOL xwt_drv_get_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   HWND hWnd = ( HWND ) wWidget->get_top_widget( wWidget );

   switch( prop->type ) {
      case XWT_PROP_TEXT:
         // todo: check widget
         prop->value.string.iLength = GetWindowTextLength( hWnd );
         if ( prop->value.string.iLength > 0 )  {
            prop->value.string.text = (char *) hb_xgrab( prop->value.string.iLength+1 );
            GetWindowText( hWnd, prop->value.string.text, prop->value.string.iLength );
            return TRUE;
         }
      return FALSE;
      
      
      case XWT_PROP_SIZE:
      {
         RECT rc;
         // todo: check widget
         if ( ! GetWindowRect( hWnd, &rc) )
            return FALSE;
         prop->value.size.width = rc.right - rc.left;
         prop->value.size.height = rc.bottom - rc.top;
      }
      return TRUE;
      
      case XWT_PROP_POSITION:
      {
         RECT rc;
         // todo: check widget
         if ( ! GetWindowRect( hWnd, &rc) )
            return FALSE;
         prop->value.position.x = rc.left;
         prop->value.position.y = rc.top;
      }
      return TRUE; 
      
   }
   
   return FALSE;
}


BOOL xwt_drv_create( PXWT_WIDGET xwtData )
{
   switch( xwtData->type )
   {
      //case XWT_TYPE_WINDOW:  return xwt_win_createWindow( xwtData );
      case XWT_TYPE_FRAME:   return xwt_win_createFrameWindow( xwtData );
   }
   return FALSE;

}

BOOL xwt_drv_destroy( PXWT_WIDGET wWidget )
{
   HWND wSelf = (HWND) wWidget->get_top_widget( wWidget->widget_data );
   SetWindowLong( wSelf, GWL_USERDATA, 0 );
   DestroyWindow( wSelf );
   if( wWidget->destructor != NULL )
   {
      wWidget->destructor( wWidget->widget_data );
   }
   hb_xfree( wWidget );
   return TRUE;
}

/******************************************************
*  Modal procedures
*/

void xwt_drv_modal( PXWT_WIDGET widget )
{

}

/******************************************************
*  Container Functions
*/

BOOL xwt_drv_add( PXWT_WIDGET wWSelf, PXWT_WIDGET wWChild )
{
  
   return FALSE;
}

BOOL xwt_drv_remove( PXWT_WIDGET wWSelf, PXWT_WIDGET wWChild )
{

   return FALSE;
}


/***************************************/
/* Procedural functions
*/

BOOL xwt_drv_init( int argc, char **argv )
{ 
   WNDCLASS wc;
   
   if (!hb_hPrevInstance) 
    { 
        wc.style = 0; 
        wc.lpfnWndProc = (WNDPROC) xwt_gtk_framewndproc; 
        wc.cbClsExtra = 0; 
        wc.cbWndExtra = 0; 
        wc.hInstance = hb_hInstance; 
        /* TODO: manage XWT application icons */
        wc.hIcon = LoadIcon((HINSTANCE) NULL, IDI_APPLICATION); 
        
        wc.hCursor = LoadCursor((HINSTANCE) NULL, IDC_ARROW); 
        wc.hbrBackground = GetStockObject(WHITE_BRUSH); 
        wc.lpszMenuName =  NULL; 
        wc.lpszClassName = XWT_WIN_FRMCLSNAME; 
 
        if (!RegisterClass(&wc)) 
            return FALSE; 
    } 

   return TRUE;
}

BOOL xwt_drv_process_events()
{
   MSG msg;
   BYTE bRet;
   
   while( (bRet = GetMessage( &msg, NULL, 0, 0 )) != 0 )
   {
      if ( bRet > 0 ) 
      {
         TranslateMessage(&msg); 
         DispatchMessage(&msg); 
      }
      //hb_gcAll( FALSE );
   } 

   return TRUE;
}

BOOL xwt_drv_quit()
{  
   PostQuitMessage( 0 );
   //UnregisterClass( "XWTWIN_Framewnd", hb_hInstance);
   return TRUE;
}


/*************************************************/
   
void *xwt_win_get_topwidget_neuter( void *data )
{
   return ((PXWT_WIDGET) data)->widget_data;
}
