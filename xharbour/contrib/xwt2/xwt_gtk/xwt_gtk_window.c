/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_window.c,v 1.4 2003/08/27 20:09:24 xthefull Exp $

   GTK interface - Window widget specifics
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static gboolean wnd_evt_destroy( GtkWidget *widget,  GdkEvent  *event, gpointer cb_data )
{
   PHB_ITEM pSelf = (PHB_ITEM) cb_data;
   
   return xwt_rise_event( pSelf, "destroy", 0 );
}


GtkWidget *xwt_gtk_window_topwidget( PXWT_WIDGET widget )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) widget->widget_data;
   return wnd->window;
}

BOOL xwt_gtk_window_destroy( PXWT_WIDGET widget )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) widget->widget_data;
   // eventually disconnect children, they will be destroyed by GC
   wnd->disconnect( widget );
   // Main widget (and ancillary widgets as status bar and menu bar) 
   // here is always related to the window and is not referenced,
   // this meas that is destroyed when the window is destroyed
   gtk_widget_destroy( wnd->window );
   wnd->window = NULL;
   return TRUE;
}

BOOL xwt_gtk_window_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{  
   BOOL ret = TRUE;
   PXWT_GTK_WND wFrame = (PXWT_GTK_WND) widget->widget_data;
   GtkWidget *wTop = wFrame->window;
   GtkWidget *wParent = gtk_widget_get_parent( wTop );
   int xpos, ypos, width, height;
         
   // todo: call parent class (window) setprop
   
   // if parent != 0 is considered as a standard widget.
   if ( wParent == 0 && strcmp( "x", prop ) == 0 ) 
   {
      gtk_window_get_position( GTK_WINDOW(wTop), &xpos, &ypos );
      gtk_window_move( GTK_WINDOW(wTop), hb_itemGetNI( pValue ), ypos );
   }
   else if ( wParent == 0 && strcmp( "y", prop ) == 0 ) 
   {
      gtk_window_get_position( GTK_WINDOW(wTop), &xpos, &ypos );
      gtk_window_move( GTK_WINDOW(wTop), xpos, hb_itemGetNI( pValue ) );
   }
   else if ( strcmp( "width", prop ) == 0 ) 
   {
      gtk_window_get_size( GTK_WINDOW(wTop), &width, &height );
      if ( height < 0 )
      {
         height = 1;
      }
      gtk_window_resize( GTK_WINDOW(wTop), hb_itemGetNI( pValue ), height );
   }
   else if ( strcmp( "height", prop ) == 0 ) 
   {
      gtk_window_get_size( GTK_WINDOW(wTop), &width, &height );
      if ( width < 0 )
      {
         width = 1;
      }
      gtk_window_resize( GTK_WINDOW(wTop), width, hb_itemGetNI( pValue ) );
   }
   else if ( strcmp( prop, "text" ) == 0 )
   {
      gtk_window_set_title (GTK_WINDOW(wTop), hb_itemGetCPtr( pValue ) );
   }
   else 
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }
   
   return ret;
}


BOOL xwt_gtk_window_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{   
   BOOL ret = TRUE;
   PXWT_GTK_WND wFrame = (PXWT_GTK_WND) widget->widget_data;
   GtkWidget *wTop = wFrame->window;
   GtkWidget *wParent = gtk_widget_get_parent( wTop );
   int xpos, ypos, width, height;
   
   if ( wParent == 0 && strcmp( prop, "x" ) == 0 )
   {
      gtk_window_get_position( GTK_WINDOW(wTop), &xpos, &ypos );
      hb_itemPutNI( pValue, xpos );
   }
   else if ( wParent == 0 && strcmp( prop, "y" ) == 0 )
   {
      gtk_window_get_position( GTK_WINDOW(wTop), &xpos, &ypos );
      hb_itemPutNI( pValue, ypos );
   }
   else if ( strcmp( prop, "width" ) == 0 )
   {
      gtk_window_get_size( GTK_WINDOW(wTop), &width, &height );
      hb_itemPutNI( pValue, width );      
   }
   else if ( strcmp( prop, "height" ) == 0 )
   {
      gtk_window_get_size( GTK_WINDOW(wTop), &width, &height );
      hb_itemPutNI( pValue, height ); 
   }
   else if ( strcmp( prop, "text" ) == 0 )
   {
      char *cTitle = (char *) gtk_window_get_title ( GTK_WINDOW(wTop) );
      hb_itemPutC( pValue, cTitle );
   }
   else {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }
   
   return ret;
}


BOOL xwt_gtk_window_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   HB_ITEM hbValue;
   GtkWidget *wTop = ( (PXWT_GTK_WND)widget->widget_data)->window;
   int xpos, ypos, width, height;
   
   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_base_getall( widget, pRet ) )
   {
      gtk_window_get_position( GTK_WINDOW(wTop), &xpos, &ypos );
      hb_hashAddChar( pRet, "x", hb_itemPutNI( &hbValue, xpos ) );
      hb_hashAddChar( pRet, "y", hb_itemPutNI( &hbValue, ypos ) );
      gtk_window_get_size( GTK_WINDOW(wTop), &width, &height );
      hb_hashAddChar( pRet, "width", hb_itemPutNI( &hbValue, width ) );
      hb_hashAddChar( pRet, "height", hb_itemPutNI( &hbValue, height ) );
      hb_itemPutC( &hbValue, (char *)gtk_window_get_title ( GTK_WINDOW(wTop) ) );
      hb_hashAddChar( pRet, "text", &hbValue );
      
      hb_itemClear( &hbValue );
   
      return TRUE;
   }
   
   return FALSE;
}


BOOL xwt_gtk_createWindow( PXWT_WIDGET xwtData )
{
   XWT_GTK_WND *wnd = (XWT_GTK_WND *) hb_xgrab( sizeof( XWT_GTK_WND ) );

   wnd->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   g_object_ref( G_OBJECT(wnd->window));
   wnd->INH( nId ) = 0;
   wnd->INH(main_widget) = gtk_table_new( 1, 1, TRUE );
   wnd->INH(top_widget) = xwt_gtk_window_topwidget;
   gtk_container_add( GTK_CONTAINER(wnd->window), wnd->INH(main_widget) );
   gtk_widget_show (wnd->INH(main_widget));
   wnd->connect = xwt_gtk_window_connect;
   wnd->disconnect = xwt_gtk_window_disconnect;

   /* The window destroy event is the only one that can be risen independently by the user,
   so it must be checked and passed to the internal destroy system.
   That system will eventually rise the destroy signal to propagate child auto-destruction.
   Unclean objects will be taken by the gc. */

   g_signal_connect (G_OBJECT(wnd->window), "delete_event", G_CALLBACK (wnd_evt_destroy),
      xwtData->pOwner );

   // A center position is a generally good default
   gtk_window_set_position( GTK_WINDOW(wnd->window), GTK_WIN_POS_CENTER );

   xwtData->widget_data = (void *)wnd;
   xwtData->destroy = xwt_gtk_window_destroy;
   
   // property handlers
   xwtData->set_property = xwt_gtk_window_setprop;
   xwtData->get_property = xwt_gtk_window_getprop;
   xwtData->get_all_properties = xwt_gtk_window_getall;
   
   // nothing more than the base one
   xwtData->set_pgroup = xwt_gtk_setpgroup;

   xwt_gtk_base_signal_connect( xwtData );

   return TRUE;
}

BOOL xwt_gtk_window_connect( PXWT_WIDGET wWindow, PXWT_WIDGET wChild )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) wWindow->widget_data;
   PXWT_GTK_BASE base = (PXWT_GTK_BASE) wChild->widget_data;
   GtkWidget *gtkChild = base->top_widget( wChild );
   
   gtk_table_attach_defaults( GTK_TABLE( wnd->INH(main_widget) ), gtkChild, 0,1,0,1 );
   return TRUE;
}

BOOL xwt_gtk_window_disconnect( PXWT_WIDGET wWindow )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) wWindow->widget_data;
   GList *child = gtk_container_get_children( GTK_CONTAINER( wnd->INH(main_widget) ) );
   
   if( child != NULL && child->data != NULL )
   {
      // reference it so that it can live on its own
      gtk_container_remove(GTK_CONTAINER( wnd->INH(main_widget) ), GTK_WIDGET( child->data ));
      return TRUE;
   }
   return FALSE;
}
