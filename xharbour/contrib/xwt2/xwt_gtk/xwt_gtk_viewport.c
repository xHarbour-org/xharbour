/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_viewport.c,v 1.4 2003/08/27 20:09:24 xthefull Exp $

   Viewport - an infinite scroller container & drawing area
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>


static void viewport_scrolled(GtkScrolledWindow *scrolledwindow,
   GtkScrollType arg1,
   gboolean arg2,
   gpointer cb_data)
{
   HB_ITEM hb_row, hb_col;
   GtkAdjustment *adjust;
   PXWT_WIDGET widget = (PXWT_WIDGET) cb_data;
   PXWT_GTK_WND viewport = (PXWT_GTK_WND) widget->widget_data;

   hb_row.type = HB_IT_NIL;
   hb_col.type = HB_IT_NIL;
   
   adjust = gtk_scrolled_window_get_vadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
   hb_itemPutNI( &hb_row, gtk_adjustment_get_value( adjust ) );
   adjust = gtk_scrolled_window_get_hadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
   hb_itemPutNI( &hb_col, gtk_adjustment_get_value( adjust ) );

   xwt_rise_event( widget->pOwner, "scrolled", 2, &hb_row, &hb_col );
}


static BOOL xwt_gtk_viewport_connect( PXWT_WIDGET wWindow, PXWT_WIDGET wChild )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) wWindow->widget_data;
   PXWT_GTK_BASE base = (PXWT_GTK_BASE) wChild->widget_data;
   GtkWidget *gtkChild = base->top_widget( wChild );
   
   // does the widget supports native scroll adjustment?
   /*if ( gtkChild->set_scroll_adjustments_signal != 0 )
   {
      // then just add it
      gtk_container_add( GTK_CONTAINER( wnd->INH(main_widget) ), gtkChild );
   }
   else
   {*/
      // else also add a scrollable viewport
      gtk_scrolled_window_add_with_viewport( GTK_SCROLLED_WINDOW( wnd->INH(main_widget) ), gtkChild );
   //}
   return TRUE;
}


static BOOL xwt_gtk_viewport_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_WND viewport = (PXWT_GTK_WND) widget->widget_data;
   GtkAdjustment *adjust;
      
   if ( strcmp( prop, "row-position" ) == 0 )
   {
      adjust = gtk_scrolled_window_get_vadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      gtk_adjustment_set_value( adjust, hb_itemGetNI( pValue ) );
   }
   else if ( strcmp( prop, "col-position" ) == 0 )
   {
      adjust = gtk_scrolled_window_get_hadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      gtk_adjustment_set_value( adjust, hb_itemGetNI( pValue ) );
   }
   else 
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );   
   }
   
   return ret;
}

static BOOL xwt_gtk_viewport_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_WND viewport = (PXWT_GTK_WND) widget->widget_data;
   GtkAdjustment *adjust;
      
   if ( strcmp( prop, "row-position" ) == 0 )
   {
      adjust = gtk_scrolled_window_get_vadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      hb_itemPutNI( pValue, gtk_adjustment_get_value( adjust ) );
   }
   else if ( strcmp( prop, "col-position" ) == 0 )
   {
      adjust = gtk_scrolled_window_get_hadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      hb_itemPutNI( pValue, gtk_adjustment_get_value( adjust ) );
   }
   else 
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );   
   }
   
   return ret;
}

   
static BOOL xwt_gtk_viewport_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   HB_ITEM hb_row, hb_col;
   GtkAdjustment *adjust;
   PXWT_GTK_WND viewport = (PXWT_GTK_WND) widget->widget_data;

   if ( xwt_gtk_base_getall( widget, pRet ) )
   {
      hb_row.type = HB_IT_NIL;
      hb_col.type = HB_IT_NIL;
      
      adjust = gtk_scrolled_window_get_vadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      hb_itemPutNI( &hb_row, gtk_adjustment_get_value( adjust ) );
      hb_hashAddChar( pRet, "row-position", &hb_row );
      adjust = gtk_scrolled_window_get_hadjustment( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ) );
      hb_itemPutNI( &hb_col, gtk_adjustment_get_value( adjust ) );
      hb_hashAddChar( pRet, "col-position", &hb_col )
      return TRUE;
    }
      
   return FALSE;
}


BOOL xwt_gtk_createViewPort( PXWT_WIDGET xwtData )
{
   PXWT_GTK_WND viewport;

   viewport = (PXWT_GTK_WND) hb_xgrab( sizeof( XWT_GTK_WND ) );
   viewport->INH(main_widget) = gtk_scrolled_window_new( NULL, NULL );
   g_object_ref( G_OBJECT(viewport->INH(main_widget)) );
   viewport->INH(nId) = 0;
   viewport->INH(top_widget) = xwt_gtk_base_topwidget;
   
   viewport->connect = xwt_gtk_viewport_connect;
   // disconnecting has nothing special: using the default window.
   viewport->disconnect = xwt_gtk_window_disconnect;
   

   gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( viewport->INH(main_widget) ),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );

   gtk_widget_show( viewport->INH(main_widget) );

   g_signal_connect (G_OBJECT(viewport->INH(main_widget)),
      "scroll-child", G_CALLBACK (viewport_scrolled),
      xwtData->pOwner );

   // no need for destructor, the data is just our widget for now

   xwtData->widget_data = (void *)viewport;
   xwtData->destroy = xwt_gtk_base_destroy;
   xwtData->set_property = xwt_gtk_viewport_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_viewport_getprop;
   xwtData->get_all_properties = xwt_gtk_viewport_getall;
   
   return TRUE;
}
