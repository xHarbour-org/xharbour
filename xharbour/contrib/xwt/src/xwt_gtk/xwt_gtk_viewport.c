/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_viewport.c,v 1.1 2003/04/12 23:47:15 jonnymind Exp $

   Viewport - an infinite scroller container & drawing area
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>


static void viewport_scrolled(GtkScrolledWindow *scrolledwindow,
   GtkScrollType arg1,
   gboolean arg2,
   gpointer cb_data)
{
   HB_ITEM itmX, itmY;

   XWT_GTK_MAKESELF( cb_data );
   //TODO: add some parameters
   hb_itemPutNI( &itmX, 0 );
   hb_itemPutNI( &itmY, 0 );

   xwt_rise_event( &Self, XWT_E_SCROLL, 2, &itmX, &itmY );
}

static void *viewport_get_topwidget( void *data )
{
   XWT_GTK_WND *widget = (XWT_GTK_WND *) data;
   return widget->window;
}

BOOL xwt_gtk_createViewPort( PXWT_WIDGET xwtData )
{
   PXWT_GTK_WND viewport;

   viewport = (PXWT_GTK_WND) hb_xgrab( sizeof( XWT_GTK_WND ) );
   viewport->main_widget = NULL;

   viewport->window = gtk_scrolled_window_new( NULL, NULL );
   gtk_scrolled_window_set_policy( GTK_SCROLLED_WINDOW( viewport->window ),
      GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );

   gtk_widget_show( viewport->window );

   g_signal_connect (G_OBJECT(viewport->window),
      "scroll-child", G_CALLBACK (viewport_scrolled),
      xwtData->owner );

   // no need for destructor, the data is just our widget for now

   xwtData->widget_data = (void *)viewport;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = viewport_get_topwidget;

   return TRUE;
}
