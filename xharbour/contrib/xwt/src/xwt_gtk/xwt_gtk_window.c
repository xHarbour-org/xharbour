/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_window.c,v 1.4 2003/03/28 14:44:40 gian Exp $

   GTK interface - Window widget specifics
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static gboolean wnd_evt_destroy( GtkWidget *widget,  GdkEvent  *event, gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );

   if ( ! xwt_rise_event( &Self, XWT_E_DESTROY_REQ, 0 ) )
   {
      // This will 1: rise destroyed event, 2: call widget destructor, 
      //   3: rise destruction signals/events in all the childs
      hb_objSendMsg( &Self, "DESTROY", 0 );
   }
   // event managed
   return TRUE;
}

static void *wnd_get_mainwidget( void *data )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) data;
   return wnd->main_widget;
}

static void *wnd_get_topwidget( void *data )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) data;
   return wnd->window;
}

PXWT_WIDGET xwt_gtk_createWindow( PHB_ITEM pSelf )
{
   PXWT_WIDGET xwtData;
   XWT_GTK_WND *wnd = (XWT_GTK_WND *) hb_xgrab( sizeof( XWT_GTK_WND ) );

   wnd->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

   wnd->main_widget = gtk_fixed_new();
   gtk_container_add (GTK_CONTAINER (wnd->window), wnd->main_widget);
   gtk_widget_show (wnd->main_widget);

   /* The window destroy event is the only one that can be risen independently by the user,
   so it must be checked and passed to the internal destroy system.
   That system will eventually rise the destroy signal to propagate child auto-destruction.
   Unclean objects will be taken by the gc. */

   // We must send only the internal object pointer. pSelf will be destroyed with stack pop
   wnd->owner = pSelf->item.asArray.value;

   g_signal_connect (G_OBJECT(wnd->window), "delete_event", G_CALLBACK (wnd_evt_destroy),
      wnd->owner );

   // A center position is a generally good default
   gtk_window_set_position( GTK_WINDOW( wnd->window), GTK_WIN_POS_CENTER );

   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_WINDOW;
   xwtData->widget_data = (void *)wnd;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = wnd_get_mainwidget;
   xwtData->get_top_widget = wnd_get_topwidget;

   return xwtData;
}
