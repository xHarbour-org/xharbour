/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_framewnd.c,v 1.4 2003/06/08 14:05:35 jonnymind Exp $

   GTK interface - Frame window
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

static void *frame_get_mainwidget( void *data )
{
   PXWT_GTK_FRAMEWND wnd = (PXWT_GTK_FRAMEWND) data;
   #if __GNUC__<3
   return wnd->a.a.main_widget;
   #else
   return wnd->main_widget;
   #endif
}

static void *frame_get_topwidget( void *data )
{
   PXWT_GTK_FRAMEWND wnd = (PXWT_GTK_FRAMEWND) data;
   #if __GNUC__ <3
   return wnd->a.window;
   #else  
   return wnd->window;
   #endif
}

BOOL xwt_gtk_createFrameWindow( PXWT_WIDGET xwtData )
{
   PXWT_GTK_FRAMEWND frame = hb_xgrab( sizeof(XWT_GTK_FRAMEWND) );
   PHB_BASEARRAY pSelf;

   pSelf = xwtData->owner;
   #if __GNUC__ <3
   frame->a.window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   #else  
   frame->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   #endif

   frame->vbox = gtk_vbox_new(FALSE, 0);
   #if __GNUC__ <3
   gtk_container_add (GTK_CONTAINER (frame->a.window), frame->vbox);
   #else
   gtk_container_add (GTK_CONTAINER (frame->window), frame->vbox);
   #endif

   /* Now we create the menu bar */
   frame->menu_box = gtk_handle_box_new();
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->menu_box, FALSE, FALSE, 0 );
   gtk_widget_show (frame->menu_box);

   frame->menu_bar = gtk_menu_bar_new ();
   gtk_container_add( GTK_CONTAINER( frame->menu_box ), frame->menu_bar );
   gtk_widget_show( frame->menu_box );
   gtk_widget_show( frame->menu_bar );

   /* Create a void grid of 1,1 as the central widget, for now.
      Inside this central area, many widgets can then find place.*/
   #if __GNUC__ <3
   {
   frame->a.a.main_widget = gtk_table_new( 1, 1, TRUE );
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->a.a.main_widget, TRUE, TRUE, 0 );
   gtk_widget_show(frame->a.a.main_widget);
   }
   #else
   {
   frame->main_widget = gtk_table_new( 1, 1, TRUE );
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->main_widget, TRUE, TRUE, 0 );
   gtk_widget_show(frame->main_widget);
   }

   #endif
   /* Create the status bar. */
   frame->status_bar = gtk_statusbar_new();
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->status_bar, FALSE, FALSE, 0 );
   gtk_widget_show(frame->status_bar);

   gtk_widget_show (frame->vbox);

   /* The window destroy event is the only one that can be risen independently by the user,
   so it must be checked and passed to the internal destroy system.
   That system will eventually rise the destroy signal to propagate child auto-destruction.
   Unclean objects will be taken by the gc. */

   // We must send only the internal object pointer. pSelf will be destroyed with stack pop
   #if __GNUC__ < 3
   g_signal_connect(G_OBJECT(frame->a.window), "delete_event",
      G_CALLBACK (wnd_evt_destroy), pSelf );

   // A center position is a generally good default
   gtk_window_set_position( GTK_WINDOW( frame->a.window), GTK_WIN_POS_CENTER );
   #else
    g_signal_connect(G_OBJECT(frame->window), "delete_event",
      G_CALLBACK (wnd_evt_destroy), pSelf );

   // A center position is a generally good default
   gtk_window_set_position( GTK_WINDOW( frame->window), GTK_WIN_POS_CENTER );
   #endif
   

   xwtData->widget_data = (void *) frame;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = frame_get_mainwidget;
   xwtData->get_top_widget = frame_get_topwidget;

   // add a container to the window
   return TRUE;
}



void xwt_gtk_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray )
{
   PXWT_GTK_FRAMEWND frame;
   PHB_BASEARRAY pBaseArray = pMenuArray->item.asArray.value;
   ULONG ulPos;

   frame = (PXWT_GTK_FRAMEWND) xwtData->widget_data;
   // todo: cancelation of the old bar

   for ( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBaseArray->pItems + ulPos;

      hb_objSendMsg( pMenuItem, "ORAWWIDGET",0 );
      xwtData = (PXWT_WIDGET) HB_VM_STACK.Return.item.asPointer.value;
      gtk_menu_bar_append (GTK_MENU_BAR (frame->menu_bar),
         GTK_WIDGET( xwtData->get_top_widget( xwtData->widget_data ) ) );
   }
}

void xwt_gtk_resetMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray )
{
   PXWT_GTK_FRAMEWND frame;
   PHB_BASEARRAY pBaseArray = pMenuArray->item.asArray.value;
   ULONG ulPos;

   frame = (PXWT_GTK_FRAMEWND) xwtData->widget_data;
   // todo: cancelation of the old bar

   for ( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBaseArray->pItems + ulPos;
      hb_objSendMsg( pMenuItem, "ORAWWIDGET",0 );
      xwtData = (PXWT_WIDGET) HB_VM_STACK.Return.item.asPointer.value;
      gtk_container_remove (GTK_CONTAINER (frame->menu_bar), 
         GTK_WIDGET( xwtData->get_top_widget( xwtData->widget_data ) ) );        
   }
}
