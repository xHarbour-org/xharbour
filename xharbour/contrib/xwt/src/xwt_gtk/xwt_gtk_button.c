/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_button.c,v 1.4 2003/03/28 14:44:40 gian Exp $

   GTK interface - management of button widget
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void btn_clicked( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

static void btn_pressed( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_PRESSED, 0 );
}

static void btn_released( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_RELEASED, 0 );
}

static void btn_enter( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_ENTER, 0 );
}

static void btn_leave( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_LEAVE, 0 );
}


PXWT_WIDGET xwt_gtk_createButton( PHB_ITEM pSelf )
{
   GtkWidget *button;
   PXWT_WIDGET xwtData;

   button = gtk_button_new ();
   // add a container to the window

   g_signal_connect (G_OBJECT(button), "pressed", G_CALLBACK (btn_pressed), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "released", G_CALLBACK (btn_released), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (btn_clicked), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "enter", G_CALLBACK (btn_enter), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "leave", G_CALLBACK (btn_leave), pSelf->item.asArray.value );

   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_BUTTON;
   xwtData->widget_data = button;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return xwtData;
}
