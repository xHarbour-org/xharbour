/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_button.c,v 1.2 2003/04/12 23:47:15 jonnymind Exp $

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


BOOL xwt_gtk_createButton( PXWT_WIDGET xwtData )
{
   GtkWidget *button;

   button = gtk_button_new ();
   // add a container to the window

   g_signal_connect (G_OBJECT(button), "pressed", G_CALLBACK (btn_pressed), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "released", G_CALLBACK (btn_released), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (btn_clicked), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "enter", G_CALLBACK (btn_enter), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "leave", G_CALLBACK (btn_leave), xwtData->owner );

   xwtData->widget_data = button;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( button );

   return TRUE;
}
