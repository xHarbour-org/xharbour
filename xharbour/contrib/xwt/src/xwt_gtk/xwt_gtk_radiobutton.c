/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_radiobutton.c,v 1.2 2003/04/21 06:56:33 jonnymind Exp $

   GTK interface - management of radio button widget
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void rdbtn_clicked( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

static void rdbtn_pressed( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_PRESSED, 0 );
}

static void rdbtn_released( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_RELEASED, 0 );
}

static void rdbtn_enter( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_ENTER, 0 );
}

static void rdbtn_leave( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_LEAVE, 0 );
}

/* Called when status is changed */
static void rdbtn_toggled( GtkWidget *widget,  gpointer cb_data )
{
   HB_ITEM lStatus;
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutL( &lStatus, gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( widget ) ) );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, &lStatus );
}

BOOL xwt_gtk_createRadioButton( PXWT_WIDGET xwtData )
{
   GtkWidget *button;

   button = gtk_radio_button_new ( NULL );
   // add a container to the window

   g_signal_connect (G_OBJECT(button), "pressed", G_CALLBACK (rdbtn_pressed), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "released", G_CALLBACK (rdbtn_released), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (rdbtn_clicked), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "enter", G_CALLBACK (rdbtn_enter), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "leave", G_CALLBACK (rdbtn_leave), xwtData->owner );
   g_signal_connect (G_OBJECT(button), "toggled", G_CALLBACK (rdbtn_toggled), xwtData->owner );

   xwtData->widget_data = button;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( button );

   return TRUE;
}

