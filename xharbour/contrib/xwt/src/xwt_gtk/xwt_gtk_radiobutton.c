/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_radiobutton.c,v 1.1 2003/04/12 23:47:15 jonnymind Exp $

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

PXWT_WIDGET xwt_gtk_createRadioButton( PHB_ITEM pSelf )
{
   GtkWidget *button;
   PXWT_WIDGET xwtData;

   button = gtk_radio_button_new ( NULL );
   // add a container to the window

   g_signal_connect (G_OBJECT(button), "pressed", G_CALLBACK (rdbtn_pressed), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "released", G_CALLBACK (rdbtn_released), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (rdbtn_clicked), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "enter", G_CALLBACK (rdbtn_enter), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "leave", G_CALLBACK (rdbtn_leave), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(button), "toggled", G_CALLBACK (rdbtn_toggled), pSelf->item.asArray.value );

   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_RADIOBUTTON;
   xwtData->widget_data = button;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( button );

   return xwtData;
}

