/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Rafa Carmona ( Thefull )

   $Id: xwt_gtk_togglebutton.c,v 1.1 2003/05/11 15:14:43 jonnymind Exp $

   GTK interface - management of toggle button widget
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

/* Called when status is changed */
static void btn_toggled( GtkWidget *widget,  gpointer cb_data )
{
   HB_ITEM lStatus;
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutL( &lStatus, gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( widget ) ) );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, &lStatus );
}


BOOL xwt_gtk_createToggleButton( PXWT_WIDGET xwtData )
{
   GtkWidget *togglebutton;

   togglebutton = gtk_toggle_button_new();
   // add a container to the window

   g_signal_connect (G_OBJECT(togglebutton), "pressed", G_CALLBACK (btn_pressed), xwtData->owner );
   g_signal_connect (G_OBJECT(togglebutton), "released", G_CALLBACK (btn_released), xwtData->owner );
   g_signal_connect (G_OBJECT(togglebutton), "clicked", G_CALLBACK (btn_clicked), xwtData->owner );
   g_signal_connect (G_OBJECT(togglebutton), "enter", G_CALLBACK (btn_enter), xwtData->owner );
   g_signal_connect (G_OBJECT(togglebutton), "leave", G_CALLBACK (btn_leave), xwtData->owner );
   g_signal_connect (G_OBJECT(togglebutton), "toggled", G_CALLBACK (btn_toggled ), xwtData->owner );

   xwtData->widget_data = togglebutton;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( togglebutton );

   return TRUE;
}
