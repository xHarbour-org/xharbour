/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_button.c,v 1.2 2003/04/12 23:47:15 jonnymind Exp $

   GTK interface - management of checkbox widget
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void chkb_clicked( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

static void chkb_pressed( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_PRESSED, 0 );
}

static void chkb_released( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_RELEASED, 0 );
}

static void chkb_enter( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_ENTER, 0 );
}

static void chkb_leave( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_LEAVE, 0 );
}

/* Called when status is changed */
static void chkb_toggled( GtkWidget *widget,  gpointer cb_data )
{
   HB_ITEM lStatus;
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutL( &lStatus, gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( widget ) ) );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, &lStatus );
}

PXWT_WIDGET xwt_gtk_createCheckbox( PHB_ITEM pSelf )
{
   GtkWidget *checkbox;
   PXWT_WIDGET xwtData;

   checkbox = gtk_check_button_new ();
   // add a container to the window

   g_signal_connect (G_OBJECT(checkbox), "pressed", G_CALLBACK (chkb_pressed), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "released", G_CALLBACK (chkb_released), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "clicked", G_CALLBACK (chkb_clicked), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "enter", G_CALLBACK (chkb_enter), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "leave", G_CALLBACK (chkb_leave), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "toggled", G_CALLBACK (chkb_toggled ), pSelf->item.asArray.value );

   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_CHECKBOX;
   xwtData->widget_data = checkbox;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( checkbox );

   return xwtData;
}
