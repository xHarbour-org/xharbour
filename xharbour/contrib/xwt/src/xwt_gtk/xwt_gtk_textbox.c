/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_textbox.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Text box - basic input field
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void box_changed( GtkWidget *widget,  gpointer cb_data )
{
   PHB_ITEM pString = hb_itemNew( NULL );
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutC( pString, (char *)gtk_entry_get_text( GTK_ENTRY( widget ) ) );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, pString );
   hb_itemRelease( pString );
}

static void box_activate( GtkWidget *widget,  gpointer cb_data )
{
   PHB_ITEM pString = hb_itemNew( NULL );
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutC( pString, (char *)gtk_entry_get_text( GTK_ENTRY( widget ) ) );
   if ( ! xwt_rise_event( &Self, XWT_E_TEXT, 1, pString ) )
   {
      xwt_rise_event( &Self, XWT_E_UPDATED, 0 );
   }
   hb_itemRelease( pString );
}

BOOL xwt_gtk_createTextbox( PXWT_WIDGET xwtData )
{
   GtkWidget *box;
   box = gtk_entry_new();

   gtk_widget_show( box );

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)box;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   g_signal_connect (G_OBJECT(box), "activate", G_CALLBACK (box_activate), xwtData->owner );
   g_signal_connect (G_OBJECT(box), "changed", G_CALLBACK (box_changed), xwtData->owner );

   return TRUE;
}
