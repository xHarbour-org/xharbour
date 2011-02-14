/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2004 Lorenzo Fiorini

   $Id$

   Text box - basic input field
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

BOOL xwt_gtk_createEditBox( PXWT_WIDGET xwtData )
{
   GtkWidget     *view;
   GtkWidget *sw;
   GtkTextBuffer *buffer;
   
   view = gtk_text_view_new();

   sw = gtk_scrolled_window_new (NULL, NULL);
   gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				      GTK_POLICY_AUTOMATIC,
				      GTK_POLICY_AUTOMATIC);
   gtk_container_add (GTK_CONTAINER (sw), view);
  
   buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (view));
   
   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)sw;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   gtk_widget_show( view );
   gtk_widget_show( sw );

   return TRUE;
}
