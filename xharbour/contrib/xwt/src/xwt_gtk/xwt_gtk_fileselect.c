/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Luiz Rafael Culik

   $Id: xwt_image.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   GTK interface - File Selection Box 
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>


static void file_ok_sel( GtkWidget *widget,  gpointer cb_data )
{
   PXWT_GTK_BASE xwtFilew = (PXWT_GTK_BASE) cb_data;
   HB_ITEM itmFileName;
   const char *fname;
   // this builds the Self object
   // If you use this macro, you must put it AFTER variable decl,
   // and BEFORE any other statement
   XWT_GTK_MAKESELF( (xwtFilew->owner) );

   fname = gtk_file_selection_get_filename (
      GTK_FILE_SELECTION ( xwtFilew->main_widget )
   );

   // itemPutC uses the char* parameter as it were const: it does not
   // mangles with that, it just creates a new local copy of the param.
   hb_itemPutC( &itmFileName, ( char *) fname );

   //rising the updated event, to signal that we have a candidate filename
   xwt_rise_event( &Self, XWT_E_UPDATED, 1, &itmFileName );
}

PXWT_WIDGET xwt_gtk_createFileSelection( PHB_ITEM pSelf )
{
   GtkWidget *filew;
   PXWT_GTK_BASE xwtFilew;
   PXWT_WIDGET xwtData;

   XWT_CREATE_WIDGET( xwtData );
   xwtFilew = (PXWT_GTK_BASE) hb_xgrab( sizeof( XWT_GTK_BASE ) );

   filew = gtk_file_selection_new("");
   gtk_widget_show( filew );

   xwtFilew->main_widget = filew;
   xwtFilew->owner = pSelf->item.asArray.value;

   // we need both the owner of the widget, and the widget itself;
   // so it is useful to pass the xwt_gtk data.
   g_signal_connect (
      G_OBJECT (GTK_FILE_SELECTION (filew)->ok_button),
      "clicked", G_CALLBACK (file_ok_sel), xwtFilew
   );

   /* Connect the cancel_button to destroy the widget */
   g_signal_connect_swapped (
      G_OBJECT (GTK_FILE_SELECTION (filew)->cancel_button),
      "clicked", G_CALLBACK (gtk_widget_destroy),
      G_OBJECT (filew)
   );

   xwtData->type = XWT_TYPE_FILESEL;
   // you ALWAYS need to set the xwtData->widget_data.
   // if no driver level widget wrapper is needed, you can
   // use the gtkWidget here, and set NULL for the destructor.
   xwtData->widget_data = xwtFilew;
   // main_widget is just allocated with hb_xgrab;
   // an xfree will be enough to get rid of it.
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_mainwidget_base;


   return xwtData;
}
