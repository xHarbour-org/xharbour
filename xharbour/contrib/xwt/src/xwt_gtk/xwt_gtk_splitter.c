/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_layout.c,v 1.4 2003/04/08 18:21:52 jonnymind Exp $

   Splitter window
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createSplitter( PHB_ITEM pSelf )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_WIDGET xwtData;
   PXWT_GTK_SPLITTER gtkSplitter;

   gtkSplitter = ( PXWT_GTK_SPLITTER ) hb_xgrab( sizeof( XWT_GTK_SPLITTER ) );
   gtkSplitter->owner = pSelf->item.asArray.value;

   gtkSplitter->main_widget = NULL;
   gtkSplitter->first_widget = NULL;
   gtkSplitter->second_widget = NULL;

   gtkSplitter->bShrink1 = TRUE;
   gtkSplitter->bShrink2 = TRUE;

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_SPLITTER;
   // no widget for now.
   xwtData->widget_data = (void *) gtkSplitter;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_mainwidget_base;

   return xwtData;
}

/* This function creates the real gtk widget. */
BOOL xwt_gtk_splitter_create_with_mode( PXWT_WIDGET wWidget, int mode  )
{
   PXWT_GTK_SPLITTER lay = (PXWT_GTK_SPLITTER ) wWidget->widget_data;

   if ( lay->main_widget != NULL )
   {
      return FALSE;
   }

   if ( mode == XWT_LM_HORIZ )
   {
      lay->main_widget = gtk_hpaned_new();
   }
   else
   {
      lay->main_widget = gtk_vpaned_new();
   }

   gtk_widget_show( lay->main_widget );
   return TRUE;
 }
