/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id$

   Splitter window
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

BOOL xwt_gtk_createSplitter( PXWT_WIDGET xwtData )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_GTK_SPLITTER gtkSplitter;

   gtkSplitter = ( PXWT_GTK_SPLITTER ) hb_xgrab( sizeof( XWT_GTK_SPLITTER ) );
   gtkSplitter->a.main_widget = NULL;
   gtkSplitter->first_widget = NULL;
   gtkSplitter->second_widget = NULL;

   gtkSplitter->bShrink1 = TRUE;
   gtkSplitter->bShrink2 = TRUE;

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *) gtkSplitter;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_mainwidget_base;

   return TRUE;
}

/* This function creates the real gtk widget. */
BOOL xwt_gtk_splitter_create_with_mode( PXWT_WIDGET wWidget, int mode  )
{
   PXWT_GTK_SPLITTER lay = (PXWT_GTK_SPLITTER ) wWidget->widget_data;
   if ( lay->a.main_widget != NULL )
   {
      return FALSE;
   }

   if ( mode == XWT_LM_HORIZ )
   {
      lay->a.main_widget = gtk_hpaned_new();
   }
   else
   {
     lay->a.main_widget = gtk_vpaned_new();
   }
   gtk_widget_show( lay->a.main_widget );
   return TRUE;
 }
