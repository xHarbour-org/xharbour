/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_splitter.c,v 1.2 2003/06/08 14:05:36 jonnymind Exp $

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
   #if __GNUC__ < 3
   gtkSplitter->a.main_widget = NULL;
   #else
   gtkSplitter->main_widget = NULL;
   #endif
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
   #if __GNUC__ <3
   if ( lay->a.main_widget != NULL )
   #else
   if ( lay->main_widget != NULL )
   #endif
   {
      return FALSE;
   }

   if ( mode == XWT_LM_HORIZ )
   {
   #if __GNUC__ < 3
      lay->a.main_widget = gtk_hpaned_new();
   #else
      lay->main_widget = gtk_hpaned_new();
   #endif
   }
   else
   {
   #if __GNUC__ <3
   lay->a.main_widget = gtk_vpaned_new();
   #else
     lay->main_widget = gtk_vpaned_new();
   #endif
   }
   #if __GNUC__ <3
   gtk_widget_show( lay->a.main_widget );
   #else
   gtk_widget_show( lay->main_widget );
   #endif
   return TRUE;
 }
