/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_pane.c,v 1.5 2003/06/08 14:05:36 jonnymind Exp $

   Pane - basic container with no layout capability
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>



BOOL xwt_gtk_createPane( PXWT_WIDGET xwtData )
{
   PXWT_GTK_CONTAINER pane;

   pane = (PXWT_GTK_CONTAINER) hb_xgrab( sizeof( XWT_GTK_CONTAINER ) );
   #if __GNUC__ <3
   pane->a.a.main_widget = gtk_fixed_new();
   #else
   pane->main_widget = gtk_fixed_new();
   #endif
   #if __GNUC__ <3
   pane->frame = NULL;
   pane->a.align = NULL; // no frame for now
   pane->a.iHAlign = XWT_ALIGN_CENTER; // no frame for now
   pane->a.iVAlign = XWT_ALIGN_TOP; // no frame for now
   #else
   pane->frame = NULL;
   pane->align = NULL; // no frame for now
   pane->iHAlign = XWT_ALIGN_CENTER; // no frame for now
   pane->iVAlign = XWT_ALIGN_TOP; // no frame for now

   #endif

   // add a container to the window
   #if __GNUC__ <3
   gtk_widget_show( pane->a.a.main_widget );
   #else
   gtk_widget_show( pane->main_widget );
   #endif

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return TRUE;
}

