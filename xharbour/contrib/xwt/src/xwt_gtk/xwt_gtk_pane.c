/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id$

   Pane - basic container with no layout capability
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>



BOOL xwt_gtk_createPane( PXWT_WIDGET xwtData )
{
   PXWT_GTK_CONTAINER pane;

   pane = (PXWT_GTK_CONTAINER) hb_xgrab( sizeof( XWT_GTK_CONTAINER ) );
   pane->a.a.main_widget = gtk_fixed_new();
   pane->frame = NULL;
   pane->a.align = NULL; // no frame for now
   pane->a.iHAlign = XWT_ALIGN_CENTER; // no frame for now
   pane->a.iVAlign = XWT_ALIGN_TOP; // no frame for now

   // add a container to the window
   gtk_widget_show( pane->a.a.main_widget );

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return TRUE;
}

