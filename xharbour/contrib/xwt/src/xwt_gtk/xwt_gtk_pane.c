/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: XWT_GTK_CONTAINER.c,v 1.2 2003/04/07 15:41:08 jonnymind Exp $

   Pane - basic container with no layout capability
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>



PXWT_WIDGET xwt_gtk_createPane( PHB_ITEM pSelf )
{
   PXWT_GTK_CONTAINER pane;
   PXWT_WIDGET xwtData;

   pane = (PXWT_GTK_CONTAINER) hb_xgrab( sizeof( XWT_GTK_CONTAINER ) );
   pane->container = gtk_fixed_new();
   pane->owner = pSelf->item.asArray.value;

   // add a container to the window
   gtk_widget_show( pane->container );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = XWT_TYPE_PANE;
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = container_get_mainwidget;
   xwtData->get_top_widget = container_get_topwidget;

   return xwtData;
}

