/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_pane.c,v 1.3 2003/04/07 18:20:33 jonnymind Exp $

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
   pane->main_widget = gtk_fixed_new();
   pane->owner = pSelf->item.asArray.value;

   pane->frame = NULL;
   pane->align = NULL; // no frame for now
   pane->iHAlign = XWT_ALIGN_CENTER; // no frame for now
   pane->iVAlign = XWT_ALIGN_TOP; // no frame for now

   // add a container to the window
   gtk_widget_show( pane->main_widget );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = XWT_TYPE_PANE;
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return xwtData;
}

