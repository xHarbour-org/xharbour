/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_pane.c,v 1.2 2003/03/28 14:44:40 gian Exp $

   Pane - basic container with no layout capability
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createPane( PHB_ITEM pSelf )
{
   GtkWidget *pane;
   PXWT_WIDGET xwtData;
   
   pane = gtk_fixed_new();
   
   // add a container to the window
   gtk_widget_show( pane );
   
   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   
   xwtData->type = XWT_TYPE_PANE;
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return xwtData;
}


