/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_label.c,v 1.2 2003/03/28 14:44:40 gian Exp $

   Label - Basic text label to draw on the screen
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createLabel( PHB_ITEM pSelf )
{
   GtkWidget *label;
   PXWT_WIDGET xwtData;
   label = gtk_label_new("");

   gtk_widget_show( label );
   
   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_LABEL;
   xwtData->widget_data = (void *)label;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return xwtData;
}
