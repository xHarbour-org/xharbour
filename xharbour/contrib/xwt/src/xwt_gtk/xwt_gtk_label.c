/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_label.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Label - Basic text label to draw on the screen
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createLabel( PHB_ITEM pSelf )
{
   GtkWidget *label;
   PXWT_WIDGET xwtData;
   PXWT_GTK_SENSIBLE sens;

   label = gtk_label_new("");
   gtk_widget_show( label );

   sens = (PXWT_GTK_SENSIBLE) hb_xgrab( sizeof( XWT_GTK_SENSIBLE ) );
   sens->main_widget = label;
   sens->align = sens->evt_window = NULL;
   sens->iHAlign = XWT_ALIGN_LEFT;
   sens->iVAlign = XWT_ALIGN_CENTER;
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) sens );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_LABEL;
   xwtData->widget_data = (void *)sens;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_sensible;

   return xwtData;
}
