/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_label.c,v 1.2 2003/04/08 18:21:51 jonnymind Exp $

   Label - Basic text label to draw on the screen
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

BOOL xwt_gtk_createLabel( PXWT_WIDGET xwtData )
{
   GtkWidget *label;
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
   xwtData->widget_data = (void *)sens;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_sensible;

   return TRUE;
}
