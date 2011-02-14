/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id$

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
   sens->a.a.main_widget = label;
   sens->a.align = sens->evt_window = NULL;  
   sens->a.iHAlign = XWT_ALIGN_LEFT;
   sens->a.iVAlign = XWT_ALIGN_CENTER;
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) sens );

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)sens;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_sensible;

   return TRUE;
}
