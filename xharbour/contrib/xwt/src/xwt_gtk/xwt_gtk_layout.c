/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_layout.c,v 1.2 2003/04/07 15:41:08 jonnymind Exp $

   Layout - Horizontal or vertical layout manager
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createLayout( PHB_ITEM pSelf )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_WIDGET xwtData;
   PXWT_GTK_LAYOUT gtkLayout;

   gtkLayout = ( PXWT_GTK_LAYOUT ) hb_xgrab( sizeof( XWT_GTK_LAYOUT ) );
   gtkLayout->iMode = -1; // still undefined
   gtkLayout->frame = NULL; // no frame for now
   gtkLayout->container = NULL; // still not available
   gtkLayout->owner = pSelf->item.asArray.value;

   gtkLayout->iPadding = 0;
   gtkLayout->bFill = FALSE;
   gtkLayout->bExpand = FALSE;

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_LAYOUT;
   // no widget for now.
   xwtData->widget_data = (void *) gtkLayout;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = container_get_mainwidget;
   xwtData->get_top_widget = container_get_topwidget;

   return xwtData;
}

/* This function creates the real gtk widget. */
BOOL xwt_gtk_layout_create_with_mode( PXWT_WIDGET wWidget, int mode  )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT ) wWidget->widget_data;

   if ( lay->iMode != -1 )
   {
      return FALSE;
   }

   lay->iMode = mode;
   if ( mode == XWT_LM_HORIZ )
   {
      lay->container = gtk_hbox_new( FALSE, 0 );
   }
   else
   {
      lay->container = gtk_vbox_new( FALSE, 0 );
   }

   gtk_widget_show( lay->container );

   return TRUE;
 }
