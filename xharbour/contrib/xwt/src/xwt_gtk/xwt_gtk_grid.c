/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_layout.c,v 1.3 2003/04/07 18:20:33 jonnymind Exp $

   Grid - a col/row layout adapter
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createGrid( PHB_ITEM pSelf )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_WIDGET xwtData;
   PXWT_GTK_GRID grid;

   grid = ( PXWT_GTK_GRID ) hb_xgrab( sizeof( XWT_GTK_GRID ) );
   grid->iRows = 1; // still undefined
   grid->iCols = 1; // no frame for now
   grid->main_widget = gtk_table_new( 1, 1, FALSE );
   grid->frame = NULL;
   grid->iYPad = 0;
   grid->iXPad = 0;


   // add a container to the window
   gtk_widget_show( grid->main_widget );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = XWT_TYPE_GRID;
   xwtData->widget_data = (void *)grid;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return xwtData;
}

