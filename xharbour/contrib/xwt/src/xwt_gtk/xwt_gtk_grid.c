/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_grid.c,v 1.1 2003/04/08 18:23:07 jonnymind Exp $

   Grid - a col/row layout adapter
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

PXWT_WIDGET xwt_gtk_createGrid( PHB_ITEM pSelf )
{
   PXWT_WIDGET xwtData;
   PXWT_GTK_GRID grid;

   grid = ( PXWT_GTK_GRID ) hb_xgrab( sizeof( XWT_GTK_GRID ) );
   grid->iRows = 1; // still undefined
   grid->iCols = 1; // no frame for now
   grid->main_widget = gtk_table_new( 1, 1, FALSE );
   grid->frame = NULL;
   grid->iYPad = 0;
   grid->iXPad = 0;


   gtk_widget_show( grid->main_widget );

   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = XWT_TYPE_GRID;
   xwtData->widget_data = (void *)grid;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return xwtData;
}

