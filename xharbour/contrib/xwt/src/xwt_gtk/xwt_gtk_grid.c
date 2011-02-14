/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id$

   Grid - a col/row layout adapter
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

BOOL xwt_gtk_createGrid( PXWT_WIDGET xwtData )
{
   PXWT_GTK_GRID grid;

   grid = ( PXWT_GTK_GRID ) hb_xgrab( sizeof( XWT_GTK_GRID ) );
   grid->iRows = 1; // still undefined
   grid->iCols = 1; // no frame for now
   grid->a.a.a.a.main_widget = gtk_table_new( 1, 1, FALSE );
   grid->a.a.a.align = NULL; // no frame for now
   grid->a.a.a.iHAlign = XWT_ALIGN_CENTER; // no frame for now
   grid->a.a.a.iVAlign = XWT_ALIGN_TOP; // no frame for now
   grid->a.a.frame = NULL;
   grid->iYPad = 0;
   grid->iXPad = 0;
   gtk_widget_show( grid->a.a.a.a.main_widget );

   xwtData->widget_data = (void *)grid;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return TRUE;
}

