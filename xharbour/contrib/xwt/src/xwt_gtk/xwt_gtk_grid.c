/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_grid.c,v 1.4 2003/06/08 14:05:35 jonnymind Exp $

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
   #if __GNUC__ <3
   grid->a.a.a.a.main_widget = gtk_table_new( 1, 1, FALSE );
   #else
   grid->main_widget = gtk_table_new( 1, 1, FALSE );
   #endif
   #if  __GNUC__ <3
   grid->a.a.a.align = NULL; // no frame for now
   #else
   grid->align = NULL; // no frame for now
   #endif
   #if __GNUC__ <3
   grid->a.a.a.iHAlign = XWT_ALIGN_CENTER; // no frame for now
   grid->a.a.a.iVAlign = XWT_ALIGN_TOP; // no frame for now
   grid->a.a.frame = NULL;
   #else
   grid->iHAlign = XWT_ALIGN_CENTER; // no frame for now
   grid->iVAlign = XWT_ALIGN_TOP; // no frame for now
   grid->frame = NULL;
   #endif
   grid->iYPad = 0;
   grid->iXPad = 0;
   #if __GNUC__ <3
   gtk_widget_show( grid->a.a.a.a.main_widget );
   #else
   gtk_widget_show( grid->main_widget );
   #endif

   xwtData->widget_data = (void *)grid;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return TRUE;
}

