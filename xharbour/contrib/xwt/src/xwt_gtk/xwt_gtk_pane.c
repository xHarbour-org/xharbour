/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_pane.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   Pane - basic container with no layout capability
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void *pane_get_mainwidget( void *data )
{
   PXWT_GTK_PANE pane = (PXWT_GTK_PANE) data;
   return pane->pane;
}

static void *pane_get_topwidget( void *data )
{
   PXWT_GTK_PANE pane = (PXWT_GTK_PANE) data;
   if ( pane->frame != NULL)
   {
      return pane->frame;
   }
   return pane->pane;
}


PXWT_WIDGET xwt_gtk_createPane( PHB_ITEM pSelf )
{
   PXWT_GTK_PANE pane;
   PXWT_WIDGET xwtData;

   pane = (PXWT_GTK_PANE) hb_xgrab( sizeof( XWT_GTK_PANE ) );
   pane->pane = gtk_fixed_new();
   pane->owner = pSelf->item.asArray.value;

   // add a container to the window
   gtk_widget_show( pane->pane );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = XWT_TYPE_PANE;
   xwtData->widget_data = (void *)pane;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = pane_get_mainwidget;
   xwtData->get_top_widget = pane_get_topwidget;

   return xwtData;
}


BOOL xwt_gtk_pane_set_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_PANE pane = ( PXWT_GTK_PANE ) wWidget->widget_data;

   //Have we already a box?
   if ( pane->frame != NULL )
   {
      return FALSE;
   }
   pane->frame = xwt_gtk_enframe( pane->pane );

   return TRUE;
}


BOOL xwt_gtk_pane_reset_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_PANE pane = (PXWT_GTK_PANE ) wWidget->widget_data;

   //if we haven't a box...
   if ( pane->frame == NULL )
   {
      return FALSE;
   }
   xwt_gtk_deframe( pane->frame, pane->pane );
   pane->frame = NULL;
   return TRUE;
}


