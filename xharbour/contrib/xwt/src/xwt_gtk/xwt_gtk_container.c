/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_pane.c,v 1.2 2003/04/07 15:41:08 jonnymind Exp $

   Abstract container management.
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

void *container_get_mainwidget( void *data )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) data;
   return lay->container;
}

void *container_get_topwidget( void *data )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) data;
   if ( lay->frame != NULL)
   {
      return lay->frame;
   }
   return lay->container;
}

BOOL xwt_gtk_container_set_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;

   //Have we already a box?
   if ( lay->frame != NULL )
   {
      return FALSE;
   }

   lay->frame = xwt_gtk_enframe( lay->container );
   return TRUE;
}


BOOL xwt_gtk_container_reset_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;

   //if we haven't a box...
   if ( lay->frame == NULL )
   {
      return FALSE;
   }

   xwt_gtk_deframe( lay->frame, lay->container );
   lay->frame = NULL;
   return TRUE;
}



GtkWidget *xwt_gtk_enframe( GtkWidget *framed )
{
   GtkWidget *parent;
   GtkWidget *frame;

   frame = gtk_frame_new( NULL );
   parent = gtk_widget_get_parent( framed );

   //Moving the new frame to the old parent if necessary
   if ( parent != NULL )
   {
      g_object_ref( framed );
      gtk_container_remove( GTK_CONTAINER( parent ) , framed );
      gtk_container_add( GTK_CONTAINER( parent ), frame );
   }
   gtk_container_add( GTK_CONTAINER( frame ), framed );
   if ( parent != NULL )
   {
      g_object_unref( framed );
   }
   gtk_widget_show( frame );

   return frame;
}


void xwt_gtk_deframe( GtkWidget *frame, GtkWidget *framed )
{
   GtkWidget *parent;
   parent = gtk_widget_get_parent( frame );

   g_object_ref( framed );
   gtk_container_remove( GTK_CONTAINER( frame ), framed );

   if ( parent != NULL )
   {
      gtk_container_remove( GTK_CONTAINER( parent ) , frame );
      gtk_container_add( GTK_CONTAINER( parent ), framed );
   }
   gtk_widget_destroy( frame );
}

