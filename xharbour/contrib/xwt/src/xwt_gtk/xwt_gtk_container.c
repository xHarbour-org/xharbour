/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_container.c,v 1.2 2003/04/08 18:21:50 jonnymind Exp $

   Abstract container management.
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

void *container_get_topwidget( void *data )
{
   PXWT_GTK_CONTAINER widget = (PXWT_GTK_CONTAINER ) data;
   if ( widget->frame != NULL)
   {
      return widget->frame;
   }
   #if __GNUC__ <3
   if ( widget->a.align != NULL )
   {
      return widget->a.align;
   }
   #else
   if ( widget->align != NULL )
   {
      return widget->align;
   }
   #endif
   #if __GNUC__ <3
   return widget->a.a.main_widget;
   #else
   return widget->main_widget;
   #endif
}

BOOL xwt_gtk_container_set_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;

   //Have we already a box?
   if ( lay->frame != NULL )
   {
      return FALSE;
   }

   lay->frame = xwt_gtk_enframe( wWidget->get_top_widget( wWidget->widget_data ) );
   return TRUE;
}


BOOL xwt_gtk_container_reset_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;
   GtkWidget *oldFrame;

   //if we haven't a box...
   if ( lay->frame == NULL )
   {
      return FALSE;
   }

   oldFrame = lay->frame;
   lay->frame = NULL;

   xwt_gtk_deframe( oldFrame, wWidget->get_top_widget( wWidget->widget_data ) );

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

