/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_textbox.c,v 1.2 2003/03/28 14:44:40 gian Exp $

   Layout - Horizontal or vertical layout manager
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void *layout_get_mainwidget( void *data )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) data;
   return lay->layout;
}

static void *layout_get_topwidget( void *data )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) data;
   if ( lay->frame != NULL)
   {
      return lay->frame;
   }

   return lay->layout;
}


PXWT_WIDGET xwt_gtk_createLayout( PHB_ITEM pSelf )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_WIDGET xwtData;
   PXWT_GTK_LAYOUT gtkLayout;

   gtkLayout = ( PXWT_GTK_LAYOUT ) hb_xgrab( sizeof( XWT_GTK_LAYOUT ) );
   gtkLayout->iMode = -1; // still undefined
   gtkLayout->frame = NULL; // no frame for now
   gtkLayout->layout = NULL; // still not available

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_LAYOUT;
   // no widget for now.
   xwtData->widget_data = (void *) gtkLayout;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = layout_get_mainwidget;
   xwtData->get_top_widget = layout_get_topwidget;

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
      lay->layout = gtk_hbox_new( FALSE, 0 );
   }
   else
   {
      lay->layout = gtk_vbox_new( FALSE, 0 );
   }

   gtk_widget_show( lay->layout );

   return TRUE;
 }

BOOL xwt_gtk_layout_set_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
   GtkWidget *parent;

   //Have we already a box?
   if ( lay->frame != NULL )
   {
      return FALSE;
   }

   // let's get layout parent
   lay->frame = gtk_frame_new( NULL );

   parent = gtk_widget_get_parent( lay->layout );
   gtk_widget_reparent( lay->layout, lay->frame );

   //Moving the new frame to the old parent if necessary
   if ( parent != NULL )
   {
      gtk_container_add( GTK_CONTAINER( parent ), lay->frame );
   }
   gtk_widget_show( lay->frame );

   return TRUE;
}


BOOL xwt_gtk_layout_reset_box( PXWT_WIDGET wWidget )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT ) wWidget->widget_data;
   GtkWidget *parent;

   //if we haven't a box...
   if ( lay->frame == NULL )
   {
      return FALSE;
   }

   parent = gtk_widget_get_parent( lay->frame );

   if ( parent != NULL )
   {
      gtk_widget_reparent( lay->layout, lay->frame );
   }
   else
   {
      gtk_container_remove( GTK_CONTAINER( lay->frame ), lay->layout );
   }
   gtk_widget_destroy( lay->frame );
   lay->frame = NULL;

   return TRUE;
}
