/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_layout.c,v 1.6 2003/07/23 15:58:10 lculik Exp $

   Layout - Horizontal or vertical layout manager
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

BOOL xwt_gtk_createLayout( PXWT_WIDGET xwtData )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_GTK_LAYOUT gtkLayout;

   gtkLayout = ( PXWT_GTK_LAYOUT ) hb_xgrab( sizeof( XWT_GTK_LAYOUT ) );
   gtkLayout->iMode = -1; // still undefined

   #if __GNUC__ <3
   gtkLayout->a.a.frame = NULL; // no frame for now
   gtkLayout->a.a.a.align = NULL; // no frame for now
   gtkLayout->a.a.a.iHAlign = XWT_ALIGN_CENTER; // no frame for now
   gtkLayout->a.a.a.iVAlign = XWT_ALIGN_TOP; // no frame for now
   #else
   gtkLayout->frame = NULL; // no frame for now
   gtkLayout->align = NULL; // no frame for now
   gtkLayout->iHAlign = XWT_ALIGN_CENTER; // no frame for now
   gtkLayout->iVAlign = XWT_ALIGN_TOP; // no frame for now
   #endif

   #if __GNUC__ < 3
   gtkLayout->a.a.a.a.main_widget = NULL; // still not available
   #else
   gtkLayout->main_widget = NULL; // still not available
   #endif

   gtkLayout->iPadding = 0;
   #if __GNUC__ <3
   gtkLayout->a.bFill = FALSE;
   gtkLayout->a.bExpand = FALSE;
   #else
   gtkLayout->bFill = FALSE;
   gtkLayout->bExpand = FALSE;
   #endif

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *) gtkLayout;
   xwtData->destructor = hb_xfree;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = container_get_topwidget;

   return TRUE;
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
   #if __GNUC__ <3
      lay->a.a.a.a.main_widget = gtk_hbox_new( FALSE, 0 );
   #else
      lay->main_widget = gtk_hbox_new( FALSE, 0 );
   #endif
   }
   else
   {
   #if __GNUC__ <3
      lay->a.a.a.a.main_widget = gtk_vbox_new( FALSE, 0 );
   #else
      lay->main_widget = gtk_vbox_new( FALSE, 0 );
   #endif
   }
   #if __GNUC__ <3
   gtk_widget_show( lay->a.a.a.a.main_widget );
   #else
   gtk_widget_show( lay->main_widget );
   #endif

   return TRUE;
 }
