/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_menuitem.c,v 1.2 2003/03/28 14:44:40 gian Exp $

   Menu item managemetn
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void mi_activate( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

/* Currently, menuitems are not containers */
static void *mi_get_topwidget( void *data )
{
   return data;
}


PXWT_WIDGET xwt_gtk_createMenuItem( PHB_ITEM pSelf )
{
   GtkWidget *menuitem;
   PXWT_WIDGET xwtData;
      
   menuitem = gtk_menu_item_new ();
   // add a container to the window

   g_signal_connect (G_OBJECT (menuitem), "activate", G_CALLBACK (mi_activate),
      pSelf->item.asArray.value );
   
   gtk_widget_show( menuitem );

   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_MENUITEM;
   xwtData->widget_data = menuitem;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return xwtData;
}
