/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_menu.c,v 1.2 2003/03/28 14:44:40 gian Exp $

   Menu management
*/
#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void *menu_get_mainwidget( void *data )
{
   GtkWidget *menu_item = (GtkWidget *) data;
   return gtk_menu_item_get_submenu( GTK_MENU_ITEM( menu_item) );
}


PXWT_WIDGET xwt_gtk_createMenu( PHB_ITEM pSelf )
{
   GtkWidget *menu, *bar_menu_item;
   PXWT_WIDGET xwtData;

   menu = gtk_menu_new();

   bar_menu_item = gtk_menu_item_new();
   // add a container to the window
   gtk_menu_item_set_submenu( GTK_MENU_ITEM(bar_menu_item), menu );
   
   gtk_widget_show( bar_menu_item );
   
   // no need for destructor, the data is just our widget for now
   XWT_CREATE_WIDGET( xwtData );
   
   xwtData->type = XWT_TYPE_MENU;
   xwtData->widget_data = (void *)bar_menu_item;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = menu_get_mainwidget;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return xwtData;
}


