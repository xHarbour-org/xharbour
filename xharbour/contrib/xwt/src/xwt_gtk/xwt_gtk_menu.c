/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_menu.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

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


BOOL xwt_gtk_createMenu( PXWT_WIDGET xwtData )
{
   GtkWidget *menu, *bar_menu_item;

   menu = gtk_menu_new();

   bar_menu_item = gtk_menu_item_new();
   // add a container to the window
   gtk_menu_item_set_submenu( GTK_MENU_ITEM(bar_menu_item), menu );
   
   gtk_widget_show( bar_menu_item );
   
   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)bar_menu_item;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = menu_get_mainwidget;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;

   return TRUE;
}


