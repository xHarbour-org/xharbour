/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_menu.c,v 1.2 2003/06/08 14:05:35 jonnymind Exp $

   Menu management
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static GtkWidget *menu_topwidget( PXWT_WIDGET widget )
{
   XWT_GTK_MENU *wMenu = (XWT_GTK_MENU *) widget->widget_data;
   return wMenu->bar_item;
}

static BOOL xwt_gtk_menu_destroy( PXWT_WIDGET widget )
{
   XWT_GTK_MENU *wMenu = (XWT_GTK_MENU *) widget->widget_data;
   gtk_widget_destroy( wMenu->bar_item );
   return TRUE;
}

static BOOL xwt_gtk_menu_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_MENU *wSelf = (PXWT_GTK_MENU) widget->widget_data;

   if ( strcmp( prop, "text" ) == 0)
   {
      gtk_label_set_label( GTK_LABEL( wSelf->label ), hb_itemGetCPtr( pValue ) );
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_menu_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_MENU *wSelf = (PXWT_GTK_MENU) widget->widget_data;
   const char *szPropVal;
   
   if ( strcmp( prop, "text" ) == 0)
   {
      szPropVal = gtk_label_get_label( GTK_LABEL( wSelf->label ));
      hb_itemPutCRawStatic( pValue, (char*)szPropVal, strlen( szPropVal ) );
   }
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_menu_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   HB_ITEM hbValue;
   XWT_GTK_MENU *wSelf = (PXWT_GTK_MENU) widget->widget_data;
   const char *szPropVal;

   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_base_getall( widget, pProps ) )
   {   
      szPropVal = gtk_label_get_label( GTK_LABEL( wSelf->label ));
      hb_itemPutCRawStatic( &hbValue, (char*)szPropVal, strlen( szPropVal ) );
      hb_hashAddChar( pProps, "text", &hbValue );
      
      hb_itemClear( &hbValue );
      
      return TRUE;
   }

   return FALSE;
}


BOOL xwt_gtk_createMenu( PXWT_WIDGET xwtData )
{
   GtkWidget *menu, *bar_menu_item;
   PXWT_GTK_MENU widget;
   
   menu = gtk_menu_new();
   bar_menu_item = gtk_menu_item_new();
   g_object_ref( G_OBJECT(bar_menu_item));
   // add a container to the window
   gtk_menu_item_set_submenu( GTK_MENU_ITEM(bar_menu_item), menu );
   gtk_widget_show( bar_menu_item );
   
   widget = (PXWT_GTK_MENU) hb_xgrab( sizeof( XWT_GTK_MENU) );
   widget->INH(nId) = 0;
   widget->INH(main_widget) = menu;
   widget->INH(top_widget) = menu_topwidget;
   widget->bar_item = bar_menu_item;
   widget->label = gtk_label_new("");
   gtk_label_set_use_underline( GTK_LABEL(widget->label), TRUE );
   gtk_container_add( GTK_CONTAINER( bar_menu_item ), widget->label );
   gtk_widget_show( widget->label );
   
   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *)widget;
   xwtData->destroy = xwt_gtk_menu_destroy;
   xwtData->set_property = xwt_gtk_menu_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_menu_getprop;
   xwtData->get_all_properties = xwt_gtk_menu_getall;   
   xwt_gtk_base_signal_connect( xwtData );   

   return TRUE;
}

