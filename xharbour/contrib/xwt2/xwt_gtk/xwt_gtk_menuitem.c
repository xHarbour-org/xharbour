/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_menuitem.c,v 1.1 2004/05/17 09:27:11 jonnymind Exp $

   Menu item managemetn
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void mi_activate( GtkWidget *widget,  gpointer user_data )
{
   HB_ITEM hb_xval; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_MENUITEM item = (PXWT_GTK_MENUITEM) ((PXWT_WIDGET) user_data)->widget_data;
   
   hb_xval.type = HB_IT_NIL;
   hb_itemPutNI( &hb_xval, item->INH(INH(nId)) );
   xwt_rise_event( pSelf, item->INH(szEventName), 1, &hb_xval );
}

static void mi_toggle( GtkWidget *widget,  gpointer user_data )
{
   // doesn't rise any XWT event
   PXWT_GTK_MENUITEM item = (PXWT_GTK_MENUITEM) ((PXWT_WIDGET) user_data)->widget_data;
   // just records widget status
   g_object_get( G_OBJECT(item->INH(INH(main_widget))), "active", &item->bValue, NULL );
}


static BOOL xwt_gtk_menuitem_destroy( PXWT_WIDGET widget )
{
   XWT_GTK_MENUITEM *wMenuitem = (XWT_GTK_MENUITEM *) widget->widget_data;
   gtk_widget_destroy( wMenuitem->INH(INH( main_widget ))  );
   
   // if it's a separator, the align inside it is unconected
   if ( GTK_WIDGET_TYPE( wMenuitem->INH(INH( main_widget ) ) ) == GTK_TYPE_SEPARATOR_MENU_ITEM  )
   {
      gtk_widget_destroy( wMenuitem->align );
   } 
   return TRUE;
}


static BOOL xwt_gtk_menuitem_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_MENUITEM *wSelf = (PXWT_GTK_MENUITEM) widget->widget_data;

   if ( strcmp( prop, "event" ) == 0 )
   {
      strncpy( wSelf->INH(szEventName), hb_itemGetCPtr( pValue ), XWT_EVENT_NAME_SIZE );
   }
   else if ( strcmp( prop, "text" ) == 0)
   {
      gtk_label_set_label( GTK_LABEL( wSelf->label ), hb_itemGetCPtr( pValue ) );
   }
   else if ( strcmp( prop, "separator" ) == 0 )
   {
      GtkWidget *menu = wSelf->INH(INH(main_widget));
      
      if ( hb_itemGetL( pValue ) == TRUE && 
               GTK_WIDGET_TYPE( wSelf->INH(INH( main_widget ) ) ) != GTK_TYPE_SEPARATOR_MENU_ITEM )
      {
         g_object_ref( G_OBJECT( wSelf->align ) );
         gtk_container_remove( GTK_CONTAINER( menu ), wSelf->align );
         gtk_widget_destroy( menu );
         menu = gtk_separator_menu_item_new();
      }
      else if ( hb_itemGetL( pValue ) == FALSE && 
               GTK_WIDGET_TYPE( wSelf->INH(INH( main_widget ) ) ) == GTK_TYPE_SEPARATOR_MENU_ITEM )
      {
         g_object_ref( G_OBJECT( wSelf->align ) );
         gtk_widget_destroy( menu );
         
         if ( wSelf->bIsCheck )
         {
            menu = gtk_check_menu_item_new();
            gtk_check_menu_item_set_active( GTK_CHECK_MENU_ITEM( menu ), (gboolean) wSelf->bValue );
            g_signal_connect ( G_OBJECT(menu), "toggled", G_CALLBACK(mi_toggle), widget );
         }
         else
         {
            menu = gtk_menu_item_new();
         }         
         gtk_container_add( GTK_CONTAINER( menu ), wSelf->align );
         g_signal_connect ( G_OBJECT(menu), "activate", G_CALLBACK(mi_activate), widget );
      }
      
      g_object_ref( G_OBJECT( menu ));
      gtk_widget_show( menu );
      wSelf->INH(INH(main_widget)) = menu;
   }
   else if ( strcmp( prop, "checkbox" ) == 0 )
   {
      BOOL bOld = wSelf->bIsCheck;
      wSelf->bIsCheck = hb_itemGetL( pValue );
      
      if ( GTK_WIDGET_TYPE( wSelf->INH(INH( main_widget ) ) ) != GTK_TYPE_SEPARATOR_MENU_ITEM 
            && bOld != wSelf->bIsCheck )
      {
         GtkWidget *menu = wSelf->INH(INH(main_widget));
         
         g_object_ref( G_OBJECT( wSelf->align ) );
         gtk_container_remove( GTK_CONTAINER( menu ), wSelf->align );
         gtk_widget_destroy( menu );
         
         if ( !bOld && wSelf->bIsCheck )
         {   
            menu = gtk_check_menu_item_new();
            gtk_check_menu_item_set_active( GTK_CHECK_MENU_ITEM( menu ), (gboolean) wSelf->bValue );
            g_signal_connect (G_OBJECT (menu), "toggled", G_CALLBACK(mi_toggle), widget );
         }
         else if ( ! bOld && wSelf->bIsCheck )
         {
            menu = gtk_menu_item_new();
         }
         
         gtk_container_add( GTK_CONTAINER( menu ), wSelf->align );
         g_object_ref( G_OBJECT( menu ));
         gtk_widget_show( menu );
         g_signal_connect (G_OBJECT (menu), "activate", G_CALLBACK(mi_activate), widget );
         wSelf->INH(INH(main_widget)) = menu;
      }
   }
   else if ( strcmp( prop, "checked" ) == 0 )
   {
      BOOL bOld = wSelf->bValue;
      wSelf->bValue = hb_itemGetL( pValue );
      if ( bOld != wSelf->bValue && wSelf->bIsCheck )
      {
         gtk_check_menu_item_set_active( GTK_CHECK_MENU_ITEM( wSelf ), (gboolean) wSelf->bValue );
      }
   }
   else if ( strcmp( prop, "enabled" ) == 0 )
   {
      gtk_widget_set_sensitive( wSelf->INH(INH( main_widget )), hb_itemGetL( pValue ) );
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_menuitem_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_MENUITEM *wSelf = (PXWT_GTK_MENUITEM) widget->widget_data;
   const char *szPropVal;
   
   if ( strcmp( prop, "event" ) == 0 )
   {
      hb_itemPutCRawStatic( pValue, wSelf->INH(szEventName), strlen(wSelf->szEventName) );
   }
   else if ( strcmp( prop, "text" ) == 0)
   {
      szPropVal = gtk_label_get_label( GTK_LABEL( wSelf->label ));
      hb_itemPutCRawStatic( pValue, (char*)szPropVal, strlen( szPropVal ) );
   }
   else if ( strcmp( prop, "separator" ) == 0)
   {
      hb_itemPutL( pValue, GTK_WIDGET_TYPE( wSelf->INH(INH( main_widget ) ) ) == GTK_TYPE_SEPARATOR_MENU_ITEM );
   }
   else if ( strcmp( prop, "checkbox" ) == 0)
   {
      hb_itemPutL( pValue, wSelf->bIsCheck );
   }
   else if ( strcmp( prop, "checked" ) == 0 )
   {
      hb_itemPutL( pValue, wSelf->bValue );
   }
   else if ( strcmp( prop, "enabled" ) == 0 )
   {
      BOOL bSens;
      g_object_get( G_OBJECT( wSelf->INH(INH(main_widget)) ), "sensitive", &bSens, NULL );
      hb_itemPutL( pValue, bSens );
   }
   else
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_menuitem_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   BOOL bSens;
   HB_ITEM hbValue;
   XWT_GTK_MENUITEM *wSelf = (PXWT_GTK_MENUITEM) widget->widget_data;
   const char *szPropVal;

   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_base_getall( widget, pProps ) )
   {   
      hb_itemPutCRawStatic( &hbValue, wSelf->INH(szEventName), strlen(wSelf->szEventName) );
      hb_hashAddChar( pProps, "event", &hbValue );
      
      hb_itemPutL( &hbValue, GTK_WIDGET_TYPE( wSelf->INH(INH( main_widget ) ) ) == GTK_TYPE_SEPARATOR_MENU_ITEM );
      hb_hashAddChar( pProps, "separator", &hbValue );
      
      hb_hashAddChar( pProps, "checkbox", hb_itemPutL( &hbValue, wSelf->bIsCheck ) );
      hb_hashAddChar( pProps, "checked", hb_itemPutL( &hbValue, wSelf->bValue ) );
      
      g_object_get( G_OBJECT( wSelf->INH(INH(main_widget)) ), "sensitive", &bSens, NULL );
      hb_hashAddChar( pProps, "enabled", hb_itemPutL( &hbValue, bSens ) );
      
      szPropVal = gtk_label_get_label( GTK_LABEL( wSelf->label ));
      hb_itemPutCRawStatic( &hbValue, (char*)szPropVal, strlen( szPropVal ) );
      hb_hashAddChar( pProps, "text", &hbValue );
      
      hb_itemClear( &hbValue );
      
      return TRUE;
   }

   return FALSE;
}


BOOL xwt_gtk_createMenuItem( PXWT_WIDGET xwtData )
{
   PXWT_GTK_MENUITEM menuitem;

   menuitem = (PXWT_GTK_MENUITEM) hb_xgrab( sizeof( XWT_GTK_MENUITEM ) );
   menuitem->INH(INH(main_widget)) = gtk_menu_item_new ();
   menuitem->INH(INH(top_widget)) = xwt_gtk_base_topwidget;
   menuitem->INH(INH(nId)) = 0;
   menuitem->INH( INH( fgColor ) )= NULL;       
   menuitem->INH( INH( bgColor ) )= NULL;          
   menuitem->INH( INH( textColor ) ) = NULL;          
   menuitem->INH( INH( baseColor ) ) = NULL;          

   strncpy( menuitem->INH(szEventName), "menu", 5);
   
   g_object_ref( G_OBJECT(menuitem->INH(INH(main_widget))));
   menuitem->hbox = gtk_hbox_new( FALSE, 2 );
   menuitem->image = gtk_image_new();
   menuitem->label = gtk_label_new("");
   menuitem->align = gtk_alignment_new( 0.0, 0.5, 0.0, 0.0 );
   menuitem->bIsCheck = FALSE;
   menuitem->bValue = FALSE;
   gtk_container_add( GTK_CONTAINER( menuitem->hbox), menuitem->image );
   gtk_container_add( GTK_CONTAINER( menuitem->hbox ), menuitem->label );
   gtk_container_add( GTK_CONTAINER( menuitem->align), menuitem->hbox );
   gtk_container_add( GTK_CONTAINER( menuitem->INH(INH(main_widget))), menuitem->align );

   gtk_widget_show( menuitem->hbox );
   gtk_widget_show( menuitem->label );
   gtk_label_set_use_underline( GTK_LABEL(menuitem->label), TRUE );
   gtk_widget_show( menuitem->image );
   gtk_widget_show( menuitem->align );

   g_signal_connect (G_OBJECT (menuitem->INH(INH(main_widget))), "activate", G_CALLBACK(mi_activate),
      xwtData );
   gtk_widget_show( menuitem->INH(INH(main_widget)) );

   
   xwtData->widget_data = menuitem;
   xwtData->destroy = xwt_gtk_base_destroy;
   xwtData->destroy = xwt_gtk_menuitem_destroy;      
   xwtData->set_property = xwt_gtk_menuitem_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_menuitem_getprop;
   xwtData->get_all_properties = xwt_gtk_menuitem_getall;   
   xwt_gtk_base_signal_connect( xwtData );   

   return TRUE;
}
