/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_button.c,v 1.3 2003/06/08 14:05:35 jonnymind Exp $

   GTK interface - management of button widget
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void btn_clicked( GtkWidget *widget,  gpointer user_data )
{
   HB_ITEM hb_xval; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_IDWID btn = (PXWT_GTK_IDWID) ((PXWT_WIDGET) user_data)->widget_data;
   
   hb_xval.type = HB_IT_NIL;
   hb_itemPutNI( &hb_xval, btn->INH(nId) );
   xwt_rise_event( pSelf, btn->szEventName, 1, &hb_xval );
}

static void btn_pressed( GtkWidget *widget, gpointer user_data )
{
   HB_ITEM hb_xval; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_IDWID btn = (PXWT_GTK_IDWID) ((PXWT_WIDGET) user_data)->widget_data;
   
   hb_xval.type = HB_IT_NIL;
   hb_itemPutNI( &hb_xval, btn->INH(nId) );
   xwt_rise_event( pSelf, "pressed", 1, &hb_xval );
}

static void btn_released( GtkWidget *widget, gpointer user_data )
{
   HB_ITEM hb_xval; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_IDWID btn = (PXWT_GTK_IDWID) ((PXWT_WIDGET) user_data)->widget_data;
   
   hb_xval.type = HB_IT_NIL;
   hb_itemPutNI( &hb_xval, btn->INH(nId) );
   xwt_rise_event( pSelf, "released", 1, &hb_xval );
}


static BOOL xwt_gtk_button_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_IDWID *wSelf = (PXWT_GTK_IDWID) widget->widget_data;

   if ( strcmp( prop, "event" ) == 0 )
   {
      strncpy( wSelf->szEventName, hb_itemGetCPtr( pValue ), XWT_EVENT_NAME_SIZE );
   }
   else if ( strcmp( prop, "text" ) == 0)
   {
      gtk_button_set_label( GTK_BUTTON( wSelf->main_widget), hb_itemGetCPtr( pValue ) );
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_button_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_IDWID *wSelf = (PXWT_GTK_IDWID) widget->widget_data;
   const char *szPropVal;
   
   if ( strcmp( prop, "event" ) == 0 )
   {
      hb_itemPutCRawStatic( pValue, wSelf->szEventName, strlen(wSelf->szEventName) );
   }
   else if ( strcmp( prop, "text" ) == 0)
   {
      szPropVal = gtk_button_get_label( GTK_BUTTON( wSelf->main_widget ));
      hb_itemPutCRawStatic( pValue, (char*)szPropVal, strlen( szPropVal ) );
   }
   else
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_button_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   HB_ITEM hbValue;
   XWT_GTK_IDWID *wSelf = (PXWT_GTK_IDWID) widget->widget_data;
   const char *szPropVal;

   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_base_getall( widget, pProps ) )
   {   
      hb_itemPutCRawStatic( &hbValue, wSelf->szEventName, strlen(wSelf->szEventName) );
      hb_hashAddChar( pProps, "event", &hbValue );
      
      szPropVal = gtk_button_get_label( GTK_BUTTON( wSelf->main_widget ));
      hb_itemPutCRawStatic( &hbValue, (char*)szPropVal, strlen( szPropVal ) );
      hb_hashAddChar( pProps, "text", &hbValue );
      
      hb_itemClear( &hbValue );
      
      return TRUE;
   }

   return FALSE;
}



BOOL xwt_gtk_createButton( PXWT_WIDGET xwtData )
{
   GtkWidget *button;
   XWT_GTK_IDWID *widget;
   
   // Gtk widget preparation
   button = gtk_button_new ();
   // our GC will dispose the widget, prevent auto destruction on parent disconnection
   g_object_ref( G_OBJECT(button) );
   g_signal_connect (G_OBJECT(button), "pressed", G_CALLBACK (btn_pressed), xwtData );
   g_signal_connect (G_OBJECT(button), "released", G_CALLBACK (btn_released), xwtData );
   g_signal_connect (G_OBJECT(button), "clicked", G_CALLBACK (btn_clicked), xwtData );
   gtk_widget_show( button );

   // XWT_GTK widget preparation
   widget = (PXWT_GTK_IDWID) hb_xgrab( sizeof( XWT_GTK_IDWID ) );
   
   widget->INH( nId ) = 0;
   widget->INH( main_widget ) = button;
   widget->INH( top_widget ) = xwt_gtk_base_topwidget;
   strcpy(widget->szEventName, "clicked");
      
   // XWT shell preparation
   xwtData->widget_data = widget;
   xwtData->destroy = xwt_gtk_base_destroy;      
   xwtData->set_property = xwt_gtk_button_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_button_getprop;
   xwtData->get_all_properties = xwt_gtk_button_getall;
   
   xwt_gtk_base_signal_connect( xwtData );
   xwt_gtk_base_general_connect( xwtData );   

   return TRUE;
}
