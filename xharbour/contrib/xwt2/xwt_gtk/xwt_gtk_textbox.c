/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_textbox.c,v 1.2 2003/06/08 14:05:36 jonnymind Exp $

   Text box - basic input field
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void box_changed( GtkWidget *widget, gpointer user_data )
{
   HB_ITEM hbNid, hbText; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_IDWID btn = (PXWT_GTK_IDWID) ((PXWT_WIDGET) user_data)->widget_data;
   char *szText;
   
   hbNid.type = HB_IT_NIL;
   hb_itemPutNI( &hbNid, btn->INH(nId) );
   hbText.type = HB_IT_NIL;
   szText = (char *)gtk_entry_get_text( GTK_ENTRY(btn->main_widget) );
   hb_itemPutCRawStatic( &hbText, szText, strlen( szText ) );
      
   xwt_rise_event( pSelf, btn->szEventName, 2, &hbNid, &hbText );
}

static void box_activate( GtkWidget *widget, gpointer user_data )
{
   HB_ITEM hbNid, hbText; 
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_IDWID btn = (PXWT_GTK_IDWID) ((PXWT_WIDGET) user_data)->widget_data;
   char *szText;
   
   hbNid.type = HB_IT_NIL;
   hb_itemPutNI( &hbNid, btn->INH(nId) );
   hbText.type = HB_IT_NIL;
   szText =(char *) gtk_entry_get_text( GTK_ENTRY(btn->main_widget) );
   hb_itemPutCRawStatic( &hbText, szText, strlen( szText ) );
      
   xwt_rise_event( pSelf, "updated", 2, &hbNid, &hbText );
}


static BOOL xwt_gtk_textbox_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_IDWID *wSelf = (PXWT_GTK_IDWID) widget->widget_data;

   if ( strcmp( prop, "event" ) == 0 )
   {
      strncpy( wSelf->szEventName, hb_itemGetCPtr( pValue ), XWT_EVENT_NAME_SIZE );
   }
   else if ( strcmp( prop, "text" ) == 0)
   {
      gtk_entry_set_text( GTK_ENTRY(wSelf->main_widget), hb_itemGetCPtr( pValue ) );
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_textbox_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
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
      szPropVal = gtk_entry_get_text( GTK_ENTRY( wSelf->main_widget ));
      hb_itemPutCRawStatic( pValue, (char*)szPropVal, strlen( szPropVal ) );
   }
   else
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }
   
   return ret;
}


static BOOL xwt_gtk_textbox_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   HB_ITEM hbValue;
   XWT_GTK_IDWID *wSelf = (PXWT_GTK_IDWID) widget->widget_data;
   const char *szPropVal;

   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_base_getall( widget, pProps ) )
   {   
      hb_itemPutCRawStatic( &hbValue, wSelf->szEventName, strlen(wSelf->szEventName) );
      hb_hashAddChar( pProps, "event", &hbValue );
      
      szPropVal = gtk_entry_get_text( GTK_ENTRY( wSelf->main_widget ));
      hb_itemPutCRawStatic( &hbValue, (char*)szPropVal, strlen( szPropVal ) );
      hb_hashAddChar( pProps, "text", &hbValue );
            
      hb_itemClear( &hbValue );
      
      return TRUE;
   }

   return FALSE;
}


BOOL xwt_gtk_createTextbox( PXWT_WIDGET xwtData )
{
   PXWT_GTK_IDWID widget;
   GtkWidget *box;
   
   box = gtk_entry_new();
   g_object_ref( G_OBJECT( box ) );
   g_signal_connect (G_OBJECT(box), "activate", G_CALLBACK (box_activate), xwtData );
   g_signal_connect (G_OBJECT(box), "changed", G_CALLBACK (box_changed), xwtData );
   gtk_widget_show( box );

   widget = (PXWT_GTK_IDWID) hb_xgrab( sizeof( XWT_GTK_IDWID ) );
   widget->INH( nId ) = 0;
   widget->INH( top_widget ) = xwt_gtk_base_topwidget;
   widget->INH( main_widget ) = box;
   strcpy( widget->szEventName, "changed" );
   
   xwtData->widget_data = widget;
   xwtData->destroy = xwt_gtk_base_destroy;
   xwtData->set_property = xwt_gtk_textbox_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_textbox_getprop;
   xwtData->get_all_properties = xwt_gtk_textbox_getall;
   
   xwt_gtk_base_signal_connect( xwtData );
   xwt_gtk_base_general_connect( xwtData );  

   return TRUE;
}
