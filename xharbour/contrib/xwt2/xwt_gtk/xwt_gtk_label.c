/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_label.c,v 1.5 2003/08/27 20:09:24 xthefull Exp $

   Label - Basic text label to draw on the screen
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>

static BOOL xwt_gtk_label_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_BASE wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wMain = wSelf->main_widget;

   if ( strcmp( prop, "text" ) == 0 )
   {
      gtk_label_set_label (GTK_LABEL(wMain), hb_itemGetCPtr( pValue ) );
   }
   else {
      ret = xwt_gtk_sensible_setprop( widget, prop, pValue );
   }
   
   return ret;
}

static BOOL xwt_gtk_label_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_BASE wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wMain = wSelf->main_widget;

   if ( strcmp( prop, "text" ) == 0 )
   {
      hb_itemPutC( pValue, (char *) gtk_label_get_label (GTK_LABEL(wMain) ) );
   }
   else {
      ret = xwt_gtk_sensible_getprop( widget, prop, pValue );
   }
   
   return ret;
}

   
static BOOL xwt_gtk_label_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   PXWT_GTK_BASE wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wMain = wSelf->main_widget;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   xwt_gtk_sensible_getall( widget, pRet );
   //text
   hb_itemPutC( &hbValue, (char *)gtk_label_get_label (GTK_LABEL(wMain) ) );
   hb_hashAddChar( pRet, "text", &hbValue );
   
   hb_itemClear( &hbValue );
   return TRUE;
}

   
BOOL xwt_gtk_createLabel( PXWT_WIDGET xwtData )
{
   GtkWidget *label;
   PXWT_GTK_SENSIBLE sens;

   label = gtk_label_new("");
   g_object_ref( G_OBJECT(label) );
   
   gtk_widget_show( label );

   sens = (PXWT_GTK_SENSIBLE) hb_xgrab( sizeof( XWT_GTK_SENSIBLE ) );
   
   sens->INH( INH( nId )) = 0;       
   sens->INH( INH( main_widget )) = label;
   sens->INH( INH( top_widget )) = xwt_gtk_sensible_topwidget;
   sens->INH( align ) = NULL;
   sens->evt_window = NULL;  
   sens->INH( iHAlign ) = XWT_ALIGN_LEFT;
   sens->INH( iVAlign ) = XWT_ALIGN_CENTER;
   
   // all sensible (basic) objects must have a valid alignment window
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) sens );
   
   // by default not sensible

   xwtData->widget_data = (void *)sens;
   xwtData->destroy = xwt_gtk_sensible_destroy;
   
   // property handlers
   xwtData->set_property = xwt_gtk_label_setprop;
   // nothing more than the base one
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_label_getprop;
   xwtData->get_all_properties = xwt_gtk_label_getall;

   xwt_gtk_base_signal_connect( xwtData );
   
   return TRUE;
}
