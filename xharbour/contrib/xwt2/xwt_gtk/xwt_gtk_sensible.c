/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_label.c,v 1.5 2003/08/27 20:09:24 xthefull Exp $

  Sensible widget - block over normally unsensible widgets (labels and images)
  to give them eventual responsivity to events.
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>

/*
static gboolean
  button_press_callback (GtkWidget      *event_box,
                         GdkEventButton *event,
                         gpointer        data)
{
   HB_ITEM pPosX;
   HB_ITEM pPosY;
   PHB_ITEM pSelf = (PHB_ITEM) data;

   pPosX.type = HB_IT_NIL;
   pPosY.type = HB_IT_NIL;
   
   hb_itemPutNI( &pPosX, event->x );
   hb_itemPutNI( &pPosY, event->y );

   xwt_rise_event( pSelf, "click", 2, &pPosX, &pPosY );
   return TRUE;
}
*/

GtkWidget *xwt_gtk_sensible_topwidget( PXWT_WIDGET widget )
{
   XWT_GTK_SENSIBLE *wBase = (XWT_GTK_SENSIBLE *) widget->widget_data;
   if ( wBase->evt_window != NULL )
   {
      return wBase->evt_window;
   }
   
   return xwt_gtk_align_topwidget( widget );
}

BOOL xwt_gtk_sensible_destroy( PXWT_WIDGET widget )
{
   GtkWidget *wnd;
   PXWT_GTK_SENSIBLE wSelf = (PXWT_GTK_SENSIBLE) widget->widget_data;
   
   wnd = wSelf->evt_window;
   if ( wnd != NULL ) 
   {
      gtk_widget_destroy( wnd );
      wSelf->evt_window = NULL;
   }
   return xwt_gtk_align_destroy( widget );
}
   
BOOL xwt_gtk_sensible_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_SENSIBLE wSelf;

   /* NOTE: this does not refer to all the possibily sensibilizable widgets, but
      to those widgets that have not a special handling of their sensibility, that is
      all those widgets that have a widget_data->main_widget that can be immediately
      aligned and then sensibilized. */
   
   if ( strcmp( "sensibility", prop ) == 0 )
   {
      wSelf = ( PXWT_GTK_SENSIBLE ) widget->widget_data;
      
      if ( hb_itemGetL( pValue ) )
      {
         ret = xwt_gtk_sensibilize( widget, wSelf->INH(align) );
      }
      else
      {
         ret = xwt_gtk_desensibilize( widget, wSelf->INH( align ) );
      }
   }
   else {
      ret = xwt_gtk_align_setprop( widget, prop, pValue );
   }
   
   return ret;
}

BOOL xwt_gtk_sensible_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_SENSIBLE wSelf;

   if ( strcmp( "sensibility", prop ) == 0 )
   {
      wSelf = ( PXWT_GTK_SENSIBLE ) widget->widget_data;
      if ( wSelf->evt_window != NULL )
      {
         hb_itemPutL( pValue, TRUE );
      }
      else
      {
         hb_itemPutL( pValue, FALSE );
      }
   }
   else {
      ret = xwt_gtk_align_getprop( widget, prop, pValue );
   }
   
   return ret;
}

BOOL xwt_gtk_sensible_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   PXWT_GTK_SENSIBLE wSelf = ( PXWT_GTK_SENSIBLE ) widget->widget_data;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_align_getall( widget, pProps ) ) 
   {
      if ( wSelf->evt_window != NULL )
      {
         hb_itemPutL( &hbValue, TRUE );
      }
      else 
      {
         hb_itemPutL( &hbValue, FALSE );
      }
         
      hb_hashAddChar( pProps, "sensibility", &hbValue );
   }
   
   hb_itemClear( &hbValue );
   return TRUE;
}


BOOL xwt_gtk_sensibilize( PXWT_WIDGET wWidget, GtkWidget *master )
{
   PXWT_GTK_SENSIBLE wSelf = ( PXWT_GTK_SENSIBLE ) wWidget->widget_data;
   GtkWidget *evt;
   
   if ( wSelf->evt_window != NULL )
   {
      return FALSE;
   }

   evt = gtk_event_box_new();
   g_object_ref( G_OBJECT( evt ) );
   gtk_container_add( GTK_CONTAINER(evt), master );

   gtk_widget_show( evt );
   
   wSelf->evt_window = evt;
   xwt_gtk_base_signal_connect( wWidget );
   xwt_gtk_base_general_connect( wWidget );   
   return TRUE;
}


BOOL xwt_gtk_desensibilize( PXWT_WIDGET wWidget, GtkWidget *master )
{
   PXWT_GTK_SENSIBLE wSelf = ( PXWT_GTK_SENSIBLE ) wWidget->widget_data;
   GtkWidget *evt = wSelf->evt_window;
   
   if ( evt == NULL )
   {
      return FALSE;
   }

   gtk_container_remove( GTK_CONTAINER(evt), master );
   gtk_widget_destroy( evt );
   wSelf->evt_window = NULL;
   
   return TRUE;
}

/* end of xwt_gtk_sensible.c */
