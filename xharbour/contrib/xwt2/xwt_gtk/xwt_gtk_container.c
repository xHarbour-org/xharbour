/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_container.c,v 1.3 2003/07/23 15:58:10 lculik Exp $

   Abstract container management.
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>


/**********************************************************************
* Container property managers.
*/

   
BOOL xwt_gtk_container_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_CONTAINER wSelf;

   /* NOTE: see sensibilizable widgets note */
   
   if ( strcmp( "frame", prop ) == 0 )
   {
      wSelf = ( PXWT_GTK_CONTAINER ) widget->widget_data;
      
      if ( hb_itemGetL( pValue ) )
      {
            ret = xwt_gtk_enframe( widget, wSelf->INH(align) );
      }
      else
      {
            ret = xwt_gtk_deframe( widget, wSelf->INH( align ) );
      }
   }
   else
   {
      ret = xwt_gtk_align_getprop( widget, prop, pValue );
   }
   
   return ret;
}

BOOL xwt_gtk_container_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_CONTAINER wSelf;

   if ( strcmp( "frame", prop ) == 0 )
   {
      wSelf = ( PXWT_GTK_CONTAINER ) widget->widget_data;
      if ( wSelf->frame != NULL )
      {
         hb_itemPutL( pValue, TRUE );
      }
      else
      {
         hb_itemPutL( pValue, FALSE );
      }
   }
   else
   {
      ret = xwt_gtk_align_getprop( widget, prop, pValue );
   }
   
   return ret;
}

BOOL xwt_gtk_container_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) widget->widget_data;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   if ( lay->frame != NULL )
   {
      hb_itemPutL( &hbValue, TRUE );  
   }
   else 
   {
       hb_itemPutL( &hbValue, FALSE );
   }
   hb_hashAddChar( pProps, "frame", &hbValue );
   
   hb_itemClear( &hbValue );
   return TRUE;
}


/******************************************************
*  Container common Functions
*/

BOOL xwt_gtk_container_remove( PXWT_WIDGET wParent, PXWT_WIDGET wChild )
{
   PXWT_GTK_BASE cont = (PXWT_GTK_BASE) wParent->widget_data;
   PXWT_GTK_BASE child = (PXWT_GTK_BASE) wChild->widget_data;
   
   gtk_container_remove(GTK_CONTAINER( cont->main_widget ), child->top_widget( wChild ) );
   return TRUE;
}


GtkWidget *xwt_gtk_container_topwidget( PXWT_WIDGET widget )
{
   PXWT_GTK_CONTAINER cont = (PXWT_GTK_CONTAINER) widget->widget_data;
   if ( cont->frame != NULL )
   {
      return cont->frame;
   }
   return xwt_gtk_align_topwidget( widget );
}

BOOL xwt_gtk_container_destroy( PXWT_WIDGET wWidget )
{
   PXWT_GTK_BASE cont = (PXWT_GTK_BASE) wWidget->widget_data;
   // remove all children
   GList *child = gtk_container_get_children( GTK_CONTAINER( cont->main_widget) );
   
   while( child != NULL )
   {
      if ( child->data != NULL )
      {
         gtk_container_remove(GTK_CONTAINER( cont->main_widget ), GTK_WIDGET( child->data ));
      }
      child = child->next;
   }
   
   gtk_widget_destroy( cont->top_widget( wWidget ) );

   return TRUE;
}


BOOL xwt_gtk_enframe( PXWT_WIDGET wWidget, GtkWidget *framed )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;
   GtkWidget *parent;
   
   //Have we already a box?
   if ( lay->frame != NULL )
   {
      return FALSE;
   }

   lay->frame = gtk_frame_new( NULL );
   g_object_ref( lay->frame );
   parent = gtk_widget_get_parent( framed );

   //Moving the new frame to the old parent if necessary
   if ( parent != NULL )
   {
      g_object_ref( framed );
      gtk_container_remove( GTK_CONTAINER( parent ) , framed );
      gtk_container_add( GTK_CONTAINER( parent ), lay->frame );
   }
   gtk_container_add( GTK_CONTAINER( lay->frame  ), framed );
   if ( parent != NULL )
   {
      g_object_unref( framed );
   }
   gtk_widget_show( lay->frame );

   return TRUE;
}


BOOL xwt_gtk_deframe( PXWT_WIDGET wWidget, GtkWidget *framed )
{
   PXWT_GTK_CONTAINER lay = (PXWT_GTK_CONTAINER ) wWidget->widget_data;
   GtkWidget *parent;

   // return if we haven't a box...
   if ( lay->frame == NULL )
   {
      return FALSE;
   }
   
   parent = gtk_widget_get_parent( lay->frame );
      
   g_object_ref( framed );
   gtk_container_remove( GTK_CONTAINER( lay->frame ), framed );

   if ( parent != NULL )
   {
      gtk_container_remove( GTK_CONTAINER( parent ) , lay->frame );
      gtk_container_add( GTK_CONTAINER( parent ), framed );
   }
   gtk_widget_destroy( lay->frame );
   
   return TRUE;
}





