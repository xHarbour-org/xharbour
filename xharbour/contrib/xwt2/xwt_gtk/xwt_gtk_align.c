/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_align.c,v 1.2 2004/05/20 15:41:38 jonnymind Exp $

  Align widget - block over unaligned widgets to provide them with alignment ability.
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>

/** properties of the align widget:
   align: left, center, right.
   valign: top, center, bottom.
*/

GtkWidget *xwt_gtk_align_topwidget( PXWT_WIDGET widget )
{
   XWT_GTK_ALIGN *wBase = (XWT_GTK_ALIGN *) widget->widget_data;
   if ( wBase->align != NULL )
   {
      return wBase->align;
   }

   return xwt_gtk_base_topwidget( widget );
}

BOOL xwt_gtk_align_destroy( PXWT_WIDGET widget )
{
   GtkWidget *wnd;
   PXWT_GTK_ALIGN wSelf = (PXWT_GTK_ALIGN) widget->widget_data;

   wnd = wSelf->align;
   if ( wnd != NULL )
   {
      gtk_widget_destroy( wnd );
      wSelf->align = NULL;
   }
   return xwt_gtk_base_destroy( widget );
}

BOOL xwt_gtk_align_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_ALIGN* wMain = (XWT_GTK_ALIGN*) widget->widget_data;
   char *szAName;

   if ( strcmp( prop, "valign" ) == 0 )
   {
      szAName = hb_itemGetCPtr( pValue );
      if ( szAName == NULL )
      {
         ret = FALSE;
      }
      else
      {
         if ( strcmp( "top", szAName ) == 0 )
         {
            wMain->iVAlign = XWT_ALIGN_TOP;
         }
         else if ( strcmp( "center", szAName ) == 0 )
         {
            wMain->iVAlign = XWT_ALIGN_CENTER;
         }
         else if ( strcmp( "bottom", szAName ) == 0 )
         {
            wMain->iVAlign = XWT_ALIGN_BOTTOM;
         }
         else
         {
            ret = FALSE;
         }
      }

      if (ret) {
         xwt_gtk_set_alignment( wMain );
      }
   }
   else if ( strcmp( prop, "align" ) == 0 )
   {
     szAName = hb_itemGetCPtr( pValue );
      if ( szAName == NULL )
      {
         ret = FALSE;
      }
      else
      {
         if ( strcmp( "left", szAName ) == 0 )
         {
            wMain->iHAlign = XWT_ALIGN_LEFT;
         }
         else if ( strcmp( "center", szAName ) == 0 )
         {
            wMain->iHAlign = XWT_ALIGN_CENTER;
         }
         else if ( strcmp( "right", szAName ) == 0 )
         {
            wMain->iHAlign = XWT_ALIGN_RIGHT;
         }
         else
         {
            ret = FALSE;
         }
      }

      if (ret) {
         xwt_gtk_set_alignment( wMain );
      }
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }

   return ret;
}

BOOL xwt_gtk_align_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_ALIGN* wMain = (XWT_GTK_ALIGN*) widget->widget_data;

   if ( strcmp( prop, "valign" ) == 0 )
   {
      switch( wMain->iVAlign )
      {
         case XWT_ALIGN_TOP: hb_itemPutC( pValue, "top"); break;
         case XWT_ALIGN_CENTER: hb_itemPutC( pValue, "center"); break;
         case XWT_ALIGN_BOTTOM: hb_itemPutC( pValue, "bottom"); break;
         default: ret = FALSE;
      }
   }
   else if ( strcmp( prop, "align" ) == 0 )
   {
      switch( wMain->iHAlign )
      {
         case XWT_ALIGN_LEFT: hb_itemPutC( pValue, "left"); break;
         case XWT_ALIGN_CENTER: hb_itemPutC( pValue, "center"); break;
         case XWT_ALIGN_RIGHT: hb_itemPutC( pValue, "right"); break;
         default: ret = FALSE;
      }
   }
   else
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }

   return ret;
}

BOOL xwt_gtk_align_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   HB_ITEM hbValue;
   XWT_GTK_ALIGN* wMain = (XWT_GTK_ALIGN*) widget->widget_data;

   hbValue.type = HB_IT_NIL;

   if (! xwt_gtk_base_getall( widget, pProps ) )
   {
      return FALSE;
   }

   // Vertical alignment
   switch( wMain->iVAlign )
   {
      case XWT_ALIGN_TOP: hb_itemPutC( &hbValue, "top"); break;
      case XWT_ALIGN_CENTER: hb_itemPutC( &hbValue, "center"); break;
      case XWT_ALIGN_BOTTOM: hb_itemPutC( &hbValue, "bottom"); break;
      default: return FALSE;
   }
   hb_hashAddChar( pProps, "valign", &hbValue );

   // Horizontal alignment
   switch( wMain->iHAlign )
   {
      case XWT_ALIGN_LEFT: hb_itemPutC( &hbValue, "left"); break;
      case XWT_ALIGN_CENTER: hb_itemPutC( &hbValue, "center"); break;
      case XWT_ALIGN_RIGHT: hb_itemPutC( &hbValue, "right"); break;
      default: return FALSE;
   }
   hb_hashAddChar( pProps, "align", &hbValue );


   // clear the item in case it is a complex value;
   // to be removed after developement stage if this is not the case
   hb_itemClear( &hbValue );
   return TRUE;
}

/******************************************************/
void xwt_gtk_set_alignment( XWT_GTK_ALIGN* widget )
{
   GtkWidget *parent;
   GtkWidget *target = widget->INH(main_widget );
   double vpos, hpos;


   switch( widget->iVAlign )
   {
      case XWT_ALIGN_TOP: vpos = 0.0; break;
      case XWT_ALIGN_CENTER: vpos = 0.5; break;
      case XWT_ALIGN_BOTTOM: vpos = 1.0; break;
   }

   switch( widget->iHAlign )
   {
      case XWT_ALIGN_LEFT: hpos = 0.0; break;
      case XWT_ALIGN_CENTER: hpos = 0.5; break;
      case XWT_ALIGN_RIGHT: hpos = 1.0; break;
   }

   if ( widget->align == NULL )
   {
      widget->align = gtk_alignment_new( hpos, vpos, 0.0, 0.0);
      g_object_ref( widget->align );
      parent = gtk_widget_get_parent( target );

      //Moving the new frame to the old parent if necessary
      if ( parent != NULL )
      {
         g_object_ref( target );
         gtk_container_remove( GTK_CONTAINER( parent ) , target );
         gtk_container_add( GTK_CONTAINER( parent ), widget->align );
      }
      gtk_container_add( GTK_CONTAINER( widget->align ), target );
      if ( parent != NULL )
      {
         g_object_unref( target );
      }
      gtk_widget_show( widget->align );
   }
   else
   {
      gtk_alignment_set( GTK_ALIGNMENT( widget->align), hpos, vpos, 0.0, 0.0 );
   }

}
/* end of xwt_gtk_sensible.c */
