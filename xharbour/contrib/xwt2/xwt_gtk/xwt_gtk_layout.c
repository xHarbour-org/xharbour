/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_layout.c,v 1.7 2003/08/27 20:09:24 xthefull Exp $

   Layout - Horizontal or vertical layout manager
*/
#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>

/** Layout manager functions */
           
BOOL xwt_gtk_layout_add( PXWT_WIDGET wParent, PXWT_WIDGET wChild )
{
   GtkWidget *gtkChild =  ((PXWT_GTK_BASE) wChild->widget_data)->top_widget( wChild );
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) wParent->widget_data;
   GtkWidget *gtkSelf = ((PXWT_GTK_BASE) wParent->widget_data)->main_widget;
   
   gtk_box_pack_start( GTK_BOX( gtkSelf ), gtkChild, lay->bExpand, lay->bFill, lay->iPadding );
   
   return TRUE;   
}


BOOL xwt_gtk_layout_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) widget->widget_data;
         
   if ( strcmp( prop, "mode" ) == 0 )
   {
      if (HB_IS_STRING( pValue ) )
      {
         char *szVal = hb_itemGetCPtr( pValue );
         if ( strcmp( szVal, "h" ) == 0 || strcmp( szVal, "horizontal" ) == 0 )
         {
            lay->iMode = XWT_LM_HORIZ;
         }
         else if ( strcmp( szVal, "v" ) == 0 || strcmp( szVal, "vertical" ) == 0 )
         {
            lay->iMode = XWT_LM_VERT;
         }
         else
         {
            ret = FALSE;
         }
      }
      else if ( HB_IS_NUMERIC( pValue ) )
      {
         switch( hb_itemGetNI( pValue ) )
         {
            case XWT_LM_HORIZ: lay->iMode = XWT_LM_HORIZ; break;
            case XWT_LM_VERT: lay->iMode = XWT_LM_VERT; break;
            default: ret = FALSE;
         }
      }
      else
      {
         ret = FALSE;
      }
      
      if ( ret ) 
      {
         xwt_gtk_layout_create_with_mode( widget );
      }
   }
   else if ( strcmp( prop, "padding" ) == 0 )
   {
      if ( HB_IS_NUMERIC( pValue ) )
      {
         lay->iPadding = hb_itemGetNI( pValue );
         if ( lay->INH(INH(INH(main_widget))) != NULL )
         {
            xwt_gtk_layout_repack( widget );
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "expand" ) == 0 )
   {
      if ( HB_IS_LOGICAL( pValue ) )
      {
         lay->bExpand = hb_itemGetL( pValue );
         if ( lay->INH(INH(INH(main_widget))) != NULL )
         {
            xwt_gtk_layout_repack( widget );
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "fill" ) == 0 )
   {
      if ( HB_IS_LOGICAL( pValue ) )
      {
         lay->bFill = hb_itemGetL( pValue );
         if ( lay->INH(INH(INH(main_widget))) != NULL )
         {
            xwt_gtk_layout_repack( widget );
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else
   {
      ret = xwt_gtk_container_setprop( widget, prop, pValue );
   }
   
   return ret;
}
                   

BOOL xwt_gtk_layout_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) widget->widget_data;
         
   if ( strcmp( prop, "mode" ) == 0 )
   {
      switch( lay->iMode )
      {
         case XWT_LM_HORIZ: hb_itemPutCRawStatic( pValue, "horizontal", 10 ); break; 
         case XWT_LM_VERT: hb_itemPutCRawStatic( pValue, "vertical", 8 ); break; 
         default: ret = FALSE;
      }
   }
   else if ( strcmp( prop, "padding" ) == 0 )
   {
      hb_itemPutNI( pValue, lay->iPadding );
   }
   else if ( strcmp( prop, "expand" ) == 0 )
   {
      hb_itemPutL( pValue, lay->bExpand);
   }
   else if ( strcmp( prop, "fill" ) == 0 )
   {
      hb_itemPutL( pValue, lay->bFill);
   }
   else
   {
      ret = xwt_gtk_container_setprop( widget, prop, pValue );
   }
   
   return ret;
}

BOOL xwt_gtk_layout_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) widget->widget_data;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   xwt_gtk_container_getall( widget, pProps );
   
   switch( lay->iMode )
   {
      case XWT_LM_HORIZ: hb_itemPutCRawStatic(  &hbValue, "horizontal", 10 ); break; 
      case XWT_LM_VERT: hb_itemPutCRawStatic(  &hbValue, "vertical", 8 ); break; 
      default: hb_itemPutCRawStatic(  &hbValue, "undefined", 9 ); break;
   }
   hb_hashAddChar( pProps, "mode", &hbValue );
   hb_hashAddChar( pProps, "padding", hb_itemPutNI( &hbValue, lay->iPadding )  );
   hb_hashAddChar( pProps, "fill", hb_itemPutL( &hbValue, lay->bFill )  );
   hb_hashAddChar( pProps, "expand", hb_itemPutL( &hbValue, lay->bExpand )  );
   
   return TRUE;
}

BOOL xwt_gtk_createLayout( PXWT_WIDGET xwtData )
{
   // We can't create the widget right now, as we need to know how the widget
   // will be layed (horiz/vert): they are 2 different layout systems
   PXWT_GTK_LAYOUT wLayout;

   wLayout = ( PXWT_GTK_LAYOUT ) hb_xgrab( sizeof( XWT_GTK_LAYOUT ) );
   wLayout->iMode = XWT_LM_VERT; // default VERTICAL

   wLayout->INH(frame) = NULL; // no frame for now
   wLayout->INH(add) = xwt_gtk_layout_add; // layout specific adder
   wLayout->INH(remove) = xwt_gtk_container_remove; // generic container remover
   wLayout->INH(INH(align)) = NULL; // no alignment window for now
   wLayout->INH(INH(iHAlign)) = XWT_ALIGN_CENTER; // defaults to center/center
   wLayout->INH(INH(iVAlign)) = XWT_ALIGN_CENTER; 

   wLayout->INH(INH(INH(main_widget))) = gtk_vbox_new( TRUE, 0 ); 
   wLayout->INH(INH(INH(top_widget))) = xwt_gtk_container_topwidget; // wich is the enframer
   // create also the mandatory aligment widget
   xwt_gtk_set_alignment( (XWT_GTK_ALIGN *) wLayout );

   wLayout->iPadding = 0;
   wLayout->bFill = TRUE;
   wLayout->bExpand = TRUE;

   // no need for destructor, the data is just our widget for now
   xwtData->widget_data = (void *) wLayout;
   xwtData->destroy = xwt_gtk_container_destroy;
   // property handlers
   xwtData->set_property = xwt_gtk_layout_setprop;
   xwtData->get_property = xwt_gtk_layout_getprop;
   xwtData->get_all_properties = xwt_gtk_layout_getall;
   
   // nothing more than the base one
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   
   xwt_gtk_base_signal_connect( xwtData );
   xwt_gtk_base_general_connect( xwtData ); 
   
   return TRUE;
}

/* This function creates the real gtk widget. */
BOOL xwt_gtk_layout_create_with_mode( PXWT_WIDGET wWidget )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) wWidget->widget_data;
   GtkWidget *parent;
   GList *copy;
   
   // Recreation?
   if ( lay->INH(INH(INH(main_widget))) != NULL )
   {
      GList *child = gtk_container_get_children( GTK_CONTAINER( lay->INH(INH(INH(main_widget))) ) );
      copy = g_list_copy( child );
      while ( child != NULL )
      {
         // child widgets are referenced in constructor so they are not self destroyed here
         gtk_container_remove( GTK_CONTAINER( lay->INH(INH(INH(main_widget))) ), GTK_WIDGET( child->data ) );
         child = child->next;
      }
      parent = gtk_widget_get_parent( lay->INH(INH(INH(main_widget))) );
      gtk_widget_destroy( lay->INH(INH(INH(main_widget))) );
   }
   else
   {
      copy = NULL;
      parent = NULL;
   }
      
   if ( lay->iMode == XWT_LM_HORIZ )
   {
      lay->INH(INH(INH(main_widget))) = gtk_hbox_new( FALSE, 0 );
   }
   else
   {
      lay->INH(INH(INH(main_widget))) = gtk_vbox_new( FALSE, 0 );
   }
   // reference the widget
   g_object_ref( lay->INH(INH(INH(main_widget))) );
   gtk_widget_show( lay->INH(INH(INH(main_widget))) );
   
   // readd to parent (usually an align)
   if ( parent != NULL )
   {
      gtk_container_add( GTK_CONTAINER( parent ), lay->INH(INH(INH(main_widget))) );
   }
   
   // have we to reset the widgets in?
   if ( copy != NULL )
   {
      GList *child = copy;
      while ( child != NULL )
      {
         gtk_box_pack_start( GTK_BOX( lay->INH(INH(INH(main_widget))) ), GTK_WIDGET( child->data ), 
            lay->bExpand, lay->bFill, lay->iPadding );
         child = child->next;
      }
      g_list_free( copy );
   }

   return TRUE;
}


/* This changes current packing settings */
BOOL xwt_gtk_layout_repack( PXWT_WIDGET wWidget )
{
   PXWT_GTK_LAYOUT lay = (PXWT_GTK_LAYOUT) wWidget->widget_data;
   GtkWidget *box = lay->INH(INH(INH(main_widget)));
   GList *child;
   
   if ( box == NULL )
   {
      return FALSE;
   }
       
   child = gtk_container_get_children( GTK_CONTAINER( box ));
   while ( child != NULL )
   {
      gtk_box_pack_start( GTK_BOX( box ), GTK_WIDGET( child->data ), 
         lay->bExpand, lay->bFill, lay->iPadding );
      child = child->next;
   }
   
   return TRUE;
}
