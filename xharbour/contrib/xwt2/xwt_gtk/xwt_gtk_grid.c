/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_grid.c,v 1.5 2003/07/23 15:58:10 lculik Exp $

   Grid - a col/row layout adapter
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>
#include <xwt.ch>

static BOOL xwt_gtk_grid_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_GRID wSelf = (PXWT_GTK_GRID) widget->widget_data;
   GtkWidget *wMain = wSelf->INH(INH(INH( main_widget )));
    
   if ( strcmp( prop, "rows" ) == 0 )
   {
      wSelf->iRows = hb_itemGetNI( pValue );
      gtk_table_resize( GTK_TABLE( wMain ), wSelf->iRows, wSelf->iCols );
   }
   else if ( strcmp( prop, "columns" ) == 0 )
   {
      wSelf->iCols = hb_itemGetNI( pValue );
      gtk_table_resize( GTK_TABLE( wMain ), wSelf->iRows, wSelf->iCols );
   }
   else if ( strcmp( prop, "shrink" ) == 0 )
   {
      wSelf->bShrink = hb_itemGetL( pValue );
   }
   else if ( strcmp( prop, "expand" ) == 0 )
   {
      wSelf->bExpand = hb_itemGetL( pValue );
   }
   else if ( strcmp( prop, "fill" ) == 0 )
   {
      wSelf->bFill = hb_itemGetL( pValue );
   }
   else if ( strcmp( prop, "rowpadding" ) == 0 )
   {
      wSelf->iYPad = hb_itemGetNI( pValue );
   }
   else if ( strcmp( prop, "colpadding" ) == 0 )
   {
      wSelf->iXPad = hb_itemGetNI( pValue );
   }
   else if ( strcmp( prop, "homogeneous" ) == 0 )
   {
      gtk_table_set_homogeneous( GTK_TABLE( wMain ), ( gboolean ) hb_itemGetL( pValue ) ) ;
   }
   else if ( strcmp( prop, "attach" ) == 0 )
   {
      PHB_ITEM pChild = hb_arrayGetItemPtr( pValue, 1 );
      PHB_ITEM pRow = hb_arrayGetItemPtr( pValue, 2 );
      PHB_ITEM pCol = hb_arrayGetItemPtr( pValue, 3 );
      PHB_ITEM pHeight = hb_arrayGetItemPtr( pValue, 4 );
      PHB_ITEM pWidth = hb_arrayGetItemPtr( pValue, 5 );
      PXWT_WIDGET wChild = hb_itemGetPtr( pChild );
      PXWT_GTK_BASE wChildBase = (PXWT_GTK_BASE) wChild->widget_data;

      gtk_table_attach(
         GTK_TABLE( wSelf->INH(INH(INH(main_widget))) ),
         GTK_WIDGET( wChildBase->top_widget( wChild ) ),
         hb_itemGetNI( pCol )-1,
         hb_itemGetNI( pCol )-1 + hb_itemGetNI( pWidth ),
         hb_itemGetNI( pRow )-1,
         hb_itemGetNI( pRow )-1 + hb_itemGetNI( pHeight ),
         (wSelf->bFill << 2) + (wSelf->bShrink << 1) + wSelf->bExpand,
         (wSelf->bFill << 2) + (wSelf->bShrink << 1) + wSelf->bExpand,
         wSelf->iXPad, wSelf->iYPad
      );
   }
   else {
      ret = xwt_gtk_align_setprop( widget, prop, pValue );
   }
   
   return ret;
}

static BOOL xwt_gtk_grid_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_GRID wSelf = (PXWT_GTK_GRID) widget->widget_data;

   if ( strcmp( prop, "rows" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->iRows );
   }
   else if ( strcmp( prop, "columns" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->iCols );
   }
   else if ( strcmp( prop, "shrink" ) == 0 )
   {
      hb_itemPutL( pValue, wSelf->bShrink );
   }
   else if ( strcmp( prop, "expand" ) == 0 )
   {
      hb_itemPutL( pValue, wSelf->bExpand );
   }
   else if ( strcmp( prop, "fill" ) == 0 )
   {
      hb_itemPutL( pValue, wSelf->bFill );
   }
   else if ( strcmp( prop, "rowpadding" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->iYPad );
   }
   else if ( strcmp( prop, "colpadding" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->iXPad );
   }
   else if ( strcmp( prop, "homogeneous") == 0 )
   {
      hb_itemPutL( pValue, gtk_table_get_homogeneous( GTK_TABLE( wSelf->INH(INH(INH(main_widget))) ) ) );
   }
   else {
      ret = xwt_gtk_align_getprop( widget, prop, pValue );
   }
   
   return ret;
}

   
static BOOL xwt_gtk_grid_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   PXWT_GTK_GRID wSelf = (PXWT_GTK_GRID) widget->widget_data;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   xwt_gtk_align_getall( widget, pRet );
   hb_hashAddChar( pRet, "rows", hb_itemPutNI( &hbValue, wSelf->iRows ));
   hb_hashAddChar( pRet, "columns", hb_itemPutNI( &hbValue, wSelf->iCols ));
   hb_hashAddChar( pRet, "colpadding", hb_itemPutNI( &hbValue, wSelf->iXPad ));
   hb_hashAddChar( pRet, "rowpadding", hb_itemPutNI( &hbValue, wSelf->iYPad ));
   hb_hashAddChar( pRet, "shrink", hb_itemPutL( &hbValue, wSelf->bShrink ));
   hb_hashAddChar( pRet, "expand", hb_itemPutL( &hbValue, wSelf->bExpand ));
   hb_hashAddChar( pRet, "fill", hb_itemPutL( &hbValue, wSelf->bFill ));
   hb_hashAddChar( pRet, "homogeneous", hb_itemPutL( &hbValue, 
         gtk_table_get_homogeneous( GTK_TABLE( wSelf->INH(INH(INH(main_widget))) ) )));
   
   return TRUE;
}


BOOL xwt_gtk_createGrid( PXWT_WIDGET xwtData )
{
   PXWT_GTK_GRID grid;

   grid = ( PXWT_GTK_GRID ) hb_xgrab( sizeof( XWT_GTK_GRID ) );
   grid->INH(INH(INH(main_widget))) = gtk_table_new( 1, 1, FALSE );
   g_object_ref( G_OBJECT( grid->INH(INH(INH(main_widget))) ) ); 
   grid->INH(INH(INH(nId))) = 0;
   grid->INH(INH(INH(top_widget))) = xwt_gtk_align_topwidget;
   grid->INH(INH(align)) = NULL; 
   grid->INH(INH(iHAlign)) = XWT_ALIGN_CENTER; 
   grid->INH(INH(iVAlign)) = XWT_ALIGN_TOP; 
   grid->INH(frame) = NULL;
   // grid hasn't a "direct" add function; each ADD is transalted into
   // an "attach" property setting by XWT PRG level
   grid->INH(add) = NULL;
   // but it has a standard remove
   grid->INH(remove) = xwt_gtk_container_remove;
   
   grid->iRows = 1; 
   grid->iCols = 1; 
   grid->iYPad = 0;
   grid->iXPad = 0;
   grid->bFill = FALSE;
   grid->bExpand = TRUE;
   grid->bShrink = TRUE;
   
   // all allignable objects must have a valid alignment window
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) grid );
   
   xwtData->widget_data = (void *)grid;
   xwtData->destroy = xwt_gtk_container_destroy;
   // property handlers
   xwtData->set_property = xwt_gtk_grid_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_grid_getprop;
   xwtData->get_all_properties = xwt_gtk_grid_getall;

   xwt_gtk_base_signal_connect( xwtData );
   gtk_widget_show( grid->INH(INH(INH(main_widget))) );
   
   return TRUE;
}

