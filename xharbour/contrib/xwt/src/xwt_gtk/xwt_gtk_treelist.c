/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_checkbox.c,v 1.1 2003/04/21 06:56:33 jonnymind Exp $

   GTK interface - management of checkbox widget
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void xwt_gtk_renderItem( GtkTreeViewColumn *tree_column,
                                GtkCellRenderer *cell,
                                GtkTreeModel *tree_model,
                                GtkTreeIter *iter,
                                gpointer data)
{
   PHB_ITEM pItem;
   int nCol;
   char buf[32];

   buf[0] = 0;
   nCol = GPOINTER_TO_INT( data );
   gtk_tree_model_get( tree_model, iter, nCol, (gpointer) &(pItem),-1 );

   switch( pItem->type )
   {
      case HB_IT_STRING:
         g_object_set( G_OBJECT( cell ),
                  "text",  (GValue *) hb_itemGetC( pItem ), NULL );
      break;
      case HB_IT_INTEGER:
         sprintf( buf, "%d", pItem->item.asInteger.value );
      break;
      case HB_IT_LONG:
         sprintf( buf, "%ld", pItem->item.asLong.value );
      break;
      case HB_IT_DOUBLE:
         sprintf( buf, "%f", pItem->item.asDouble.value );
      break;
      default:
         sprintf( buf, "Not Impl." );
      break;
   }

   if ( buf[0] != 0 )
   {
         g_object_set( G_OBJECT( cell ), "text", (GValue *) buf, NULL );
   }
}

/*
static void chkb_clicked( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

static void chkb_pressed( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_PRESSED, 0 );
}

static void chkb_released( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_RELEASED, 0 );
}

static void chkb_enter( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_ENTER, 0 );
}

static void chkb_leave( GtkWidget *widget,  gpointer cb_data )
{
   XWT_GTK_MAKESELF( cb_data );
   xwt_rise_event( &Self, XWT_E_LEAVE, 0 );
}


static void chkb_toggled( GtkWidget *widget,  gpointer cb_data )
{
   HB_ITEM lStatus;
   XWT_GTK_MAKESELF( cb_data );

   hb_itemPutL( &lStatus, gtk_toggle_button_get_active( GTK_TOGGLE_BUTTON( widget ) ) );
   xwt_rise_event( &Self, XWT_E_CHANGED, 1, &lStatus );
}
*/

PXWT_WIDGET xwt_gtk_createTreelist( PHB_ITEM pSelf )
{
   GtkWidget *treelist;
   PXWT_WIDGET xwtData;

   treelist = gtk_tree_view_new();
   // add a container to the window

   /*
   g_signal_connect (G_OBJECT(checkbox), "pressed", G_CALLBACK (chkb_pressed), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "released", G_CALLBACK (chkb_released), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "clicked", G_CALLBACK (chkb_clicked), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "enter", G_CALLBACK (chkb_enter), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "leave", G_CALLBACK (chkb_leave), pSelf->item.asArray.value );
   g_signal_connect (G_OBJECT(checkbox), "toggled", G_CALLBACK (chkb_toggled ), pSelf->item.asArray.value );
*/
   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_TREELIST;
   xwtData->widget_data = treelist;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;
   gtk_widget_show( treelist );

   return xwtData;
}

BOOL xwt_gtk_treelist_set_content( PXWT_WIDGET xwtData, PHB_ITEM pContent )
{
   GtkTreeView *tv = (GtkTreeView *) xwtData->widget_data;
   GtkTreeStore *model;
   GtkTreeIter iter;
   PHB_ITEM pData, pItem;
   int nCols, nRows, i, j;

   // Begin creating models
   // If the content is an object, then we have a TreeItem XWT object
   if ( HB_IS_OBJECT( pContent ) )
   {
      do {
         hb_objSendMsg( pContent, "XCONTENT", 0 );
         pData = &HB_VM_STACK.Return;
      } while ( HB_IS_OBJECT( pData ) );
   }
   else if( HB_IS_ARRAY( pContent ) )
   {
      pData = pContent;
   }
   else
   {
      return FALSE;
   }

   //now, if we have a list the first element of our array is NOT an array
   // nor an object
   pItem = hb_arrayGetItemPtr( pData, 1 );
   if ( ! HB_IS_ARRAY( pItem ) )
   {
      // we have a single column list or tree
      nCols = 1;
      model = gtk_tree_store_new( 1, GTK_TYPE_POINTER);
      //now we have our model, lets fill it with data
      nRows = hb_arrayLen( pData );
      for (i = 0; i < nRows; i++ )
      {
         pItem = hb_arrayGetItemPtr( pData, i + 1 );
         gtk_tree_store_append( model, &iter, NULL );
         gtk_tree_store_set( model, &iter, 0, pItem, -1);
      }
   }
   else
   {
      //we have a flat table or a table tree
      GType *gtCols;

      nCols = hb_arrayLen( pItem );
      gtCols = hb_xgrab( sizeof( GType ) * nCols );

      for (i = 0; i < nCols; i++ )
      {
         gtCols[i] = GTK_TYPE_POINTER;
      }

      model = gtk_tree_store_newv( nCols, gtCols );
      hb_xfree( gtCols );

      //now we have our model, lets fill it with data
      nRows = hb_arrayLen( pData );
      for (i = 0; i < nRows; i++ )
      {
         PHB_ITEM pRow = hb_arrayGetItemPtr( pData, i + 1 );
         gtk_tree_store_append( model, &iter, NULL );
         for ( j = 0; j < nCols; j++ )
         {
            pItem = hb_arrayGetItemPtr( pRow, j + 1 );
            gtk_tree_store_set( model, &iter, j, pItem, -1);
         }
      }
   }

   gtk_tree_view_set_model( tv, GTK_TREE_MODEL( model ) );
   return TRUE;
}

BOOL xwt_gtk_treelist_set_columns( PXWT_WIDGET xwtData, PHB_ITEM pCols )
{
   GtkCellRenderer *renderer;
   GtkTreeViewColumn *column;
   GtkTreeView *tv;
   UINT nLen, i, nColi;
   PHB_ITEM pItem;

   tv = (GtkTreeView *) xwtData->widget_data;
   renderer = gtk_cell_renderer_text_new();

   // todo: remove previously existing columns
   nLen = hb_arrayLen( pCols );
   for ( i = 0; i < nLen; i ++ )
   {
      pItem = hb_arrayGetItemPtr( pCols, i + 1 );

      nColi = gtk_tree_view_insert_column_with_attributes (
            GTK_TREE_VIEW (tv),
            -1,
            hb_itemGetC( pItem ),
            renderer,
            //"text", i,  todo. add attribs
            NULL);

      column = gtk_tree_view_get_column (GTK_TREE_VIEW (tv), nColi - 1);

      gtk_tree_view_column_set_cell_data_func(
         column,
         renderer,
         xwt_gtk_renderItem,
         GINT_TO_POINTER( i ),
         NULL);

   }
   return TRUE;
}

