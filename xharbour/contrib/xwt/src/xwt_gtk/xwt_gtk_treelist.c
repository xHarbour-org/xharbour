/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_treelist.c,v 1.1 2003/06/05 17:06:22 jonnymind Exp $

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
                  "text",  (GValue *) hb_itemGetCPtr( pItem ), NULL );
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


static void cell_edited(
      GtkWidget *widget, gchar *arg1, gchar *arg2, gpointer cb_data )
{
   PXWT_WIDGET xwtData = (PXWT_WIDGET *) cb_data;
   XWT_GTK_MAKESELF( xwtData->owner );
   // just a test for now
   xwt_rise_event( &Self, XWT_E_CLICKED, 0 );
}

BOOL xwt_gtk_createTreelist( PXWT_WIDGET xwtData )
{
   GtkWidget *treelist;

   treelist = gtk_tree_view_new();
   // add a container to the window

   xwtData->widget_data = treelist;
   xwtData->destructor = NULL;
   xwtData->get_main_widget = xwtData->get_top_widget = xwt_gtk_get_topwidget_neuter;
   gtk_widget_show( treelist );

   return TRUE;
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

static void xwt_gtk_treelist_setdefattr( GtkTreeViewColumn *column )
{
   gtk_tree_view_column_set_clickable( column, TRUE );
   gtk_tree_view_column_set_sizing( column, GTK_TREE_VIEW_COLUMN_AUTOSIZE );
   gtk_tree_view_column_set_resizable( column, TRUE );
   // TODO: How to make it clean?
   //gtk_tree_view_column_set_reorderable( column, TRUE );
}


BOOL xwt_gtk_treelist_create_columns( PXWT_WIDGET xwtData, int nCols )
{
   GtkCellRenderer *renderer;
   GtkTreeViewColumn *column;
   GtkTreeView *tv;
   UINT i, nColi;

   tv = (GtkTreeView *) xwtData->widget_data;
   renderer = gtk_cell_renderer_text_new();

   // todo: remove previously existing columns
   // invisible header for headerless list at the beginning
   gtk_tree_view_set_headers_visible( tv, FALSE );

   for ( i = 0; i < nCols; i ++ )
   {
      renderer = gtk_cell_renderer_text_new();

      g_object_set_data(G_OBJECT (renderer),
         "column", GINT_TO_POINTER( i ) );

      g_object_set(G_OBJECT (renderer),
         "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE,
         NULL);

      g_signal_connect (renderer, "edited", G_CALLBACK (cell_edited), xwtData);

      nColi = gtk_tree_view_insert_column_with_attributes (
            GTK_TREE_VIEW (tv),
            -1,
            "",   // no title in the beginning
            renderer,
            //"text", i,  todo. add attribs
            NULL);

      column = gtk_tree_view_get_column (GTK_TREE_VIEW (tv), nColi - 1);

      xwt_gtk_treelist_setdefattr( column );

      gtk_tree_view_column_set_cell_data_func(
         column,
         renderer,
         xwt_gtk_renderItem,
         GINT_TO_POINTER( i ),
         NULL);

   }

   return TRUE;
}



BOOL xwt_gtk_treelist_set_columns( PXWT_WIDGET xwtData, PHB_ITEM pCols )
{
   GtkTreeViewColumn *column;
   GtkTreeView *tv;
   UINT nLen, i;
   PHB_ITEM pItem;

   tv = (GtkTreeView *) xwtData->widget_data;

   // todo: remove previously existing columns
   nLen = hb_arrayLen( pCols );

   // Now we know some header will be displayed
   gtk_tree_view_set_headers_visible( tv, TRUE );

   for ( i = 0; i < nLen; i ++ )
   {
      pItem = hb_arrayGetItemPtr( pCols, i + 1 );

      column = gtk_tree_view_get_column (GTK_TREE_VIEW (tv), i);

      gtk_tree_view_column_set_title( column, hb_itemGetCPtr( pItem ) );
   }

   return TRUE;
}


BOOL xwt_gtk_treelist_set_colattr( PXWT_WIDGET xwtData, char *prop, void *data )
{
   GtkTreeViewColumn *column;
   GtkTreeView *tv;
   GList *glRenderers;
   GtkCellRenderer *renderer;

   tv = (GtkTreeView *) xwtData->widget_data;

   // Let's see what kind of attribute we have to set
   if (strcmp( prop, "editable" ) == 0 )
   {
      int nCol = GPOINTER_TO_INT( data );
      BOOL bEditable = TRUE;
      if ( nCol < 0 )
      {
         nCol = -nCol;
         bEditable = FALSE;
      }
      nCol--;

      column = gtk_tree_view_get_column (GTK_TREE_VIEW (tv), nCol);
      if ( column == NULL )
      {
         return FALSE;
      }

      glRenderers = gtk_tree_view_column_get_cell_renderers( column );
      renderer = (GtkCellRenderer *)glRenderers->data;
      if ( bEditable )
      {
         g_object_set( G_OBJECT( renderer ),
            "mode", GTK_CELL_RENDERER_MODE_EDITABLE,
            "editable", bEditable,
            NULL);
      }
      else
      {
         g_object_set( G_OBJECT( renderer ),
            "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE,
            "editable", bEditable,
            NULL);
      }

      g_list_free( glRenderers );

      return TRUE;
   }

   return FALSE;
}

