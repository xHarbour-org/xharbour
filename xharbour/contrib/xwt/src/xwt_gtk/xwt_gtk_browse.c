/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_treelist.c,v 1.2 2003/06/08 14:05:36 jonnymind Exp $

   GTK interface - browse widget
*/

#include "hbapi.h"
#include "hbvm.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

/******************************************************************
  Part 1:
  Creating a custom model to get the "integer" that correspond
  to the line.
  NOTE: This makes this table unorderable like the original tget.
  I Will think something more intellinent in the future.
******************************************************************/

/* Interface delcaration */
#define TBCOL_TYPE_LIST            (tbcol_list_get_type())
#define TBCOL_LIST(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TBCOL_TYPE_LIST, TBColList))
#define TBCOL_LIST_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass),  TBCOL_TYPE_LIST, TBColListClass))
#define TBCOL_IS_LIST(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TBCOL_TYPE_LIST))
#define TBCOL_IS_LIST_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass),  TBCOL_TYPE_LIST))
#define TBCOL_LIST_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),  TBCOL_TYPE_LIST, TBColListClass))


typedef struct tag_TBColList
{
   GObject         parent;      /* this MUST be the first member */

   guint           num_rows;    /* number of rows that we have   */

   /* The array of codeblock that represent our columns */
   HB_ITEM hbColumns;  /* will receive a PHB_BASEARRAY when ready */
   HB_ITEM hbTBrowse;  /* Pointer to tbrowse that may be used by codeblocks */
   gint n_columns;  /* pre-calculated array lenght */

   gint            stamp;

} TBColList;

typedef struct tag_TBColListClass
{
  GObjectClass parent_class;
} TBColListClass;


/*INTERFACE DECL */
static void tbcol_list_class_init (TBColListClass *klass);
static void tbcol_list_tree_model_init (GtkTreeModelIface *iface);
static void tbcol_list_init( TBColList *pkg_tree);
static void tbcol_list_finalize( GObject *object);
static GtkTreeModelFlags tbcol_list_get_flags( GtkTreeModel *tree_model );
static gint tbcol_list_get_n_columns( GtkTreeModel *tree_model );
static GType tbcol_list_get_column_type( GtkTreeModel *tree_model, gint index);
static gboolean tbcol_list_get_iter (GtkTreeModel *tree_model,
         GtkTreeIter *iter, GtkTreePath       *path);
static GtkTreePath *tbcol_list_get_path(GtkTreeModel *tree_model, GtkTreeIter *iter);
static void tbcol_list_get_value(GtkTreeModel *tree_model, GtkTreeIter *iter,
         gint column, GValue *value);
static gboolean tbcol_list_iter_next (GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean tbcol_list_iter_children(GtkTreeModel *tree_model,
   GtkTreeIter *iter, GtkTreeIter *parent);
static gboolean tbcol_list_iter_has_child(GtkTreeModel *tree_model,
   GtkTreeIter *iter);
static gint tbcol_list_iter_n_children(GtkTreeModel *tree_model, GtkTreeIter *iter);
static gboolean     tbcol_list_iter_nth_child  (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreeIter       *parent,
                                                 gint               n);

static gboolean     tbcol_list_iter_parent     (GtkTreeModel      *tree_model,
                                                 GtkTreeIter       *iter,
                                                 GtkTreeIter       *child);

static GObjectClass *parent_class = NULL;

/***********************************/
GType tbcol_list_get_type (void)
{
  static GType tbcol_list_type = 0;

  if (tbcol_list_type)
    return tbcol_list_type;

  /* Some boilerplate type registration stuff */
  {
    static const GTypeInfo tbcol_list_info =
    {
      sizeof (TBColListClass),
      NULL,                                         /* base_init */
      NULL,                                         /* base_finalize */
      (GClassInitFunc) tbcol_list_class_init,
      NULL,                                         /* class finalize */
      NULL,                                         /* class_data */
      sizeof (TBColList),
      0,                                           /* n_preallocs */
      (GInstanceInitFunc) tbcol_list_init
    };

    static const GInterfaceInfo tree_model_info =
    {
      (GInterfaceInitFunc) tbcol_list_tree_model_init,
      NULL,
      NULL
    };

    tbcol_list_type = g_type_register_static (G_TYPE_OBJECT, "TBColList",
                                               &tbcol_list_info, (GTypeFlags)0);
    g_type_add_interface_static (tbcol_list_type, GTK_TYPE_TREE_MODEL, &tree_model_info);
  }

  return tbcol_list_type;
}



/**** Class initialization */

static void tbcol_list_class_init (TBColListClass *klass)
{
  GObjectClass *object_class;

  parent_class = (GObjectClass*) g_type_class_peek_parent (klass);
  object_class = (GObjectClass*) klass;

  object_class->finalize = tbcol_list_finalize;
}


/**** Model initialization */

static void tbcol_list_tree_model_init (GtkTreeModelIface *iface)
{
  iface->get_flags       = tbcol_list_get_flags;
  iface->get_n_columns   = tbcol_list_get_n_columns;
  iface->get_column_type = tbcol_list_get_column_type;
  iface->get_iter        = tbcol_list_get_iter;
  iface->get_path        = tbcol_list_get_path;
  iface->get_value       = tbcol_list_get_value;
  iface->iter_next       = tbcol_list_iter_next;
  iface->iter_children   = tbcol_list_iter_children;
  iface->iter_has_child  = tbcol_list_iter_has_child;
  iface->iter_n_children = tbcol_list_iter_n_children;
  iface->iter_nth_child  = tbcol_list_iter_nth_child;
  iface->iter_parent     = tbcol_list_iter_parent;
}


/**** Instance (object) initialization */

static void tbcol_list_init (TBColList *tbcol_list )
{
   tbcol_list->num_rows = 0;
   tbcol_list->n_columns = 0;
   tbcol_list->hbColumns.type = HB_IT_NIL;
   tbcol_list->hbTBrowse.type = HB_IT_NIL;

   tbcol_list->stamp = g_random_int();  /* Random int to check whether an iter belongs to our model */
}


/**** Object destructor */
static void tbcol_list_finalize (GObject *object)
{
  /* Currently not allocating any memory */

  /* must chain up - finalize parent */
  (* parent_class->finalize) (object);
}



/**** Just say that we are a list/with persistant iterators */

static GtkTreeModelFlags tbcol_list_get_flags (GtkTreeModel *tree_model)
{
  g_return_val_if_fail (TBCOL_IS_LIST(tree_model), (GtkTreeModelFlags)0);

  return (GTK_TREE_MODEL_LIST_ONLY | GTK_TREE_MODEL_ITERS_PERSIST);
}

/** Get the number of columns */

static gint tbcol_list_get_n_columns (GtkTreeModel *tree_model)
{
   g_return_val_if_fail (TBCOL_IS_LIST(tree_model), 0);
   return TBCOL_LIST(tree_model)->n_columns;
}

/* Column type // retunrs always a string */
static GType tbcol_list_get_column_type (GtkTreeModel *tree_model, gint index)
{
   g_return_val_if_fail (TBCOL_IS_LIST(tree_model), G_TYPE_INVALID);
   g_return_val_if_fail (index < TBCOL_LIST(tree_model)->n_columns && index >= 0, G_TYPE_INVALID);

   return G_TYPE_STRING;
}


/**** Get an interator from a path */
static gboolean
tbcol_list_get_iter (GtkTreeModel *tree_model,
                      GtkTreeIter  *iter,
                      GtkTreePath  *path)
{
  TBColList    *tbcol_list;
  gint         *indices, n, depth;

  g_assert(TBCOL_IS_LIST(tree_model));
  g_assert(path!=NULL);
  g_assert( tbcol_list->hbColumns.type == HB_IT_ARRAY );

  tbcol_list = TBCOL_LIST(tree_model);

  indices = gtk_tree_path_get_indices(path);
  depth   = gtk_tree_path_get_depth(path);

  /* we do not allow children */
  g_assert(depth == 1); /* depth 1 = top level; a list only has top level nodes and no children */

  n = indices[0]; /* the n-th top level row */

  if ( n >= tbcol_list->num_rows || n < 0 )
    return FALSE;


  /* We just store the row number that will be used by the model to
     generate/get the PHB_ITEM from the codeblock return */
  iter->stamp      = tbcol_list->stamp;
  iter->user_data  = GINT_TO_POINTER( n );
  iter->user_data2 = NULL;   /* unused */
  iter->user_data3 = NULL;   /* unused */

  return TRUE;
}


/* Do the reverse: get the path from the iter */
static GtkTreePath * tbcol_list_get_path (GtkTreeModel *tree_model,
                      GtkTreeIter  *iter)
{
  GtkTreePath  *path;
  TBColList   *tbcol_list;

  g_return_val_if_fail (TBCOL_IS_LIST(tree_model), NULL);
  g_return_val_if_fail (iter != NULL,               NULL);
  g_return_val_if_fail (iter->user_data != NULL,    NULL);

  tbcol_list = TBCOL_LIST(tree_model);

  path = gtk_tree_path_new();
  gtk_tree_path_append_index(path, GPOINTER_TO_INT( iter->user_data ));

  return path;
}


/* Gets the value returned by a codeblock */

static void
tbcol_list_get_value (GtkTreeModel *tree_model,
                       GtkTreeIter  *iter,
                       gint          column,
                       GValue       *value)
{
   HB_THREAD_STUB

   PHB_ITEM  pItem;
   PHB_ITEM  pColumn;
   HB_ITEM hbCodeBlock, hbRow;
   TBColList *tbcol_list;
   guint nRow;

   g_return_if_fail (TBCOL_IS_LIST (tree_model));
   g_return_if_fail (iter != NULL);
   tbcol_list = TBCOL_LIST(tree_model);
   g_return_if_fail (tbcol_list->hbColumns.type == HB_IT_ARRAY);
   g_return_if_fail (column < tbcol_list->n_columns);


   g_value_init (value, G_TYPE_STRING);

   nRow =  GPOINTER_TO_INT( iter->user_data );

   if(nRow >= tbcol_list->num_rows)
      g_return_if_reached();

   pColumn = hb_arrayGetItemPtr( &(tbcol_list->hbColumns), column + 1 );
   hbCodeBlock.type = HB_IT_NIL;
   hb_objGetPropValue( pColumn, "BGETITEM", &hbCodeBlock );


   /* Executes the codeblock */
   hbRow.type = HB_IT_NIL;
   hb_itemPutNL( &hbRow, nRow+1 );
   if ( hbCodeBlock.type == HB_IT_ARRAY )
   {
      if ( hb_arrayLen( &hbCodeBlock ) < 4 )
      {
         hb_arraySize( &hbCodeBlock, 4 );
         hb_itemCopy( hb_arrayGetItemPtr( &hbCodeBlock, 3), &tbcol_list->hbTBrowse  );
         hb_itemCopy( hb_arrayGetItemPtr( &hbCodeBlock, 4), pColumn  );
      }
      hb_itemPutNL( hb_arrayGetItemPtr( &hbCodeBlock, 2), nRow + 1 );

      hb_execFromArray( &hbCodeBlock );

   }
   else if ( hbCodeBlock.type == HB_IT_BLOCK )
   {
      hb_vmPushSymbol( &hb_symEval );
      hb_vmPush( &hbCodeBlock );

      hb_vmPush( &hbRow );
      hb_vmPush( &tbcol_list->hbTBrowse );
      hb_vmPush( pColumn );

      hb_vmDo( 3 );
   }
   pItem = hb_stackReturn();

   switch( pItem->type )
   {
      case HB_IT_STRING:
         g_value_set_string( value, hb_itemGetCPtr( pItem ) );
         break;

      default:
         g_value_set_string( value, "INVALID" );
   }
}


/* Next iterator */
static gboolean
tbcol_list_iter_next (GtkTreeModel  *tree_model,
                       GtkTreeIter   *iter)
{
  TBColList    *tbcol_list;
  guint nRow;

  g_return_val_if_fail (TBCOL_IS_LIST (tree_model), FALSE);

  if (iter == NULL )
    return FALSE;

  tbcol_list = TBCOL_LIST(tree_model);

  nRow = GPOINTER_TO_INT( iter->user_data );

  /* Is this the last record in the list? */
  if ((nRow + 1) >= tbcol_list->num_rows)
    return FALSE;

  iter->stamp     = tbcol_list->stamp;
  iter->user_data = GINT_TO_POINTER( nRow+1 );

  return TRUE;
}

/* Iterator position record's child or top element */

static gboolean
tbcol_list_iter_children (GtkTreeModel *tree_model,
                           GtkTreeIter  *iter,
                           GtkTreeIter  *parent)
{
  TBColList    *tbcol_list;

  g_return_val_if_fail (parent == NULL || parent->user_data != NULL, FALSE);

  /* this is a list, nodes have no children */
  if (parent)
    return FALSE;

  /* parent == NULL is a special case; we need to return the first top-level row */

  g_return_val_if_fail (TBCOL_IS_LIST (tree_model), FALSE);

  tbcol_list = TBCOL_LIST(tree_model);

  /* No rows => no first row */
  if (tbcol_list->num_rows == 0)
    return FALSE;

  /* Set iter to first item in list */
  iter->stamp     = tbcol_list->stamp;
  iter->user_data = GINT_TO_POINTER( 0 );

  return TRUE;
}


/* We have no child */

static gboolean tbcol_list_iter_has_child (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter)
{
  return FALSE;
}


/* For a flat list: count of rows */
static gint
tbcol_list_iter_n_children (GtkTreeModel *tree_model,
                             GtkTreeIter  *iter)
{
  TBColList  *tbcol_list;

  g_return_val_if_fail (TBCOL_IS_LIST (tree_model), -1);

  tbcol_list = TBCOL_LIST(tree_model);

  /* special case: if iter == NULL, return number of top-level rows */
  if (!iter)
    return tbcol_list->num_rows;

  return 0; /* otherwise, this is easy again for a list */
}


/* Get the nth child; we don't have children, so we return nth row */

static gboolean tbcol_list_iter_nth_child (GtkTreeModel *tree_model,
                            GtkTreeIter  *iter,
                            GtkTreeIter  *parent,
                            gint          n)
{
  TBColList    *tbcol_list;

  g_return_val_if_fail (TBCOL_IS_LIST (tree_model), FALSE);

  tbcol_list = TBCOL_LIST(tree_model);

  /* a list has only top-level rows */
  if(parent)
    return FALSE;

  /* special case: if parent == NULL, set iter to n-th top-level row */

  if( n >= tbcol_list->num_rows )
    return FALSE;

  iter->stamp = tbcol_list->stamp;
  iter->user_data = GINT_TO_POINTER( n );

  return TRUE;
}


/* We don't have parents */

static gboolean
tbcol_list_iter_parent (GtkTreeModel *tree_model,
                         GtkTreeIter  *iter,
                         GtkTreeIter  *child)
{
  return FALSE;
}


TBColList * tbcol_list_new( PXWT_WIDGET xwtData )
{
   TBColList *newlist;

   newlist = (TBColList*) g_object_new (TBCOL_TYPE_LIST, NULL);
   g_assert( newlist != NULL );

   newlist->hbTBrowse.type = HB_IT_ARRAY;
   newlist->hbTBrowse.item.asArray.value = xwtData->owner;

   newlist->hbColumns.type = HB_IT_NIL;
   hb_objGetPropValue( &(newlist->hbTBrowse),
      "ACOLUMNS",
      &newlist->hbColumns );

   newlist->n_columns = hb_arrayLen( &newlist->hbColumns );
   newlist->num_rows = hb_itemGetNI(
      hb_objGetPropValue( &(newlist->hbTBrowse), "NROWS", NULL ) );

   return newlist;
}

/******************************************************************
  Part 2: XWT widget interface
******************************************************************/


static void cell_edited(
      GtkWidget *widget, gchar *arg1, gchar *arg2, gpointer cb_data )
{
   PXWT_WIDGET xwtData = (PXWT_WIDGET) cb_data;
   HB_ITEM hbColNum, hbRowNum, hbNewData;

   XWT_GTK_MAKESELF( xwtData->owner );

   hbColNum.type = HB_IT_NIL;
   hb_itemPutNL( &hbColNum,
      GPOINTER_TO_INT( g_object_get_data( G_OBJECT(widget), "column_num" )) );
   hbRowNum.type = HB_IT_NIL;
   hb_itemPutNL( &hbRowNum, atol( arg1 )+1 );
   hbNewData.type = HB_IT_NIL;
   hb_itemPutC( &hbNewData, arg2 );

   xwt_rise_event( &Self, XWT_E_UPDATED, 3, &hbColNum, &hbRowNum, &hbNewData);
}

BOOL xwt_gtk_createBrowse( PXWT_WIDGET xwtData )
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

static void xwt_gtk_browse_setdefattr( GtkTreeViewColumn *column )
{
   gtk_tree_view_column_set_clickable( column, TRUE );
   gtk_tree_view_column_set_sizing( column, GTK_TREE_VIEW_COLUMN_AUTOSIZE );
   gtk_tree_view_column_set_resizable( column, TRUE );
   // TODO: How to make it clean?
   //gtk_tree_view_column_set_reorderable( column, TRUE );
}

static BOOL xwt_gtk_browse_create_columns( PXWT_WIDGET xwtData )
{
   GtkCellRenderer *renderer;
   GtkTreeViewColumn *column;
   ULONG i, nColi;
   ULONG nCols;
   GtkTreeView *tv;
   PHB_ITEM pCol, pName;
   HB_ITEM hbData;
   char *szColTitle;

   /* Options */
   BOOL bEditable;
   char *szColor, *szBg, *szCellBg;

   /* Must stay here! */
   XWT_GTK_MAKESELF( xwtData->owner );

   renderer = gtk_cell_renderer_text_new();
   tv = (GtkTreeView *) xwtData->widget_data;

   hbData.type = HB_IT_NIL;
   hb_objGetPropValue( &Self, "ACOLUMNS", &hbData );
   nCols = hb_arrayLen( &hbData );

   for ( i = 0; i < nCols; i ++ )
   {
      renderer = gtk_cell_renderer_text_new();
      pCol = hb_arrayGetItemPtr( &hbData, i+1 );

      g_object_set_data(G_OBJECT (renderer),
         "column_num", GINT_TO_POINTER( i+1 ) );

      /* Return value is not needed anymore */
      bEditable = hb_itemGetL(
            hb_objGetPropValue( pCol, "BEDITABLE", NULL ));

      if ( bEditable )
      {
         g_object_set(G_OBJECT (renderer),
            "mode", GTK_CELL_RENDERER_MODE_EDITABLE,
            "editable", bEditable,
            NULL);
      }
      else
      {
         g_object_set(G_OBJECT (renderer),
            "mode", GTK_CELL_RENDERER_MODE_ACTIVATABLE,
            NULL);
      }

      /* All other parameters */
      szColor = hb_itemGetCPtr(
            hb_objGetPropValue( pCol, "CCOLOR", NULL ));
      szBg = hb_itemGetCPtr(
            hb_objGetPropValue( pCol, "CBACKGROUND", NULL ));
      szCellBg = hb_itemGetCPtr(
            hb_objGetPropValue( pCol, "CHIGHLIGHT", NULL ));

      g_object_set(G_OBJECT (renderer),
            "foreground", szColor,
            "background", szBg,
            "cell-background", szCellBg,
            NULL);

      g_signal_connect (renderer, "edited", G_CALLBACK(cell_edited), xwtData);

      /* get the column name */
      pName = hb_objGetPropValue( pCol, "CHEADING", NULL );
      if ( pName->type == HB_IT_NIL )
      {
         szColTitle = "";
      }
      else
      {
         szColTitle = hb_itemGetCPtr( pName );
      }

      nColi = gtk_tree_view_insert_column_with_attributes (
            GTK_TREE_VIEW (tv),
            -1,
            szColTitle,
            renderer,
            "text", i,
            NULL);

      column = gtk_tree_view_get_column (GTK_TREE_VIEW (tv), nColi - 1);

      xwt_gtk_browse_setdefattr( column );
   }

   return TRUE;
}


BOOL xwt_gtk_browse_set_content( PXWT_WIDGET xwtData )
{
   GtkTreeView *tv = (GtkTreeView *) xwtData->widget_data;
   TBColList *model;

   /* We have only one column object per column */
   model = tbcol_list_new( xwtData );
   xwt_gtk_browse_create_columns( xwtData );

   gtk_tree_view_set_model( tv, GTK_TREE_MODEL( model ) );
   return TRUE;
}

