/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_textarea.c,v 1.1 2004/05/20 15:41:38 jonnymind Exp $

   Text Area - Advanced editor
*/
#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static void view_changed( GtkTextBuffer *buffer, gpointer user_data )
{
   HB_ITEM hbNid;
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   PXWT_GTK_TEXTAREA ta = (PXWT_GTK_TEXTAREA) ((PXWT_WIDGET) user_data)->widget_data;

   hbNid.type = HB_IT_NIL;
   hb_itemPutNI( &hbNid,ta->INH(nId) );

   xwt_rise_event( pSelf,"changed", 1, &hbNid );
}


static GtkWidget *xwt_gtk_textarea_topwidget( PXWT_WIDGET widget )
{
   PXWT_GTK_TEXTAREA wSelf = (PXWT_GTK_TEXTAREA) widget->widget_data;
   return GTK_WIDGET(wSelf->frame);
}

static BOOL xwt_gtk_textarea_destroy( PXWT_WIDGET widget )
{
   PXWT_GTK_TEXTAREA wSelf = (PXWT_GTK_TEXTAREA) widget->widget_data;
   gtk_widget_destroy( GTK_WIDGET( wSelf->frame ) );
   return TRUE;
}

static BOOL xwt_gtk_textarea_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_TEXTAREA *wSelf = (PXWT_GTK_TEXTAREA) widget->widget_data;

   if ( strcmp( prop, "text" ) == 0)
   {
      gtk_text_buffer_set_text( wSelf->textbuf, hb_itemGetCPtr( pValue ), -1 );
   }
   else if ( strcmp( prop, "row" ) == 0)
   {
      int nRow = hb_itemGetNI( pValue ) -1;
      GtkTextIter tiPos, tiCursor;
      GtkTextMark *cursor;
      int curCol, maxCol;

      if ( nRow > 0 && nRow <= gtk_text_buffer_get_line_count(wSelf->textbuf) )
      {
         cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
         gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
         curCol = gtk_text_iter_get_line_offset( &tiCursor );
         gtk_text_buffer_get_iter_at_line( wSelf->textbuf, &tiPos, nRow );
         maxCol = gtk_text_iter_get_chars_in_line( &tiPos );
         if ( curCol < maxCol )
         {
            gtk_text_iter_set_line_offset( &tiPos, curCol );
         }
         else if ( maxCol > 0 )
         {
            gtk_text_iter_set_line_offset( &tiPos, maxCol -1 );
         }
         gtk_text_buffer_place_cursor( wSelf->textbuf, &tiPos );
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "text-slice" ) == 0)
   {
      GtkTextIter startIter, endIter;
      int nStart, nEnd, nMax;

      // retreive the required slice
      hb_objSendMsg( widget->pOwner, "NSLICESTART", 0 );
      nStart = hb_itemGetNI( &HB_VM_STACK.Return );
      hb_objSendMsg( widget->pOwner, "NSLICEEND", 0 );
      nEnd = hb_itemGetNI( &HB_VM_STACK.Return );
      gtk_text_buffer_get_end_iter( wSelf->textbuf, &endIter );
      nMax = gtk_text_iter_get_offset( &endIter );
      if ( nStart > nEnd || nStart < 0 || nEnd > nMax )
      {
         ret = FALSE;
      }
      else
      {
         gtk_text_iter_set_offset( &endIter, nEnd);
         gtk_text_buffer_get_end_iter( wSelf->textbuf, &startIter );
         gtk_text_iter_set_offset( &startIter, nStart);
         gtk_text_buffer_delete( wSelf->textbuf, &startIter, &endIter );
         gtk_text_buffer_insert( wSelf->textbuf, &startIter, hb_itemGetCPtr( pValue ), -1 );
      }
   }
   else if ( strcmp( prop, "column" ) == 0)
   {
      int nCol = hb_itemGetNI( pValue ) -1;
      GtkTextIter tiCursor;
      GtkTextMark *cursor;
      int maxCol;

      if ( nCol >= 0 )
      {
         cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
         gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
         maxCol = gtk_text_iter_get_chars_in_line( &tiCursor );
         if ( nCol < maxCol )
         {
            gtk_text_iter_set_line_offset( &tiCursor, nCol );
            gtk_text_buffer_place_cursor( wSelf->textbuf, &tiCursor );
         }
         else
         {
            ret = FALSE;
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "position" ) == 0 )
   {
      int nPos = hb_itemGetNI( pValue ) -1;
      int maxPos;
      GtkTextIter tiCursor;

      if ( nPos >= 0 )
      {
         maxPos = gtk_text_buffer_get_char_count( wSelf->textbuf );
         if ( nPos < maxPos )
         {
            gtk_text_buffer_get_start_iter(  wSelf->textbuf, &tiCursor );
            gtk_text_iter_set_offset( &tiCursor, nPos);
            gtk_text_buffer_place_cursor( wSelf->textbuf, &tiCursor );
         }
         else
         {
            ret = FALSE;
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "selection-end" ) == 0)
   {
      int nPos = hb_itemGetNI( pValue ) -1;
      int maxPos;
      GtkTextIter tiCursor;
      GtkTextMark *selEnd;

      if ( nPos >= 0 )
      {
         maxPos = gtk_text_buffer_get_char_count( wSelf->textbuf );
         if ( nPos < maxPos )
         {
            gtk_text_buffer_get_start_iter(  wSelf->textbuf, &tiCursor );
            selEnd = gtk_text_buffer_get_mark( wSelf->textbuf, "selection_bound" );
            gtk_text_iter_set_offset( &tiCursor, nPos);
            gtk_text_buffer_move_mark( wSelf->textbuf, selEnd, &tiCursor );
         }
         else
         {
            ret = FALSE;
         }
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "editable" ) == 0)
   {
      gtk_text_view_set_editable( GTK_TEXT_VIEW(wSelf->INH( main_widget )), hb_itemGetL( pValue ) );
   }
   else
   {
      ret = xwt_gtk_base_setprop( widget, prop, pValue );
   }

   return ret;
}



static BOOL xwt_gtk_textarea_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_TEXTAREA *wSelf = (PXWT_GTK_TEXTAREA) widget->widget_data;

   if ( strcmp( prop, "text" ) == 0)
   {
      GtkTextIter beg, end;
      char *txt;

      gtk_text_buffer_get_start_iter(  wSelf->textbuf, &beg );
      gtk_text_buffer_get_end_iter(  wSelf->textbuf, &end );

      txt = gtk_text_buffer_get_text( wSelf->textbuf, &beg, &end, FALSE );
      // we can't use this newly allocated memory, as it is not allocated with hb_xgrab
      hb_itemPutC( pValue, txt );
      free( txt );
   }
   else if ( strcmp( prop, "row" ) == 0 )
   {
      GtkTextIter tiCursor;
      GtkTextMark *cursor;

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
      hb_itemPutNI( pValue, gtk_text_iter_get_line( &tiCursor ) + 1);
   }
   else if ( strcmp( prop, "column" ) == 0 )
   {
      GtkTextIter tiCursor;
      GtkTextMark *cursor;

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
      hb_itemPutNI( pValue, gtk_text_iter_get_line_offset( &tiCursor ) + 1);
   }
   else if ( strcmp( prop, "position" ) == 0 )
   {
      GtkTextIter tiCursor;
      GtkTextMark *cursor;

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
      hb_itemPutNI( pValue, gtk_text_iter_get_offset( &tiCursor ) +1 );
   }
   else if ( strcmp( prop, "text-slice" ) == 0)
   {
      GtkTextIter startIter, endIter;
      char *txt;
      int nStart, nEnd, nMax;

      // retreive the required slice
      hb_objSendMsg( widget->pOwner, "NSLICESTART", 0 );
      nStart = hb_itemGetNI( &HB_VM_STACK.Return );
      hb_objSendMsg( widget->pOwner, "NSLICEEND", 0 );
      nEnd = hb_itemGetNI( &HB_VM_STACK.Return );
      gtk_text_buffer_get_end_iter( wSelf->textbuf, &endIter );
      nMax = gtk_text_iter_get_offset( &endIter );

      if ( nStart > nEnd || nStart < 0 || nEnd > nMax )
      {
         ret = FALSE;
      }
      else
      {
         gtk_text_iter_set_offset( &endIter, nEnd);
         gtk_text_buffer_get_end_iter( wSelf->textbuf, &startIter );
         gtk_text_iter_set_offset( &startIter, nStart);
         txt = gtk_text_buffer_get_text( wSelf->textbuf, &startIter, &endIter, FALSE );
         // we can't use this newly allocated memory, as it is not allocated with hb_xgrab
         hb_itemPutC( pValue, txt );
         free( txt );
      }
   }
   else if ( strcmp( prop, "selection-end" ) == 0 )
   {
      GtkTextIter tiCursor;
      GtkTextMark *cursor;

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "selection_bound" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
      hb_itemPutNI( pValue, gtk_text_iter_get_offset( &tiCursor ) +1 );
   }
   else if ( strcmp( prop, "modified" ) == 0)
   {
      hb_itemPutL( pValue, gtk_text_buffer_get_modified( wSelf->textbuf ) );
   }
   else if ( strcmp( prop, "editable" ) == 0)
   {
      hb_itemPutL( pValue, gtk_text_view_get_editable( GTK_TEXT_VIEW( wSelf->INH( main_widget ) ) ));
   }
   else
   {
      ret = xwt_gtk_base_getprop( widget, prop, pValue );
   }

   return ret;
}


static BOOL xwt_gtk_textarea_getall( PXWT_WIDGET widget, PHB_ITEM pProps )
{
   HB_ITEM hbValue;
   XWT_GTK_TEXTAREA *wSelf = (PXWT_GTK_TEXTAREA) widget->widget_data;

   hbValue.type = HB_IT_NIL;

   if ( xwt_gtk_base_getall( widget, pProps ) )
   {
      GtkTextIter tiCursor, beg, end;
      GtkTextMark *cursor;
      char *txt;

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "insert" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );

      gtk_text_buffer_get_start_iter(  wSelf->textbuf, &beg );
      gtk_text_buffer_get_end_iter(  wSelf->textbuf, &end );

      txt = gtk_text_buffer_get_text( wSelf->textbuf, &beg, &end, FALSE );
      // we can't use this newly allocated memory, as it is not allocated with hb_xgrab
      hb_itemPutC( &hbValue, txt );
      hb_hashAddChar( pProps, "text", &hbValue );
      free( txt );

      hb_itemPutNI( &hbValue, gtk_text_iter_get_line( &tiCursor ) + 1);
      hb_hashAddChar( pProps, "row", &hbValue );
      hb_itemPutNI( &hbValue, gtk_text_iter_get_line_offset( &tiCursor ) + 1);
      hb_hashAddChar( pProps, "column", &hbValue );
      hb_itemPutNI( &hbValue, gtk_text_iter_get_offset( &tiCursor ) + 1);
      hb_hashAddChar( pProps, "position", &hbValue );
      hb_itemPutL( &hbValue, gtk_text_buffer_get_modified( wSelf->textbuf ) );

      cursor = gtk_text_buffer_get_mark( wSelf->textbuf, "selection_bound" );
      gtk_text_buffer_get_iter_at_mark( wSelf->textbuf, &tiCursor, cursor );
      hb_itemPutNI( &hbValue, gtk_text_iter_get_offset( &tiCursor ) +1 );
      hb_hashAddChar( pProps, "selection-end", &hbValue );

      hb_hashAddChar( pProps, "modified", &hbValue );
      hb_itemPutL( &hbValue, gtk_text_view_get_editable( GTK_TEXT_VIEW(wSelf->INH( main_widget )) ));
      hb_hashAddChar( pProps, "editable", &hbValue );

      return TRUE;
   }

   return FALSE;
}


BOOL xwt_gtk_createTextarea( PXWT_WIDGET xwtData )
{
   PXWT_GTK_TEXTAREA widget;
   GtkWidget *view;

   view = gtk_text_view_new();
   gtk_widget_show( view );

   widget = (PXWT_GTK_TEXTAREA) hb_xgrab( sizeof( XWT_GTK_TEXTAREA ) );

   widget->textbuf = gtk_text_view_get_buffer( GTK_TEXT_VIEW( view ) );
   widget->frame = GTK_SCROLLED_WINDOW (gtk_scrolled_window_new( NULL, NULL ));
   gtk_scrolled_window_set_policy ( widget->frame, GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC );
   gtk_container_add (GTK_CONTAINER (widget->frame), view);
   g_object_ref( G_OBJECT( widget->frame ) );
   gtk_widget_show( GTK_WIDGET(widget->frame) );
   widget->INH( main_widget ) = view;
   widget->INH( top_widget ) = xwt_gtk_textarea_topwidget;
   widget->INH( nId ) = 0;

   xwtData->widget_data = widget;
   xwtData->destroy = xwt_gtk_textarea_destroy;
   xwtData->set_property = xwt_gtk_textarea_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_textarea_getprop;
   xwtData->get_all_properties = xwt_gtk_textarea_getall;

   xwt_gtk_base_signal_connect( xwtData );
   xwt_gtk_base_general_connect( xwtData );

   g_signal_connect (G_OBJECT(widget->textbuf), "changed", G_CALLBACK (view_changed), xwtData );

   return TRUE;
}
