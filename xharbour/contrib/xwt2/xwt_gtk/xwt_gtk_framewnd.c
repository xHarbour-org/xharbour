/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_framewnd.c,v 1.2 2004/05/17 09:27:11 jonnymind Exp $

   GTK interface - Frame window
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hashapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>

static gboolean wnd_evt_destroy( GtkWidget *widget,  GdkEvent  *event, gpointer Self )
{
   if ( ! xwt_rise_event( Self, "destroy", 0 ) )
   {
      hb_objSendMsg( Self, "HIDE", 0 );
      // the object is really destroyed only by the garbage collector
   }
   // event managed
   return TRUE;
}

static BOOL xwt_gtk_frame_destroy( PXWT_WIDGET widget )
{
   PXWT_GTK_FRAMEWND wFrame = (PXWT_GTK_FRAMEWND) widget->widget_data;
   if ( wFrame->status_message != NULL )
   {
      hb_xfree( wFrame->status_message );
   }
   return xwt_gtk_window_destroy( widget );
}

static BOOL xwt_gtk_frame_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_FRAMEWND wFrame = (PXWT_GTK_FRAMEWND) widget->widget_data;
   
   if ( strcmp( prop, "menubar" ) == 0 )
   {
      xwt_gtk_setMenuBar( widget, pValue );
   }
   else if ( strcmp( prop, "statusbar" ) == 0 )
   {   
      if ( wFrame->status_message != NULL )
      {
         hb_xfree( wFrame->status_message );
      }
         
      // COPY the value!
      wFrame->status_message = hb_itemGetC( pValue );
      gtk_statusbar_push( GTK_STATUSBAR(wFrame->status_bar), wFrame->status_context, wFrame->status_message );
   }
   else 
   {
      ret = xwt_gtk_window_setprop( widget, prop, pValue );   
   }
   return ret;
}

static BOOL xwt_gtk_frame_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   PXWT_GTK_FRAMEWND wFrame = (PXWT_GTK_FRAMEWND) widget->widget_data;
   
   if ( strcmp( prop, "statusbar" ) == 0 )
   {
      if ( wFrame->status_message == NULL )
      {
         hb_itemPutCRawStatic( pValue, "", 0 );
      }
      else
      {
         hb_itemPutC( pValue, wFrame->status_message );
      }
   }
   else 
   {
      ret = xwt_gtk_window_getprop( widget, prop, pValue );
   }
   
   return ret;
}

   
static BOOL xwt_gtk_frame_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   PXWT_GTK_FRAMEWND wFrame = (PXWT_GTK_FRAMEWND) widget->widget_data;
   HB_ITEM hbValue;
   
   hbValue.type = HB_IT_NIL;
   
   if ( xwt_gtk_window_getall( widget, pRet ) )
   {
      // for now, use only text
      if ( wFrame->status_message == NULL )
      {
         hb_itemPutCRawStatic( &hbValue, "", 0 );
      }
      else
      {
         hb_itemPutC( &hbValue, wFrame->status_message );
      }
      
      hb_hashAddChar( pRet, "statusbar", &hbValue );
      return TRUE;
   }
   return FALSE;
}

BOOL xwt_gtk_createFrameWindow( PXWT_WIDGET xwtData )
{
   PXWT_GTK_FRAMEWND frame = hb_xgrab( sizeof(XWT_GTK_FRAMEWND) );
   PHB_ITEM pSelf;

   pSelf = xwtData->pOwner;
   frame->INH(window) = gtk_window_new (GTK_WINDOW_TOPLEVEL);
   g_object_ref( G_OBJECT(frame->INH(window)));
   frame->vbox = gtk_vbox_new(FALSE, 0);
   gtk_container_add( GTK_CONTAINER(frame->INH(window)), frame->vbox );
   frame->INH( INH(nId)) = 0;
   frame->INH( INH( fgColor ) ) = NULL;       
   frame->INH( INH( bgColor ) ) = NULL;          
   frame->INH( INH( textColor ) ) = NULL;          
   frame->INH( INH( baseColor ) ) = NULL;          

   frame->INH(INH(top_widget)) = xwt_gtk_window_topwidget;
   frame->INH(connect) = xwt_gtk_window_connect;
   frame->INH(disconnect) = xwt_gtk_window_disconnect;


   /* Now we create the menu bar */
   frame->menu_box = gtk_handle_box_new();
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->menu_box, FALSE, FALSE, 0 );
   gtk_widget_show (frame->menu_box);

   frame->menu_bar = gtk_menu_bar_new ();
   gtk_container_add( GTK_CONTAINER( frame->menu_box ), frame->menu_bar );
   gtk_widget_show( frame->menu_box );
   gtk_widget_show( frame->menu_bar );

   /* Create a void grid of 1,1 as the central widget, for now.
      Inside this central area, many widgets can then find place.*/
   frame->INH(INH(main_widget)) = gtk_table_new( 1, 1, TRUE );
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->INH(INH(main_widget)), TRUE, TRUE, 0 );
   gtk_widget_show(frame->INH(INH(main_widget)));

   /* Create the status bar. */
   frame->status_bar = gtk_statusbar_new();
   gtk_box_pack_start(GTK_BOX(frame->vbox), frame->status_bar, FALSE, FALSE, 0 );
   frame->status_context = gtk_statusbar_get_context_id( GTK_STATUSBAR(frame->status_bar), 
         "Generic XWT-GTK context" );
   frame->status_message = NULL;
   gtk_statusbar_push( GTK_STATUSBAR(frame->status_bar), frame->status_context, "");
   gtk_widget_show(frame->status_bar);

   gtk_widget_show (frame->vbox);

   /* The window destroy event is the only one that can be risen independently by the user,
   so it must be checked and passed to the internal destroy system.
   That system will eventually rise the destroy signal to propagate child auto-destruction.
   Unclean objects will be taken by the gc. */

   // We must send only the internal object pointer. pSelf will be destroyed with stack pop
   g_signal_connect( G_OBJECT(frame->INH(window)), "delete_event",
      G_CALLBACK(wnd_evt_destroy), pSelf );

   // A center position is a generally good default
   gtk_window_set_position( GTK_WINDOW( frame->INH(window)), GTK_WIN_POS_CENTER );
   

   xwtData->widget_data = (void *) frame;
   xwtData->destroy = xwt_gtk_frame_destroy;

   xwtData->set_property = xwt_gtk_frame_setprop;
   xwtData->set_pgroup = xwt_gtk_setpgroup;
   xwtData->get_property = xwt_gtk_frame_getprop;
   xwtData->get_all_properties = xwt_gtk_frame_getall;
   
   xwt_gtk_base_signal_connect( xwtData );

   // add a container to the window
   return TRUE;
}

      
void xwt_gtk_setMenuBar( PXWT_WIDGET xwtData, PHB_ITEM pMenuArray )
{
   PXWT_GTK_FRAMEWND frame;
   GtkWidget *menu;
   PHB_BASEARRAY pBaseArray;
   ULONG ulPos;

   // eventually reset the menu bar
   xwt_gtk_resetMenuBar( xwtData );
   
   if ( HB_IS_NIL( pMenuArray ) )
   {
      // nothing to set
      return;
   }
   
   pBaseArray = pMenuArray->item.asArray.value;
    
   frame = (PXWT_GTK_FRAMEWND) xwtData->widget_data;
   // todo: cancelation of the old bar

   for ( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBaseArray->pItems + ulPos;

      hb_objSendMsg( pMenuItem, "ORAWWIDGET",0 );
      xwtData = (PXWT_WIDGET) HB_VM_STACK.Return.item.asPointer.value;
      menu = ((PXWT_GTK_BASE)xwtData->widget_data)->top_widget( xwtData );
      gtk_menu_bar_append (GTK_MENU_BAR (frame->menu_bar), menu );
   }
}

void xwt_gtk_resetMenuBar( PXWT_WIDGET xwtData )
{
   PXWT_GTK_FRAMEWND frame;
   GtkWidget *menu;
   PHB_BASEARRAY pBaseArray;
   ULONG ulPos;
   PHB_ITEM pMenuArray;

   frame = (PXWT_GTK_FRAMEWND) xwtData->widget_data;
   
   hb_objSendMsg( xwtData->pOwner, "AMENUS", 0 );
   pMenuArray = &(HB_VM_STACK.Return);
   if ( ! HB_IS_ARRAY( pMenuArray ) )
   {
      return;
   }
   
   pBaseArray = pMenuArray->item.asArray.value;
   
   for ( ulPos = 0; ulPos < pBaseArray->ulLen; ulPos++ )
   {
      PHB_ITEM pMenuItem = pBaseArray->pItems + ulPos;
      hb_objSendMsg( pMenuItem, "ORAWWIDGET",0 );
      xwtData = (PXWT_WIDGET) HB_VM_STACK.Return.item.asPointer.value;
      menu = ((PXWT_GTK_BASE)xwtData->widget_data)->top_widget( xwtData );
      g_object_ref( G_OBJECT( menu ) );
      gtk_container_remove (GTK_CONTAINER (frame->menu_bar), menu );        
   }
}
