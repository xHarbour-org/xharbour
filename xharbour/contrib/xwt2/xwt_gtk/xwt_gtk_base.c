/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_base.c,v 1.2 2004/05/17 09:27:11 jonnymind Exp $

   GTK Base widget for XWT system.
*/

#include "hbapi.h"
#include "hashapi.h"
#include <xwt.ch>
#include <xwt_api.h>
#include <xwt_gtk.h>

/** Generic destructor common to many widgets */

BOOL xwt_gtk_base_destroy( PXWT_WIDGET wWidget )
{
   XWT_GTK_BASE *wBase = (XWT_GTK_BASE *) wWidget->widget_data;
   if ( wBase->main_widget )
   {
      gtk_widget_destroy( wBase->main_widget );
      wBase->main_widget = NULL;
   }

   // managed.
   return TRUE;
}

GtkWidget *xwt_gtk_base_topwidget( PXWT_WIDGET widget )
{
   XWT_GTK_BASE *wBase = (XWT_GTK_BASE *) widget->widget_data;
   return wBase->main_widget;
}

void xwt_gtk_base_recordsize( GtkWidget *widget, GtkAllocation *size, gpointer *gbase )
{
   PXWT_GTK_BASE wBase = (PXWT_GTK_BASE) gbase;
   wBase->x = size->x;
   wBase->y = size->y;
   wBase->width = size->width;
   wBase->height = size->height;
}

static gboolean xwt_gtk_base_btn_manage(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
   HB_ITEM hb_xpos;
   HB_ITEM hb_ypos;
   HB_ITEM hb_state_shift;
   HB_ITEM hb_state_ctrl;
   HB_ITEM hb_state_alt;

   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;

   char *szEvent;

   hb_xpos.type = HB_IT_NIL;
   hb_ypos.type = HB_IT_NIL;
   hb_state_shift.type = HB_IT_NIL;
   hb_state_ctrl.type = HB_IT_NIL;
   hb_state_alt.type = HB_IT_NIL;

   hb_itemPutNI( &hb_xpos, (int) event->x );
   hb_itemPutNI( &hb_ypos, (int) event->y );
   hb_itemPutL(  &hb_state_shift, (( event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK) );
   hb_itemPutL(  &hb_state_ctrl, (( event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) );
   hb_itemPutL(  &hb_state_alt, (( event->state & GDK_MOD1_MASK) == GDK_MOD1_MASK) );

   //creates everything about the event
   switch( event->button )
   {
      case 1:
         if ( event->type == GDK_3BUTTON_PRESS )
         {
            szEvent = "triple-click";
         }
         else if ( event->type == GDK_2BUTTON_PRESS )
         {
            szEvent = "double-click";
         }
         else if ( event->type == GDK_BUTTON_RELEASE )
         {
            szEvent = "release";
         }
         else
         {
            szEvent = "click";
         }
      break;

      case 2:
         if ( event->type == GDK_BUTTON_RELEASE )
         {
            szEvent = "middle-release";
         }
         else
         {
            szEvent = "middle-click";
         }
      break;

      case 3:
         if ( event->type == GDK_BUTTON_RELEASE )
         {
            szEvent = "right-release";
         }
         else
         {
            szEvent = "right-click";
         }
      break;

      case 4:
         if ( event->type == GDK_BUTTON_RELEASE )
         {
            szEvent = "fuorth-release";
         }
         else
         {
            szEvent = "fuorth-click";
         }
      break;

      case 5:
         if ( event->type == GDK_BUTTON_RELEASE )
         {
            szEvent = "fifth-release";
         }
         else
         {
            szEvent = "fifth-click";
         }
      break;
   }

   return xwt_rise_event( pSelf, szEvent, 5, &hb_xpos, &hb_ypos, &hb_state_shift, &hb_state_ctrl, &hb_state_alt );

}



static gboolean xwt_gtk_base_cross(GtkWidget *widget, GdkEventCrossing *event, gpointer user_data)
{
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   HB_ITEM hb_xpos;
   HB_ITEM hb_ypos;

   hb_xpos.type = HB_IT_NIL;
   hb_ypos.type = HB_IT_NIL;

   hb_itemPutNI( &hb_xpos, (int) event->x );
   hb_itemPutNI( &hb_ypos, (int) event->y );

   char *szEvent;

   if ( event->type == GDK_ENTER_NOTIFY )
   {
      szEvent = "enter";
   }
   else
   {
      szEvent = "leave";
   }

   return xwt_rise_event( pSelf, szEvent, 2, &hb_xpos, &hb_ypos );
}



static gboolean xwt_gtk_base_focus(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   char *szEvent;

   if ( event->in )
   {
      szEvent = "got-focus";
   }
   else
   {
      szEvent = "lost-focus";
   }

   return xwt_rise_event( pSelf, szEvent, 0 );
}



static gboolean xwt_gtk_base_key_manage(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   char szVal[5];
   int iHbKey;
   HB_ITEM hb_string;
   HB_ITEM hb_keyval;
   HB_ITEM hb_scancode;
   HB_ITEM hb_state_shift;
   HB_ITEM hb_state_ctrl;
   HB_ITEM hb_state_alt;
   HB_ITEM hb_state_caps;

   hb_string.type = HB_IT_NIL;
   hb_keyval.type = HB_IT_NIL;
   hb_scancode.type = HB_IT_NIL;
   hb_state_shift.type = HB_IT_NIL;
   hb_state_ctrl.type = HB_IT_NIL;
   hb_state_alt.type = HB_IT_NIL;
   hb_state_caps.type = HB_IT_NIL;

   char *szEvent;

   if ( event->type == GDK_KEY_PRESS )
   {
      szEvent = "key-press";
   }
   else
   {
      szEvent = "key-release";
   }

   // until xharbour doesn't support UTF16, it's quite useless to get it.
   // we do the conversion to ascii.
   if ( event->keyval > 26 && event->keyval < 0x128 )
   {
      szVal[0] = event->keyval;
      szVal[1] = '\0';
      iHbKey = event->keyval;

      hb_itemPutCRawStatic( &hb_string, szVal, 1 );
   }
   else
   {
      if ( event->keyval <= 26 )
      {
         iHbKey = event->keyval;
      }
      else
      {
         iHbKey = xwt_gtk_translate_key( event->keyval );
      }

      guint32 unicode = gdk_keyval_to_unicode( event->keyval );
      // What about endianity?
      memcpy( szVal, &unicode, 4 );
      szVal[4] = '\0';

      hb_itemPutCRawStatic( &hb_string, szVal, 4 );
   }

   hb_itemPutNI( &hb_keyval, iHbKey );
   hb_itemPutNI( &hb_scancode, event->hardware_keycode );
   hb_itemPutL(  &hb_state_shift, (( event->state & GDK_SHIFT_MASK) == GDK_SHIFT_MASK) );
   hb_itemPutL(  &hb_state_ctrl, (( event->state & GDK_CONTROL_MASK) == GDK_CONTROL_MASK) );
   hb_itemPutL(  &hb_state_alt, (( event->state & GDK_MOD1_MASK) == GDK_MOD1_MASK) );
   hb_itemPutL(  &hb_state_caps, (( event->state & GDK_LOCK_MASK) == GDK_LOCK_MASK) );

   return xwt_rise_event( pSelf, szEvent, 7, &hb_string, &hb_keyval, &hb_scancode, &hb_state_shift,
                        &hb_state_ctrl, &hb_state_alt, &hb_state_caps );
}


static gboolean xwt_gtk_base_motion_manage(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
   PHB_ITEM pSelf = ((PXWT_WIDGET) user_data)->pOwner;
   HB_ITEM hb_xpos;
   HB_ITEM hb_ypos;

   hb_xpos.type = HB_IT_NIL;
   hb_ypos.type = HB_IT_NIL;

   hb_itemPutNI( &hb_xpos, (int) event->x );
   hb_itemPutNI( &hb_ypos, (int) event->y );


   return xwt_rise_event( pSelf, "motion", 2, &hb_xpos, &hb_ypos );
}



void xwt_gtk_base_signal_connect( PXWT_WIDGET widget )
{
   PXWT_GTK_BASE base = (PXWT_GTK_BASE) widget->widget_data;
   GObject *target = G_OBJECT(base->top_widget( widget ) );

   base->x = 0;
   base->y = 0;
   base->width = -1;
   base->height = -1;
   base->nId = 0;
   base->bBroadcast = TRUE;

   // records GTK size assignment
   g_signal_connect( target, "size-allocate", G_CALLBACK(xwt_gtk_base_recordsize), base );
}

void xwt_gtk_base_general_connect( PXWT_WIDGET widget )
{
   PXWT_GTK_BASE base = (PXWT_GTK_BASE) widget->widget_data;
   GObject *target = G_OBJECT(base->top_widget( widget ) );

   // standard signal connections
   g_signal_connect( target, "button-press-event", G_CALLBACK(xwt_gtk_base_btn_manage), widget);
   g_signal_connect( target, "button-release-event", G_CALLBACK(xwt_gtk_base_btn_manage), widget );
   g_signal_connect( target, "enter-notify-event", G_CALLBACK(xwt_gtk_base_cross), widget );
   g_signal_connect( target, "leave-notify-event", G_CALLBACK(xwt_gtk_base_cross), widget );
   g_signal_connect( target, "key-press-event", G_CALLBACK(xwt_gtk_base_key_manage), widget );
   g_signal_connect( target, "key-release-event", G_CALLBACK(xwt_gtk_base_key_manage), widget );
   g_signal_connect( target, "motion-notify-event", G_CALLBACK(xwt_gtk_base_motion_manage), widget );
   // other notable callbacks.
}

void xwt_gtk_base_focus_connect( PXWT_WIDGET widget )
{
   PXWT_GTK_BASE base = (PXWT_GTK_BASE) widget->widget_data;
   GObject *target = G_OBJECT(base->top_widget( widget ) );

   g_signal_connect( target, "focus-in", G_CALLBACK(xwt_gtk_base_focus), widget );
   g_signal_connect( target, "focus-out", G_CALLBACK(xwt_gtk_base_focus), widget );
}
/****************************************************
* Generic property management
*/

BOOL xwt_gtk_base_setprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_BASE *wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wTop = wSelf->top_widget( widget );
   char *szPropVal;

   if ( strcmp( prop, "x" ) == 0 )
   {
      wSelf->x = hb_itemGetNI(pValue);
      gtk_widget_set_uposition( wTop, wSelf->x , wSelf->y );
   }
   else if ( strcmp( prop, "y" ) == 0 )
   {
      wSelf->y = hb_itemGetNI(pValue);
      gtk_widget_set_uposition( wTop, wSelf->x , wSelf->y );
   }
   else if ( strcmp( prop, "width" ) == 0 )
   {
      wSelf->width = hb_itemGetNI(pValue);
      if ( wSelf->height > 0 )
      {
         gtk_widget_set_size_request( wTop, wSelf->width , wSelf->height );
      }
   }
   else if ( strcmp( prop, "height" ) == 0 )
   {
      wSelf->height = hb_itemGetNI(pValue);
      if ( wSelf->width > 0 )
      {
         gtk_widget_set_size_request( wTop, wSelf->width , wSelf->height );
      }
   }
   else if ( strcmp( prop, "id" ) == 0 )
   {
      wSelf->nId = hb_itemGetNI(pValue);
   }
   else if ( strcmp( prop, "broadcast" ) == 0 )
   {
      wSelf->bBroadcast = hb_itemGetL(pValue);
   }
   else if ( strcmp( prop, "visibility" ) == 0 )
   {
      szPropVal = hb_itemGetCPtr( pValue );
      if( szPropVal == NULL )
      {
         ret = FALSE;
      }
      else if ( strcmp( szPropVal, "normal" ) == 0 )
      {
         gtk_widget_show( wTop );
      }
      else if ( strcmp( szPropVal, "hidden" ) == 0 )
      {
         gtk_widget_hide( wTop );
      }
      else
      {
         ret = FALSE;
      }
   }
   else if ( strcmp( prop, "focus" ) == 0 )
   {
      if( hb_itemGetL( pValue ) )
      {
         gtk_widget_grab_focus( wTop );
      }
      else
      {
         ret = FALSE; // can't just give away focus
      }
   }
   else
   {
      ret = FALSE;
   }

   return ret;
}

BOOL xwt_gtk_base_getprop( PXWT_WIDGET widget, char *prop, PHB_ITEM pValue )
{
   BOOL ret = TRUE;
   XWT_GTK_BASE *wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wTop = wSelf->top_widget( widget );

   if ( strcmp( prop, "x" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->x );
   }
   else if ( strcmp( prop, "x" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->y );
   }
   else if ( strcmp( prop, "width" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->width );
   }
   else if ( strcmp( prop, "height" ) == 0 )
   {
      hb_itemPutNI( pValue, wSelf->height );
   }
   else if ( strcmp( prop, "id" ) == 0 )
   {
      hb_itemPutNI(pValue, wSelf->nId );
   }
   else if ( strcmp( prop, "visibility" ) == 0 )
   {
      if ( GTK_WIDGET_VISIBLE(wTop) )
      {
         hb_itemPutCRawStatic( pValue, "visible", 7 );
      }
      else
      {
         hb_itemPutCRawStatic( pValue, "hidden", 6 );
      }
   }
   else if ( strcmp( prop, "focus" ) == 0 )
   {
      hb_itemPutL( pValue, (BOOL) gtk_widget_is_focus( wTop ) );
   }
   else if ( strcmp( prop, "broadcast" ) == 0 )
   {
      hb_itemPutL( pValue, wSelf->bBroadcast );
   }
   else
   {
      ret = FALSE;
   }

   return ret;
}


BOOL xwt_gtk_base_getall( PXWT_WIDGET widget, PHB_ITEM pRet )
{
   HB_ITEM hbValue;
   XWT_GTK_BASE *wSelf = (PXWT_GTK_BASE) widget->widget_data;
   GtkWidget *wTop = wSelf->top_widget( widget );

   hbValue.type = HB_IT_NIL;

   hb_hashAddChar( pRet, "x", hb_itemPutNI( &hbValue, wSelf->x ) );
   hb_hashAddChar( pRet, "y", hb_itemPutNI( &hbValue, wSelf->y ) );
   hb_hashAddChar( pRet, "width", hb_itemPutNI( &hbValue, wSelf->width ) );
   hb_hashAddChar( pRet, "height", hb_itemPutNI( &hbValue, wSelf->height ) );
   hb_hashAddChar( pRet, "id", hb_itemPutNI( &hbValue, wSelf->nId ) );
   hb_hashAddChar( pRet, "broadcast", hb_itemPutL( &hbValue, wSelf->bBroadcast ) );
   if ( GTK_WIDGET_VISIBLE(wTop) )
   {
      hb_itemPutCRawStatic( &hbValue, "visible", 7 );
   }
   else
   {
      hb_itemPutCRawStatic( &hbValue, "hidden", 6 );
   }
   hb_hashAddChar( pRet, "visibility", &hbValue );
   hb_hashAddChar( pRet, "focus", hb_itemPutL( &hbValue, gtk_widget_is_focus( wTop )) );

   hb_itemClear( &hbValue );

   return TRUE;
}


/** Developement solution: just set ALL properties */
BOOL xwt_gtk_setpgroup( PXWT_WIDGET widget, PHB_ITEM pValue )
{
   ULONG ulPos = 1;
   BOOL ret = TRUE;

   while ( ulPos <= hb_hashLen( pValue ) )
   {
      PHB_ITEM pProp = hb_hashGetKeyAt( pValue, ulPos );
      PHB_ITEM pVal = hb_hashGetValueAt( pValue, ulPos );
      if (! widget->set_property( widget, hb_itemGetCPtr( pProp ), pVal ) )
      {
         ret = FALSE;
      }
      ulPos ++;
   }

   return ret;
}

