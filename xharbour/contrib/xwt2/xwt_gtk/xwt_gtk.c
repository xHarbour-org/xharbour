/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.c,v 1.1 2004/05/11 15:03:29 jonnymind Exp $

   Global declarations, common functions

  2003-08-25 Luiz Rafael Culik Guimaraes
     Color and Font support

*/

#include <hbapi.h>
#include <hashapi.h>
#include <hbinit.h>
#include <xwt_gtk.h>
#include <xwt_api.h>
#include <xwt.ch>
#include <inkey.ch>
#include <gdk/gdkkeysyms.h>

/***********************************************************/
int xwt_gtk_translate_type( int iType )
{
   switch( iType )
   {
      case HB_IT_INTEGER: return GTK_TYPE_INT;
      case HB_IT_LONG: return GTK_TYPE_LONG;
      case HB_IT_DOUBLE: return GTK_TYPE_DOUBLE;
      case HB_IT_LOGICAL: return GTK_TYPE_BOOL;
      case HB_IT_STRING: return GTK_TYPE_STRING;
   }
   return GTK_TYPE_INVALID;
}

/***********************************************************/
int xwt_gtk_translate_key( unsigned int keyval )
{
   if ( keyval < 0x128 )
   {
      return keyval;
   }
   
   switch( keyval ) 
   {
      case GDK_F1: return K_F1;
      case GDK_F2: return K_F2;
      case GDK_F3: return K_F3;
      case GDK_F4: return K_F4;
      case GDK_F5: return K_F5;
      case GDK_F6: return K_F6;
      case GDK_F7: return K_F7;
      case GDK_F8: return K_F8;
      case GDK_F9: return K_F9;
      case GDK_F10: return K_F10;
      case GDK_F11: return K_F11;
      case GDK_F12: return K_F12;
      case GDK_Escape: return K_ESC;
      case GDK_Delete: return K_DEL;
      case GDK_Insert: return K_INS;
      case GDK_Return: return K_RETURN;
      
      case GDK_Home: return K_HOME;
      case GDK_End: return K_END;
      case GDK_Left: return K_LEFT;
      case GDK_Up: return K_UP;
      case GDK_Right: return K_RIGHT;
      case GDK_Down: return K_DOWN;
      case GDK_Page_Up: return K_PGUP;
      case GDK_Page_Down: return K_PGDN;
   }
      
   // no equivalent clipper key
   return 0;
}

/***********************************************************
* Coordinate retrival utility
*/
BOOL xwt_gtk_getxy( GtkWidget *wTop, int *xpos, int *ypos )
{
   GtkWidget *wParent = gtk_widget_get_parent( wTop );
   GList *pList;
   
   if( wParent != 0 && strcmp( "GtkFixed", G_OBJECT_TYPE_NAME( wParent )) == 0 )
   {
      // find ourselves among the children and get the x/y
      pList = GTK_FIXED(wParent)->children;
      while ( pList != NULL )
      {
         if( pList->data == wTop )
         {
            *xpos = ((GtkFixedChild *) pList->data)->x;
            *ypos = ((GtkFixedChild *) pList->data)->y;
            return TRUE;
         }
         pList = pList->next;
      }
   }
   
   return FALSE;
}


/***************************************
* Driver declaration
*/

static BOOL xwt_gtk_init( int argc, char **argv )
{
   gtk_init( &argc, &argv );
   /* Setting up idle system */
   g_idle_add( xwt_idle_function, NULL );
   return TRUE;
}

static BOOL xwt_gtk_process_events()
{
   gtk_main ();
   return TRUE;
}

static BOOL xwt_gtk_quit()
{
   gtk_main_quit ();
   return TRUE;
}

static void xwt_gtk_modal( PXWT_WIDGET widget )
{
   PXWT_GTK_MODAL wSelf = ( (PXWT_GTK_MODAL) widget->widget_data );

   // this flag will be changed by widget callbacks
   wSelf->modal = TRUE;

   while( wSelf->modal )
   {
      gtk_main_iteration();
   }
}

static BOOL xwt_gtk_create( PXWT_WIDGET xwtData )
{
   switch( xwtData->type )
   {
      case XWT_TYPE_FRAME:   return xwt_gtk_createFrameWindow( xwtData );
      case XWT_TYPE_LABEL:   return xwt_gtk_createLabel( xwtData );
      case XWT_TYPE_LAYOUT:  return xwt_gtk_createLayout( xwtData );
      case XWT_TYPE_TEXTBOX: return xwt_gtk_createTextbox( xwtData );
      case XWT_TYPE_BUTTON:  return xwt_gtk_createButton( xwtData );
      case XWT_TYPE_WINDOW:  return xwt_gtk_createWindow( xwtData );
      case XWT_TYPE_MENU:    return xwt_gtk_createMenu( xwtData );
      case XWT_TYPE_MENUITEM:return xwt_gtk_createMenuItem( xwtData );
      case XWT_TYPE_GRID:    return xwt_gtk_createGrid( xwtData );
      case XWT_TYPE_VIEWPORT:    return xwt_gtk_createViewPort( xwtData );
      /*case XWT_TYPE_IMAGE:   return xwt_gtk_createImage( xwtData );
      case XWT_TYPE_PANE:    return xwt_gtk_createPane( xwtData );
      
      case XWT_TYPE_RADIOBUTTON: return xwt_gtk_createRadioButton( xwtData );
      case XWT_TYPE_CHECKBOX:    return xwt_gtk_createCheckbox( xwtData );
      case XWT_TYPE_FILESEL:     return xwt_gtk_createFileSelection( xwtData );
      case XWT_TYPE_SPLITTER:    return xwt_gtk_createSplitter( xwtData );
      case XWT_TYPE_TOGGLEBUTTON: return xwt_gtk_createToggleButton( xwtData );
      case XWT_TYPE_TREELIST:    return xwt_gtk_createTreelist( xwtData );
      case XWT_TYPE_BROWSE:      return xwt_gtk_createBrowse( xwtData );
      case XWT_TYPE_FONTSEL:     return xwt_gtk_createFontSelection( xwtData );
      case XWT_TYPE_CALENDAR:    return xwt_gtk_createCalendar(xwtData);
      case XWT_TYPE_CALENDARM:   return xwt_gtk_createCalendarModal(xwtData);
      case XWT_TYPE_COMBOBOX:    return xwt_gtk_createComboBox(xwtData);
      case XWT_TYPE_LISTBOX:     return xwt_gtk_createListBox( xwtData );
      case XWT_TYPE_COLORSELECT: return xwt_gtk_createColorSelection( xwtData );
      case XWT_TYPE_PROGRESSBAR: return xwt_gtk_createProgressBar( xwtData );
      */
   }
   return FALSE;
}


BOOL xwt_gtk_add( PXWT_WIDGET wWidget, PXWT_WIDGET wChild )
{
   PXWT_GTK_BASE wChildBase = (PXWT_GTK_BASE) wChild->widget_data;
   GtkWidget *gtkChild = wChildBase->top_widget( wChild );
   PXWT_GTK_BASE wParentBase = (PXWT_GTK_BASE) wWidget->widget_data;
   GtkWidget *gtkSelf = wParentBase->main_widget;
   
   switch( wWidget->type )
   {
      case XWT_TYPE_PANE:
         gtk_fixed_put( GTK_FIXED( gtkSelf ), gtkChild, 0, 0 );
      break;

      case XWT_TYPE_MENU:
         gtk_menu_shell_append (GTK_MENU_SHELL(gtkSelf), gtkChild );
      break;

      default:
      {
         PXWT_GTK_CONTAINER container = ( PXWT_GTK_CONTAINER ) wWidget->widget_data;
         container->add( wWidget, wChild );
      }
      break;

   }
   return TRUE;
}


BOOL xwt_gtk_remove( PXWT_WIDGET wWidget, PXWT_WIDGET wChild )
{
   PXWT_GTK_BASE wChildBase = (PXWT_GTK_BASE) wChild->widget_data;
   GtkWidget *gtkChild = wChildBase->top_widget( wChild );
   PXWT_GTK_BASE wParentBase = (PXWT_GTK_BASE) wWidget->widget_data;
   GtkWidget *gtkSelf = wParentBase->top_widget( wWidget );
   
   // removal of the child object should NOT cause it's destruction
   g_object_ref( G_OBJECT( gtkChild ) );
   gtk_container_remove( GTK_CONTAINER( gtkSelf ), gtkChild );

   if ( wWidget->type == XWT_TYPE_VIEWPORT )
   {
      PXWT_GTK_WND vp = ( PXWT_GTK_WND ) wWidget->widget_data;
      vp->INH( main_widget )= NULL;
   }

   return TRUE;
}

BOOL xwt_gtk_connect( PXWT_WIDGET wWidget, PXWT_WIDGET wChild )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) wWidget->widget_data;
   
   return wnd->connect( wWidget, wChild );
}

BOOL xwt_gtk_disconnect( PXWT_WIDGET wWidget )
{
   PXWT_GTK_WND wnd = (PXWT_GTK_WND) wWidget->widget_data;
   
   return wnd->disconnect( wWidget );
}

/****************************************************
* Idle function
*/

gboolean xwt_idle_function( gpointer data )
{
   hb_idleState();
   return TRUE;
}

/****************************************************************************+
* Gtk driver registration
*/
static XWT_DRIVER s_gtkDriver;

XWT_MODULE_ANNOUNCE( GTK );

HB_CALL_ON_STARTUP_BEGIN( _xwt_register_gtk )
   
   sprintf( s_gtkDriver.name, "gtk" );
   s_gtkDriver.init = xwt_gtk_init;
   s_gtkDriver.quit = xwt_gtk_quit;
   s_gtkDriver.process_events = xwt_gtk_process_events;
   s_gtkDriver.do_modal = xwt_gtk_modal;
   s_gtkDriver.create = xwt_gtk_create;
   s_gtkDriver.message_box = xwt_gtk_message_box;
   s_gtkDriver.add = xwt_gtk_add;
   s_gtkDriver.remove = xwt_gtk_remove;
   s_gtkDriver.connect = xwt_gtk_connect;
   s_gtkDriver.disconnect = xwt_gtk_disconnect;
    
   xwt_register_driver( &s_gtkDriver );
   
HB_CALL_ON_STARTUP_END( _xwt_register_gtk )

