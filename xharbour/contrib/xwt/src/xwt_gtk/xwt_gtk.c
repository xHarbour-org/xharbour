/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.c,v 1.3 2003/04/07 15:41:07 jonnymind Exp $

   Global declarations, common functions
*/

#include <hbapi.h>
#include <xwt_gtk.h>
#include <xwt_api.h>

BOOL xwt_drv_set_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   GtkWidget *wSelf = GTK_WIDGET( wWidget->get_top_widget( wWidget->widget_data ));

   switch( prop->type )
   {
      case XWT_PROP_FOCUS:
         // Setting focus to false has no sense
         if ( prop->value.setting && GTK_WIDGET_CAN_FOCUS( wSelf ) )
         {
            gtk_widget_grab_focus( wSelf );
            return TRUE;
         }
      return FALSE;


      case XWT_PROP_EDITABLE:
      //TODO
      return FALSE;
      case XWT_PROP_VISIBLE:
      //TODO
      return FALSE;

      case XWT_PROP_FIXED:
      //TODO
      return FALSE;

      case XWT_PROP_MODAL:
         if (
               wWidget->type == XWT_TYPE_WINDOW
               //TODO: other modal widgets
            )
         {
            gtk_window_set_modal( GTK_WINDOW( wSelf ),
               (gboolean) prop->value.setting );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_POSITION:
      {
         GtkWidget *wParent = gtk_widget_get_parent( wSelf );

         if( wParent == 0 )
         {
            if(wWidget->type == XWT_TYPE_FRAME || wWidget->type == XWT_TYPE_WINDOW)
            {
               gtk_widget_set_uposition( wSelf,
                  prop->value.position.x, prop->value.position.y );
               return TRUE;
            }
            return FALSE;
         }

         /* moving a widget in GTK is a complicate matter. We can do it ONLY from within
            a "FIXED" pane. */
         if( strcmp( "GtkFixed", G_OBJECT_TYPE_NAME( wParent )) == 0 )
         {
            gtk_fixed_move( GTK_FIXED(wParent), wSelf,
               prop->value.position.x, prop->value.position.y );
            return TRUE;
         }
      }
      return FALSE;

      case XWT_PROP_SELREGION:
      //TODO
      break;

      //Size parameter
      case XWT_PROP_SIZE:
         if(
            wWidget->type == XWT_TYPE_FRAME ||
            wWidget->type == XWT_TYPE_WINDOW
         )
         {
            gtk_window_resize( GTK_WINDOW( wSelf ) ,
               prop->value.size.width, prop->value.size.height );
         }
         else
         {
            gtk_widget_set_size_request( wSelf,
               prop->value.size.width, prop->value.size.height );
         }
      return TRUE;

      //Text parameters
      case XWT_PROP_TEXT:
         switch( wWidget->type )
         {
            case XWT_TYPE_WINDOW:
            case XWT_TYPE_FRAME:
               gtk_window_set_title (GTK_WINDOW(wSelf), prop->value.text );
            return TRUE;

            case XWT_TYPE_BUTTON:
               gtk_button_set_label (GTK_BUTTON(wSelf), prop->value.text );
            return TRUE;

            case XWT_TYPE_LABEL:
               gtk_label_set_label (GTK_LABEL(wSelf), prop->value.text );
            return TRUE;

            case XWT_TYPE_TEXTBOX:
               gtk_entry_set_text (GTK_ENTRY(wSelf), prop->value.text );
            return TRUE;

            case XWT_TYPE_MENU:
            case XWT_TYPE_MENUITEM:
               {
                  //GtkWidget *wMain = GTK_WIDGET( wWidget->get_main_widget( wWidget->widget_data ));
                  GList *child = gtk_container_children( GTK_CONTAINER( wSelf ) );
                  GtkWidget *label = gtk_label_new_with_mnemonic ( prop->value.text );
                  if( child )
                  {
                     gtk_container_remove(GTK_CONTAINER( wSelf ), GTK_WIDGET( child ));
                  }
                  gtk_container_add( GTK_CONTAINER( wSelf ), label);
                  gtk_widget_show( label );
               }
            return TRUE;

            case XWT_TYPE_IMAGE:
            return xwt_gtk_imageLoad( wWidget, prop->value.text );

            case XWT_TYPE_LAYOUT: case XWT_TYPE_PANE:
               if ( wSelf != NULL )
               {
                  gtk_frame_set_label( GTK_FRAME( wSelf ), prop->value.text );
                  return TRUE;
               }
            return FALSE;
         }
      return FALSE;

      case XWT_PROP_VISIBILITY:
         // common for all widgets
         switch( prop->value.number )
         {
            case XWT_VIS_HIDDEN:
               gtk_widget_hide( wSelf );
            return TRUE;

            case XWT_VIS_NORMAL:
               gtk_widget_show( wSelf );
            return TRUE;
         }

         // Only for window widgets
         if (
            wWidget->type != XWT_TYPE_FRAME ||
            wWidget->type != XWT_TYPE_WINDOW
         )
         {
            return FALSE;
         }
         switch( prop->value.number )
         {
            case XWT_VIS_MAXIMIZED_H:
            case XWT_VIS_MAXIMIZED_V:
            case XWT_VIS_MAXIMIZED:
            case XWT_VIS_MINIMIZED:
            //TODO
            break;
         }

      return FALSE;


      case XWT_PROP_SENSIBLE:
         if ( wWidget->type != XWT_TYPE_IMAGE )
         {
            return FALSE;
         }
         // TODO: don't ignore the bool parameter
         // TODO: also for labels.
      return xwt_gtk_image_setSensible( wWidget );

      case XWT_PROP_SETMENUBAR:
         if ( wWidget->type == XWT_TYPE_FRAME )
         {
            xwt_gtk_setMenuBar( wWidget, (PHB_ITEM) prop->value.data );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_RSTMENUBAR:
         if ( wWidget->type == XWT_TYPE_FRAME )
         {
            xwt_gtk_resetMenuBar( wWidget, (PHB_ITEM) prop->value.data );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_LAYMODE:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            xwt_gtk_layout_create_with_mode( wWidget, prop->value.number );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_PADDING:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            lay->iPadding = prop->value.number;
            return TRUE;
         }
      return FALSE;


      case XWT_PROP_FILL:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            lay->bFill = prop->value.setting;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_EXPAND:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            lay->bExpand = prop->value.setting;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_HOMOGENEOUS:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            gtk_box_set_homogeneous(
               GTK_BOX( wWidget->get_main_widget( wWidget->widget_data ) ),
               ( gboolean ) prop->value.setting ) ;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BORDER:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_PANE ||
             wWidget->type == XWT_TYPE_FRAME || wWidget->type == XWT_TYPE_WINDOW )
         {
            gtk_container_set_border_width(
               GTK_CONTAINER( wWidget->get_main_widget( wWidget->widget_data ) ),
                  (gint) prop->value.number );

            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BOX:
         if( wWidget->type == XWT_TYPE_PANE || wWidget->type == XWT_TYPE_LAYOUT )
            if ( prop->value.setting )
            {
               return xwt_gtk_container_set_box( wWidget );
            }
            else
            {
               return xwt_gtk_container_reset_box( wWidget );
            }
         break;
      return FALSE;
   }

   return FALSE;
}


BOOL xwt_drv_get_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   GtkWidget *wSelf = GTK_WIDGET( wWidget->get_top_widget( wWidget->widget_data ));

   switch( prop->type )
   {
      case XWT_PROP_FOCUS:
         // Setting focus to false has no sense
         if ( GTK_WIDGET_CAN_FOCUS( wSelf ) )
         {
            prop->value.setting = (BOOL) gtk_widget_is_focus( wSelf );
            return TRUE;
         }
      return FALSE;


      case XWT_PROP_EDITABLE:
      //TODO
      return FALSE;
      case XWT_PROP_VISIBLE:
      //TODO
      return FALSE;

      case XWT_PROP_FIXED:
      //TODO
      return FALSE;

      case XWT_PROP_MODAL:
         if (
               wWidget->type == XWT_TYPE_WINDOW
               //TODO: other modal widgets
            )
         {
            prop->value.setting = (BOOL) gtk_window_get_modal( GTK_WINDOW( wSelf ) );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_POSITION:
      {
         GtkWidget *wParent = gtk_widget_get_parent( wSelf );
         GList *pList;

         if( wParent == 0 )
         {
            if(wWidget->type == XWT_TYPE_FRAME || wWidget->type == XWT_TYPE_WINDOW)
            {
               gtk_window_get_position( GTK_WINDOW(wSelf),
                  &prop->value.position.x, &prop->value.position.y );
               return TRUE;
            }
            return FALSE;
         }

         /* moving a widget in GTK is a complicate matter. We can do it ONLY from within
            a "FIXED" pane. */
         if( strcmp( "GtkFixed", G_OBJECT_TYPE_NAME( wParent )) == 0 )
         {
            // find ourselves among the children and get the x/y
            pList = GTK_FIXED(wParent)->children;
            while ( pList != NULL )
            {
               if( pList->data == wSelf )
               {
                  prop->value.position.x = ((GtkFixedChild *) pList->data)->x;
                  prop->value.position.y = ((GtkFixedChild *) pList->data)->y;
                  return TRUE;
               }
               pList = pList->next;
            }
            return FALSE;
         }
      }
      return FALSE;

      case XWT_PROP_SELREGION:
      //TODO
      break;

      //Size parameter
      case XWT_PROP_SIZE:
         if(
            wWidget->type == XWT_TYPE_FRAME ||
            wWidget->type == XWT_TYPE_WINDOW
         )
         {
            gtk_window_get_default_size( GTK_WINDOW( wSelf ) ,
               &prop->value.size.width, &prop->value.size.height );
         }
         else
         {
            gtk_widget_get_size_request( wSelf,
               &prop->value.size.width, &prop->value.size.height );
         }

         if (prop->value.size.width == -1 )
         {
            return FALSE;
         }
      return TRUE;

      //Text parameters
      case XWT_PROP_TEXT:
         switch( wWidget->type )
         {
            case XWT_TYPE_WINDOW:
            case XWT_TYPE_FRAME:
               prop->value.text = gtk_window_get_title ( GTK_WINDOW(wSelf) );
            return TRUE;

            case XWT_TYPE_BUTTON:
               prop->value.text = gtk_button_get_label (GTK_BUTTON(wSelf) );
            return TRUE;

            case XWT_TYPE_LABEL:
               prop->value.text = gtk_label_get_label (GTK_LABEL(wSelf) );
            return TRUE;

            case XWT_TYPE_TEXTBOX:
               prop->value.text = gtk_entry_get_text (GTK_ENTRY(wSelf) );
            return TRUE;

            case XWT_TYPE_MENU:
            case XWT_TYPE_MENUITEM:
            //TODO
            return FALSE;

            case XWT_TYPE_IMAGE:
               prop->value.text = ( (PXWT_GTK_IMAGE) wWidget->widget_data)->filename;
            return TRUE;

            case XWT_TYPE_LAYOUT: case XWT_TYPE_PANE:
            {
               if( wSelf != NULL )
               {
                  prop->value.text = gtk_frame_get_label( GTK_FRAME( wSelf ) );
                  return TRUE;
               }
            }
            return FALSE;
         }
      return FALSE;

      //Numeric parameters
      case XWT_PROP_VISIBILITY:
         //TODO
      return FALSE;

      case XWT_PROP_SENSIBLE:
         if ( wWidget->type != XWT_TYPE_IMAGE )
         {
            return TRUE;
         }
         else
         {
            if ( ( (PXWT_GTK_IMAGE) wWidget->widget_data)->evt_window == NULL )
            {
               return FALSE;
            }
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BORDER:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_PANE ||
             wWidget->type == XWT_TYPE_FRAME || wWidget->type == XWT_TYPE_WINDOW )
         {
            prop->value.number = gtk_container_get_border_width(
               GTK_CONTAINER( wWidget->get_main_widget( wWidget->widget_data )));

            return TRUE;
         }
      return FALSE;

      case XWT_PROP_HOMOGENEOUS:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            prop->value.setting = gtk_box_get_homogeneous(
                  GTK_BOX( wWidget->get_main_widget( wWidget->widget_data ) ) ) ;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_PADDING:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            prop->value.number = lay->iPadding;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FILL:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            prop->value.setting = lay->bFill;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_EXPAND:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWidget->widget_data;
            prop->value.setting = lay->bExpand;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BOX:
         if( wWidget->type == XWT_TYPE_LAYOUT ||  wWidget->type == XWT_TYPE_PANE )
         {
            if ( wWidget->get_top_widget( wWidget->widget_data) !=
                  wWidget->get_main_widget( wWidget->widget_data) )
            {
               prop->value.setting = TRUE;
            }
            else
            {
               prop->value.setting = FALSE;
            }
            return TRUE;
         }
      return FALSE;
   }

   return FALSE;
}


PXWT_WIDGET xwt_drv_create(  PHB_ITEM pSelf, int type )
{
   switch( type )
   {
      case XWT_TYPE_WINDOW:  return xwt_gtk_createWindow( pSelf );
      case XWT_TYPE_FRAME:   return xwt_gtk_createFrameWindow( pSelf );
      case XWT_TYPE_PANE:    return xwt_gtk_createPane( pSelf );
      case XWT_TYPE_BUTTON:  return xwt_gtk_createButton( pSelf );
      case XWT_TYPE_LABEL:   return xwt_gtk_createLabel( pSelf );
      case XWT_TYPE_MENU:    return xwt_gtk_createMenu( pSelf );
      case XWT_TYPE_MENUITEM:return xwt_gtk_createMenuItem( pSelf );
      case XWT_TYPE_TEXTBOX: return xwt_gtk_createTextbox( pSelf );
      case XWT_TYPE_IMAGE:  return xwt_gtk_createImage( pSelf );
      case XWT_TYPE_LAYOUT:  return xwt_gtk_createLayout( pSelf );
   }
   return FALSE;
}

BOOL xwt_drv_destroy( PXWT_WIDGET wWidget )
{
   GtkWidget *wSelf = GTK_WIDGET( wWidget->get_top_widget( wWidget->widget_data ));
   gtk_widget_destroy( wSelf );
   if( wWidget->destructor != NULL )
   {
      wWidget->destructor( wWidget->widget_data );
   }
   hb_xfree( wWidget );
   return TRUE;
}


/******************************************************
*  Container Functions
*/

BOOL xwt_drv_add( PXWT_WIDGET wWSelf, PXWT_WIDGET wWChild )
{
   GtkWidget *wSelf = GTK_WIDGET( wWSelf->get_main_widget( wWSelf->widget_data ));
   GtkWidget *wChild = GTK_WIDGET( wWChild->get_top_widget( wWChild->widget_data ));

   switch( wWSelf->type )
   {
      case XWT_TYPE_WINDOW:
      case XWT_TYPE_PANE:
         gtk_fixed_put( GTK_FIXED( wSelf ), wChild, 0, 0 );
      break;

      case XWT_TYPE_FRAME:
         gtk_table_attach_defaults( GTK_TABLE( wSelf ), wChild, 0,1,0,1 );
      break;

      case XWT_TYPE_MENU:
         gtk_menu_shell_append (GTK_MENU_SHELL(wSelf), wChild );
      break;

      case XWT_TYPE_LAYOUT:
      {
         PXWT_GTK_LAYOUT lay = ( PXWT_GTK_LAYOUT ) wWSelf->widget_data;
         gtk_box_pack_start( GTK_BOX( wSelf ), wChild, lay->bExpand, lay->bFill, lay->iPadding );
      }
      break;

      default:
         gtk_container_add( GTK_CONTAINER( wSelf ), wChild );
   }
   return TRUE;
}

BOOL xwt_drv_remove( PXWT_WIDGET wWSelf, PXWT_WIDGET wWChild )
{
   GtkWidget *wSelf = GTK_WIDGET( wWSelf->get_main_widget( wWSelf->widget_data ) );
   GtkWidget *wChild = GTK_WIDGET( wWChild->get_top_widget( wWChild->widget_data ) );

   gtk_container_remove( GTK_CONTAINER( wSelf ), wChild );
   return TRUE;
}


/***************************************/
/* Procedural functions
*/

BOOL xwt_drv_init( int argc, char **argv )
{
   gtk_init( &argc, &argv );
   /* Setting up idle system */
   g_idle_add( xwt_idle_function, NULL );
   return TRUE;
}

BOOL xwt_drv_process_events()
{
   gtk_main ();
   return TRUE;
}

BOOL xwt_drv_quit()
{
   gtk_main_quit ();
   return TRUE;
}

/****************************************************
* Idle function
*/

gboolean xwt_idle_function( gpointer data )
{
   hb_idleState();
   return TRUE;
}


/****************************************************
* Utility function used internally by the OO engine
*/

void *xwt_gtk_get_topwidget_neuter( void *data )
{
   return data;
}

