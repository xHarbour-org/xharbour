
/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk.c,v 1.25 2003/10/09 23:57:23 jonnymind Exp $

   Global declarations, common functions

  2003-08-25 Luiz Rafael Culik Guimaraes
     Color and Font support

*/

#include <hbapi.h>
#include <xwt_gtk.h>
#include <xwt_api.h>
#define FGCOLOR 1
#define BGCOLOR 2
#define BASECOLOR 3
#define TEXTCOLOR 4

/*
 Static function to change an color of an widget for buttons,checkboxes/radios and others
*/
void widget_set_color(GtkWidget* entry,GdkColor* color,int 
component)

{

    gtk_widget_ensure_style(entry);

    GtkStyle* newstyle;

    {

	int i;
        newstyle = gtk_style_copy(gtk_widget_get_style(entry));


        for( i=0;i<5;++i) {

            if (component & FGCOLOR)

                newstyle->fg[i] = *color;

            if (component &  BGCOLOR)

                newstyle->bg[i] = *color;

            if (component &  TEXTCOLOR)

                newstyle->text[i] = *color;

            if (component & BASECOLOR)

                newstyle->base[i] = *color;

        };

    };

    gtk_widget_set_style(entry,newstyle);

}

BOOL xwt_drv_set_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   GtkWidget *wSelf = GTK_WIDGET( wWidget->get_top_widget( wWidget->widget_data ));
   GtkWidget *wMain = GTK_WIDGET( wWidget->get_main_widget( wWidget->widget_data ));

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
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
               gtk_button_set_label (GTK_BUTTON(wSelf), prop->value.text );
            return TRUE;

            case XWT_TYPE_LABEL:
               gtk_label_set_label (GTK_LABEL(wMain), prop->value.text );
            return TRUE;

            case XWT_TYPE_TEXTBOX:
               gtk_entry_set_text (GTK_ENTRY(wMain), prop->value.text );
            return TRUE;

            case XWT_TYPE_MENU:
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

            case XWT_TYPE_MENUITEM:
            {
               PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
               gtk_container_remove(GTK_CONTAINER( itm->hbox ), itm->label );
               itm->label = gtk_label_new_with_mnemonic ( prop->value.text );
               gtk_container_add(GTK_CONTAINER( itm->hbox ), itm->label );
               gtk_widget_show( itm->label );
            }
            return TRUE;

            case XWT_TYPE_IMAGE:
            return xwt_gtk_imageLoad( wWidget, prop->value.text );

            case XWT_TYPE_GRID: case XWT_TYPE_LAYOUT: case XWT_TYPE_PANE:
               if ( wSelf != wMain )
               {
                  gtk_frame_set_label( GTK_FRAME( wSelf ), prop->value.text );
                  return TRUE;
               }
            return FALSE;
         }
      return FALSE;

      case XWT_PROP_IMAGE:
         if ( wWidget->type == XWT_TYPE_IMAGE )
         {
            return xwt_gtk_imageLoad( wWidget, prop->value.text );
         }
         else if ( wWidget->type == XWT_TYPE_MENUITEM )
         {
            PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
            gtk_image_set_from_file( GTK_IMAGE( itm->image ) , prop->value.text );
            if ( gtk_image_get_storage_type( GTK_IMAGE( itm->image ) ) != GTK_IMAGE_STOCK )
            {
               return TRUE;
            }
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
         else if ( wWidget->type == XWT_TYPE_SPLITTER )
         {
            xwt_gtk_splitter_create_with_mode( wWidget, prop->value.number );
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
         else if( wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_GRID grid = ( PXWT_GTK_GRID ) wWidget->widget_data;
            grid->iXPad = prop->value.size.width;
            grid->iYPad = prop->value.size.height;
            return TRUE;
         }
      return FALSE;


      case XWT_PROP_FILL:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_LAYCONTAINER lay = ( PXWT_GTK_LAYCONTAINER ) wWidget->widget_data;
            lay->bFill = prop->value.setting;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_EXPAND:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_LAYCONTAINER lay = ( PXWT_GTK_LAYCONTAINER ) wWidget->widget_data;
            lay->bExpand = prop->value.setting;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_SHRINK:
         if( wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_GRID grid = ( PXWT_GTK_GRID ) wWidget->widget_data;
            grid->bShrink = prop->value.setting;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FIRSTSHRINK:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER split = ( PXWT_GTK_SPLITTER ) wWidget->widget_data;
            if ( split->bShrink1 != prop->value.setting )
            {
               split->bShrink1 = prop->value.setting;
               if ( split->first_widget != NULL )
               {
                  g_object_ref( G_OBJECT( split->first_widget ) );
                  gtk_container_remove( GTK_CONTAINER(wMain), split->first_widget );
                  gtk_paned_pack1( GTK_PANED( wMain ), split->first_widget,
                     TRUE, split->bShrink1);
               }
            }
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_SECSHRINK:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER split = ( PXWT_GTK_SPLITTER ) wWidget->widget_data;
            if ( split->bShrink2 != prop->value.setting )
            {
               split->bShrink2 = prop->value.setting;
               if ( split->second_widget != NULL )
               {
                  g_object_ref( G_OBJECT( split->second_widget ) );
                  gtk_container_remove( GTK_CONTAINER(wMain), split->second_widget );
                  gtk_paned_pack2( GTK_PANED( wMain ), split->second_widget,
                     TRUE, split->bShrink2);
               }
            }
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_HOMOGENEOUS:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            gtk_box_set_homogeneous( GTK_BOX( wMain ), ( gboolean ) prop->value.setting ) ;
            return TRUE;
         }
         else if ( wWidget->type == XWT_TYPE_GRID )
         {
            gtk_table_set_homogeneous( GTK_TABLE( wMain ), ( gboolean ) prop->value.setting ) ;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BORDER:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_PANE ||
             wWidget->type == XWT_TYPE_FRAME || wWidget->type == XWT_TYPE_WINDOW )
         {
            gtk_container_set_border_width( GTK_CONTAINER( wMain ),
                  (gint) prop->value.number );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BOX:
         if( wWidget->type == XWT_TYPE_PANE || wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_GRID )
         {
            if ( prop->value.setting )
            {
               return xwt_gtk_container_set_box( wWidget );
            }
            else
            {
               return xwt_gtk_container_reset_box( wWidget );
            }
         }
      return FALSE;

      case XWT_PROP_COLROWS:
         if( wWidget->type == XWT_TYPE_GRID )
         {
            gtk_table_resize( GTK_TABLE( wMain ),
               prop->value.size.height, prop->value.size.width );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FILENAME:
         gtk_file_selection_set_filename(GTK_FILE_SELECTION(wMain), prop->value.text);
         return TRUE;
//      case XWT_PROP_FONTNAME:
      

      case XWT_PROP_ATTACH:

         if( wWidget->type == XWT_TYPE_GRID )
         {

            PHB_ITEM pChild = hb_arrayGetItemPtr( (PHB_ITEM) prop->value.data, 1 );
            PHB_ITEM pRow = hb_arrayGetItemPtr( (PHB_ITEM) prop->value.data, 2 );
            PHB_ITEM pCol = hb_arrayGetItemPtr( (PHB_ITEM) prop->value.data, 3 );
            PHB_ITEM pHeight = hb_arrayGetItemPtr( (PHB_ITEM) prop->value.data, 4 );
            PHB_ITEM pWidth = hb_arrayGetItemPtr( (PHB_ITEM) prop->value.data, 5 );

            PXWT_GTK_GRID grid = (PXWT_GTK_GRID) wWidget->widget_data;
            PXWT_WIDGET wChild = pChild->item.asPointer.value;

	    #if __GNUC__ <3
            gtk_table_attach(
               GTK_TABLE( wMain ),
               GTK_WIDGET( wChild->get_top_widget( wChild->widget_data ) ),
               hb_itemGetNI( pCol ),
               hb_itemGetNI( pCol ) + hb_itemGetNI( pWidth ),
               hb_itemGetNI( pRow ),
               hb_itemGetNI( pRow ) + hb_itemGetNI( pHeight ),
               (grid->a.bFill << 2) + (grid->bShrink << 1) + grid->a.bExpand,
               (grid->a.bFill << 2) + (grid->bShrink << 1) + grid->a.bExpand,
               grid->iXPad, grid->iYPad
            );
	    #else
            gtk_table_attach(
               GTK_TABLE( wMain ),
               GTK_WIDGET( wChild->get_top_widget( wChild->widget_data ) ),
               hb_itemGetNI( pCol ),
               hb_itemGetNI( pCol ) + hb_itemGetNI( pWidth ),
               hb_itemGetNI( pRow ),
               hb_itemGetNI( pRow ) + hb_itemGetNI( pHeight ),
               (grid->bFill << 2) + (grid->bShrink << 1) + grid->bExpand,
               (grid->bFill << 2) + (grid->bShrink << 1) + grid->bExpand,
               grid->iXPad, grid->iYPad
            );
	    #endif
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_STATUS:
         if ( wWidget->type == XWT_TYPE_CHECKBOX || wWidget->type == XWT_TYPE_RADIOBUTTON || wWidget->type == XWT_TYPE_TOGGLEBUTTON )
         {
            gtk_toggle_button_set_active( GTK_TOGGLE_BUTTON( wSelf ), (gboolean) prop->value.setting );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_RADIOGROUP:
         if ( wWidget->type == XWT_TYPE_RADIOBUTTON)
         {
            PXWT_WIDGET wRadio = ( PXWT_WIDGET ) prop->value.data;
            GSList *gl = gtk_radio_button_get_group( GTK_RADIO_BUTTON( wRadio->widget_data ) );
            gtk_radio_button_set_group( GTK_RADIO_BUTTON( wMain ), gl );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FIRSTWID:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER split = (PXWT_GTK_SPLITTER) wWidget->widget_data;
            PXWT_WIDGET wid = ( PXWT_WIDGET) prop->value.data;

            if( split->first_widget != NULL )
            {
               gtk_object_ref( GTK_OBJECT(prop->value.data) );
               gtk_container_remove( GTK_CONTAINER( wSelf ), split->first_widget );
            }

            if ( wid !=  NULL )
            {
               split->first_widget = wid->get_top_widget( wid->widget_data );
               gtk_paned_pack1( GTK_PANED( wSelf ), split->first_widget,
                  (gboolean) TRUE, (gboolean) split->bShrink1);
            }
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_SECWID:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER split = (PXWT_GTK_SPLITTER) wWidget->widget_data;
            PXWT_WIDGET wid = ( PXWT_WIDGET) prop->value.data;

            if( split->second_widget != NULL )
            {
               gtk_object_ref( GTK_OBJECT(prop->value.data) );
               gtk_container_remove( GTK_CONTAINER( wSelf ), split->second_widget );
            }
            if ( wid != NULL )
            {
               split->second_widget = wid->get_top_widget( wid->widget_data );
               gtk_paned_pack2( GTK_PANED( wSelf ), split->second_widget,
                  (gboolean) TRUE, (gboolean) split->bShrink2);
            }
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_CONTENT:
         if( wWidget->type == XWT_TYPE_TREELIST )
         {
            return xwt_gtk_treelist_set_content( wWidget, prop->value.data );
         }
      return FALSE;

      case XWT_PROP_UPDATE:
         if( wWidget->type == XWT_TYPE_BROWSE )
         {
            return xwt_gtk_browse_set_content( wWidget );
         }
      return FALSE;

      case XWT_PROP_TITLES:
         if( wWidget->type == XWT_TYPE_TREELIST )
         {
            return xwt_gtk_treelist_set_columns( wWidget, prop->value.data );
         }

      return FALSE;

      case XWT_PROP_COLUMNS:
         if( wWidget->type == XWT_TYPE_TREELIST )
         {
            return xwt_gtk_treelist_create_columns( wWidget, prop->value.number );
         }
      return FALSE;

      case XWT_PROP_COLEDITABLE:
         if( wWidget->type == XWT_TYPE_TREELIST )
         {
            return xwt_gtk_treelist_set_colattr( wWidget, "editable",
                  GINT_TO_POINTER( prop->value.number ) );
         }
      return FALSE;

      case XWT_PROP_FONT:
      {
         PangoFontDescription *font_desc =  pango_font_description_from_string(prop->value.font);
         switch( wWidget->type )
         {
            case XWT_TYPE_BUTTON:
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
            {

      //	        GtkWidget *child = gtk_bin_get_child(GTK_BIN(wSelf));
      //                style = gtk_widget_get_modifier_style(child);
      //                style -> font_desc = font_desc;
      //	        gtk_widget_modify_style(GTK_WIDGET(child) , style);
            gtk_widget_modify_font(GTK_WIDGET(gtk_bin_get_child ( GTK_BIN(wSelf) ) ),font_desc);
            pango_font_description_free (font_desc);
            }
            break;
            case XWT_TYPE_LABEL:
	    {
/*	        style = gtk_widget_get_modifier_style(wMain);
                style -> font_desc = font_desc;

	        gtk_widget_modify_style(GTK_WIDGET(wMain) , style);
*/
		gtk_widget_modify_font(GTK_WIDGET(wMain),font_desc);
		pango_font_description_free (font_desc);

		break;
	}
            case XWT_TYPE_MENUITEM:
	    {
	       PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
		gtk_widget_modify_font(GTK_WIDGET(itm->label),font_desc);
		pango_font_description_free (font_desc);
		
	       break;
	    }	
        }		
	 return TRUE;
   }
   return FALSE;

      case XWT_PROP_FGCOLOR:
      {

        GdkColor color;
        gdk_color_parse (prop->color.fg, &color);
        switch( wWidget->type )
         {
            case XWT_TYPE_BUTTON:
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
		widget_set_color(wSelf, &color,FGCOLOR)
;
		break;
            case XWT_TYPE_LABEL:
		widget_set_color(wMain, &color,FGCOLOR)
;
		break;
	    case XWT_TYPE_MENUITEM:
	    {
//	        gtk_widget_modify_fg (wSelf, GTK_STATE_NORMAL, &color);
	        PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
		widget_set_color(itm->label, &color,FGCOLOR)
;
	    }
    	        break;

         }

        return TRUE;
   }
    
         case XWT_PROP_BGCOLOR:
      {
        GdkColor color;
        gdk_color_parse (prop->color.bg, &color);
        switch( wWidget->type )
         {
            case XWT_TYPE_BUTTON:
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
		widget_set_color(wSelf, &color,BGCOLOR)
;
		break;
            case XWT_TYPE_LABEL:
		widget_set_color(wMain, &color,BGCOLOR)
;
    		break;
	    case XWT_TYPE_MENUITEM:
	    {
	        PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
		widget_set_color(itm->label, &color,BGCOLOR)
;
/*	        gtk_widget_modify_bg (wSelf, GTK_STATE_NORMAL, &color);
*/
	    }	    
            break;

         }

        return TRUE;
   }


      case XWT_PROP_BASECOLOR:
      {
        GdkColor color;
        gdk_color_parse (prop->color.base, &color);
        switch( wWidget->type )
         {
            case XWT_TYPE_BUTTON:
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
		widget_set_color(wSelf, &color,BASECOLOR)
;
	    
		break;
            case XWT_TYPE_LABEL:
	    	widget_set_color(wMain, &color,BASECOLOR)
;

	    break;	    
	    case XWT_TYPE_MENUITEM:
	    {
//	        gtk_widget_modify_base (wSelf, GTK_STATE_NORMAL, &color);
	        PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
		widget_set_color(itm->label, &color,BASECOLOR)
;
}
	    	    
            break;

         }

        return TRUE;
   }

      case XWT_PROP_TEXTCOLOR:
      {
        GdkColor color;
        gdk_color_parse (prop->color.text, &color);
        switch( wWidget->type )
         {
            case XWT_TYPE_BUTTON:
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
	{    
	        GtkWidget *child = gtk_bin_get_child(GTK_BIN(wSelf));
    		widget_set_color(child, &color,FGCOLOR)
;
        }
		break;
            case XWT_TYPE_LABEL:
	    widget_set_color(wMain, &color,TEXTCOLOR)
;

	    break;
	    case XWT_TYPE_MENUITEM:
	    {
	        PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
		widget_set_color(itm->label, &color,TEXTCOLOR)
;
	    }

            break;

         }

        return TRUE;
   }


   }


   return FALSE;
}


BOOL xwt_drv_get_property( PXWT_WIDGET wWidget, PXWT_PROPERTY prop )
{
   GtkWidget *wSelf = GTK_WIDGET( wWidget->get_top_widget( wWidget->widget_data ));
   GtkWidget *wMain = GTK_WIDGET( wWidget->get_main_widget( wWidget->widget_data ));

   // GTK always return static data
   prop->bStatic = TRUE;
   
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
            case XWT_TYPE_TOGGLEBUTTON:
            case XWT_TYPE_RADIOBUTTON:
            case XWT_TYPE_CHECKBOX:
               prop->value.text = gtk_button_get_label (GTK_BUTTON(wSelf) );
            return TRUE;

            case XWT_TYPE_LABEL:
               prop->value.text = gtk_label_get_label (GTK_LABEL(wMain) );
            return TRUE;

            case XWT_TYPE_TEXTBOX:
               prop->value.text = gtk_entry_get_text (GTK_ENTRY(wMain) );
            return TRUE;

            case XWT_TYPE_MENU:
            return FALSE;

            case XWT_TYPE_MENUITEM:
            {
               PXWT_GTK_MENUITEM itm = (PXWT_GTK_MENUITEM) wWidget->widget_data;
               prop->value.text = gtk_label_get_label( GTK_LABEL( itm->label ) );
            }
            return TRUE;

            case XWT_TYPE_IMAGE:
               prop->value.text = ( (PXWT_GTK_IMAGE) wWidget->widget_data)->filename;
            return TRUE;

            case XWT_TYPE_GRID: case XWT_TYPE_LAYOUT: case XWT_TYPE_PANE:
            {
               if( wSelf != wMain )
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
	 #if __GNUC__ <3
            if ( ( (PXWT_GTK_IMAGE) wWidget->widget_data)->a.evt_window == NULL )
	 #else
            if ( ( (PXWT_GTK_IMAGE) wWidget->widget_data)->evt_window == NULL )
	 #endif
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
            prop->value.number = gtk_container_get_border_width( GTK_CONTAINER( wMain ));

            return TRUE;
         }
      return FALSE;

      case XWT_PROP_HOMOGENEOUS:
         if( wWidget->type == XWT_TYPE_LAYOUT )
         {
            prop->value.setting = gtk_box_get_homogeneous( GTK_BOX( wMain ) ) ;
            return TRUE;
         }
         else if (wWidget->type == XWT_TYPE_LAYOUT )
         {
            prop->value.setting = gtk_table_get_homogeneous( GTK_TABLE( wMain ) ) ;
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
         else if( wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_GRID grid = ( PXWT_GTK_GRID ) wWidget->widget_data;
            prop->value.size.width = grid->iXPad;
            prop->value.size.height = grid->iYPad;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FILL:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_LAYCONTAINER lay = ( PXWT_GTK_LAYCONTAINER ) wWidget->widget_data;
            prop->value.setting = lay->bFill;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_EXPAND:
         if( wWidget->type == XWT_TYPE_LAYOUT || wWidget->type == XWT_TYPE_GRID )
         {
            PXWT_GTK_LAYCONTAINER lay = ( PXWT_GTK_LAYCONTAINER ) wWidget->widget_data;
            prop->value.setting = lay->bExpand;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FIRSTSHRINK:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER lay = ( PXWT_GTK_SPLITTER ) wWidget->widget_data;
            prop->value.setting = lay->bShrink1;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_SECSHRINK:
         if( wWidget->type == XWT_TYPE_SPLITTER )
         {
            PXWT_GTK_SPLITTER lay = ( PXWT_GTK_SPLITTER ) wWidget->widget_data;
            prop->value.setting = lay->bShrink2;
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_FILENAME:
         if ( ! (( PXWT_GTK_MODAL ) wWidget->widget_data)->canceled )
         {
            prop->value.text = gtk_file_selection_get_filename( GTK_FILE_SELECTION( wSelf ) );
         }
         else
         {
            prop->value.text = "";
         }
         return TRUE;

      case XWT_PROP_FONTNAME:
         if ( ! (( PXWT_GTK_MODAL ) wWidget->widget_data)->canceled )
         {
            prop->value.text =  gtk_font_selection_dialog_get_font_name( GTK_FONT_SELECTION_DIALOG( wSelf ) );
         }
         else
         {
            prop->value.text = "";
         }
         return TRUE;



      case XWT_PROP_STATUS:
         if ( wWidget->type == XWT_TYPE_CHECKBOX || wWidget->type == XWT_TYPE_RADIOBUTTON || wWidget->type == XWT_TYPE_TOGGLEBUTTON)
         {
            prop->value.setting = (gboolean) gtk_toggle_button_get_active(
                  GTK_TOGGLE_BUTTON( wSelf ) );
            return TRUE;
         }
      return FALSE;

      case XWT_PROP_BOX:
         if( wWidget->type == XWT_TYPE_LAYOUT ||  wWidget->type == XWT_TYPE_PANE || wWidget->type == XWT_TYPE_GRID )
         {
            if ( wSelf != wMain )
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


BOOL xwt_drv_create( PXWT_WIDGET xwtData )
{
   switch( xwtData->type )
   {
      case XWT_TYPE_WINDOW:  return xwt_gtk_createWindow( xwtData );
      case XWT_TYPE_FRAME:   return xwt_gtk_createFrameWindow( xwtData );
      case XWT_TYPE_PANE:    return xwt_gtk_createPane( xwtData );
      case XWT_TYPE_BUTTON:  return xwt_gtk_createButton( xwtData );
      case XWT_TYPE_LABEL:   return xwt_gtk_createLabel( xwtData );
      case XWT_TYPE_MENU:    return xwt_gtk_createMenu( xwtData );
      case XWT_TYPE_MENUITEM:return xwt_gtk_createMenuItem( xwtData );
      case XWT_TYPE_TEXTBOX: return xwt_gtk_createTextbox( xwtData );
      case XWT_TYPE_IMAGE:   return xwt_gtk_createImage( xwtData );
      case XWT_TYPE_LAYOUT:  return xwt_gtk_createLayout( xwtData );
      case XWT_TYPE_GRID:    return xwt_gtk_createGrid( xwtData );
      case XWT_TYPE_VIEWPORT:    return xwt_gtk_createViewPort( xwtData );
      case XWT_TYPE_RADIOBUTTON: return xwt_gtk_createRadioButton( xwtData );
      case XWT_TYPE_CHECKBOX:    return xwt_gtk_createCheckbox( xwtData );
      case XWT_TYPE_FILESEL:     return xwt_gtk_createFileSelection( xwtData );
      case XWT_TYPE_SPLITTER:    return xwt_gtk_createSplitter( xwtData );
      case XWT_TYPE_TOGGLEBUTTON: return xwt_gtk_createToggleButton( xwtData );
      case XWT_TYPE_TREELIST:    return xwt_gtk_createTreelist( xwtData );
      case XWT_TYPE_BROWSE:      return xwt_gtk_createBrowse( xwtData );
      case XWT_TYPE_FONTSEL:     return xwt_gtk_createFontSelection( xwtData );

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
*  Modal procedures
*/

void xwt_drv_modal( PXWT_WIDGET widget )
{
   PXWT_GTK_MODAL wSelf = ( (PXWT_GTK_MODAL) widget->widget_data );

   // this flag will be changed by widget callbacks
   wSelf->modal = TRUE;

   while( wSelf->modal )
   {
      gtk_main_iteration();
   }

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
	 #if __GNUC__ < 3
         gtk_box_pack_start( GTK_BOX( wSelf ), wChild, lay->a.bExpand, lay->a.bFill, lay->iPadding );
	 #else
         gtk_box_pack_start( GTK_BOX( wSelf ), wChild, lay->bExpand, lay->bFill, lay->iPadding );
	 #endif
      }
      break;

      case XWT_TYPE_VIEWPORT:
      {
         PXWT_GTK_WND vp = ( PXWT_GTK_WND ) wWSelf->widget_data;
         #if __GNUC__ <3
         vp->a.main_widget = wChild;
         #else
         vp->main_widget = wChild;
         #endif
         gtk_scrolled_window_add_with_viewport( GTK_SCROLLED_WINDOW( vp->window ), wChild );
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

   // removal of the child object should NOT cause it's destruction
   g_object_ref( G_OBJECT( wChild ) );
   gtk_container_remove( GTK_CONTAINER( wSelf ), wChild );

   if ( wWSelf->type == XWT_TYPE_VIEWPORT )
   {
      PXWT_GTK_WND vp = ( PXWT_GTK_WND ) wWSelf->widget_data;
      #if __GNUC__ <3
      vp->a.main_widget = NULL;
      #else
      vp->main_widget = NULL;
      #endif
   }

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

void *xwt_gtk_get_mainwidget_base( void *data )
{
   XWT_GTK_BASE *widget = (XWT_GTK_BASE *) data;
   return widget->main_widget;

}

void *xwt_gtk_get_topwidget_base( void *data )
{
   XWT_GTK_BASE *widget = (XWT_GTK_BASE *) data;
   return widget->main_widget;

}

void *xwt_gtk_get_topwidget_align( void *data )
{
   XWT_GTK_ALIGN *widget = (XWT_GTK_ALIGN *) data;
   if ( widget->align != NULL )
   {
      return widget->align;
   }
   #if __GNUC__ <3
   return widget->a.main_widget;
   #else
   return widget->main_widget;
   #endif
}

void *xwt_gtk_get_topwidget_sensible( void *data )
{
   XWT_GTK_SENSIBLE *widget = (XWT_GTK_SENSIBLE *) data;
   if ( widget->evt_window != NULL )
   {
    return widget->evt_window;
   }
   #if __GNUC__ < 3
   if ( widget->a.align != NULL )
   {
      return widget->a.align;
   }
   #else
      if ( widget->align != NULL )
   {
      return widget->align;
   }

   #endif
   #if __GNUC__ <3
   return widget->a.a.main_widget;
   #else
   return widget->main_widget;
   #endif
}

/******************************************************/
void xwt_gtk_set_alignment( XWT_GTK_ALIGN* widget )
{
   GtkWidget *parent;
   #if __GNUC__ <3
   GtkWidget *target = GTK_WIDGET( widget->a.main_widget );
   #else
   GtkWidget *target = GTK_WIDGET( widget->main_widget );
   #endif
   double vpos, hpos;


   switch( widget->iVAlign )
   {
      case XWT_ALIGN_TOP: vpos = 0.0; break;
      case XWT_ALIGN_CENTER: vpos = 0.5; break;
      case XWT_ALIGN_BOTTOM: vpos = 1.0; break;
   }

   switch( widget->iHAlign )
   {
      case XWT_ALIGN_LEFT: hpos = 0.0; break;
      case XWT_ALIGN_CENTER: hpos = 0.5; break;
      case XWT_ALIGN_RIGHT: hpos = 1.0; break;
   }

   if ( widget->align == NULL )
   {
      widget->align = gtk_alignment_new( hpos, vpos, 0.0, 0.0 );
      parent = gtk_widget_get_parent( target );

      //Moving the new frame to the old parent if necessary
      if ( parent != NULL )
      {
         g_object_ref( target );
         gtk_container_remove( GTK_CONTAINER( parent ) , target );
         gtk_container_add( GTK_CONTAINER( parent ), widget->align );
      }
      gtk_container_add( GTK_CONTAINER( widget->align ), target );
      if ( parent != NULL )
      {
         g_object_unref( target );
      }
      gtk_widget_show( widget->align );
   }
   else
   {
      gtk_alignment_set( GTK_ALIGNMENT( widget->align), hpos, vpos, 0.0, 0.0 );
   }

}

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
