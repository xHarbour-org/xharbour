/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_image.c,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

   GTK interface - Clickable image widget
*/

#include "hbapi.h"
#include <xwt_api.h>
#include <xwt_gtk.h>


static gboolean
  button_press_callback (GtkWidget      *event_box,
                         GdkEventButton *event,
                         gpointer        data)
{
   HB_ITEM pPosX;
   HB_ITEM pPosY;
   XWT_GTK_MAKESELF( data );

   hb_itemPutNI( &pPosX, event->x );
   hb_itemPutNI( &pPosY, event->y );

   xwt_rise_event( &Self, XWT_E_PRESSED, 2, &pPosX, &pPosY );
   return TRUE;
}
/*
static gboolean
  button_click_callback (GtkWidget      *event_box,
                         GdkEventButton *event,
                         gpointer        data)
{
   PHB_ITEM pPosX = hb_itemNew( NULL );
   PHB_ITEM pPosY = hb_itemNew( NULL );
   XWT_GTK_MAKESELF( data );

   hb_itemPutNI( pPosX, event->x );
   hb_itemPutNI( pPosY, event->y );

   xwt_rise_event( &Self, XWT_E_CLICKED, 2, pPosX, pPosY );
   hb_itemRelease( pPosX );
   hb_itemRelease( pPosY );
   return TRUE;
}
*/

static void *image_mainwidget( void *data )
{
   PXWT_GTK_IMAGE img = (PXWT_GTK_IMAGE) data;
   return img->image;
}

static void *image_topwidget( void *data )
{
   PXWT_GTK_IMAGE img = (PXWT_GTK_IMAGE) data;
   if ( img->evt_window != NULL )
   {
      return img->evt_window;
   }

   return img->image;
}

static void image_destroy( void *data )
{
   PXWT_GTK_IMAGE img = (PXWT_GTK_IMAGE) data;
   if ( img->filename != NULL )
   {
      hb_xfree( img->filename );
   }
   hb_xfree( img );
}

PXWT_WIDGET xwt_gtk_createImage( PHB_ITEM pSelf )
{
   PXWT_WIDGET xwtData;
   PXWT_GTK_IMAGE imgdata = (PXWT_GTK_IMAGE) hb_xgrab( sizeof( XWT_GTK_IMAGE ) );

   imgdata->image = GTK_IMAGE( gtk_image_new () );
   imgdata->evt_window = NULL;
   imgdata->pixmap = NULL;
   imgdata->owner = pSelf->item.asArray.value;
   imgdata->filename = NULL;
   gtk_widget_show( GTK_WIDGET( imgdata->image ) );

   XWT_CREATE_WIDGET( xwtData );
   xwtData->type = XWT_TYPE_IMAGE;
   xwtData->widget_data = imgdata;
   xwtData->destructor = image_destroy;
   xwtData->get_main_widget = image_mainwidget;
   xwtData->get_top_widget = image_topwidget;

   return xwtData;
}

BOOL xwt_gtk_imageLoad( PXWT_WIDGET wSelf, const char *filename )
{
   PXWT_GTK_IMAGE imgdata = ( PXWT_GTK_IMAGE ) wSelf->widget_data;

   if ( imgdata->filename != NULL )
   {
      hb_xfree( imgdata->filename );
      imgdata->filename = NULL;
   }

   gtk_image_set_from_file( imgdata->image , filename );

   /* An invalid load will default to the stock icon "broken image" */
   if ( gtk_image_get_storage_type( imgdata->image ) != GTK_IMAGE_STOCK )
   {
      imgdata->filename = (char *) hb_xgrab( strlen( filename ) + 1 );
      strcpy( imgdata->filename, filename );
      return TRUE;
   }
   return FALSE;
}

BOOL xwt_gtk_image_setSensible( PXWT_WIDGET wSelf )
{
   PXWT_GTK_IMAGE imgSelf = ( PXWT_GTK_IMAGE ) wSelf->widget_data;

   if ( imgSelf->evt_window != NULL )
   {
      return FALSE;
   }

   imgSelf->evt_window = gtk_event_box_new ();
   gtk_container_add (GTK_CONTAINER (imgSelf->evt_window), GTK_WIDGET( imgSelf->image ));

   g_signal_connect (G_OBJECT (imgSelf->evt_window),"button_press_event",
                      G_CALLBACK (button_press_callback), imgSelf->owner);

   gtk_widget_show( imgSelf->evt_window );
   return TRUE;
}
