/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_image.c,v 1.2 2003/04/08 18:21:51 jonnymind Exp $

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

static gboolean
  button_click_callback (GtkWidget      *event_box,
                         GdkEventButton *event,
                         gpointer        data)
{
   HB_ITEM pPosX;
   HB_ITEM pPosY;
   XWT_GTK_MAKESELF( data );

   hb_itemPutNI( &pPosX, event->x );
   hb_itemPutNI( &pPosY, event->y );

   xwt_rise_event( &Self, XWT_E_CLICKED, 2, &pPosX, &pPosY );
   return TRUE;
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

BOOL xwt_gtk_createImage( PXWT_WIDGET xwtData )
{
   PXWT_GTK_IMAGE imgdata = (PXWT_GTK_IMAGE) hb_xgrab( sizeof( XWT_GTK_IMAGE ) );

   imgdata->main_widget = gtk_image_new();
   imgdata->align = imgdata->evt_window = NULL;
   imgdata->iVAlign = XWT_ALIGN_CENTER;
   imgdata->iHAlign = XWT_ALIGN_CENTER;
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) imgdata );

   imgdata->pixmap = NULL;
   imgdata->filename = NULL;
   gtk_widget_show( GTK_WIDGET( imgdata->main_widget ) );

   xwtData->widget_data = imgdata;
   xwtData->destructor = image_destroy;
   xwtData->get_main_widget = xwt_gtk_get_mainwidget_base;
   xwtData->get_top_widget = xwt_gtk_get_topwidget_sensible;

   return TRUE;
}

BOOL xwt_gtk_imageLoad( PXWT_WIDGET wSelf, const char *filename )
{
   PXWT_GTK_IMAGE imgdata = ( PXWT_GTK_IMAGE ) wSelf->widget_data;

   if ( imgdata->filename != NULL )
   {
      hb_xfree( imgdata->filename );
      imgdata->filename = NULL;
   }

   gtk_image_set_from_file( GTK_IMAGE( imgdata->main_widget ) , filename );

   /* An invalid load will default to the stock icon "broken image" */
   if ( gtk_image_get_storage_type( GTK_IMAGE( imgdata->main_widget) ) != GTK_IMAGE_STOCK )
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
   GtkWidget *evt;

   if ( imgSelf->evt_window != NULL )
   {
      return FALSE;
   }

   evt = gtk_event_box_new();
   gtk_container_add (GTK_CONTAINER (evt),GTK_WIDGET( wSelf->get_top_widget( wSelf->widget_data ) ));

   g_signal_connect (G_OBJECT (evt),"button_press_event",
                      G_CALLBACK (button_press_callback), wSelf->owner);
   gtk_widget_show( evt );
   imgSelf->evt_window = evt;
   return TRUE;
}
