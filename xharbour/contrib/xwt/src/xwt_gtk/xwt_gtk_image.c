/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_image.c,v 1.4 2003/07/23 15:58:10 lculik Exp $

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
   #if __GNUC__ <3
   imgdata->a.a.a.main_widget = gtk_image_new();
   #else
   imgdata->main_widget = gtk_image_new();
   #endif
   #if __GNUC__ <3
   imgdata->a.a.align = imgdata->a.evt_window = NULL;
   imgdata->a.a.iVAlign = XWT_ALIGN_CENTER;
   imgdata->a.a.iHAlign = XWT_ALIGN_CENTER;
   #else
   imgdata->align = imgdata->evt_window = NULL;
   imgdata->iVAlign = XWT_ALIGN_CENTER;
   imgdata->iHAlign = XWT_ALIGN_CENTER;

   #endif
   xwt_gtk_set_alignment( (PXWT_GTK_ALIGN) imgdata );

   imgdata->pixmap = NULL;
   imgdata->filename = NULL;

   #if __GNUC__ <3
   gtk_widget_show( GTK_WIDGET( imgdata->a.a.a.main_widget ) );
   #else
   gtk_widget_show( GTK_WIDGET( imgdata->main_widget ) );
   #endif

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
   #if __GNUC__ <3
   gtk_image_set_from_file( GTK_IMAGE( imgdata->a.a.a.main_widget ) , filename );
   #else
   gtk_image_set_from_file( GTK_IMAGE( imgdata->main_widget ) , filename );
   #endif

   /* An invalid load will default to the stock icon "broken image" */
   #if __GNUC__ <3
   if ( gtk_image_get_storage_type( GTK_IMAGE( imgdata->a.a.a.main_widget) ) != GTK_IMAGE_STOCK )
   #else
   if ( gtk_image_get_storage_type( GTK_IMAGE( imgdata->main_widget) ) != GTK_IMAGE_STOCK )
   #endif
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
   #if __GNUC__ <3
   if ( imgSelf->a.evt_window != NULL )
   #else
   if ( imgSelf->evt_window != NULL )
   #endif
   {
      return FALSE;
   }

   evt = gtk_event_box_new();
   gtk_container_add (GTK_CONTAINER (evt),GTK_WIDGET( wSelf->get_top_widget( wSelf->widget_data ) ));

   g_signal_connect (G_OBJECT (evt),"button_press_event",
                      G_CALLBACK (button_press_callback), wSelf->owner);
   gtk_widget_show( evt );
   #if __GNUC__ <3
   imgSelf->a.evt_window = evt;
   #else
   imgSelf->evt_window = evt;
   #endif
   return TRUE;
}
