/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_msgbox.c,v 1.1 2003/03/27 23:19:10 gian Exp $

   Message box implementation
*/

#include <hbapi.h>
#include <xwt_gtk.h>
#include <xwt_api.h>
#include <xwt.ch>

HB_FUNC( XWT_DRV_MSGBOX )
{
   GtkWidget *gbox;
   GtkWidget *wParent = NULL;
   PHB_ITEM pParent = hb_param( 1, HB_IT_POINTER );
   
   char *msg = hb_parc( 2 );
   int iButtons = hb_parni( 3 );
   int iBoxtype = hb_parni( 4 );
   int iResponse;

   if( pParent == NULL )
   {
      GList *tops = gtk_window_list_toplevels();
      wParent =  tops->data;
   }
   else
   {
      PXWT_WIDGET wxParent = (PXWT_WIDGET) pParent->item.asPointer.value;
      wParent = GTK_WIDGET( wxParent->get_top_widget( wxParent->widget_data ));
   }
   
   if( iBoxtype == 0 )
   {
      iBoxtype = XWT_MSGBOX_INFO;
   }
   
   switch ( iBoxtype )
   {
      case XWT_MSGBOX_INFO: iBoxtype = GTK_MESSAGE_INFO; break;
      case XWT_MSGBOX_QUESTION: iBoxtype = GTK_MESSAGE_QUESTION; break;
      case XWT_MSGBOX_ERROR: iBoxtype = GTK_MESSAGE_ERROR; break;
      case XWT_MSGBOX_WARNING: iBoxtype = GTK_MESSAGE_WARNING; break;
   }
   
   gbox = gtk_message_dialog_new( GTK_WINDOW( wParent), 0, iBoxtype,
      GTK_BUTTONS_NONE, msg );
   
   if( iButtons == 0 )
   {
      iButtons = XWT_MSGBOX_OK;
   }
   
   if ( (iButtons & XWT_MSGBOX_OK) == XWT_MSGBOX_OK )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), GTK_STOCK_OK, XWT_MSGBOX_OK );
   }
   
   if ( (iButtons & XWT_MSGBOX_YES) == XWT_MSGBOX_YES )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), GTK_STOCK_YES, XWT_MSGBOX_YES );
   }
   
   if ( (iButtons & XWT_MSGBOX_NO) == XWT_MSGBOX_NO )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), GTK_STOCK_NO, XWT_MSGBOX_NO );
   }
   
   if ( (iButtons & XWT_MSGBOX_OK) == XWT_MSGBOX_ABORT )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), "Abort", XWT_MSGBOX_ABORT );
   }
   
   if ( (iButtons & XWT_MSGBOX_OK) == XWT_MSGBOX_RETRY )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), "Retry", XWT_MSGBOX_RETRY );
   }

   if ( (iButtons & XWT_MSGBOX_CLOSE) == XWT_MSGBOX_CLOSE )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), GTK_STOCK_CLOSE, XWT_MSGBOX_CLOSE );
   }

   if ( (iButtons & XWT_MSGBOX_CANCEL) == XWT_MSGBOX_CANCEL )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), GTK_STOCK_CANCEL, XWT_MSGBOX_CANCEL );
   }
   
   iResponse = gtk_dialog_run( GTK_DIALOG( gbox ) );
   
   hb_retni( iResponse == GTK_RESPONSE_NONE ? XWT_MSGBOX_CANCEL : iResponse );
   gtk_widget_destroy( gbox );
}
