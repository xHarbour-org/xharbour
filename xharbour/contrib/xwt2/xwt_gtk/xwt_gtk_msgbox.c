/*
   XWT_GTK - xHarbour Windowing Toolkit/ GTK interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_gtk_msgbox.c,v 1.3 2004/03/18 04:13:20 ronpinkas Exp $

   Message box implementation
*/

#include <hbapi.h>
#include <hbapiitm.h>
#include <hashapi.h>
#include <xwt_gtk.h>
#include <xwt_api.h>
#include <xwt.ch>

int xwt_gtk_message_box( PXWT_WIDGET xwtParent, PHB_ITEM pSettings )
{
   GtkWidget *gbox;
   GtkWidget *wParent;
   int iResponse;
   ULONG ulElem;
   PHB_ITEM pItem;
   HB_ITEM hbKey;
   int iBoxType;
   int iButtons;
   char *msg;
   
   if( xwtParent == NULL )
   {
      GList *tops = gtk_window_list_toplevels();
      wParent =  tops->data;
   }
   else
   {
      PXWT_GTK_BASE baseParent = (PXWT_GTK_BASE) xwtParent->widget_data;
      wParent = GTK_WIDGET( baseParent->top_widget( xwtParent ));
   }

   hbKey.type = HB_IT_NIL;
   hb_itemPutCRawStatic( &hbKey, "type", 4 );
   
   if ( hb_hashScan( pSettings, &hbKey, &ulElem ) )
   {
      pItem = hb_hashGetValueAt( pSettings, ulElem );
      iBoxType = hb_itemGetNI( hb_hashGetValueAt( pSettings, ulElem ) );
      if ( iBoxType == 0 ) 
      {
         char *szType = hb_itemGetCPtr( pItem );
         if ( szType == NULL )
         {
            iBoxType = XWT_MSGBOX_INFO;
         }
         else 
         {
            if ( strcmp( szType, "info" ) == 0 )
            {
               iBoxType = XWT_MSGBOX_INFO;
            }
            else if ( strcmp( szType, "question" ) == 0 )
            {
               iBoxType = XWT_MSGBOX_QUESTION;
            }
            else if ( strcmp( szType, "warning" ) == 0 )
            {
               iBoxType = XWT_MSGBOX_WARNING;
            }
            else if ( strcmp( szType, "error" ) == 0 )
            {
               iBoxType = XWT_MSGBOX_ERROR;
            }
            else
            {
               iBoxType = XWT_MSGBOX_INFO;
            }
         }
      }          
   }
   else
   {
      iBoxType = XWT_MSGBOX_INFO;
   }
   
   switch ( iBoxType )
   {
      case XWT_MSGBOX_QUESTION: iBoxType = GTK_MESSAGE_QUESTION; break;
      case XWT_MSGBOX_ERROR: iBoxType = GTK_MESSAGE_ERROR; break;
      case XWT_MSGBOX_WARNING: iBoxType = GTK_MESSAGE_WARNING; break;
      default: iBoxType = GTK_MESSAGE_INFO; break;
   }

   hb_itemPutCRawStatic( &hbKey, "text", 4 );
   if ( hb_hashScan( pSettings, &hbKey, &ulElem ) )
   {
      msg = hb_itemGetCPtr( hb_hashGetValueAt( pSettings, ulElem ) );
      if ( msg == NULL )
      {
         msg = "";
      }
   }
   else
   {
      msg = "";
   }
   
   gbox = gtk_message_dialog_new( GTK_WINDOW(wParent), 0, iBoxType,
      GTK_BUTTONS_NONE, msg );
  
   hb_itemPutCRawStatic( &hbKey, "title", 5 ); 
   if ( hb_hashScan( pSettings, &hbKey, &ulElem ) )
   {
      char *title = hb_itemGetCPtr( hb_hashGetValueAt( pSettings, ulElem ) );
      if ( title != NULL )
      {
         gtk_window_set_title( GTK_WINDOW( gbox ), title );
      }
   }
   
   hb_itemPutCRawStatic( &hbKey, "buttons", 7 );
   if ( hb_hashScan( pSettings, &hbKey, &ulElem ) )
   {
      iButtons = hb_itemGetNI( hb_hashGetValueAt( pSettings, ulElem ) );
   }
   else
   {
      iButtons = 0;
      hb_itemPutCRawStatic( &hbKey, "ok", 2 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_OK;
      }
      
      hb_itemPutCRawStatic( &hbKey, "cancel", 6 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_CANCEL;
      }
      
      hb_itemPutCRawStatic( &hbKey, "yes", 3 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_YES;
      }
      
      hb_itemPutCRawStatic( &hbKey, "no", 2 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_NO;
      }

      hb_itemPutCRawStatic( &hbKey, "abort", 5 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_ABORT;
      }
      
      hb_itemPutCRawStatic( &hbKey, "retry", 5 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_RETRY;
      }
      
      hb_itemPutCRawStatic( &hbKey, "close", 5 );
      if( hb_hashScan( pSettings, &hbKey, &ulElem ) )
      {
         iButtons |= XWT_MSGBOX_CLOSE;
      }

      if ( iButtons == 0 )
      {
         iButtons = XWT_MSGBOX_OK;
      }
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

   if ( (iButtons & XWT_MSGBOX_ABORT) == XWT_MSGBOX_ABORT )
   {
      gtk_dialog_add_button( GTK_DIALOG( gbox ), "Abort", XWT_MSGBOX_ABORT );
   }

   if ( (iButtons & XWT_MSGBOX_RETRY) == XWT_MSGBOX_RETRY )
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

   iResponse = iResponse == GTK_RESPONSE_NONE ? XWT_MSGBOX_CANCEL : iResponse;
   gtk_widget_destroy( gbox );
   
   return iResponse;
}
