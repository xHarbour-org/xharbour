/*
   XWT_WIN - xHarbour Windowing Toolkit/ MS-Windows interface

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_win_msgbox.c,v 1.2 2003/10/13 11:54:08 jonnymind Exp $

   Message box implementation
*/

#include <hbapi.h>
#include <xwt_win.h>
#include <xwt.ch>

HB_FUNC( XWT_DRV_MSGBOX )
{

   HWND hbox;
   HWND hParent = NULL;
   PHB_ITEM pParent = hb_param( 1, HB_IT_POINTER );
   char *caption = hb_parcx(5);

   char *msg = hb_parcx( 2 );
   int iButtons = hb_parni( 3 );
   int iBoxtype = hb_parni( 4 );
   int iResponse;

   UINT uType;

   if( pParent != NULL )
   {
      PXWT_WIDGET wxParent = (PXWT_WIDGET) pParent->item.asPointer.value;
      hParent = (HWND) wxParent->get_top_widget( wxParent->widget_data );
   }

   if( iBoxtype == 0 )
   {
      iBoxtype = XWT_MSGBOX_INFO;
   }

   switch ( iBoxtype )
   {
      case XWT_MSGBOX_INFO:
         uType = MB_ICONINFORMATION;
         if ( caption == NULL )
         {
            caption = "Information";
         }
      break;
      case XWT_MSGBOX_QUESTION:
         uType = MB_ICONQUESTION;
         if ( caption == NULL )
         {
            caption = "Question";
         }
      break;
      case XWT_MSGBOX_ERROR:
         uType = MB_ICONERROR;
         if ( caption == NULL )
         {
            caption = "Error";
         }
      break;
      case XWT_MSGBOX_WARNING:
         uType = MB_ICONWARNING;
         if ( caption == NULL )
         {
            caption = "Warning";
         }
      break;
   }

   if( iButtons == 0 )
   {
      iButtons = XWT_MSGBOX_OK;
   }

   if ( (iButtons & XWT_MSGBOX_OK) == XWT_MSGBOX_OK )
   {
      if ( (iButtons & XWT_MSGBOX_CANCEL) == XWT_MSGBOX_CANCEL )
      {
         uType |= MB_OKCANCEL;
      }
      else {
         uType |= MB_OK;
      }
   }
   else if ( (iButtons & XWT_MSGBOX_YES) == XWT_MSGBOX_YES )
   {
      if ( (iButtons & XWT_MSGBOX_CANCEL) == XWT_MSGBOX_CANCEL )
      {
         uType |= MB_YESNOCANCEL;
      }
      else {
         uType |= MB_YESNO;
      }
   }
   else  if ( (iButtons & XWT_MSGBOX_ABORT) == XWT_MSGBOX_ABORT )
   {
      uType |= MB_ABORTRETRYIGNORE;
   }
   else if ( (iButtons & XWT_MSGBOX_RETRY) == XWT_MSGBOX_RETRY )
   {
      uType |= MB_RETRYCANCEL;
   }

   iResponse = MessageBox( hParent, msg, caption, uType | MB_APPLMODAL);
   switch( iResponse )
   {
      case IDABORT: iResponse = XWT_MSGBOX_ABORT; break;
      case IDOK: case IDCONTINUE: iResponse = XWT_MSGBOX_OK; break;
      case IDCANCEL: iResponse = XWT_MSGBOX_CANCEL; break;
      case IDYES: iResponse = XWT_MSGBOX_YES; break;
      case IDNO: iResponse = XWT_MSGBOX_NO; break;
      case IDRETRY: iResponse = XWT_MSGBOX_RETRY; break;
      default: iResponse = XWT_MSGBOX_CANCEL;
   }

   hb_retni( iResponse );
}
