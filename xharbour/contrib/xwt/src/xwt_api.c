/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_api.c,v 1.17 2004/01/26 13:52:21 lculik Exp $

   XWT DRIVER PROGRAMMING INTERFACE
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbvm.h"
#include <xwt_api.h>
#include <stdarg.h>


int xwt_rise_event( PHB_ITEM pObject, int iEventType, int argc, ... )
{
   PHB_DYNS pExecSym;
   PHB_ITEM pEventParams, pItem;
   PHB_ITEM pEvent;


   PHB_BASEARRAY pBaseArray;
   int i;
   va_list ap;

   /* Avoid sending events for widget without listeners */
   /*
   Disactivated because ::RiseEvent backpropagates the event to the owner
   hb_objSendMsg( pObject, "AEVENTLISTENERS", 0 );
   pBaseArray = ( PHB_BASEARRAY ) HB_VM_STACK.Return.item.asArray.value;
   if( pBaseArray->ulLen == 0 )
   {
      return FALSE;
   }
   */
   /* Create the array for event parameters */
   pEventParams = hb_itemNew( NULL );
   hb_arrayNew( pEventParams, argc );
   va_start(ap, argc);

   pBaseArray = ( PHB_BASEARRAY ) pEventParams->item.asArray.value;
   for ( i = 0 ; i < argc; i ++)
   {
      hb_itemForwardValue( pBaseArray->pItems + i, va_arg(ap, PHB_ITEM) );
   }
   va_end( ap );

   /* Create the event */
   pExecSym = hb_dynsymFindName( "XWTEVENT" );
   hb_vmPushSymbol( pExecSym->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   /* The event is in the return */
   pEvent = hb_itemNew( NULL );
   hb_itemCopy( pEvent, &(HB_VM_STACK.Return) );
   /* Call the constructor */
   pItem = hb_itemNew( NULL );
   hb_itemPutNI( pItem, iEventType );
   hb_objSendMsg( pEvent, "NEW", 3, pItem, pObject, pEventParams );

   /* Rise the event in pObject */
   hb_objSendMsg( pObject, "RISEEVENT", 1, pEvent );

   /* free memory */
   hb_itemRelease( pEventParams );
   hb_itemRelease( pEvent );
   hb_itemRelease( pItem );

   if( HB_VM_STACK.Return.type == HB_IT_LOGICAL &&
         HB_VM_STACK.Return.item.asLogical.value == TRUE )
   {
      return TRUE;
   }
   return FALSE;
}


/* A faster version of the rise event method. */
HB_FUNC( XWT_FASTRISEEVENT )
{
   PHB_DYNS pExecSym;
   PHB_ITEM pEvent;
   PHB_ITEM pEventId, pSender, pEventParams;
   int nParamCount, i;
   PHB_BASEARRAY pBaseArray;

   /* Get the parameters the array for event parameters */
   pEventId = hb_param( 1, HB_IT_NUMERIC );

   /* Avoid sending events for widget without listeners */
   pSender = hb_param( 2, HB_IT_OBJECT );

   /*
   Disactivated because ::RiseEvent backpropagates the event to the owner
   hb_objSendMsg( pSender, "AEVENTLISTENERS", 0 );
   pBaseArray = ( PHB_BASEARRAY ) HB_VM_STACK.Return.item.asArray.value;
   if( pBaseArray->ulLen == 0 )
   {
      hb_retl( FALSE );
      return;
   }*/

   nParamCount = hb_pcount() -2 ;
   if ( nParamCount < 0 )
   {
      nParamCount = 0;
   }
   /* Create the array for event parameters */
   pEventParams = hb_itemNew( NULL );
   hb_arrayNew( pEventParams, nParamCount );

   if ( nParamCount > 0 )
   {
      pBaseArray = ( PHB_BASEARRAY ) pEventParams->item.asArray.value;
      for ( i = 0 ; i < nParamCount; i ++)
      {
         hb_itemForwardValue( pBaseArray->pItems + i, hb_param( i+3, HB_IT_ANY) );
      }
   }

   /* Create the event */
   pExecSym = hb_dynsymFindName( "XWTEVENT" );
   hb_vmPushSymbol( pExecSym->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   /* The event is in the return */
   pEvent = hb_itemNew( NULL );
   hb_itemCopy( pEvent, &(HB_VM_STACK.Return) );

   hb_objSendMsg( pEvent, "NEW", 3, pEventId, pSender, pEventParams );

   /* Rise the event in pObject */
   //TODO: Implement here the riseevent loop
   hb_objSendMsg( pSender, "RISEEVENT", 1, pEvent );

   /* free memory */
   hb_itemRelease( pEvent );
   hb_itemRelease( pEventParams );

   if( HB_VM_STACK.Return.type == HB_IT_LOGICAL &&
         HB_VM_STACK.Return.item.asLogical.value == TRUE )
   {
      hb_retl( TRUE );
   }
   hb_retl( FALSE );
}


/* A Function for doing modal dialogs */
HB_FUNC( XWT_MODAL )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PXWT_WIDGET wSelf;
   
   /* Unready driver widget? */
   if ( pSelf == NULL ) 
   { 
      hb_retl( FALSE );
      return;
   }
   
   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;
   xwt_drv_modal( wSelf );
}
/*******************************************************
* Generic xwt widget functions
*/

HB_FUNC( XWT_CREATE )
{
   PXWT_WIDGET xwtData;
   PHB_ITEM pSelf = hb_param(1, HB_IT_ARRAY );
   XWT_CREATE_WIDGET( xwtData );

   xwtData->type = hb_parni( 2 );
   xwtData->owner = pSelf->item.asArray.value;
   /* Forbid releasing of the owner. */
   hb_gcLock( xwtData->owner );

   if ( xwt_drv_create( xwtData ) )
   {
      hb_retptr( xwtData );
   }
   else
   {
      hb_xfree( xwtData );
      // TODO: signal error here.
      hb_ret();
   }
}


HB_FUNC( XWT_DESTROY )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PXWT_WIDGET wSelf;

   /* Unready driver widget? */
   if ( pSelf == NULL )
   {
      hb_retl( FALSE );
      return;
   }

   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;

   /* release owner so it can be destroyed */
   hb_gcUnlock( wSelf->owner );

   /* Destruction sequence in drivers will not touch the owner */
   hb_retl( xwt_drv_destroy( wSelf ) );
}


HB_FUNC( XWT_SETPROPERTY )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PXWT_WIDGET wSelf;
   XWT_PROPERTY prop;

   /* Unready driver widget? */
   if ( pSelf == NULL )
   {
      hb_retl( FALSE );
      return;
   }

   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;

   prop.type = hb_parni( 2 );

   switch( prop.type )
   {
      // Boolean parameter
      case XWT_PROP_FOCUS:
      case XWT_PROP_EDITABLE:
      case XWT_PROP_VISIBLE:
      case XWT_PROP_FIXED:
      case XWT_PROP_MODAL:
      case XWT_PROP_HOMOGENEOUS:
      case XWT_PROP_EXPAND:
      case XWT_PROP_FILL:
      case XWT_PROP_SHRINK:
      case XWT_PROP_FIRSTSHRINK:
      case XWT_PROP_SECSHRINK:
      case XWT_PROP_BOX:
      case XWT_PROP_STATUS:
      case XWT_PROP_UPDATE:
         prop.value.setting = hb_parl( 3 );
      break;

      //Position parameter
      case XWT_PROP_POSITION:
      case XWT_PROP_SCROLL:
      case XWT_PROP_SELREGION:
         prop.value.position.x = hb_parni( 3 );
         prop.value.position.y = hb_parni( 4 );
      break;

      //Size parameter
      case XWT_PROP_SIZE:
      case XWT_PROP_COLROWS:
         prop.value.size.width = hb_parni( 3 );
         prop.value.size.height = hb_parni( 4 );
      break;

      //Text parameters
      case XWT_PROP_TEXT:
      case XWT_PROP_IMAGE:
      case XWT_PROP_NAME:
      case XWT_PROP_FILEMASK:
      case XWT_PROP_FILENAME:
         prop.value.text = hb_parc( 3 );
      break;

      //Numeric parameters
      case XWT_PROP_VISIBILITY:
      case XWT_PROP_LAYMODE:
      case XWT_PROP_BORDER:
      case XWT_PROP_COLUMNS:
      case XWT_PROP_COLEDITABLE:
         prop.value.number = hb_parni( 3 );
      break;

      // Numeric or size
      case XWT_PROP_PADDING:
         if ( wSelf->type == XWT_TYPE_GRID )
         {
            prop.value.size.width = hb_parni( 4 );
            prop.value.size.height = hb_parni( 3 );
         }
         else
         {
            prop.value.number = hb_parni( 3 );
         }
      break;

      // Pointer
      case XWT_PROP_FIRSTWID:
      case XWT_PROP_SECWID:
      case XWT_PROP_RADIOGROUP:
         prop.value.data = hb_parptr( 3 );
      break;

      //Font Parameter
      case XWT_PROP_FONT:
         prop.value.font = hb_parc( 3 );
      break;

      case XWT_PROP_FGCOLOR:
         prop.color.fg = hb_parc( 3 );
      break;

      case XWT_PROP_BGCOLOR:
         prop.color.bg = hb_parc( 3 );
      break;

      case XWT_PROP_BASECOLOR:
         prop.color.base = hb_parc( 3 );
      break;

      case XWT_PROP_TEXTCOLOR:
         prop.color.text = hb_parc( 3 );
      break;

      //Array
      case XWT_PROP_SETMENUBAR:
      case XWT_PROP_RSTMENUBAR:
      case XWT_PROP_ATTACH:
      case XWT_PROP_CONTENT:
      case XWT_PROP_TITLES:
      case XWT_PROP_SETCOMBOITEMS:
         prop.value.data = hb_param( 3, HB_IT_ARRAY );
         if ( prop.value.data == NULL )
         {
            hb_retl( FALSE );
            return;
         }
      break;
     case XWT_PROP_SETDATE:
     {
        prop.year=1900;
	prop.month=01;
	prop.day=01;
     }
     break;
	
	 
   }

   hb_retl( xwt_drv_set_property( wSelf, &prop ) );
}


HB_FUNC( XWT_GETPROPERTY )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PXWT_WIDGET wSelf;
   XWT_PROPERTY prop;
   PHB_ITEM pParam1, pParam2;
   BOOL bRet = FALSE;
   
   /* Unready driver widget? */
   if ( pSelf == NULL ) 
   { 
      hb_retl( FALSE );
      return;
   }
   
   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;

   prop.type = hb_parni( 2 );
   if ( ! xwt_drv_get_property( wSelf, &prop ) )
   {
      hb_retl( FALSE );
      return;
   }

   pParam1 = hb_param( 3, HB_IT_BYREF );
   pParam2 = hb_param( 4, HB_IT_BYREF );

   switch( prop.type )
   {
      // Boolean parameter
      case XWT_PROP_FOCUS:
      case XWT_PROP_EDITABLE:
      case XWT_PROP_VISIBLE:
      case XWT_PROP_FIXED:
      case XWT_PROP_MODAL:
      case XWT_PROP_HOMOGENEOUS:
      case XWT_PROP_EXPAND:
      case XWT_PROP_SHRINK:
      case XWT_PROP_FILL:
      case XWT_PROP_BOX:
      case XWT_PROP_STATUS:
         if( pParam1 != NULL )
         {
            hb_itemPutL( pParam1, prop.value.setting );
            bRet = TRUE;
         }
      break;

      //Position parameter
      case XWT_PROP_POSITION:
      case XWT_PROP_SCROLL:
      case XWT_PROP_SELREGION:
         if( pParam1 != NULL && pParam2 != NULL )
         {
            hb_itemPutNI( pParam1, prop.value.position.x );
            hb_itemPutNI( pParam2, prop.value.position.y );
            bRet = TRUE;
         }
      break;

      //Size parameter
      case XWT_PROP_SIZE:
         if( pParam1 != NULL && pParam2 != NULL )
         {
            hb_itemPutNI( pParam1, prop.value.size.width );
            hb_itemPutNI( pParam2, prop.value.size.height );
            bRet = TRUE;
         }
      break;

      //Text parameters
      case XWT_PROP_TEXT:
      case XWT_PROP_IMAGE:
      case XWT_PROP_NAME:
      case XWT_PROP_FILEMASK:
      case XWT_PROP_FILENAME:
         if( pParam1 != NULL )
         {
            if ( prop.bStatic )
            {
               hb_itemPutC( pParam1, (char *)prop.value.text );
            }
            else
            {
               hb_itemPutCRaw( pParam1, prop.value.string.text, prop.value.string.iLength );
            } 
            bRet = TRUE;    
         }
      break;
      // Font Parametre
      case XWT_PROP_FONT:
         if( pParam1 != NULL )
         {
            hb_itemPutC( pParam1, (char *)prop.value.font );
            bRet = TRUE;
         }
      break;
      case XWT_PROP_FGCOLOR:
         if( pParam1 != NULL )
         {
            hb_itemPutC( pParam1, (char *)prop.color.fg );
            bRet = TRUE;
         }
      break;

      case XWT_PROP_BGCOLOR:
         if( pParam1 != NULL )
         {
            hb_itemPutC( pParam1, (char *)prop.color.bg );
            bRet = TRUE;
         }
      break;

      case XWT_PROP_BASECOLOR:
         if( pParam1 != NULL )
         {
            hb_itemPutC( pParam1, (char *)prop.color.base );
            bRet = TRUE;
         }
      break;
      case XWT_PROP_TEXTCOLOR:
         if( pParam1 != NULL )
         {
            hb_itemPutC( pParam1, (char *)prop.color.text );
            bRet = TRUE;
         }
      break;

      //Numeric parameters
      case XWT_PROP_VISIBILITY:
      case XWT_PROP_LAYMODE:
      case XWT_PROP_BORDER:
      case XWT_PROP_COLEDITABLE:
         if( pParam1 != NULL )
         {
            hb_itemPutNI( pParam1, prop.value.number );
            bRet = TRUE;
         }
      break;




      case XWT_PROP_PADDING:
         if ( wSelf->type == XWT_TYPE_GRID  && pParam1 != NULL && pParam2 != NULL )
         {
            hb_itemPutNI( pParam1, prop.value.size.height );
            hb_itemPutNI( pParam2, prop.value.size.width );
            bRet = TRUE;
         }
         else if( pParam1 != NULL )
         {
            hb_itemPutNI( pParam1, prop.value.size.height );
            bRet = TRUE;
         }
      break;
     // Date Parametres
     case XWT_PROP_GETDATE:
     case XWT_PROP_GETDATEMODAL:
         if ( pParam1 != NULL )
	 {
	 TraceLog(NULL,"%i%i%i",(long)prop.year,(long)prop.month,(long) prop.day);
	   hb_itemPutD( pParam1,(long)prop.year,(long)prop.month,(long) prop.day);
	   bRet= TRUE;
	 }
     break;	   
	   
   }

   hb_retl( bRet );
}


/******************************************************
*  Container Functions
*
* IN GTK all container objects (e.g. window) have a single
* child, which is the "layout" manager. We must operate on
* THAT child, that is the real container.
*/

HB_FUNC( XWT_ADD )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PHB_ITEM pChild = hb_param( 2, HB_IT_POINTER );
   PXWT_WIDGET wSelf;
   PXWT_WIDGET wChild;

   /* Unready driver widget? */
   if ( pSelf == NULL || pChild == NULL ) 
   { 
      hb_retl( FALSE );
      return;
   }
   
   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;
   wChild = (PXWT_WIDGET) pChild->item.asPointer.value;

   hb_retl( xwt_drv_add( wSelf, wChild ) );
}

HB_FUNC( XWT_REMOVE )
{
   PHB_ITEM pSelf = hb_param( 1, HB_IT_POINTER );
   PHB_ITEM pChild = hb_param( 2, HB_IT_POINTER );
   PXWT_WIDGET wSelf;
   PXWT_WIDGET wChild;

   /* Unready driver widget? */
   if ( pSelf == NULL || pChild == NULL ) 
   { 
      hb_retl( FALSE );
      return;
   }
   
   wSelf = (PXWT_WIDGET) pSelf->item.asPointer.value;
   wChild = (PXWT_WIDGET) pChild->item.asPointer.value;
   
   hb_retl( xwt_drv_remove( wSelf, wChild ) );
}

/***************************************/
/* Procedural functions
*/

HB_FUNC( XWTINIT )
{
   int argc = hb_pcount();
   char **argv;
   int i;
   
   if ( argc > 0 )
   {
      argv = (char **) hb_xgrab( sizeof( char *) * (argc +1));
      for ( i = 0; i < argc; i ++ )
      {
         argv[ i ] = hb_parc( i );
      }
      argv[ argc ] = 0;
   }
   else
   {
      argc = 1;
      argv = (char **) hb_xgrab( sizeof( char *) * (argc +1));
      argv[0] = "DummyName";
      argv[1] = 0;
   }
   hb_retl( xwt_drv_init( argc, argv ) );
   
   hb_xfree( argv );
}

HB_FUNC( XWTMAINLOOP )
{
   /*TODO: real management */
   xwt_drv_process_events();
}

HB_FUNC( XWTQUIT )
{
   hb_retl( xwt_drv_quit() );
}
