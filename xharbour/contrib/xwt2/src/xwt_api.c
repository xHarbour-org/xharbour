/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: xwt_api.c,v 1.20 2004/03/18 04:12:31 ronpinkas Exp $

   XWT DRIVER PROGRAMMING INTERFACE
*/

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hashapi.h"
#include "hbfast.h"
#include "hbstack.h"
#include "hbvm.h"
#include <xwt_api.h>
#include <stdarg.h>

static PXWT_DRIVER s_driver = NULL;

int xwt_rise_event( PHB_ITEM pObject, char *szEventType, int argc, ... )
{
   PHB_DYNS pExecSym;
   HB_ITEM hbEventParams, hbItem;
   HB_ITEM hbEvent;

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
   hbEventParams.type = HB_IT_NIL;
   hb_arrayNew( &hbEventParams, argc );
   va_start(ap, argc);

   pBaseArray = ( PHB_BASEARRAY ) hbEventParams.item.asArray.value;
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
   hbEvent.type = HB_IT_NIL;
   hb_itemCopy( &hbEvent, &(HB_VM_STACK.Return) );
   /* Call the constructor */
   hbItem.type = HB_IT_NIL;
   hb_itemPutCStatic( &hbItem, szEventType );
   hb_objSendMsg( &hbEvent, "NEW", 3, &hbItem, pObject, &hbEventParams );

   /* Rise the event in pObject */
   hb_objSendMsg( pObject, "RISEEVENT", 1, &hbEvent );
   hb_itemClear( &hbEvent );
   hb_itemClear( &hbItem );
   hb_itemClear( &hbEventParams );

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
   PHB_ITEM pEventId, pSender;
   HB_ITEM hbEvent;
   HB_ITEM hbEventParams;
   int nParamCount, i;
   PHB_BASEARRAY pBaseArray;

   /* Get the parameters the array for event parameters */
   pEventId = hb_param( 1, HB_IT_STRING );

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
   hbEventParams.type = HB_IT_NIL;
   hb_arrayNew( &hbEventParams, nParamCount );

   if ( nParamCount > 0 )
   {
      pBaseArray = ( PHB_BASEARRAY ) hbEventParams.item.asArray.value;
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
   hbEvent.type = HB_IT_NIL;
   hb_itemCopy( &hbEvent, &(HB_VM_STACK.Return) );

   hb_objSendMsg( &hbEvent, "NEW", 3, pEventId, pSender, &hbEventParams );

   /* Rise the event in pObject */
   //TODO: Implement here the riseevent loop
   hb_objSendMsg( pSender, "RISEEVENT", 1, &hbEvent );
   hb_itemClear( &hbEvent );
   hb_itemClear( &hbEventParams );


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
   PXWT_WIDGET pSelf = (PXWT_WIDGET) hb_parpointer( 1 );

   /* Unready driver widget? */
   if ( pSelf == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_MODAL", 
         1, hb_paramError( 1 ) );
      return;
   }
   
   s_driver->do_modal( pSelf );
}
/*******************************************************
* Generic xwt widget functions
*/

HB_GARBAGE_FUNC( xwt_widget_finalize )
{
   PXWT_WIDGET pWidget = (PXWT_WIDGET) Cargo;
   
   if ( pWidget->sign != XWT_WIDGET_SIGN )
   {
      hb_errInternal( HB_EI_MEMCORRUPT,
         "xwt_widget_finalize: Corrupted widget object at 0x%p",
         (char *) Cargo, NULL );
      return;
   }
   
   if ( pWidget->destroy!= NULL )
   {
      pWidget->destroy( pWidget );
   }
   
   if ( pWidget->widget_data != NULL ) 
   {
      hb_xfree( pWidget->widget_data );
   }
   
   //hb_owner acts as a weak reference; more later
   hb_gcFree( Cargo );
}

void xwt_widget_set_owner( PXWT_WIDGET widget, PHB_ITEM owner )
{
   widget->hbOwner.type = owner->type;
   widget->hbOwner.item.asArray.value = owner->item.asArray.value;
   widget->pOwner = &(widget->hbOwner);
}


HB_FUNC( XWT_CREATE )
{
   PXWT_WIDGET pWidget;
   PHB_ITEM pSelf = hb_param(1, HB_IT_OBJECT );
   pWidget = (PXWT_WIDGET) hb_gcAlloc( sizeof(XWT_WIDGET) , xwt_widget_finalize );
   
   /** Wrong Call ? */
   if ( pSelf == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_CREATE", 
         1, hb_paramError( 1 ) );
      return;
   }
   pWidget->sign = XWT_WIDGET_SIGN;
   pWidget->type = hb_parni( 2 );
   xwt_widget_set_owner( pWidget, pSelf );

   if ( s_driver->create( pWidget ) )
   {
      hb_retptrGC( pWidget );
   }
   else
   {
      hb_gcFree( pWidget );
   }
   //retunr nil
}


HB_FUNC( XWT_SETPROPERTY )
{
   PXWT_WIDGET pWidget = hb_parpointer( 1 );
   PHB_ITEM pProp = hb_param(2, HB_IT_STRING | HB_IT_HASH );
   PHB_ITEM pValue = hb_param( 3, HB_IT_ANY );
   
   /** Wrong Call ? */
   if ( pWidget == NULL || pProp == NULL || ( HB_IS_STRING( pProp) && pValue == NULL ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_SETPROPERTY", 
         3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }
   
   // is it a group or signle property call?
   if ( HB_IS_STRING( pProp ) )
   {
      hb_retl( pWidget->set_property( pWidget, hb_itemGetCPtr(pProp), pValue ));
   }
   else { // pProp is a hash containing a set of properties to be set
      hb_retl( pWidget->set_pgroup( pWidget, pProp ));
   }
}

HB_FUNC( XWT_GETPROPERTY )
{
   PXWT_WIDGET pWidget = hb_parpointer( 1 );
   PHB_ITEM pProp = hb_param(2, HB_IT_STRING );
   HB_ITEM hbValue;
   
   /** Wrong Call ? */
   if ( pWidget == NULL || pProp == NULL  )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_GETPROPERTY", 
         2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hbValue.type = HB_IT_NIL;
   if ( pWidget->get_property( pWidget, hb_itemGetCPtr( pProp ), &hbValue ) )
   {
      hb_itemReturn( &hbValue );
   }
   // else NIL      
}

HB_FUNC( XWT_GETALLPROPERTIES )
{
   PXWT_WIDGET pWidget = hb_parpointer( 1 );
   HB_ITEM hbHash;
   
   /** Wrong Call ? */
   if ( pWidget == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_GETALLPROPERTIES", 
         1, hb_paramError( 1 ) );
      return;
   }
      
   hbHash.type = HB_IT_NIL;
   hb_hashNew( &hbHash );
   
   pWidget->get_all_properties( pWidget, &hbHash );
   hb_itemReturn( &hbHash );
}


HB_FUNC( XWT_MSGBOX )
{
   PXWT_WIDGET wParent;
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pSettings = hb_param( 2, HB_IT_HASH );
   PHB_ITEM pParent = hb_param(3, HB_IT_OBJECT );
   
   // wparent may be null 
   if ( pSettings == NULL && pText == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_MSGBOX", 
         3, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
      return;
   }
   
   if( pText != NULL )
   {
      hb_hashAddChar( pSettings, "text", pText );
   }
   if ( pParent != NULL )
   {
      hb_objSendMsg( pParent, "ORAWWIDGET", 0 );
      wParent = (PXWT_WIDGET) hb_itemGetPtr( &(HB_VM_STACK.Return) );
   }
   else
   {
      wParent = NULL;
   }
   
   hb_retni( s_driver->message_box( wParent, pSettings ) );
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
   PXWT_WIDGET pParent = hb_parpointer( 1 );
   PXWT_WIDGET pChild = hb_parpointer( 2 );

   if ( pParent == NULL || pChild == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_ADD", 
         2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_retl( s_driver->add( pParent, pChild ) );
}

HB_FUNC( XWT_REMOVE )
{
   PXWT_WIDGET pParent = hb_parpointer( 1 );
   PXWT_WIDGET pChild = hb_parpointer( 2 );

   if ( pParent == NULL || pChild == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_REMOVE", 
         2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_retl( s_driver->remove( pParent, pChild ) );
}

HB_FUNC( XWT_CONNECT )
{
   PXWT_WIDGET pParent = hb_parpointer( 1 );
   PXWT_WIDGET pChild = hb_parpointer( 2 );

   if ( pParent == NULL || pChild == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_CONNECT", 
         2, hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   hb_retl( s_driver->connect( pParent, pChild ) );
}

HB_FUNC( XWT_DISCONNECT )
{
   PXWT_WIDGET pParent = hb_parpointer( 1 );

   if ( pParent == NULL )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "XWT_DISCONNECT", 
         1, hb_paramError( 1 ) );
      return;
   }

   hb_retl( s_driver->disconnect( pParent ) );
}

/***************************************/
/* Procedural functions
*/

// Quite simple for now.
void xwt_register_driver( PXWT_DRIVER drv )
{
   s_driver = drv;
}

HB_FUNC( XWTINIT )
{
   int argc = hb_pcount();
   char **argv;
   int i;

   if(  s_driver == NULL )
   {
      hb_errInternal( 0, "XWT driver NULL: I don't know how to render graphics", NULL, NULL );
   }
   
   if ( argc > 0 )
   {
      argv = (char **) hb_xgrab( sizeof( char *) * (argc +1));
      for ( i = 0; i < argc; i ++ )
      {
         argv[ i ] = hb_parcx( i );
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
   hb_retl( s_driver->init( argc, argv ) );

   hb_xfree( argv );
}

HB_FUNC( XWTMAINLOOP )
{
   /*TODO: real management */
   s_driver->process_events();
}

HB_FUNC( XWTQUIT )
{
   hb_retl( s_driver->quit() );
}


//------------------------------------------------------
// XWT MODULES LINKAGE
// hack for now... link the only module for a platform
XWT_MODULE_REQUEST( GTK )
