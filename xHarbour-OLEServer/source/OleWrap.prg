#include "hbclass.ch"
#include "error.ch"

#define INVOKE_FUNC           1
#define INVOKE_PROPERTYGET    2
#define INVOKE_PROPERTYPUT    4
#define INVOKE_PROPERTYPUTREF 8

#define IMPLTYPEFLAG_FDEFAULT        0x1
#define IMPLTYPEFLAG_FSOURCE         0x2
#define IMPLTYPEFLAG_FRESTRICTED     0x4
#define IMPLTYPEFLAG_FDEFAULTVTABLE  0x8

#define DISPID_UNKNOWN  (-1)
#define DISPID_VALUE  (0)
#define DISPID_PROPERTYPUT  (-3)
#define DISPID_NEWENUM  (-4)
#define DISPID_EVALUATE  (-5)
#define DISPID_CONSTRUCTOR  (-6)
#define DISPID_DESTRUCTOR  (-7)
#define DISPID_COLLECT  (-8)

REQUEST TypeLib
REQUEST EnumTypeInfo
REQUEST ConstantTypeInfo
REQUEST ObjectTypeInfo
REQUEST InterfaceTypeInfo
REQUEST PropertyTypeInfo
REQUEST MethodTypeInfo
REQUEST ArgumentTypeInfo

REQUEST __OleInvokeDispatch, PCount

//----------------------------------------------------------------------------//
#pragma BEGINDUMP

   #ifndef CINTERFACE
      #define CINTERFACE 1
   #endif

   #define NONAMELESSUNION

   #include "hbapi.h"
   #include "hbstack.h"
   #include "hbapierr.h"
   #include "hbapiitm.h"
   #include "hbvm.h"
   #include "hboo.ch"
   #include "hbfast.h"

   #include <windows.h>

   extern HRESULT VariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

   static DISPPARAMS s_EmptyDispParams;
   static VARIANTARG RetVal;

   static PHB_DYNS s_pSym_hObj = NULL;
   static PHB_DYNS s_pSym_ConnectedInterfaces = NULL;
   static PHB_DYNS s_pSym_Events              = NULL;
   static PHB_DYNS s_pSym_MemberID            = NULL;
   static PHB_DYNS s_pSym_EventHandler        = NULL;
   static PHB_DYNS s_pSym_Name                = NULL;

   static PHB_DYNS s_pSym_Eval                = NULL;

#pragma ENDDUMP

#ifdef DEMO
  #pragma BEGINDUMP
     HB_FUNC_STATIC( MESSAGEBOX )
     {
        hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ) ) );
     }
  #pragma ENDDUMP
#endif

STATIC s_aConnected := {}

//----------------------------------------------------------------------------//
INIT PROCEDURE OleInit

   HB_INLINE {
      s_pSym_hObj                = hb_dynsymFind( "HOBJ" );
      s_pSym_ConnectedInterfaces = hb_dynsymFind( "CONNECTEDINTERFACES" );
      s_pSym_Events              = hb_dynsymGetCase( "EVENTS" );
      s_pSym_MemberID            = hb_dynsymFind( "MEMBERID" );
      s_pSym_EventHandler        = hb_dynsymGetCase( "EVENTHANDLER" );
      s_pSym_Name                = hb_dynsymFind( "NAME" );

      s_pSym_Eval                = hb_dynsymFind( "__EVAL" );

      //TraceLog( NULL, "%p %p %p %p %p %p", s_pSym_hObj, s_pSym_ConnectedInterfaces, s_pSym_Events, s_pSym_MemberID, s_pSym_EventHandler, s_pSym_Name );

      s_EmptyDispParams.rgvarg            = NULL;
      s_EmptyDispParams.cArgs             = 0;
      s_EmptyDispParams.rgdispidNamedArgs = 0;
      s_EmptyDispParams.cNamedArgs        = 0;

      VariantInit( &RetVal );
   }

RETURN

//--------------------------------------------------------------------
EXIT PROCEDURE OleExit

   LOCAL aSet, hObj, ConnectedInterfaces, aConnection, Interface, hConnection

   FOR EACH aSet IN s_aConnected
      hObj := aSet[1]
      ConnectedInterfaces := aSet[2]

      FOR EACH aConnection IN ConnectedInterfaces
         IF aConnection == NIL
            LOOP
         ENDIF

         Interface   := aConnection[1]
         hConnection := aConnection[2]
         aConnection := NIL

         TRY
            //__OleDisconnectInterface( ::hObj, aConnection[1]:IID, aConnection[2] )
            __OleDisconnectInterface( hObj, Interface:IID, hConnection )
         CATCH
         END
      NEXT

      aSize( ConnectedInterfaces, 0 )
   NEXT

RETURN

//--------------------------------------------------------------------
CLASS OleWrapper FROM TOleAuto
   DATA hObj

   DATA ConnectedInterfaces  INIT {}
   DATA EventHandler         INIT Hash()

   //METHOD New( oTOleAuto )   INLINE Self //::hObj := CreateOleObject( __OBJGETCLSNAME( Self ) ), /*TraceLog( __OBJGETCLSNAME( Self ), ::hObj ),*/ Self

   //METHOD OleNewEnumerator   INLINE __OleInvokeDispatch( Self:hObj, DISPID_NEWENUM, INVOKE_FUNC, "", HB_aParams() )

   METHOD OleValue           INLINE __OleInvokeDispatch( Self:hObj, DISPID_VALUE, INVOKE_PROPERTYGET, "", HB_aParams() )
   METHOD _OleValue( Value ) INLINE __OleInvokeDispatch( Self:hObj, DISPID_VALUE, INVOKE_PROPERTYPUT, "", HB_aParams() )

   //METHOD OleEnumerate( nEnumOp, nIndex ) OPERATOR "FOR EACH"

   METHOD ConnectEvents( xHandler, Interfaces )
   METHOD DisconnectEvents()
   //METHOD InvokeEvent()

   //ERROR HANDLER OnError()

   DESTRUCTOR Release()

ENDCLASS

//--------------------------------------------------------------------
METHOD ConnectEvents( xHandler, Interfaces ) CLASS OleWrapper

   LOCAL nIndex, Interface, Event, Handler, lHandled := .F.
   LOCAL hMembers := Hash()

   //? ProcName(1), Len( ::TypeLib:Objects[1]:Interfaces )

   IF Empty( ::TypeLib )
      TraceLog( "No TypeLib found!", __OBJGETCLSNAME( Self ), ::GUID )
      RETURN Self
   ENDIF

   ::DisconnectEvents()

   IF ValType( xHandler ) == 'H'
      ::EventHandler := xHandler
   ELSE
      xHandler := ::EventHandler
   ENDIF

   #if 0
      FOR EACH Interface IN ::TypeLib:Interfaces
         TraceLog( "Interface:", Interface:Name, Interface:GUID, Interface:Flags, Interface:HelpString )
      NEXT
   #endif

   IF Empty( xHandler )
      TraceLog( "No EventHandler found!", __OBJGETCLSNAME( Self ), ::GUID )
      RETURN Self
   ENDIF

   nIndex := aScan( ::TypeLib:Objects, {|oObject| /*TraceLog( oObject:Name, oObject:GUID, ::GUID ),*/ oObject:GUID == ::GUID .OR. oObject:Interfaces[1]:GUID == ::GUID } )

   IF nIndex == 0
      TraceLog( "Not connectable!", __OBJGETCLSNAME( Self ), ::GUID )
      RETURN Self
   ENDIF

   FOR EACH Interface IN ::TypeLib:Objects[ nIndex ]:Interfaces
      //TraceLog( Interface:Name, Interface:GUID, Interface:Flags, Interface:HelpString )

      IF ( Interface:Flags & ( IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FDEFAULT ) ) == ( IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FDEFAULT )
         //TraceLog( Interface:Name, Interface:Flags, Interface:HelpString )

         IF aScan( Interfaces, Interface,  ,  , .T. ) > 0
            //TraceLog( Interface:Name, "Will be processed explictly" )
            EXIT
         ENDIF

         nIndex := hGetPos( xHandler, "*" )

         IF nIndex > 0 .AND. Empty( Interfaces )
            lHandled := .T.
            Handler := hGetValueAt( xHandler, nIndex )

            IF ValType( Handler ) == 'C'
               Handler := HB_FuncPtr( Handler )
            ENDIF

            xHandler[ "*" ] := { , Handler }
            //TraceLog( xHandler[ "*" ][ 1 ], xHandler[ "*" ][ 2] )
         ENDIF

         FOR EACH Event IN Interface:Events
            hMembers[ Event:Name ] := Event:MemberID

            nIndex := hGetPos( xHandler, Event:Name )

            //TraceLog( HB_EnumIndex(), Event:Name, Event:MemberID, nIndex )

            // If explicit Interfaces specified than do not assign any handlers!
            IF nIndex > 0 .AND. Empty( Interfaces )
               lHandled := .T.
               Handler := hGetValueAt( xHandler, nIndex )

               IF ValType( Handler ) == 'C'
                  Handler := HB_FuncPtr( Handler )
               ENDIF

               xHandler[ Event:MemberID ] := { Event:Name, Handler }
               //TraceLog( Event:Name, Event:MemberID, Handler )
            ELSE
               xHandler[ Event:MemberID ] := { Event:Name, NIL }
            ENDIF
         NEXT

         IF lHandled
            //TraceLog( Interface:Name, Interface:GUID, Interface:Flags, Interface:HelpString )
            aAdd( ::ConnectedInterfaces, { Interface, __OleConnectInterface( Self, ::hObj, Interface:IID, xHandler, hMembers ) } )
         ENDIF

         EXIT
      ENDIF
   NEXT

   IF ! Empty( Interfaces )
      FOR EACH Interface IN Interfaces
         //TraceLog( Interface:Name, Interface:Flags )

         IF ( Interface:Flags & IMPLTYPEFLAG_FSOURCE ) == IMPLTYPEFLAG_FSOURCE
            //TraceLog( Interface:Name )

            nIndex := hGetPos( xHandler, "*" )

            IF nIndex > 0
               lHandled := .T.
               Handler := hGetValueAt( xHandler, nIndex )

               IF ValType( Handler ) == 'C'
                  Handler := HB_FuncPtr( Handler )
               ENDIF

               xHandler[ "*" ] := { , Handler }
               //TraceLog( xHandler[ "*" ][ 1 ], xHandler[ "*" ][ 2] )
            ENDIF

            FOR EACH Event IN Interface:Events
               hMembers[ Event:Name ] := Event:MemberID

               nIndex := hGetPos( xHandler, Event:Name )

               //TraceLog( HB_EnumIndex(), Event:Name, Event:MemberID, nIndex )

               IF nIndex > 0
                  lHandled := .T.
                  Handler := hGetValueAt( xHandler, nIndex )

                  IF ValType( Handler ) == 'C'
                     Handler := HB_FuncPtr( Handler )
                  ENDIF

                  xHandler[ Event:MemberID ] := { Event:Name, Handler }
                  //TraceLog( Event:Name, Event:MemberID, Handler )
               ELSE
                  xHandler[ Event:MemberID ] := { Event:Name, NIL }
               ENDIF
            NEXT

            IF lHandled
               lHandled := .F.
               //TraceLog( Interface:Name, Interface:GUID, Interface:Flags, Interface:HelpString )
               aAdd( ::ConnectedInterfaces, { Interface, __OleConnectInterface( Self, ::hObj, Interface:IID, xHandler, hMembers ) } )
            ENDIF
         ELSE
            TraceLog( "Not an Events Interface!", Interface:Name, Interface:Flags )
         ENDIF
      NEXT

   ENDIF

   IF Empty( ::ConnectedInterfaces )
      TraceLog( "No Events Interface found!", __OBJGETCLSNAME( Self ), ::GUID )
   ELSE
      aAdd( s_aConnected, { ::hObj, ::ConnectedInterfaces } )
   ENDIF

RETURN Self

//--------------------------------------------------------------------
METHOD DisconnectEvents()

   LOCAL aConnection

   FOR EACH aConnection IN ::ConnectedInterfaces
      IF aConnection == NIL
         LOOP
      ENDIF

      //TraceLog( aConnection[2] )
      __OleDisconnectInterface( ::hObj, aConnection[1]:IID, aConnection[2] )
   NEXT

   aSize( ::ConnectedInterfaces, 0 )

RETURN Self

//--------------------------------------------------------------------

#if 0

#pragma BEGINDUMP

  #include "hashapi.h"

  //--------------------------------------------------------------------
  HB_FUNC_STATIC( OLEWRAPPER_INVOKEEVENT )
  {
     PHB_ITEM paConnections;
     int iConnection, iEvent;
     PHB_ITEM pName = NULL;
     LONG lEventID = hb_itemGetNL( hb_stackItemFromTop(-1) );
     PHB_ITEM paEvents = hb_itemNew( NULL );
     PHB_ITEM pMemberID = hb_itemNew( NULL );

     //paConnections = hb_itemNew( hb_objSendMsg( hb_stackSelfItem(), "ConnectedInterfaces", 0 ) );
     hb_vmPushSymbol( s_pSym_ConnectedInterfaces->pSymbol );
     hb_vmPush( hb_stackSelfItem() );
     hb_vmSend( 0 );
     paConnections = hb_itemNew( hb_stackReturnItem() );

     //TraceLog( NULL, "Event: %li\n", lEventID );

     for( iConnection = 0; iConnection < paConnections->item.asArray.value->ulLen; iConnection++ )
     {
        PHB_ITEM paConnection = paConnections->item.asArray.value->pItems + iConnection;
        PHB_ITEM pInterface = paConnection->item.asArray.value->pItems;
        PHB_ITEM pEvent;

        //paEvents = hb_itemNew( hb_objSendMsg( pInterface, "Events", 0 ) );
        hb_vmPushSymbol( s_pSym_Events->pSymbol );
        hb_vmPush( pInterface );
        hb_vmSend( 0 );
        hb_itemForwardValue( paEvents, hb_stackReturnItem() );

        for( iEvent = 0; iEvent < paEvents->item.asArray.value->ulLen; iEvent++ )
        {
           pEvent = paEvents->item.asArray.value->pItems + iEvent;

           //pMemberID = hb_itemNew( hb_objSendMsg( pEvent, "MemberID", 0 ) );
           hb_vmPushSymbol( s_pSym_MemberID->pSymbol );
           hb_vmPush( pEvent );
           hb_vmSend( 0 );
           hb_itemForwardValue( pMemberID, hb_stackReturnItem() );

           //TraceLog( NULL, "(%i,%i) %li == %li ? %i\n", iConnection, iEvent, lEventID, hb_itemGetNL( pMemberID ), hb_itemGetNL( pMemberID ) == lEventID );

           if( hb_itemGetNL( pMemberID ) == lEventID )
           {
              //pName = hb_itemNew( hb_objSendMsg( pEvent, "Name", 0 ) );
              hb_vmPushSymbol( s_pSym_Name->pSymbol );
              hb_vmPush( pEvent );
              hb_vmSend( 0 );
              pName = hb_itemNew( hb_stackReturnItem() );

              //TraceLog( NULL, "pName: %p\n", pName );
              break;
           }
        }

        if( pName )
        {
           break;
           //TraceLog( NULL, "*pName: %p\n", pName );
        }
     }

     hb_itemRelease( pMemberID );
     hb_itemRelease( paEvents );
     hb_itemRelease( paConnections );

     //TraceLog( NULL, "**pName: %p\n", pName );

     if( pName )
     {
        PHB_ITEM pEventHandler;
        PHB_ITEM pGeneric = NULL;
        ULONG ulPos = 0;
        BOOL bGenericDone = FALSE;

        //TraceLog( NULL, "Event: '%s'\n", hb_itemGetCPtr( pName ) );

        //pEventHandler = hb_itemNew( hb_objSendMsg( hb_stackSelfItem(), "EventHandler", 0 ) );
        hb_vmPushSymbol( s_pSym_EventHandler->pSymbol );
        hb_vmPush( hb_stackSelfItem() );
        hb_vmSend( 0 );
        pEventHandler = hb_itemNew( hb_stackReturnItem() );

        //TraceLog( NULL, "pEventHandler: %p type: %i\n", pEventHandler, pEventHandler->type );

        // Reset the RETURN value!
        hb_itemClear( hb_stackReturnItem() );

       SearchHandler:

        if( hb_hashScan( pEventHandler, ( bGenericDone ? pGeneric : pName ), &ulPos ) )
        {
           PHB_ITEM pHandler = hb_itemNew( NULL );
           PHB_SYMB pSym = NULL;
           PHB_ITEM pSelf = NULL;

           hb_hashGet( pEventHandler, ulPos, pHandler );

           //TraceLog( NULL, "Handler type: %i\n", pHandler->type );

           if( HB_IS_STRING( pHandler ) )
           {
              PHB_DYNS pDynSym = hb_dynsymFindName( hb_itemGetCPtr( pHandler ) );

              //TraceLog( NULL, "Searched: '%s', Found: %p\n", hb_itemGetCPtr( pHandler ), pDynSym );

              if( pDynSym )
              {
                 pSym = pDynSym->pSymbol;
                 //TraceLog( NULL, "Symbol: %p Fun: %p\n", pSym, pSym->value.pFunPtr );
              }
           }
           else if( HB_IS_BLOCK( pHandler ) )
           {
              // Save!
              pSelf = hb_itemNew( hb_stackSelfItem() );

              // Switch!
              hb_itemForwardValue( hb_stackSelfItem(), pHandler );

              pSym = s_pSym_Eval->pSymbol;
           }
           else if( HB_IS_POINTER( pHandler ) )
           {
              pSym = (PHB_SYMB) hb_itemGetPtr( pHandler );
           }
           else
           {
              TraceLog( NULL, "Unexpected case in: %s(%i)\n", __FILE__, __LINE__ );
           }

           if( pSym && pSym->value.pFunPtr )
           {
              PHB_FUNC pFunc = pSym->value.pFunPtr;
              char *szSymbol;

              // No longer need MemebrID argument
              hb_stackPop();
              hb_stackBaseItem()->item.asSymbol.paramcnt--;

              // Save!
              szSymbol = hb_stackBaseItem()->item.asSymbol.value->szName;

              // Switch the symbol.
              hb_stackBaseItem()->item.asSymbol.value->szName = hb_itemGetCPtr( pName );//pSym;

              //TraceLog( NULL, "Doing: '%s' with Self types: %i %i\n", pSym->szName, hb_stackSelfItem()->type, pSelf ? pSelf->type : NULL );

              // Do it!
              pFunc();

              // Restore!
              hb_stackBaseItem()->item.asSymbol.value->szName = szSymbol;

              // Restore!
              if( pSelf )
              {
                 hb_itemForwardValue( hb_stackSelfItem(), pSelf );
                 hb_itemRelease( pSelf );
              }
           }
           else
           {
              TraceLog( NULL, "Unexpected case in: %s(%i)\n", __FILE__, __LINE__ );
           }

           hb_itemRelease( pHandler );
        }

        if( ! bGenericDone )
        {
           bGenericDone = TRUE;

           pGeneric = hb_itemNew( NULL );
           hb_itemPutCStatic( pGeneric, "*" );

           goto SearchHandler;
        }

        hb_itemRelease( pGeneric );
        hb_itemRelease( pEventHandler );
        hb_itemRelease( pName );
     }
     else
     {
        // Reset the RETURN value!
        hb_itemClear( hb_stackReturnItem() );
     }
  }

#pragma ENDDUMP

#endif

//--------------------------------------------------------------------
/*
METHOD OnError(...) CLASS OleWrapper

   TraceLog( __GetMessage(), HB_aParams() )

RETURN HB_ExecFromArray( ::Super, __GetMessage(), hb_aParams() )
*/
//--------------------------------------------------------------------
PROCEDURE Release() CLASS OleWrapper

   //TraceLog( __OBJGETCLSNAME( Self ), ::hObj )

   IF ! Empty( ::hObj )
      TRY
         ::DisconnectEvents()
      CATCH
      END

      OleReleaseObject( ::hObj )
      //::hObj := NIL
   ENDIF

RETURN

//--------------------------------------------------------------------
FUNCTION CreateObject( cID, cLicense, _3, xArg )

   LOCAL oErr, hObj, oTypeLib, ProgID, hClass, oServer

   #ifdef DEMO
      QOut( "Thanks for evaluating xHBOle from http://www.xHarbour.com" )
      Alert( "Thanks for evaluating xHBOle from http://www.xHarbour.com" )
      MessageBox( 0, "Thanks for evaluating xHBOle from http://www.xHarbour.com", "http://www.xHarbour.com", 0 )
   #endif

   IF ValType(_3) == 'L' .AND. _3 == .F.
      RETURN TOleAuto():New( cID, cLicense )
   ENDIF

   TRY
      hObj := CreateOleObject( cID, cLicense )

      IF xArg == NIL
         xArg := cID
      ENDIF

      //oServer := WrapDispatch( NameFromDispatch( hObj ), hObj, .T. )
      oTypeLib := LoadTypeLib( hObj, xArg, @ProgID )
      //TraceLog( oTypeLib )
      oServer := WrapTypeLib( oTypeLib, ProgID, "OleWrapper", cID )

      oServer:hObj := hObj

      //TraceLog( cID, __OBJGETCLSNAME( oServer ), oServer:GUID, oServer:hObj )

      TRY
         IF ! Empty( _3 )
            oServer:ConnectEvents( _3 )
         ENDIF
      CATCH oErr
         TraceLog( cID, cLicense, oErr:ModuleName, oErr:ProcName, oErr:ProcLine, oErr:Operation, oErr:Description, oErr:Args )
      END

   CATCH oErr
      //TraceLog( cID, cLicense, oErr:ModuleName, oErr:ProcName, oErr:ProcLine, oErr:Operation, oErr:Description, oErr:Args )
      oServer := TOleAuto():New( cID, , cLicense )
   END

RETURN oServer

//--------------------------------------------------------------------
FUNCTION GetActiveObject( cID, _2, xArg )

   LOCAL hObj, oErr, oTypeLib, ProgID, hClass, oServer

   IF _2 == .F.
      RETURN TOleAuto():GetActiveObject( cID )
   ENDIF

   hObj := GetOleObject( cID )

   IF Empty( hObj ) .OR. OleError() != 0
      IF Ole2TxtError() == "DISP_E_EXCEPTION"
         oErr := ErrorNew()
         oErr:Args          := { cID, _2 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := OLEExceptionDescription()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ProcName()
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := OLEExceptionSource()

         RETURN Throw( oErr )
      ELSE
         oErr := ErrorNew()
         oErr:Args          := { cID, _2 }
         oErr:CanDefault    := .F.
         oErr:CanRetry      := .F.
         oErr:CanSubstitute := .T.
         oErr:Description   := Ole2TxtError()
         oErr:GenCode       := EG_OLEEXECPTION
         oErr:Operation     := ProcName()
         oErr:Severity      := ES_ERROR
         oErr:SubCode       := -1
         oErr:SubSystem     := "OleWrap"

         RETURN Throw( oErr )
      ENDIF
   ENDIF

   TRY
      IF xArg == NIL
         xArg := cID
      ENDIF

      oTypeLib := LoadTypeLib( hObj, xArg, @ProgID )
      oServer := WrapTypeLib( oTypeLib, ProgID, "OleWrapper", cID )
      oServer:hObj := hObj

   CATCH oErr
      TraceLog( oErr:ModuleName, oErr:ProcName, oErr:ProcLine, oErr:Operation, oErr:Description )

      oServer := TOleAuto()
      oServer:hObj := hObj
      oServer:cClassName := cID
      RETURN oServer
   END

   TRY
      IF ! Empty( _2 )
         oServer:ConnectEvents( _2 )
      ENDIF
   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ModuleName, oErr:ProcName, oErr:ProcLine )
   END

RETURN oServer

//--------------------------------------------------------------------
FUNCTION WrapTypeLib( oTypeLib, cID, xSuper, ProgID )

   LOCAL oServer, oObject, oInterface, cClassName

   //TraceLog( cID, xSuper, ProgID, oTypeLib:Objects, oTypeLib:Interfaces )

   IF Empty( xSuper )
      xSuper := "OleWrapper"
   ENDIF

   FOR EACH oObject IN oTypeLib:Objects
      //TraceLog( HB_EnumIndex(), cID, oObject:Name, oObject:ProgID, oObject:GUID, oObject:ProgID = cID, cID = oObject:ProgID )

      IF Empty( oObject:ProgID )
         IF Empty( cID )
            cClassName := oTypeLib:Name + '.' + oObject:Interfaces[1]:Name
         ELSE
            cClassName := cID + '.' + oObject:Interfaces[1]:Name
         ENDIF

         oObject:ProgID := cClassName
         oObject:Interfaces[1]:ProgID := cClassName
      ELSE
         cClassName := oObject:ProgID
      ENDIF

      IF cID == NIL
         WrapInterface( xSuper, oObject:Interfaces[1], cClassName, oObject:GUID, .F., oTypeLib )
      ELSEIF oServer == NIL .AND. ( oObject:ProgID == cID .OR. oObject:GUID == cID .OR. ( oObject:Interfaces[1]:GUID == cID .AND. oObject:ProgID = ProgID ) )
         //TraceLog( oObject:ProgID, cID, oObject:GUID, cID )
         oServer := WrapInterface( xSuper, oObject:Interfaces[1], oObject:ProgID, oObject:GUID, .T., oTypeLib )
      ELSE
         WrapInterface( xSuper, oObject:Interfaces[1], cClassName, oObject:GUID, .F., oTypeLib )
      ENDIF
   NEXT

   IF cID == NIL
      cID := oTypeLib:Name
   ENDIF

   FOR EACH oInterface IN oTypeLib:Interfaces
      //TraceLog( oInterface:Name, oInterface:GUID, oInterface:ProgID )

      IF Empty( oInterface:ProgID )
         cClassName := cID + '.' + oInterface:Name

         oInterface:ProgID := cClassName
         //TraceLog( oInterface:Name, oInterface:GUID, cClassName )
      ELSE
         cClassName := oInterface:ProgID
      ENDIF

      IF oServer == NIL .AND. oInterface:GUID == cID
         oServer := WrapInterface( xSuper, oInterface, cClassName, oInterface:GUID, .T., oTypeLib )
      ELSE
         WrapInterface( xSuper, oInterface, cClassName, oInterface:GUID, .F., oTypeLib )
      ENDIF
   NEXT

   IF Empty( ProgID )
      RETURN NIL
   ENDIF

   IF oServer == NIL
      //TraceLog( "Could not locate:", cID, ProgID )
   ENDIF

RETURN oServer

//--------------------------------------------------------------------
STATIC FUNCTION WrapInterface( xSuper, oInterface, cProgID, cGUID, lInstance, oTypeLib )

   LOCAL cClassID, hClass, oMetaClass, Property, Method, cParams1, Arg, cBlock, oInstance

   IF ( oInterface:Flags & IMPLTYPEFLAG_FSOURCE ) == IMPLTYPEFLAG_FSOURCE
      //TraceLog( "Skip Events Interface:", oInterface:Name )
      RETURN NIL
   ENDIF

   //TraceLog( oInterface:Name, cProgID, cGUID )

   IF Empty( cProgID )
      cClassID := cGUID
   ELSE
      cClassID := cProgID
   ENDIF

   IF ( hClass := __ClsGetHandleFromName( cClassID ) ) == 0
      //TraceLog( cClassID )
      oMetaClass := HBClass():New( cClassID, xSuper )
      //TraceLog( "Class: " + cProgID + " for:", oInterface:Name, oInterface:GUID, xSuper )

      oMetaClass:AddClassData( "TypeLib", oTypeLib, 'C', HB_OO_CLSTP_READONLY, .F. )
      oMetaClass:AddClassData( "TypeInfo", oInterface, 'O', HB_OO_CLSTP_READONLY, .F. )
      //oMetaClass:AddClassData( "cClassName", oInterface:Name, 'C', HB_OO_CLSTP_READONLY, .F. )
      oMetaClass:AddClassData( "GUID", cGUID, 'C', HB_OO_CLSTP_READONLY, .F. )

      FOR EACH Property IN oInterface:Properties
         IF Property:MemberID == NIL
            HB_INLINE( Property:Name, oInterface:Name );
            {
               #ifdef _DEBUG
                  void OutputDebugValues( const char *sFormat, ... );
                  OutputDebugValues( "Oops! Property: '%s' of Interface: '%s' has no MemberID.\n", hb_parc(1), hb_parc(2) );
               #endif
            }

            LOOP
         ENDIF

         cParams1 := ""

         FOR EACH Arg IN Property:Arguments
            cParams1 += "," + Arg:Name
         NEXT

         cBlock := "{|Self" + cParams1 + "| __OleInvokeDispatch( Self:hObj," + Str( Property:MemberID ) + "," + Str( INVOKE_PROPERTYGET ) + ", '" + Property:GUID + "', HB_aParams() ) }"
         //TraceLog( cID, Property:Name, cBlock )
         oMetaClass:AddInline( Property:Name, &( cBlock ) , HB_OO_CLSTP_EXPORTED, .F. )

         IF ! Property:ReadOnly
            cBlock := "{|Self" + cParams1 + "| __OleInvokeDispatch( Self:hObj," + Str( Property:MemberID ) + "," + Str( INVOKE_PROPERTYPUT ) + ", '" + Property:GUID + "', HB_aParams() ) }"

            //TraceLog( cID, Property:Name, cBlock )
            oMetaClass:AddInline( "_" + Property:Name, &( cBlock ) , HB_OO_CLSTP_EXPORTED, .F. )
         ENDIF
      NEXT

      FOR EACH Method IN oInterface:Methods
         IF Method:MemberID == NIL
            HB_INLINE( Method:Name, oInterface:Name );
            {
               #ifdef _DEBUG
                  void OutputDebugValues( const char *sFormat, ... );
                  OutputDebugValues( "Oops! Method: '%s' of Interface: '%s' has no MemberID.\n", hb_parc(1), hb_parc(2) );
               #endif
            }

            LOOP
         ENDIF

         cParams1 := ""

         FOR EACH Arg IN Method:Arguments
            cParams1 += "," + Arg:Name
         NEXT

         cBlock := "{|Self" + cParams1 + "| __OleInvokeDispatch( Self:hObj," + Str( Method:MemberID ) + "," + Str( INVOKE_FUNC ) + ", '" + Method:GUID + "', HB_aParams() ) }"
         //TraceLog( cID, Method:Name, cBlock )
         oMetaClass:AddInline( Method:Name, &( cBlock ) , HB_OO_CLSTP_EXPORTED, .F. )
      NEXT

      oMetaClass:Create()
      hClass := oMetaClass:hClass
   ENDIF

   IF lInstance
      //TraceLog( oInterface:Name, cClassID, cProgID, cGUID )
      oInstance := __clsInst( hClass )
   ENDIF

   IF cClassID == cProgID .AND. ( hClass := __ClsGetHandleFromName( cGUID ) ) == 0
      //TraceLog( cGUID )
      oMetaClass := HBClass():New( cGUID, cClassID )
      //TraceLog( "Class: " + cGUID + " for:", cProgID, oInterface:Name, oInterface:GUID, xSuper )

      oMetaClass:Create()
   ENDIF

   IF ( hClass := __ClsGetHandleFromName( oInterface:GUID ) ) == 0
      //TraceLog( oInterface:GUID )
      oMetaClass := HBClass():New( oInterface:GUID, cClassID )
      oMetaClass:AddClassData( "GUID", oInterface:GUID, 'C', HB_OO_CLSTP_READONLY, .F. )
      //TraceLog( "Class: " + oInterface:GUID + " for:", cProgID, oInterface:Name, oInterface:GUID, xSuper )

      oMetaClass:Create()
   ENDIF

RETURN oInstance

//--------------------------------------------------------------------
FUNCTION WrapTOleAuto( oTOleAuto, xArg )

   LOCAL hObj, cID, oTypeLib, oWrapper

   IF oTOleAuto:ClassName == "TOLEAUTO"
      hObj := oTOleAuto:hObj

      IF xArg == NIL
         xArg := oTOleAuto:cClassName
      ENDIF

      cID := NameFromDispatch( hObj )

      IF __ClsGetHandleFromName( cID ) == 0
         oTypeLib := LoadTypeLib( hObj, xArg )
         WrapTypeLib( oTypeLib, NIL, "OleWrapper", NIL )
      ENDIF

      oWrapper := WrapDispatch( cID, hObj, xArg )

      //oTOleAuto:hObj := NIL
      //oTOleAuto := NIL
   ELSE
      TraceLog( "Unexpected argument!", oTOleAuto )
   ENDIF

RETURN oWrapper

//--------------------------------------------------------------------
FUNCTION WrapDispatch( cGUID, hObj, xArg )

   LOCAL oErr, hClass := 0, oDisp, nPos, cProgID, oTypeLib

   //TraceLog( cGUID, NumToHex( hObj ), xArg )

   hClass := __ClsGetHandleFromName( cGUID )

   IF hClass == 0
      RETURN NIL

      /*
      IF xArg == NIL
         xArg := cGUID
      ENDIF

      oTypeLib := LoadTypeLib( hObj, xArg )
      WrapTypeLib( oTypeLib, NIL, "OleWrapper", NIL )
      hClass := __ClsGetHandleFromName( cGUID )
      */
   ENDIF

   /*
   IF hClass == 0
      oErr := ErrorNew()
      oErr:Args          := { cGUID }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Wrapper class not found"
      oErr:GenCode       := EG_OLEEXECPTION
      oErr:Operation     := ProcName()
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "OLE"

      RETURN Throw( oErr )
   ENDIF
   */

   oDisp := __clsInst( hClass )

   nPos := aScan( oDisp:TypeLib:Objects, {|oObject| oObject:GUID == cGUID .AND. ! Empty( oObject:ProgID ) } )

   IF nPos > 0
      cProgID := oDisp:TypeLib:Objects[ nPos ]:ProgID
   ELSE
      nPos := aScan( oDisp:TypeLib:Objects, {|oObject| oObject:Interfaces[1]:GUID == cGUID .AND. ! Empty( oObject:ProgID ) } )

      IF nPos > 0
         cProgID := oDisp:TypeLib:Objects[ nPos ]:ProgID
      ELSE
         nPos := aScan( oDisp:TypeLib:Interfaces, {|oInterface| oInterface:GUID == cGUID .AND. ! Empty( oInterface:ProgID ) } )

         IF nPos > 0
            cProgID := oDisp:TypeLib:Interfaces[ nPos ]:ProgID
         ELSE
            TraceLog( "Failed to locate Wrapper Class for:", cGUID )
            RETURN oDisp
         ENDIF
      ENDIF
   ENDIF

   hClass := __ClsGetHandleFromName( cProgID )

   IF hClass > 0
      oDisp := __clsInst( hClass )
   ELSE
      TraceLog( "Failed to locate hClass for ProgID:", cProgID, cGUID )
      RETURN oDisp
   ENDIF

   OleAddRef( hObj )
   oDisp:hObj := hObj

RETURN oDisp
//--------------------------------------------------------------------
