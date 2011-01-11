#include "hbclass.ch"
#include "ole.ch"

//----------------------------------------------------------------------------------------------------------------------------//
CLASS TActiveX FROM TOleAuto

   DATA pOleInterface
   DATA hWnd
   DATA hFrame

  #ifdef INPLACE_WINDOW
  #else
   DATA hControlWnd
  #endif

   METHOD New( hWnd, cClass, nVerb, hFrame ) CONSTRUCTOR

  #ifndef USE_ATLAX
   METHOD __OnSize()                   INLINE ActiveX_Resize( ::pOleInterface )
  #else
      #ifdef INPLACE_WINDOW
      #else
         METHOD __OnSize()             INLINE ActiveX_Resize( ::hWnd, ::hControlWnd )
      #endif
  #endif

   METHOD __Properties()               INLINE ActiveX_Properties( ::pOleInterface )

  #ifndef USE_ATLAX
   METHOD __Close( nSave )             INLINE ActiveX_Close( ::pOleInterface, ::hObj, nSave ), ::pOleInterface := NIL, ::hObj := NIL
  #else
    #ifdef INPLACE_WINDOW
      METHOD __Close( nSave )             INLINE ActiveX_Close( ::pOleInterface, ::hObj, nSave ), ::pOleInterface := NIL, ::hObj := NIL
    #else
      METHOD __Close( nSave )             INLINE ActiveX_Close( ::pOleInterface, ::hObj, nSave, ::hControlWnd ), ::pOleInterface := NIL, ::hObj := NIL
    #endif
  #endif

   // FWH Compatible
  #ifndef USE_ATLAX
   METHOD Show() INLINE ActiveX_Show( ::pOleInterface )
  #endif
   METHOD Do( p1, p2, p3, p4, p5, p6, p7, p8, p9 )
  #ifndef USE_ATLAX
   METHOD AdjClient() INLINE ::__OnSize()
  #endif

   DESTRUCTOR Release()

ENDCLASS

//----------------------------------------------------------------------------------------------------------------------------//
METHOD New( hWnd, cClass, cLicense, nVerb ) CLASS TActiveX

   LOCAL oErr, hObj

  #ifndef USE_ATLAX
  #else
    #ifdef INPLACE_WINDOW
    #else
      LOCAL hControlWnd
    #endif
  #endif

   //TraceLog()

   TRY
      IF ValType( hWnd ) == 'O'
         hWnd := hWnd:hWnd
      ENDIF

      IF nVerb == NIL
         nVerb := OLEIVERB_INPLACEACTIVATE // OLEIVERB_UIACTIVATE
      ENDIF

      ::hWnd := hWnd

      IF ValType( cClass ) == 'C'
          #ifndef USE_ATLAX
           ::pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb )
          #else
            #ifdef INPLACE_WINDOW
               ::pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb )
            #else
               ::pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb, @::hControlWnd )
            #endif
          #endif

          SWITCH ::pOleInterface
             CASE -1
             CASE -2
             CASE -3
                Throw( ErrorNew( ProcName(), 1001, "Create", "Could not create object: " + Str( ::pOleInterface, 2 ), HB_aParams() ) )
          END

          #ifndef USE_ATLAX
             ::hObj := ActiveX_GetDispatch( ::pOleInterface )
          #else
            #ifdef INPLACE_WINDOW
               ::hObj := ActiveX_GetDispatch( ::hWnd )
            #else
               ::hObj := ActiveX_GetDispatch( ::hControlWnd )
            #endif
          #endif

          ::cClassName := cClass
      ELSE
         ::hObj := cClass
      ENDIF

   CATCH oErr
      TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
      Throw( oErr )
   END

RETURN Self

//----------------------------------------------------------------------------------------------------------------------------//
PROCEDURE Release() CLASS TActiveX

   //TraceLog( "*** Destructor ***" )

   IF ! Empty( ::pOleInterface )
      ActiveX_Close( ::pOleInterface, ::hObj, OLECLOSE_SAVEIFDIRTY )
      ::pOleInterface := NIL
      ::hObj := NIL
   ENDIF

   //TraceLog( "*** Destructor COMPLETED ***" )

RETURN

//----------------------------------------------------------------------------------------------------------------------------//
METHOD Do( p1, p2, p3, p4, p5, p6, p7, p8, p9 ) CLASS TActiveX

   SWITCH PCount()
      CASE 1
        #ifndef USE_ATLAX
         IF Upper( p1 ) == "SHOW"
            RETURN ActiveX_Show( ::pOleInterface )
         ENDIF
        #endif

         RETURN ::Invoke( p1 )

      CASE 2
         RETURN ::Invoke( p1, p2 )

      CASE 3
         RETURN ::Invoke( p1, p2, p3 )

      CASE 4
         RETURN ::Invoke( p1, p2, p3, p4 )

      CASE 5
         RETURN ::Invoke( p1, p2, p3, p4, p5 )

      CASE 6
         RETURN ::Invoke( p1, p2, p3, p4, p5, p6 )

      CASE 7
         RETURN ::Invoke( p1, p2, p3, p4, p5, p7 )

      CASE 8
         RETURN ::Invoke( p1, p2, p3, p4, p5, p7, p8 )

      CASE 9
         RETURN ::Invoke( p1, p2, p3, p4, p5, p7, p8, p9 )

      DEFAULT
         Throw( ErrorNew( ProcName(), 1001, "Do", "Invalid number of arguments: " + Str( PCount(), 3 ), HB_aParams() ) )
   END

RETURN Self

//----------------------------------------------------------------------------------------------------------------------------//
CLASS OleXWrapper FROM OleWrapper, TActiveX
   DESTRUCTOR Release()
ENDCLASS

//----------------------------------------------------------------------------------------------------------------------------//
PROCEDURE Release() CLASS OleXWrapper

   //TraceLog( "*** Destructor ***" )

   IF ! Empty( ::pOleInterface )
      #ifndef USE_ATLAX
         ActiveX_Close( ::pOleInterface, ::hObj, OLECLOSE_SAVEIFDIRTY )
      #else
        #ifdef INPLACE_WINDOW
           ActiveX_Close( ::pOleInterface, ::hObj, OLECLOSE_SAVEIFDIRTY, ::hWnd )
        #else
           ActiveX_Close( ::pOleInterface, ::hObj, OLECLOSE_SAVEIFDIRTY, ::hControlWnd )
        #endif
      #endif

      ::pOleInterface := NIL
      ::hObj := NIL
   ENDIF

   //TraceLog( "*** Destructor COMPLETED ***" )

RETURN

//----------------------------------------------------------------------------------------------------------------------------//
FUNCTION CreateActiveX( hWnd, cClass, cLicense, nVerb, hFrame, _6, xArg )

   LOCAL hClass, oErr, oServer, pOleInterface, hObj, oTypeLib, ProgID

   #ifndef USE_ATLAX
   #else
     #ifdef INPLACE_WINDOW
     #else
        LOCAL hControlWnd
     #endif
   #endif

   #ifdef DEMO
      QOut( "Thanks for evaluating xHBOle from http://www.xHarbour.com" )
      Alert( "Thanks for evaluating xHBOle from http://www.xHarbour.com" )
      MessageBox( hwnd, "Thanks for evaluating xHBOle from http://www.xHarbour.com", "http://www.xHarbour.com", 0 )
   #endif

   IF _6 == .F.
      RETURN TActiveX():New( hWnd, cClass, cLicense, nVerb, hFrame )
   ENDIF

   IF ( hClass := __ClsGetHandleFromName( cClass ) ) == 0
      TRY
         #ifndef USE_ATLAX
            pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb )
            hObj          := ActiveX_GetDispatch( pOleInterface )
         #else
            #ifdef INPLACE_WINDOW
              pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb )
              hObj          := ActiveX_GetDispatch( hWnd )
            #else
              pOleInterface := __CreateActiveX( hWnd, cClass, cLicense, nVerb, @hControlWnd )
              hObj          := ActiveX_GetDispatch( hControlWnd )
            #endif

            //TraceLog( hControlWnd, hObj )
         #endif

         IF xArg == NIL
            xArg := cClass
         ENDIF

         oTypeLib := LoadTypeLib( hObj, xArg, @ProgID )
         //TraceLog( oTypeLib, ProgID, cClass )

         oServer  := WrapTypeLib( oTypeLib, ProgID, "OleXWrapper", cClass )

         IF oServer == NIL
            oServer := TActiveX():New( hWnd, hObj, cLicense, nVerb, hFrame )
            oServer:cClassName := cClass
         ELSE
            oServer:hObj := hObj
            oServer:hWnd := hWnd
         END

         oServer:pOleInterface := pOleInterface
         oServer:hFrame := hFrame

         #ifndef USE_ATLAX
         #else
            #ifdef INPLACE_WINDOW
            #else
               oServer:hControlWnd  := hControlWnd
            #endif
         #endif

         TRY
            IF ! Empty( _6 )
               oServer:ConnectEvents( _6 )
            ENDIF
         CATCH oErr
            TraceLog( oErr:Operation, oErr:Description, oErr:ModuleName, oErr:ProcName, oErr:ProcLine, oErr:Args )
         END

      CATCH oErr
         TraceLog( oErr:Operation, oErr:Description, oErr:ProcName, oErr:ProcLine )
         oServer := TActiveX():New( hWnd, cClass, cLicense, nVerb, hFrame )
      END
   ELSE
      oServer := __clsInst( hClass ):New( hWnd, cClass, nVerb, hFrame )
   ENDIF

RETURN oServer

//----------------------------------------------------------------------------------------------------------------------------//
FUNCTION ActiveX( hWnd, cClass, cLicense, nVerb, hFrame, _6 )
RETURN CreateActiveX( hWnd, cClass, cLicense, nVerb, hFrame, _6 )

#pragma BEGINDUMP

#include "hbapi.h"

#include <windows.h>

HB_FUNC_STATIC( MESSAGEBOX )
{
   hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parcx( 2 ), hb_parcx( 3 ), hb_parni( 4 ) ) );
}
#pragma ENDDUMP
