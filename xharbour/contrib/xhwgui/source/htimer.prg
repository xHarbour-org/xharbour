/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HTimer class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define  TIMER_FIRST_ID   33900

CLASS HTimer INHERIT HObject

   CLASS VAR aTimers   INIT {}
   DATA id
   DATA value
   DATA oParent
   DATA bAction

   METHOD New( oParent,id,value,bAction )
   METHOD End()

ENDCLASS

METHOD New( oParent,nId,value,bAction ) CLASS HTimer

   ::oParent := Iif( oParent==Nil, HWindow():GetMain(), oParent )
   ::id      := Iif( nId==Nil, TIMER_FIRST_ID + Len( ::oParent:aControls ), ;
                         nId )
   ::value   := value
   ::bAction := bAction

   SetTimer( oParent:handle, ::id, ::value )
   Aadd( ::aTimers,Self )

Return Self

METHOD End() CLASS HTimer
Local i

   KillTimer( ::oParent:handle,::id )
   i := Ascan( ::aTimers,{|o|o:id==::id} )
   IF i != 0
      Adel( ::aTimers,i )
      Asize( ::aTimers,Len( ::aTimers )-1 )
   ENDIF

Return Nil

Function TimerProc( hWnd, idTimer, time )
Local i := Ascan( HTimer():aTimers,{|o|o:id==idTimer} )

   IF i != 0
      Eval( HTimer():aTimers[i]:bAction,time )
   ENDIF

Return Nil

EXIT PROCEDURE CleanTimers
Local oTimer, i

   For i := 1 TO Len( HTimer():aTimers )
      oTimer := HTimer():aTimers[i]
      KillTimer( oTimer:oParent:handle,oTimer:id )
   NEXT

Return
