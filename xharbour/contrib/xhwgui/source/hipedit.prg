/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HTab class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"
#include "commctrl.ch"

//- HIPedit

CLASS HIPedit INHERIT HControl

   CLASS VAR winclass   INIT WC_IPADDRESS
   DATA bSetGet
   DATA bChange
   DATA bKillFocus
   DATA bGetFocus

   METHOD New( oWndParent,nId,bSetGet, nStyle,nLeft,nTop,nWidth,nHeight, ;
                  aValue,oFont,bGetFocus,bKillFocus )
   METHOD Activate()
   METHOD Init()
   METHOD SetValue( aValue )
   METHOD GetValue(  )
   METHOD Clear(  )
   METHOD End()

   HIDDEN:
     DATA  aValue           // Valor atual

ENDCLASS

METHOD New( oWndParent,nId,bSetGet, nStyle,nLeft,nTop,nWidth,nHeight, ;
                  aValue,oFont,bGetFocus,bKillFocus ) CLASS HIPedit

   // ::classname:= "HIPedit"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := ""
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+WS_TABSTOP )
   ::oFont   := Iif( oFont==Nil, ::oParent:oFont, oFont )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bSetGet := bSetGet
   DEFAULT aValue := {0,0,0,0}
   ::aValue  := aValue
   ::bGetFocus  := bGetFocus
   ::bKillFocus := bKillFocus

   ::Activate()

   ::oParent:AddControl( Self )

   IF Valtype(bSetGet) == "B"
      // WriteLog("hIpEdit:New() -> bSetGet == Block")
      ::oParent:AddEvent( IPN_FIELDCHANGED,::id,{|o,id|__Valid(o:FindControl(id))} ,.t.)
   ELSE
      // WriteLog("hIpEdit:New() -> bSetGet != Block")
      IF Valtype(::bLostFocus) == "B"
         ::oParent:AddEvent( IPN_FIELDCHANGED,::id,::bLostFocus, .t. )
      ENDIF
   ENDIF

   // Notificacoes de Ganho e perda de foco
   ::oParent:AddEvent( EN_SETFOCUS , ::id, {|o,id|__GetFocus(o:FindControl(id))} )
   ::oParent:AddEvent( EN_KILLFOCUS, ::id, {|o,id|__KillFocus(o:FindControl(id))} )
   

Return Self

METHOD Activate CLASS HIPedit
   IF ::oParent:handle != 0
      ::handle := InitIPAddress ( ::oParent:handle, ::id, ::style ,;
                  ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init() CLASS HIPedit
Local i

   IF !::lInit
      Super:Init()
      ::SetValue(::aValue)
      ::lInit := .t.
   ENDIF

Return Nil

METHOD SetValue( aValue ) CLASS HIPedit
   SETIPADDRESS(::handle , aValue[1], aValue[2], aValue[3], aValue[4])
   ::aValue := aValue
   // writelog( "SetValue()" )
Return Nil


METHOD GetValue( ) CLASS HIPedit
   ::aValue := GETIPADDRESS(::handle)
   // writelog(  )
Return (::aValue)

METHOD Clear( ) CLASS HIPedit
   CLEARIPADDRESS(::handle)
   // writelog(  )
   ::aValue := { 0,0,0,0 }
Return (::aValue)


METHOD End() CLASS HIPedit

   // Nothing to do here, yet!

Return Nil


Static Function __Valid( oCtrl )
   //WriteLog("Entrando em valid do IP")

   oCtrl:aValue := oCtrl:GetValue()

   IF Valtype(oCtrl:bSetGet) == "B" 
      Eval( oCtrl:bSetGet,oCtrl:aValue )
   ENDIF

   IF Valtype(oCtrl:bLostFocus) == "B" .AND. !Eval( oCtrl:bLostFocus, oCtrl:aValue, oCtrl )
      SetFocus( oCtrl:handle )
   ENDIF

   //WriteLog("Saindo de valid do IP")

Return .T.

Static Function __GetFocus( oCtrl )
   Local xRet
   WriteLog("Entrando em GetFocus do IP")

   IF Valtype(oCtrl:bGetFocus) == "B" 
      xRet := Eval( oCtrl:bGetFocus,oCtrl )
   ENDIF

   WriteLog("Saindo de GetFocus do IP")

Return xRet


Static Function __KillFocus( oCtrl )
   Local xRet
   WriteLog("Entrando em KillFocus do IP")

   IF Valtype(oCtrl:bKillFocus) == "B" 
      xRet := Eval( oCtrl:bKillFocus,oCtrl )
   ENDIF

   WriteLog("Saindo de KillFocus do IP")

Return xRet
