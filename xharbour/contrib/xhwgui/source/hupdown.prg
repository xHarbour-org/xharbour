/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HUpDown class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#undef  UDS_SETBUDDYINT
#undef  UDS_ALIGNRIGHT
#define UDS_SETBUDDYINT     2
#define UDS_ALIGNRIGHT      4

CLASS HUpDown INHERIT HControl

   CLASS VAR winclass   INIT "EDIT"
   DATA bSetGet
   DATA value
   DATA hUpDown, idUpDown, styleUpDown
   DATA nLower INIT 0
   DATA nUpper INIT 999
   DATA nUpDownWidth INIT 12
   DATA lChanged    INIT .F.

   METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
         oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt,tcolor,bcolor,nUpDWidth,nLower,nUpper )
   METHOD Activate()
   METHOD Init()
   METHOD Refresh()

ENDCLASS

METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
         oFont,bInit,bSize,bPaint,bGfocus,bLfocus,ctoolt,tcolor,bcolor,   ;
         nUpDWidth,nLower,nUpper ) CLASS HUpDown

   // ::classname:= "HUPDOWN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::idUpDown := ::NewId()
   IF vari != Nil
      IF Valtype(vari) != "N"
         vari := 0
         Eval( bSetGet,vari )
      ENDIF
      ::title := Str(vari)
   ENDIF
   ::bSetGet := bSetGet
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+WS_TABSTOP )
   ::styleUpDown := UDS_SETBUDDYINT+UDS_ALIGNRIGHT
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   IF nLower != Nil ; ::nLower := nLower ; ENDIF
   IF nUpper != Nil ; ::nUpper := nUpper ; ENDIF
   IF nUpDWidth != Nil ; ::nUpDownWidth := nUpDWidth ; ENDIF
   ::SetColor( tcolor,Iif( bcolor==Nil,GetSysColor( COLOR_BTNHIGHLIGHT ),bcolor ) )

   ::Activate()
   ::oParent:AddControl( Self )

   IF bSetGet != Nil
      ::bGetFocus := bGFocus
      ::bLostFocus := bLFocus
      ::oParent:AddEvent( EN_SETFOCUS,::id,{|o,id|__When(o:FindControl(id))} )
      ::oParent:AddEvent( EN_KILLFOCUS,::id,{|o,id|__Valid(o:FindControl(id))} )
   ELSE
      IF bGfocus != Nil
         ::oParent:AddEvent( EN_SETFOCUS,::id,bGfocus )
      ENDIF
      IF bLfocus != Nil
         ::oParent:AddEvent( EN_KILLFOCUS,::id,bLfocus )
      ENDIF
   ENDIF

Return Self

METHOD Activate CLASS HUpDown
   IF ::oParent:handle != 0
      ::handle := CreateEdit( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init()  CLASS HUpDown
   IF !::lInit
      Super:Init()
      ::hUpDown := CreateUpDownControl( ::oParent:handle, ::idUpDown, ;
          ::styleUpDown,0,0,::nUpDownWidth,0,::handle,::nUpper,::nLower,Val(::title) )
   ENDIF
Return Nil

METHOD Refresh()  CLASS HUpDown
Local vari

   IF ::bSetGet != Nil
      ::value := Eval( ::bSetGet )
      IF Str(::value) != ::title
         ::title := Str( ::value )
         SetUpDown( ::hUpDown, ::value )
      ENDIF
   ELSE
      SetUpDown( ::hUpDown, Val(::title) )
   ENDIF

Return Nil

Static Function __When( oCtrl )

   oCtrl:Refresh()
   IF oCtrl:bGetFocus != Nil 
      Return Eval( oCtrl:bGetFocus, Eval( oCtrl:bSetGet ), oCtrl )
   ENDIF

Return .T.

Static Function __Valid( oCtrl )

   oCtrl:title := GetEditText( oCtrl:oParent:handle, oCtrl:id )
   oCtrl:value := Val( Ltrim( oCtrl:title ) )
   IF oCtrl:bSetGet != Nil
      Eval( oCtrl:bSetGet,oCtrl:value )
   ENDIF
   IF oCtrl:bLostFocus != Nil .AND. !Eval( oCtrl:bLostFocus, oCtrl:value, oCtrl ) .OR. ;
         oCtrl:value > oCtrl:nUpper .OR. oCtrl:value < oCtrl:nLower
      SetFocus( oCtrl:handle )
   ENDIF

Return .T.
