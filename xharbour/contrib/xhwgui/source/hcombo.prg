/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HCombo class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define CB_ERR              (-1)
#define CBN_SELCHANGE       1
#define CBN_DBLCLK          2
#define CBN_SETFOCUS        3
#define CBN_KILLFOCUS       4
#define CBN_EDITCHANGE      5
#define CBN_EDITUPDATE      6
#define CBN_DROPDOWN        7
#define CBN_CLOSEUP         8
#define CBN_SELENDOK        9
#define CBN_SELENDCANCEL    10


CLASS HComboBox INHERIT HControl

   CLASS VAR winclass   INIT "COMBOBOX"
   DATA  aItems
   DATA  bSetGet
   DATA  value    INIT 1
   DATA  bChangeSel

   METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  aItems,oFont,bInit,bSize,bPaint,bChange,cTooltip )
   METHOD Activate()
   METHOD Redefine( oWnd,nId,vari,bSetGet,aItems,oFont,bInit,bSize,bDraw,bChange,cTooltip )
   METHOD Init( aCombo, nCurrent )

ENDCLASS

METHOD New( oWndParent,nId,vari,bSetGet,nStyle,nLeft,nTop,nWidth,nHeight,aItems,oFont, ;
                  bInit,bSize,bPaint,bChange,cTooltip ) CLASS HComboBox

   // ::classname:= "HCOMBOBOX"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::value   := Iif( vari==Nil .OR. Valtype(vari)!="N",1,vari )
   ::bSetGet := bSetGet
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), CBS_DROPDOWNLIST+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::aItems  := aItems
   ::tooltip := cTooltip

   ::Activate()
   ::oParent:AddControl( Self )

   IF bSetGet != Nil
      ::bChangeSel := bChange
      ::oParent:AddEvent( CBN_SELCHANGE,::id,{|o,id|__Valid(o:FindControl(id))} )
   ENDIF

Return Self

METHOD Activate CLASS HComboBox
   IF ::oParent:handle != 0
      ::handle := CreateCombo( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Redefine( oWndParent,nId,vari,bSetGet,aItems,oFont,bInit,bSize,bPaint, ;
                  bChange,cTooltip ) CLASS HComboBox
   // ::classname:= "HCOMBOBOX"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::value   := Iif( vari==Nil .OR. Valtype(vari)!="N",1,vari )
   ::bSetGet := bSetGet
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::aItems  := aItems
   ::tooltip := cTooltip

   ::oParent:AddControl( Self )
   IF bSetGet != Nil
      ::bChangeSel := bChange
      ::oParent:AddEvent( CBN_SELCHANGE,::id,{|o,id|__Valid(o:FindControl(id))} )
   ENDIF
Return Self

METHOD Init() CLASS HComboBox
Local i

   IF !::lInit
      Super:Init()
      IF ::aItems != Nil
         IF ::value == Nil
            ::value := 1
         ENDIF
         SendMessage( ::handle, CB_RESETCONTENT, 0, 0)
         FOR i := 1 TO Len( ::aItems )
            ComboAddString( ::handle, ::aItems[i] )
         NEXT
         ComboSetString( ::handle, ::value )
      ENDIF
   ENDIF
Return Nil

Static Function __Valid( oCtrl )

   oCtrl:value := SendMessage( oCtrl:handle,CB_GETCURSEL,0,0 ) + 1

   IF oCtrl:bSetGet != Nil
      Eval( oCtrl:bSetGet,oCtrl:value )
   ENDIF
   IF oCtrl:bChangeSel != Nil
      Eval( oCtrl:bChangeSel, oCtrl:value, oCtrl )
   ENDIF

Return .T.
