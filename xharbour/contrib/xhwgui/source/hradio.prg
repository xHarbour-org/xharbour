/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HRadioButton class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HRadioGroup INHERIT HObject
   CLASS VAR oGroupCurrent
   DATA aButtons
   DATA value  INIT 1
   DATA bSetGet

   METHOD New( vari,bSetGet )
   METHOD EndGroup( nSelected )
   METHOD SetValue( nValue )
ENDCLASS

METHOD New( vari,bSetGet ) CLASS HRadioGroup
   ::oGroupCurrent := Self
   ::aButtons := {}

   IF vari != Nil
      IF Valtype( vari ) == "N"
         ::value := vari
      ENDIF
      ::bSetGet := bSetGet
   ENDIF

Return Self

METHOD EndGroup( nSelected )  CLASS HRadioGroup
Local nLen

   IF ::oGroupCurrent != Nil .AND. ( nLen:=Len(::oGroupCurrent:aButtons) ) > 0

      nSelected := Iif( nSelected!=Nil.AND.nSelected<=nLen.AND.nSelected > 0, ;
                        nSelected, ::oGroupCurrent:value )
      IF nSelected != 0 .AND. nSelected <= nlen
         IF ::oGroupCurrent:aButtons[nlen]:handle > 0
            CheckRadioButton( ::oGroupCurrent:aButtons[nlen]:oParent:handle, ;
                  ::oGroupCurrent:aButtons[1]:id,    ;
                  ::oGroupCurrent:aButtons[nLen]:id, ;
                  ::oGroupCurrent:aButtons[nSelected]:id )
         ELSE
            ::oGroupCurrent:aButtons[nLen]:bInit :=                     ;
                &( "{|o|CheckRadioButton(o:oParent:handle," +           ;
                  Ltrim(Str(::oGroupCurrent:aButtons[1]:id)) + "," +    ;
                  Ltrim(Str(::oGroupCurrent:aButtons[nLen]:id)) + "," + ;
                  Ltrim(Str(::oGroupCurrent:aButtons[nSelected]:id)) + ")}" )
         ENDIF
      ENDIF
   ENDIF
   ::oGroupCurrent := Nil
Return Nil

METHOD SetValue( nValue )  CLASS HRadioGroup
Local nLen

   IF ( nLen:=Len(::aButtons) ) > 0 .AND. nValue > 0 .AND. nValue <= nLen
      CheckRadioButton( ::aButtons[nlen]:oParent:handle, ;
            ::aButtons[1]:id,    ;
            ::aButtons[nLen]:id, ;
            ::aButtons[nValue]:id )
   ENDIF
Return Nil


CLASS HRadioButton INHERIT HControl

   CLASS VAR winclass   INIT "BUTTON"
   DATA  oGroup

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Redefine( oWnd,nId,oFont,bInit,bSize,bPaint,bClick,lInit,ctoolt,tcolor,bcolor )

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HRadioButton

   // ::classname:= "HRADIOBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::oGroup  := HRadioGroup():oGroupCurrent
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_AUTORADIOBUTTON+;
                     WS_CHILD+WS_VISIBLE+ ;
                     Iif( ::oGroup != Nil .AND. Empty( ::oGroup:aButtons ),WS_GROUP,0 ) )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::tcolor  := tcolor
   IF tColor != Nil .AND. bColor == Nil
      bColor := GetSysColor( COLOR_3DFACE )
   ENDIF
   ::bcolor  := bcolor
   IF bColor != Nil
      ::brush := HBrush():Add( bcolor )
   ENDIF

   ::Activate()
   ::oParent:AddControl( Self )
   IF bClick != Nil .AND. ( ::oGroup == Nil .OR. ::oGroup:bSetGet == Nil )
      ::oParent:AddEvent( 0,::id,bClick )
   ENDIF
   IF ::oGroup != Nil
      Aadd( ::oGroup:aButtons,Self )
      IF ::oGroup:bSetGet != Nil
         ::bLostFocus := bClick
         ::oParent:AddEvent( BN_CLICKED,::id,{|o,id|__Valid(o:FindControl(id))} )
      ENDIF
   ENDIF

Return Self

METHOD Activate CLASS HRadioButton
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Redefine( oWndParent,nId,oFont,bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HRadioButton
   // ::classname:= "HRADIOBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::oGroup  := HRadioGroup():oGroupCurrent
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::tcolor  := tcolor
   IF tColor != Nil .AND. bColor == Nil
      bColor := GetSysColor( COLOR_3DFACE )
   ENDIF
   ::bcolor  := bcolor
   IF bColor != Nil
      ::brush := HBrush():Add( bcolor )
   ENDIF

   ::oParent:AddControl( Self )
   IF bClick != Nil .AND. ( ::oGroup == Nil .OR. ::oGroup:bSetGet == Nil )
      ::oParent:AddEvent( 0,::id,bClick )
   ENDIF
   IF ::oGroup != Nil
      Aadd( ::oGroup:aButtons,Self )
      IF ::oGroup:bSetGet != Nil
         ::bLostFocus := bClick
         ::oParent:AddEvent( BN_CLICKED,::id,{|o,id|__Valid(o:FindControl(id))} )
      ENDIF
   ENDIF
Return Self

Static Function __Valid( oCtrl )

   oCtrl:oGroup:value := Ascan( oCtrl:oGroup:aButtons,{|o|o:id==oCtrl:id} )
   IF oCtrl:oGroup:bSetGet != Nil
      Eval( oCtrl:oGroup:bSetGet,oCtrl:oGroup:value )
   ENDIF
   IF oCtrl:bLostFocus != Nil
      Eval( oCtrl:bLostFocus, oCtrl:oGroup:value, oCtrl )
   ENDIF

Return .T.