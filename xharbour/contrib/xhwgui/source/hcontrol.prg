/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HControl class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define  CONTROL_FIRST_ID   34000


//- HControl

CLASS HControl INHERIT HCustomWindow

   DATA id
   DATA tooltip
   DATA lInit    INIT .F.

   METHOD Init()
   METHOD SetColor( tcolor,bcolor,lRepaint )
   METHOD NewId()
   METHOD Move( x1,y1,width,height )
   
   METHOD Disable()	INLINE EnableWindow( ::handle, .F. )
   METHOD Enable()	INLINE EnableWindow( ::handle, .T. )
   METHOD IsEnabled()   INLINE IsWindowEnabled( ::Handle )
   METHOD SetFocus()	INLINE SetFocus( ::handle  )   

ENDCLASS

METHOD NewId() CLASS HControl
Local nId := CONTROL_FIRST_ID + Len( ::oParent:aControls )

   IF Ascan( ::oParent:aControls, {|o|o:id==nId} ) != 0
      nId --
      DO WHILE nId >= CONTROL_FIRST_ID .AND. Ascan( ::oParent:aControls, {|o|o:id==nId} ) != 0
         nId --
      ENDDO
   ENDIF
Return nId

METHOD INIT CLASS HControl
Local i

   IF !::lInit
      IF ::tooltip != Nil
         i := AddToolTip( ::oParent:handle, ::id, ::tooltip )
      ENDIF
      IF ::oFont != Nil
         SetCtrlFont( ::oParent:handle, ::id, ::oFont:handle )
      ELSEIF ::oParent:oFont != Nil
         SetCtrlFont( ::oParent:handle, ::id, ::oParent:oFont:handle )
      ENDIF
      IF ::bInit != Nil
         Eval( ::bInit, Self )
      ENDIF
      ::lInit := .T.
   ENDIF
RETURN Nil

METHOD SetColor( tcolor,bcolor,lRepaint ) CLASS HControl
   IF tcolor != Nil
      ::tcolor  := tcolor
   ENDIF
   IF tColor != Nil .AND. bColor == Nil .AND. ::bColor == Nil
      bColor := GetSysColor( COLOR_3DFACE )
   ENDIF
   IF bcolor != Nil
      ::bcolor  := bcolor
      IF bColor != Nil
         IF ::brush != Nil
            ::brush:Release()
         ENDIF
         ::brush := HBrush():Add( bcolor )
      ENDIF
   ENDIF
   IF lRepaint != Nil .AND. lRepaint
      RedrawWindow( ::handle, RDW_ERASE + RDW_INVALIDATE )
   ENDIF
Return Nil

METHOD Move( x1,y1,width,height )  CLASS HControl

   IF x1 != Nil
      ::nLeft := x1
   ENDIF
   IF y1 != Nil
      ::nTop  := y1
   ENDIF
   IF width != Nil
      ::nWidth := width
   ENDIF
   IF height != Nil
      ::nHeight := height
   ENDIF
   MoveWindow( ::handle,::nLeft,::nTop,::nWidth,::nHeight )

Return Nil

//- HStatus

CLASS HStatus INHERIT HControl

   CLASS VAR winclass   INIT "STATUS"
   DATA aParts
   METHOD New( oWndParent,nId,nStyle,oFont,aParts,bInit,bSize,bPaint )
   METHOD Activate()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,oFont,aParts,bInit,bSize,bPaint ) CLASS HStatus

   // ::classname:= "HSTATUS"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := nStyle
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := Iif( bSize!=Nil, bSize, {|o,x,y|MoveWindow(o:handle,0,y-20,x,y)} )
   ::bPaint  := bPaint
   ::aParts  := aParts

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HStatus
Local aCoors

   IF ::oParent:handle != 0
      ::handle := CreateStatusWindow( ::oParent:handle, ::id, Len(::aParts), ::aParts )
      aCoors := GetWindowRect( ::handle )
      ::oParent:aOffset[4] := aCoors[4] - aCoors[2]
   ENDIF
Return Nil


//- HStatic

CLASS HStatic INHERIT HControl

   CLASS VAR winclass   INIT "STATIC"

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont,bInit, ;
                  bSize,bPaint,ctoolt,tcolor,bcolor,lTransp )
   METHOD Redefine( oWndParent,nId,oFont,bInit, ;
                  bSize,bPaint,ctoolt,tcolor,bcolor,lTransp )
   METHOD Activate()
   METHOD SetValue(value) INLINE SetDlgItemText( ::oParent:handle,::id,value )

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont,bInit, ;
                  bSize,bPaint,ctoolt,tcolor,bcolor,lTransp ) CLASS HStatic

   // ::classname:= "HSTATIC"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ),WS_VISIBLE+WS_CHILD )
   IF lTransp != Nil .AND. lTransp
      ::extStyle += WS_EX_TRANSPARENT
   ENDIF
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Redefine( oWndParent,nId,cCaption,oFont,bInit, ;
                  bSize,bPaint,ctoolt,tcolor,bcolor,lTransp )  CLASS HStatic
   // ::classname:= "HSTATIC"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::title   := cCaption
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   IF lTransp != Nil .AND. lTransp
      ::extStyle += WS_EX_TRANSPARENT
   ENDIF
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::oParent:AddControl( Self )
Return Self

METHOD Activate CLASS HStatic
   IF ::oParent:handle != 0
      ::handle := CreateStatic( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title, ::extStyle )
      ::Init()
   ENDIF
Return Nil

//- HButton

CLASS HButton INHERIT HControl

   CLASS VAR winclass   INIT "BUTTON"
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Redefine( oWnd,nId,oFont,bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor )

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HButton

   // ::classname:= "HBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_PUSHBUTTON+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := Iif( nWidth==Nil,90,nWidth )
   ::nHeight := Iif( nHeight==Nil,30,nHeight )
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )
   IF bClick != Nil
      ::oParent:AddEvent( 0,::id,bClick )
   ENDIF

Return Self

METHOD Activate CLASS HButton
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Redefine( oWndParent,nId,oFont,bInit,bSize,bPaint,bClick,ctoolt,tcolor,bcolor ) CLASS HButton
   // ::classname:= "HBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::style   := ::nLeft := ::nTop := ::nWidth := ::nHeight := 0
   ::oFont   := oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::oParent:AddControl( Self )
   IF bClick != Nil
      ::oParent:AddEvent( 0,::id,bClick )
   ENDIF
Return Self

//- HGroup

CLASS HGroup INHERIT HControl

   CLASS VAR winclass   INIT "BUTTON"
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption, ;
                  oFont,bInit,bSize,bPaint,tcolor,bcolor )
   METHOD Activate()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption, ;
                  oFont,bInit,bSize,bPaint,tcolor,bcolor ) CLASS HGroup

   // ::classname:= "HGROUP"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_GROUPBOX+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HGroup
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

// hline

CLASS HLine INHERIT HControl

   CLASS VAR winclass   INIT "STATIC"
   DATA lVert
   DATA oPenLight, oPenGray

   METHOD New( oWndParent,nId,lVert,nLeft,nTop,nLength,bSize )
   METHOD Activate()
   METHOD Paint()

ENDCLASS


METHOD New( oWndParent,nId,lVert,nLeft,nTop,nLength,bSize ) CLASS hline

   // ::classname:= "HLine"
   ::title := ""
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := SS_OWNERDRAW+WS_CHILD+WS_VISIBLE
   ::lVert   := lVert
   ::nLeft   := nLeft
   ::nTop    := nTop
   IF lVert
      ::nWidth  := 10
      ::nHeight := Iif( nLength==Nil,20,nLength )
   ELSE
      ::nWidth  := Iif( nLength==Nil,20,nLength )
      ::nHeight := 10
   ENDIF
   ::bSize   := bSize
   ::bPaint  := {|o,lp|o:Paint(lp)}

   ::oPenLight := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DHILIGHT) )
   ::oPenGray  := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DSHADOW) )

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS hline
   IF ::oParent:handle != 0
      ::handle := CreateStatic( ::oParent:handle, ::id,            ;
                  ::style, ::nLeft, ::nTop, ::nWidth,::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Paint( lpdis ) CLASS hline
Local drawInfo := GetDrawItemInfo( lpdis )
Local hDC := drawInfo[3], x1 := drawInfo[4], y1 := drawInfo[5], x2 := drawInfo[6], y2 := drawInfo[7]

   SelectObject( hDC, ::oPenLight:handle )
   IF ::lVert
      // DrawEdge( hDC,x1,y1,x1+2,y2,EDGE_SUNKEN,BF_RIGHT )
      DrawLine( hDC, x1+1,y1,x1+1,y2 )
   ELSE
      // DrawEdge( hDC,x1,y1,x2,y1+2,EDGE_SUNKEN,BF_RIGHT )
      DrawLine( hDC, x1,y1+1,x2,y1+1 )
   ENDIF
   
   SelectObject( hDC, ::oPenGray:handle )
   IF ::lVert
      DrawLine( hDC, x1,y1,x1,y2 )
   ELSE
      DrawLine( hDC, x1,y1,x2,y1 )
   ENDIF
   
Return Nil
