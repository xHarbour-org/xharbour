/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HOwnButton class, which implements owner drawn buttons
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "inkey.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HOwnButton INHERIT HControl

   DATA winclass   INIT "OWNBTN"
   CLASSDATA oSelected   INIT Nil
   DATA lFlat
   DATA state
   DATA bClick
   DATA lPress  INIT .F.
   DATA text,ofont,xt,yt,widtht,heightt
   DATA bitmap,xb,yb,widthb,heightb,lTransp

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  bInit,bSize,bPaint,bClick,lflat,           ;
                  cText,color,font,xt,yt,widtht,heightt,     ;
                  bmp,lResour,xb,yb,widthb,heightb,lTr,      ;
                  cTooltip )

   METHOD Activate()
   METHOD Init()
   METHOD Redefine( oWndParent,nId,bInit,bSize,bPaint,bClick,lflat, ;
                  cText,color,font,xt,yt,widtht,heightt,     ;
                  bmp,lResour,xb,yb,widthb,heightb,lTr,      ;
                  cTooltip )
   METHOD Paint()
   METHOD MouseMove( wParam, lParam )
   METHOD MDown()
   METHOD MUp()
   METHOD Press()   INLINE ( ::lPress := .T., ::MDown() )
   METHOD Release()
   METHOD End()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  bInit,bSize,bPaint,bClick,lflat,           ;
                  cText,color,font,xt,yt,widtht,heightt,     ;
                  bmp,lResour,xb,yb,widthb,heightb,lTr,      ;
                  cTooltip ) CLASS HOwnButton

   // ::classname:= "HOWNBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::lFlat   := lFlat
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::bClick  := bClick
   ::state   := OBTN_INIT
   ::tooltip := cToolTip

   ::text    := cText
   ::tcolor  := color
   ::ofont   := font
   ::xt      := xt
   ::yt      := yt
   ::widtht  := widtht
   ::heightt := heightt

   IF bmp != Nil
      ::bitmap  := Iif( lResour.OR.Valtype(bmp)=="N",HBitmap():AddResource( bmp ), HBitmap():AddFile( bmp ) )
   ENDIF
   ::xb      := xb
   ::yb      := yb
   ::widthb  := widthb
   ::heightb := heightb
   ::lTransp := lTr

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS HOwnButton
   IF ::oParent:handle != 0    
      ::handle := CreateOwnBtn( ::oParent:handle, ::id, ;
                  ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HOwnButton
   // SetWindowObject( ::handle,Self )
   // Hwg_InitOwnbtnProc( ::handle )
   Super:Init()

Return Nil

METHOD Redefine( oWndParent,nId,bInit,bSize,bPaint,bClick,lflat, ;
                  cText,color,font,xt,yt,widtht,heightt,     ;
                  bmp,lResour,xb,yb,widthb,heightb,lTr,      ;
                  cTooltip ) CLASS HOwnButton

   // ::classname:= "HOWNBUTTON"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::style   := ::nLeft := ::nTop := ::nWidth := 0
   ::lFlat   := lFlat
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint 
   ::bClick  := bClick
   ::state   := OBTN_INIT
   ::tooltip := cToolTip

   ::text    := cText
   ::tcolor  := color
   ::ofont   := font
   ::xt      := xt
   ::yt      := yt
   ::widtht  := widtht
   ::heightt := heightt

   IF bmp != Nil
      ::bitmap  := Iif( lResour,HBitmap():AddResource( bmp ), HBitmap():AddFile( bmp ) )
   ENDIF
   ::xb      := xb
   ::yb      := yb
   ::widthb  := widthb
   ::heightb := heightb
   ::lTransp := lTr

   ::oParent:AddControl( Self )
Return Self

METHOD Paint() CLASS HOwnButton
Local pps, hDC
Local aCoors, aMetr, oPen, oldBkColor, x1, y1, x2, y2

   pps := DefinePaintStru()
   hDC := BeginPaint( ::handle, pps )
   aCoors := GetClientRect( ::handle )
   aMetr := GetTextMetric( hDC )

   oldBkColor := SetBkColor( hDC,GetSysColor(COLOR_3DFACE) )
   IF ::state == OBTN_INIT
      ::state := OBTN_NORMAL
   ENDIF
   IF ::lFlat
      IF ::state == OBTN_NORMAL
         DrawButton( hDC, aCoors[1],aCoors[2],aCoors[3],aCoors[4],0 )
      ELSEIF ::state == OBTN_MOUSOVER
         DrawButton( hDC, aCoors[1],aCoors[2],aCoors[3],aCoors[4],1 )
      ELSEIF ::state == OBTN_PRESSED
         DrawButton( hDC, aCoors[1],aCoors[2],aCoors[3],aCoors[4],2 )
      ENDIF
   ELSE
      IF ::state == OBTN_NORMAL
         DrawButton( hDC, aCoors[1],aCoors[2],aCoors[3],aCoors[4],5 )
      ELSEIF ::state == OBTN_PRESSED
         DrawButton( hDC, aCoors[1],aCoors[2],aCoors[3],aCoors[4],6 )
      ENDIF
   ENDIF
   IF ::text != Nil
      IF ::ofont != Nil
         SelectObject( hDC, ::ofont:handle )
      ENDIF
      IF ::tcolor != Nil
         SetTextColor( hDC,::tcolor )
      ENDIF
      x1 := Iif( ::xt!=Nil .AND. ::xt!=0, ::xt, aCoors[1]+2 )
      y1 := Iif( ::yt!=Nil .AND. ::yt!=0, ::yt, ;
                              Round( ( aCoors[4]-aCoors[2]-aMetr[1] ) / 2, 0 ) )
      x2 := Iif( ::widtht!=Nil .AND. ::widtht!=0, ;
                          ::xt+::widtht-1, aCoors[3]-2 )
      y2 := Iif( ::heightt!=Nil .AND. ::heightt!=0, ;
                 ::yt+::heightt-1, y1+aMetr[1] )
      DrawText( hDC, ::text, x1, y1, x2, y2, DT_CENTER )
   ENDIF
   IF ::bitmap != Nil
      IF ::widthb == Nil .OR. ::widthb == 0
         ::widthb := ::bitmap:nWidth
         ::heightb := ::bitmap:nHeight
      ENDIF
      x1 := Iif( ::xb!=Nil .AND. ::xb!=0, ::xb, ;
                 Round( (aCoors[3]-aCoors[1]-::widthb) / 2, 0 ) )
      y1 := Iif( ::yb!=Nil .AND. ::yb!=0, ::yb, ;
                 Round( (aCoors[4]-aCoors[2]-::heightb) / 2, 0 ) )
      IF ::lTransp
         DrawTransparentBitmap( hDC, ::bitmap:handle, x1, y1 )
      ELSE
         DrawBitmap( hDC, ::bitmap:handle,, x1, y1, ::widthb, ::heightb )
      ENDIF
   ENDIF
   SetBkColor( hDC,oldBkColor )
   EndPaint( ::handle, pps )
Return Nil

METHOD MouseMove( wParam, lParam )  CLASS HOwnButton
Local aCoors, xPos, yPos, otmp
Local res := .F.

   IF ::lFlat .AND. ::state != OBTN_INIT
      otmp := SetOwnBtnSelected()
      IF otmp != Nil .AND. otmp:id != ::id .AND. !oTmp:lPress
         otmp:state := OBTN_NORMAL
         InvalidateRect( otmp:handle, 0 )
         PostMessage( otmp:handle, WM_PAINT, 0, 0 )
         SetOwnBtnSelected( Nil )
      ENDIF
      aCoors := GetClientRect( ::handle )
      xPos := LoWord( lParam )
      yPos := HiWord( lParam )
      IF ::state == OBTN_NORMAL
         ::state := OBTN_MOUSOVER
         // aBtn[ CTRL_HANDLE ] := hBtn
         InvalidateRect( ::handle, 0 )
         PostMessage( ::handle, WM_PAINT, 0, 0 )
         SetOwnBtnSelected( Self )
      ENDIF
   ENDIF
Return Nil

METHOD MDown()  CLASS HOwnButton
   IF ::state != OBTN_PRESSED
      ::state := OBTN_PRESSED
      InvalidateRect( ::handle, 0 )
      PostMessage( ::handle, WM_PAINT, 0, 0 )
      SetOwnBtnSelected( Self )
   ENDIF
Return Nil

METHOD MUp() CLASS HOwnButton
   IF ::state == OBTN_PRESSED
      IF !::lPress
         ::state := IIF( ::lFlat,OBTN_MOUSOVER,OBTN_NORMAL )
         InvalidateRect( ::handle, 0 )
         PostMessage( ::handle, WM_PAINT, 0, 0 )
      ENDIF
      IF !::lFlat
         SetOwnBtnSelected( Nil )
      ENDIF
      IF ::bClick != Nil
         Eval( ::bClick, ::oParent, ::id )
      ENDIF
   ENDIF
Return Nil

METHOD Release()  CLASS HOwnButton
   ::lPress := .F.
   ::state := OBTN_NORMAL
   InvalidateRect( ::handle, 0 )
   PostMessage( ::handle, WM_PAINT, 0, 0 )
Return Nil

METHOD End()  CLASS HOwnButton
   IF ::ofont != Nil
       ::ofont:Release()
       ::ofont := Nil
   ENDIF
   IF ::bitmap != Nil
      ::bitmap:Release()
      ::bitmap := Nil
   ENDIF
Return Nil

Function SetOwnBtnSelected( oBtn )
Local otmp := HOwnButton():oSelected
   IF Pcount() > 0
      HOwnButton():oSelected := oBtn
   ENDIF
Return otmp


FUNCTION OwnBtnProc( hBtn, msg, wParam, lParam )
Local i, oBtn
   // WriteLog( "Obtn: "+Str(hBtn,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   if msg != WM_CREATE
      if Ascan( { WM_MOUSEMOVE, WM_PAINT, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK, WM_DESTROY }, msg ) > 0
         if ( oBtn := FindSelf( hBtn ) ) == Nil
            return .F.
         endif
         // oBtn := GetWindowObject( hBtn )
         if msg == WM_PAINT
            oBtn:Paint()
         elseif msg == WM_LBUTTONDOWN
            oBtn:MDown()
         elseif msg == WM_LBUTTONUP
            oBtn:MUp()
         elseif msg == WM_LBUTTONDBLCLK
         elseif msg == WM_MOUSEMOVE
            oBtn:MouseMove( wParam, lParam )
         elseif msg == WM_DESTROY
            oBtn:End()
            return .T.
         endif
      endif
   endif
RETURN .F.

