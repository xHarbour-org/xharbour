/*
 * HWIDE
 * HControlGen class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define UDS_SETBUDDYINT     2
#define UDS_ALIGNRIGHT      4

Static aBDown := { Nil,0,0,.F. }

CLASS HControlGen INHERIT HControl

   DATA lProcess INIT .T.

ENDCLASS


//- HEditGen

CLASS HEditGen INHERIT HControlGen

   CLASS VAR winclass   INIT "EDIT"
   DATA bSetGet

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HEditGen

   // ::classname:= "HEDITGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ),WS_VISIBLE+WS_CHILD+WS_BORDER )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt


   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HEditGen
   IF ::oParent:handle != 0
      ::handle := CreateEdit( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HEditGen
   Super:Init()
   Hwgi_InitEditProc( ::handle )
Return Nil

//- HUpDownGen

CLASS HUpDownGen INHERIT HControlGen

   CLASS VAR winclass   INIT "EDIT"
   DATA bSetGet
   DATA hUpDown, idUpDown

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HUpDownGen

   // ::classname:= "HUPDOWNGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::idUpDown:= ::NewId()
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ),WS_VISIBLE+WS_CHILD )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt


   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HUpDownGen
   IF ::oParent:handle != 0
      ::handle := CreateEdit( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HUpDownGen
   Super:Init()
   Hwgi_InitEditProc( ::handle )
   ::hUpDown := CreateUpDownControl( ::oParent:handle, ::idUpDown, ;
       UDS_SETBUDDYINT+UDS_ALIGNRIGHT,0,0,0,0,::handle,10,0,0 )
   EnableWindow( ::hUpDown,.F. )
Return Nil

//- HDateGen

CLASS HDateGen INHERIT HControlGen

   CLASS VAR winclass   INIT "SYSDATETIMEPICK32"
   DATA bSetGet

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HDateGen

   // ::classname:= "HDATEGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ),WS_VISIBLE+WS_CHILD )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt

   HWG_InitCommonControlsEx()
   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HDateGen
   IF ::oParent:handle != 0
      ::handle := CreateDatePicker( ::oParent:handle, ::id, ;
                  ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HDateGen
   Super:Init()
   Hwgi_InitDateProc( ::handle )
Return Nil


//- HButtonGen

CLASS HButtonGen INHERIT HControlGen

   CLASS VAR winclass   INIT "BUTTON"
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HButtonGen

   // ::classname:= "HBUTTONGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_PUSHBUTTON+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HButtonGen
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HButtonGen
   Super:Init()
   Hwgi_InitButtonProc( ::handle )
Return Nil

//- HCheckBoxGen

CLASS HCheckBoxGen INHERIT HControlGen

   CLASS VAR winclass   INIT "BUTTON"
   DATA bSetGet
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HCheckBoxGen

   // ::classname:= "HCHECKBOXGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_AUTOCHECKBOX+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HCheckBoxGen
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HCheckBoxGen
   Super:Init()
   Hwgi_InitCheckProc( ::handle )
Return Nil

//- HRadioBoxGen

CLASS HRadioButtonGen INHERIT HControlGen

   CLASS VAR winclass   INIT "BUTTON"
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HRadioButtonGen

   // ::classname:= "HRADIOBUTTONGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), BS_AUTORADIOBUTTON+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HRadioButtonGen
   IF ::oParent:handle != 0
      ::handle := CreateButton( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight, ::title )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HRadioButtonGen
   Super:Init()
   Hwgi_InitRadioProc( ::handle )
Return Nil

//- HComboGen

CLASS HComboGen INHERIT HControlGen

   CLASS VAR winclass   INIT "COMBOBOX"
   DATA bSetGet
   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor )
   METHOD Activate()
   METHOD Init()

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight,cCaption,oFont, ;
                  ctoolt,tcolor,bcolor ) CLASS HComboGen

   // ::classname:= "HCOMBOGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := cCaption
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), CBS_DROPDOWNLIST+WS_CHILD+WS_VISIBLE )
   ::oFont   := oFont
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::tooltip := ctoolt
   ::SetColor( tcolor,bcolor )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HComboGen
   IF ::oParent:handle != 0
      ::handle := CreateCombo( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HComboGen
   Super:Init()
   Hwgi_InitComboProc( ::handle )
Return Nil

// HPanelGen

CLASS HPanelGen INHERIT HControl

   DATA winclass   INIT "STATIC"

   METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight )
   METHOD Activate()
   METHOD Paint()

ENDCLASS

METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight ) CLASS HPanelGen

   // ::classname:= "HPANELGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := SS_OWNERDRAW+WS_CHILD+WS_VISIBLE
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bPaint  := {|o,lp|o:Paint(lp)}

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS HPanelGen
   IF ::oParent:handle != 0
      ::handle := CreateStatic( ::oParent:handle, ::id,            ;
                  ::style, ::nLeft, ::nTop, ::nWidth,::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Paint( lpdis ) CLASS HPanelGen
Local drawInfo := GetDrawItemInfo( lpdis )
Local hDC := drawInfo[3], x1 := drawInfo[4], y1 := drawInfo[5], x2 := drawInfo[6], y2 := drawInfo[7]
Local oPenLight, oPenGray

   oPenLight := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DHILIGHT) )
   SelectObject( hDC, oPenLight:handle )
   DrawLine( hDC, 5,1,x2-5,1 )
   oPenGray := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DSHADOW) )
   SelectObject( hDC, oPenGray:handle )
   DrawLine( hDC, 5,0,x2-5,0 )

   oPenGray:Release()
   oPenLight:Release()

Return Nil

// HOwnButGen

CLASS HOwnButGen INHERIT HControl

   DATA winclass   INIT "STATIC"

   METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight )
   METHOD Activate()
   METHOD Paint()

ENDCLASS

METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight ) CLASS HOwnButGen

   // ::classname:= "HOWNBUTGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := SS_OWNERDRAW+WS_CHILD+WS_VISIBLE
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bPaint  := {|o,lp|o:Paint(lp)}

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS HOwnButGen
   IF ::oParent:handle != 0
      ::handle := CreateStatic( ::oParent:handle, ::id,            ;
                  ::style, ::nLeft, ::nTop, ::nWidth,::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Paint( lpdis ) CLASS HOwnButGen
Local drawInfo := GetDrawItemInfo( lpdis )
Local hDC := drawInfo[3], x1 := drawInfo[4], y1 := drawInfo[5], x2 := drawInfo[6], y2 := drawInfo[7]

   DrawButton( hDC, x1, y1, x2, y2, 5 )

Return Nil


// HBrowseGen

CLASS HBrowseGen INHERIT HControl

   DATA winclass   INIT "STATIC"

   METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight )
   METHOD Activate()
   METHOD Paint()

ENDCLASS

METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight ) CLASS HBrowseGen

   // ::classname:= "HBROWSEGEN"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := SS_OWNERDRAW+WS_CHILD+WS_VISIBLE
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bPaint  := {|o,lp|o:Paint(lp)}

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS HBrowseGen
   IF ::oParent:handle != 0
      ::handle := CreateStatic( ::oParent:handle, ::id,            ;
                  ::style, ::nLeft, ::nTop, ::nWidth,::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Paint( lpdis ) CLASS HBrowseGen
Local drawInfo := GetDrawItemInfo( lpdis )
Local hDC := drawInfo[3], x1 := drawInfo[4], y1 := drawInfo[5], x2 := drawInfo[6], y2 := drawInfo[7]
Local oPenGray := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DDKSHADOW) )
Local oPenLight := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DHIGHLIGHT) )

   SelectObject( hDC, oPenGray:handle )
   DrawLine( hDC, x1,y1,x2,y1 )
   DrawLine( hDC, x1,y1+1,x2-1,y1+1 )
   DrawLine( hDC, x1,y1,x1,y2 )
   DrawLine( hDC, x1+1,y1,x1+1,y2-1 )
   SelectObject( hDC, oPenLight:handle )
   DrawLine( hDC, x1+1,y2-1,x2-1,y2-1 )
   DrawLine( hDC, x2-1,y1+1,x2-1,y2-1 )

   oPenGray:Release()
   oPenLight:Release()
   FillRect( hDC, x1+2, y1+2, x2-2, y2-2, COLOR_WINDOW+1 )
   DrawButton( hDC, x1+2, y1+2, x1+20, y1+20, 5 )
   DrawButton( hDC, x1+21, y1+2, x1+70, y1+20, 5 )
   DrawButton( hDC, x1+2, y1+21, x1+20, y1+50, 5 )

Return Nil


Function DefButtonGenProc( hCtrl, msg, wParam, lParam )
Local oParent, oCtrl
   // writelog( "DefButtonGenProc: " + Str(hCtrl,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   oParent := FindParent( hCtrl )
   oCtrl := oParent:FindControl(,hCtrl)
   IF msg == WM_MOUSEMOVE
      IF aBDown[1] != Nil
         IF aBDown[4] > 0
            CtrlResize( aBDown[1],LoWord(lParam)+oCtrl:nLeft,HiWord(lParam)+oCtrl:nTop )
         ELSE
            CtrlMove( aBDown[1],LoWord(lParam)+oCtrl:nLeft,HiWord(lParam)+oCtrl:nTop,.T. )
         ENDIF
      ENDIF
   ELSEIF msg == WM_LBUTTONDOWN
      SetBDown( oCtrl,LoWord(lParam)+oCtrl:nLeft, HiWord(lParam)+oCtrl:nTop,0 )
      Return 0
   ELSEIF msg == WM_LBUTTONUP
      CtrlButtonUp( oCtrl,LoWord( lParam ), HiWord( lParam ) )
      Return 0
   ELSEIF msg == WM_RBUTTONUP
      RButtonUp( oCtrl:oParent, LoWord( lParam ) + oCtrl:nLeft, ;
                                HiWord( lParam ) + oCtrl:nTop )
      Return 0
   ELSEIF msg == WM_LBUTTONDBLCLK
      SetStyle( oCtrl:oParent )
      Return 0
   ELSEIF msg == WM_KEYUP
      IF wParam == 40        // Down
         IF ( oCtrl := GetCtrlSelected( oCtrl:oParent ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,0,1,.F. )
         ENDIF
      ELSEIF wParam == 38    // Up
         IF ( oCtrl := GetCtrlSelected( oCtrl:oParent ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,0,-1,.F. )
         ENDIF
      ELSEIF wParam == 39    // Right
         IF ( oCtrl := GetCtrlSelected( oCtrl:oParent ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,1,0,.F. )
         ENDIF
      ELSEIF wParam == 37    // Left
         IF ( oCtrl := GetCtrlSelected( oCtrl:oParent ) ) != Nil
            SetBDown( ,0,0,0 )
            CtrlMove( oCtrl,-1,0,.F. )
         ENDIF
      ENDIF
      Return 1
   ELSEIF msg == WM_KEYDOWN
      IF wParam == 46    // Del
         DeleteCtrl()
      ENDIF
      Return 1
   ENDIF
Return -1

Static Function CtrlButtonUp( oCtrl, xPos, yPos )
   SetBDown( Nil,xPos,yPos,0 )
Return Nil

Static Function FindParent( hEdit )
Local oParent, hParent := GetParent( hEdit )
   IF ( oParent := Atail( HDialog():aModalDialogs ) ) != Nil .AND. ;
                oParent:handle == hParent
      Return oParent
   ELSEIF ( oParent := HDialog():FindDialog(hParent) ) != Nil .AND.;
                oParent:handle == hParent
      Return oParent
   ELSEIF ( oParent := HWindow():FindWindow(hParent) ) != Nil .AND.;
                oParent:handle == hParent
      Return oParent
   ENDIF
Return Nil

Function CtrlMove( oCtrl,xPos,yPos,lMouse )

   IF xPos != aBDown[2] .OR. yPos != aBDown[3]
      IF lMouse .AND. Abs( xPos - aBDown[2] ) < 3 .AND. Abs( yPos - aBDown[3] ) < 3 
         Return Nil
      ENDIF
      InvalidateRect( oCtrl:oParent:handle, 1, ;
               oCtrl:nLeft-4, oCtrl:nTop-4, ;
               oCtrl:nLeft+oCtrl:nWidth+3,  ;
               oCtrl:nTop+oCtrl:nHeight+3 )
      oCtrl:nLeft += xPos - aBDown[2]
      oCtrl:nTop  += yPos - aBDown[3]
      aBDown[2] := xPos
      aBDown[3] := yPos
      InvalidateRect( oCtrl:oParent:handle, 0, ;
               oCtrl:nLeft-4, oCtrl:nTop-4, ;
               oCtrl:nLeft+oCtrl:nWidth+3,  ;
               oCtrl:nTop+oCtrl:nHeight+3 )
      MoveWindow( oCtrl:handle, oCtrl:nLeft, oCtrl:nTop, oCtrl:nWidth, oCtrl:nHeight )
      IF oCtrl:classname() == "HUPDOWNGEN"
         MoveWindow( oCtrl:hUpDown, oCtrl:nLeft+oCtrl:nWidth-20, oCtrl:nTop, 20, oCtrl:nHeight )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
      ShowCtrlPos( oCtrl )
   ENDIF
Return Nil

Function CtrlResize( oCtrl,xPos,yPos )
   IF xPos != aBDown[2] .OR. yPos != aBDown[3]
      InvalidateRect( oCtrl:oParent:handle, 1, ;
               oCtrl:nLeft-4, oCtrl:nTop-4, ;
               oCtrl:nLeft+oCtrl:nWidth+3,  ;
               oCtrl:nTop+oCtrl:nHeight+3 )
      IF aBDown[4] == 1
         oCtrl:nLeft += xPos - aBDown[2]
         oCtrl:nWidth -= xPos - aBDown[2]
      ELSEIF aBDown[4] == 2
         oCtrl:nTop  += yPos - aBDown[3]
         oCtrl:nHeight  -= yPos - aBDown[3]
      ELSEIF aBDown[4] == 3
         oCtrl:nWidth += xPos - aBDown[2]
      ELSEIF aBDown[4] == 4
         oCtrl:nHeight  += yPos - aBDown[3]
      ENDIF
      aBDown[2] := xPos
      aBDown[3] := yPos
      InvalidateRect( oCtrl:oParent:handle, 0, ;
               oCtrl:nLeft-4, oCtrl:nTop-4, ;
               oCtrl:nLeft+oCtrl:nWidth+3,  ;
               oCtrl:nTop+oCtrl:nHeight+3 )
      MoveWindow( oCtrl:handle, oCtrl:nLeft, oCtrl:nTop, oCtrl:nWidth, oCtrl:nHeight )
      IF oCtrl:classname() == "HUPDOWNGEN"
         MoveWindow( oCtrl:hUpDown, oCtrl:nLeft+oCtrl:nWidth-20, oCtrl:nTop, 20, oCtrl:nHeight )
      ENDIF
      oCtrl:oParent:oParent:lChanged := .T.
      ShowCtrlPos( oCtrl )
   ENDIF
Return Nil

Function SetBDown( oCtrl,xPos,yPos,nBorder )
   aBDown[1] := oCtrl
   aBDown[2]  := xPos
   aBDown[3]  := yPos
   aBDown[4] := nBorder
   IF oCtrl != Nil
      SetCtrlSelected( oCtrl:oParent,oCtrl )
   ENDIF
Return Nil

Function GetBDown
Return aBDown

Function SetCtrlSelected( oDlg,oCtrl )
Local oFrm := oDlg:oParent, handle

   IF ( oFrm:oCtrlSelected == Nil .AND. oCtrl != Nil ) .OR. ;
        ( oFrm:oCtrlSelected != Nil .AND. oCtrl == Nil ) .OR. ;
        ( oFrm:oCtrlSelected != Nil .AND. oCtrl != Nil .AND. ;
        oFrm:oCtrlSelected:handle != oCtrl:handle )
      handle := Iif( oCtrl!=Nil,oCtrl:oParent:handle, ;
                        oFrm:oCtrlSelected:oParent:handle )
      IF oFrm:oCtrlSelected != Nil
         InvalidateRect( oFrm:oCtrlSelected:oParent:handle, 1, ;
                  oFrm:oCtrlSelected:nLeft-4, oFrm:oCtrlSelected:nTop-4, ;
                  oFrm:oCtrlSelected:nLeft+oFrm:oCtrlSelected:nWidth+3,  ;
                  oFrm:oCtrlSelected:nTop+oFrm:oCtrlSelected:nHeight+3 )
      ENDIF
      oFrm:oCtrlSelected := oCtrl
      IF oCtrl != Nil
         InvalidateRect( oCtrl:oParent:handle, 0, ;
                  oCtrl:nLeft-4, oCtrl:nTop-4, ;
                  oCtrl:nLeft+oCtrl:nWidth+3,  ;
                  oCtrl:nTop+oCtrl:nHeight+3 )
      ENDIF
      ShowCtrlPos( oCtrl )
      SendMessage( handle,WM_PAINT,0,0 )
   ENDIF
Return Nil

Function GetCtrlSelected( oDlg )
Return Iif( oDlg!=Nil,oDlg:oParent:oCtrlSelected,Nil )

Function CheckResize( oCtrl,xPos,yPos )
   IF xPos > oCtrl:nLeft-5 .AND. xPos < oCtrl:nLeft+3 .AND. ;
      yPos >= oCtrl:nTop .AND. yPos < oCtrl:nTop + oCtrl:nHeight
      IF oCtrl:classname() != "HLINE" .OR. !oCtrl:lVert
         Return 1
      ENDIF
   ELSEIF xPos > oCtrl:nLeft+oCtrl:nWidth-5 .AND. xPos < oCtrl:nLeft+oCtrl:nWidth+3 .AND. ;
      yPos >= oCtrl:nTop .AND. yPos < oCtrl:nTop + oCtrl:nHeight
      IF oCtrl:classname() != "HLINE" .OR. !oCtrl:lVert
         Return 3
      ENDIF
   ELSEIF yPos > oCtrl:nTop-5 .AND. yPos < oCtrl:nTop+3 .AND. ;
      xPos >= oCtrl:nLeft .AND. xPos < oCtrl:nLeft + oCtrl:nWidth
      IF oCtrl:classname() != "HLINE" .OR. oCtrl:lVert
         Return 2
      ENDIF
   ELSEIF yPos > oCtrl:nTop+oCtrl:nHeight-5 .AND. yPos < oCtrl:nTop+oCtrl:nHeight+3 .AND. ;
      xPos >= oCtrl:nLeft .AND. xPos < oCtrl:nLeft + oCtrl:nWidth
      IF oCtrl:classname() != "HLINE" .OR. oCtrl:lVert
         Return 4
      ENDIF
   ENDIF
Return 0
