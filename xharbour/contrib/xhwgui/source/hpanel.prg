/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HPanel class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HPanel INHERIT HControl

   DATA winclass   INIT "PANEL"

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  bInit,bSize,bPaint,lDocked )
   METHOD Activate()
   METHOD Init()
   METHOD Redefine( oWndParent,nId,nHeight,bInit,bSize,bPaint,lDocked )
   METHOD Paint()
   METHOD End()

ENDCLASS


METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  bInit,bSize,bPaint,lDocked ) CLASS HPanel

   // ::classname:= "HPANEL"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := Iif( nWidth==Nil,0,nWidth )
   ::nHeight := nHeight
   ::oFont   := ::oParent:oFont
   ::bInit   := bInit
   IF bSize == Nil
      ::bSize := {|o,x,y|MoveWindow(o:handle,0,0,Iif(::nHeight!=0.and.(::nWidth>::nHeight.or.::nWidth==0),x,::nWidth),Iif(::nWidth!=0.and.(::nHeight>::nWidth.or.::nHeight==0),y,::nHeight))}
   ELSE
      ::bSize   := bSize
   ENDIF
   ::bPaint  := bPaint
   IF __ObjHasMsg( ::oParent,"AOFFSET" ) .AND. ::oParent:type == WND_MDI
      IF ::nWidth > ::nHeight .OR. ::nWidth == 0
         ::oParent:aOffset[2] := ::nHeight
      ELSEIF ::nHeight > ::nWidth .OR. ::nHeight == 0
         IF ::nLeft == 0
            ::oParent:aOffset[1] := ::nWidth
         ELSE
            ::oParent:aOffset[3] := ::nWidth
         ENDIF
      ENDIF
   ENDIF

   ::oParent:AddControl( Self )
   ::Activate()

Return Self

METHOD Activate CLASS HPanel
Local handle := ::oParent:handle, oClient

   IF handle != 0
      ::handle := CreatePanel( handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init CLASS HPanel
   Super:Init()
   SetWindowObject( ::handle,Self )
   Hwg_InitPanelProc( ::handle )
Return Nil


METHOD Redefine( oWndParent,nId,nHeight,bInit,bSize,bPaint,lDocked ) CLASS HPanel
   // ::classname:= "HPANEL"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := nId
   ::style   := ::nLeft := ::nTop := ::nWidth := 0
   ::oFont   := ::oParent:oFont
   ::bInit   := bInit
   ::bSize   := bSize
   ::nHeight := IIF( nHeight!=Nil,nHeight,0 )

   ::oParent:AddControl( Self )

Return Self

METHOD Paint() CLASS HPanel
Local pps, hDC, aCoors, oPenLight, oPenGray

   IF ::bPaint != Nil
      Eval( ::bPaint,Self )
   ELSE
      pps := DefinePaintStru()
      hDC := BeginPaint( ::handle, pps )
      aCoors := GetClientRect( ::handle )

      oPenLight := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DHILIGHT) )
      SelectObject( hDC, oPenLight:handle )
      DrawLine( hDC, 5,1,aCoors[3]-5,1 )
      oPenGray := HPen():Add( BS_SOLID,1,GetSysColor(COLOR_3DSHADOW) )
      SelectObject( hDC, oPenGray:handle )
      DrawLine( hDC, 5,0,aCoors[3]-5,0 )

      oPenGray:Release()
      oPenLight:Release()
      EndPaint( ::handle, pps )
   ENDIF

Return Nil

METHOD End() CLASS HPanel
Local aControls := ::aControls, nControls := Len( aControls ), i
   FOR i := 1 TO nControls
      IF __ObjHasMsg( aControls[i],"END" )
         aControls[i]:End()
      ENDIF
   NEXT
Return Nil


FUNCTION PanelProc( hPanel, msg, wParam, lParam )
Local oPanel
   // WriteLog( "Panel: "+Str(hPanel,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   if msg != WM_CREATE
      /*
      if ( oPanel := FindSelf( hPanel ) ) == Nil
         Return .F.
      endif
      */
      oPanel := GetWindowObject( hPanel )
      if msg == WM_PAINT
         oPanel:Paint()
      elseif msg == WM_CTLCOLORSTATIC
         Return DlgCtlColor( oPanel,wParam,lParam )
      else
         DefProc( oPanel, msg, wParam, lParam )
      endif
   endif
RETURN -1
