/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HProgressBar class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

CLASS HProgressBar INHERIT HControl

   CLASS VAR winclass   INIT "msctls_progress32"
   DATA  maxPos
   DATA  lNewBox
   DATA  nCount INIT 0
   DATA  nLimit

   METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight,maxPos,nRange )
   METHOD NewBox( cTitle,nLeft,nTop,nWidth,nHeight,maxPos,nRange )
   METHOD Activate()
   METHOD Increment() INLINE UpdateProgressBar( ::handle )
   METHOD Step()
   METHOD Set( cTitle,nPos )
   METHOD End()

ENDCLASS

METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight,maxPos,nRange ) CLASS HProgressBar

   // ::classname:= "HPROGRESSBAR"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::style   := WS_CHILD+WS_VISIBLE
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::maxPos  := Iif( maxPos==Nil,20,maxPos )
   ::lNewBox := .F.
   ::nLimit := Iif( nRange != Nil,Int( nRange/::maxPos ),1 )

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD NewBox( cTitle,nLeft,nTop,nWidth,nHeight,maxPos,nRange ) CLASS HProgressBar

   // ::classname:= "HPROGRESSBAR"
   ::style   := WS_CHILD+WS_VISIBLE
   nWidth := Iif( nWidth==Nil,220,nWidth )
   nHeight := Iif( nHeight==Nil,55,nHeight )
   nLeft   := Iif( nLeft==Nil,0,nLeft )
   nTop    := Iif( nTop==Nil,0,nTop )
   nWidth  := Iif( nWidth==Nil,220,nWidth )
   nHeight := Iif( nHeight==Nil,55,nHeight )
   ::nLeft := 20
   ::nTop  := 25
   ::nWidth  := nWidth-40
   ::maxPos  := Iif( maxPos==Nil,20,maxPos )
   ::lNewBox := .T.
   ::nLimit := Iif( nRange != Nil,Int( nRange/::maxPos ),1 )

   INIT DIALOG ::oParent TITLE cTitle       ;
        AT nLeft,nTop SIZE nWidth,nHeight   ;
        STYLE WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+WS_SIZEBOX+Iif( nTop==0,DS_CENTER,0 )

   ACTIVATE DIALOG ::oParent NOMODAL

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HProgressBar
   IF ::oParent:handle != 0
      ::handle := CreateProgressBar( ::oParent:handle, ::maxPos, ;
                  ::nLeft, ::nTop, ::nWidth )
      ::Init()
   ENDIF
Return Nil

METHOD Step()

   ::nCount ++
   IF ::nCount == ::nLimit
      ::nCount := 0
      UpdateProgressBar( ::handle )
   ENDIF

Return Nil

METHOD Set( cTitle,nPos ) CLASS HProgressBar

   IF cTitle != Nil
      SetWindowText( ::oParent:handle,cTitle )
   ENDIF
   IF nPos != Nil
      SetProgressBar( ::handle,nPos )
   ENDIF

Return Nil

METHOD End()

   DestroyWindow( ::handle )
   IF ::lNewBox
      EndDialog( ::oParent:handle )
   ENDIF

Return Nil

