/*
 *$Id: htab.prg,v 1.2 2003/11/14 07:44:12 alkresin Exp $
 *
 * HWGUI - Harbour Win32 GUI library source code:
 * HTab class
 *
 * Copyright 2002 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"

#define TCM_SETCURSEL           4876     // (TCM_FIRST + 12)
#define TCM_SETCURFOCUS         4912     // (TCM_FIRST + 48)
#define TCM_GETCURFOCUS         4911     // (TCM_FIRST + 47)
#define TCM_GETITEMCOUNT        4868     // (TCM_FIRST + 4)

//- HTab

CLASS HTab INHERIT HControl

   CLASS VAR winclass   INIT "SysTabControl32"
   DATA  aTabs
   DATA  aPages  INIT {}
   DATA  bChange, bChange2
   DATA  oTemp

   METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bSize,bPaint,aTabs,bChange )
   METHOD Activate()
   METHOD Init()
   METHOD SetTab( n )
   METHOD StartPage( cname )
   METHOD EndPage()
   METHOD ChangePage( nPage )
   METHOD HidePage( nPage )
   METHOD ShowPage( nPage )
   METHOD GetActivePage( nFirst,nEnd )
   METHOD  End()

   HIDDEN:
     DATA  nActive           // Active Page

ENDCLASS

METHOD New( oWndParent,nId,nStyle,nLeft,nTop,nWidth,nHeight, ;
                  oFont,bInit,bSize,bPaint,aTabs,bChange ) CLASS HTab

   // ::classname:= "HTAB"
   ::oParent := Iif( oWndParent==Nil, ::oDefaultParent, oWndParent )
   ::id      := Iif( nId==Nil,::NewId(), nId )
   ::title   := ""
   ::style   := Hwg_BitOr( Iif( nStyle==Nil,0,nStyle ), WS_CHILD+WS_VISIBLE+WS_TABSTOP )
   ::oFont   := Iif( oFont==Nil, ::oParent:oFont, oFont )
   ::nLeft   := nLeft
   ::nTop    := nTop
   ::nWidth  := nWidth
   ::nHeight := nHeight
   ::bInit   := bInit
   ::bSize   := bSize
   ::bPaint  := bPaint
   ::aTabs   := Iif( aTabs==Nil,{},aTabs )
   ::bChange := bChange
   ::bChange2 := bChange

   ::Activate()
   ::oParent:AddControl( Self )

Return Self

METHOD Activate CLASS HTab
   IF ::oParent:handle != 0
      ::handle := CreateTabControl( ::oParent:handle, ::id, ;
                  ::style, ::nLeft, ::nTop, ::nWidth, ::nHeight )
      ::Init()
   ENDIF
Return Nil

METHOD Init() CLASS HTab
Local i

   IF !::lInit
      Super:Init()
      InitTabControl( ::handle,::aTabs )
      SetWindowObject( ::handle,Self )
      FOR i := 2 TO Len( ::aPages )
         ::HidePage( i )
      NEXT
      Hwg_InitTabProc( ::handle )
   ENDIF

Return Nil

METHOD SetTab( n ) CLASS HTab
   SendMessage( ::handle, TCM_SETCURFOCUS, n-1, 0 )
   // writelog( str(::handle )+" "+Str(SendMessage(::handle,TCM_GETCURFOCUS,0,0 ))+" "+Str(SendMessage(::handle,TCM_GETITEMCOUNT,0,0 )) )
Return Nil

METHOD StartPage( cname ) CLASS HTab
Local i := Ascan( ::aTabs,cname )
Local lNew := ( i == 0 )

   ::oTemp := ::oDefaultParent
   ::oDefaultParent := Self
   IF lNew
      Aadd( ::aTabs,cname )
      i := Len( ::aTabs )
   ENDIF
   DO WHILE Len( ::aPages ) < i
      Aadd( ::aPages, { Len( ::aControls ),0,lNew } )
   ENDDO
   ::nActive := i

Return Nil

METHOD EndPage() CLASS HTab

   ::aPages[ ::nActive,2 ] := Len( ::aControls ) - ::aPages[ ::nActive,1 ]
   IF ::aPages[ ::nActive,3 ] .AND. ::handle != Nil .AND. ::handle > 0
      AddTab( ::handle,::nActive,::aTabs[::nActive] )
   ENDIF
   IF ::nActive > 1 .AND. ::handle != Nil .AND. ::handle > 0
      ::HidePage( ::nActive )
   ENDIF
   ::nActive := 1

   ::oDefaultParent := ::oTemp
   ::oTemp := Nil

   ::bChange = {|o,n|o:ChangePage(n)}

Return Nil

METHOD ChangePage( nPage ) CLASS HTab

   IF !Empty( ::aPages )

      ::HidePage( ::nActive )

      ::nActive := nPage

      ::ShowPage( ::nActive )

   ENDIF

   IF ::bChange2 != Nil
      Eval( ::bChange2,Self,nPage )
   ENDIF

Return Nil

METHOD HidePage( nPage ) CLASS HTab
Local i, nFirst, nEnd

   nFirst := ::aPages[ nPage,1 ] + 1
   nEnd   := ::aPages[ nPage,1 ] + ::aPages[ nPage,2 ]
   FOR i := nFirst TO nEnd
      ::aControls[i]:Hide()
   NEXT

Return Nil

METHOD ShowPage( nPage ) CLASS HTab
Local i, nFirst, nEnd

   nFirst := ::aPages[ nPage,1 ] + 1
   nEnd   := ::aPages[ nPage,1 ] + ::aPages[ nPage,2 ]
   FOR i := nFirst TO nEnd
      ::aControls[i]:Show()
   NEXT
   FOR i := nFirst TO nEnd
      IF __ObjHasMsg( ::aControls[i],"BSETGET" ) .AND. ::aControls[i]:bSetGet != Nil
         SetFocus( ::aControls[i]:handle )
         Exit
      ENDIF
   NEXT

Return Nil

METHOD GetActivePage( nFirst,nEnd ) CLASS HTab

   IF !Empty( ::aPages )
      nFirst := ::aPages[ ::nActive,1 ] + 1
      nEnd   := ::aPages[ ::nActive,1 ] + ::aPages[ ::nActive,2 ]
   ELSE
      nFirst := 1
      nEnd   := Len( ::aControls )
   ENDIF

Return ::nActive

METHOD End() CLASS HTab
Local aControls := ::aControls, nControls := Len( aControls ), i
   FOR i := 1 TO nControls
      IF __ObjHasMsg( aControls[i],"END" )
         aControls[i]:End()
      ENDIF
   NEXT
Return Nil

Function DefTabProc( hTab, msg, wParam, lParam )
Local oTab, iParHigh := HiWord( wParam ), iParLow := LoWord( wParam ), iItem, res, nCode

   // writelog( "TabProc: " + Str(hTab,10)+"|"+Str(msg,6)+"|"+Str(wParam,10)+"|"+Str(lParam,10) )
   oTab := GetWindowObject( hTab )
   IF msg == WM_COMMAND
      // oTab := FindSelf( hTab )
      // writelog( "DefTabProc "+str(Len(oTab:aEvents)) )
      IF oTab:aEvents != Nil .AND. ;
         ( iItem := Ascan( oTab:aEvents, {|a|a[1]==iParHigh.and.a[2]==iParLow} ) ) > 0
         Eval( oTab:aEvents[ iItem,3 ],oTab,iParLow )
      ENDIF
      Return 1
   ELSEIF msg == WM_NOTIFY
      nCode := GetNotifyCode( lParam )
      IF oTab:aNotify != Nil .AND. ;
         ( iItem := Ascan( oTab:aNotify, {|a|a[1]==nCode.and.a[2]==wParam} ) ) > 0
         IF ( res := Eval( oTab:aNotify[ iItem,3 ],oTab,wParam ) ) != Nil
            Return res
         ENDIF
      ENDIF
   ELSEIF msg == WM_DRAWITEM
      Return DlgDrawItem( oTab,wParam,lParam )
   ELSEIF msg == WM_CTLCOLORSTATIC
      Return DlgCtlColor( oTab,wParam,lParam )
   ELSE
      DefProc( oTab, msg, wParam, lParam )
   ENDIF

Return -1
