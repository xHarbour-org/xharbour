/* $Id$ */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugConsole FROM TabPage
  DATA oDebugger
  DATA oOut
  DATA oInput
  DATA aHistory    INIT {}
  DATA nHistoryPos INIT 1
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD Create()
  METHOD Clear()                  INLINE ::oOut:Caption := ""
  METHOD DisableInput()           BLOCK {|o,f| f := o:oInput:HasFocus, o:oInput:Disable( .F. ), f }
  METHOD Do( cCommand )
  METHOD DoIt()
  METHOD EnableInput( lFocus )    INLINE ::oInput:Enable( .T. ), IIf( lFocus, ::oInput:SetFocus, )
  METHOD OnChildGetDlgCode() INLINE DLGC_WANTALLKEYS | DLGC_WANTARROWS | DLGC_WANTCHARS | DLGC_HASSETSEL
  METHOD OnChildKeyDown( hwnd, nMsg, nWParam, nLParam )
  METHOD Out( cText )
  METHOD ShowUp() INLINE NIL
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugConsole
  ::Super:Init( oParent )
  ::oDebugger := oDebugger

  ::Caption     := "   Console  "
  ::Height      := 300
  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugConsole
  ::Super:Create()

  ::oInput := EditBox( Self )
  WITH OBJECT ::oInput
    :Height := 30
    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Bottom := :Parent
    :Dock:Right  := :Parent
    :Create()
  END
  
  ::oOut := EditBox( Self )
  WITH OBJECT ::oOut
    :MultiLine := .T.
    :ReadOnly := .T.
    :VertScroll := .T.
    :AutoVScroll := .T.

    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top    := :Parent
    :Dock:Right  := :Parent
    :Dock:Bottom := ::oInput
    :Create()
  END

  ::DockIt()
  //::Show()
RETURN Self


METHOD Do( cCommand ) CLASS XHDebugConsole
  WITH OBJECT ::oOut
    :SetSel( :LineIndex( :GetLineCount() - 2 ), -1 )
    :ReplaceSel( .F., "" )
  END
  ::Out( "> " + cCommand )
  ::oDebugger:DoConsoleCommand( cCommand )
RETURN Self


METHOD DoIt() CLASS XHDebugConsole
  WITH OBJECT ::oInput
    IF Len( Trim( :Caption ) ) > 0
      ::Do( :Caption )
      AAdd( ::aHistory, :Caption )
      ::nHistoryPos := Len( ::aHistory ) + 1
      :Caption := ""
    ENDIF
  END
RETURN Self


METHOD OnChildKeyDown( hwnd, nMsg, nWParam, nLParam ) CLASS XHDebugConsole

  (hWnd)
  (nMsg)
  (nLParam)
  
  WITH OBJECT ::oInput
    IF nWParam == 13
      ::DoIt( )
      RETURN 0
    ELSEIF nWParam == VK_UP
      IF ::nHistoryPos > 1 .AND. Len( ::aHistory ) > 0
        IF ::nHistoryPos > Len( ::aHistory ) .AND. !Empty( :Caption )
          AAdd( ::aHistory, :Caption )
        ENDIF
        :Caption := ::aHistory[ --::nHistoryPos ]
      ENDIF
    ELSEIF nWParam == VK_DOWN
      IF ::nHistoryPos <= Len( ::aHistory )
        IF ::nHistoryPos++ == Len( ::aHistory )
          :Caption := ""
        ELSE
          :Caption := ::aHistory[ ::nHistoryPos ]
        ENDIF
      ENDIF
    ELSE
      RETURN NIL
    ENDIF
    :SetSel( Len( :Caption ), Len( :Caption ) )
  END
RETURN 0


METHOD Out( cText ) CLASS XHDebugConsole
  WITH OBJECT ::oOut
    :SetSel( -1, -1 )
    :ReplaceSel( .F., cText + Chr( 13 ) + Chr( 10 ) )
  END
RETURN Self
