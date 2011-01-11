/*
 * $Id$
 */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugCallStack FROM TabPage
  DATA oDebugger
  DATA aCalls
  DATA oList
  DATA lDirty INIT .T.
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD Create()
  METHOD ChangeFrame()
  METHOD SetCalls( aCalls )
  METHOD ShowUp()
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugCallStack
  ::Super:Init( oParent )
  ::oDebugger := oDebugger

  ::Caption := "   Call Stack  "
  
  ::Dock:Left := ::Parent
  ::Dock:Top := ::Parent
  ::Dock:Right := ::Parent
  ::Dock:Bottom := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugCallStack
  ::Super:Create()
  
  WITH OBJECT ::oList := XHDebugList( Self )
    :Height := 150
    :ForeColor   := C_BLACK
    :BackColor   := C_WHITE
    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top    := :Parent
    :Dock:Bottom := :Parent
    :Dock:Right  := :Parent
    :EventHandler[ "OnSelChange" ] := "ChangeFrame"
    :Create()
  END
  
  ::DockIt()
RETURN Self


METHOD ChangeFrame() CLASS XHDebugCallStack
  LOCAL n := ::oList:GetCurSel() - 1
  
  IF n >= 0
    ::oDebugger:Frame( n )
  ENDIF
RETURN Self


METHOD SetCalls( aCalls ) CLASS XHDebugCallStack
  LOCAL i, aCall
  
  ::aCalls := aCalls
  WITH OBJECT ::oList
    :ResetContent()
    FOR i := Len( aCalls ) TO 1 STEP -1
      aCall := aCalls[ i ]
      :AddItem( aCall[ _FUNCTION ] + " (" + LTrim( Str( aCall[ _LINENO ] ) ) ;
                + ") in " + aCall[ _MODULE ] )
    NEXT
  END
RETURN Self


METHOD ShowUp() CLASS XHDebugCallStack
  IF ::lDirty
    ::oDebugger:SyncCalls() 
    ::lDirty := .F.
  ENDIF
RETURN Self
