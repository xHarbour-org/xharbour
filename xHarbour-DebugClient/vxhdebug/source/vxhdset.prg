/*
 * $Id$
 */

#include "dbstruct.ch"
#include "hbclass.ch"

#include "colors.ch"
#include "set.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugSet FROM TabPage
  DATA oDebugger
  DATA oSetGrid
  DATA aTable INIT {}
  DATA lDirty INIT .T.
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD Create()
  METHOD ShowUp()
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugSet
  ::Super:Init( oParent )
  ::oDebugger := oDebugger

  ::Caption := "   Set() Values  "

  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugSet
  ::Super:Create()

  WITH OBJECT ::oSetGrid := DataGrid( Self )
    :FullRowSelect := .T.
    :DataSource := MemoryTable( ::Parent )
    WITH OBJECT :DataSource
      :Structure := { { "Number", 'N', 3 }, { "Name", 'C', 20 }, { "Value", 'C', 40 } }
      :Table := ::aTable
      :Create()
    END
    :AutoAddColumns( .F. )
    :Width := (3 + 20 + 40) * 7 + 3 * 3
    :Dock:Top    := :Parent
    :Dock:Left   := :Parent
    :Dock:Right  := :Parent
    :Dock:Bottom := :Parent
    :Create()
    :Hide()
  END
  
  ::DockIt()
RETURN Self


METHOD ShowUp() CLASS XHDebugSet
  local i

  IF !::lDirty
    RETURN Self
  ENDIF
 
  ::aTable := ::oDebugger:ReadSets()
  for i:=1 to len(::aTable)
    if valtype(::aTable[i][3]) == 'A'
      ::aTable[i][3] := CStr(::aTable[i][3] )
    endif
  next
  
  ::oSetGrid:DataSource:Table := ::aTable
  ::oSetGrid:Update():Refresh()
  ::oSetGrid:Show()
  
  ::lDirty := .F.
RETURN Self
