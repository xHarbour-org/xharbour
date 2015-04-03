/* $Id$ */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"
#include "debug.ch"

#include "vxhdebug.ch"


CLASS XHDebugMonitor FROM TabPage
  DATA oDebugger
  DATA aVars
  DATA oGrid
  DATA aTable INIT {}
  DATA lDirty INIT .T.
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD Create()
  METHOD Inspect( nRecord )
  METHOD Save( nRecord, xData )
  METHOD SetVars( aVars )
  METHOD ShowUp()
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugMonitor
  ::Super:Init( oParent )
  ::oDebugger := oDebugger

  ::Caption := "Monitor"

  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugMonitor
  LOCAL oAllGlobals, oGlobal, oLocal, oPrivate, oPublic, oStatic

  ::Super:Create()

  WITH OBJECT oGlobal := CheckBox( Self )
    :Text        := "Global"
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Left   := :Parent
    :Dock:Top    := :Parent
    :Action      := {|o| If( ::oDebugger:lShowGlobals := o:Checked, oAllGlobals:Enable(), oAllGlobals:Disable), ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oLocal := CheckBox( Self )
    :Text        := "Local"
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Left   := oGlobal
    :Dock:Top    := :Parent
    :Action      := {|o| ::oDebugger:lShowLocals := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oPrivate := CheckBox( Self )
    :Text        := "Private"
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Left   := oLocal
    :Dock:Top    := :Parent
    :Action      := {|o| ::oDebugger:lShowPrivates := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oPublic := CheckBox( Self )
    :Text        := "Public"
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Left   := oPrivate
    :Dock:Top    := :Parent
    :Action      := {|o| ::oDebugger:lShowPublics := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oStatic := CheckBox( Self )
    :Caption     := "Static"
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Left   := oPublic
    :Dock:Top    := :Parent
    :Action      := {|o| ::oDebugger:lShowStatics := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oAllGlobals := CheckBox( Self )
    :Caption     := "Show all GLOBALs"
    :AutoSize    := .T.
    :Transparent := .T.
    :Dock:Margin := 2
    :Dock:Top    := :Parent
    :Dock:Right  := :Parent
    :Action      := {|o| ::oDebugger:lShowAllGlobals := o:Checked, ::oDebugger:SyncVars() }
    :Create()
    :Disable()
  END

  ::oGrid := DataGrid( Self )
  WITH OBJECT ::oGrid
    :DataSource := MemoryTable( ::Parent )
    WITH OBJECT :DataSource
      :Structure := { { "Scope", 'C', 8 }, { "Name", 'C', 30 }, { "Type", 'C', 1 }, { "Value", 'C', 55 } }
      :Create()
    END

    :Action := {|o| ::Inspect( o:DataSource:Record ) }

    :AutoAddColumns( .F. )
    WITH OBJECT ATail( :Children )
      :Picture := "@k"
      :Control := {|o, n| If( ::oGrid:DataSource:Table[ n ][ 3 ] $ "AHO", NIL, MaskEdit( o ) ) }
      :ControlAccessKey := GRID_LCLICK
      :OnSave := {| , oGrid, xData| ::Save( oGrid:DataSource:Record, xData ) }
    END

    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top    := oGlobal
    :Dock:Bottom := :Parent
    :Dock:Right  := :Parent
    :Create()

  END

  ::DockIt()
RETURN Self


METHOD Inspect( nRecord ) CLASS XHDebugMonitor
  IF ::oGrid:DataSource:Table[ nRecord ][ 3 ] $ "AHO"
    ::oDebugger:Inspect( ::oGrid:DataSource:Table[ nRecord ][ 2 ] )
    RETURN .T.
  ENDIF
RETURN .F.


METHOD Save( nRecord, xData ) CLASS XHDebugMonitor
  LOCAL cValue := ::oDebugger:ReadExpressionValue( ::oGrid:DataSource:Table[ nRecord ][ 2 ] + ":=" + xData )

  ::oGrid:DataSource:Table[ nRecord ] := { ::oGrid:DataSource:Table[ nRecord ][ 1 ], ::oGrid:DataSource:Table[ nRecord ][ 2 ], cValue[ 1 ], SubStr( cValue, 2 ) }
RETURN .T.


METHOD SetVars( aVars ) CLASS XHDebugMonitor
  LOCAL aVar, cValue

  ::aVars := aVars
  ASize( ::oGrid:DataSource:Table, 0 )
  FOR EACH aVar IN ::aVars
      cValue := ::oDebugger:ReadVarValue( aVar )
      AAdd( ::oGrid:DataSource:Table, { aVar[ _SCOPE ], aVar[ _NAME ], cValue[ 1 ], PadR( SubStr( cValue, 2 ), 256 ) } )
  NEXT
  ::oGrid:Update():Refresh()
RETURN Self


METHOD ShowUp() CLASS XHDebugMonitor
  IF ::lDirty
    ::oDebugger:SyncVars()
    ::lDirty := .F.
  ENDIF
RETURN Self
