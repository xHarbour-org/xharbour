/* $Id$ */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

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

  ::Caption := "   Monitor  "

  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugMonitor
  LOCAL oAllGlobals

  ::Super:Create()

  WITH OBJECT CheckBox( Self )
    :Caption := "Global"
    :Dock:Margin := 2
    :Dock:Left := :Parent
    :Dock:Top := :Parent
    :Action := {|o| If( ::oDebugger:lShowGlobals := o:Checked, oAllGlobals:Enable(), oAllGlobals:Disable), ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT CheckBox( Self )
    :Caption := "Local"
    :Dock:Margin := 2
    :Dock:Left := :Form:CheckBox1
    :Dock:Top := :Parent
    :Action := {|o| ::oDebugger:lShowLocals := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT CheckBox( Self )
    :Caption := "Private"
    :Dock:Margin := 2
    :Dock:Left := :Form:CheckBox2
    :Dock:Top := :Parent
    :Action := {|o| ::oDebugger:lShowPrivates := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT CheckBox( Self )
    :Caption := "Public" 
    :Dock:Margin := 2
    :Dock:Left := :Form:CheckBox3
    :Dock:Top := :Parent
    :Action := {|o| ::oDebugger:lShowPublics := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT CheckBox( Self )
    :Caption := "Static"
    :Dock:Margin := 2
    :Dock:Left := :Form:CheckBox4
    :Dock:Top := :Parent
    :Action := {|o| ::oDebugger:lShowStatics := o:Checked, ::oDebugger:SyncVars() }
    :Create()
  END

  WITH OBJECT oAllGlobals := CheckBox( Self )
    :Caption := "Show all GLOBALs " /* the endspace is needed to bypass docking bug */
    :Dock:Margin := 2
    :Dock:Top := :Parent
    :Dock:Right := :Parent
    :Action := {|o| ::oDebugger:lShowAllGlobals := o:Checked, ::oDebugger:SyncVars() }
    :Create()
    :Disable()
  END

  ::oGrid := DataGrid( Self )
  WITH OBJECT ::oGrid
    //:FullRowSelect := .T.
    :DataSource := MemoryTable( ::Parent )
    WITH OBJECT :DataSource
      :Structure := { { "Scope", 'C', 8 }, { "Name", 'C', 30 }, { "Type", 'C', 1 }, { "Value", 'C', 55 } }
      :Table := ::aTable
      :Create()
    END
    
    :Action := {|o| ::Inspect( o:DataSource:Record ) }
    
    :AutoAddColumns( .F. )
    WITH OBJECT ATail( :Children )
      :Picture := "@k"
      :Control := {|o, n| If( ::aTable[ n ][ 3 ] $ "AHO", NIL, MaskEdit( o ) ) }
      //:Control := {|o, n| If( ::Inspect( n ), NIL, /*Mask*/Edit( o ) ) }
      :ControlAccessKey := GRID_LCLICK
      :OnSave := {| , oGrid, xData| ::Save( oGrid:DataSource:Record, xData ) }
    END
    
    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top    := :Form:CheckBox1
    :Dock:Bottom := :Parent
    :Dock:Right  := :Parent
    :Create()
    
  END  
  
  ::DockIt()
RETURN Self


METHOD Inspect( nRecord ) CLASS XHDebugMonitor
  IF ::aTable[ nRecord ][ 3 ] $ "AHO"
    ::oDebugger:Inspect( ::aTable[ nRecord ][ 2 ] )
    RETURN .T.
  ENDIF
RETURN .F.


METHOD Save( nRecord, xData ) CLASS XHDebugMonitor
  LOCAL cValue := ::oDebugger:ReadExpressionValue( ::aTable[ nRecord ][ 2 ] + ":=" + xData )

  ::aTable[ nRecord ] := { ::aTable[ nRecord ][ 1 ], ::aTable[ nRecord ][ 2 ], cValue[ 1 ], SubStr( cValue, 2 ) }
RETURN .T.


METHOD SetVars( aVars ) CLASS XHDebugMonitor
  LOCAL aVar, cValue
  
  ::aVars := aVars
  ASize( ::aTable, 0 )
  FOR EACH aVar IN ::aVars
    cValue := ::oDebugger:ReadVarValue( aVar )
    AAdd( ::aTable, { aVar[ _SCOPE ], aVar[ _NAME ], cValue[ 1 ], PadR( SubStr( cValue, 2 ), 256 ) } )
  NEXT
  ::oGrid:Update():Refresh()
RETURN Self


METHOD ShowUp() CLASS XHDebugMonitor
  IF ::lDirty
    ::oDebugger:SyncVars()
    ::lDirty := .F.
  ENDIF
RETURN Self
