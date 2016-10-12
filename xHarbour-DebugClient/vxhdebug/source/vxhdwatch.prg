/*
 * $Id$
 */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugWatch FROM TabPage
   DATA oRemove, oTrace, oWatch
   DATA oDebugger
   DATA aWatch
   DATA oGrid
   DATA oEdit
   DATA aTable INIT {}
   DATA lDirty INIT .T.
   METHOD Init( oParent, oDebugger ) CONSTRUCTOR
   METHOD Inspect( nRecord )
   METHOD Create()
   METHOD OnEn_Change()
   METHOD Remove()
   METHOD ShowUp()
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugWatch
   ::Super:Init( oParent )
   ::oDebugger      := oDebugger
   ::Text           := "Watch"
   ::Dock:Margin    := 2
   ::Dock:TopMargin := 4
   ::Dock:Left      := ::Parent
   ::Dock:Top       := ::Parent
   ::Dock:Bottom    := ::Parent
   ::Dock:Right     := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugWatch
   ::Super:Create()

   WITH OBJECT ::oTrace := Button( Self )
      :Caption        := " Add tracepoint"
      :Action         := {|| ::oDebugger:AddPoint( .T., ::oEdit:Caption ), ::oEdit:Caption := "" }
      :Dock:Top       := :Parent
      :Dock:TopMargin := 4
      :Dock:Right     := :Parent
      :Create()
      :Width          := :Drawing:GetTextExtentPoint32( :Text )[1]+20
      :Disable()
   END

   WITH OBJECT ::oWatch := Button( Self )
      :Caption        := " Add watchpoint"
      :Action         := {|| ::oDebugger:AddPoint( .F., ::oEdit:Caption ), ::oEdit:Caption := "" }
      :Dock:Top       := :Parent
      :Dock:TopMargin := 4
      :Dock:Right     := ::oTrace
      :Create()
      :Width          := :Drawing:GetTextExtentPoint32( :Text )[1]+20
      :Disable()
   END

   ::oEdit := EditBox( Self )
   WITH OBJECT ::oEdit
      :Height         := 30
      :Dock:Margin    := 2
      :Dock:TopMargin := 4
      :Dock:Left      := :Parent
      :Dock:Top       := :Parent
      :Dock:Right     := ::oWatch
      :Create()
      :EventHandler[ "OnEn_Change" ] := "OnEn_Change"
   END

   WITH OBJECT ::oRemove := Button( Self )
      :Caption     := "Remove"
      :Action      := {|| ::Remove() }
      :Dock:Bottom := :Parent
      :Dock:Right  := :Parent
      :Create()
      :Disable()
   END

   ::oGrid := DataGrid( Self )
   WITH OBJECT ::oGrid
      :DataSource := MemoryTable( ::Parent )
      WITH OBJECT :DataSource
         :Structure := { { "Expression", 'C', 25 }, { "Trace", 'L', 1 }, { "Value", 'C', 55 } }
         :Table := ::aTable
         :Create()
      END
      :Action := {|o| ::oDebugger:Inspect( ::aTable[ o:DataSource:Record ][ 1 ] ) }
      :AutoAddColumns( .F. )
      WITH OBJECT ATail( :Children )
         :Picture := "@k"
         :Control := {|o, n| If( ::aTable[ n ][ 3 ][ 1 ] == '{', NIL, MaskEdit( o ) ) }
         :ControlAccessKey := GRID_LCLICK
         :OnSave := {|| .F. }
      END

      :Dock:Margin    := 2
      :Dock:TopMargin := 4
      :Dock:Left      := :Parent
      :Dock:Top       := ::oEdit
      :Dock:Right     := :Parent
      :Dock:Bottom    := ::oRemove
      :Create()
   END

   ::DockIt()
RETURN Self


METHOD Inspect( nRecord ) CLASS XHDebugWatch
   IF ::aTable[ nRecord ][ 3 ][ 1 ] == '{'
      ::oDebugger:Inspect( "(" + ::aTable[ nRecord ][ 1 ] + ")" )
      RETURN .T.
   ENDIF
RETURN .F.


METHOD OnEn_Change() CLASS XHDebugWatch
   ::oTrace:Enabled := Len( ::oEdit:Caption ) > 0
   ::oWatch:Enabled := Len( ::oEdit:Caption ) > 0
RETURN 0


METHOD Remove() CLASS XHDebugWatch
   ::oDebugger:UnWatch( ::aWatch[ ::oGrid:DataSource:Record ][ _NUMBER ] )
   ::lDirty := .T.
   ::ShowUp()
RETURN Self


METHOD ShowUp() CLASS XHDebugWatch
   LOCAL aWatch, cValue

   IF ::lDirty
      ::oDebugger:Do( ".wp" )
      ::aWatch := ::oDebugger:ReadWatches()

      ASize( ::aTable, 0 )
      FOR EACH aWatch IN ::aWatch
          ::oDebugger:Do( ".wp " + LTrim( Str( aWatch[ _NUMBER ] ) ) )
          cValue := ::oDebugger:ReadValue()

          AAdd( ::aTable, { aWatch[ _EXPR ], aWatch[ _TRACE ], SubStr( cValue, 2 ) } )
      NEXT
      ::oGrid:Update():Refresh()
      ::oRemove:Enabled := Len( ::aWatch ) > 0
      ::lDirty := .F.
   ENDIF
RETURN Self
