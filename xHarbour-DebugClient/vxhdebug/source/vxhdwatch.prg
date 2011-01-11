/*
 * $Id$
 */

#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugWatch FROM TabPage
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
  ::oDebugger := oDebugger

  ::Caption := "   Watch  "

  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugWatch
  ::Super:Create()

  WITH OBJECT Button( Self )
    :Caption := "Add tracepoint"
    :Action := {|| ::oDebugger:AddPoint( .T., ::oEdit:Caption ), ::oEdit:Caption := "" }
    :Dock:Top := :Parent
    :Dock:Right := :Parent
    :Create()
    :Disable()
  END

  WITH OBJECT Button( Self )
    :Caption := "Add watchpoint"
    :Action := {|| ::oDebugger:AddPoint( .F., ::oEdit:Caption ), ::oEdit:Caption := "" }
    :Dock:Top := :Parent
    :Dock:Right := :Form:Button1
    :Create()
    :Disable()
  END

  ::oEdit := EditBox( Self )
  WITH OBJECT ::oEdit
    :Height := 30
    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top = :Parent
    :Dock:Right  := :Form:Button2
    :Create()
    :EventHandler[ "OnEn_Change" ] := "OnEn_Change"
  END

  WITH OBJECT Button( Self )
    :Caption := "Remove"
    :Action := {|| ::Remove() }
    :Dock:Bottom := :Parent
    :Dock:Right := :Parent
    :Create()
    :Disable()
  END

  ::oGrid := DataGrid( Self )
  WITH OBJECT ::oGrid
    //:FullRowSelect := .T.
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
/*    :SetBlocks()
    WITH OBJECT GridColumn( :this )
      :Caption := "Expression"
      :Data := "hb_QSelf():DataSource:Fields:Expression"
      :AllowSize := .T.
      :AllowDrag := .T.
      :Picture := "@k"
      :Control := {|o,n| MaskEdit( o ) }
      :ControlAccessKey := GRID_CHAR | GRID_LDBLCLK
      :OnSave := {|oCol, oGrid, xData| If( !Empty( xData := AllTrim( xData ) ), ;
                                           ::SavePoint( oGrid:DataSource:Record, xData ), ) }
      :Create()
    END
    WITH OBJECT GridColumn( :this )
      :Caption := "Trace"
      :Data := "hb_QSelf():DataSource:Fields:Expression"
      :AllowSize := .T.
      :AllowDrag := .T.
      :OnSave := {|oCol, oGrid, xData| ::SavePoint( oGrid:DataSource:Record, xData ) }
      :Create()
    END
    WITH OBJECT GridColumn( :this )
      :Caption := "Value"
      :Data := "hb_QSelf():DataSource:Fields:Value"
      :AllowSize := .T.
      :AllowDrag := .T.
      :Create()
    END*/

    :Dock:Margin := 2
    :Dock:TopMargin := 4
    :Dock:Left   := :Parent
    :Dock:Top    := ::oEdit//ToolBar1
    :Dock:Right  := :Parent
    :Dock:Bottom := :Form:Button3 // Remove
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
  IF Len( ::oEdit:Caption ) > 0
    ::Form:Button1:Enable()
    ::Form:Button2:Enable()
  ELSE
    ::Form:Button1:Disable()
    ::Form:Button2:Disable()
  ENDIF
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
    IF Len( ::aWatch ) > 0
      ::Form:Button3:Enable()
    ELSE
      ::Form:Button3:Disable()
    ENDIF
    ::lDirty := .F.
  ENDIF
RETURN Self
