/*
 * $Id$
 */

#include "dbstruct.ch"
#include "hbclass.ch"

#include "colors.ch"
#include "vxh.ch"

#include "vxhdebug.ch"


CLASS XHDebugWorkArea FROM TabPage
  DATA oDebugger
  DATA oAreaList
  DATA oAreaInfo
  DATA oAreaStruct
  DATA aAreas INIT {}
  DATA aTable INIT {}
  DATA aStruct INIT {}
  DATA nArea INIT 0
  DATA lDirty INIT .T.
  METHOD Init( oParent, oDebugger ) CONSTRUCTOR
  METHOD Create()
  METHOD ChangeArea() INLINE If( ::oAreaList:HasFocus, ::UpdateInfo(), )
  METHOD ShowUp()
  METHOD UpdateInfo()
ENDCLASS


METHOD Init( oParent, oDebugger ) CLASS XHDebugWorkArea
  ::Super:Init( oParent )
  ::oDebugger := oDebugger

  ::Caption := "   Work Areas  "

  ::Dock:Margin := 2
  ::Dock:TopMargin := 4
  ::Dock:Left   := ::Parent
  ::Dock:Top    := ::Parent
  ::Dock:Bottom := ::Parent
  ::Dock:Right  := ::Parent
RETURN Self


METHOD Create() CLASS XHDebugWorkArea
  ::Super:Create()

  WITH OBJECT ::oAreaList := XHDebugList( Self )
    :Dock:Left := :Parent
    :Dock:Top := :Parent
    :Dock:Bottom := :Parent
    :EventHandler[ "OnSelChange" ] := "ChangeArea"
    :Create()
    :Enable( .F. )
  END

  WITH OBJECT ::oAreaStruct := DataGrid( Self )
    :FullRowSelect := .T.
    :DataSource := MemoryTable( ::Parent )
    WITH OBJECT :DataSource
      :Structure := { { "Name", 'C', 12 }, { "Type", 'C', 1 }, { "Length", 'N', 5 }, { "Decimals", 'N', 3 } }
      :Table := ::aStruct
      :Create()
    END
    :AutoAddColumns( .F. )
    :Width := (12 + 6 + 8 + 10) * 7 + 4 * 5
    :Dock:Top    := :Parent
    :Dock:Right  := :Parent
    :Dock:Bottom := :Parent
    :Create()
    :Hide()
  END
  
  WITH OBJECT ::oAreaInfo := XHDebugList( Self )
    :Dock:Left := ::oAreaList
    :Dock:Top := :Parent
    :Dock:Bottom := :Parent
    :Dock:Right := ::oAreaStruct
    :Create()
    :Hide()
  END

  ::DockIt()
RETURN Self


METHOD ShowUp() CLASS XHDebugWorkArea
  LOCAL aArea, nArea

  IF !::lDirty
    RETURN Self
  ENDIF
  
  ::oDebugger:Do( ".areas" )
  ::aAreas := ::oDebugger:ReadAreas()

  WITH OBJECT ::oAreaList
    nArea := :GetCurSel()
    :ResetContent()
    FOR EACH aArea IN ::aAreas
      :AddItem( aArea[ _ALIAS ] )
    NEXT
    IF nArea > 0 .AND. nArea <= Len( ::aAreas )
      :SetCurSel( nArea )
    ENDIF
    IF Len( ::aAreas ) == 0
      :AddItem( "No open areas" )
      :Enable( .F. )
      ::oAreaInfo:Hide()
      ::oAreaStruct:Hide()
    ELSE
      :Enable( .T. )
      ::oAreaInfo:Show()
      ::oAreaStruct:Show()
    ENDIF
  END

  ::UpdateInfo()
  ::lDirty := .F.
RETURN Self


METHOD UpdateInfo() CLASS XHDebugWorkArea
  LOCAL nArea := ::oAreaList:GetCurSel(), nAreaNo
  LOCAL hInfo, aRecord, cString
  
  IF Len( ::aAreas ) < 1 .OR. nArea == 0
    RETURN Self
  ENDIF
  
  nAreaNo := ::aAreas[ nArea ][ _NUMBER ]
  hInfo := ::oDebugger:ReadWorkAreaInfo( nAreaNo )
  ::aStruct := hInfo[ "DbStruct" ]
  aRecord := ::oDebugger:ReadWorkAreaRecord( nAreaNo, ::aStruct )
  WITH OBJECT ::oAreaInfo
    :ResetContent()
    FOR EACH cString IN ::oDebugger:WorkAreaInfoText( nAreaNo, hInfo, ::aStruct, aRecord )
      :AddItem( cString )
    NEXT
  END

  ::oAreaStruct:DataSource:Table := ::aStruct
  ::oAreaStruct:Update():Refresh()
RETURN Self