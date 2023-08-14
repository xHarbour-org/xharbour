/*
 * $Id$
 */

#include "hbclass.ch"

#include "gtk.ch"

#include "vxhdebug.ch"

CLASS XHDebugMonitorGTK FROM GtkVBox
  METHOD new( oDebugger ) CONSTRUCTOR
  METHOD OnEdited( oCell, oPath, cText )
  METHOD SetVars( aVars )
  METHOD ShowUp()
  DATA oDebugger
  DATA oModel
  DATA aVars INIT {}
  DATA lDirty INIT .T.
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugMonitorGTK
  LOCAL oView, oCell, oBox, oButton, oAllGlobalsButton, oSW, oColumn
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  oBox := GtkHBox():new():set( "spacing", 10 )
  oButton := GtkCheckButton():new( "_Global", .T. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowGlobals := o:get_active(), ::oDebugger:SyncVars(), oAllGlobalsButton:set_sensitive( o:get_active() ) } )
  oBox:pack_start( oButton, .F., .F. )
  oButton := GtkCheckButton():new( "_Local", .T. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowLocals := o:get_active(), ::oDebugger:SyncVars() } )
  oBox:pack_start( oButton, .F., .F. )
  oButton := GtkCheckButton():new( "_Private", .T. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowPrivates := o:get_active(), ::oDebugger:SyncVars() } )
  oBox:pack_start( oButton, .F., .F. )
  oButton := GtkCheckButton():new( "_Public", .T. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowPublics := o:get_active(), ::oDebugger:SyncVars() } )
  oBox:pack_start( oButton, .F., .F. )
  oButton := GtkCheckButton():new( "_Static", .T. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowStatics := o:get_active(), ::oDebugger:SyncVars() } )
  oBox:pack_start( oButton, .F., .F. )
  oButton := GtkCheckButton():new( "S_how all GLOBALs", .T. ):set_sensitive( .F. )
  oButton:connect( "toggled", {|o| ::oDebugger:lShowAllGlobals := o:get_active(), ::oDebugger:SyncVars() } )
  oBox:pack_end( oButton, .T., .F. )
  oAllGlobalsButton := oButton
  ::pack_start( oBox, .F., .F. )
  
  ::oModel := GtkTreeStore():new( G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING )
  
  oView := GtkTreeView():new( ::oModel ):set_rules_hint( .T. )
  WITH OBJECT oView
    oCell := GtkCellRendererText():new()
    :append_column( GtkTreeViewColumn():new( "Scope", oCell, "text", 0 ) )
    :append_column( GtkTreeViewColumn():new( "Name", oCell, "text", 1 ) )
    :append_column( GtkTreeViewColumn():new( "Type", oCell, "text", 2 ) )
    oCell := GtkCellRendererText():new():set( "editable", .T. )
    oCell:connect( "edited", ;
                   {|cell, path, new_text| ::OnEdited( cell, path, new_text ) } )
    oColumn := GtkTreeViewColumn():new( "Value", oCell, "text", 3 )
    :append_column( oColumn )
    :set_expander_column( oColumn )
    :set_search_column( 1 )
    :connect( "row-expanded", ;
              {|view, iter, path| ::oDebugger:OnRowExpanded( ::oModel, 1, iter, path ) } )
  END
  
  oSW := GtkScrolledWindow():new()
  oSW:set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC ):add( oView )
  ::pack_end( oSW )
  ::show_all()
RETURN Self


METHOD OnEdited( oCell, oPath, cText ) CLASS XHDebugMonitorGTK
  LOCAL iter, cValue

  WITH OBJECT ::oModel
    iter := :get_iter( oPath )
    cValue := ::oDebugger:ReadExpressionValue( :get_value( iter, 1 ) + ":=" + cText )
    :set( iter, 2, cValue[ 1 ], ;
                3, SubStr( cValue, 2 ) )
  END
RETURN Self


METHOD SetVars( aVars ) CLASS XHDebugMonitorGTK
  LOCAL aVar, cValue, iter

  ::aVars := aVars
  WITH OBJECT ::oModel
    :clear()
    FOR EACH aVar IN ::aVars
      cValue := ::oDebugger:ReadVarValue( aVar )
      iter := :append()
      :set( iter, 0, aVar[ _SCOPE ], ;
                  1, aVar[ _NAME ], ;
                  2, cValue[ 1 ], ;
                  3, SubStr( cValue, 2 ) )
      IF cValue[ 1 ] $ "AHO"
        :append( iter )
      ENDIF
    NEXT
  END
RETURN Self


METHOD ShowUp() CLASS XHDebugMonitorGTK
  IF ::lDirty
    ::oDebugger:SyncVars()
    ::lDirty := .F.
  ENDIF
RETURN Self

