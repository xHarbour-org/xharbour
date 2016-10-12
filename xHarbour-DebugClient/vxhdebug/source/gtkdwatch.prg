/*
 * $Id$
 */

#include "hbclass.ch"

#include "gtk.ch"

#include "vxhdebug.ch"


CLASS XHDebugWatchGTK FROM GtkVBox
  METHOD new( oDebugger ) CONSTRUCTOR
  METHOD OnEdited( oCell, oPath, cText )
  METHOD Remove()
  METHOD ShowUp()
  DATA oDebugger
  DATA oModel
  DATA oView
  DATA lDirty INIT .T.
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugWatchGTK
  LOCAL oCell, oBox, oButton, oButton1, oButton2, oSW, oColumn, oEntry
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  oBox := GtkHBox():new()
  
  oEntry := GtkEntry():new()
  oBox:pack_start( oEntry )
  oButton1 := GtkButton():new( "Add _Watchpoint", .F., .T. ):set_sensitive( .F. )
  oButton1:connect( "clicked", {|| ::oDebugger:AddPoint( .F., oEntry:get_text() ), ;
                                   oEntry:set_text( "" ) } )
  oBox:pack_end( oButton1, .F., .F. )
  oButton2 := GtkButton():new( "Add _Tracepoint", .F., .T. ):set_sensitive( .F. )
  oButton2:connect( "clicked", {|| ::oDebugger:AddPoint( .T., oEntry:get_text() ), ;
                                   oEntry:set_text( "" ) } )
  oBox:pack_end( oButton2, .F., .F. )
  ::pack_start( oBox, .F., .F. )
  
  oEntry:connect( "changed", {|o,s| s := AllTrim( o:get_text ), ;
                                      oButton1:set_sensitive( Len( s ) > 0 ), ;
                                      oButton2:set_sensitive( Len( s ) > 0 ) } );

  oButton := GtkButton():new( "_Remove", .F., .T. ):set_sensitive( .F. )
  oButton:connect( "clicked", {|| ::Remove() } )
  ::pack_start( oButton, .F., .F. )

  ::oModel := GtkTreeStore():new( G_TYPE_INT, G_TYPE_BOOLEAN, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING )
  
  ::oView := GtkTreeView():new( ::oModel ):set_rules_hint( .T. )
  WITH OBJECT ::oView
    oCell := GtkCellRendererToggle():new()
    :append_column( GtkTreeViewColumn():new( "Trace", oCell, "active", 1 ) )
    oCell := GtkCellRendererText():new()/*:set( "editable", .T. )
    oCell:connect( "edited", ;
                   {|cell, path, new_text| ::OnEdited( cell, path, new_text ) } )*/
    :append_column( GtkTreeViewColumn():new( "Expression", oCell, "text", 2 ) )
    oCell := GtkCellRendererText():new()
    :append_column( GtkTreeViewColumn():new( "Type", oCell, "text", 3 ) )
    oColumn := GtkTreeViewColumn():new( "Value", oCell, "text", 4 )
    :append_column( oColumn )
    :set_expander_column( oColumn )
    :set_search_column( 1 )
    :connect( "row-expanded", ;
              {|view, iter, path| ::oDebugger:OnRowExpanded( ::oModel, 2, iter, path ) } )
    :get_selection():connect( "changed", ;
                              {|o| oButton:set_sensitive( o:get_selected() ) } )
  END
  
  oSW := GtkScrolledWindow():new()
  oSW:set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC ):add( ::oView )
  ::pack_end( oSW )
  ::show_all()
RETURN Self


METHOD OnEdited( oCell, oPath, cText ) CLASS XHDebugWatchGTK
RETURN Self


METHOD Remove() CLASS XHDebugWatchGTK
  LOCAL iter
  
  IF ::oView:get_selection:get_selected( , @iter )
    ::oDebugger:UnWatch( ::oModel:get_value( iter, 0 ) )
    ::lDirty := .T.
    ::ShowUp()
  ENDIF
RETURN Self

  
METHOD ShowUp() CLASS XHDebugWatchGTK
  LOCAL aWatch, cValue, iter
  
  IF ::lDirty
    ::oModel:clear()
    WITH OBJECT ::oDebugger
      :Do( ".wp" )
      FOR EACH aWatch IN :ReadWatches()
        :Do( ".wp " + LTrim( Str( aWatch[ _NUMBER ] ) ) )
        cValue := :ReadValue()
        WITH OBJECT ::oModel
          iter := :append()
          :set( iter, 0, aWatch[ _NUMBER ], ;
                      1, aWatch[ _TRACE ], ;
                      2, aWatch[ _EXPR ], ;
                      3, cValue[ 1 ], ;
                      4, SubStr( cValue, 2 ) )
          IF cValue[ 1 ] $ "AHO"
            :append( iter )
          ENDIF
        END
      NEXT
    END
    ::lDirty := .F.
  ENDIF
RETURN Self

