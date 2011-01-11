/*
 * $Id$
 */

#include "hbclass.ch"

#include "gtk.ch"
#include "vxhdebug.ch"

CLASS XHDebugCallStackGTK FROM GtkScrolledWindow
  METHOD new( oDebugger )
  METHOD ChangeFrame( oSelection )
  METHOD SetCalls( aCalls )
  METHOD ShowUp()
  DATA oDebugger
  DATA oView
  DATA oModel
  DATA lDirty INIT .T.
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugCallStackGTK
  LOCAL oSW, oView, oCell
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  ::oModel := GtkListStore():new( G_TYPE_INT, G_TYPE_STRING )
  
  ::oView := GtkTreeView():new( ::oModel ):set_rules_hint( .T. )
  WITH OBJECT ::oView
    oCell := GtkCellRendererText():new()
    :append_column( GtkTreeViewColumn():new( "Stack Frame", oCell, "text", 1 ) )
    :set_headers_visible( .F. )
    :get_selection():connect( "changed", {|o| ::ChangeFrame( o ) } )
  END
  
  ::set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC )
  ::add( ::oView )
  ::show_all()
RETURN Self


METHOD ChangeFrame( oSelection ) CLASS XHDebugCallStackGTK
  LOCAL n, iter

  IF oSelection:get_selected( , @iter )
    n := ::oModel:get_value( iter, 0 )
    ::oDebugger:Frame( n )
  ENDIF
RETURN Self


METHOD SetCalls( aCalls ) CLASS XHDebugCallStackGTK
  LOCAL oIter, i, nDepth := Len( aCalls )
  
  WITH OBJECT ::oModel
    :clear()
    FOR i := 1 TO nDepth
      oIter := :prepend()
      :set( oIter, 0, nDepth - i )
      :set( oIter, 1, ( aCalls[ i ][ _FUNCTION ] + ;
                        " (" + LTrim( Str( aCalls[ i ][ _LINENO ] ) ) + ;
                        ") in " + aCalls[ i ][ _MODULE ] ) )
    NEXT
  END
RETURN Self


METHOD ShowUp() CLASS XHDebugCallStackGTK
  IF ::lDirty
    ::oDebugger:SyncCalls()
    ::lDirty := .F.
  ENDIF
RETURN Self

