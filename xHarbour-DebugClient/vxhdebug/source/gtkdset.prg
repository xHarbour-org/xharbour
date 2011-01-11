/*
 * $Id$
 */

#include "hbclass.ch"

#include "gtk.ch"

#include "vxhdebug.ch"

CLASS XHDebugSetGTK FROM GtkScrolledWindow
  METHOD new( oDebugger ) CONSTRUCTOR
  METHOD ShowUp()
  DATA oDebugger
  DATA oModel
  DATA lDirty INIT .T.
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugSetGTK
  LOCAL oView, oCell
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  ::oModel := GtkTreeStore():new( G_TYPE_INT, G_TYPE_STRING, G_TYPE_STRING )
  
  oView := GtkTreeView():new( ::oModel ):set_rules_hint( .T. )
  WITH OBJECT oView
    oCell := GtkCellRendererText():new()
    :append_column( GtkTreeViewColumn():new( "Number", oCell, "text", 0 ) )
    :append_column( GtkTreeViewColumn():new( "Name", oCell, "text", 1 ) )
    :append_column( GtkTreeViewColumn():new( "Value", oCell, "text", 2 ) )
  
    :set_search_column( 1 )
  END
  
  ::set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC ):add( oView )
  ::show_all()
RETURN Self


METHOD ShowUp() CLASS XHDebugSetGTK
  LOCAL aTable, aEntry
  
  IF ::lDirty
    aTable := ::oDebugger:ReadSets()
    WITH OBJECT ::oModel
      :clear()
      FOR EACH aEntry IN aTable
        :set( :append(), 0, aEntry[ 1 ], 1, aEntry[ 2 ], 2, ValToPrgExp( aEntry[ 3 ] ) )
      NEXT
    END
    ::lDirty := .F.
  ENDIF
RETURN Self

