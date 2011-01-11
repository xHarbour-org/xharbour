
/*
 * $Id$
 */

#include "dbstruct.ch"
#include "hbclass.ch"

#include "gtk.ch"

#include "vxhdebug.ch"


CLASS XHDebugWorkAreaGTK FROM GtkHBox
  METHOD new( oDebugger ) CONSTRUCTOR
  METHOD ShowUp()
  METHOD UpdateInfo()
  DATA oDebugger
  DATA oModel
  DATA oAreaView
  DATA oAreaModel
  DATA oInfoSW
  DATA oInfoModel
  DATA oStructSW
  DATA oStructModel
  DATA lDirty INIT .T.
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugWorkAreaGTK
  LOCAL oCell, oBox, oButton, oButton1, oButton2, oSW, oColumn, oView
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  ::oAreaModel := GtkListStore():new( G_TYPE_INT, G_TYPE_STRING )
  ::oAreaView := GtkTreeView():new( ::oAreaModel )
  oCell := GtkCellRendererText():new()
  WITH OBJECT ::oAreaView
    :append_column( GtkTreeViewColumn():new( "Alias", oCell, "text", 1 ) )
    :set_search_column( 1 )
    :set_headers_visible( .F. )
    :get_selection():connect( "changed", ;
                              {|o| If( o:get_selected(), ::UpdateInfo(), ) } )
  END
  oSW := GtkScrolledWindow():new():set_policy( GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC )
  oSW:add( ::oAreaView )
  ::pack_start( oSW, .F., .F. )
 
  ::oInfoModel := GtkListStore():new( G_TYPE_STRING )
  oView := GtkTreeView():new( ::oInfoModel )
  WITH OBJECT oView
    :append_column( GtkTreeViewColumn():new( "Info", oCell, "text", 0 ) )
    :set_headers_visible( .F. )
  END
  ::oInfoSW := GtkScrolledWindow():new():set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC )
  ::oInfoSW:add( oView )
  ::pack_start( ::oInfoSW )

  ::oStructModel := GtkListStore():new( G_TYPE_STRING, G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT )
  oView := GtkTreeView():new( ::oStructModel )
  WITH OBJECT oView
    :append_column( GtkTreeViewColumn():new( "Name", oCell, "text", 0 ) )
    :append_column( GtkTreeViewColumn():new( "Type", oCell, "text", 1 ) )
    :append_column( GtkTreeViewColumn():new( "Length", oCell, "text", 2 ) )
    :append_column( GtkTreeViewColumn():new( "Decimals", oCell, "text", 3 ) )
    :set_search_column( 0 )
  END
  ::oStructSW := GtkScrolledWindow():new():set_policy( GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC )
  ::oStructSW:add( oView )
  ::pack_start( ::oStructSW, .F., .F. )
  
  ::show_all()
RETURN Self

 
METHOD ShowUp() CLASS XHDebugWorkAreaGTK
  LOCAL aArea, aAreas, aWatch, cValue, iter, cAlias
  
  IF ::lDirty
    ::oDebugger:Do( ".areas" )
    aAreas := ::oDebugger:ReadAreas()

    WITH OBJECT ::oAreaModel
      IF ::oAreaView():get_selection():get_selected( , @iter )
        cAlias := :get_value( iter, 1 )
      ENDIF
      :clear()
      FOR EACH aArea IN aAreas
        iter := :append()
        :set( iter, 0, aArea[ _NUMBER ], ;
                    1, aArea[ _ALIAS ] )
        IF aArea[ _ALIAS ] == cAlias
          ::oAreaView:set_cursor( :get_path( iter ) )
        ENDIF
      NEXT
      IF Len( aAreas ) == 0
        :set( :append(), 1, "No open areas" )
        ::oAreaView:set_sensitive( .F. )
      ELSE
        ::oAreaView:set_sensitive( .T. )
      ENDIF
    END

    ::UpdateInfo()
    ::lDirty := .F.
  ENDIF
RETURN Self


METHOD UpdateInfo() CLASS XHDebugWorkAreaGTK
  LOCAL i, nAreaNo, iter, hInfo, aStruct, aRecord, cString

  IF ::oAreaView:get_selection():get_selected( , @iter )
    nAreaNo := ::oAreaModel:get_value( iter, 0 )
    hInfo := ::oDebugger:ReadWorkAreaInfo( nAreaNo )
    aStruct := hInfo[ "DbStruct" ]
    aRecord := ::oDebugger:ReadWorkAreaRecord( nAreaNo, aStruct )
    WITH OBJECT ::oInfoModel
      :clear()
      FOR EACH cString IN ::oDebugger:WorkAreaInfoText( nAreaNo, hInfo, aStruct, aRecord )
        :set( :append(), 0, cString )
      NEXT
    END
    WITH OBJECT ::oStructModel
      :clear()
      FOR EACH aRecord IN aStruct
        :set( :append(), 0, aRecord[ DBS_NAME ], ;
                         1, aRecord[ DBS_TYPE ], ;
                         2, aRecord[ DBS_LEN ], ;
                         3, aRecord[ DBS_DEC ] )
      NEXT
    END
    ::oInfoSW:show_all()
    ::oStructSW:show_all()
  ELSE
    ::oInfoSW:hide_all()
    ::oStructSW:hide_all()
  ENDIF
RETURN Self
