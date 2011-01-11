/*
 * $Id$
 */

#include "hbclass.ch"

#include "gtk.ch"

CLASS XHDebugConsoleGTK FROM GtkVBox
  METHOD new( oDebugger )
  METHOD DisableInput() INLINE ::oInput:set_sensitive( .F. )
  METHOD Do()
  METHOD DoIt()
  METHOD EnableInput() INLINE ::oInput:set_sensitive( .T. )
  METHOD Out( cText )
  DATA oDebugger
  DATA oInput
  DATA oOut
  DATA oBuffer
ENDCLASS


METHOD new( oDebugger ) CLASS XHDebugConsoleGTK
  LOCAL oSW
  
  ::Super:new()
  
  ::oDebugger := oDebugger

  ::oOut := GtkTextView():new():set_editable( .F. )
  ::oBuffer := ::oOut:get_buffer()
  ::oBuffer:create_tag( "monospace" ):set( "family", "monospace" )
  ::oInput := GtkComboBoxEntry():new_text()
  ::oInput:child:connect( "activate", {|| ::DoIt() } )
  
  oSW := GtkScrolledWindow():new()
  oSW:set_policy( GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC ):add( ::oOut )
  
  ::pack_start( oSW )
  ::pack_end( ::oInput, .F., .F. )
  //::show_all()
RETURN Self


METHOD Do( cCommand ) CLASS XHDebugConsoleGTK
  WITH OBJECT ::oBuffer
    :delete( :get_iter_at_line( :get_line_count() - 2), :get_end_iter() )
  END
  ::Out( "> " + cCommand )
  ::oDebugger:DoConsoleCommand( cCommand )
RETURN Self


METHOD DoIt() CLASS XHDebugConsoleGTK
  LOCAL cCommand
  
  WITH OBJECT ::oInput
    cCommand := :get_active_text()
    IF Len( Trim( cCommand ) ) > 0
      ::Do( cCommand )
      :prepend_text( cCommand )
      :child:set_text("")
    ENDIF
  END
RETURN Self


METHOD Out( cText ) CLASS XHDebugConsoleGTK
  WITH OBJECT ::oBuffer
    :place_cursor( :get_end_iter() )
    :insert_at_cursor( cText + Chr( 10 ), -1 )
    :apply_tag_by_name( "monospace", :get_start_iter(), :get_end_iter() )
    ::oOut:scroll_mark_onscreen( :get_insert )
  END
RETURN Self

