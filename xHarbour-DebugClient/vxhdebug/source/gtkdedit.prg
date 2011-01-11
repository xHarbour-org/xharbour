/*
 * $Id$
 */

#include "hbclass.ch"

#include "pango.ch"
#include "gtk.ch"

CLASS XHDebuggerEditorGTK FROM GtkNotebook
  METHOD new()
  METHOD current_file() INLINE ::aFiles[ ::get_current_page() + 1 ]
  METHOD current_line()
  METHOD go_line( nLine )
  METHOD open( cFile )
  METHOD toggle_bookmark()
  DATA aFiles INIT {}
  DATA aBuffers INIT {}
  DATA aViews INIT {}
ENDCLASS


METHOD new() CLASS XHDebuggerEditorGTK
  ::Super:new()
RETURN Self


METHOD current_line() CLASS XHDebuggerEditorGTK
  WITH OBJECT ::aBuffers[ ::get_current_page() + 1 ]
    RETURN :get_iter_at_mark( :get_insert() ):get_line() + 1
  END
RETURN


METHOD go_line( nLine ) CLASS XHDebuggerEditorGTK
  LOCAL nPos
 
  nPos := ::get_current_page() + 1
  IF nPos > 0
    WITH OBJECT ::aBuffers[ nPos ]
      :remove_tag_by_name( "current_line", :get_start_iter(), :get_end_iter() )
      IF nLine > 0
        :place_cursor( :get_iter_at_line( nLine - 1 ) )
        :apply_tag_by_name( "current_line", ;
                            :get_iter_at_line( nLine - 1 ), ;
                            :get_iter_at_line( nLine ) )
        ::aViews[ nPos ]:scroll_mark_onscreen( :get_insert() )
      ENDIF
    END
  ENDIF
RETURN Self


METHOD open( cFile ) CLASS XHDebuggerEditorGTK
  LOCAL nPos, n, cFileShort
  LOCAL oScroll, oBuffer, oTag
  
  cFileShort := cFile
  n := RAt( '/', cFileShort )
  IF n > 0
    cFileShort := SubStr( cFile, n + 1 )
  ENDIF
  
  nPos := AScan( ::aFiles, cFileShort )
  IF nPos == 0
    AAdd( ::aFiles, cFileShort )
    nPos := Len( ::aFiles )
    
    AAdd( ::aViews, GtkTextView():new():set_editable( .F. ):show_all() )
    oBuffer := ::aViews[ nPos ]:get_buffer()
    AAdd( ::aBuffers, oBuffer )
    
    WITH OBJECT oBuffer
      IF !File( cFile )
        :set_text( "File not found", -1 )
      ELSE
        :set_text( MemoRead( cFile ), -1 )
        :create_tag( "current_line" ):set( "paragraph-background", "yellow" )
        :create_tag( "bookmark" ):set( "weight", PANGO_WEIGHT_BOLD, ;
                                       "foreground", "red" )
        oTag := :create_tag():set( "family", "monospace" )
        :apply_tag( oTag, :get_start_iter(), :get_end_iter() )
      ENDIF
    END
    
    oScroll := GtkScrolledWindow():new()
    oScroll:set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC )
    oScroll:add( ::aViews[ nPos ] )
    ::append_page( oScroll, GtkLabel():new( cFileShort ) )
    ::show_all()
  ENDIF
  ::set_current_page( nPos - 1 )
RETURN Self


METHOD toggle_bookmark() CLASS XHDebuggerEditorGTK
  LOCAL nLine := ::current_line(), iter1, iter2, oTag
  
  WITH OBJECT ::aBuffers[ ::get_current_page() + 1 ]
    iter1 := :get_iter_at_line( nLine - 1 )
    iter2 := :get_iter_at_line( nLine )
    oTag := :get_tag_table():lookup( "bookmark" )
    IF :get_iter_at_mark( :get_insert() ):has_tag( oTag )
      :remove_tag( oTag, iter1, iter2 )
    ELSE
      :apply_tag( oTag, iter1, iter2 )
    ENDIF
  END
RETURN Self
