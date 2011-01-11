/*
 * $Id$
 */

#include "common.ch"
#include "hbclass.ch"

#include "gtk.ch"

#define INTERACTIVE_BUTTONS { "Stop", "StepInto", "StepOver", "StepOut", "RunToCursor", "ToggleBreak" }

CLASS XHDebuggerGTK FROM XHDebugger
  METHOD new( oEditor, oBar, oWindow ) CONSTRUCTOR
  METHOD AddPoint( lTrace, cExpr )
  METHOD Inspect( cExpr )
  METHOD Listen( lFlag )
  METHOD OnEdited( oModel, oCell, oPath, cText ) 
  METHOD OnRowExpanded( oModel, nColumn, iter, path ) 
  METHOD RunToCursor()
  METHOD ShowUp()
  METHOD Stop()
  METHOD Sync( lSyncCalls )
  METHOD ToggleBreak()
  DATA oBox
  DATA oBook
  DATA oEditor
  DATA oWindow
  DATA oUI
ENDCLASS


METHOD new( oEditor, oUI, oWindow ) CLASS XHDebuggerGTK
  LOCAL oBox
  LOCAL aButton, aButtons
  
  ::Super:New()

  ::oEditor := oEditor
  ::oWindow := oWindow
  ::oUI := oUI
  ::oUI:get_action( "/ui/DebuggerToolBar/Go" ):set_sensitive( .F. )
  ::oUI:get_action( "/ui/DebuggerToolBar/Break" ):set_sensitive( .F. )
  AEval( INTERACTIVE_BUTTONS, {|name| ::oUI:get_action( "/ui/DebuggerToolBar/" + name ):set_sensitive( .F. ) } )
  
  ::oBox := GtkVBox():new()
  ::oBox:pack_start( oUI:get_widget( "/ui/DebuggerToolBar"):set_tooltips( .T. ), .F., .F. )
  
  ::oBook := GtkNotebook():new()
  ::oConsole := XHDebugConsoleGTK():new( Self )
  ::oMonitor := XHDebugMonitorGTK():new( Self )
  ::oCallStack := XHDebugCallStackGTK():new( Self )
  ::oWatch := XHDebugWatchGTK():new( Self )
  ::oWorkArea := XHDebugWorkAreaGTK():new( Self )
  ::oSets := XHDebugSetGTK():new( Self )
  WITH OBJECT ::oBook
    :append_page( ::oConsole, GtkLabel():new( "_Console" ):set_use_underline( .T. ) )
    :append_page( ::oMonitor, GtkLabel():new( "_Monitor" ):set_use_underline( .T. ) )
    :append_page( ::oCallStack, GtkLabel():new( "Call _Stack" ):set_use_underline( .T. ) )
    :append_page( ::oWatch, GtkLabel():new( "_Watch" ):set_use_underline( .T. ) )
    :append_page( ::oWorkArea, GtkLabel():new( "Work _Areas" ):set_use_underline( .T. ) )
    :append_page( ::oSets, GtkLabel():new( "S_et() Values" ):set_use_underline( .T. ) )
    :connect( "switch-page", ;
              {|book, o, n| If( n > 0, book:get_nth_page( n ):ShowUp(), ) } )
    :set_current_page( 0 )
  END
  ::oBox:pack_end( ::oBook )
RETURN Self


METHOD AddPoint( lTrace, cExpr ) CLASS XHDebuggerGTK
  ::Super:AddPoint( lTrace, cExpr )
  ::oWatch:lDirty := .T.
  ::ShowUp()
RETURN Self


METHOD Inspect( cExpr ) CLASS XHDebuggerGTK
  LOCAL cValue := ::ReadExpressionValue( cExpr ), cType
  LOCAL oModel, oDialog, oView, oCell, oColumn, oIter, oSW

  cType := cValue[ 1 ]
  cValue := SubStr( cValue, 2 )
  
  oModel := GtkTreeStore():new( G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING )
  WITH OBJECT oModel
    oIter := :append()
    :set( oIter, 0, cExpr, 1, cType, 2, cValue )
    IF cType $ "AHO"
      :append( oIter )
    ENDIF
  END
  
  oView := GtkTreeView():new( oModel ):set_rules_hint( .T. )
  WITH OBJECT oView
    oCell := GtkCellRendererText():new()
    oColumn := GtkTreeViewColumn():new( "Expression", oCell, "text", 0 )
    :append_column( oColumn )
    :set_expander_column( oColumn )
    :append_column( GtkTreeViewColumn():new( "Type", oCell, "text", 1 ) )
    oCell := GtkCellRendererText():new():set( "editable", .T. )
    oCell:connect( "edited", ;
                   {|cell, path, new_text| ::OnEdited( oModel, cell, path, new_text ) } )
    :append_column( GtkTreeViewColumn():new( "Value", oCell, "text", 2 ) )
    :set_search_column( 0 )
    :connect( "row-expanded", ;
              {|view, iter, path| ::OnRowExpanded( oModel, 0, iter, path ) } )
  END

  oSW := GtkScrolledWindow():new()
  oSW:set_policy( GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC ):add( oView )
  
  oDialog := GtkDialog():new_with_buttons( cExpr + " " + cValue, ::oWindow, ;
                                           GTK_DIALOG_DESTROY_WITH_PARENT, ;
                                           { GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE } )
  WITH OBJECT oDialog
    :vbox:pack_start( oSW )
    :show_all()
    :connect( "response", {|| oDialog:destroy() } )
  END
RETURN Self


METHOD Listen( lFlag ) CLASS XHDebuggerGTK
  ::Super:Listen( lFlag )
  WITH OBJECT ::oUI
    :get_action( "/ui/DebuggerToolBar/Break" ):set_sensitive( lFlag )
    :get_action( "/ui/DebuggerToolBar/Go" ):set_sensitive( !lFlag )
  END

  /* hide():show() is a workaround for the GTK+ bug #56070 */
  AEval( INTERACTIVE_BUTTONS, ;
         {|name| ::oUI:get_action( "/ui/DebuggerToolBar/" + name ):set_sensitive( !lFlag ), ;
                 ::oUI:get_widget( "/ui/DebuggerToolBar/" + name ):hide():show() } )
  If lFlag
    ::oConsole:DisableInput()
  ELSE
    ::oUI:get_widget( "/ui/DebuggerToolBar/Go" ):set_label( "_Continue" )
    ::oConsole:EnableInput()
  ENDIF
RETURN Self


METHOD OnEdited( oModel, oCell, oPath, cText ) CLASS XHDebuggerGTK
  LOCAL oIter
  LOCAL cValue

  WITH OBJECT oModel
    oIter := :get_iter( oPath )
    cValue := ::ReadExpressionValue( :get_value( oIter, 0 ) + ":=" + cText )
    :set( oIter, 1, cValue[ 1 ], ;
                 2, SubStr( cValue, 2 ) )
  END
RETURN Self


METHOD OnRowExpanded( oModel, nColumn, iter, path ) CLASS XHDebuggerGTK
  LOCAL cExpr, cExprFull, oSubIter, cType, cValue, nLen, i, aKeys
  
  WITH OBJECT oModel
    oSubIter := :iter_children( iter )
    cExpr := :get_value( oSubIter, nColumn )
    IF Len( cExpr ) == 0
      cExpr := :get_value( iter, nColumn )
      cType := :get_value( iter, nColumn + 1 )
      SWITCH cType
        CASE 'A'
          nLen := Val( SubStr( :get_value( iter, nColumn + 2 ), 12 ) )
          EXIT
        CASE 'H'
          aKeys := ::ReadHashKeys( cExpr )
          nLen := Len( aKeys )
          EXIT
        CASE 'O'
          aKeys := ::ReadObjectMessages( cExpr )
          nLen := Len( aKeys )
      END
      FOR i := 1 TO nLen
        SWITCH cType
          CASE 'A'
            cExprFull := cExpr + "[" + LTrim( Str( i ) ) + "]"
            EXIT
          CASE 'H'
            cExprFull := cExpr + "[" + ValToPrgExp( aKeys[ i ] ) + "]"
            EXIT
          CASE 'O'
            cExprFull := cExpr + ":" + aKeys[ i ]
        END
        cValue = ::ReadExpressionValue( cExprFull )
        IF oSubIter == NIL
          oSubIter := :append( iter)
        ENDIF
        :set( oSubIter, nColumn, cExprFull )
        :set( oSubIter, nColumn + 1, cValue[ 1 ] )
        :set( oSubIter, nColumn + 2, SubStr( cValue, 2 ) )
        IF cValue[ 1 ] $ "AHO"
          :append( oSubIter )
        ENDIF
        oSubIter := NIL
      NEXT
    ENDIF
  END
RETURN Self


METHOD RunToCursor() CLASS XHDebuggerGTK
  WITH OBJECT ::oEditor
    IF ::IsValidStopLine( :current_file(), :current_line() )
      ::Until( :current_file(), :current_line() )
    ENDIF
  END
RETURN Self


METHOD ShowUp() CLASS XHDebuggerGTK
  LOCAL nPage
  
  WITH OBJECT ::oBook
    nPage := :get_current_page()
    IF nPage > 0
      :get_nth_page( nPage ):ShowUp()
    ENDIF
  END
RETURN Self


METHOD Stop() CLASS XHDebuggerGTK
  ::Super:Stop()
  
  ::oBook:set_current_page( 0 )
  ::oUI:get_widget( "/ui/DebuggerToolBar/Go" ):set_label( "_Start" )
  ::oUI:get_action( "/ui/DebuggerToolBar/Break" ):set_sensitive( .F. )
  AEval( INTERACTIVE_BUTTONS, {|name| ::oUI:get_action( "/ui/DebuggerToolBar/" + name ):set_sensitive( .F. ) } )
  ::oEditor:go_line( 0 )
RETURN Self
  

METHOD Sync( lSyncCalls ) CLASS XHDebuggerGTK
  LOCAL nPage
  DEFAULT lSyncCalls TO .T.
  
  ::oMonitor:lDirty := .T.
  IF lSyncCalls
    ::oCallStack:lDirty := .T.
  ENDIF
  ::oWatch:lDirty := .T.
  ::oWorkArea:lDirty := .T.
  ::oSets:lDirty := .T.
  ::ShowUp()
  
  ::oWindow:open_src( ::cModule )
  ::oEditor:go_line( ::nLine )
RETURN Self


METHOD ToggleBreak() CLASS XHDebuggerGTK
  WITH OBJECT ::oEditor
    IF ::IsValidStopLine( :current_file(), :current_line() )
      :toggle_bookmark()
      ::Super:ToggleBreak( :current_file(), :current_line() )
    ENDIF
  END
RETURN Self
