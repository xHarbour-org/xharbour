/*
 * $Id$
 */

#include "hbclass.ch"

#include "gdk.ch"
#include "gtk.ch"

#define VXHD_Version "1.71"
#define VXHD_Copyright "© Copyright 2006-"+Str(Year(Date()))+" xHarbour.com Inc. All rights reserved"


PROCEDURE Main( ... )
  LOCAL hParam := parse_cmd_line( hb_aParams() )
  LOCAL oMain

  gtk_init()
  oMain := MainWindow():new( hParam )
  AEval( hParam[ "sources" ], {|s| oMain:open_src( s ) } )
  IF !Empty( hParam[ "host" ] ) .OR. !Empty( hParam[ "args" ] )
    oMain:start()
  ENDIF
  gtk_main()
RETURN


CLASS MainWindow FROM GtkWindow
  METHOD new( hParam ) CONSTRUCTOR
  METHOD about_dialog()
  METHOD create_action_group()
  METHOD open( cFile )
  METHOD open_app_dialog()
  METHOD open_src( cFile ) INLINE ::oEditor:open( find_source( cFile, ::aPath ) )
  METHOD open_src_dialog()
  METHOD start()
  METHOD update_resize_grip( oEvent )
  METHOD update_statusbar()
  DATA oStatusBar
  DATA oDebugger
  DATA oEditor
  DATA cArgs
  DATA cHost
  DATA cPort
  DATA aPath
ENDCLASS


METHOD new( hParam ) CLASS MainWindow
  LOCAL cUI, oUI, oTable, oBar, oPaned

  ::cArgs := hParam[ "args" ]
  ::cHost := hParam[ "host" ]
  ::cPort := hParam[ "port" ]
  ::aPath := hParam[ "sourcepath" ]

  cUI := ( "<ui>" + ;
           "  <menubar name='MenuBar'>" + ;
           "    <menu action='FileMenu'>" + ;
           "      <menu action='OpenMenu'>" + ;
           "        <menuitem action='OpenApp'/>" + ;
           "        <menuitem action='OpenSrc'/>" + ;
           "      </menu>" + ;
           "      <separator/>" + ;
           "      <menuitem action='Quit'/>" + ;
           "    </menu>" + ;
           "    <menu action='RunMenu'>" + ;
           "      <menuitem action='Go'/>" + ;
           "      <menuitem action='Break'/>" + ;
           "      <menuitem action='Stop'/>" + ;
           "      <separator/>" + ;
           "      <menuitem action='StepInto'/>" + ;
           "      <menuitem action='StepOver'/>" + ;
           "      <menuitem action='StepOut'/>" + ;
           "      <separator/>" + ;
           "      <menuitem action='RunToCursor'/>" + ;
           "      <separator/>" + ;
           "      <menuitem action='ToggleBreak'/>" + ;
           "    </menu>" + ;
           "    <menu action='HelpMenu'>" + ;
           "      <menuitem action='About'/>" + ;
           "    </menu>" + ;
           "  </menubar>" + ;
           "  <toolbar name='ToolBar'>" + ;
           "    <toolitem action='OpenApp'/>" + ;
           "    <toolitem action='OpenSrc'/>" + ;
           "    <separator/>" + ;
           "    <toolitem action='Quit'/>" + ;
           "  </toolbar>" + ;
           "  <toolbar name='DebuggerToolBar'>" + ;
           "    <toolitem action='Go'/>" + ;
           "    <toolitem action='Break'/>" + ;
           "    <toolitem action='Stop'/>" + ;
           "    <separator/>" + ;
           "    <toolitem action='StepInto'/>" + ;
           "    <toolitem action='StepOver'/>" + ;
           "    <toolitem action='StepOut'/>" + ;
           "    <separator/>" + ;
           "    <toolitem action='RunToCursor'/>" + ;
           "    <separator/>" + ;
           "    <toolitem action='ToggleBreak'/>" + ;
           "  </toolbar>" + ;
           "</ui>" )

  ::GtkWindow:new():set_default_size( 800, 600 )
  ::set_title( "Visual xHarbour Debugger" )
  ::set_border_width( 2 )

  oUI := GtkUIManager():new()
  oUI:insert_action_group( ::create_action_group(), 0 )
  ::add_accel_group( oUI:get_accel_group() )
  oUI:add_ui_from_string( cUI )
 
  oTable := GtkTable():new( 1, 4, .F. )
  ::add( oTable )

  oBar := oUI:get_widget( "/ui/MenuBar" )
  oTable:attach( oBar, 0, 1, 0, 1, GTK_EXPAND | GTK_FILL, 0, 0, 0 )

  oBar := oUI:get_widget( "/ui/ToolBar" )
  oBar:set_tooltips( .T. )
  oTable:attach( oBar, 0, 1, 1, 2, GTK_EXPAND | GTK_FILL, 0, 0, 0 )

  oPaned := GtkVPaned():new()
  oTable:attach( oPaned, 0, 1, 2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND | GTK_FILL, 0, 0 )

  ::oEditor := XHDebuggerEditorGTK():new()
  ::oDebugger := XHDebuggerGTK():new( ::oEditor, oUI, Self )

  oPaned:pack1( ::oEditor, .T., .F. )
  oPaned:pack2( ::oDebugger:oBox, .T., .F. )

  ::oStatusBar := GtkStatusbar():new()
  oTable:attach( ::oStatusBar, 0, 1, 3, 4, GTK_EXPAND | GTK_FILL, 0, 0, 0 )

  ::connect( "destroy", {|| __Quit() } )
  ::connect( "window_state_event", {|w,e| ::update_resize_grip( e ) } )
  ::update_statusbar()

  ::show_all()
RETURN Self


METHOD about_dialog()
  WITH OBJECT GtkAboutDialog():new()
    :set_name( "xHarbour Visual Debugger" )
    :set_version( VXHD_Version )
    :set_copyright( VXHD_Copyright + Chr( 10 ) + "Powered by xbGTK (http://xbgtk.sf.net)" )
    :set_website( "http://www.xharbour.com" )
    :set_authors( { "Phil Krylov" } )
    :set_license( "STUB: License info" )
    :connect( "response", {|o| o:destroy(), .F. } )
    :show()
  END
RETURN


METHOD create_action_group() CLASS MainWindow
  LOCAL aEntries, oActionGroup

  aEntries = { ;
    { "FileMenu", , "_File" }, ;
    { "RunMenu", , "_Run" }, ;
    { "HelpMenu", , "_Help" }, ;
    { "OpenMenu", GTK_STOCK_OPEN, "_Open" }, ;
    { "OpenApp", GTK_STOCK_EXECUTE, "Open _Application", "<control><shift>O", "Open an application", {|| ::open_app_dialog() } }, ;
    { "OpenSrc", GTK_STOCK_OPEN, "Open _Source File", "<control>O", "Open a source file", {|| ::open_src_dialog() } }, ;
    { "Quit", GTK_STOCK_QUIT, "_Quit", "<control>Q", "Quit", {|| __Quit() } }, ;
    { "About", GTK_STOCK_HELP, "_About", , "About the debugger", {|| ::about_dialog() } }, ;
    { "Go", GTK_STOCK_MEDIA_PLAY, "_Start", "F5", "Start execution", {|| ::start() } }, ;
    { "Break", GTK_STOCK_MEDIA_PAUSE, "_Break", "<control>B", "Break into execution flow", {|| ::oDebugger:Invoke() } }, ;
    { "Stop", GTK_STOCK_MEDIA_STOP, "S_top", "<shift>F5", "Stop", {|| ::oDebugger:Stop() } }, ;
    { "StepInto", GTK_STOCK_JUMP_TO, "Step _Into", "F11", "Step into", {|| ::oDebugger:Step() } }, ;
    { "StepOver", GTK_STOCK_MEDIA_NEXT, "Step _Over", "F10", "Step over", {|| ::oDebugger:Next() } }, ;
    { "StepOut", GTK_STOCK_REDO, "Step O_ut", "<shift>F11", "Step out of current function", {|| ::oDebugger:StepOut() } }, ;
    { "RunToCursor", GTK_STOCK_GOTO_BOTTOM, "_Run to Cursor", "F8", "Run to Cursor", {|| ::oDebugger:RunToCursor() } }, ;
    { "ToggleBreak", GTK_STOCK_INDENT, "Toggle Break_point", "F9", "Toggle Breakpoint", {|| ::oDebugger:ToggleBreak() } } ;
  }
  oActionGroup = GtkActionGroup( "MainWindowActions" ):new()
  oActionGroup:add_actions( aEntries )
RETURN oActionGroup


METHOD open() CLASS MainWindow
  LOCAL nSeconds
  
  IF Empty( ::cHost )
    IF Empty( ::cPort )
      RUN ( ::cArgs + " //debug &" )
    ELSE
      RUN ( ::cArgs + " //debug //debugport:" + ::cPort + " &" )
    ENDIF

    nSeconds := Seconds()
    DO WHILE Seconds() < nSeconds + 2
    ENDDO

    IF Empty( ::cPort )
      ::oDebugger:Start()
    ELSE
      ::oDebugger:Start( , Val( ::cPort ) )
    ENDIF
  ELSE
    IF Empty( ::cPort )
      ::oDebugger:Start( ::cHost )
    ELSE
      ::oDebugger:Start( ::cHost, Val( ::cPort ) )
    ENDIF
  ENDIF
  
RETURN Self


METHOD open_app_dialog() CLASS MainWindow
  LOCAL oDialog

  oDialog := GtkFileChooserDialog():new( "Open Application", Self, ;
                                         GTK_FILE_CHOOSER_ACTION_OPEN, ;
                                         { GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, ;
                                           GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT } )
  IF oDialog:run() == GTK_RESPONSE_ACCEPT
    ::cArgs := oDialog:get_filename()
    ::open()
  ENDIF
  oDialog:destroy()
RETURN Self


METHOD open_src_dialog() CLASS MainWindow
  LOCAL oDialog, oModel, oView, cFile, oCell, iter

  oModel := GtkListStore():new( G_TYPE_STRING )
  WITH OBJECT oModel
    FOR EACH cFile IN ASort( ::oDebugger:GetSourceFiles() )
      :set( :append(), 0, cFile )
    NEXT
  END

  oView := GtkTreeView():new( oModel )
  WITH OBJECT oView
    :append_column( GtkTreeViewColumn():new( "File", GtkCellRendererText():new(), "text", 0 ) )
    :set_search_column( 0 )
    :set_headers_visible( .F. )
    :show_all()
  END

  oDialog := GtkDialog():new()
  WITH OBJECT oDialog
    :set_title( "Open a source file")
    :set_transient_for( Self )
    :vbox:add( oView )
    :add_button( GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT )
    :add_button( "Another source...", GTK_RESPONSE_CANCEL )
  END

  IF oDialog:run() == GTK_RESPONSE_CANCEL .OR. !oView:get_selection():get_selected( , @iter )
    oDialog:destroy()
    oDialog := GtkFileChooserDialog():new( "Open a source file", Self, ;
                                           GTK_FILE_CHOOSER_ACTION_OPEN, ;
                                           { GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, ;
                                             GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT } )
    IF oDialog:run() == GTK_RESPONSE_ACCEPT
      ::open_src( oDialog:get_filename() )
    ENDIF
    oDialog:destroy()
  ELSE
    oDialog:destroy()
    ::open_src( oModel:get_value( iter, 0 ) )
  ENDIF
RETURN Self


METHOD start() CLASS MainWindow
  IF ::oDebugger:socket == NIL
    ::open()
  ELSE
    ::oDebugger():Start()
  ENDIF
RETURN Self


METHOD update_resize_grip( oEvent ) CLASS MainWindow
  LOCAL mask := GDK_WINDOW_STATE_MAXIMIZED | GDK_WINDOW_STATE_FULLSCREEN

  IF oEvent:changed_mask & mask != 0
    ::oStatusBar:set_has_resize_grip( oEvent:new_window_state & mask == 0 )
  ENDIF
RETURN Self


METHOD update_statusbar() CLASS MainWindow
  WITH OBJECT ::oStatusBar
    :pop( 0 )
    :push( 0, "Visual Debugger. " + VXHD_Version + " - " + VXHD_Copyright )
  END
RETURN Self

