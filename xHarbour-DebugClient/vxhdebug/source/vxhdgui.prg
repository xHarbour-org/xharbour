/*
 * $Id$
 */

#include "colors.ch"
#include "vxh.ch"
#include "DEBUG.ch"

#define SCI_MARKERADD 2043

CLASS XHDebuggerGUI FROM XHDebugger
  DATA oEditor
  DATA oTab
  DATA oToolBar
  DATA lFocusConsole INIT .F.
  DATA aTabs
  DATA aSources
  DATA aUnavailableFiles INIT {}

  DATA btnGo, btnBreak, btnStop, btnInto, btnOver, btnOut, btnCursor, btnToggle

  DATA hHook

  ACCESS oApp INLINE __GetApplication()

  METHOD New() CONSTRUCTOR
  METHOD AddPoint( lTrace, cExpr )
  METHOD GetUnavailableEditor( cFile )
  METHOD Inspect( cExpr )
  METHOD InspectSave( oGrid, cExpr, cType, xData )
  METHOD Listen( lFlag )
  METHOD OnStart()
  //METHOD ProcessFnKey( nKey, nParam )
  METHOD DebugHookKeys( nCode, wParam, lParam )
  METHOD RunToCursor()
  METHOD Start() INLINE IIf( ::lStopped .AND. ::oApp:ClassName == "DEBUGGER", ::oApp:Start(), ::Super:Start() ),::oApp:DebuggerPanel:Show()
  METHOD Stop()
  METHOD Sync()
  METHOD ToggleBreak()
ENDCLASS


METHOD New() CLASS XHDebuggerGUI
//  LOCAL lSysColor := ::oApp:DebuggerPanel:BackColor == __GetSystem():CurrentScheme:ToolStripPanelGradientEnd
  ::oEditor := ::oApp:SourceEditor:oEditor

  WITH OBJECT ::oToolBar := ToolStrip( ::oApp:DebuggerPanel )
    :Dock:Left   := :Parent
    :Dock:Top    := :Parent
    :Dock:Right  := :Parent
    :Height      := 18
    :ShowChevron := .F.
    :Create()
    WITH OBJECT ::btnGo := ToolStripButton( :this )
      :Caption    := "Start"
      :ToolTip:Text := "Start <F5>"
      :Action     := {|| ::Start() }
      :Create()
    END

    WITH OBJECT ::btnBreak := ToolStripButton( :this )
      :Caption    := "Break"
      :ToolTip:Text    := "Break into <Ctrl+Alt+Break>"
      :Action     := {|| ::Invoke() }
      :Create()
    END

    WITH OBJECT ::btnStop := ToolStripButton( :this )
      :Caption    := "Stop"
      :ToolTip:Text    := "Stop <Shift+F5>"
      :Action     := {|| ::Stop() }
      :Create()
    END

    WITH OBJECT ::btnInto := ToolStripButton( :this )
      :Caption    := "Step Into"
      :ToolTip:Text    := "Step Into <F11>"
      :Action     := {|| ::Step() }
      :Create()
    END

    WITH OBJECT ::btnOver := ToolStripButton( :this )
      :Caption    := "Step Over"
      :ToolTip:Text    := "Step Over <F10>"
      :Action     := {|| ::Next() }
      :Create()
    END

    WITH OBJECT ::btnOut := ToolStripButton( :this )
      :Caption    := "Step Out"
      :ToolTip:Text    := "Step Out <Shift+F11>"
      :Action     := {|| ::StepOut() }
      :Create()
    END

    WITH OBJECT ::btnCursor := ToolStripButton( :this )
      :Caption := "Run to Cursor"
      :ToolTip:Text := "Run to Cursor <F8>"
      :Action  := {|| ::RunToCursor() }
      :Create()
    END

    WITH OBJECT ::btnToggle := ToolStripButton( :this )
      :Caption    := "Toggle Breakpoint"
      :ToolTip:Text    := "Toggle Breakpoint <F9>"
      :Action     := {|| ::ToggleBreak() }
      :Create()
    END

  END /* ::oToolBar */

  ::oTab := TabControl( ::oApp:DebuggerPanel )
  WITH OBJECT ::oTab
    :Dock:Top     := ::oToolBar
    :Dock:Left    := :Parent
    :Dock:Right   := :Parent
    :Dock:Bottom  := :Parent
    :Dock:Margins := "-1,0,-3,-2"
    :Create()
    :DockIt()

    ::oConsole := XHDebugConsole( :this, Self )
    ::oConsole:Create()

    ::oMonitor := XHDebugMonitor( :this, Self )
    ::oMonitor:Create()

    ::oCallStack := XHDebugCallStack( :this, Self )
    ::oCallStack:Create()

    ::oWatch := XHDebugWatch( :this, Self )
    ::oWatch:Create()

    ::oWorkArea := XHDebugWorkArea( :this, Self )
    ::oWorkArea:Create()

    ::oSets := XHDebugSet( :this, Self )
    ::oSets:Create()

    ::aTabs := { ::oConsole, ::oMonitor, ::oCallStack, ::oWatch, ::oWorkArea, ::oSets }

    :OnSelChanged := {| , , new| IIF( ::socket != NIL .OR. new $ { 1, 3 }, ;
                                      ::aTabs[ new ]:ShowUp(), ;
                                      ::oConsole:Select() ) }
  END /* ::oTab */

  ::Super:New()
  IF !::oApp:IdeActive
     ::hHook := SetWindowsHookEx( WH_KEYBOARD, HB_ObjMsgPtr( Self, "DebugHookKeys" ), NIL, GetCurrentThreadId(), Self )
  ENDIF

RETURN Self


METHOD AddPoint( lTrace, cExpr ) CLASS XHDebuggerGUI
  ::Super:AddPoint( lTrace, cExpr )
  ::oWatch:lDirty := .T.
  IF ::aTabs[ ::oTab:CurSel ] == ::oWatch
    ::oWatch:ShowUp()
  ENDIF
RETURN Self


METHOD GetUnavailableEditor( cFile ) CLASS XHDebuggerGUI
   LOCAL n := AScan( ::oApp:SourceEditor:aDocs, {|o| Empty( o:cFile ) } )
   IF n == 0
      ::oEditor := Source( ::oApp:SourceEditor )
      ::oEditor:TreeItem := ::oApp:FileExplorer:AddItem( "Unavailable code" )
      ::oEditor:TreeItem:Select()
   ELSE
      ::oEditor := ::oApp:SourceEditor:aDocs[ n ]
   ENDIF
   ::oEditor:TreeItem:Select()
   ::oEditor:Load( , "CODE NOT AVAILABLE FOR " + cFile, .T. )
   ::oEditor:lReadOnly := .T.
RETURN n


METHOD Inspect( cExpr ) CLASS XHDebuggerGUI
  LOCAL cValue := ::ReadExpressionValue( cExpr )
  LOCAL aArray, i, aMsg, cElem, oGrid

  IF cValue[ 1 ] == 'A'
    aArray := Array( Val( SubStr( cValue, 13 ) ) )
    FOR i := 1 TO Len( aArray )
      cElem := ::ReadExpressionValue( cExpr + "[" + LTrim( Str( i ) ) + "]" )
      aArray[ i ] := { i, cElem[ 1 ], PadR( SubStr( cElem, 2 ), 256 ) }
    NEXT
    Browse( , , , , aArray, { "Number", "Type", "Value" }, cExpr + " " + SubStr( cValue, 2 ), ;
            {|o| ::Inspect( cExpr + "[" + LTrim( Str( o:DataSource:Record ) ) + "]" ) }, @oGrid )
  ELSEIF cValue[ 1 ] == 'H'
    aMsg := ::ReadHashKeys( cExpr )
    aArray := Array( Len( aMsg ) )
    FOR i := 1 TO Len( aArray )
      cElem := ::ReadExpressionValue( cExpr + "[" + ValToPrgExp( aMsg[ i ] ) + "]" )
      aArray[ i ] := { ValToPrgExp( aMsg[ i ] ), cElem[ 1 ], PadR( SubStr( cElem, 2 ), 256 ) }
    NEXT
    Browse( , , , , aArray, { "Key", "Type", "Value" }, cExpr + " " + SubStr( cValue, 2 ), ;
            {|o| ::Inspect( cExpr + "[" + ValToPrgExp( aMsg[ o:DataSource:Record ] ) + "]" ) }, @oGrid )
  ELSEIF cValue[ 1 ] == 'O'
    aMsg := ::ReadObjectMessages( cExpr )
    aArray := Array( Len( aMsg ) )
    FOR i := 1 TO Len( aArray )
      cElem := ::ReadExpressionValue( cExpr + ":" + aMsg[ i ] )
      aArray[ i ] := { aMsg[ i ], cElem[ 1 ], PadR( SubStr( cElem, 2 ), 256 ) }
    NEXT
    Browse( , , , , aArray, { "Member", "Type", "Value" }, cExpr + " " + SubStr( cValue, 2 ), ;
            {|o| ::Inspect( cExpr + ":" + aMsg[ o:DataSource:Record ] ) }, @oGrid )
  ELSE
    ::oConsole:Out( cExpr + " == (" + cValue[ 1 ] + ") " + SubStr( cValue, 2 ) )
    RETURN Self
  ENDIF

  WITH OBJECT ATail( oGrid:Children )
    :Picture := "@k"
    :Control := {|o, n| If( oGrid:DataSource:Table[ n ][ 2 ] $ "AHO", NIL, ;
                            ( n := MaskEdit( o ), n:SetLimitText( 1024 ), n ) ) }
    :ControlAccessKey := GRID_LCLICK
    :OnSave := {| , oGrid, xData| ::InspectSave( oGrid, cExpr, cValue[ 1 ], xData ) }
  END
RETURN Self


METHOD InspectSave( oGrid, cExpr, cType, xData ) CLASS XHDebuggerGUI
  LOCAL cValue, aTable := oGrid:DataSource:Table
  LOCAL nRec := oGrid:DataSource:Record

  IF cType == 'A'
    cExpr += "[" + LTrim( Str( nRec ) ) + "]"
  ELSEIF cType == 'O'
    cExpr += ":" + aTable[ nRec ][ 1 ]
  ENDIF

  cValue := ::ReadExpressionValue( cExpr + ":=" + xData )
  aTable[ nRec ] := { aTable[ nRec ][ 1 ], cValue[ 1 ], SubStr( cValue, 2 ) }
RETURN .T.


METHOD Listen( lFlag ) CLASS XHDebuggerGUI

  ::Super:Listen( lFlag )

  ::btnBreak:Enabled := lFlag

   AEval( { ::btnGo, ::btnStop, ::btnInto, ::btnOver, ::btnOut, ::btnCursor }, ;
           {|o| o:Enabled := !lFlag } )

  IF lFlag
    ::lFocusConsole := ::oConsole:DisableInput()
  ELSE

    ::btnGo:Caption := "Continue"
    ::btnGo:Tooltip:Text := "Continue <F5>"
    ::oConsole:EnableInput( ::lFocusConsole )
  ENDIF

RETURN Self


METHOD OnStart() CLASS XHDebuggerGUI
  TRY
     ::oApp:DebugWindow:Hide()
     TRY
        ::oApp:Props[ "ViewDebugBuildItem" ]:Checked := .F.
     CATCH
     END
     IF ::oApp:IdeActive
        IF ::hHook != NIL
           UnhookWindowsHookEx( ::hHook )
           ::hHook := NIL
        ENDIF
        ::oApp:EnableBars( .T. )
     ENDIF
  CATCH
  END

  ::oApp:DebuggerPanel:Show()
  ::oApp:DebuggerPanel:DockIt()
  ::oToolBar:Show()
  ::oToolBar:DockIt()

  IF ::oApp:IdeActive
     ::hHook := SetWindowsHookEx( WH_KEYBOARD, HB_ObjMsgPtr( Self, "DebugHookKeys" ), NIL, GetCurrentThreadId(), Self )
     ::oApp:EnableBars( .F. )
  ENDIF

  ::oTab:Children[1]:Select()

  ::oConsole:Clear()

  ::oApp:MainWindow:UpdateWindow()

RETURN Self


//METHOD ProcessFnKey( nKey, nParam ) CLASS XHDebuggerGUI
METHOD DebugHookKeys( nCode, nwParam, nlParam ) CLASS XHDebuggerGUI

  LOCAL lAlt
  LOCAL lCtrl
  LOCAL lShift

  // Bit 31 of nlParam is the TRANSITION flag - Only intrested in KEYUP.
  IF nCode < 0 .OR. nlParam < 0
     RETURN CallNextHookEx( ::hHook, nCode, nwParam, nlParam)
  ENDIF


  lAlt   := GetKeyState( VK_MENU ) & 0x8000 != 0
  lCtrl  := GetKeyState( VK_CONTROL ) & 0x8000 != 0
  lShift := GetKeyState( VK_SHIFT ) & 0x8000 != 0

  //TraceLog( lAlt, lCtrl, lShift, nwParam, nlParam )
  SWITCH nwParam
    CASE VK_RETURN
         ::oConsole:DoIt()
       RETURN 1
    CASE VK_F5
       IF lShift
          ::Stop()
       ELSE
          IF ::socket == NIL
            ::oApp:Start()
          ELSE
            ::Continue()
          ENDIF
       ENDIF
       RETURN 1

    CASE VK_F8
       ::RunToCursor()
       RETURN 1

    CASE VK_F9
       ::ToggleBreak()
       RETURN 1

    CASE VK_F11
       IF lShift
          ::StepOut()
       ELSE
          ::Step()
       ENDIF
       RETURN 1

    CASE VK_F10
       ::Next()
       RETURN 1

    CASE VK_CANCEL
       IF lCtrl .AND. lAlt
          ::Invoke()
          RETURN 1
       ENDIF
       EXIT
  END

RETURN CallNextHookEx( ::hHook, nCode, nwParam, nlParam)


METHOD RunToCursor() CLASS XHDebuggerGUI
   local cFile, nLine
   cFile := ::oApp:SourceEditor:Source:FileName
   nLine := ::oApp:SourceEditor:Source:GetCurLine()+1
   IF ::IsValidStopLine( cFile, nLine )
      ::Super:Until( cFile, nLine )
   ENDIF
RETURN Self


METHOD Stop() CLASS XHDebuggerGUI
  ::Super:Stop()
  IF ::oEditor != NIL
     WITH OBJECT ::oEditor
       :HighlightedLine := NIL
     END
  ENDIF
  IF ::oApp:ClassName != "DEBUGGER"
     ::oApp:DebuggerPanel:Hide()
     IF ::oApp:IdeActive
        IF ::hHook != NIL
           UnhookWindowsHookEx( ::hHook )
           ::hHook := NIL
        ENDIF
        ::oApp:EnableBars( .T. )
     ENDIF
  ELSE
    ::oTab:Children[1]:Select()
    ::btnGo:Caption := "Start"
    ::btnGo:Tooltip:Text := "Start <F5>"

    AEval( { ::btnBreak, ::btnStop, ::btnInto, ::btnOver, ::btnOut, ::btnCursor, ::btnToggle }, {|o| o:Disable() } )

    ::aSources := NIL
  ENDIF

RETURN Self


METHOD Sync() CLASS XHDebuggerGUI
  LOCAL n, cPath, nDocs//, cSource
  LOCAL cFile := "", lAutoFind:=.F.

  ::oMonitor:lDirty := .T.
  ::oCallStack:lDirty := .T.
  ::oWatch:lDirty := .T.
  ::oWorkArea:lDirty := .T.
  ::aTabs[ ::oTab:CurSel ]:ShowUp()

  IF ::oApp:ClassName == "DEBUGGER"
    cFile := ::cModule
    IF ::aSources == NIL
      ::aSources := ASort( ::GetSourceFiles() )
//       WITH OBJECT ::oApp:MainWindow:oButtonOpenSrc
//         :Menu:Destroy()
//         :Menu := MenuPopup( :Parent )
//         FOR EACH cSource IN ::aSources
//           :AddMenuItem( cSource ):Action := {|o| ::oApp:Project:OpenSource( o:Caption, .T. ) }
//         NEXT
//       END
    ENDIF
  ELSE
    IF ::oApp:Project:Properties == NIL
       cPath := ""
      ELSE
       cPath := ::oApp:Project:Properties:Path
    ENDIF

    cFile := cPath + "\" + ::oApp:Project:Properties:Source + "\" + ::cModule
  END

  //::oEditor := NIL

  SetForegroundWindow( ::oApp:MainWindow:hWnd )
  //SetActiveWindow( ::oApp:MainWindow:hWnd )
  //SetFocus( ::oApp:MainWindow:hWnd )

  nDocs := ::oApp:SourceEditor:DocCount

  IF ( n := AScan( ::oApp:SourceEditor:aDocs, {|o| Lower( IIF( '\' IN cFile, o:File, o:FileName ) ) == Lower( cFile ) } ) ) > 0
    ::oEditor := ::oApp:SourceEditor:aDocs[ n ]
    ::oApp:Project:SourceTabChanged( n )

    IF !::oApp:SourceEditor:IsWindowVisible()
      ::oApp:EditorPage:Select()
    ENDIF

    ::oApp:SourceEditor:SetFocus()
  ELSE
    ::oApp:SourceEditor:Show()

    IF lAutoFind
       cFile:=find_source( cFile, ::oApp:aPath )
    ENDIF

    IF File( cFile )
      ::oEditor := Source( ::oApp:SourceEditor, cFile )
      IF RIGHT( UPPER( cFile ), 4 ) == ".XFM"
         ::oEditor:lReadOnly := .T.
      ENDIF
      IF __objhasMsg( ::oApp:FileExplorer, "SetFile" )
         ::oApp:FileExplorer:SetFile( ::oEditor )
      ENDIF
      IF ::oEditor:TreeItem != NIL
         ::oEditor:TreeItem:Select()
      ENDIF

      //::oApp:SourceTabs:InsertTab( ::cModule )
      //::oApp:SourceTabs:SetCurSel( nDocs )
      //::oApp:Project:SourceTabChanged( nDocs )

      IF !::oApp:SourceEditor:IsWindowVisible()
        ::oApp:EditorPage:Select()
      ENDIF

      //::oApp:SourceEditor:SetFocus()

      OnShowEditors()
    ELSE
      IF cFile $ ::aUnavailableFiles
        n := ::GetUnavailableEditor( cFile )
      ELSE
        ::oApp:Project:OpenSource( cFile )


        nDocs := ::oApp:SourceEditor:DocCount
        IF ( n := AScan( ::oApp:SourceEditor:aDocs, ;
                         {|o| Lower( IIF( '\' IN cFile, o:cPath, "" ) + o:cFile ) ;
                              == Lower( cFile ) } ) ) > 0
          ::oEditor := ::oApp:SourceEditor:aDocs[ n ]

          IF RIGHT( UPPER( cFile ), 4 ) == ".XFM"
            ::oEditor:lReadOnly := .T.
          ENDIF
        ELSE
          n := ::GetUnavailableEditor( cFile )
          AAdd(::aUnavailableFiles, cFile)
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  ::oEditor:GoLine( ::nLine )
  ::oEditor:Highlight()
RETURN Self


METHOD ToggleBreak() CLASS XHDebuggerGUI
   local cFile, nLine
   cFile := ::oApp:SourceEditor:Source:FileName
   nLine := ::oApp:SourceEditor:Source:GetCurLine()+1
   IF ::IsValidStopLine( cFile, nLine )
      ::oApp:SourceEditor:ToggleBookmark()
      ::Super:ToggleBreak( cFile, nLine )
   ENDIF
RETURN Self
