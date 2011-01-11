/*
 * $Id$
 */

STATIC cCurFolder

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "hbexcept.ch"

#include "inkey.ch"
#include "commdlg.ch"
#include "fileio.ch"
#include "xEditConstants.ch"

//#define VXH

#define VXHD_Version "1.71"

ANNOUNCE GTSys

PROCEDURE Main( ... )
  LOCAL hParam := parse_cmd_line( hb_aParams() )

  Debugger():Init( hParam )
RETURN


PROCEDURE OnShowEditors()
RETURN


CLASS Debugger FROM Application

  DATA DefaultFolder INIT ""
  DATA Project
  DATA SourceTabs
  DATA EditorPage
  DATA FileMenu
  DATA CloseMenu
  DATA DebuggerPanel
  DATA SourceEditor
  DATA oDebugger
  DATA CoolBar
  DATA DebugWindow
  DATA MainWindow
  DATA Exe           INIT ""
  DATA Args          INIT ""
  DATA RunMode       INIT 1
  DATA cHost
  DATA cPort
  DATA aPath
  DATA cStartPath
  DATA aSources

  METHOD Init( hParam ) CONSTRUCTOR
  METHOD Open( cExe, aSources )
  METHOD Start()
  METHOD SetEditorPos( nLine, nColumn )
ENDCLASS


METHOD Init( hParam ) CLASS Debugger

  ::Super:Init( NIL )

    // Global var to tel app's that VXH is in designmode
   //::IdeActive := TRUE
   
   #ifdef XDEBUG_DEMO
   
    MessageBox( GetActiveWindow(), "Thank you for evaluating Visual xDebugger." + CRLF + CRLF + ;
                                   "Copyright (c) 2006-"+Str(Year(Date()))+" xHarbour.com Inc." + CRLF + ;
                                   "http://www.xHarbour.com" + CRLF ;
                                  ,"Visual xDebugger Demo", MB_OK | MB_ICONINFORMATION )
   #endif

  ::System:CurrentScheme:AutoScheme := .F.
  ::System:CurrentScheme:ColorScheme := "Classic"
  ::System:CurrentScheme:Load()
  
  ::MainWindow := MainWindow():Init()

  ::Project := Project( NIL )

  ::cHost      := hParam[ "host" ]
  ::cPort      := hParam[ "port" ]
  ::aPath      := hParam[ "sourcepath" ]
  ::cStartPath := hParam[ "startpath" ]
  ::aSources   := hParam[ "sources" ]

  ::Open( hParam[ "args" ], hParam[ "sources" ] )

  ::Run()

RETURN Self


METHOD Open( cArgs, aSources ) CLASS Debugger

  LOCAL nAt, cExe, cStartPath

  IF ::oDebugger == NIL
    ::oDebugger := XHDebuggerGUI():new( __GetApplication() )
  ELSE
    ::oDebugger:lStopped := .F.
  ENDIF

  IF Empty( ::cHost )
    IF Empty( cArgs )
       RETURN Self
    ENDIF

    nAt := At( ".exe", Lower( cArgs ) )
    IF nAt > 0
      cExe  := Left( cArgs, nAt + 3 )
      ::Exe := cExe
      cArgs := SubStr( cArgs, nAt + 4 )
      ::Args := cArgs

      IF !Empty( cExe ) .AND. File( cExe )
        #if 0
            IF !Empty( aSources )
               AEval( aSources, {|x| ::Project:OpenSource( x, .T., ::aPath ) } )
            ENDIF
        #else
           (aSources)
        #endif

        IF ! Empty( ::cStartPath )
           cExe := RelativeToAbsolutePath( cExe, DiskName() + ':\' + CurDir() )

           cStartPath := ::cStartPath

           IF cStartPath[2] == ':'
             DiskChange( ::cStartPath[1] )
             cStartPath := SubStr( cStartPath, 3 )
           ENDIF

           IF ! Empty( cStartPath )
             IF DirChange( cStartPath ) != 0
                Throw( ErrorNew( "xDebug", 0, 1001, ProcName(), "Could not switch to folder: " + cStartPath, HB_aParams() ) )
             ENDIF
           ENDIF
        ENDIF

        ShellExecute( GetActiveWindow(), "open", '"' + cExe + '"', ;
                      cArgs + " //debug" + If( !Empty( ::cPort ), " //debugport:" + ::cPort, "" ), ;
                      , SW_SHOW )

        ::oDebugger:Start( , If( !Empty( ::cPort ), Val( ::cPort ), ) )
      ENDIF
    ENDIF
  ELSE
    ::oDebugger:Start( ::cHost, If( !Empty( ::cPort ), Val( ::cPort ), ) )
  ENDIF

RETURN Self


METHOD Start() CLASS Debugger
  ::Open( ::Exe + " " + ::Args )
RETURN Self


METHOD SetEditorPos( nLine, nColumn ) CLASS Debugger
   LOCAL cCol, cLine
   cLine := "Row: "+ XSTR( nLine )
   cCol  := "Col: "+ XSTR( nColumn )

   WITH OBJECT ::MainForm
      :StatusBarPanel3:Width := :Drawing:GetTextExtentPoint32( cLine )[1] + 10
      :StatusBarPanel3:Caption := cLine                   //PADC( XSTR( nLine ), :StatusBarPanel3:Width / 4 )
      :StatusBarPanel4:Width := :Drawing:GetTextExtentPoint32( cCol )[1] + 10
      :StatusBarPanel4:Caption := cCol                    //PADC( XSTR( nColumn ), :StatusBarPanel4:Width / 4 )
   END
RETURN Self


CLASS MainWindow FROM WinForm
  METHOD Init() CONSTRUCTOR
  DATA oButtonOpenSrc
  DATA oButtonOpen
ENDCLASS

METHOD Init() CLASS MainWindow

   LOCAL aEntries, cProject, oItem

   ::Super:Init()

   ::Caption := "Visual xHarbour Debugger"
   ::Left    := 0
   ::Top     := 0
   ::Width   := 800
   ::Height  := 600
   ::Icon    := ::Application:LoadIcon( "VXHDICON" )

   ::Create()

   //::Show()
   ::Cursor  := IDC_ARROW

   //::Maximize()

   // StatusBar section ----------------------
   StatusBar( Self )
   WITH OBJECT ::StatusBar1
      :ImageList := ImageList( :this, 16, 16 ):Create()
      :ImageList:AddIcon( "AMAIN" )
      :Create()

      WITH OBJECT StatusBarPanel( :this )
         :ImageIndex := 0
         :Caption    := "Visual Debugger. " + VXHD_Version + " - Copyright " + CHR(169) + " 2003-"+Str(Year(Date()))+" xHarbour.com Inc. All rights reserved"
         :Width      := :Parent:Drawing:GetTextExtentPoint32( :Caption )[1] + 40
         :Create()
      END
      WITH OBJECT StatusBarPanel( :this )
         :Width      := 120
         :Create()
      END
      WITH OBJECT StatusBarPanel( :this )
         :Width      := -1
         :Create()
      END
      WITH OBJECT StatusBarPanel( :this )
         :Width      := 140
         :Create()
      END
   END

   //Alert( "0" )

   // CoolMenu section --------
   
   WITH OBJECT CoolMenu( Self )
      :ImageList := ImageList( :this, 16, 16 ):Create()
      :ImageList:AddImage( IDB_STD_SMALL_COLOR )
      :ImageList:MaskColor := C_LIGHTCYAN
      :ImageList:AddBitmap( "TBEXTRA" )
      :Create()

      ::Application:FileMenu := CoolMenuItem( :this )
      WITH OBJECT ::Application:FileMenu
         :Caption := "&File"/*, :ImageList */
         :Create()
         //---------------------------------------------------------------
         
         WITH OBJECT :AddMenuItem( "&Open" )
            :ImageList    := ::CoolMenu1:ImageList
            :ImageIndex   := STD_FILEOPEN
            :Create()

            WITH OBJECT :AddMenuItem( "&Application" )
               :ImageIndex   := 22
               :ShortCutText := "Ctrl+Shift+O"
               :ShortCut     := { FVIRTKEY | FCONTROL | FSHIFT, ASC( "O" ) }
               :Action       := {|| ::Application:Project:Open() }
               :Create()
            END

            WITH OBJECT :AddMenuItem( "&File" )
               :ImageIndex   := STD_FILEOPEN
               :ShortCutText := "Ctrl+O"
               :ShortCut     := { FVIRTKEY | FCONTROL, ASC( "O" ) }
               :Action       := {|| ::Application:Project:OpenSource() }
               :Create()
            END
         END

         WITH OBJECT ::Application:CloseMenu := :AddMenuItem( "&Close Application" )
            :ImageIndex   := 99
            :Action := {|| ::Application:Project:Close() }
            //:Disable()
            :Create()
         END

         WITH OBJECT ::Application:CloseMenu := :AddMenuItem( "Close &Source file" )
            :ImageIndex   := 99
            :Action := {|| ::Application:Project:CloseSource() }
            //:Disable()
            :Create()
         END

         :AddMenuItem( "-" )

         WITH OBJECT :AddMenuItem( "&Exit  " )
            :ImageIndex   := 99
            :Action := {|| ::Application:MainWindow:Close() }
            :Create()
         END
      END
   END

   // ToolBar section: Standard Project Commands -----
   
   WITH OBJECT ToolBar( Self )
      :Create()

      :ImageList := ImageList( :this, 16, 16 ):Create()
      :ImageList:AddImage( IDB_STD_SMALL_COLOR )
      :ImageList:MaskColor := C_LIGHTCYAN
      :ImageList:AddBitmap( "TBEXTRA" )
      :SetImageList( :ImageList )
      :DrawArrows()

      WITH OBJECT ::oButtonOpen := ToolButton( :this )
         :ImageIndex := STD_FILEOPEN
         :ToolTip    := "Open"
         :Action     := {|| ::Application:Project:Open() }
         :DropDown   := .T.
         :Create()
         aEntries := ::Application:AppIniFile:GetEntries( "Recent" )

         :Menu := MenuPopup( :Parent )
         FOR EACH cProject IN aEntries
             oItem := :AddMenuItem( cProject )
             oItem:Action := {|o| ::Application:Project:Open( o:Caption ) }
         NEXT
      END

      WITH OBJECT ToolButton( :this )
         :ImageIndex := 15
         :ToolTip    := "Close"
         :Action     := {|| ::Application:Project:Close() }
         :Create()
         :Disable()
      END

      WITH OBJECT ToolButton( :this )
         :ImageIndex := STD_FILESAVE
         :ToolTip    := "Save"
         :Action     := {|| ::Application:Project:Save() }
         :Create()
         :Disable()
      END

      WITH OBJECT ::oButtonOpenSrc := ToolButton( :this )
         :ImageIndex := STD_FILEOPEN
         :ToolTip    := "Open source"
         :Action     := {|| ::Application:Project:OpenSource() }
         :DropDown := .T.
         :Create()
         :Menu := MenuPopup( :Parent )
      END
   END

   //Alert( "1" )

   ::Application:CoolBar := CoolBar( Self )
   ::CoolBar1:Create()

   WITH OBJECT ::CoolBar1
      
      WITH OBJECT CoolBarBand( :this )
         :MinHeight := 22
         :BandChild := ::CoolMenu1
         :Chevron   := .T.
         :Create()
      END

      // CollBarBand 1 Standard command
      WITH OBJECT CoolBarBand( :this )
         :MinWidth    := 200
         :MinHeight   := 22
         :BandChild   := ::ToolBar1
         :Break       := .T.
         :Chevron     := .T.
         :AllowUndock := .T.
         :Create()
      END
   END

   //Alert( "2" )

   // Panel EXCLUSIVE for vxh-debugger
   WITH OBJECT ::Application:DebuggerPanel := Panel( Self )
      :Caption        := "Debugger"
      :Left           := 0
      :Top            := 0
      :Width          := 680
      :Height         := 200
      :StaticEdge     := .T.
      :Dock:Left      := :Parent
      :Dock:Bottom    := ::StatusBar1
      :Dock:Right     := :Parent
      :AllowClose     := .T.
      :AllowUndock    := .T.
      :OnWMSysCommand := {|o,n| If( n == SC_CLOSE, ( o:Hide(), 0 ), ) }

      Splitter( :Parent )
      ::Splitter1:Owner := :this
      ::Splitter1:Create()

      :Create()
      :Hide()
   END

   //Alert( "3" )

   // TabControl
   
   WITH OBJECT TabControl( Self )
      :Flat      := TRUE
      :Width     := 680
      :Height    := 300
      :Multiline := .T.

      //:AllowClose  := .T.
      :BoldSelection := .T.
      :AutoDock := .T.

      :Dock:Margin := 0
      :Dock:Left   := :Parent
      :Dock:Top    := ::CoolBar1
      :Dock:Right  := :Parent
      :Dock:Bottom := ::Application:DebuggerPanel

      :Create()

      WITH OBJECT ::Application:EditorPage := TabPage( :this )
         :Caption := "   Source Code Editor  "
         :OnWMShowWindow := {|| OnShowEditors() }
         :Create()

         WITH OBJECT ::Application:SourceTabs := TabControl( :this )
            :Height       := 22
            :Frame        := .F.
            :Flat         := .T.
            :Dock:Left    := :Parent
            :Dock:Top     := :Parent
            :Dock:Right   := :Parent
            :OnSelChanged := {|,x,y| ::Application:Project:SourceTabChanged( x,y ) }
            :Create()
         END

         WITH OBJECT StatusBar( :this )
            :Create()

            WITH OBJECT StatusBarPanel( :this )
               :Width      := 320
               :Create()
            END
            WITH OBJECT StatusBarPanel( :this )
               :Width      := -1
               :Create()
            END
            WITH OBJECT StatusBarPanel( :this )
               :Width      := 35
               :Create()
            END
            WITH OBJECT StatusBarPanel( :this )
               :Width      := 30
               :Create()
            END
            WITH OBJECT StatusBarPanel( :this )
               :Width      := 100
               :Create()
            END
            WITH OBJECT StatusBarPanel( :this )
               :Width      := 50
               :Create()
            END
         END

         WITH OBJECT ::Application:SourceEditor := SourceEditor( :this )
            :Left        := 400
            :Top         := 100
            :Width       := 200
            :Height      := 600

            :Dock:Margin := 2
            :Dock:Left   := :Parent
            :Dock:Top    := ::Application:SourceTabs
            :Dock:Right  := :Parent
            :Dock:Bottom := ::StatusBar1

            :Create()
            :DockIt()
            //:Disable()
         END

      END
   END

   //Alert( "4" )

   ::Maximize()

   //::Show()

RETURN Self

CLASS SourceEditor FROM Control
  VAR oEditor
  METHOD Init( oParent ) CONSTRUCTOR
  METHOD Create()
ENDCLASS

METHOD Init( oParent ) CLASS SourceEditor
  ::ClsName := "xEdit"
  ::Super:Init( oParent )
  ::Style := WS_CHILD | WS_VISIBLE | WS_TABSTOP | ES_WANTRETURN | ES_MULTILINE | ES_AUTOHSCROLL | ES_AUTOVSCROLL | WS_VSCROLL | WS_HSCROLL | ES_MULTILINE
  ::ExStyle := WS_EX_CLIENTEDGE
RETURN Self


METHOD Create() CLASS SourceEditor
   LOCAL nPointer

  ::Super:Create()
  IF ::hWnd != NIL
    nPointer := SendMessage( ::hWnd, EN_GETEDITOR )
    IF nPointer != 0
      ::oEditor := ArrayFromPointer( nPointer )
    ENDIF
  ENDIF
RETURN Self

CLASS Project
   ACCESS Application    INLINE __GetApplication()
   ACCESS System         INLINE __GetSystem()

   DATA Windows          EXPORTED INIT {}
   DATA Programs         EXPORTED INIT {}
   DATA Index            PROTECTED
   DATA lModified        PROTECTED INIT .F.
   DATA ProjectFile      EXPORTED
   DATA CurrentProgram   EXPORTED

   DATA Properties       EXPORTED
   DATA CopyBuffer       EXPORTED INIT {}
   DATA PasteOn          EXPORTED INIT .F.
   DATA lDebugging       EXPORTED INIT .F.
   DATA Debugger         EXPORTED

   ASSIGN Modified(lMod) INLINE ::SetCaption( lMod )
   ACCESS Modified       INLINE ::lModified

   METHOD Init() CONSTRUCTOR
   METHOD SetCaption( lMod )
   METHOD Save()
   METHOD Close()
   METHOD Open()
   METHOD ResetQuickOpen()
   METHOD EditCopy()
   METHOD EditCut()
   METHOD EditPaste()
   METHOD EditReset()

   METHOD OpenSource( cSource, lNoDialog )
   METHOD CloseSource()
   METHOD SaveSource()
   METHOD SaveSourceAs()

   METHOD SourceTabChanged()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init() CLASS Project
RETURN Self

METHOD SetCaption( lMod ) CLASS Project

   LOCAL cCaption

   ::lModified := lMod

   IF ::Properties == NIL
      cCaption := "Visual xHarbour Debugger"
     ELSE
      IF ::Properties:Name == NIL
         cCaption := "Visual xHarbour Debugger [Untitled"
       ELSE
         cCaption := "Visual xHarbour Debugger [" + ::Properties:Path + "\" + ::Properties:Name + ".vxh"
      ENDIF

      cCaption += IIF( lMod," * ]","]" )

      IF lMod
         //::Application:MainWindow:ToolBar1:ToolButton4:Enable()   // Save Button
       ELSE
         //::Application:MainWindow:ToolBar1:ToolButton4:Disable()   // Save Button
      ENDIF
   ENDIF

   ::Application:MainWindow:Caption := cCaption

RETURN Self

METHOD EditReset(n) CLASS Project

   LOCAL lCopied, lSelected

   DEFAULT n TO IIF( ::Application:SourceEditor:Parent:Hidden, 1, 0 )

   // TODO! Edit Menu
   TRY
      IF n == 1
         lCopied   := .F.
         lSelected := .F.
       ELSE
         lCopied   := IsClipboardFormatAvailable( CF_TEXT )
         lSelected := ::Application:SourceEditor:oEditor:nLineFrom > 0
      ENDIF
      IF lCopied
         ::Application:EditMenu:CoolMenuItem3:Enable()  // Paste MenuItem
         ::Application:MainWindow:ToolBar2:ToolButton3:Enable()   // Paste Button
       ELSE
         ::Application:EditMenu:CoolMenuItem3:Disable()  // Paste MenuItem
         ::Application:MainWindow:ToolBar2:ToolButton3:Disable()   // Paste Button
      ENDIF

      IF !lSelected
         ::Application:EditMenu:CoolMenuItem1:Disable()
         ::Application:EditMenu:CoolMenuItem2:Disable()
         ::Application:MainWindow:ToolBar2:ToolButton1:Disable()
         ::Application:MainWindow:ToolBar2:ToolButton2:Disable()
       ELSE
         ::Application:EditMenu:CoolMenuItem1:Enable()
         ::Application:EditMenu:CoolMenuItem2:Enable()
         ::Application:MainWindow:ToolBar2:ToolButton1:Enable()
         ::Application:MainWindow:ToolBar2:ToolButton2:Enable()
      ENDIF
   CATCH
   END

RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD SourceTabChanged( nPrev, nCur ) CLASS Project
   (nPrev)
   ::Application:SourceEditor:oEditor := xEdit_GetEditors()[ nCur ]
   ::Application:SourceEditor:oEditor:SetDisplay( ::Application:SourceEditor:oEditor:oDisplay, .T. )
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD EditCopy() CLASS Project
      // Simulate keystrokes for xEdit
      keybd_event( VK_CONTROL )
      keybd_event( ASC("C") )
      keybd_event( ASC("C"), KEYEVENTF_KEYUP )
      keybd_event( VK_CONTROL, KEYEVENTF_KEYUP )
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD EditCut() CLASS Project
   
      // Simulate keystrokes for xEdit
      keybd_event( VK_CONTROL )
      keybd_event( ASC("X") )
      keybd_event( ASC("X"), KEYEVENTF_KEYUP )
      keybd_event( VK_CONTROL, KEYEVENTF_KEYUP )

RETURN NIL

METHOD EditPaste() CLASS Project
      keybd_event( VK_CONTROL )
      keybd_event( ASC("V") )
      keybd_event( ASC("V"), KEYEVENTF_KEYUP )
      keybd_event( VK_CONTROL, KEYEVENTF_KEYUP )
RETURN NIL

//-------------------------------------------------------------------------------------------------------

METHOD Close() CLASS Project

   LOCAL nRes, n, oMsg, lRem

   IF ::Modified
      nRes := MessageBox( GetActiveWindow(), "Save changes before closing?", IIF( EMPTY(::Properties:Name), "Untitled", ::Properties:Name ), MB_YESNOCANCEL | MB_ICONQUESTION )
      SWITCH nRes
         CASE IDYES
              ::Save()
              EXIT

         CASE IDCANCEL
              RETURN .F.
      END
   ENDIF

  IF ::lDebugging
     ::DebugStop()
  ENDIF

  lRem := .F.
   FOR n := 1 TO LEN( xEdit_GetEditors() )
       IF xEdit_GetEditors()[n]:lModified
          IF nRes == NIL
             oMsg := MsgBoxEx( ::Application:MainWindow, "Save changes to "+xEdit_GetEditors()[n]:cPath+xEdit_GetEditors()[n]:cFile+" before closing?", "Source File", IDI_QUESTION )
             nRes := oMsg:Result
             IF nRes == 4002 // No to All
                lRem := .T.
              ELSEIF nRes == 4001 // Yes to All
                nRes := IDYES
                lRem := .T.
             ENDIF
          ENDIF
          SWITCH nRes
             CASE IDYES
                  IF EMPTY( xEdit_GetEditors()[n]:cFile )
                     ::SaveSourceAs( xEdit_GetEditors()[n] )
                   ELSE
                     xEdit_GetEditors()[n]:Save()
                  ENDIF
                  EXIT

             CASE IDCANCEL
                  RETURN .F.
          END
          IF !lRem
             nRes := NIL
          ENDIF
       ENDIF

       xEdit_GetEditors()[n]:lModified := .F.
       ::Application:SourceTabs:DeleteTab(n)

       xEdit_GetEditors()[n]:Close()
       n--
   NEXT

   ::Application:MainWindow:ToolBar1:ToolButton1:Disable()   // Close Button
   ::Application:MainWindow:ToolBar1:ToolButton2:Disable()   // Save Button


   ::Application:MainWindow:Caption := "Visual Debugger"

   ::Application:SourceEditor:Disable()
   ::Application:SourceEditor:Hide()

   ::Application:EditorPage:Select()

   ::CopyBuffer := {}
   ::PasteOn := .F.

   ::ProjectFile       := NIL
   ::Modified          := .F.
   ::CurrentProgram    := NIL
   ::Properties        := NIL
   ::Modified          := .F.

   ::EditReset(1)

   ::Application:SourceEditor:Caption := ""

   HB_GCALL()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
METHOD SaveSource( oEditor ) CLASS Project
   DEFAULT oEditor TO ::Application:SourceEditor:oEditor
   oEditor:Save()
   oEditor:lModified := .F.
RETURN Self

METHOD SaveSourceAs( oEditor, lSetTabName ) CLASS Project
   LOCAL ofn, cFile, n, x
   DEFAULT oEditor TO ::Application:SourceEditor:oEditor
   DEFAULT lSetTabName TO .F.

   WHILE .T.
      ofn := (struct OPENFILENAME)

      ofn:lStructSize     := 76
      ofn :hwndOwner      := ::Application:MainWindow:hWnd
      ofn:hInstance       := GetModuleHandle()
      ofn:nMaxFile        := MAX_PATH + 1

      ofn:lpstrFile       := Space( MAX_PATH )
      ofn:lpstrDefExt     := "prg"
      ofn:lpstrFilter     := "xHarbour source files" + Chr(0) + "*.prg;*.ch;*.xbs;*.xfm" + Chr(0) +;
                             "C sources"             + Chr(0) + "*.c;*.cpp;*.h"          + Chr(0) +;
                             "Resource source file"  + Chr(0) + "*.rc"                   + Chr(0) +;
                             "xBuild project files"  + Chr(0) + "*.xbp;*.inc"            + Chr(0) +;
                             "Log files"             + Chr(0) + "*.log"                  + Chr(0) +;
                             "Text files"            + Chr(0) + "*.txt"                  + Chr(0) +;
                             "All files"             + Chr(0) + "*.*"                    + Chr(0)
      ofn:lpstrTitle      := "Visual xHarbour - Save File As"
      ofn:Flags           := OFN_NOREADONLYRETURN | OFN_OVERWRITEPROMPT | OFN_PATHMUSTEXIST

      IF GetSaveFileName( @ofn )
         cFile := Left( ofn:lpstrFile, At( Chr(0), ofn:lpstrFile ) - 1 )
         n := ASCAN( xEdit_GetEditors(), {|o| o == oEditor} )
         IF ( x := ASCAN( xEdit_GetEditors(), {|o| o:cPath+o:cFile == cFile } ) ) > 0 .AND. x != n
            MessageBox( 0, cFile +" cannot be overwritten because it's being edited, please select another path and / or name", "Save As", MB_ICONEXCLAMATION )
            ::Application:Yield()
            LOOP
         ENDIF
         oEditor:Save( cFile )
         IF lSetTabName
            ::Application:SourceTabs:SetItem( n, oEditor:cFile )
         ENDIF
      ENDIF
      EXIT
   ENDDO
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CloseSource() CLASS Project

   LOCAL nRes, n := ::Application:SourceTabs:GetCurSel()

   nRes := IDNO
   IF ::Application:SourceEditor:oEditor:lModified
      nRes := MessageBox( 0, "Save changes before closing?", ::Application:SourceEditor:oEditor:cFile, MB_YESNOCANCEL | MB_ICONQUESTION )
      IF nRes == IDCANCEL
         RETURN Self
      ENDIF
   ENDIF

   ::Application:SourceTabs:DeleteTab( n )
   IF nRes == IDYES
      ::SaveSource()
   ENDIF

   ::Application:SourceEditor:oEditor:Close()

   n := MIN( n, LEN( xEdit_GetEditors() ) )
   IF n > 0
      ::Application:SourceTabs:SetCurSel( n )
      ::SourceTabChanged(, n )
    ELSE
      ::Application:SourceEditor:Caption := ""
      ::Application:SourceEditor:Hide()

      ::Application:CloseMenu:Caption := "&Close Project"
      ::Application:CloseMenu:Disable()
   ENDIF

RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD OpenSource( cSource, lNoDialog ) CLASS Project
   LOCAL oFile, n, oEditor, cPath, cName
   LOCAL aPath := ::Application:aPath

   DEFAULT cSource TO ""
   DEFAULT lNoDialog TO .F.

   //Alert( ProcName(1) + "->\" + CurDir() + "->" + CStr( lNoDialog ) + "->" + cSource + "->" + CStr( File( cSource ) ) )

   IF lNoDialog .AND. ( ! Empty( cSource ) ) .AND. ( File( cSource ) .OR. File( find_source( cSource, aPath ) ) )
      n := Max( RAt( "\", cSource ), RAt( "/", cSource ) )
      IF n == 0
        cSource := find_source( cSource, aPath )
        n := Max( RAt( "\", cSource ), RAt( "/", cSource ) )
      ENDIF
      cPath := If( n == 0, ::Application:DefaultFolder, Left( cSource, n - 1 ) )
      cName := SubStr( cSource, n + 1 )
   ELSE
      IF ValType( cSource ) == 'C'
        IF !( '/' $ cSource .OR. '\' $ cSource )
          cSource := find_source( cSource, aPath )
        ENDIF
      ENDIF

      oFile := CFile( cSource )
      oFile:Flags := OFN_NOCHANGEDIR + OFN_FILEMUSTEXIST

      IF Empty( cSource ) .OR. ( n := Max( RAt( "\", cSource ), RAt( "/", cSource ) ) ) == 0
        oFile:Path := ::Application:DefaultFolder
      ELSE
        oFile:Name := SubStr( cSource, n + 1 )
        oFile:Path := Left( cSource, n - 1 )
      ENDIF

      IF !File( cSource )
         IF Empty( cSource )
            oFile:AddFilter( "Source File (*.prg,*.c,*.cpp,*.rc)", "*.prg;*.c;*.cpp;*.rc" )
         ELSE
            oFile:AddFilter( "Source File " + oFile:Name, oFile:Name )
         ENDIF

         oFile:OpenDialog()

         ::Application:MainWindow:UpdateWindow()

         IF oFile:Result == IDCANCEL
            RETURN Self
         ENDIF
      ENDIF

      cPath := oFile:Path
      cName := oFile:Name
   ENDIF

   cPath += '\'

   //Alert( cPath + "->" + cName )

   IF ( n := ASCAN( xEdit_GetEditors(), {|o| o:cPath == cPath .AND. o:cFile == cName } ) ) > 0
      // File is open, just re-show

      ::Application:SourceTabs:SetCurSel( n )
      ::SourceTabChanged(, n )

      IF !::Application:SourceEditor:IsWindowVisible()
         ::Application:EditorPage:Select()
      ENDIF
      ::Application:SourceEditor:SetFocus()
      RETURN Self
   ENDIF

   ::Application:SourceEditor:Show()

   //Alert( "Open: " + cPath + "->" + cName )

   oEditor := Editor():New( ,,,, cPath + cName, ::Application:SourceEditor:oEditor:oDisplay )
   oEditor:SetExtension( "prg" )

   ::Application:SourceTabs:InsertTab( cName )
   ::Application:SourceTabs:SetCurSel( LEN( xEdit_GetEditors() ) )
   ::SourceTabChanged(, LEN( xEdit_GetEditors() ) )

   IF !::Application:SourceEditor:IsWindowVisible()
      ::Application:EditorPage:Select()
   ENDIF

   ::Application:SourceEditor:SetFocus()
   OnShowEditors()

RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Open( cProject ) CLASS Project

   LOCAL oFile

   (cProject)
   
   oFile := CFile( "" )

   //oFile:Flags := OFN_NOCHANGEDIR
   oFile:AddFilter( "Application File (*.exe)", "*.exe" )
   oFile:Path := ::Application:DefaultFolder

   WHILE .T.
      oFile:OpenDialog()

      IF oFile:Result == IDCANCEL
         RETURN Self
      ENDIF

      IF lower( Right( oFile:Name, 4 ) ) == ".exe"
         EXIT
      ENDIF

      Tone()
   END

   ::Application:MainWindow:UpdateWindow()

   ::Modified := .F.
   ::Application:Open( oFile:Path + "\" + oFile:Name + " " + ::Application:Args )

RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Save() CLASS Project


RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------

METHOD ResetQuickOpen( cFile ) CLASS Project
   LOCAL aEntries, n, cProject, oItem

   // IniFile Recently open projects
   aEntries := ::Application:AppIniFile:GetEntries( "Recent" )
   AEVAL( aEntries, {|c| ::Application:AppIniFile:DelEntry( "Recent", c ) } )

   IF ( n := ASCAN( aEntries, {|c| c == cFile } ) ) > 0
      ADEL( aEntries, n, .T. )
   ENDIF
   IF FILE( cFile )
      aIns( aEntries, 1, cFile, .T. )
      IF LEN( aEntries )>=20
         ASIZE( aEntries, 20 )
      ENDIF
   ENDIF

   AEVAL( aEntries, {|c| ::Application:AppIniFile:WriteString( "Recent", c, "" ) } )

   // Reset Open Dropdown menu
   WITH OBJECT ::Application:MainWindow:oButtonOpen   // Open Button
      :Menu:Destroy()
      :Menu := MenuPopup( :Parent )
      FOR EACH cProject IN aEntries

          oItem := :AddMenuItem( cProject )

          //nId := VAL( LEFT( ALLTRIM( STR( oItem:hMenu ) ), 4 ) )
          //DO WHILE oItem:Menu:GetItem( nId ) <> NIL
          //   nId++
          //ENDDO
          //oItem:Id := nId
          oItem:Action := {|o| ::Application:Project:Open( o:Caption ) }
      NEXT
   END
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------

CLASS MsgBoxEx INHERIT Dialog
   DATA BodyText   EXPORTED INIT ""
   DATA nIcon      EXPORTED
   METHOD Init( oParent ) CONSTRUCTOR
   METHOD OnCommand()
   METHOD OnInitDialog()
ENDCLASS

METHOD Init( oParent, cText, cCaption, nIcon ) CLASS MsgBoxEx
   Super:Init( oParent )
   ::nIcon    := nIcon
   ::Template := "MSGBOXEX"
   ::Modal    := .T.
   ::Caption  := cCaption
   ::Center   := .T.
   ::BodyText := cText
   ::Create()
RETURN Self

METHOD OnInitDialog() CLASS MsgBoxEx
   ::Label1:SetStyle( SS_ICON, .T. )
   ::Label1:SendMessage( STM_SETICON, LoadIcon(, IDI_QUESTION ) )
   ::Label2:Caption := ::BodyText
RETURN 0

METHOD OnCommand() CLASS MsgBoxEx
   ::Close( ::wParam )
RETURN 0

FUNCTION HB_ResetWith()
RETURN NIL
