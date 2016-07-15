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
//#include "xEditConstants.ch"

//#define VXH

#define VXHD_Version "2.0"

ANNOUNCE GTSys

PROCEDURE Main( ... )
  LOCAL hParam := parse_cmd_line( hb_aParams() )

  Debugger():Init( hParam )
RETURN


PROCEDURE OnShowEditors()
RETURN


CLASS Debugger FROM Application

  DATA Props            EXPORTED INIT {=>}

  DATA DefaultFolder INIT ""
  DATA Project
  DATA EditorPage
  DATA FileMenu
  DATA CloseMenu
  DATA DebuggerPanel
  DATA SourceEditor
  DATA oDebugger
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
  DATA FileExplorer

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

  //::System:CurrentScheme:AutoScheme := .F.
  //::System:CurrentScheme:ColorScheme := "Classic"
  //::System:CurrentScheme:Load()

  ::Project := Project( NIL )

  ::MainWindow := MainWindow():Init()

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

            ::oDebugger:Start( , Iif( !Empty( ::cPort ), Val( ::cPort ), ) )
         ENDIF
      ENDIF
    ELSE
      ::oDebugger:Start( ::cHost, Iif( !Empty( ::cPort ), Val( ::cPort ), ) )
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
   METHOD OnClose()  INLINE IIF( ::Application:Project == NIL .OR. ::Application:Project:Close(), (Super:OnClose(), ::SaveLayout()), 0 )
   DATA oButtonOpenSrc
   DATA oButtonOpen
ENDCLASS

METHOD Init() CLASS MainWindow
   //LOCAL aEntries, cProject, oItem

   ::Super:Init()

   ::Caption := "Visual xHarbour Debugger"
   ::Left    := 0
   ::Name    := "Main"
   ::Top     := 0
   ::Width   := 800
   ::Height  := 600
   ::Icon    := ::Application:LoadIcon( "VXHDICON" )

   ::RestoreLayout()

   ::Create()

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

   WITH OBJECT ::Application:Props[ "ToolStripContainer" ] := ToolStripContainer( Self )
      :Create()
      WITH OBJECT ToolStrip( :this )
         :Caption      := "Open"
         :Row          := 1
         :Height       := 58
         :ShowChevron  := .F.
         :ShowGrip     := .F.
         :ImageList    := ImageList( :this, 32, 32 ):Create()
         :ImageList:AddIcon( "ICO_OPEN" )
         :ImageList:AddIcon( "ICO_CLOSE" )
         :ImageList:AddIcon( "ICO_SAVE" )
         :ImageList:AddIcon( "ICO_SEARC" )
         :ImageList:AddIcon( "ICO_GOTO" )
         :Create()

         WITH OBJECT ::Application:Props[ "OpenBttn" ] := ToolStripButton( :this )
            :ImageIndex       := 1
            :Text             := "Open"
            :ToolTip:Text     := "Open Application"
            :ImageAlign       := DT_CENTER
            :Action           := {||::Application:Project:Open() }
            :DropDown         := 2
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC( "O" )
            :Create()
            ::Application:Project:ResetQuickOpen()
         END

         WITH OBJECT ::Application:Props[ "CloseBttn" ] := ToolStripButton( :this )
            :ImageIndex   := 2
            :ImageAlign   := DT_CENTER
            :Text         := "Close"
            :ToolTip:Text := "Close Application"
            :Action       := {||::Application:Project:Close() }
            :Create()
         END

         WITH OBJECT ::Application:Props[ "SaveBttn" ] := ToolStripButton( :this )
            :ImageIndex       := 3
            :Text             := "Save"
            :ImageAlign       := DT_CENTER
            :Action           := {||::Application:Project:Save() }
            :Enabled          := .F.
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC( "S" )
            :Create()
         END

         WITH OBJECT ::Application:Props[ "FindBttn" ] := ToolStripButton( :this )
            :ImageIndex       := 4
            :Text             := "Search"
            :ToolTip:Text     := "Search in Source"
            :ImageAlign       := DT_CENTER
            :Action           := {||::Application:Project:Find() }
//            :Enabled          := .F.
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC( "F" )
            :Create()
         END
         WITH OBJECT ::Application:Props[ "ReplBttn" ] := ToolStripButton( :this )
            :ImageIndex       := 0
            :Text             := ""
//            :ImageAlign       := DT_CENTER
            :Action           := {||::Application:Project:Replace() }
            :Enabled          := .F.
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC( "H" )
            :Create()
         END
         WITH OBJECT ::Application:Props[ "SearchGoto" ] := ToolStripButton( :this )
            :ImageIndex       := 5
            :Text             := "Go to"
            :ToolTip:Text     := "Junp to Line"
            :ImageAlign       := DT_CENTER
//            :Enabled          := .F.
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC("G")
            :Action           := {|| ::Application:SourceEditor:GotoDialog() }
            :Create()
         END
         WITH OBJECT ::Application:Props[ "SourceBttn" ] := ToolStripButton( :this )
            :ImageIndex       := 1
            :Text             := "Source"
            :ToolTip:Text     := "Open Source File"
            :ImageAlign       := DT_CENTER
            :Action           := {||::Application:Project:OpenSource() }
            :DropDown         := 2
            :ShortCutKey:Ctrl := .T.
            :ShortCutKey:Key  := ASC( "S" )
            :Create()
         END
      END
   END

   // Panel EXCLUSIVE for vxh-debugger
   WITH OBJECT ::Application:DebuggerPanel := Panel( Self )
      :Caption        := "Debugger"
      :Width          := 680
      :Height         := 200
      :Border         := .T.
      :Dock:Margin    := 2
      :Dock:Left      := :Parent
      :Dock:Bottom    := ::StatusBar1
      :Dock:Right     := :Parent

      Splitter( :Parent )
      ::Splitter1:Owner := :this
      ::Splitter1:Create()

      :Create()
      :Hide()
   END

   WITH OBJECT ::Application:FileExplorer := FileExplorer( Self )
      :Text          := "File Explorer"
      :Width         := 150
      :Border        := .T.
      :LinesAtRoot   := .F.
      :HasLines      := .F.
      :ShowSelAlways := .T.
      :Dock:Margin   := 2
      :StaticEdge    := .F.
      :ClientEdge    := .F.
      :Dock:Right    := :Parent
      :Dock:Top      := ::Application:Props[ "ToolStripContainer" ]
      :Dock:Bottom   := ::Application:DebuggerPanel
      :Create()
   END

   WITH OBJECT ::Application:SourceEditor := SourceEditor( Self )
      :Border      := .T.
      :Dock:Margin := 2
      :Dock:Left   := :Parent
      :Dock:Top    := ::Application:Props[ "ToolStripContainer" ]
      :Dock:Right  := ::Application:FileExplorer
      :Dock:Bottom := ::Application:DebuggerPanel
      :Create()
   END
   ::Show()

RETURN Self

CLASS FileExplorer INHERIT TreeView
   METHOD SetFile()
ENDCLASS

METHOD SetFile( oObj ) CLASS FileExplorer
   oObj:TreeItem := ::AddItem( oObj:FileName, 1 )
   oObj:TreeItem:Cargo := oObj
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

   DATA FindDialog       EXPORTED
   DATA ReplaceDialog    EXPORTED

   DATA __cFindText      EXPORTED INIT ""

   ASSIGN Modified(lMod) INLINE ::SetCaption( lMod )
   ACCESS Modified       INLINE ::lModified

   METHOD Init() CONSTRUCTOR
   METHOD SetCaption( lMod )
   METHOD Save()
   METHOD Close()
   METHOD Open()
   METHOD Find()
   METHOD Replace()

   METHOD ResetQuickOpen()
   METHOD EditReset()

   METHOD OpenSource( cSource, lNoDialog )
   METHOD CloseSource()
   METHOD SaveSource()
   METHOD SaveSourceAs()
   METHOD OpenFile()
   METHOD SourceTabChanged()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init() CLASS Project
RETURN Self

METHOD Find() CLASS Project
   LOCAL cSel
   IF ::Application:SourceEditor:IsWindowVisible() .AND. ::Application:SourceEditor:Source != NIL
      IF ::ReplaceDialog != NIL
         ::ReplaceDialog:Close()
      ENDIF
      ::Application:SourceEditor:SetFocus()

      ::FindDialog := FindTextDialog( ::Application:SourceEditor )
      ::FindDialog:Owner := ::Application:SourceEditor

      cSel := ::Application:SourceEditor:Source:GetSelText()
      IF ! EMPTY( cSel )
         ::__cFindText := cSel
      ENDIF
      ::FindDialog:Show(, ::__cFindText )
   ENDIF
RETURN 0

METHOD Replace() CLASS Project
   IF ::Application:SourceEditor:IsWindowVisible() .AND. ::Application:SourceEditor:Source != NIL
      IF ::FindDialog != NIL
         ::FindDialog:Close()
      ENDIF
      IF ::ReplaceDialog != NIL .AND. ::ReplaceDialog:IsWindowVisible()
         ::ReplaceDialog:FindWhat:Caption := ::Application:SourceEditor:Source:GetSelText()
         ::ReplaceDialog:FindWhat:SetFocus()
       ELSE
         ::ReplaceDialog := FindReplace( ::Application:SourceEditor )
      ENDIF
   ENDIF
RETURN 0

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
   ENDIF

   ::Application:MainWindow:Caption := cCaption

   ::Application:Props[ "SaveBttn" ]:Enabled := lMod

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

METHOD SourceTabChanged( nCur ) CLASS Project
   ::Application:SourceEditor:aDocs[ nCur ]:Select()
   ::Application:SourceEditor:Text := ::Application:SourceEditor:aDocs[ nCur ]:File
   ::Application:SourceEditor:aDocs[ nCur ]:TreeItem:Select()
RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Close() CLASS Project

   LOCAL nRes, n, oMsg, lRem

   IF ! ::Application:oDebugger:lStopped
      ::Application:oDebugger:Stop()
   ENDIF

   lRem := .F.
   FOR n := 1 TO ::Application:SourceEditor:DocCount
       IF ::Application:SourceEditor:aDocs[n]:Modified
          IF nRes == NIL
             oMsg := MsgBoxEx( ::Application:MainWindow, "Save changes to "+::Application:SourceEditor:aDocs[n]:cPath+::Application:SourceEditor:aDocs[n]:cFile+" before closing?", "Source File", IDI_QUESTION )
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
                  IF EMPTY( ::Application:SourceEditor:aDocs[n]:cFile )
                     ::SaveSourceAs( ::Application:SourceEditor:aDocs[n] )
                   ELSE
                     ::Application:SourceEditor:aDocs[n]:Save()
                  ENDIF
                  EXIT

             CASE IDCANCEL
                  RETURN .F.
          END
          IF !lRem
             nRes := NIL
          ENDIF
       ENDIF

       ::Application:SourceEditor:aDocs[n]:Modified := .F.
       ::Application:SourceEditor:aDocs[n]:Close()
       n--
   NEXT

   ::Application:SourceEditor:Clean()
   ::Application:MainWindow:Caption := "Visual Debugger"

   ::CopyBuffer := {}
   ::PasteOn := .F.

   ::ProjectFile       := NIL
   ::Modified          := .F.
   ::CurrentProgram    := NIL
   ::Properties        := NIL
   ::Modified          := .F.

   ::Application:SourceEditor:Text := ""
   ::Application:DebuggerPanel:Hide()

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
      ofn:hwndOwner      := ::Application:MainWindow:hWnd
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
         n := ASCAN( ::Application:SourceEditor:aDocs, {|o| o == oEditor} )
         IF ( x := ASCAN( ::Application:SourceEditor:aDocs, {|o| o:cPath+o:cFile == cFile } ) ) > 0 .AND. x != n
            MessageBox( 0, cFile +" cannot be overwritten because it's being edited, please select another path and / or name", "Save As", MB_ICONEXCLAMATION )
            ::Application:Yield()
            LOOP
         ENDIF
         oEditor:Save( cFile )
         IF lSetTabName
            oEditor:TreeItem:Text := oEditor:FileName
            IF ::Application:SourceEditor:Source == oEditor
               ::Application:SourceEditor:Text := oEditor:File
            ENDIF
         ENDIF
      ENDIF
      EXIT
   ENDDO
RETURN Self

//-------------------------------------------------------------------------------------------------------
METHOD CloseSource() CLASS Project

   LOCAL nRes, n := ::Application:SourceTabs:GetCurSel()

   nRes := IDNO
   IF ::Application:SourceEditor:Source:Modified
      nRes := MessageBox( 0, "Save changes before closing?", ::Application:SourceEditor:oEditor:cFile, MB_YESNOCANCEL | MB_ICONQUESTION )
      IF nRes == IDCANCEL
         RETURN Self
      ENDIF
   ENDIF

   ::Application:SourceTabs:DeleteTab( n )
   IF nRes == IDYES
      ::SaveSource()
   ENDIF

   ::Application:SourceEditor:Source:Close()

   n := MIN( n, ::Application:SourceEditor:DocCount )
   IF n > 0
      ::Application:SourceTabs:SetCurSel( n )
      ::SourceTabChanged( n )
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
      AADD( ::Application:aPath,cPath )
   ENDIF

   cPath += '\'

   //Alert( cPath + "->" + cName )

   IF ( n := ASCAN( ::Application:SourceEditor:aDocs, {|o| o:cPath == cPath .AND. o:cFile == cName } ) ) > 0
      // File is open, just re-show

//      ::Application:SourceTabs:SetCurSel( n )
      ::SourceTabChanged( n )

      IF !::Application:SourceEditor:IsWindowVisible()
         ::Application:EditorPage:Select()
      ENDIF
      ::Application:SourceEditor:SetFocus()
      RETURN Self
   ENDIF

   ::Application:SourceEditor:Show()

   oEditor := Source( ::Application:SourceEditor, cPath + cName )

   ::Application:FileExplorer:SetFile( oEditor )

   ::SourceTabChanged( ::Application:SourceEditor:DocCount )

   IF !::Application:SourceEditor:IsWindowVisible()
      ::Application:EditorPage:Select()
   ENDIF

   ::Application:SourceEditor:SetFocus()
   OnShowEditors()

RETURN Self

METHOD OpenFile( cFile ) CLASS Project
   LOCAL n, lRet := .F.
   IF ( n := ASCAN( ::Application:SourceEditor:aDocs, {|o| UPPER(o:File) == UPPER(cFile) } ) ) > 0
      ::Application:SourceEditor:aDocs[n]:TreeItem:Select()
      ::SourceTabChanged( n )
      ::Application:SourceEditor:Text := ::Application:SourceEditor:aDocs[n]:File

      IF !::Application:SourceEditor:IsWindowVisible()
         ::Application:EditorPage:Select()
      ENDIF
      ::Application:SourceEditor:SetFocus()
      lRet := .T.
   ENDIF
RETURN lRet

//-------------------------------------------------------------------------------------------------------

METHOD Open( cFile ) CLASS Project
   LOCAL oFile

   IF ! FILE( cFile )
      oFile := CFile( "" )

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
      ENDDO
      cFile := oFile:Path + "\" + oFile:Name
   ENDIF

   ::Application:MainWindow:UpdateWindow()

   ::ResetQuickOpen( cFile )

   ::Modified := .F.
   ::Application:Open( cFile + " " + ::Application:Args )

RETURN Self

//-------------------------------------------------------------------------------------------------------

METHOD Save() CLASS Project
   LOCAL n
   FOR n := 1 TO ::Application:SourceEditor:DocCount
       IF ::Application:SourceEditor:aDocs[n]:Modified
          IF EMPTY( ::Application:SourceEditor:aDocs[n]:cFile )
             ::SaveSourceAs( ::Application:SourceEditor:aDocs[n] )
           ELSE
             ::Application:SourceEditor:aDocs[n]:Save()
          ENDIF
       ENDIF
       ::Application:SourceEditor:aDocs[n]:Modified := .F.
   NEXT
   ::Modified  := .F.
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------

METHOD ResetQuickOpen( cFile ) CLASS Project
   LOCAL lMems, aEntries, n, oItem, /*oLink, x,*/ lLink := .T.

   aEntries := ::Application:IniFile:GetSectionEntries( "Recent", .T. )
   IF cFile != NIL .AND. ( n := ASCAN( aEntries, {|c| lower(c) == lower(cFile) } ) ) > 0
      ADEL( aEntries, n, .T. )
   ENDIF
   IF cFile != NIL .AND. FILE( cFile )
      AINS( aEntries, 1, cFile, .T. )
      IF LEN( aEntries )>=20
         ASIZE( aEntries, 20 )
      ENDIF
   ENDIF

   lMems := ::Application:GenerateMembers
   ::Application:GenerateMembers := .F.

   WITH OBJECT ::Application:Props[ "OpenBttn" ]   // Open Button
      FOR n := 1 TO LEN( aEntries )
          IF ! EMPTY( aEntries[n] )
             IF :Children == NIL .OR. LEN( :Children ) < n
                oItem := MenuStripItem( :this )
                oItem:Create()
              ELSE
                oItem := :Children[n]
             ENDIF
             oItem:Text   := aEntries[n]
             oItem:Action := {|o| ::Application:Project:Open( o:Text ) }
          ENDIF
      NEXT
   END

   ::Application:IniFile:Write( "Recent", aEntries )
   ::Application:GenerateMembers := lMems

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

//-------------------------------------------------------------------------------------------------------
FUNCTION xBuild_GUI_ONERROR()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
FUNCTION xBuild_GUI_SETERROR()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
FUNCTION GETLOGERRORS()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
FUNCTION POPUPEDITOR()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
FUNCTION XEDITLISTVIEW()
RETURN .T.

//-------------------------------------------------------------------------------------------------------
FUNCTION __GETPROPERCASE()
RETURN .T.
