/*
 * $Id: Editor.prg,v 1.28 2015/05/12 08:35:49 Augusto Infante Exp $
 */

static s_nSecs

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"
#include "Scintilla.ch"
#include "SciLexer.ch"
#include "fileio.ch"

#define MARGIN_SCRIPT_FOLD_INDEX  2
#define MARKER_MASK 0 //1 | (1 << 1) | (1 << 2) | (1 << 3)


#define EOL Chr(13) + Chr(10)

//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------

CLASS SourceEditor INHERIT TitleControl
   DATA pPrevSel      EXPORTED
   DATA xTabWidth     PROTECTED INIT 3
   ASSIGN TabWidth(n) INLINE ::xTabWidth := n, AEVAL( ::aDocs, {|o| o:SetTabWidth(o:Owner:TabWidth)} ), IIF( ::Source != NIL, ::Source:Select(),)
   ACCESS TabWidth    INLINE ::xTabWidth

   DATA xSource  PROTECTED

   ASSIGN Source(o) INLINE ::Parent:Text := IIF( o == NIL, "Source Code Editor", o:FileName ), IIF( ::xSource != NIL, ::xSource:SavePos(),), IIF( o != NIL, o:Select(),)
   ACCESS Source    INLINE ::xSource

   DATA aDocs        EXPORTED INIT {}
   DATA PosFind      EXPORTED INIT -1
   DATA FindInStyle  EXPORTED INIT .F.
   DATA FindStyle    EXPORTED INIT 0
   DATA InSelection  EXPORTED INIT .F.
   ACCESS DocCount       INLINE LEN( ::aDocs )

   // Compatibility with xedit debugger ----------------------------------------------------
   ACCESS oEditor    INLINE ::xSource
   ASSIGN oEditor(o) INLINE ::Source(o)
   //---------------------------------------------------------------------------------------

   DATA KeyWords1         EXPORTED INIT ""
   DATA KeyWords2         EXPORTED INIT ""
   DATA KeyWords3         EXPORTED INIT ""
   DATA KeyWords4         EXPORTED INIT ""

   DATA ColorNormalText   EXPORTED
   DATA ColorBackground   EXPORTED
   DATA ColorSelectedLine EXPORTED

   DATA ColorNumbers      EXPORTED
   DATA ColorStrings      EXPORTED
   DATA ColorComments     EXPORTED
   DATA ColorOperators    EXPORTED
   DATA ColorPreprocessor EXPORTED

   DATA ColorKeywords1    EXPORTED
   DATA ColorKeywords2    EXPORTED
   DATA ColorKeywords3    EXPORTED
   DATA ColorKeywords4    EXPORTED

   DATA FontFaceName      EXPORTED
   DATA FontSize          EXPORTED
   DATA FontBold          EXPORTED
   DATA FontItalic        EXPORTED

   DATA cFindWhat         EXPORTED INIT ""
   DATA nDirection        EXPORTED
   DATA CaretLineVisible  EXPORTED
   DATA AutoIndent        EXPORTED
   DATA TrimEOLSpaces     EXPORTED

   DATA EditMenuItems     EXPORTED
   DATA FindInFiles       EXPORTED INIT .F.


   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD GetEditor()
   METHOD StyleClearAll()                 INLINE SCI_SEND( SCI_STYLECLEARALL, 0, 0 ), Self

   METHOD StyleSetFont( cFont )           INLINE ::FontFaceName := cFont, SendEditorString( ::hWnd, SCI_STYLESETFONT, STYLE_DEFAULT, cFont ), Self
   METHOD StyleSetBold( lBold )           INLINE ::FontBold := lBold, SCI_SEND( SCI_STYLESETBOLD, STYLE_DEFAULT, lBold ), Self
   METHOD StyleSetItalic( lItalic )       INLINE ::FontItalic := lItalic, SCI_SEND( SCI_STYLESETITALIC, STYLE_DEFAULT, lItalic ), Self

   METHOD StyleGetFont( cFont )           INLINE cFont := SPACE(255), SCI_SEND( SCI_STYLEGETFONT, STYLE_DEFAULT, @cFont ), ALLTRIM(cFont)

   METHOD StyleSetSize( nSize )           INLINE ::FontSize := nSize, SCI_SEND( SCI_STYLESETSIZE, STYLE_DEFAULT, nSize ), Self
   METHOD StyleGetSize( nSize )           INLINE SCI_SEND( SCI_STYLEGETSIZE, STYLE_DEFAULT, @nSize ), nSize

   METHOD SetLexer( nLexer )              INLINE SCI_SEND( SCI_SETLEXER, nLexer, 0 )
   METHOD GetCurDoc()                     INLINE SCI_SEND( SCI_GETDOCPOINTER, 0, 0 )

   METHOD SelectDocument( pDoc )          INLINE SCI_SEND( SCI_SETDOCPOINTER, 0, pDoc )
   METHOD OnDestroy()
   METHOD OnParentNotify()

   METHOD Colorise( nStart, nEnd )        INLINE SCI_SEND( SCI_COLOURISE, nStart, nEnd )

   METHOD BookmarkAdd(nLine)              INLINE SCI_SEND( SCI_MARKERADD, nLine, MARKER_MASK )
   METHOD BookmarkGet(nLine)              INLINE SCI_SEND( SCI_MARKERGET, nLine, MARKER_MASK )
   METHOD BookmarkDel(nLine)              INLINE SCI_SEND( SCI_MARKERDELETE, nLine, MARKER_MASK )
   METHOD BookmarkDelAll()                INLINE SCI_SEND( SCI_MARKERDELETEALL, MARKER_MASK, 0 )

   METHOD ToggleBookmark()
   METHOD BookmarkNext()
   METHOD BookmarkPrev()                  INLINE SCI_SEND( SCI_MARKERPREVIOUS, ::Source:GetCurLine()-1, 1<<MARKER_MASK )

   METHOD GetTabWidth()                   INLINE SCI_SEND( SCI_GETTABWIDTH, 0, 0 )

   METHOD StyleSetFore( nStyle, nColor )  INLINE SCI_SEND( SCI_STYLESETFORE, nStyle, nColor )
   METHOD StyleSetBack( nStyle, nColor )  INLINE SCI_SEND( SCI_STYLESETBACK, nStyle, nColor )
   METHOD OnTimer()
   METHOD CheckChanges()
   METHOD InvertCase()
   METHOD UpperCase()
   METHOD LowerCase()
   METHOD Capitalize()
   METHOD GotoDialog()                    INLINE GotoDialog( Self )
   METHOD OnSetFocus()                    INLINE IIF( ::Application:MainForm:HasMessage("EnableSearchMenu"),::Application:MainForm:EnableSearchMenu( .T. ),), SCI_SEND( SCI_SETFOCUS, 1, 0 ), NIL
   METHOD OnFindNext()
   METHOD OnReplace()
   METHOD OnReplaceAll()

   METHOD GetSearchFlags()
   METHOD FindNext()
   METHOD EnsureRangeVisible()
   METHOD OnLButtonUp()
   METHOD OnKeyDown()
   METHOD InitLexer()
   METHOD AutoIndentText()
   METHOD ToggleRectSel()
   METHOD GetWithObject()
   METHOD SetColors()
   METHOD Clean()
ENDCLASS

METHOD BookmarkNext() CLASS SourceEditor
   LOCAL nPos, nLine := ::Source:GetCurLine()
   WHILE .T.
      nPos := SCI_SEND( SCI_MARKERNEXT, nLine+1, 1<<MARKER_MASK )

      IF ::Application:EditorProps:WrapSearch == 1 .AND. nPos == -1 .AND. nLine > 0
         nLine := 0
         LOOP
      ENDIF
      EXIT
   ENDDO
RETURN nPos

METHOD Clean() CLASS SourceEditor
   SCI_SEND( SCI_SETTEXT, 0, "" )
RETURN NIL

METHOD GetEditor( cFile ) CLASS SourceEditor
   LOCAL n := ASCAN( ::aDocs, {|o| o:File == cFile} )
   IF n > 0
      RETURN ::aDocs[n]
   ENDIF
RETURN NIL

METHOD ToggleBookmark() CLASS SourceEditor
   LOCAL nLineStart, nLineEnd, n
   nLineStart := ::Source:LineFromPosition( ::Source:GetSelectionStart() )
   nLineEnd   := ::Source:LineFromPosition( ::Source:GetSelectionEnd() )
   FOR n := nLineStart TO nLineEnd
       IF ::BookmarkGet(n) > 0
          ::BookmarkDel(n)
        ELSE
          ::BookmarkAdd(n)
       ENDIF
   NEXT
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS SourceEditor
   LOCAL cSyntax := ::Application:Path + "\\vxh.syn"
   ::__xCtrlName := "SourceEditor"
   ::ClsName := "Scintilla"
   ::Super:Init( oParent )
   ::Style := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN
   ::EventHandler[ "OnFindNext" ]   := "OnFindNext"
   ::EventHandler[ "OnReplace" ]    := "OnReplace"
   ::EventHandler[ "OnReplaceAll" ] := "OnReplaceAll"
   ::EventHandler[ "OnFindClose" ]  := "OnFindClose"

   IF FILE( cSyntax )
      ::Keywords1 := STRTRAN( GetPrivateProfileSection( "Keywords1", cSyntax ), CHR(0), " " )
      ::Keywords2 := STRTRAN( GetPrivateProfileSection( "Keywords2", cSyntax ), CHR(0), " " )
      ::Keywords3 := STRTRAN( GetPrivateProfileSection( "Keywords3", cSyntax ), CHR(0), " " )
      ::Keywords4 := STRTRAN( GetPrivateProfileSection( "Keywords4", cSyntax ), CHR(0), " " )
   ENDIF

   ::ColorNormalText   := ::Application:IniFile:ReadColor( "Colors", "NormalText",   ::System:Color:Black          )
   ::ColorBackground   := ::Application:IniFile:ReadColor( "Colors", "BackGround",   ::System:Color:White          )
   ::ColorSelectedLine := ::Application:IniFile:ReadColor( "Colors", "SelectedLine", RGB( 240, 240, 240 )          )

   ::ColorNumbers      := ::Application:IniFile:ReadColor( "Colors", "Numbers",      ::System:Color:Green          )
   ::ColorStrings      := ::Application:IniFile:ReadColor( "Colors", "Strings",      ::System:Color:Teal           )
   ::ColorComments     := ::Application:IniFile:ReadColor( "Colors", "Comments",     ::System:Color:Gray           )
   ::ColorOperators    := ::Application:IniFile:ReadColor( "Colors", "Operators",    ::System:Color:DarkOliveGreen )
   ::ColorPreprocessor := ::Application:IniFile:ReadColor( "Colors", "Preprocessor", ::System:Color:Tomato         )

   ::ColorKeywords1    := ::Application:IniFile:ReadColor( "Colors", "Keywords1",    ::System:Color:Maroon         )
   ::ColorKeywords2    := ::Application:IniFile:ReadColor( "Colors", "Keywords2",    ::System:Color:DarkCyan       )
   ::ColorKeywords3    := ::Application:IniFile:ReadColor( "Colors", "Keywords3",    ::System:Color:SteelBlue      )
   ::ColorKeywords4    := ::Application:IniFile:ReadColor( "Colors", "Keywords4",    ::System:Color:DarkSlateGray  )

   ::FontFaceName      := ::Application:IniFile:ReadString( "Font", "FaceName", "FixedSys" )
   ::FontSize          := ::Application:IniFile:ReadInteger( "Font", "Size", 10 )
   ::FontBold          := ::Application:IniFile:ReadInteger( "Font", "Bold", 0 ) == 1
   ::FontItalic        := ::Application:IniFile:ReadInteger( "Font", "Italic", 0 ) == 1

   ::CaretLineVisible  := ::Application:IniFile:ReadInteger( "Settings", "CaretLineVisible", 1 )
   ::AutoIndent        := ::Application:IniFile:ReadInteger( "Settings", "AutoIndent", 1 )
   ::TrimEOLSpaces     := ::Application:IniFile:ReadInteger( "Settings", "TrimEOLSpaces", 0 )


   //IF ( n := ::Application:Props:FontList:FindString(, ::FontFaceName ) ) > 0
   //   ::Application:Props:FontList:SetCurSel(n)
   //ENDIF
   //IF ( n := ::Application:Props:FontSize:FindString(, xStr(::FontSize) ) ) > 0
   //   ::Application:Props:FontSize:SetCurSel(n)
   //ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS SourceEditor
   ::Super:Create()

   SCI_Init( ::hWnd )

//   SCI_SEND( SCI_SETLEXER, SCLEX_CONTAINER )

   SCI_SEND( SCI_SETLEXER, SCLEX_FLAGSHIP, 0 )
   SCI_SEND( SCI_SETSTYLEBITS, SCI_SEND( SCI_GETSTYLEBITSNEEDED ) )

   ::StyleSetBack( STYLE_DEFAULT, ::ColorBackground )
   ::StyleSetFore( STYLE_DEFAULT, ::ColorNormalText )

   ::StyleSetFont( ::FontFaceName )
   ::StyleSetSize( ::FontSize )
   ::StyleSetBold( ::FontBold )
   ::StyleSetItalic( ::FontItalic )

   ::StyleClearAll()


   ::InitLexer()
   ::xTabWidth := ::Application:IniFile:ReadInteger( "Settings", "TabSpacing", ::xTabWidth )

   ::EditMenuItems := {=>}
   HSetCaseMatch( ::EditMenuItems, .F. )

   WITH OBJECT ( ::ContextMenu := ContextStrip( Self ) )
      :Create()
      WITH OBJECT ( ::EditMenuItems[ "Undo" ]   := MENUSTRIPITEM( :this ) )
         :Text         := "&Undo"
         :ShortCutText := "Ctrl+Z"
         :Action       := {|| ::Source:Undo() }
         :Create()
      END
      WITH OBJECT ( ::EditMenuItems[ "Cut" ]    := MENUSTRIPITEM( :this ) )
         :Text         := "C&ut"
         :ShortCutText := "Ctrl+X"
         :Begingroup   := .T.
         :Action       := {|| ::Source:Cut() }
         :Create()
      END
      WITH OBJECT ( ::EditMenuItems[ "Copy" ]   := MENUSTRIPITEM( :this ) )
         :Text         := "&Copy"
         :ShortCutText := "Ctrl+C"
         :Action       := {|| ::Source:Copy() }
         :Create()
      END
      WITH OBJECT ( ::EditMenuItems[ "Paste" ]  := MENUSTRIPITEM( :this ) )
         :Text         := "&Paste"
         :ShortCutText := "Ctrl+V"
         :Action       := {|| ::Source:Paste() }
         :Create()
      END
      WITH OBJECT ( ::EditMenuItems[ "Delete" ] := MENUSTRIPITEM( :this ) )
         :Text         := "&Delete"
         :ShortCutText := "Del"
         :Action       := {|| ::Source:Delete() }
         :Create()
      END
      WITH OBJECT ( ::EditMenuItems[ "SelAll" ] := MENUSTRIPITEM( :this ) )
         :Text         := "Select &All"
         :ShortCutText := "Ctrl+A"
         :Action       := {|| ::Source:SelectAll() }
         :Begingroup   := .T.
         :Create()
      END
   END
RETURN Self

METHOD OnTimer( nId ) CLASS SourceEditor
   ::CallWindowProc()
   IF nId == 1001
      ::KillTimer( nId )
      ::CheckChanges()
      ::SetTimer( nId, 2000 )
   ENDIF
RETURN 0

METHOD CheckChanges() CLASS SourceEditor
   IF ::Source != NIL .AND. ! EMPTY( ::Source:FileTime ) .AND. ::Source:FileTime != FileTime( ::Source:File )
      IF ::MessageBox( "Another application has updated file " + ::Source:File + ". Reload it?", "Visual xHarbour", MB_YESNO | MB_ICONQUESTION ) == IDYES
         ::Source:Reload()
      ENDIF
   ENDIF
RETURN Self


//------------------------------------------------------------------------------------------------------------------------------------
METHOD SetColors() CLASS SourceEditor
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_KEYWORD,  ::ColorKeywords1 )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_KEYWORD2, ::ColorKeywords2 )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_KEYWORD3, ::ColorKeywords3 )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_KEYWORD4, ::ColorKeywords4 )

   SCI_SEND( SCI_SETCARETLINEBACK, ::ColorSelectedLine )
   SCI_SEND( SCI_SETCARETLINEVISIBLE, ::CaretLineVisible )

   SCI_SEND( SCI_STYLESETFORE, SCE_FS_COMMENT,        ::ColorComments  )

   SCI_SEND( SCI_STYLESETFORE, SCE_FS_COMMENT,        ::ColorComments  )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_COMMENTLINE,    ::ColorComments  )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_COMMENTDOC,     ::ColorComments  )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_COMMENTLINEDOC, ::ColorComments  )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_OPERATOR,       ::ColorOperators )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_STRING,         ::ColorStrings   )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_NUMBER,         ::ColorNumbers   )
   SCI_SEND( SCI_STYLESETFORE, SCE_FS_PREPROCESSOR,   ::ColorPreprocessor )

   SCI_SEND( SCI_SETFOLDMARGINCOLOUR, 1, ::ColorBackground )
   SCI_SEND( SCI_SETFOLDMARGINHICOLOUR, 1, ::ColorBackground )

   SCI_SEND( SCI_SETCARETFORE, ::ColorNormalText )

   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDER,        ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPEN,    ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPENMID, ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERSUB,     ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERMIDTAIL, ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERTAIL,    ::ColorBackground )
   SCI_SEND( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEREND,     ::ColorBackground )

   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDER,        ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPEN,    ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPENMID, ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERSUB,     ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERMIDTAIL, ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERTAIL,    ::ColorNormalText )
   SCI_SEND( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEREND,     ::ColorNormalText )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD InitLexer() CLASS SourceEditor
   //LOCAL nKey := 9 + ( SCMOD_CTRL << 16 )
   //SCI_SEND( SCI_ASSIGNCMDKEY, nKey, SCN_CHARADDED )

   SCI_SEND( SCI_SETPROPERTY, "fold", "1" )
   SCI_SEND( SCI_SETPROPERTY, "fold.compact", "0" )
   SCI_SEND( SCI_SETPROPERTY, "fold.comment", "1" )
   SCI_SEND( SCI_SETPROPERTY, "fold.preprocessor", "0" )
   SCI_SEND( SCI_SETPROPERTY, "fold.directive", "0" )
   SCI_SEND( SCI_SETPROPERTY, "strip.trailing.spaces", "1" )

   SCI_SEND( SCI_SETKEYWORDS, 0, ::Keywords1 )
   SCI_SEND( SCI_SETKEYWORDS, 1, ::Keywords2 )
   SCI_SEND( SCI_SETKEYWORDS, 2, ::Keywords3 )
   SCI_SEND( SCI_SETKEYWORDS, 3, ::Keywords4 )

   SCI_SEND( SCI_SETCARETPERIOD, 500, 0 )

   SCI_SEND( SCI_SETCARETWIDTH, 2 )
   SCI_SEND( SCI_SETCARETSTYLE, 1 )

   SCI_SEND( SCI_SETMARGINWIDTHN, 1, 15 )
   SCI_SEND( SCI_SETMARGINTYPEN,  1, SC_MARGIN_BACK )

   // Bookmark markers
   SCI_SEND( SCI_MARKERDEFINE,  MARKER_MASK, SC_MARK_ARROW )
   SCI_SEND( SCI_MARKERSETFORE, MARKER_MASK, RGB( 255, 0, 0 ) )
   SCI_SEND( SCI_MARKERSETBACK, MARKER_MASK, RGB( 255, 0, 0 ) )

   SCI_SEND( SCI_SETMARGINTYPEN,  MARGIN_SCRIPT_FOLD_INDEX, SC_MARGIN_SYMBOL )
   SCI_SEND( SCI_SETMARGINWIDTHN, MARGIN_SCRIPT_FOLD_INDEX, 15 )
   SCI_SEND( SCI_SETMARGINMASKN,  MARGIN_SCRIPT_FOLD_INDEX, SC_MASK_FOLDERS )
   SCI_SEND( SCI_SETMARGINSENSITIVEN, MARGIN_SCRIPT_FOLD_INDEX, 1 )

   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPEN,    SC_MARK_MINUS )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDER,        SC_MARK_PLUS  )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERSUB,     SC_MARK_EMPTY )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERTAIL,    SC_MARK_EMPTY )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEREND,     SC_MARK_EMPTY )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY )
   SCI_SEND( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY )
   SCI_SEND( SCI_USEPOPUP, 0 )

   SCI_SEND( SCI_SETFOLDFLAGS, 16, 0)

   SCI_SEND( SCI_CLEARREGISTEREDIMAGES, 0, 0 )

   SciRegisterPropertyImage( ::hWnd, 8 )
   SciRegisterMethodImage( ::hWnd, 7 )
   SciRegisterEventImage( ::hWnd, 6 )

   SCI_SEND( SCI_SETMODEVENTMASK, SC_MOD_CHANGEFOLD | SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT | SC_PERFORMED_UNDO | SC_PERFORMED_REDO | SC_MOD_CHANGEMARKER )
   ::SetColors()
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS SourceEditor
   ::Application:Project:EditReset()
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD InvertCase() CLASS SourceEditor
   LOCAL cSel, cInv, cChar, nStartPos, nEndPos, cBuffer
   IF ::Source:GetSelLen() > 0

      nStartPos := ::Source:GetSelectionStart()
      nEndPos   := ::Source:GetSelectionEnd()
      cSel      := ::Source:GetSelText()
      cBuffer   := SPACE( SCI_SEND( SCI_GETSELTEXT, 0, 0 ) )
      SCI_SEND( SCI_GETSELTEXT, 0, cBuffer )
      cBuffer := Left(cBuffer,LEN(cBuffer)-1)

      cInv := ""
      FOR EACH cChar IN cSel
          cInv += IIF( IsLower(cChar), UPPER(cChar), lower(cChar) )
      NEXT
      ::Source:ReplaceSel( cInv )
      ::Source:SetSelection( nStartPos, nEndPos )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD UpperCase() CLASS SourceEditor
   LOCAL cInv, nStartPos, nEndPos
   IF ::Source:GetSelLen() > 0
      nStartPos := ::Source:GetSelectionStart()
      nEndPos   := ::Source:GetSelectionEnd()
      cInv := UPPER( ::Source:GetSelText() )
      ::Source:ReplaceSel( cInv )
      ::Source:SetSelection( nStartPos, nEndPos )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD LowerCase() CLASS SourceEditor
   LOCAL cInv, nStartPos, nEndPos
   IF ::Source:GetSelLen() > 0
      nStartPos := ::Source:GetSelectionStart()
      nEndPos   := ::Source:GetSelectionEnd()
      cInv := LOWER( ::Source:GetSelText() )
      ::Source:ReplaceSel( cInv )
      ::Source:SetSelection( nStartPos, nEndPos )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Capitalize() CLASS SourceEditor
   LOCAL cSel, nStartPos, nEndPos
   IF ::Source:GetSelLen() > 0
      nStartPos := ::Source:GetSelectionStart()
      nEndPos   := ::Source:GetSelectionEnd()
      cSel      := ::Source:GetSelText()
      ::Source:ReplaceSel( UPPER(cSel[1])+LOWER(SUBSTR(cSel,2)) )
      ::Source:SetSelection( nStartPos, nEndPos )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD ToggleRectSel() CLASS SourceEditor
   SCI_SEND( SCI_SETSELECTIONMODE, SC_SEL_RECTANGLE, 0 )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnKeyDown( nKey ) CLASS SourceEditor
   LOCAL nNext, nLast, lMarkPrev

   DEFAULT s_nSecs TO 0
   IF nKey == VK_F3
      IF ::Source:GetSelLen() > 0
         ::cFindWhat := ::Source:GetSelText()
      ENDIF
      IF ! EMPTY( ::cFindWhat )
         IF ! ::FindNext( ::cFindWhat, CheckBit( GetKeyState( VK_SHIFT ), 32768 ) )
            ::MessageBox( "Cannot find literal text: " + ::cFindWhat, "Visual xHarbour", MB_ICONEXCLAMATION )
         ENDIF
       ELSE
         ::Application:Project:Find()
      ENDIF

    ELSEIF nKey == VK_TAB .AND. IsKeyDown( VK_CONTROL )

      nNext := 0
      IF ( Seconds() - s_nSecs ) > 1.5
         nNext := ASCAN( ::aDocs, {|o|o:pSource==::pPrevSel} )
      ENDIF

      IF nNext == 0
         nLast := ASCAN( ::aDocs, {|o|o:pSource==::Source:pSource} )
         nNext := nLast + IIF( IsKeyDown( VK_SHIFT ), -1, 1 )
         IF nNext > LEN( ::aDocs )
            nNext := 1
          ELSEIF nNext <= 0
            nNext := LEN( ::aDocs )
         ENDIF
         lMarkPrev := .F.
      ENDIF
      ::Application:Project:SourceTabChanged( nNext, .F., lMarkPrev )
      s_nSecs := Seconds()

   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS SourceEditor
   LOCAL scn, nPos, n, cObj, cText, nLine, nChar, nPosStart, nPosEnd, oObj, aObj, aProperties, aProp, cList, aMethods, Topic, Event, aList//, cFind
   LOCAL nWrap, nVisLine
   (nwParam, nlParam)
   IF ::Source != NIL
      DO CASE
//         CASE hdr:code == SCN_STYLENEEDED
//              scn := (struct SCNOTIFICATION*) nlParam
//              SCI_SEND( SCI_STARTSTYLING, 0, 0x1f )
//              SCI_SEND( SCI_SETSTYLING, 50, 11 )

         CASE hdr:code == SCN_UPDATEUI
              scn := (struct SCNOTIFICATION*) nlParam
              n := ::Source:GetCurrentPos()
              ::Application:SetEditorPos( ::Source:LineFromPosition(n)+1, ::Source:GetColumn(n)+1 )

         CASE hdr:code == SCN_MODIFIED
              scn := (struct SCNOTIFICATION*) nlParam
              IF ( scn:modificationType & SC_MOD_CHANGEFOLD ) == 0
                 IF ! ::Source:FirstOpen
                    IF ! ::Source:Modified
                       ::Source:Modified := .T.
                       n := aScan( ::aDocs, ::Source,,, .T. )
                       IF ::Source:TreeItem != NIL
                          cText := ::Source:TreeItem:Text
                          IF cText != NIL
                             cText := ALLTRIM( STRTRAN( cText, "*" ) )
                             ::Source:TreeItem:Text := cText + " *"
                          ENDIF
                          ::Parent:Text := ::Source:TreeItem:Text
                       ENDIF
                       IF ! ::Application:Project:Modified
                          ::Application:Project:Modified := .T.
                       ENDIF
                    ENDIF
                 ENDIF
                 IF __ObjHasMsg( ::Application:Project, "SetEditMenuItems" )
                    ::Application:Project:SetEditMenuItems()
                 ENDIF
              ENDIF

         CASE hdr:code == SCN_MARGINCLICK
              scn := (struct SCNOTIFICATION*) nlParam
              IF scn:margin == MARGIN_SCRIPT_FOLD_INDEX
                 nLine := ::Source:LineFromPosition( scn:position )
                 SCI_SEND( SCI_TOGGLEFOLD, nLine, 0 )
                 SCI_SEND( SCI_ENSUREVISIBLEENFORCEPOLICY, nLine )
                 SCI_SEND( SCI_GOTOLINE, nLine )
              ENDIF

         CASE hdr:code == SCN_CHARADDED
              IF ::System:__ToolStripFlags[ "s_CurrentObject" ] != NIL
                 ::Application:Props:MainMenu:OnSysKeyDown( VK_MENU )
              ENDIF

              scn := (struct SCNOTIFICATION*) nlParam
              IF scn:ch == 13 // indent
                 IF ::AutoIndent == 1
                    ::AutoIndentText()
                 ENDIF

              ELSEIF scn:ch == 58
                 nPosEnd   := ::Source:GetCurrentPos()-1
                 nPosStart := ::Source:PositionFromLine( ::Source:LineFromPosition( nPosEnd ) )
                 cObj := ""
                 FOR n := nPosEnd TO nPosStart STEP -1
                     nChar := ::Source:GetCharAt(n)
                     IF nChar == 32
                        EXIT
                     ENDIF
                     cObj := CHR(nChar) + cObj
                     IF ::Source:GetColumn(n)==0
                        EXIT
                     ENDIF
                 NEXT
                 IF cObj == ":"
                    nWrap := ::Application:EditorProps:WrapSearch
                    ::Application:EditorProps:WrapSearch := 0
                    nPos     := SCI_SEND( SCI_GETCURRENTPOS, 0, 0 )
                    nVisLine := SCI_SEND( SCI_GETFIRSTVISIBLELINE, 0, 0 )

                    cObj := ::GetWithObject()

                    ::Application:EditorProps:WrapSearch := nWrap

                    SCI_SEND( SCI_GOTOPOS, nPos, 0 )
                    SCI_SEND( SCI_SETFIRSTVISIBLELINE, nVisLine, 0 )
                 ENDIF
                 IF LEN( cObj ) >= 2
                    IF LEFT(cObj,2) == "::"
                       IF LEN(cObj) > 2
                          cObj := "WinForm:"+SUBSTR(cObj,3)
                        ELSE
                          cObj := "WinForm"
                       ENDIF
                    ENDIF
                    IF cObj[-1] == ":"
                       cObj := LEFT( cObj, LEN(cObj)-1 )
                    ENDIF
                    aObj := hb_aTokens( cObj, ":" )
                    IF aObj[1] == "WinForm"
                       oObj := ::Source:Form
                     ELSE
                       TRY
                          oObj := &(aObj[1])
                       CATCH
                       END
                    ENDIF
                    FOR n := 2 TO LEN( aObj )
                        TRY
                           oObj := oObj:&(aObj[n])
                        CATCH
                        END
                    NEXT

                    IF VALTYPE(oObj) == "O"
                       aList := {}

                       IF __ObjHasMsg( oObj, "__hObjects" ) .AND. oObj:__hObjects != NIL
                          HEval( oObj:__hObjects, {|k| AADD( aList, k + "?8" ) } )
                       ENDIF

                       IF __ObjHasMsg( oObj, "Events" ) .AND. oObj:Events != NIL
                          FOR EACH Topic IN oObj:Events
                              FOR EACH Event IN Topic[2]
                                  AADD( aList, Event[1]+"?6" )
                              NEXT
                          NEXT
                       ENDIF

                       aProperties := __GetMembers( oObj,, HB_OO_CLSTP_PUBLISHED, HB_OO_MSG_DATA )
                       FOR n := 1 TO LEN( aProperties )
                           IF ASCAN( aList, {|c| Upper(PosDel(c,,2))==Upper(aProperties[n])} ) == 0
                              IF __ObjHasMsg( oObj, "__a_"+aProperties[n] )
                                 aProp   := __objSendMsg( oObj, "__a_"+aProperties[n] )
                                 AADD( aList, aProp[1]+"?8" )
                               ELSE
                                 AADD( aList, aProperties[n] + "?8" )
                              ENDIF
                           ENDIF
                       NEXT

                       aMethods := __objGetMethodList( oObj, HB_OO_MTHD_SYMBOL )
                       //aMethods := __GetMembers( oObj,,HB_OO_MTHD_SYMBOL, HB_OO_MSG_METHOD | HB_OO_MSG_INLINE )
                       FOR n := 1 TO LEN( aMethods )
                           IF ASCAN( aList, {|c| Upper(PosDel(c,,2))==Upper(aMethods[n])} ) == 0
                              IF __ObjHasMsg( oObj, "__m_"+aMethods[n] )
                                 aProp   := __objSendMsg( oObj, "__m_"+aMethods[n] )
                                 AADD( aList, aProp[1]+"?7" )
                               ELSE
                                 AADD( aList, Upper(aMethods[n][1])+Lower(SubStr(aMethods[n],2))+"?7" )
                              ENDIF
                           ENDIF
                       NEXT

                       aSort( aList,,,{|x, y| x < y})
                       cList := ""
                       FOR n := 1 TO LEN( aList )
                           cList += aList[n]+ IIF( n<LEN(aList)," ", "" )
                       NEXT
                       SCI_SEND( SCI_AUTOCSETCANCELATSTART, 0 )
                       SCI_SEND( SCI_AUTOCSETMAXHEIGHT, 15 )
                       SCI_SEND( SCI_AUTOCSETIGNORECASE, 1 )
                       SCI_SEND( SCI_AUTOCSHOW, 0, cList )
                    ENDIF
                 ENDIF
              ENDIF
      ENDCASE
   ENDIF
RETURN NIL

METHOD GetWithObject() CLASS SourceEditor
   LOCAL cObj, nWith, nChar, nPos := ::Source:GetCurrentPos()
   cObj := ""
   IF ::FindNext( "WITH OBJECT", .T. )
      nWith := ::PosFind
      ::Source:GotoPosition( nPos )
      IF ! ::FindNext( "END", .T. ) .OR. ::PosFind < nWith // END is before WITH OBJECT
         ::Source:GotoPosition( nPos )
         IF ! ::FindNext( "METHOD", .T. ) .OR. ::PosFind < nWith
            ::Source:GotoPosition( nPos )
            IF ! ::FindNext( "FUNCTION", .T. ) .OR. ::PosFind < nWith
               ::PosFind := nWith+11
               WHILE ( nChar := ::Source:GetCharAt(::PosFind) ) != 13
                  cObj += CHR(nChar)
                  ::PosFind++
               ENDDO
               cObj := ALLTRIM(cObj)+":"
               IF cObj[1] == ":"
                  ::Source:GotoPosition( nWith-1 )
                  cObj := ::GetWithObject() + cObj
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   ::Source:GotoPosition( nPos )
RETURN cObj

//------------------------------------------------------------------------------------------------------------------------------------
METHOD AutoIndentText() CLASS SourceEditor
   LOCAL nCurLine     := ::Source:GetCurLine()
   LOCAL nIndentation := ::Source:GetLineIndentation( nCurLine - 1 )
   ::Source:SetLineIndentation( nCurLine, nIndentation )
   ::Source:GotoPosition( ::Source:GetCurrentPos() + nIndentation )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnDestroy() CLASS SourceEditor
   ::Super:OnDestroy()
   aEval( ::aDocs, {|oDoc| IIF( oDoc != NIL, oDoc:Close(),) } )

   IF __ObjHasMsg( ::Application, "EditorProps" )
      ::Application:IniFile:WriteInteger( "Settings", "WrapSearch", ::Application:EditorProps:WrapSearch )
   ENDIF

   ::Application:IniFile:WriteColor( "Colors", "NormalText",   ::ColorNormalText )
   ::Application:IniFile:WriteColor( "Colors", "BackGround",   ::ColorBackground )
   ::Application:IniFile:WriteColor( "Colors", "SelectedLine", ::ColorSelectedLine )

   ::Application:IniFile:WriteColor( "Colors", "Numbers",      ::ColorNumbers )
   ::Application:IniFile:WriteColor( "Colors", "Strings",      ::ColorStrings )
   ::Application:IniFile:WriteColor( "Colors", "Comments",     ::ColorComments )
   ::Application:IniFile:WriteColor( "Colors", "Operators",    ::ColorOperators )
   ::Application:IniFile:WriteColor( "Colors", "Preprocessor", ::ColorPreprocessor )

   ::Application:IniFile:WriteColor( "Colors", "Keywords1",    ::ColorKeywords1 )
   ::Application:IniFile:WriteColor( "Colors", "Keywords2",    ::ColorKeywords2 )
   ::Application:IniFile:WriteColor( "Colors", "Keywords3",    ::ColorKeywords3 )
   ::Application:IniFile:WriteColor( "Colors", "Keywords4",    ::ColorKeywords4 )

   ::Application:IniFile:WriteString( "Font", "FaceName",      ::FontFaceName )
   ::Application:IniFile:WriteInteger( "Font", "Size",         ::FontSize )
   ::Application:IniFile:WriteInteger( "Font", "Bold",         IIF( ::FontBold, 1, 0 ) )
   ::Application:IniFile:WriteInteger( "Font", "Italic",       IIF( ::FontItalic, 1, 0 ) )

   ::Application:IniFile:WriteInteger( "Settings", "CaretLineVisible", ::CaretLineVisible )
   ::Application:IniFile:WriteInteger( "Settings", "AutoIndent", ::AutoIndent )
   ::Application:IniFile:WriteInteger( "Settings", "TrimEOLSpaces", ::TrimEOLSpaces )

   ::Application:IniFile:WriteInteger( "Settings", "TabSpacing", ::TabWidth )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetSearchFlags( oFind ) CLASS SourceEditor
   LOCAL nFlags := 0
   IF oFind:MatchCase
      nFlags := nFlags | SCFIND_MATCHCASE
   ENDIF
   IF oFind:WholeWord
      nFlags := nFlags | SCFIND_WHOLEWORD
   ENDIF
RETURN nFlags

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnFindNext( oFind ) CLASS SourceEditor
   LOCAL oSource, hFile, cLine, nCol, nRow, nLine

   ::cFindWhat  := oFind:FindWhat
   ::nDirection := oFind:Direction

   ::Application:Project:__cFindText := ::cFindWhat

   IF ! ::FindInFiles

      ::Source:SetSearchFlags( ::GetSearchFlags( oFind ) )
      IF ! ::FindNext( oFind:FindWhat, oFind:Direction == 0 )
         ::MessageBox( "Cannot find literal text: " + oFind:FindWhat, "Visual xHarbour", MB_ICONEXCLAMATION )
         oFind:SetFocus()
      ENDIF

    ELSE
      oFind:Close()
      ::FindInFiles := .F.
      ::Application:FindInFilesList:ResetContent()
      ::Application:ResultPanel:Visible := .T.
      ::Application:FindInFilesList:Parent:Select()
      FOR EACH oSource IN ::aDocs
          IF ( hFile := FOpen( oSource:File ) ) >= 0
             nLine := 1
             DO WHILE HB_FReadLine( hFile, @cLine, Chr(13) + Chr(10) ) == 0
                IF ! oFind:MatchCase
                   nCol := AT( Upper(::cFindWhat), Upper(cLine) )
                 ELSE
                   nCol := AT( ::cFindWhat, cLine )
                ENDIF

                IF nCol > 0
                   IF oFind:WholeWord .AND. ( ( nCol > 1 .AND. Upper(cLine[nCol-1]) $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" ) .OR. Upper(cLine[ nCol + Len( ::cFindWhat )+1 ]) $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" )
                      nCol := 0
                   ENDIF
                ENDIF

                IF nCol > 0
                   WITH OBJECT ::Application:FindInFilesList
                      nRow := :InsertItem( oSource:FileName,,,, nCol )
                      :SetItemText( nRow, 1, xStr( nLine ) )
                      :SetItemText( nRow, 2, AllTrim( cLine ) )
                   END
                ENDIF
                nLine ++
             ENDDO
             FClose( hFile )
          ENDIF
      NEXT
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD FindNext( cFindWhat, lReverseDirection ) CLASS SourceEditor
   LOCAL lFound, nStartPos, nEndPos
   LOCAL nStart, nEnd

   DEFAULT cFindWhat TO ::cFindWhat
   DEFAULT lReverseDirection TO ::nDirection == 0

   DEFAULT cFindWhat TO ""
   DEFAULT lReverseDirection TO .F.

   lFound := .F.

   IF LEN( cFindWhat ) == 0
      return lFound
   ENDIF

   nStartPos := ::Source:GetSelectionEnd()
   nEndPos   := ::Source:GetTextLen()

   IF lReverseDirection
      nStartPos := ::Source:GetSelectionStart()
      nEndPos   := 0
   ENDIF

   ::PosFind := ::Source:FindInTarget( cFindWhat, nStartPos, nEndPos )

   IF ::PosFind == -1 .AND. ::Application:EditorProps:WrapSearch == 1
      IF lReverseDirection
         nStartPos := ::Source:GetTextLen()
         nEndPos   := 0
       ELSE
         nStartPos := 0
         nEndPos   := ::Source:GetTextLen()
      ENDIF
      ::PosFind := ::Source:FindInTarget( cFindWhat, nStartPos, nEndPos )
   ENDIF

   IF ( lFound := ::PosFind != -1 )
      nStart := ::Source:GetTargetStart()
      nEnd   := ::Source:GetTargetEnd()

      ::EnsureRangeVisible( nStart, nEnd, .T. )
      ::Source:SetSelection( nStart, nEnd )
   ENDIF
RETURN lFound

//------------------------------------------------------------------------------------------------------------------------------------
METHOD EnsureRangeVisible( nPosStart, nPosEnd, lEnforcePolicy ) CLASS SourceEditor
   LOCAL nLine, nLineStart, nLineEnd
   nLineStart := ::Source:LineFromPosition( MIN( nPosStart, nPosEnd ) )
   nLineEnd   := ::Source:LineFromPosition( MAX( nPosStart, nPosEnd ) )
   FOR nLine := nLineStart TO nLineEnd
       IF lEnforcePolicy
          ::Source:EnsureVisibleEnforcePolicy( nLine )
        ELSE
          ::Source:EnsureVisible( nLine )
       ENDIF
   NEXT
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnReplace( oFind ) CLASS SourceEditor
   IF ::Source:GetSelLen() == 0
      IF ! EMPTY( oFind:FindWhat )
         ::Source:SearchNext( ::GetSearchFlags( oFind ), oFind:FindWhat )
      ENDIF
    ELSEIF ::Source:GetSelLen() > 0
      ::Source:ReplaceSel( oFind:ReplaceWith )
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnReplaceAll( oFind ) CLASS SourceEditor
   ::Source:ReplaceAll( oFind:FindWhat, oFind:ReplaceWith, ::GetSearchFlags( oFind ), ::InSelection )
RETURN 0


//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------

CLASS Source INHERIT ProjectFile
   ACCESS Application INLINE ::Owner:Application
   DATA pSource      EXPORTED
   DATA Owner        EXPORTED
   DATA FileTime     EXPORTED
   DATA Modified     EXPORTED INIT .F.
   DATA FirstOpen    EXPORTED INIT .T.
   DATA SavedPos     EXPORTED INIT 0
   DATA nVisLine     EXPORTED INIT 0
   DATA Extension    EXPORTED INIT "prg"
   DATA lStyled      EXPORTED INIT .T.
   DATA cObj         EXPORTED INIT ""
   DATA nPrevLine    EXPORTED
   DATA PrevFile     EXPORTED
   DATA __xCtrlName  EXPORTED INIT "Source"
   DATA __lSelected  EXPORTED INIT .F.
   DATA __nSearchCol EXPORTED INIT 0
   // Compatibility with xedit for debugger ------------------------------------------------
   ACCESS cFile             INLINE ::FileName
   ACCESS cPath             INLINE IIF( ! EMPTY(::Path), ::Path + "\", "" )
   ACCESS nLine             INLINE ::GetCurLine()+1
   ACCESS lReadOnly         INLINE ::GetReadOnly()
   ASSIGN lReadOnly(lSet)   INLINE ::SetReadOnly(lSet)
   DATA HighlightedLine     EXPORTED

   METHOD Load(cFile,cText) INLINE IIF( ! EMPTY(cFile), ::Open(cFile), ::SetText(cText) )
   METHOD GoLine(n)         INLINE ::GoToLine(n-1)
   METHOD Highlight()       INLINE ::HighlightedLine := ::GetCurLine()
   METHOD SetDisplay()      INLINE ::Select()
   // --------------------------------------------------------------------------------------

   METHOD Init( oOwner, cFile ) CONSTRUCTOR
   METHOD SendEditor()
   METHOD Open()
   METHOD Reload()
   METHOD Close()
   METHOD Save()
   METHOD GetText()

   METHOD SavePos()                           INLINE ::SavedPos := ::GetCurrentPos(), ::nVisLine := ::SendEditor( SCI_GETFIRSTVISIBLELINE, 0, 0 )
   METHOD ReleaseDocument()                   INLINE ::Owner:SendMessage( SCI_RELEASEDOCUMENT, 0, ::pSource )

   METHOD Select()

   METHOD CreateDocument()                    INLINE SCI_SEND( SCI_CREATEDOCUMENT, 0, 0 )

   METHOD GotoPosition( nPos )                INLINE ::SendEditor( SCI_GOTOPOS, nPos, 0 )
   METHOD GotoLine( nLine )                   INLINE ::nPrevLine := ::GetCurLine()+1, ::SendEditor( SCI_GOTOLINE, nLine, 0 )
   METHOD EmptyUndoBuffer()                   INLINE ::SendEditor( SCI_EMPTYUNDOBUFFER, 0, 0 )
   METHOD CanUndo()                           INLINE ::SendEditor( SCI_CANUNDO, 0, 0 )==1
   METHOD CanRedo()                           INLINE ::SendEditor( SCI_CANREDO, 0, 0 )==1
   METHOD Undo()                              INLINE ::SendEditor( SCI_UNDO, 0, 0 )
   METHOD Redo()                              INLINE ::SendEditor( SCI_REDO, 0, 0 )
   METHOD Delete()                            INLINE ::SendEditor( SCI_DELETEBACK, 0, 0 )
   METHOD SelectAll()                         INLINE ::SendEditor( SCI_SELECTALL, 0, 0 )
   METHOD Copy()                              INLINE ::SendEditor( SCI_COPY, 0, 0 )
   METHOD Paste()                             INLINE ::SendEditor( SCI_PASTE, 0, 0 )
   METHOD Cut()                               INLINE ::SendEditor( SCI_CUT, 0, 0 )
   METHOD CanPaste()                          INLINE ::SendEditor( SCI_CANPASTE, 0, 0 )==1
   METHOD SetReadOnly( nSet )                 INLINE ::SendEditor( SCI_SETREADONLY, nSet, 0 )
   METHOD GetReadOnly()                       INLINE ::SendEditor( SCI_GETREADONLY, 0, 0 )
   METHOD GetCurrentPos()                     INLINE ::SendEditor( SCI_GETCURRENTPOS, 0, 0 )
   METHOD SetCurrentPos( nPos )               INLINE ::SendEditor( SCI_SETCURRENTPOS, nPos, 0 )
   METHOD GoToPos( nPos )                     INLINE ::SendEditor( SCI_GOTOPOS, nPos, 0 )
   METHOD GetColumn( nPos )                   INLINE ::SendEditor( SCI_GETCOLUMN, nPos, 0 )

   METHOD PositionFromPoint( x, y )           INLINE ::SendEditor( SCI_POSITIONFROMPOINT, x, y )

   METHOD LineFromPosition( nPos )            INLINE ::SendEditor( SCI_LINEFROMPOSITION, nPos, 0 )
   METHOD PositionFromLine( nLine )           INLINE ::SendEditor( SCI_POSITIONFROMLINE, nLine, 0 )
   METHOD LineLength( nLine )                 INLINE ::SendEditor( SCI_LINELENGTH, nLine, 0 )

   METHOD GetCurLine()                        INLINE ::SendEditor( SCI_LINEFROMPOSITION, ::GetCurrentPos(), 0 )
   METHOD GetCurColumn()                      INLINE ::SendEditor( SCI_GETCOLUMN, ::GetCurrentPos(), 0 )

   METHOD GetSelectionMode()                  INLINE ::SendEditor( SCI_GETSELECTIONMODE, 0, 0 )
   METHOD GetSelections()                     INLINE ::SendEditor( SCI_GETSELECTIONS, 0, 0 )

   METHOD GetSelectionStart()                 INLINE ::SendEditor( SCI_GETSELECTIONSTART, 0, 0 )
   METHOD GetSelectionEnd()                   INLINE ::SendEditor( SCI_GETSELECTIONEND, 0, 0 )
   METHOD GetSelectionNStart( nSel )          INLINE ::SendEditor( SCI_GETSELECTIONNSTART, nSel, 0 )
   METHOD GetSelectionNEnd( nSel )            INLINE ::SendEditor( SCI_GETSELECTIONNEND, nSel, 0 )

   METHOD SetSavePoint()                      INLINE ::SendEditor( SCI_SETSAVEPOINT, 0, 0 )
   METHOD GetTextLen()                        INLINE ::SendEditor( SCI_GETTEXTLENGTH, 0, 0 )
   METHOD SetUndoCollection(n)                INLINE ::SendEditor( SCI_SETUNDOCOLLECTION, n, 0 )
   METHOD BeginUndoAction()                   INLINE ::SendEditor( SCI_BEGINUNDOACTION, 0, 0 )
   METHOD EndUndoAction()                     INLINE ::SendEditor( SCI_ENDUNDOACTION, 0, 0 )

   METHOD ChkDoc()                            INLINE IIF( ::Owner:GetCurDoc() != ::pSource, (SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource ),::__lSelected := .T.),)

   METHOD FindText( nFlags, ttf )             INLINE ::SendEditor( SCI_FINDTEXT, nFlags, ttf )
   METHOD SearchNext( nFlags, cText )         INLINE ::SendEditor( SCI_SEARCHNEXT, nFlags, cText )
   METHOD SearchPrev( nFlags, cText )         INLINE ::SendEditor( SCI_SEARCHPREV, nFlags, cText )
   METHOD ReplaceSel( cText )                 INLINE ::SendEditor( SCI_REPLACESEL, 0, cText )
   METHOD GetSelLen()                         INLINE ::SendEditor( SCI_GETSELTEXT, 0, 0 )-1
   METHOD GetSelText( cBuffer )               INLINE ::ChkDoc(), cBuffer := SPACE( SCI_SEND( SCI_GETSELTEXT, 0, 0 ) ),;
                                                                 SCI_SEND( SCI_GETSELTEXT, 0, cBuffer ),;
                                                                 ALLTRIM(LEFT(cBuffer,LEN(cBuffer)-1))
   METHOD SetSearchFlags( nFlags )            INLINE ::SendEditor( SCI_SETSEARCHFLAGS, nFlags )
   METHOD SetTargetStart( nStart )            INLINE ::SendEditor( SCI_SETTARGETSTART, nStart )
   METHOD SetTargetEnd( nEnd )                INLINE ::SendEditor( SCI_SETTARGETEND, nEnd )
   METHOD ReplaceTarget( nLen, cText)         INLINE ::SendEditor( SCI_REPLACETARGET, nLen, cText )
   METHOD SearchInTarget( nLen, cText)        INLINE ::SendEditor( SCI_SEARCHINTARGET, nLen, cText )
   METHOD GetTargetStart()                    INLINE ::SendEditor( SCI_GETTARGETSTART, 0, 0 )
   METHOD GetTargetEnd()                      INLINE ::SendEditor( SCI_GETTARGETEND, 0, 0 )
   METHOD GetCharAt( nPos )                   INLINE ::SendEditor( SCI_GETCHARAT, nPos, 0 )
   METHOD PositionAfter( nPos )               INLINE ::SendEditor( SCI_POSITIONAFTER, nPos, 0 )
   METHOD SetSelection( nCaret, nEnd )        INLINE ::SendEditor( SCI_SETSELECTION, nCaret, nEnd )
   METHOD GetStyleAt( nPos )                  INLINE ::SendEditor( SCI_GETSTYLEAT, nPos, 0 )
   METHOD GetEndStyled()                      INLINE ::SendEditor( SCI_GETENDSTYLED, 0, 0 )
   METHOD EnsureVisible( nLine )              INLINE ::SendEditor( SCI_ENSUREVISIBLE, nLine, 0 )
   METHOD EnsureVisibleEnforcePolicy( nLine ) INLINE ::SendEditor( SCI_ENSUREVISIBLEENFORCEPOLICY, nLine, 0 )
   METHOD GetLineCount()                      INLINE ::SendEditor( SCI_GETLINECOUNT, 0, 0 )

   METHOD SetUseTabs( nUseTabs )              INLINE ::SendEditor( SCI_SETUSETABS, nUseTabs, 0 )
   METHOD AppendText( cText )                 INLINE ::SendEditor( SCI_APPENDTEXT, Len(cText), cText )
   METHOD UsePopUp( n )                       INLINE ::SendEditor( SCI_USEPOPUP, n )
   METHOD GetLineState( n )                   INLINE ::SendEditor( SCI_GETLINESTATE, n )
   METHOD GetLineIndentation( nLine )         INLINE ::SendEditor( SCI_GETLINEINDENTATION, nLine )
   METHOD SetLineIndentation( nLine, nInd )   INLINE ::SendEditor( SCI_SETLINEINDENTATION, nLine, nInd )
   METHOD SetTabWidth( nChars )               INLINE ::SendEditor( SCI_SETTABWIDTH, nChars, 0 )

   METHOD SetText()
   METHOD GetLine()
   METHOD FindInPos()
   METHOD ReplaceAll()
   METHOD FindInTarget()
   METHOD GetBookmarks()
ENDCLASS

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Select( lMarkPrev ) CLASS Source
   DEFAULT lMarkPrev TO .T.

   IF lMarkPrev .AND. ::Owner:Source:pSource != ::pSource
      s_nSecs := Seconds()
      ::Owner:pPrevSel := ::Owner:Source:pSource
   ENDIF
   ::__lSelected := .T.
   SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource )
   ::Owner:xSource := Self
   ::GotoPosition( ::SavedPos )
   SCI_SEND( SCI_SETFIRSTVISIBLELINE, ::nVisLine, 0 )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetBookmarks( aBookmarks ) CLASS Source
   LOCAL n, nLines, nVisLine, nPos, cMarks := "", nLine, pSource := ::Owner:GetCurDoc()

   IF ! ( pSource == ::pSource )
      nPos     := SCI_SEND( SCI_GETCURRENTPOS, 0, 0 )
      nVisLine := SCI_SEND( SCI_GETFIRSTVISIBLELINE, 0, 0 )
      SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource )
      ::__lSelected := .T.
   ENDIF

   nLines   := SCI_SEND( SCI_GETLINECOUNT, 0, 0 )
   FOR n := 1 TO nLines
       IF ( nLine := SCI_SEND( SCI_MARKERNEXT, n-1, 1<<MARKER_MASK ) ) >= 0
          cMarks += IIF( ! EMPTY(cMarks),"|","") + xStr(nLine)
          n := nLine+1
          IF aBookmarks != NIL
             AADD( aBookmarks, nLine )
          ENDIF
          LOOP
        ELSE
          EXIT
       ENDIF
   NEXT

   IF ! ( pSource == ::pSource )
      SCI_SEND( SCI_SETDOCPOINTER, 0, pSource )
      SCI_SEND( SCI_GOTOPOS, nPos, 0 )
      SCI_SEND( SCI_SETFIRSTVISIBLELINE, nVisLine, 0 )
   ENDIF
RETURN cMarks

//------------------------------------------------------------------------------------------------------------------------------------
METHOD SendEditor( nMsg, wParam, lParam ) CLASS Source
   LOCAL xReturn, pSource := ::Owner:GetCurDoc()
   IF ! ( pSource == ::pSource )
      SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource )
      ::__lSelected := .T.
   ENDIF
   xReturn := SCI_SEND( nMsg, wParam, lParam )
   IF ! ( pSource == ::pSource )
      SCI_SEND( SCI_SETDOCPOINTER, 0, pSource )
   ENDIF
RETURN xReturn

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oOwner, cFile ) CLASS Source
   ::Owner   := oOwner
   ::Owner:xSource := Self
   ::lSource := .T.
   ::pSource := ::CreateDocument()

   ::FirstOpen := .T.

   IF cFile != NIL .AND. File( cFile )
      ::Open( cFile )
   ENDIF

   AADD( ::Owner:aDocs, Self )
   ::Select()
   ::SetUseTabs(0)
   ::UsePopUp(0)
   ::SetTabWidth( ::Owner:TabWidth )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Close() CLASS Source
   LOCAL n
   ::lSource := .F.
   IF ( n := ASCAN( ::Owner:aDocs, {|o| o:pSource==::pSource} ) ) > 0
      //IF ! ::__lSelected
         ::ReleaseDocument()
      //ENDIF
      IF ::TreeItem != NIL
         ::TreeItem:Delete()
      ENDIF
      ADEL( ::Owner:aDocs, n, .T. )
      IF ( n := ASCAN( ::Owner:aDocs, {|o| o:pSource==::Owner:GetCurDoc() } ) ) > 0
         ::Owner:aDocs[n]:Select()
      ELSE
         ::Owner:Source := NIL
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Reload( cText ) CLASS Source
   LOCAL nPos, nVisLine, lReadOnly
   ::ChkDoc()
   nPos        := SCI_SEND( SCI_GETCURRENTPOS, 0, 0 )
   nVisLine    := SCI_SEND( SCI_GETFIRSTVISIBLELINE, 0, 0 )
   lReadOnly   := ::lReadOnly
   ::FirstOpen := .T.
   DEFAULT cText TO MemoRead( ::File, .F. )
   ::lReadOnly := .F.
   ::SetText( cText )
   ::lReadOnly := lReadOnly
   ::FileTime  := FileTime( ::File )
   ::EmptyUndoBuffer()
   ::Modified  := .F.
   ::FirstOpen := .F.
   ::GotoPosition( nPos )

   SCI_SEND( SCI_GOTOPOS, nPos, 0 )
   SCI_SEND( SCI_SETFIRSTVISIBLELINE, nVisLine, 0 )

   ::SetSavePoint()
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Open( cFile, cBookmarks ) CLASS Source
   LOCAL n, aBookmarks
   ::SetText( MemoRead( cFile, .F. ) )

   ::SetSavePoint()
   ::GotoPosition( 0 )
   ::EmptyUndoBuffer()
   ::Modified  := .F.
   ::lSource := .T.

   IF ! EMPTY( cBookmarks )
      aBookmarks := hb_aTokens( cBookmarks, "|" )
      AEVAL( aBookmarks, {|c| ::Owner:BookmarkAdd(VAL(c))} )
   ENDIF

   ::FirstOpen := .F.
   n := RAT( "\", cFile )
   ::FileName := SUBSTR( cFile, n+1 )
   ::Path     := SUBSTR( cFile, 1, n-1 )
   ::File     := cFile
   ::FileTime := FileTime( cFile )
   ::SetSavePoint()
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetText() CLASS Source
   LOCAL cText, nLen, pSource := ::Owner:GetCurDoc()

   IF ! ( ::pSource == pSource )
      SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource )
      ::__lSelected := .T.
   ENDIF

   nLen := SCI_SEND( SCI_GETLENGTH, 0, 0 )+1
   cText := SPACE( nLen )
   SCI_SEND( SCI_GETTEXT, nLen, @cText )
   cText := STRTRAN( cText, CHR(0) )

   IF ! ( ::pSource == pSource )
      SCI_SEND( SCI_SETDOCPOINTER, 0, pSource )
   ENDIF
RETURN cText

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetLine( nLine ) CLASS Source
   LOCAL cText, nLen
   ::ChkDoc()
   nLen := SCI_SEND( SCI_LINELENGTH, nLine, 0 )+1
   cText := SPACE( nLen )
   SCI_SEND( SCI_GETLINE, nLine, @cText )
   cText := STRTRAN( cText, CHR(0) )
RETURN cText

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Save( cFile ) CLASS Source
   LOCAL hFile, cText, n, cBak, oFile, nPos, nVisLine, nLine, nCol, cBuffer, lEof, cLine, nCurLine, aBookmarks

   LOCAL nLineLen     := 1000
   LOCAL lWrap        := .T.
   LOCAL nEndOfLine   := 0
   LOCAL nStartOffset := 1
   LOCAL nEndOffSet   := 0
   LOCAL lFound       := .F.

   IF cFile != NIL
      ::File := cFile
   ENDIF

   IF EMPTY( ::File )
      oFile := CFile( "" )
      oFile:Flags := OFN_EXPLORER | OFN_OVERWRITEPROMPT
      oFile:DefaultExtention := ".prg"
      oFile:AddFilter( "Source Files (*.prg, *.c)", "*.prg;*.c" )
      oFile:Path := ::Application:Project:Properties:Path + "\" + ::Application:Project:Properties:Source
      oFile:SaveDialog()
      IF oFile:Result == IDOK
         ::File := oFile:Path + "\" + oFile:Name
      ENDIF
   ENDIF

   IF ! EMPTY( ::File )
      ::ChkDoc()
      IF FILE( ::File ) .AND. ( ! __ObjHasMsg( ::Application, "EditorProps" ) .OR. ::Application:EditorProps:SaveBAK == 1 )
         cBak := ::File
         IF ( n := RAT( ".", cBak ) ) > 0
            cBak := SUBSTR( cBak, 1, n-1 )
         ENDIF
         cBak += ".bak"
         IF FILE( cBak )
            FERASE( cBak )
         ENDIF
         FRENAME( ::File, cBak )
      ENDIF

      IF ( hFile := fCreate( ::File ) ) <> -1
         ::SetSavePoint()

         cText := ::GetText()

         IF ::Owner:TrimEOLSpaces == 1
            nCol     := ::GetCurColumn()
            nLine    := ::GetCurLine()

            nVisLine := SCI_SEND( SCI_GETFIRSTVISIBLELINE, 0, 0 )

            cBuffer  := ""
            nCurLine := 1
            aBookmarks := {}
            DO WHILE .T.
               IF SCI_SEND( SCI_MARKERGET, nCurLine, MARKER_MASK ) == 1
                  AADD( aBookmarks, nCurLine )
               ENDIF

               HB_ReadLine( cText       , ;
                            NIL         , ;
                            nLineLen    , ;
                            NIL         , ;
                            lWrap       , ;
                            nStartOffset, ;
                            @lFound     , ;
                            @lEof       , ;
                            @nEndOffset , ;
                            @nEndOfLine   )

               IF lEof
                  EXIT
               ENDIF
               cLine := SubStr( cText, nStartOffset, nEndOffset-nStartOffset+1 )
               cBuffer += RTrim( cLine, .T. ) + CRLF
               nStartOffSet := nEndOfLine
               nCurLine ++
            ENDDO

            cText := cBuffer

            ::SetText( cText )
            IF ! Empty( aBookmarks )
               AEVAL( aBookmarks, {|nLine| SCI_SEND( SCI_MARKERADD, nLine, MARKER_MASK )} )
            ENDIF

            //::GotoLine( nLine )

            nPos := SCI_SEND( SCI_FINDCOLUMN, nLine, nCol )
            SCI_SEND( SCI_GOTOPOS, nPos, 0 )
            SCI_SEND( SCI_SETFIRSTVISIBLELINE, nVisLine, 0 )

            ::EmptyUndoBuffer()

         ENDIF

         fWrite( hFile, cText, Len(cText) )
         fClose( hFile )
         ::FileTime := FileTime( ::File )
         ::Modified := .F.

      ENDIF

      n := RAT( "\", ::File )
      ::FileName := SUBSTR( ::File, n+1 )
      ::Path     := SUBSTR( ::File, 1, n-1 )

      ::TreeItem:Text       := ::FileName
      ::Owner:Parent:Text   := ::FileName
      ::TreeItem:ImageIndex := IIF( RIGHT( lower(::FileName), 2 ) == ".c", 4, 3 )

      ::FirstOpen := .F.
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD FindInPos( cText, nStartPos, nEndPos ) CLASS Source
   DEFAULT nEndPos TO ::Source:GetTextLen()
   ::SetTargetStart( nStartPos )
   ::SetTargetEnd( nEndPos )
RETURN ::SearchInTarget( Len( cText ), cText )

//------------------------------------------------------------------------------------------------------------------------------------
METHOD SetText( cText ) CLASS Source
   ::ChkDoc()
   SCI_SEND( SCI_SETTEXT, 0, cText )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD ReplaceAll( cFind, cReplace, nFlags, lInSelection ) CLASS Source
   local nStartPos, nEndPos, nLen, nCountSel, nSelType
   local nRepLen, nStarLine, nEndLine
   local nLastMatch, nReplacements, nLenTarget, linsideASel, i, nMovePastEOL, nChrNext
   local nLenReplaced, nCurPos, nPosFind
//   local pSource, nPos, nVisLine

   IF ( nLen := LEN( cFind ) ) == 0 .OR. LEN( cReplace ) == 0
      return -1
   ENDIF
   //-------------------------------------------------------------
//    pSource  := ::Owner:GetCurDoc()
//    nPos     := SCI_SEND( SCI_GETCURRENTPOS, 0, 0 )
//    nVisLine := SCI_SEND( SCI_GETFIRSTVISIBLELINE, 0, 0 )
//
//    SCI_SEND( SCI_SETDOCPOINTER, 0, ::pSource )
   //-------------------------------------------------------------

   DEFAULT lInSelection TO .F.
   nCountSel := ::GetSelections()

   IF lInSelection
      nStartPos := ::GetSelectionStart()
      nEndPos   := ::GetSelectionEnd()
      nSelType  := ::GetSelectionMode()
      IF nSelType == SC_SEL_LINES
         // Take care to replace in whole lines
         nStarLine := ::LineFromPosition( nStartPos )
         nStartPos := ::PositionFromLine( nStarLine )
         nEndLine  := ::LineFromPosition( nEndPos )
         nEndPos   := ::PositionFromLine( nEndLine )
       ELSE
         FOR i := 1 TO nCountSel
            nStartPos := MIN( nStartPos, ::GetSelectionNStart(i) )
            nEndPos   := MAX( nEndPos, ::GetSelectionNEnd(i) )
         NEXT
      ENDIF
      IF nStartPos == nEndPos
         return -2
      ENDIF
    ELSE
      nStartPos := 0
      nEndPos := ::GetTextLen()
   ENDIF

   nRepLen := LEN( cReplace )

   ::SetSearchFlags( nFlags )
   nPosFind := ::FindInTarget( cFind, nStartPos, nEndPos )

   nReplacements := 0
   IF nPosFind >= 0 .AND. nPosFind <= nEndPos
      nLastMatch := nPosFind

      nCurPos := ::GetCurrentPos()
      ::BeginUndoAction()

      // Replacement loop
      DO WHILE nPosFind >= 0
         nLenTarget := ::GetTargetEnd() - ::GetTargetStart()
         IF lInSelection .AND. nCountSel > 1
            // We must check that the found target is entirely inside a selection
            linsideASel := .F.
            FOR i := 1 TO nCountSel
               IF ! linsideASel
                  EXIT
               ENDIF
               nStartPos := ::GetSelectionNStart(i)
               nEndPos   := ::GetSelectionNEnd(i)
               IF nPosFind >= nStartPos .AND. nPosFind + nLenTarget <= nEndPos
                  linsideASel := .T.
               ENDIF
            NEXT
            IF ! linsideASel
               // Found target is totally or partly outside the selections
               nLastMatch := nPosFind + 1

               IF nLastMatch >= nEndPos
                  // Run off the end of the document/selection with an empty match
                  nPosFind := -1
                ELSE
                  nPosFind := ::FindInTarget( cFind, nLastMatch, nEndPos )
               ENDIF
               LOOP   // No replacement
            ENDIF
         ENDIF

         nMovePastEOL := 0
         IF nLenTarget <= 0
            nChrNext := ::GetCharAt( SCI_GETCHARAT, ::GetTargetEnd() )
            IF nChrNext == CHR(13) .OR. nChrNext == CHR(10)
               nMovePastEOL := 1
            ENDIF
         ENDIF

         nLenReplaced := nRepLen

         ::ReplaceTarget( nRepLen, cReplace )

         nEndPos += nLenReplaced - nLenTarget
         nLastMatch := nPosFind + nLenReplaced + nMovePastEOL
         IF nLenTarget == 0
            nLastMatch := ::PositionAfter( nLastMatch )
         ENDIF

         IF nLastMatch >= nEndPos
            // Run off the end of the document/selection with an empty match
            nPosFind := -1
          ELSE
            nPosFind := ::FindInTarget( cFind, nLastMatch, nEndPos )
         ENDIF
         nReplacements++
      ENDDO
      IF lInSelection
         IF nCountSel == 1
            ::SetSelection( nStartPos, nEndPos )
         ENDIF
       ELSE
         //::SetSelection( nLastMatch, nLastMatch )
      ENDIF
      ::SetCurrentPos( nCurPos )
      ::EndUndoAction()
      ::Owner:PosFind := nPosFind
   ENDIF

//    SCI_SEND( SCI_SETDOCPOINTER, 0, pSource )
//    SCI_SEND( SCI_GOTOPOS, nPos, 0 )
//    SCI_SEND( SCI_SETFIRSTVISIBLELINE, nVisLine, 0 )

RETURN nReplacements

//------------------------------------------------------------------------------------------------------------------------------------
METHOD FindInTarget( cText, nStartPos, nEndPos ) CLASS Source
   LOCAL nPos, nLen := LEN( cText )
   ::SetTargetStart( nStartPos )
   ::SetTargetEnd( nEndPos )
   nPos := ::SearchInTarget( nLen, cText )

   WHILE ::Owner:FindInStyle .AND. nPos != -1 // ::Owner:FindStyle != ::GetStyleAt( nPos )
      IF nStartPos < nEndPos
         ::SetTargetStart( nPos + 1 )
         ::SetTargetEnd( nEndPos )
       ELSE
         ::SetTargetStart( nStartPos )
         ::SetTargetEnd( nPos + 1 )
      ENDIF
      nPos := ::SearchInTarget( nLen, cText )
   ENDDO
RETURN nPos

//------------------------------------------------------------------------------------------------------------------------------------

CLASS Settings INHERIT Dialog
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()

   METHOD Settings_OnLoad()
   METHOD DefBack_OnClick()
   METHOD DefFore_OnClick()
   METHOD Button3_OnClick()
   METHOD Apply()
   METHOD SetFonts()
ENDCLASS

METHOD Init() CLASS Settings
   ::Super:Init()
   ::EventHandler[ "OnLoad" ] := "Settings_OnLoad"
   WITH OBJECT ( ColorDialog( Self ) )
      :Name := "ColorDialog1"
      :Create()
   END
   ::ThickFrame    := .F.
   ::Name          := "Settings"
   ::Modal         := .T.
   ::Width         := 471
   ::Height        := 632
   ::Center        := .T.
   ::Text          := "Settings"
   ::Create()
RETURN Self

METHOD OnInitDialog() CLASS Settings
   LOCAL aFonts, n
   WITH OBJECT ( TABSTRIP( Self ) )
      :Name                 := "TabStrip1"
      WITH OBJECT :Dock
         :Left    := Self
         :Top     := Self
         :Right   := Self
         :Bottom  := Self
         :Margins := "5,5,5,35"
      END

      :Left   := 5
      :Top    := 5
      :Width  := 453
      :Height := 560
      :Create()
      WITH OBJECT ( TABPAGE( :this ) )
         :Name := "EditorSettings"
         :Text := "&Editor"
         :Create()
         WITH OBJECT ( GROUPBOX( :this ) )
            :Name      := "GroupBox1"
            :Left      := 10
            :Top       := 7
            :Width     := 431
            :Height    := 309
            :Text      := "Colors"
            :ForeColor := 0
            :Create()
            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Background"
               :Left   := 263
               :Top    := 16
               :Width  := 77
               :Height := 22
               :Text   := "Background"
               :EventHandler[ "OnClick" ] := "DefBack_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "NormalText"
               :Left   := 345
               :Top    := 16
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( LABEL( :this ) )
               :Left        := 15
               :Top         := 19
               :Width       := 96
               :Height      := 16
               :Text        := "Default"
               :Transparent := .T.
               :Alignment   := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( Label( :this ) )
               :Name       := "NormalTextEdit"
               :Left       := 124
               :Top        := 17
               :Width      := 129
               :Height     := 22
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Text       := "Normal Text"
               :Create()
            END //EDITBOX

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "SelectedLine"
               :Left   := 263
               :Top    := 43
               :Width  := 77
               :Height := 22
               :Text   := "Background"
               :EventHandler[ "OnClick" ] := "DefBack_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "SelectedLineEdit"
               :Left       := 124
               :Top        := 44
               :Width      := 129
               :Height     := 22
               :Text       := "Normal Text"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left       := 10
               :Top        := 46
               :Width      := 101
               :Height     := 16
               :Text       := "Current Line"
               :Alignment  := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Numbers"
               :Left   := 345
               :Top    := 70
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "NumbersEdit"
               :Left       := 124
               :Top        := 71
               :Width      := 129
               :Height     := 22
               :Text       := "12345678"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left      := 16
               :Top       := 74
               :Width     := 96
               :Height    := 16
               :Text      := "Numbers"
               :Alignment := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Strings"
               :Left   := 345
               :Top    := 96
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "StringsEdit"
               :Left       := 124
               :Top        := 97
               :Width      := 129
               :Height     := 22
               :Text       := '"This is text"'
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left      := 16
               :Top       := 100
               :Width     := 96
               :Height    := 16
               :Text      := "Strings"
               :Alignment := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Comments"
               :Left   := 345
               :Top    := 123
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "CommentsEdit"
               :Left       := 124
               :Top        := 124
               :Width      := 129
               :Height     := 22
               :Text       := "// Comment"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left      := 16
               :Top       := 127
               :Width     := 96
               :Height    := 16
               :Text      := "Comments"
               :Alignment := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Operators"
               :Left   := 345
               :Top    := 149
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "OperatorsEdit"
               :Left       := 124
               :Top        := 150
               :Width      := 129
               :Height     := 22
               :Text       := ":="
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left      := 16
               :Top       := 153
               :Width     := 96
               :Height    := 16
               :Text      := "Operators"
               :Alignment := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Preprocessor"
               :Left   := 345
               :Top    := 175
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "PreprocessorEdit"
               :Left       := 124
               :Top        := 176
               :Width      := 129
               :Height     := 22
               :Text       := "#include"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left      := 16
               :Top       := 179
               :Width     := 96
               :Height    := 16
               :Text      := "Preprocessor"
               :Alignment := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name   := "Keywords1"
               :Left   := 345
               :Top    := 201
               :Width  := 77
               :Height := 22
               :Text   := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "Keywords1Edit"
               :Left       := 124
               :Top        := 202
               :Width      := 129
               :Height     := 22
               :Text       := "DO CASE"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left       := 16
               :Top        := 205
               :Width      := 96
               :Height     := 16
               :Text       := "Keywords 1"
               :Alignment  := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name       := "Keywords2"
               :Left       := 345
               :Top        := 226
               :Width      := 77
               :Height     := 22
               :Text       := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "Keywords2Edit"
               :Left       := 124
               :Top        := 227
               :Width      := 129
               :Height     := 22
               :Text       := "Function/Method"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left       := 16
               :Top        := 230
               :Width      := 96
               :Height     := 16
               :Text       := "Keywords 2"
               :Alignment  := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name       := "Keywords3"
               :Left       := 345
               :Top        := 251
               :Width      := 77
               :Height     := 22
               :Text       := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "Keywords3Edit"
               :Left       := 124
               :Top        := 252
               :Width      := 129
               :Height     := 22
               :Text       := "SendMessage"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left       := 16
               :Top        := 255
               :Width      := 96
               :Height     := 16
               :Text       := "Keywords 3"
               :Alignment  := DT_RIGHT
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name       := "Keywords4"
               :Left       := 345
               :Top        := 276
               :Width      := 77
               :Height     := 22
               :Text       := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( Label( :this ) )
               :Name       := "Keywords4Edit"
               :Left       := 124
               :Top        := 277
               :Width      := 129
               :Height     := 22
               :Text       := "dbEval"
               :Alignment  := DT_CENTER
               :Border     := -1
               :VertCenter := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Left       := 16
               :Top        := 280
               :Width      := 96
               :Height     := 16
               :Text       := "Keywords 4"
               :Alignment  := DT_RIGHT
               :Create()
            END //LABEL

         END //GROUPBOX

         WITH OBJECT ( GROUPBOX( :this ) )
            :Name                 := "GroupBox4"
            :Left                 := 10
            :Top                  := 450
            :Width                := 431
            :Height               := 76
            :Text                 := "Misc"
            :ForeColor            := 0
            :Create()

            WITH OBJECT ( CHECKBOX( :this ) )
               :Name                 := "WrapSearch"
               :Left                 := 10
               :Top                  := 22
               :Width                := 83
               :Height               := 15
               :Text                 := "Wrap Search"
               :State                := ::Application:EditorProps:WrapSearch
               :Create()
            END //CHECKBOX

            WITH OBJECT ( CHECKBOX( :this ) )
               :Name                 := "CaretLine"
               :Left                 := 107
               :Top                  := 22
               :Width                := 109
               :Height               := 15
               :Text                 := "Caret Line Visible"
               :State                := ::Application:SourceEditor:CaretLineVisible
               :Create()
            END //CHECKBOX

            WITH OBJECT ( CHECKBOX( :this ) )
               :Name                 := "AutoIndent"
               :Left                 := 233
               :Top                  := 22
               :Width                := 86
               :Height               := 15
               :Text                 := "Auto Indent"
               :State                := ::Application:SourceEditor:AutoIndent
               :Create()
            END //CHECKBOX

            WITH OBJECT ( CHECKBOX( :this ) )
               :Name                 := "TrimEOLSpaces"
               :Left                 := 335
               :Top                  := 22
               :Width                := 86
               :Height               := 15
               :Text                 := "Trim spaces"
               :State                := ::Application:SourceEditor:TrimEOLSpaces
               :Create()
            END //CHECKBOX


            WITH OBJECT ( LABEL( :this ) )
               :Left                 := 12
               :Top                  := 49
               :Width                := 71
               :Height               := 16
               :Text                 := "Tab Spacing"
               :Create()
            END //LABEL

            WITH OBJECT ( UPDOWN( :this ) )
               :Name                 := "UpDown1"
               :Left                 := 128
               :Top                  := 46
               :Width                := 18
               :Height               := 22
               :Text                 := "UpDown1"
               :Buddy                := "TabSpacing"
               :Create()
            END //UPDOWN

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "TabSpacing"
               :Left                 := 90
               :Top                  := 46
               :Width                := 40
               :Height               := 22
               :Alignment            := 3
               :Number               := .T.
               :Text    := xStr(::Application:SourceEditor:TabWidth)
               :Create()
            END //EDITBOX

         END //GROUPBOX

         WITH OBJECT ( GROUPBOX( :this ) )
            :Name                 := "GroupBox5"
            :Left                 := 10
            :Top                  := 316
            :Width                := 431
            :Height               := 132
            :Text                 := "Font"
            :ForeColor            := 0
            :Create()
            WITH OBJECT ( COMBOBOX( :this ) )
               :Name           := "ComboFontName"
               :Left           := 10
               :Top            := 17
               :Width          := 166
               :Height         := 105
               :DropDownStyle  := 1
               :Action         := {|| ::SetFonts() }
               :VertScroll     := .T.
               :Create()

               aFonts := ::Drawing:EnumFonts()
               ASORT( aFonts,,, {|a,b| a[1]:lfFaceName:AsString() <  b[1]:lfFaceName:AsString() } )
               FOR n := 1 TO LEN( aFonts )
                   :AddItem( aFonts[n][1]:lfFaceName:AsString() )
               NEXT
               IF ( n := :FindString(, ::Application:SourceEditor:FontFaceName ) ) > 0
                  :SetCurSel(n)
               ENDIF

            END //COMBOBOX

            WITH OBJECT ( COMBOBOX( :this ) )
               :Name          := "ComboFontStyle"
               :Left          := 191
               :Top           := 17
               :Width         := 157
               :Height        := 105
               :DropDownStyle := 1
               :ItemHeight    := 15
               :Action        := {|| ::SetFonts() }
               :Create()
               :AddItem( "Regular" )
               :AddItem( "Italic" )
               :AddItem( "Bold" )
               :AddItem( "Bold Italic" )
               n := 1
               IF ::Application:SourceEditor:FontItalic
                  n := 2
               ENDIF
               IF ::Application:SourceEditor:FontBold
                  n := 3
               ENDIF
               IF ::Application:SourceEditor:FontItalic .AND. ::Application:SourceEditor:FontBold
                  n := 4
               ENDIF
               :SetCurSel(n)
            END //COMBOBOX

            WITH OBJECT ( COMBOBOX( :this ) )
               :Name          := "ComboFontSize"
               :Left          := 362
               :Top           := 17
               :Width         := 56
               :Height        := 105
               :DropDownStyle := 1
               :ItemHeight    := 15
               :VertScroll    := .T.
               :Action        := {|o,cText| IIF(cText==NIL, cText := o:GetSelString(),),;
                                            ::SetFonts(,,,VAL(cText) ) }
               :Create()
               aFonts := {8,9,10,11,12,14,16,18,20,22,24,26,28,36,48,72}
               FOR n := 1 TO LEN( aFonts )
                   :AddItem( xStr(aFonts[n]) )
               NEXT

               IF ( n := :FindString(, xStr(::Application:SourceEditor:FontSize) ) ) > 0
                  :SetCurSel(n)
                ELSE
                  SetWindowText( :hEdit, xStr(::Application:SourceEditor:FontSize) )
               ENDIF
            END //COMBOBOX

         END //GROUPBOX

      END //TABPAGE
      WITH OBJECT ( TABPAGE( :this ) )
         :Name := "GeneralSettings"
         :Text := "&General"
         :Create()

         WITH OBJECT ( LABEL( :this ) )
            :Left   := 12
            :Top    := 49
            :Width  := 71
            :Height := 16
            :Text   := "Grid Size"
            :Create()
         END //LABEL

         WITH OBJECT ( UPDOWN( :this ) )
            :Name   := "UpDown2"
            :Left   := 128
            :Top    := 46
            :Width  := 18
            :Height := 22
            :Buddy  := "GridSize"
            :Create()
         END //UPDOWN

         WITH OBJECT ( EDITBOX( :this ) )
            :Name      := "GridSize"
            :Left      := 90
            :Top       := 46
            :Width     := 40
            :Height    := 22
            :Alignment := 3
            :Number    := .T.
            :Text      := xStr( ::Application:IniFile:ReadInteger( "Settings", "GridSize", 4 ) )
            :Create()
         END //EDITBOX

      END

   END //TABSTRIP

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button3"
      WITH OBJECT :Dock
         :Right                := "Button4"
         :Bottom               := Self
         :Margins              := "5,5,5,5"
      END

      :Left                 := 226
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Text                 := "OK"
      :EventHandler[ "OnClick" ] := "Button3_OnClick"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button4"
      WITH OBJECT :Dock
         :Right                := "Button5"
         :Bottom               := Self
         :Margins              := "5,5,5,5"
      END

      :Left                 := 304
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Text                 := "Cancel"
      :EventHandler[ "OnClick" ] := "Close"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button5"
      WITH OBJECT :Dock
         :Right                := Self
         :Bottom               := Self
         :Margins              := "5,5,5,5"
      END

      :Left                 := 383
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Text                 := "&Apply"
      :EventHandler[ "OnClick" ] := "Apply"
      :Create()
   END //BUTTON
RETURN 0

//----------------------------------------------------------------------------------------------------
METHOD DefBack_OnClick( Sender ) CLASS Settings
   LOCAL cColor := "Color"+Sender:Name
   LOCAL cEdit := IIF( Sender:Name == "Background", "NormalText", Sender:Name ) + "Edit"
   ::ColorDialog1:Color := ::Application:SourceEditor:&cColor
   IF ::ColorDialog1:Show()
      ::&cEdit:BackColor := ::ColorDialog1:Color
      IF Sender:Name != "SelectedLine"
         ::NormalTextEdit:BackColor   := ::ColorDialog1:Color
         ::NumbersEdit:BackColor      := ::ColorDialog1:Color
         ::StringsEdit:BackColor      := ::ColorDialog1:Color
         ::CommentsEdit:BackColor     := ::ColorDialog1:Color
         ::OperatorsEdit:BackColor    := ::ColorDialog1:Color
         ::PreprocessorEdit:BackColor := ::ColorDialog1:Color
         ::Keywords1Edit:BackColor    := ::ColorDialog1:Color
         ::Keywords2Edit:BackColor    := ::ColorDialog1:Color
         ::Keywords3Edit:BackColor    := ::ColorDialog1:Color
         ::Keywords4Edit:BackColor    := ::ColorDialog1:Color
      ENDIF
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD DefFore_OnClick( Sender ) CLASS Settings
   LOCAL cColor := "Color"+Sender:Name
   LOCAL cEdit := Sender:Name + "Edit"
   ::ColorDialog1:Color := ::Application:SourceEditor:&cColor
   IF ::ColorDialog1:Show()
      ::&cEdit:ForeColor := ::ColorDialog1:Color
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Settings_OnLoad() CLASS Settings
   LOCAL nBackground := ::Application:SourceEditor:ColorBackground
   ::NormalTextEdit:BackColor   := nBackground
   ::SelectedLineEdit:BackColor := ::Application:SourceEditor:ColorSelectedLine
   ::NumbersEdit:BackColor      := nBackground
   ::StringsEdit:BackColor      := nBackground
   ::CommentsEdit:BackColor     := nBackground
   ::OperatorsEdit:BackColor    := nBackground
   ::PreprocessorEdit:BackColor := nBackground
   ::Keywords1Edit:BackColor    := nBackground
   ::Keywords2Edit:BackColor    := nBackground
   ::Keywords3Edit:BackColor    := nBackground
   ::Keywords4Edit:BackColor    := nBackground

   ::NormalTextEdit:ForeColor   := ::Application:SourceEditor:ColorNormalText
   ::SelectedLineEdit:ForeColor := ::Application:SourceEditor:ColorNormalText
   ::NumbersEdit:ForeColor      := ::Application:SourceEditor:ColorNumbers
   ::StringsEdit:ForeColor      := ::Application:SourceEditor:ColorStrings
   ::CommentsEdit:ForeColor     := ::Application:SourceEditor:ColorComments
   ::OperatorsEdit:ForeColor    := ::Application:SourceEditor:ColorOperators
   ::PreprocessorEdit:ForeColor := ::Application:SourceEditor:ColorPreprocessor
   ::Keywords1Edit:ForeColor    := ::Application:SourceEditor:ColorKeywords1
   ::Keywords2Edit:ForeColor    := ::Application:SourceEditor:ColorKeywords2
   ::Keywords3Edit:ForeColor    := ::Application:SourceEditor:ColorKeywords3
   ::Keywords4Edit:ForeColor    := ::Application:SourceEditor:ColorKeywords4
   ::SetFonts()
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD SetFonts( cName, lBold, lItalic, nSize ) CLASS Settings
   LOCAL oCtrl, aStyle := { {.F.,.F.}, {.T.,.F.}, {.F.,.T.}, {.T.,.T.}}

   DEFAULT cName   TO ::ComboFontName:GetSelString()
   DEFAULT lItalic TO aStyle[::ComboFontStyle:CurSel][1]
   DEFAULT lBold   TO aStyle[::ComboFontStyle:CurSel][2]
   DEFAULT nSize   TO VAL( _GetWindowText( ::ComboFontSize:hEdit ) )

   FOR EACH oCtrl IN ::GroupBox1:Children
       IF oCtrl:__xCtrlName == "Label" .AND. oCtrl:VertCenter
          oCtrl:Font:FaceName  := cName
          oCtrl:Font:PointSize := nSize
          oCtrl:Font:Bold      := lBold
          oCtrl:Font:Italic    := lItalic
          oCtrl:Redraw()
       ENDIF
   NEXT
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Button3_OnClick() CLASS Settings
   ::Apply()
   ::Close()
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Apply() CLASS Settings
   ::Application:IniFile:WriteInteger( "Settings", "GridSize", Val( ::GridSize:Text ) )

   IF ::Application:DesignPage:CtrlMask != NIL
      WITH OBJECT ::Application:DesignPage:CtrlMask
         :xGrid := Val( ::GridSize:Text )
         :yGrid := Val( ::GridSize:Text )
         :InvalidateRect()
      END
      IF ::Application:Project:CurrentForm != NIL
         ::Application:Project:CurrentForm:InvalidateRect()
      ENDIF
   ENDIF

   WITH OBJECT ::Application:SourceEditor
      :StyleSetFont( ::NormalTextEdit:Font:FaceName )
      :StyleSetSize( ::NormalTextEdit:Font:PointSize )
      :StyleSetBold( ::NormalTextEdit:Font:Bold )
      :StyleSetItalic( ::NormalTextEdit:Font:Italic )

      :ColorBackground   := ::NormalTextEdit:BackColor
      :ColorSelectedLine := ::SelectedLineEdit:BackColor
      :ColorNormalText   := ::NormalTextEdit:ForeColor
      :ColorNumbers      := ::NumbersEdit:ForeColor
      :ColorStrings      := ::StringsEdit:ForeColor
      :ColorComments     := ::CommentsEdit:ForeColor
      :ColorOperators    := ::OperatorsEdit:ForeColor
      :ColorPreprocessor := ::PreprocessorEdit:ForeColor
      :ColorKeywords1    := ::Keywords1Edit:ForeColor
      :ColorKeywords2    := ::Keywords2Edit:ForeColor
      :ColorKeywords3    := ::Keywords3Edit:ForeColor
      :ColorKeywords4    := ::Keywords4Edit:ForeColor
      :TabWidth          := VAL( ::TabSpacing:Text    )
      :CaretLineVisible  := ::CaretLine:GetState()
      :AutoIndent        := ::AutoIndent:GetState()
      :TrimEOLSpaces     := ::TrimEOLSpaces:GetState()

      ::Application:EditorProps:WrapSearch := ::WrapSearch:GetState()
      ::Application:Props:WrapSearchItem:Checked := ::WrapSearch:GetState()==1

      :StyleSetBack( STYLE_DEFAULT, :ColorBackground )
      :StyleSetFore( STYLE_DEFAULT, :ColorNormalText )
      :StyleClearAll()
      :SetColors()

      ::Application:EditorPage:TabBackColor := :ColorBackground
      ::Application:EditorPage:TabTextColor := :ColorNormalText
      ::Application:MainTab:Redraw()
   END
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------

CLASS GotoDialog INHERIT Dialog
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()
   METHOD Go_OnClick()
ENDCLASS

METHOD Init( oParent, aParameters ) CLASS GotoDialog
   ::Super:Init( oParent, aParameters )
   ::Name                 := "GotoDialog"
   ::Modal                := .T.
   ::Left                 := 10
   ::Top                  := 10
   ::Width                := 160
   ::Height               := 120
   ::Text                 := "Go To"
   ::MaximizeBox          := .F.
   ::MinimizeBox          := .F.
   ::DlgModalFrame        := .T.
   ::ThickFrame           := .F.
   ::Center               := .T.
   ::Create()
RETURN Self

METHOD OnInitDialog() CLASS GotoDialog
   WITH OBJECT ( LABEL( Self ) )
      :Left                 := 10
      :Top                  := 18
      :Width                := 72
      :Height               := 16
      :Text                 := "Line Number"
      :Create()
   END //LABEL

   DEFAULT ::Application:SourceEditor:Source:nPrevLine TO ::Application:SourceEditor:Source:GetCurLine()+1

   WITH OBJECT ( UPDOWN( Self ) )
      :Name                 := "UpDown1"
      :Left                 := 142
      :Top                  := 15
      :Width                := 18
      :Height               := 22
      :Text                 := ""
      :Buddy                := "LineNum"
      :Create()
   END //UPDOWN

   WITH OBJECT ( EDITBOX( Self ) )
      :Name                 := "LineNum"
      :Left                 := 90
      :Top                  := 15
      :Width                := 55
      :Height               := 22
      :Alignment            := 3
      :Number               := .T.
      :Text                 := xStr(::Application:SourceEditor:Source:nPrevLine)
      :Create()
   END //EDITBOX

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Go"
      :Left                 := 6
      :Top                  := 67
      :Width                := 70
      :Height               := 25
      :Text                 := "&Go to"
      :DefaultButton        := .T.
      :Dock:Bottom          := Self
      :Dock:Left            := Self
      :Dock:Margin          := 3
      :EventHandler[ "OnClick" ] := "Go_OnClick"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button2"
      :Left                 := 94
      :Top                  := 67
      :Width                := 70
      :Height               := 25
      :Text                 := "&Cancel"
      :EventHandler[ "OnClick" ] := "Close"
      :Dock:Bottom          := Self
      :Dock:Right           := Self
      :Dock:Margin          := 3
      :Create()
   END //BUTTON
RETURN 1

METHOD Go_OnClick() CLASS GotoDialog
   ::Application:SourceEditor:Source:GoToLine( VAL( ::LineNum:Text    )-1 )
   ::Close()
   ::Application:SourceEditor:SetFocus()
RETURN Self

CLASS FindReplace INHERIT WinForm
   // Components declaration
   METHOD Init() CONSTRUCTOR

   // Event declaration
   METHOD Next_OnClick()
   METHOD Replace_OnClick()
   METHOD All_OnClick()

ENDCLASS

METHOD Init( oParent, aParameters ) CLASS FindReplace
   LOCAL aRect

   ::Super:Init( oParent, aParameters )

   aRect := _GetWindowRect( oParent:hWnd )

   ::Name                 := "Form1"
   ::Left                 := aRect[1] + 16
   ::Top                  := aRect[2] + 17
   ::Width                := 446
   ::Height               := 153
   ::Text                 := "Replace"
   ::Resizable            := .F.
   ::MaximizeBox          := .F.
   ::MinimizeBox          := .F.
   ::DlgModalFrame        := .T.

   ::Create()

   // Populate Children
   WITH OBJECT ( EDITBOX( Self ) )
      :Name                 := "FindWhat"
      :Left                 := 93
      :Top                  := 9
      :Width                := 244
      :Text                 := ::Parent:Source:GetSelText()
      :OnWMKeyUp            := <|o,nKey|
                                 IF nKey == 27
                                    o:Parent:Close()
                                  ELSE
                                    o:Parent:Next:Enabled := ;
                                    o:Parent:Replace:Enabled := ;
                                    o:Parent:All:Enabled := ! Empty(o:Text   )
                                 ENDIF
                               >
      :Create()
      :SetFocus()
   END //COMBOBOX

   WITH OBJECT ( EDITBOX( Self ) )
      :Name                 := "ReplaceWith"
      :Left                 := 93
      :Top                  := 34
      :Width                := 244
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //COMBOBOX

   WITH OBJECT ( CHECKBOX( Self ) )
      :Name                 := "WholeWord"
      :Left                 := 15
      :Top                  := 62
      :Width                := 150
      :Height               := 15
      :Text                 := "Match Whole &Word"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //CHECKBOX

   WITH OBJECT ( CHECKBOX( Self ) )
      :Name                 := "MatchCase"
      :Left                 := 15
      :Top                  := 81
      :Width                := 100
      :Height               := 15
      :Text                 := "Match &Case"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //CHECKBOX

   WITH OBJECT ( CHECKBOX( Self ) )
      :Name                 := "Global"
      :Left                 := 15
      :Top                  := 100
      :Width                := 100
      :Height               := 15
      :Text                 := "&Global Search"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //CHECKBOX

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Next"
      :Left                 := 346
      :Top                  := 8
      :Width                := 80
      :Height               := 24
      :Enabled              := .F.
      :Text                 := "Find &Next"
      :DefaultButton        := .T.
      :EventHandler[ "OnClick" ] := "Next_OnClick"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Replace"
      :Left                 := 346
      :Top                  := 34
      :Width                := 80
      :Enabled              := .F.
      :Height               := 24
      :Text                 := "&Replace"
      :EventHandler[ "OnClick" ] := "Replace_OnClick"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "All"
      :Left                 := 346
      :Top                  := 60
      :Width                := 80
      :Height               := 24
      :Enabled              := .F.
      :Text                 := "Replace &All"
      :EventHandler[ "OnClick" ] := "All_OnClick"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Cancel"
      :Left                 := 346
      :Top                  := 86
      :Width                := 80
      :Height               := 24
      :Text                 := "Cancel"
      :EventHandler[ "OnClick" ] := "Close"
      :OnWMKeyUp            := {|o,nKey| IIF( nKey==27, o:Parent:Close(),)}
      :Create()
   END //BUTTON

   WITH OBJECT ( LABEL( Self ) )
      :Left                 := 15
      :Top                  := 12
      :Width                := 72
      :Height               := 16
      :Text                 := "&Find"
      :Create()
   END //LABEL

   WITH OBJECT ( LABEL( Self ) )
      :Left                 := 15
      :Top                  := 37
      :Width                := 70
      :Height               := 16
      :Text                 := "&Replace with"
      :Create()
   END //LABEL

   ::Show()
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Next_OnClick() CLASS FindReplace
   LOCAL nFlags := 0
   ::Parent:cFindWhat  := ::FindWhat:Text

   IF ::MatchCase:Checked()
      nFlags := nFlags | SCFIND_MATCHCASE
   ENDIF
   IF ::WholeWord:Checked()
      nFlags := nFlags | SCFIND_WHOLEWORD
   ENDIF
   ::Parent:Source:SetSearchFlags( nFlags )
   ::Parent:FindNext( ::FindWhat:Text   , .F. )
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD Replace_OnClick() CLASS FindReplace
   LOCAL nFlags := 0
   IF ::MatchCase:Checked()
      nFlags := nFlags | SCFIND_MATCHCASE
   ENDIF
   IF ::WholeWord:Checked()
      nFlags := nFlags | SCFIND_WHOLEWORD
   ENDIF
   IF ::Parent:Source:GetSelLen() == 0
      IF ! EMPTY( ::FindWhat:Text    )
         ::Parent:Source:SearchNext( nFlags, ::FindWhat:Text    )
      ENDIF
    ELSEIF ::Parent:Source:GetSelLen() > 0
      ::Parent:Source:ReplaceSel( ::ReplaceWith:Text    )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD All_OnClick() CLASS FindReplace
   LOCAL oSource, n, nFlags := 0
   IF ::MatchCase:Checked()
      nFlags := nFlags | SCFIND_MATCHCASE
   ENDIF
   IF ::WholeWord:Checked()
      nFlags := nFlags | SCFIND_WHOLEWORD
   ENDIF
   IF ::Global:Checked()
      oSource := ::Parent:Source
      FOR n := 1 TO LEN( ::Parent:aDocs )
          ::Parent:aDocs[n]:Select()
          ::Parent:aDocs[n]:ReplaceAll( ::FindWhat:Text   , ::ReplaceWith:Text   , nFlags, ::Parent:InSelection )
      NEXT
      oSource:Select()
    ELSE
      ::Parent:Source:ReplaceAll( ::FindWhat:Text   , ::ReplaceWith:Text   , nFlags, ::Parent:InSelection )
   ENDIF
RETURN Self

//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------

CLASS ProjectFile
   DATA TreeItem  EXPORTED
   DATA File      EXPORTED INIT ""
   DATA FileName  EXPORTED
   DATA lSource   EXPORTED INIT .F.
   DATA Form      EXPORTED
   DATA Path      EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Close() INLINE .T.
ENDCLASS

METHOD Init( cFile ) CLASS ProjectFile
   ::File := cFile
RETURN Self

//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------

#pragma BEGINDUMP

#include <windows.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#include "item.api"
#include "hbdefs.h"
#include "hbvmpub.h"
#include "hbinit.h"
#include "hbapi.h"
#include "hbfast.h"
#include "hbvm.h"
#include "hbapierr.h"
#include "hbpcode.h"
#include "hbstack.h"

#define SCI_GETDIRECTFUNCTION 2184
#define SCI_GETDIRECTPOINTER 2185

static HMODULE hSciLib = NULL;
static int (*fn)(void*,int,int,int);
static void * ptr;
static HWND hEditor;
typedef VOID (WINAPI *STSPACES)(void);

//--------------------------------------------------------------------------------------------------------
HB_FUNC_INIT( SCILOAD )
{
   hSciLib = LoadLibrary( "SciLexer.dll" );
}

HB_FUNC_EXIT( SCIFREE )
{
   FreeLibrary( hSciLib );
   hSciLib = NULL;
}

HB_FUNC( SCI_INIT )
{
   hEditor = (HWND) hb_parnl(1);
   fn = (int (__cdecl *)(void *,int,int,int)) SendMessage( hEditor,SCI_GETDIRECTFUNCTION,0,0);
   ptr = (void *) SendMessage(hEditor,SCI_GETDIRECTPOINTER,0,0);
}

//-------------------------------------------------------------------------------------------------
static PHB_ITEM hb_pureparam( int iParam, LONG iMask )
{
   HB_THREAD_STUB

   if( ( iParam >= 0 && iParam <= hb_pcount() ) )
   {
      PHB_ITEM pItem = hb_stackItemFromBase( iParam  );

      if( ( HB_TYPE ) iMask == HB_IT_ANY || pItem->type & ( HB_TYPE ) iMask )
      {
         return pItem;
      }
      else
      {
         if( (HB_TYPE) iMask == HB_IT_NUMERIC && HB_IS_NUMERIC( pItem ) )
         {
            return pItem;
         }
      }
   }

   return NULL;
}

HB_FUNC( SCI_SEND )
{
   BOOL bByRef = FALSE;
   LPARAM lParam;
   WPARAM wParam;

   PHB_ITEM pLPARAM;
   PHB_ITEM pWPARAM;

   if( IsWindow( hEditor ) )
   {
      if( ISCHAR(2) )
      {
         pWPARAM = hb_pureparam( 2, HB_IT_ANY );
         wParam = (WPARAM) hb_itemGetCPtr( pWPARAM );
      }
      else
      {
         wParam = ISNIL(2) ? 0 : hb_parni(2);
      }

      if( ISNIL(3) )
      {
         lParam = 0;
      }
      else
      {
         pLPARAM = hb_pureparam( 3, HB_IT_ANY );
         if( pLPARAM )
         {
            if( HB_IS_BYREF( pLPARAM ) )
            {
               bByRef = TRUE;
               pLPARAM = hb_itemUnRef( pLPARAM );
            }

            if( HB_IS_STRING( pLPARAM ) )
            {
               lParam = (LPARAM) hb_itemGetCPtr( pLPARAM );
            }
            else if( HB_IS_NUMERIC( pLPARAM ) )
            {
               lParam = hb_itemGetNL( pLPARAM );
            }
            else if( HB_IS_LOGICAL( pLPARAM ) )
            {
               lParam = (LPARAM) hb_itemGetL( pLPARAM );
            }
            else if( HB_IS_NIL( pLPARAM ) )
            {
               lParam = 0;
            }
         }
      }

      hb_retnl( (long) fn(ptr,hb_parni(1),wParam,lParam) );

      if( bByRef )
      {
         if( HB_IS_STRING( pLPARAM ) )
         {
            hb_storc( (char *) lParam, 3 );
         }
         else if( HB_IS_NUMERIC( pLPARAM ) )
         {
            hb_stornl( lParam, 3 );
         }
         else if( HB_IS_NIL( pLPARAM ) )
         {
            hb_stornl( lParam, 3 );
         }
      }
   }
}

