/*
 * $Id$
 */

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"
#include "Scintilla.ch"
#include "SciLexer.ch"
#include "fileio.ch"

#define MARGIN_SCRIPT_FOLD_INDEX  2


#define EOL Chr(13) + Chr(10)

//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------

CLASS SourceEditor INHERIT Control
   DATA hSciLib  PROTECTED
   DATA xSource  PROTECTED

   ASSIGN Source(o) INLINE IIF( ::xSource != NIL, ::xSource:SavePos(),), IIF( o != NIL, o:Select(),)
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
   DATA ColorBackGround   EXPORTED

   DATA ColorNumbers      EXPORTED
   DATA ColorStrings      EXPORTED
   DATA ColorComments     EXPORTED
   DATA ColorOperators    EXPORTED
   DATA ColorPreprocessor EXPORTED

   DATA ColorKeywords1    EXPORTED
   DATA ColorKeywords2    EXPORTED
   DATA ColorKeywords3    EXPORTED
   DATA ColorKeywords4    EXPORTED

   DATA cFindWhat         EXPORTED
   DATA nDirection        EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD StyleSetFont( cFont )           INLINE ::SendMessage( SCI_STYLESETFONT, STYLE_DEFAULT, cFont )
   METHOD StyleGetFont( cFont )           INLINE cFont := SPACE(255), ::SendMessage( SCI_STYLEGETFONT, STYLE_DEFAULT, @cFont ), ALLTRIM(cFont)

   METHOD StyleSetSize( nSize )           INLINE ::SendMessage( SCI_STYLESETSIZE, STYLE_DEFAULT, nSize )
   METHOD StyleGetSize( nSize )           INLINE ::SendMessage( SCI_STYLEGETSIZE, STYLE_DEFAULT, @nSize ), nSize

   METHOD SetLexer( nLexer )              INLINE ::SendMessage( SCI_SETLEXER, nLexer, 0 )   

   METHOD SelectDocument( pDoc )          INLINE ::SendMessage( SCI_SETDOCPOINTER, 0, pDoc )
   METHOD OnDestroy()
   METHOD OnParentNotify()
   METHOD OnSetFocus()                    INLINE ::SendMessage( SCI_SETFOCUS, 1, 0 )

   METHOD StyleSetFore( nStyle, nColor )  INLINE ::SendMessage( SCI_STYLESETFORE, nStyle, nColor )
   METHOD StyleSetBack( nStyle, nColor )  INLINE ::SendMessage( SCI_STYLESETBACK, nStyle, nColor )
   METHOD OnTimer()                       INLINE ::CallWindowProc(), 0
  
   METHOD OnFindNext()
   METHOD OnReplace()
   METHOD OnReplaceAll()
   
   METHOD GetSearchFlags()
   METHOD FindNext()
   METHOD EnsureRangeVisible()
   METHOD OnLButtonUp()
   METHOD OnKeyUp()
   METHOD OnKeyDown()
   METHOD InitLexer()
ENDCLASS

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS SourceEditor
   LOCAL nKey, cKey, n, cText, cSyntax := ::Application:Path + "\vxh.syn"
   ::hSciLib := LoadLibrary( "SciLexer212.dll" )
   ::__xCtrlName := "SourceEditor"
   ::ClsName := "Scintilla"
   ::Super:Init( oParent )
   ::Style := WS_CHILD | WS_TABSTOP | WS_CLIPCHILDREN
   ::EventHandler[ "OnFindNext" ]   := "OnFindNext"
   ::EventHandler[ "OnReplace" ]    := "OnReplace"
   ::EventHandler[ "OnReplaceAll" ] := "OnReplaceAll"
   
   IF FILE( cSyntax )
      cText := MemoRead( cSyntax )
      nKey  := 2
      WHILE ( n := AT( "[Keywords"+xStr(nKey)+"]", cText ) ) > 0
         cKey := "Keywords"+xStr(nKey-1)
         __objSendMsg( Self, "_"+cKey, SUBSTR( cText, 13, n-1 ) )
         nKey++
      ENDDO
   ENDIF

   ::ColorNormalText   := ::Application:IniFile:ReadInteger( "Colors", "NormalText",   ::System:Color:Black          )
   ::ColorBackGround   := ::Application:IniFile:ReadInteger( "Colors", "BackGround",   ::System:Color:White          ) 

   ::ColorNumbers      := ::Application:IniFile:ReadInteger( "Colors", "Numbers",      ::System:Color:Green          )
   ::ColorStrings      := ::Application:IniFile:ReadInteger( "Colors", "Strings",      ::System:Color:Teal           )
   ::ColorComments     := ::Application:IniFile:ReadInteger( "Colors", "Comments",     ::System:Color:Gray           )
   ::ColorOperators    := ::Application:IniFile:ReadInteger( "Colors", "Operators",    ::System:Color:DarkOliveGreen )
   ::ColorPreprocessor := ::Application:IniFile:ReadInteger( "Colors", "Preprocessor", ::System:Color:Tomato         )

   ::ColorKeywords1    := ::Application:IniFile:ReadInteger( "Colors", "Keywords1",    ::System:Color:Maroon         )
   ::ColorKeywords2    := ::Application:IniFile:ReadInteger( "Colors", "Keywords2",    ::System:Color:DarkCyan       )
   ::ColorKeywords3    := ::Application:IniFile:ReadInteger( "Colors", "Keywords3",    ::System:Color:SteelBlue      )
   ::ColorKeywords4    := ::Application:IniFile:ReadInteger( "Colors", "Keywords4",    ::System:Color:DarkSlateGray  ) 
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS SourceEditor
   ::Super:Create()

   ::SendMessage( SCI_SETLEXER, SCLEX_FLAGSHIP )
   ::SendMessage( SCI_SETSTYLEBITS, 7)
   
   //SciInitFunc( ::hWnd )

   ::StyleSetBack( STYLE_DEFAULT, ::ColorBackground )
   ::StyleSetFore( STYLE_DEFAULT, ::ColorNormalText )

   ::StyleSetFont( "FixedSys" )
   ::StyleSetSize( 10 )

   ::SendMessage( SCI_STYLECLEARALL, 0, 0 )
   ::InitLexer()
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD InitLexer() CLASS SourceEditor
   local cFold := space(1)

   SciSetProperty( ::hWnd, "fold", "1" )
   SciSetProperty( ::hWnd, "fold.compact", "0" )
   SciSetProperty( ::hWnd, "fold.comment", "1" )
   SciSetProperty( ::hWnd, "fold.preprocessor", "0" )
   SciSetProperty( ::hWnd, "fold.directive", "0" )

   //SciSetFold( ::hWnd, "fold", "0" )

   SciSetKeywords( ::hWnd, 0, ::Keywords1 )
   SciSetKeywords( ::hWnd, 1, ::Keywords2 )
   SciSetKeywords( ::hWnd, 2, ::Keywords3 )
   SciSetKeywords( ::hWnd, 3, ::Keywords4 )

   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD,        ::ColorKeywords1 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD2,       ::ColorKeywords2 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD3,       ::ColorKeywords3 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD4,       ::ColorKeywords4 )


   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENT,        ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTLINE,    ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTDOC,     ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTLINEDOC, ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_OPERATOR,       ::ColorOperators )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_STRING,         ::ColorStrings   )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_NUMBER,         ::ColorNumbers   )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_PREPROCESSOR,   ::ColorPreprocessor )

   ::SendMessage( SCI_SETCARETFORE, ::ColorNormalText )
   ::SendMessage( SCI_SETCARETPERIOD, 500, 0 )
   
   ::SendMessage( SCI_SETCARETWIDTH, 2 )
   ::SendMessage( SCI_SETCARETSTYLE, 1 )

   ::SendMessage( SCI_SETMARGINWIDTHN, 1, 15 )
   ::SendMessage( SCI_SETMARGINTYPEN,  MARGIN_SCRIPT_FOLD_INDEX, SC_MARGIN_SYMBOL )
   ::SendMessage( SCI_SETMARGINWIDTHN, MARGIN_SCRIPT_FOLD_INDEX, 15 )
   ::SendMessage( SCI_SETMARGINMASKN,  MARGIN_SCRIPT_FOLD_INDEX, SC_MASK_FOLDERS )
   ::SendMessage( SCI_SETMARGINSENSITIVEN, MARGIN_SCRIPT_FOLD_INDEX, 1 )

   //::SendMessage( SCI_SETEDGEMODE, 1 )

   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDER,        SC_MARK_PLUS  )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPEN,    SC_MARK_MINUS )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERSUB,     SC_MARK_EMPTY    )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY    )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY    )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERTAIL,    SC_MARK_EMPTY    )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEREND,     SC_MARK_EMPTY  )

   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDER,        RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPEN,    RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPENMID, RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERSUB,     RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERMIDTAIL, RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERTAIL,    RGB( 255, 255, 255 ) )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEREND,     RGB( 255, 255, 255 ) )

   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDER,        RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPEN,    RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPENMID, RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERSUB,     RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERMIDTAIL, RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERTAIL,    RGB( 128, 128, 128 ) )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEREND,     RGB( 128, 128, 128 ) )

   ::SendMessage( SCI_SETFOLDFLAGS, 16, 0)

   //::SendMessage( SCI_SETINDENTATIONGUIDES, 1, 0)
   //::SendMessage( SCI_SETHIGHLIGHTGUIDE, 30, 0)

   ::SendMessage( SCI_SETMODEVENTMASK, SC_MOD_CHANGEFOLD | SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT | SC_PERFORMED_UNDO | SC_PERFORMED_REDO )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS SourceEditor
   ::Application:Project:EditReset()
RETURN NIL

METHOD OnKeyDown( nKey ) CLASS SourceEditor
   IF nKey == VK_F3
      IF ::Source:GetSelLen() > 1
         ::cFindWhat := ::Source:GetSelText()
      ENDIF
      ::FindNext( ::cFindWhat, CheckBit( GetKeyState( VK_SHIFT ), 32768 ) )
   ENDIF
RETURN NIL

METHOD OnKeyUp( nKey ) CLASS SourceEditor
   IF nKey == VK_SHIFT
      ::Application:Project:EditReset()
    ELSEIF ::Application:Props:EditCopyItem:Enabled
      ::Application:Props:EditCopyItem:Enabled := .F.
      ::Application:Props:EditCopyBttn:Enabled := .F.
      ::Application:Props:EditCutItem:Enabled := .F.
      ::Application:Props:EditCutBttn:Enabled := .F.
      ::Application:Props:EditDelBttn:Enabled := .F.
   ENDIF
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS SourceEditor
   LOCAL scn, n, cText, nLine, cObj, nChar, nPosStart, nPosEnd, oObj, aProperties, aProperty, aProp, cList
   (nwParam, nlParam)
   DO CASE
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

                    cText := ::Application:SourceTabs:GetItemText(n)
                    cText := ALLTRIM( STRTRAN( cText, "*" ) )

                    ::Application:SourceTabs:SetItemText( n, " " + cText + " * ", .T. )
                    ::Application:Project:Modified := .T.
                 ENDIF
              ENDIF
              ::Application:Project:SetEditMenuItems()
           ENDIF

      CASE hdr:code == SCN_MARGINCLICK
           scn := (struct SCNOTIFICATION*) nlParam
           IF scn:margin == MARGIN_SCRIPT_FOLD_INDEX
              nLine := ::Source:LineFromPosition( scn:position )
              ::SendMessage( SCI_TOGGLEFOLD, nLine, 0 )
              ::SendMessage( SCI_ENSUREVISIBLEENFORCEPOLICY, nLine )
              ::SendMessage( SCI_GOTOLINE, nLine )
           ENDIF

      CASE hdr:code == SCN_CHARADDED
           scn := (struct SCNOTIFICATION*) nlParam
           IF scn:ch == 58
              IF ::SendMessage( SCI_AUTOCACTIVE ) > 0
                 ::SendMessage( SCI_AUTOCCANCEL )
              ENDIF
              nPosEnd   := ::Source:GetCurrentPos()-1
              nPosStart := ::Source:PositionFromLine( ::Source:LineFromPosition( nPosEnd ) )
              cObj := ""
              FOR n := nPosEnd TO nPosStart STEP -1
                  nChar := ::Source:GetCharAt(n)
                  IF nChar == 32 .OR. ::Source:GetColumn(n)==0
                     EXIT
                  ENDIF
                  cObj := CHR(nChar) + cObj
              NEXT

              IF LEN( cObj ) >= 2
                 IF LEFT(cObj,2) == "::"
                    IF LEN(cObj) > 2
                       cObj := "WinForm():"+SUBSTR(cObj,3)
                     ELSE
                       cObj := "WinForm()"
                    ENDIF
                 ENDIF
                 IF cObj[-1] == ":"
                    cObj := LEFT( cObj, LEN(cObj)-1 )
                 ENDIF
                 oObj := &cObj

                 aProperties := __ClsGetPropertiesAndValues( oObj )
                 aSort( aProperties,,,{|x, y| x[1] < y[1]})

                 cList := ""
                 FOR EACH aProperty IN aProperties
                     aProp := GetProperCase( __Proper( aProperty[1] ) )
                     cList += aProp[1]+" "
                 NEXT
                 ::SendMessage( SCI_AUTOCSHOW, 0, cList )
              ENDIF
           ENDIF
   ENDCASE
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnDestroy() CLASS SourceEditor
   ::Application:IniFile:WriteInteger( "Settings", "WrapSearch", ::Application:EditorProps:WrapSearch )

   ::Application:IniFile:WriteInteger( "Colors", "NormalText",   ::ColorNormalText )
   ::Application:IniFile:WriteInteger( "Colors", "BackGround",   ::ColorBackGround )

   ::Application:IniFile:WriteInteger( "Colors", "Numbers",      ::ColorNumbers )
   ::Application:IniFile:WriteInteger( "Colors", "Strings",      ::ColorStrings )
   ::Application:IniFile:WriteInteger( "Colors", "Comments",     ::ColorComments )
   ::Application:IniFile:WriteInteger( "Colors", "Operators",    ::ColorOperators )
   ::Application:IniFile:WriteInteger( "Colors", "Preprocessor", ::ColorPreprocessor )

   ::Application:IniFile:WriteInteger( "Colors", "Keywords1",    ::ColorKeywords1 )
   ::Application:IniFile:WriteInteger( "Colors", "Keywords2",    ::ColorKeywords2 )
   ::Application:IniFile:WriteInteger( "Colors", "Keywords3",    ::ColorKeywords3 )
   ::Application:IniFile:WriteInteger( "Colors", "Keywords4",    ::ColorKeywords4 )
   aEval( ::aDocs, {|oDoc| oDoc:Close() } )
   FreeLibrary( ::hSciLib )
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
   ::cFindWhat  := oFind:FindWhat
   ::nDirection := oFind:Direction

   ::Source:SetSearchFlags( ::GetSearchFlags( oFind ) )
   ::FindNext( oFind:FindWhat, oFind:Direction == 0 )
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
   IF ::Source:GetSelLen()-1 == 0
      IF ! EMPTY( oFind:FindWhat )
         ::Source:SearchNext( ::GetSearchFlags( oFind ), oFind:FindWhat )
      ENDIF
    ELSEIF ::Source:GetSelLen()-1 > 0
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

// Compatibility with xedit for debugger ------------------------------------------------
FUNCTION xEdit_GetEditors(); RETURN __GetApplication():SourceEditor:aDocs

CLASS Editor 
   METHOD New( nTop, nLeft, nLines, nColumns, cFile, oDisplay )
ENDCLASS

METHOD New( nTop, nLeft, nLines, nColumns, cFile, oDisplay ) CLASS Editor 
   LOCAL oSource := Source( __GetApplication():SourceEditor )
   (nTop, nLeft, nLines, nColumns, oDisplay)
   IF ! EMPTY( cFile )
      oSource:Open( cFile )
   ENDIF
RETURN oSource

CLASS EditorDisplay
   METHOD Display() INLINE NIL
ENDCLASS
// --------------------------------------------------------------------------------------

CLASS Source
   ACCESS Application INLINE ::Owner:Application
   DATA pSource   EXPORTED
   DATA Owner     EXPORTED
   DATA Path      EXPORTED
   DATA FileName  EXPORTED
   DATA File      EXPORTED INIT ""
   DATA Modified  EXPORTED INIT .F.
   DATA FirstOpen EXPORTED INIT .T.
   DATA SavedPos  EXPORTED INIT 0
   DATA Extension EXPORTED INIT "prg"
   DATA lStyled   EXPORTED INIT .T.
   DATA cObj      EXPORTED INIT ""
   // Compatibility with xedit for debugger ------------------------------------------------
   ACCESS cFile             INLINE ::FileName
   ACCESS cPath             INLINE IIF( ! EMPTY(::Path), ::Path + "\", "" )
   ACCESS nLine             INLINE ::GetCurLine()
   ACCESS lReadOnly         INLINE ::GetReadOnly()
   ASSIGN lReadOnly(lSet)   INLINE ::SetReadOnly(lSet)
   DATA HighlightedLine     EXPORTED
   DATA oDisplay            EXPORTED INIT EditorDisplay()
   METHOD Load(cFile,cText) INLINE IIF( ! EMPTY(cFile), ::Open(cFile), ::SetText(cText) )
   METHOD GoLine(n)         INLINE ::GoToLine(n)
   METHOD Highlight()       INLINE ::HighlightedLine := ::GetCurLine()
   // --------------------------------------------------------------------------------------

   METHOD Init( oOwner, cFile ) CONSTRUCTOR

   METHOD Open()
   METHOD Close()
   METHOD Save()
   METHOD GetText()

   METHOD SavePos()                           INLINE ::SavedPos := ::GetCurrentPos()
   METHOD GetCurDoc()                         INLINE ::Owner:SendMessage( SCI_GETDOCPOINTER, 0, 0 )
   METHOD ReleaseDocument()                   INLINE ::Owner:SendMessage( SCI_RELEASEDOCUMENT, 0, ::pSource )
   METHOD Select()                            INLINE ::Owner:SendMessage( SCI_SETDOCPOINTER, 0, ::pSource ), ::Owner:xSource := Self, ::GotoPosition( ::SavedPos )
   METHOD CreateDocument()                    INLINE ::Owner:SendMessage( SCI_CREATEDOCUMENT, 0, 0 )

   METHOD GotoPosition( nPos )                INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GOTOPOS, nPos, 0 )
   METHOD GotoLine( nLine )                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GOTOLINE, nLine, 0 )
   METHOD EmptyUndoBuffer()                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_EMPTYUNDOBUFFER, 0, 0 )
   METHOD CanUndo()                           INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CANUNDO, 0, 0 )==1
   METHOD CanRedo()                           INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CANREDO, 0, 0 )==1
   METHOD Undo()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_UNDO, 0, 0 )//, ::Application:Project:SetEditMenuItems()
   METHOD Redo()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_REDO, 0, 0 )//, ::Application:Project:SetEditMenuItems()
   METHOD Copy()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_COPY, 0, 0 )
   METHOD Paste()                             INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_PASTE, 0, 0 )//, ::Application:Project:SetEditMenuItems()
   METHOD Cut()                               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CUT, 0, 0 )//, ::Application:Project:SetEditMenuItems()
   METHOD CanPaste()                          INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CANPASTE, 0, 0 )==1
   METHOD SetReadOnly( nSet )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETREADONLY, nSet, 0 )
   METHOD GetReadOnly()                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETREADONLY, 0, 0 )
   METHOD GetCurrentPos()                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETCURRENTPOS, 0, 0 )
   METHOD SetCurrentPos( nPos )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETCURRENTPOS, nPos, 0 )
   METHOD GoToPos( nPos )                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GOTOPOS, nPos, 0 )
   METHOD GetColumn( nPos )                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETCOLUMN, nPos, 0 )

   METHOD LineFromPosition( nPos )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_LINEFROMPOSITION, nPos, 0 )
   METHOD PositionFromLine( nLine )           INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_POSITIONFROMLINE, nLine, 0 )
   METHOD LineLength( nLine )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_LINELENGTH, nLine, 0 )

   METHOD GetCurLine()                        INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_LINEFROMPOSITION, ::GetCurrentPos(), 0 )
   METHOD GetSelectionMode()                  INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONMODE, 0, 0 )
   METHOD GetSelections()                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONS, 0, 0 )

   METHOD GetSelectionStart()                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONSTART, 0, 0 )
   METHOD GetSelectionEnd()                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONEND, 0, 0 )
   METHOD GetSelectionNStart( nSel )          INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONNSTART, nSel, 0 )
   METHOD GetSelectionNEnd( nSel )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELECTIONNEND, nSel, 0 )

   METHOD SetSavePoint()                      INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETSAVEPOINT, 0, 0 )
   METHOD GetTextLen()                        INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETTEXTLENGTH, 0, 0 )
   METHOD BeginUndoAction()                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_BEGINUNDOACTION, 0, 0 )
   METHOD EndUndoAction()                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_ENDUNDOACTION, 0, 0 )

   METHOD ChkDoc()                            INLINE IIF( ::GetCurDoc() != ::pSource, ::Select(),)

   METHOD FindText( nFlags, ttf )             INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_FINDTEXT, nFlags, ttf )
   METHOD SearchNext( nFlags, cText )         INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SEARCHNEXT, nFlags, cText )
   METHOD SearchPrev( nFlags, cText )         INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SEARCHPREV, nFlags, cText )
   METHOD ReplaceSel( cText )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_REPLACESEL, 0, cText )
   METHOD GetSelLen()                         INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELTEXT, 0, 0 )
   METHOD GetSelText( cBuffer )               INLINE ::ChkDoc(), cBuffer := SPACE( ::Owner:SendMessage( SCI_GETSELTEXT, 0, 0 ) ),;
                                                                 ::Owner:SendMessage( SCI_GETSELTEXT, 0, cBuffer ),;
                                                                 ALLTRIM(LEFT(cBuffer,LEN(cBuffer)-1))
   METHOD SetSearchFlags( nFlags )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETSEARCHFLAGS, nFlags )
   METHOD SetTargetStart( nStart )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETTARGETSTART, nStart )
   METHOD SetTargetEnd( nEnd )                INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETTARGETEND, nEnd )
   METHOD ReplaceTarget( nLen, cText)         INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_REPLACETARGET, nLen, cText )
   METHOD SearchInTarget( nLen, cText)        INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SEARCHINTARGET, nLen, cText )
   METHOD GetTargetStart()                    INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETTARGETSTART, 0, 0 )
   METHOD GetTargetEnd()                      INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETTARGETEND, 0, 0 )
   METHOD GetCharAt( nPos )                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETCHARAT, nPos, 0 )
   METHOD PositionAfter( nPos )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_POSITIONAFTER, nPos, 0 )
   METHOD SetSelection( nCaret, nEnd )        INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETSELECTION, nCaret, nEnd )
   METHOD GetStyleAt( nPos )                  INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSTYLEAT, nPos, 0 )
   METHOD GetEndStyled()                      INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETENDSTYLED, 0, 0 )
   METHOD EnsureVisible( nLine )              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_ENSUREVISIBLE, nLine, 0 )
   METHOD EnsureVisibleEnforcePolicy( nLine ) INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_ENSUREVISIBLEENFORCEPOLICY, nLine, 0 )
   METHOD GetLineCount()                      INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETLINECOUNT, 0, 0 )

   METHOD SetTabWidth( nChars )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETTABWIDTH, nChars, 0 )
   METHOD GetTabWidth()                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETTABWIDTH, 0, 0 )
   METHOD SetUseTabs( nUseTabs )              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETUSETABS, nUseTabs, 0 )
   METHOD AppendText( cText )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_APPENDTEXT, Len(cText), cText )
   METHOD UsePopUp( n )                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_USEPOPUP, n )
   METHOD GetLineState( n )                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETLINESTATE, n )

   METHOD SetText()
   METHOD GetLine()
   METHOD FindInPos()
   METHOD ReplaceAll()
   METHOD FindInTarget()
ENDCLASS

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oOwner, cFile ) CLASS Source
   LOCAL n
   ::Owner   := oOwner
   ::pSource := ::CreateDocument()
   
   IF cFile != NIL
      n := RAT( "\", cFile )
      ::FileName := SUBSTR( cFile, n+1 )
      ::Path     := SUBSTR( cFile, 1, n-1 )
      ::File     := cFile
   ENDIF
   AADD( ::Owner:aDocs, Self )
   ::Select()
   ::SetUseTabs(0)
   ::SetTabWidth(3)
   ::UsePopUp(0)
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Close() CLASS Source
   LOCAL n
   IF ( n := ASCAN( ::Owner:aDocs, {|o| o:pSource==::pSource} ) ) > 0
      ::ReleaseDocument()
      ADEL( ::Owner:aDocs, n, .T. )
      IF ( n := ASCAN( ::Owner:aDocs, {|o| o:pSource==::GetCurDoc() } ) ) > 0
         ::Owner:Source := ::Owner:aDocs[n]
      ELSE
         ::Owner:Source := NIL
      ENDIF
      RETURN .T.
   ENDIF
RETURN .F.

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Open( cFile ) CLASS Source
   LOCAL n
   ::SetText( MemoRead( cFile, .F. ) )

   ::SetSavePoint()
   ::GotoPosition( 0 )
   ::EmptyUndoBuffer()
   ::Modified  := .F.
   ::FirstOpen := .F.
   n := RAT( "\", cFile )
   ::FileName := SUBSTR( cFile, n+1 )
   ::Path     := SUBSTR( cFile, 1, n-1 )
   ::File     := cFile
   ::SetSavePoint()
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetText() CLASS Source
   LOCAL cText, nLen
   ::ChkDoc()
   nLen := ::Owner:SendMessage( SCI_GETLENGTH, 0, 0 )+1
   cText := SPACE( nLen )
   ::Owner:SendMessage( SCI_GETTEXT, nLen, @cText )
   cText := STRTRAN( cText, CHR(0) )
RETURN cText

//------------------------------------------------------------------------------------------------------------------------------------
METHOD GetLine( nLine ) CLASS Source
   LOCAL cText, nLen
   ::ChkDoc()
   nLen := ::Owner:SendMessage( SCI_LINELENGTH, nLine, 0 )+1
   cText := SPACE( nLen )
   ::Owner:SendMessage( SCI_GETLINE, nLine, @cText )
   cText := STRTRAN( cText, CHR(0) )
RETURN cText

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Save( cFile ) CLASS Source
   LOCAL pPrev, hFile, cText, n, nPos
   IF cFile != NIL
      ::File := cFile
   ENDIF
   IF !EMPTY( ::File )
      pPrev := ::GetCurDoc()
      nPos := ::GetCurrentPos()
      ::Owner:SelectDocument( ::pSource )
      IF ( hFile := fCreate( ::File ) ) <> -1
         cText := ::GetText()
         fWrite( hFile, cText, Len(cText) )
         fClose( hFile )
      ENDIF
      ::Owner:SelectDocument( pPrev )
      ::Modified := .F.

      ::GoToPosition( nPos )

      n := RAT( "\", ::File )
      ::FileName := SUBSTR( ::File, n+1 )
      ::Path     := SUBSTR( ::File, 1, n-1 )

      cText := ::Application:SourceTabs:GetItemText()
      cText := ALLTRIM( STRTRAN( cText, "*" ) )
      ::Application:SourceTabs:SetItemText( , cText, .F. )
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
   ::Owner:SendMessage( SCI_SETTEXT, 0, cText )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD ReplaceAll( cFind, cReplace, nFlags, lInSelection ) CLASS Source
   local nStartPos, nEndPos, nLen, nCountSel, nSelType
   local nRepLen, nStarLine, nEndLine
   local nLastMatch, nReplacements, nLenTarget, linsideASel, i, nMovePastEOL, nChrNext
   local nLenReplaced, nCurPos, nPosFind

   IF ( nLen := LEN( cFind ) ) == 0
      return -1
   ENDIF
   DEFAULT lInSelection TO .F.
   nCountSel := ::Source:GetSelections()

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
      nEndPos := ::GetTextLen()
      IF ::Application:EditorProps:WrapSearch == 1
         nStartPos := 0
      ENDIF
      // If not wrapFind, replace all only from caret to end of document
   ENDIF

   nRepLen := LEN( cReplace )

   ::SetSearchFlags( nFlags )
   nPosFind := ::FindInTarget( cFind, nStartPos, nEndPos )

   IF nPosFind != -1 .AND. nPosFind <= nEndPos
      nLastMatch := nPosFind
      nReplacements := 0

      nCurPos := ::GetCurrentPos()
      ::BeginUndoAction()

      // Replacement loop
      DO WHILE nPosFind != -1
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
      RETURN nReplacements
   ENDIF
RETURN 0

//------------------------------------------------------------------------------------------------------------------------------------
METHOD FindInTarget( cText, nStartPos, nEndPos ) CLASS Source
   LOCAL nPos, nLen := LEN( cText )
   ::SetTargetStart( nStartPos )
   ::SetTargetEnd( nEndPos )
   nPos := ::SearchInTarget( nLen, cText )
   WHILE ::Owner:FindInStyle .AND. nPos != -1 && ::Owner:FindStyle != ::GetStyleAt( nPos )
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

function view( cView )
   VIEW cView
RETURN NIL