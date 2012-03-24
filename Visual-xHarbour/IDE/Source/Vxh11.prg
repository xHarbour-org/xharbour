/*
 * $Id$
 */

#include "vxh.ch"
#include "cstruct.ch"
#include "colors.ch"
#include "debug.ch"
#include "commdlg.ch"
#include "Scintilla.ch"

#define SCE_STYLE_BLACK   10
#define SCE_STYLE_ORANGE  11
#define SCE_STYLE_PURPLE  12
#define SCE_STYLE_BLUE    13


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

   DATA Color_Fore   EXPORTED
   DATA Color_Back   EXPORTED

   // Compatibility with xedit debugger ----------------------------------------------------
   ACCESS oEditor    INLINE ::xSource
   ASSIGN oEditor(o) INLINE ::Source(o)
   //---------------------------------------------------------------------------------------


   METHOD Init() CONSTRUCTOR
   METHOD Create()

   METHOD StyleSetFont( cFont )           INLINE ::SendMessage( SCI_STYLESETFONT, STYLE_DEFAULT, cFont )
   METHOD StyleGetFont( cFont )           INLINE cFont := SPACE(255), ::SendMessage( SCI_STYLEGETFONT, STYLE_DEFAULT, @cFont ), ALLTRIM(cFont)

   METHOD StyleSetSize( nSize )           INLINE ::SendMessage( SCI_STYLESETSIZE, STYLE_DEFAULT, nSize )
   METHOD StyleGetSize( nSize )           INLINE ::SendMessage( SCI_STYLEGETSIZE, STYLE_DEFAULT, @nSize ), nSize

   METHOD SetLexer( nLexer )              INLINE ::SendMessage( SCI_SETLEXER, nLexer, 0 )   

   METHOD SelectDocument( pDoc )          INLINE ::SendMessage( SCI_SETDOCPOINTER, 0, pDoc )
   METHOD OnDestroy()                     INLINE aEval( ::aDocs, {|oDoc| oDoc:Close() } ), FreeLibrary( ::hSciLib ), NIL
   METHOD OnParentNotify()
   METHOD OnSetFocus()                    INLINE ::SendMessage( SCI_SETFOCUS, 1, 0 )

   METHOD StyleSetFore( nStyle, nColor )  INLINE ::SendMessage( SCI_STYLESETFORE, nStyle, nColor )
   METHOD StyleSetBack( nStyle, nColor )  INLINE ::SendMessage( SCI_STYLESETBACK, nStyle, nColor )
  
   METHOD OnFindNext()
   METHOD OnReplace()
   METHOD OnReplaceAll()
   
   METHOD GetSearchFlags()
   METHOD SetStyle()
   METHOD FindNext()
   METHOD EnsureRangeVisible()
ENDCLASS

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS SourceEditor
   ::hSciLib := LoadLibrary( "SciLexer.dll" )
   ::__xCtrlName := "SourceEditor"
   ::ClsName := "Scintilla"
   ::Super:Init( oParent )
   ::Style := WS_CHILD | WS_TABSTOP | WS_CLIPCHILDREN
   ::EventHandler[ "OnFindNext" ]   := "OnFindNext"
   ::EventHandler[ "OnReplace" ]    := "OnReplace"
   ::EventHandler[ "OnReplaceAll" ] := "OnReplaceAll"
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS SourceEditor

   ::Color_Fore := RGB(  0,  0,  0)
   ::Color_Back := RGB(255,255,255)

   ::Super:Create()

   ::SetLexer( SCLEX_CONTAINER )
   ::StyleSetBack( STYLE_DEFAULT, ::Color_Back )
   ::StyleSetFore( STYLE_DEFAULT, ::Color_Fore )

   ::SendMessage( SCI_SETCARETFORE, ::Color_Fore )
   ::SendMessage( SCI_SETCARETPERIOD, 500 )
   ::SendMessage( SCI_SETCARETWIDTH, 2 )
   ::SendMessage( SCI_SETCARETSTYLE, 1 )

   ::SendMessage( SCI_STYLESETFORE, SCE_STYLE_BLACK,  ::Color_Fore )
   ::SendMessage( SCI_STYLESETFORE, SCE_STYLE_ORANGE, RGB( 255, 128,   0 ) )
   ::SendMessage( SCI_STYLESETFORE, SCE_STYLE_PURPLE, RGB( 255,   0, 255 ) )
   ::SendMessage( SCI_STYLESETFORE, SCE_STYLE_BLUE,   RGB(   0,   0, 255 ) )

   ::StyleSetFont( "Courier New" )
   ::StyleSetSize( 10 )

   ::SendMessage( SCI_STYLECLEARALL, 0, 0 )
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS SourceEditor
   LOCAL cText, n, scn, nLineNumber, nStartPos, nLineLen, nEndPos, cChar
   (nwParam, nlParam)
   DO CASE
      CASE hdr:code == SCN_UPDATEUI
           n := ::Source:GetCurrentPos()
           ::Application:SetEditorPos( ::Source:LineFromPosition(n), ::Source:GetColumn(n) )

      CASE hdr:code == SCN_MODIFIED
      
           IF ! ::Source:Modified .AND. ! ::Source:FirstOpen
              ::Source:Modified := .T.
              n := aScan( ::aDocs, ::Source,,, .T. )
              
              cText := ::Application:SourceTabs:GetItemText(n)
              cText := ALLTRIM( STRTRAN( cText, "*" ) )

              ::Application:SourceTabs:SetItemText( n, " " + cText + " * ", .T. )

              ::Application:Props:EditUndoItem:Enabled := ::Application:Props:EditUndoBttn:Enabled := ::Source:CanUndo()
              ::Application:Props:EditRedoItem:Enabled := ::Application:Props:EditRedoBttn:Enabled := ::Source:CanRedo()
              
              ::Application:Project:Modified := .T.
           ENDIF

//      CASE hdr:code == SCN_CHARADDED
           //scn := (struct SCNOTIFICATION*) nlParam

       CASE hdr:code == SCN_STYLENEEDED
            scn := (struct SCNOTIFICATION*) nlParam

            nLineNumber := ::Source:LineFromPosition( ::Source:GetEndStyled() )
            nStartPos   := ::Source:PositionFromLine( nLineNumber )
            nEndPos     := scn:position
VIEW nEndPos
//            nEndPos :=  ::SendMessage( EM_LINEINDEX,  nLineNumber )
            nLineLen    := ::SendMessage( SCI_LINELENGTH, nLineNumber )
view nLineNumber, nLineLen, nEndPos

            IF nLineLen > 0
               cChar := CHR( ::SendMessage( SCI_GETCHARAT, nStartPos, 0 ) )
               ::SendMessage( SCI_STARTSTYLING, nStartPos, 31 )
//view cChar, nStartPos, nEndPos

               DO CASE
                  CASE cChar == "-"
                       ::SendMessage( SCI_SETSTYLING, nLineLen, SCE_STYLE_ORANGE )
                  CASE cChar == "/"
                       ::SendMessage( SCI_SETSTYLING, nLineLen, SCE_STYLE_PURPLE )
                  CASE cChar == "*"
                       ::SendMessage( SCI_SETSTYLING, nLineLen, SCE_STYLE_BLUE )
                  OTHERWISE
                       ::SendMessage( SCI_SETSTYLING, nLineLen, SCE_STYLE_BLACK )
               END
             ELSE
               ::SendMessage( SCI_SETSTYLING, nLineLen, SCE_STYLE_BLACK )
            ENDIF

   ENDCASE
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD SetStyle( nStartPos, nPos ) CLASS SourceEditor
   (nStartPos, nPos)
   
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
   ::Source:SetSearchFlags( ::GetSearchFlags( oFind ) )
   ::FindNext( oFind:FindWhat, oFind:Direction == 0 )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD FindNext( cFindWhat, lReverseDirection ) CLASS SourceEditor
   LOCAL lFound, nStartPos, nEndPos
   LOCAL nStart, nEnd

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
   IF ::Source:GetSelText()-1 == 0
      IF ! EMPTY( oFind:FindWhat )
         ::Source:SearchNext( ::GetSearchFlags( oFind ), oFind:FindWhat )
      ENDIF
    ELSEIF ::Source:GetSelText()-1 > 0
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
   METHOD Undo()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_UNDO, 0, 0 )
   METHOD Redo()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_REDO, 0, 0 )
   METHOD Copy()                              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_COPY, 0, 0 )
   METHOD Paste()                             INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_PASTE, 0, 0 )
   METHOD Cut()                               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CUT, 0, 0 )
   METHOD CanPaste()                          INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_CANPASTE, 0, 0 )==1
   METHOD SetReadOnly( nSet )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETREADONLY, nSet, 0 )
   METHOD GetReadOnly()                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETREADONLY, 0, 0 )
   METHOD GetCurrentPos()                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETCURRENTPOS, 0, 0 )
   METHOD SetCurrentPos( nPos )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETCURRENTPOS, nPos, 0 )
   METHOD GoToPos( nPos )                     INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GOTOPOS, nPos, 0 )
   METHOD GetColumn( nPos )                   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETCOLUMN, nPos, 0 )+1

   METHOD LineFromPosition( nPos )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_LINEFROMPOSITION, nPos, 0 )+1
   METHOD PositionFromLine( nPos )            INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_POSITIONFROMLINE, nPos, 0 )+1

   METHOD GetCurLine()                        INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_LINEFROMPOSITION, ::GetCurrentPos(), 0 )+1
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
   METHOD GetSelText( cBuffer )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETSELTEXT, 0, cBuffer )
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

   METHOD SetTabWidth( nChars )               INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETTABWIDTH, nChars, 0 )
   METHOD GetTabWidth()                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETTABWIDTH, 0, 0 )
   METHOD SetUseTabs( nUseTabs )              INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETUSETABS, nUseTabs, 0 )
   METHOD AppendText( cText )                 INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_APPENDTEXT, Len(cText), cText )
   METHOD UsePopUp( n )                       INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_USEPOPUP, n )

   METHOD SetText()
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
   LOCAL n, cText := MEMOREAD( cFile )
   ::SetText( cText )
   ::SetSavePoint()
   ::GotoPosition( 0 )
   ::EmptyUndoBuffer()
   ::Modified  := .F.
   ::FirstOpen := .F.
   n := RAT( "\", cFile )
   ::FileName := SUBSTR( cFile, n+1 )
   ::Path     := SUBSTR( cFile, 1, n-1 )
   ::File     := cFile
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
METHOD Save( cFile ) CLASS Source
   LOCAL pPrev, hFile, cText, n
   IF cFile != NIL
      ::File := cFile
   ENDIF
   IF !EMPTY( ::File )
      pPrev := ::GetCurDoc()
      ::Owner:SelectDocument( ::pSource )
      IF ( hFile := fCreate( ::File ) ) <> -1
         cText := ::GetText()
         fWrite( hFile, cText, Len(cText) )
         fClose( hFile )
      ENDIF
      ::Owner:SelectDocument( pPrev )
      ::Modified := .F.

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
         nEndPos   := ::PositionFromLine( nEndLine+1 )
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
