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

static hSciLib

INIT PROCEDURE StartSci
   hSciLib := LoadLibrary( "SciLexer.dll" )
RETURN

EXIT PROCEDURE FreeSci
   FreeLibrary( hSciLib )
   hSciLib := NIL
RETURN

//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------------------------------------

CLASS SourceEditor INHERIT Control
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

   DATA cFindWhat         EXPORTED
   DATA nDirection        EXPORTED
   DATA CaretLineVisible  EXPORTED
   DATA AutoIndent        EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   
   METHOD StyleClearAll()                 INLINE ::SendMessage( SCI_STYLECLEARALL, 0, 0 ), Self
   METHOD StyleSetFont( cFont )           INLINE ::FontFaceName := cFont, SendEditorString( ::hWnd, SCI_STYLESETFONT, STYLE_DEFAULT, cFont ), Self
   METHOD StyleGetFont( cFont )           INLINE cFont := SPACE(255), ::SendMessage( SCI_STYLEGETFONT, STYLE_DEFAULT, @cFont ), ALLTRIM(cFont)

   METHOD StyleSetSize( nSize )           INLINE ::FontSize := nSize, ::SendMessage( SCI_STYLESETSIZE, STYLE_DEFAULT, nSize ), Self
   METHOD StyleGetSize( nSize )           INLINE ::SendMessage( SCI_STYLEGETSIZE, STYLE_DEFAULT, @nSize ), nSize

   METHOD SetLexer( nLexer )              INLINE ::SendMessage( SCI_SETLEXER, nLexer, 0 )   

   METHOD SelectDocument( pDoc )          INLINE ::SendMessage( SCI_SETDOCPOINTER, 0, pDoc )
   METHOD OnDestroy()
   METHOD OnParentNotify()
   METHOD OnSetFocus()                    INLINE ::SendMessage( SCI_SETFOCUS, 1, 0 )

   METHOD Colorise( nStart, nEnd )        INLINE ::SendMessage( SCI_COLOURISE, nStart, nEnd )

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
   METHOD AutoIndentText()
   METHOD ToggleRectSel()
   METHOD GetWithObject()
ENDCLASS

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS SourceEditor
   LOCAL n, cSyntax := ::Application:Path + "\\vxh.syn"
   ::__xCtrlName := "SourceEditor"
   ::ClsName := "Scintilla"
   ::Super:Init( oParent )
   ::Style := WS_CHILD | WS_TABSTOP | WS_CLIPCHILDREN
   ::EventHandler[ "OnFindNext" ]   := "OnFindNext"
   ::EventHandler[ "OnReplace" ]    := "OnReplace"
   ::EventHandler[ "OnReplaceAll" ] := "OnReplaceAll"
   
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
   ::CaretLineVisible  := ::Application:IniFile:ReadInteger( "Settings", "CaretLineVisible", 1 )
   ::AutoIndent        := ::Application:IniFile:ReadInteger( "Settings", "AutoIndent", 1 )

   IF ( n := ::Application:Props:FontList:FindString(, ::FontFaceName ) ) > 0
      ::Application:Props:FontList:SetCurSel(n)
   ENDIF
   IF ( n := ::Application:Props:FontSize:FindString(, xStr(::FontSize) ) ) > 0
      ::Application:Props:FontSize:SetCurSel(n)
   ENDIF
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD Create() CLASS SourceEditor
   ::Super:Create()

   ::SendMessage( SCI_SETLEXER, SCLEX_FLAGSHIP )
   ::SendMessage( SCI_SETSTYLEBITS, ::SendMessage( SCI_GETSTYLEBITSNEEDED ) )
   
   ::StyleSetBack( STYLE_DEFAULT, ::ColorBackground )
   ::StyleSetFore( STYLE_DEFAULT, ::ColorNormalText )

   ::StyleSetFont( ::FontFaceName )
   ::StyleSetSize( ::FontSize )

   ::StyleClearAll()
   ::InitLexer()
RETURN Self

//------------------------------------------------------------------------------------------------------------------------------------
METHOD InitLexer() CLASS SourceEditor
   SciSetProperty( ::hWnd, "fold", "1" )
   SciSetProperty( ::hWnd, "fold.compact", "0" )
   SciSetProperty( ::hWnd, "fold.comment", "1" )
   SciSetProperty( ::hWnd, "fold.preprocessor", "0" )
   SciSetProperty( ::hWnd, "fold.directive", "0" )

   SciSetKeywords( ::hWnd, 0, ::Keywords1 )
   SciSetKeywords( ::hWnd, 1, ::Keywords2 )
   SciSetKeywords( ::hWnd, 2, ::Keywords3 )
   SciSetKeywords( ::hWnd, 3, ::Keywords4 )

   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD,        ::ColorKeywords1 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD2,       ::ColorKeywords2 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD3,       ::ColorKeywords3 )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_KEYWORD4,       ::ColorKeywords4 )

   ::SendMessage( SCI_SETCARETLINEBACK, ::ColorSelectedLine )
   ::SendMessage( SCI_SETCARETLINEVISIBLE, ::CaretLineVisible )

   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENT,        ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTLINE,    ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTDOC,     ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_COMMENTLINEDOC, ::ColorComments  )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_OPERATOR,       ::ColorOperators )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_STRING,         ::ColorStrings   )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_NUMBER,         ::ColorNumbers   )
   ::SendMessage( SCI_STYLESETFORE, SCE_FS_PREPROCESSOR,   ::ColorPreprocessor )

   ::SendMessage( SCI_SETFOLDMARGINCOLOUR, 1, ::ColorBackground )
   ::SendMessage( SCI_SETFOLDMARGINHICOLOUR, 1, ::ColorBackground )

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

   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPEN,    SC_MARK_MINUS )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDER,        SC_MARK_PLUS  )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERSUB,     SC_MARK_EMPTY )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERTAIL,    SC_MARK_EMPTY )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEREND,     SC_MARK_EMPTY )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDEROPENMID, SC_MARK_EMPTY )
   ::SendMessage( SCI_MARKERDEFINE,  SC_MARKNUM_FOLDERMIDTAIL, SC_MARK_EMPTY )

   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDER,        ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPEN,    ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEROPENMID, ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERSUB,     ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERMIDTAIL, ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDERTAIL,    ::ColorBackground )
   ::SendMessage( SCI_MARKERSETFORE, SC_MARKNUM_FOLDEREND,     ::ColorBackground )

   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDER,        ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPEN,    ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEROPENMID, ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERSUB,     ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERMIDTAIL, ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDERTAIL,    ::ColorNormalText )
   ::SendMessage( SCI_MARKERSETBACK, SC_MARKNUM_FOLDEREND,     ::ColorNormalText )

   ::SendMessage( SCI_SETFOLDFLAGS, 16, 0)

   //::SendMessage( SCI_SETINDENTATIONGUIDES, 1, 0)
   //::SendMessage( SCI_SETHIGHLIGHTGUIDE, 30, 0)

   ::SendMessage( SCI_CLEARREGISTEREDIMAGES, 0, 0 )
   //::SendMessage( SCI_RGBAIMAGESETWIDTH, 16, 0 )
   //::SendMessage( SCI_RGBAIMAGESETHEIGHT, 16, 0 )

   SciRegisterPropertyImage( ::hWnd, 8 )
   SciRegisterMethodImage( ::hWnd, 7 )
   SciRegisterEventImage( ::hWnd, 6 )

   //SciRegisterImage( ::hWnd, 2, "ICO_METHOD" )
   //SciRegisterImage( ::hWnd, 3, "ICO_EVENT" )

   ::SendMessage( SCI_SETMODEVENTMASK, SC_MOD_CHANGEFOLD | SC_MOD_INSERTTEXT | SC_MOD_DELETETEXT | SC_PERFORMED_UNDO | SC_PERFORMED_REDO )
RETURN NIL

//------------------------------------------------------------------------------------------------------------------------------------
METHOD OnLButtonUp() CLASS SourceEditor
   ::Application:Project:EditReset()
RETURN NIL

METHOD ToggleRectSel() CLASS SourceEditor
   ::SendMessage( SCI_SETSELECTIONMODE, SC_SEL_RECTANGLE, 0 )
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
   LOCAL scn, nPos, n, cObj, cText, nLine, nChar, nPosStart, nPosEnd, oObj, aObj, aProperties, aProp, cList, aMethods, Topic, Event, aList//, cFind
   LOCAL nWrap
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
           IF scn:ch == 13 // indent
              IF ::AutoIndent == 1
                 ::AutoIndentText()
              ENDIF

            ELSEIF scn:ch == 58
              //IF ::SendMessage( SCI_AUTOCACTIVE ) > 0
                 //::SendMessage( SCI_AUTOCCANCEL )
              //ENDIF
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
                 nPos := ::Source:GetCurrentPos()
                  
                 cObj := ::GetWithObject()

                 ::Application:EditorProps:WrapSearch := nWrap
                 ::Source:GotoPosition( nPos ) 
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

                    aProperties := __ClsGetPropertiesAndValues( oObj )
                    FOR n := 1 TO LEN( aProperties )
                        aProp := GetProperCase( __Proper( aProperties[n][1] ) )
                        AADD( aList, aProp[1]+"?8" )
                    NEXT

                    aMethods := __objGetMethodList( oObj )
                    FOR n := 1 TO LEN( aMethods )
                        AADD( aList, __Proper( aMethods[n] )+"?7" )
                    NEXT

                    IF __ObjHasMsg( oObj, "Events" )
                       FOR EACH Topic IN oObj:Events
                           FOR EACH Event IN Topic[2]
                               AADD( aList, Event[1]+"?6" )
                           NEXT
                       NEXT
                    ENDIF

                    aSort( aList,,,{|x, y| x[1] < y[1]})
                    cList := ""
                    FOR n := 1 TO LEN( aList )
                        cList += aList[n]+ IIF( n<LEN(aList)," ", "" )
                    NEXT
                    ::SendMessage( SCI_AUTOCSETCANCELATSTART, 0 )
                    ::SendMessage( SCI_AUTOCSETAUTOHIDE, 0 )
                    ::SendMessage( SCI_AUTOCSETMAXHEIGHT, 15 )
                    ::SendMessage( SCI_AUTOCSETIGNORECASE, 1 )
                    ::SendMessage( SCI_AUTOCSHOW, 0, cList )
                 ENDIF
              ENDIF
//             ELSE
//               IF ::SendMessage( SCI_AUTOCACTIVE ) > 0
//                  cFind := ""
//                  nPosEnd   := ::Source:GetCurrentPos()-1
//                  nPosStart := ::Source:PositionFromLine( ::Source:LineFromPosition( nPosEnd ) )
//                  FOR n := nPosEnd TO nPosStart STEP -1
//                      nChar := ::Source:GetCharAt(n)
//                      IF nChar IN {32,58}
//                         EXIT
//                      ENDIF
//                      cFind := CHR(nChar) + cFind
//                      IF ::Source:GetColumn(n)==0
//                         EXIT
//                      ENDIF
//                  NEXT
//                  SendEditorString( ::hWnd, SCI_AUTOCSELECT, 0, cFind )
//               ENDIF
           ENDIF
   ENDCASE
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
   aEval( ::aDocs, {|oDoc| oDoc:Close() } )

   ::Application:IniFile:WriteInteger( "Settings", "WrapSearch", ::Application:EditorProps:WrapSearch )

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

   ::Application:IniFile:WriteString( "Font", "FaceName", ::FontFaceName )
   ::Application:IniFile:WriteInteger( "Font", "Size", ::FontSize )
   ::Application:IniFile:WriteInteger( "Settings", "CaretLineVisible", ::CaretLineVisible )
   ::Application:IniFile:WriteInteger( "Settings", "AutoIndent", ::AutoIndent )
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
   DATA Form      EXPORTED
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
   METHOD GetLineIndentation( nLine )         INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_GETLINEINDENTATION, nLine )
   METHOD SetLineIndentation( nLine, nInd )   INLINE ::ChkDoc(), ::Owner:SendMessage( SCI_SETLINEINDENTATION, nLine, nInd )

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

//------------------------------------------------------------------------------------------------------------------------------------

CLASS Settings INHERIT Dialog
   // Components declaration
   METHOD Init() CONSTRUCTOR
   METHOD OnInitDialog()

   // Event declaration
   METHOD Settings_OnLoad()
   METHOD DefBack_OnClick()
   METHOD DefFore_OnClick()

ENDCLASS

METHOD Init( oParent, aParameters ) CLASS Settings
   ::Super:Init( oParent, aParameters )

   ::EventHandler[ "OnLoad" ] := "Settings_OnLoad"

   // Populate Components
   WITH OBJECT ( ColorDialog( Self ) )
      :Name                 := "ColorDialog1"
      :Create()
   END //ColorDialog1

   // Properties declaration
   ::Name                 := "Settings"
   ::Modal                := .T.
   ::Left                 := 10
   ::Top                  := 10
   ::Width                := 471
   ::Height               := 458
   ::Center               := .T.
   ::Caption              := "Source Editor Settings"
   ::DlgModalFrame        := .T.

   ::Create()

   // Populate Children
RETURN Self

METHOD OnInitDialog() CLASS Settings
   // Properties declaration

   // Populate Children
   WITH OBJECT ( TABSTRIP( Self ) )
      :Name                 := "TabStrip1"
      WITH OBJECT :Dock
         :Left                 := "Settings"
         :Top                  := "Settings"
         :Right                := "Settings"
         :Bottom               := "Button3"
         :Margins              := "5,5,5,5"
      END

      :Left                 := 5
      :Top                  := 5
      :Width                := 453
      :Height               := 389
      :Create()
      WITH OBJECT ( TABPAGE( :this ) )
         :Name                 := "EditorSettings"
         :Caption              := "&Editor"
         :Create()
         WITH OBJECT ( GROUPBOX( :this ) )
            :Name                 := "GroupBox1"
            :Left                 := 10
            :Top                  := 7
            :Width                := 431
            :Height               := 347
            :Caption              := "Colors"
            :ForeColor            := 0
            :Create()
            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Background"
               :Left                 := 263
               :Top                  := 21
               :Width                := 77
               :Height               := 22
               :Caption              := "Background"
               :EventHandler[ "OnClick" ] := "DefBack_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "NormalText"
               :Left                 := 345
               :Top                  := 21
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label1"
               :Left                 := 15
               :Top                  := 24
               :Width                := 96
               :Height               := 16
               :Caption              := "Default"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "NormalTextEdit"
               :Left                 := 124
               :Top                  := 22
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := "Normal Text"
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "SelectedLine"
               :Left                 := 263
               :Top                  := 48
               :Width                := 77
               :Height               := 22
               :Caption              := "Background"
               :EventHandler[ "OnClick" ] := "DefBack_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "SelectedLineEdit"
               :Left                 := 124
               :Top                  := 49
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := "Normal Text"
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label2"
               :Left                 := 10
               :Top                  := 51
               :Width                := 101
               :Height               := 16
               :Caption              := "Current Line"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Numbers"
               :Left                 := 345
               :Top                  := 75
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "NumbersEdit"
               :Left                 := 124
               :Top                  := 76
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := "12345678"
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label3"
               :Left                 := 16
               :Top                  := 79
               :Width                := 96
               :Height               := 16
               :Caption              := "Numbers"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Strings"
               :Left                 := 345
               :Top                  := 101
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "StringsEdit"
               :Left                 := 124
               :Top                  := 102
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := '"This is text"'
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label4"
               :Left                 := 16
               :Top                  := 105
               :Width                := 96
               :Height               := 16
               :Caption              := "Strings"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Comments"
               :Left                 := 345
               :Top                  := 128
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "CommentsEdit"
               :Left                 := 124
               :Top                  := 129
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := "// Comment"
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label5"
               :Left                 := 16
               :Top                  := 132
               :Width                := 96
               :Height               := 16
               :Caption              := "Comments"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Operators"
               :Left                 := 345
               :Top                  := 154
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "OperatorsEdit"
               :Left                 := 124
               :Top                  := 155
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := ":="
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label6"
               :Left                 := 16
               :Top                  := 158
               :Width                := 96
               :Height               := 16
               :Caption              := "Operators"
               :Rightalign           := .T.
               :Create()
            END //LABEL

            WITH OBJECT ( BUTTON( :this ) )
               :Name                 := "Preprocessors"
               :Left                 := 345
               :Top                  := 180
               :Width                := 77
               :Height               := 22
               :Caption              := "Foreground"
               :EventHandler[ "OnClick" ] := "DefFore_OnClick"
               :Create()
            END //BUTTON

            WITH OBJECT ( EDITBOX( :this ) )
               :Name                 := "PreprocessorEdit"
               :Left                 := 124
               :Top                  := 181
               :Width                := 129
               :Height               := 22
               :StaticEdge           := .T.
               :ClientEdge           := .F.
               :Caption              := "#include"
               :ReadOnly             := .T.
               :Create()
            END //EDITBOX

            WITH OBJECT ( LABEL( :this ) )
               :Name                 := "Label7"
               :Left                 := 16
               :Top                  := 184
               :Width                := 96
               :Height               := 16
               :Caption              := "Preprocessor"
               :Rightalign           := .T.
               :Create()
            END //LABEL

         END //GROUPBOX

      END //TABPAGE

   END //TABSTRIP

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button3"
      WITH OBJECT :Dock
         :Right                := "Button4"
         :Bottom               := "Settings"
         :Margins              := "5,5,5,5"
      END

      :Left                 := 226
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Caption              := "OK"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button4"
      WITH OBJECT :Dock
         :Right                := "Button5"
         :Bottom               := "Settings"
         :Margins              := "5,5,5,5"
      END

      :Left                 := 304
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Caption              := "Cancel"
      :Create()
   END //BUTTON

   WITH OBJECT ( BUTTON( Self ) )
      :Name                 := "Button5"
      WITH OBJECT :Dock
         :Right                := "Settings"
         :Bottom               := "Settings"
         :Margins              := "5,5,5,5"
      END

      :Left                 := 383
      :Top                  := 399
      :Width                := 75
      :Height               := 24
      :Caption              := "&Apply"
      :Create()
   END //BUTTON

RETURN Self

//----------------------------------------------------------------------------------------------------
METHOD DefBack_OnClick( Sender ) CLASS Settings
   LOCAL cColor := "Color"+Sender:Name
   LOCAL cEdit := IIF( Sender:Name == "Background", "NormalText", Sender:Name ) + "Edit"
   ::ColorDialog1:Color := ::Application:SourceEditor:&cColor
   IF ::ColorDialog1:Show()
      ::&cEdit:BackColor := ::ColorDialog1:Color
      IF Sender:Name != "SelectedLineEdit"
         ::NormalTextEdit:BackColor   := ::ColorDialog1:Color
         ::NumbersEdit:BackColor      := ::ColorDialog1:Color
         ::StringsEdit:BackColor      := ::ColorDialog1:Color
         ::CommentsEdit:BackColor     := ::ColorDialog1:Color
         ::OperatorsEdit:BackColor    := ::ColorDialog1:Color
         ::PreprocessorEdit:BackColor := ::ColorDialog1:Color
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

   ::NormalTextEdit:ForeColor   := ::Application:SourceEditor:ColorNormalText
   ::SelectedLineEdit:ForeColor := ::Application:SourceEditor:ColorNormalText
   ::NumbersEdit:ForeColor      := ::Application:SourceEditor:ColorNumbers
   ::StringsEdit:ForeColor      := ::Application:SourceEditor:ColorStrings
   ::CommentsEdit:ForeColor     := ::Application:SourceEditor:ColorComments
   ::OperatorsEdit:ForeColor    := ::Application:SourceEditor:ColorOperators
   ::PreprocessorEdit:ForeColor := ::Application:SourceEditor:ColorPreprocessor
RETURN Self
