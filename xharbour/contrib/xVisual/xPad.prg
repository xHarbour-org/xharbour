/*
 * $Id: xPad.prg,v 1.3 2002/10/23 07:14:28 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * XIDE xPad Colored Editor
 *
 * Copyright 2002 Andrew J. Wos [andrwos@aust1.net]
 *
 * xIde Version Augusto Infante [augusto@2vias.com.ar]
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */


// to do
// add undo 10 or so levels

// list of reserved and standard words
Static aReserved:={;
                   'DO' ,;
                   'WHILE' ,;
                   'ENDDO' ,;
                   'CASE' ,;
                   'OTHERWISE' ,;
                   'ENDCASE' ,;
                   'IF' ,;
                   'ELSE' ,;
                   'ELSEIF' ,;
                   'ENDIF' ,;
                   'BEGIN' ,;
                   'SEQUENCE' ,;
                   'BREAK' ,;
                   'LOOP',;
                   'RECOVER',;
                   'END' ,;
                   'FOR' ,;
                   'TO' ,;
                   'NEXT' ,;
                   'STEP' ,;
                   'ANNOUNCE' ,;
                   'DECLARE' ,;
                   'EXTERNAL' ,;
                   'FIELD' ,;
                   'FUNCTION' ,;
                   'FUNC' ,;
                   'METHOD' ,;
                   'METH' ,;
                   'CLASS' ,;
                   'ENDCLASS' ,;
                   'VAR' ,;
                   'INIT' ,;
                   'PROCEDURE' ,;
                   'PROC' ,;
                   'STATIC' ,;
                   'LOCAL' ,;
                   'MEMVAR' ,;
                   'PARAMETERS' ,;
                   'PRIVATE' ,;
                   'PUBLIC' ,;
                   'REQUEST' ,;
                   'RETURN' ,;
                   'COMMAND' ,;
                   'DEFINE' ,;
                   'ERROR' ,;
                   'IFDEF' ,;
                   'IFNDEF' ,;
                   'INCLUDE' ,;
                   'STDOUT' ,;
                   'UNDEF' ,;
                   'XCOMMAND' ,;
                   'XTRANSLATE' ,;
                   'EXIT',;
                   '.T.',;
                   '.F.',;
                   '.AND.',;
                   '.NOT.',;
                   '.OR.',;
                   'NIL' ,;
                   'SET' ,;
                   'OFF' ,;
                   'ON' ;
                  }

static aStandard:={;
                   'hWnd' ,;
                   'hDC' ,;
                   'nMsg' ,;
                   'hDlg' ,;
                   'hDlgWnd' ,;
                   'nwParam' ,;
                   'nlParam' ;
                  }
Static aDictionary:={}
Static aLocals:={}
Static aGlobals:={}

#define COMMEN "*"+"/"

#Define PRG_LEFTCOL        1
#Define PRG_TOPLINE        2
#Define PRG_COL            3
#Define PRG_LINE           4
#Define PRG_ISBLOCK        5
#Define PRG_BLOCKTOP       6
#Define PRG_BLOCKLEFT      7
#Define PRG_BLOCKRIGHT     8
#Define PRG_BLOCKBOTTOM    9
#Define PRG_MARK          10
#Define PRG_READONLY      11
#Define PRG_ATEXT         12
//#define PRG_CHANGED       13

#Define PRG_STATELEN      12


// defines and includes

#Include "windows.ch"
#Include 'hbclass.ch'
#Include 'what32.ch'
#Include 'commdlg.ch'
#Include 'accel.ch'
#Include 'WinGdi.ch'
#Include 'WinTypes.ch'
#Include 'cstruct.ch'


static aUserKeys:={;
{112,.f.,.f.,{|o,w| w:=o:getword(),;
MessageBox(o:hWnd,"No help available yet","xide"),.t.}},; // F1
{112,.f.,.t.,{|o,w| w:=o:getword(),;
MessageBox(o:hWnd,"No help available yet","xide"),.t.}},; // Ctrl-F1
{113,.f.,.f.,{|o| if(o:isBlockOn,(o:toupper(),.f.),.t.)}},;  // F2
{114,.f.,.f.,{|o| if(o:isBlockOn,(o:tolower(),.f.),.t.)}},;  // F3
{ 77,.f.,.t.,{|o| o:findmatch()}},;                          // Ctrl-M
{ 89,.f.,.t.,{|o| o:delline(o:line),.f.}},;             // Ctrl-Y
{ 46,.f.,.t.,{|o| o:delrest(),.T.}},;                        // Ctrl-Del
{ 76,.f.,.t.,{|o| o:mark:=if(o:mark==0,o:line,0),.f.}},;                     // Ctrl-L
{ 71,.f.,.t.,{|o| o:gotoline(o:mark),o:Stable}},;            // Ctrl-G
{ 68,.f.,.t.,{|o| o:dupline(),o:Stable}},;                   // Ctrl-D
{ 46,.t.,.f.,{|o| o:deltext(),o:Stable}},;   // not working  // Shft-Del
{109,.f.,.t.,{|o| if(o:lInSummary,.T.,(o:collapse(),.f.))}},;                       // Ctrl "-"
{107,.f.,.t.,{|o,i| if(o:lInSummary,(o:expand(),.F.),.f.)}},;                    // Ctrl "+"
{ 83,.f.,.t.,{|o| o:home(.T.),o:settext('*'+replicate('-',78)+'*'+CRLF,1,1)}},;  // Ctrl-S
{ 85,.f.,.t.,{|o| o:undelline(),o:Stable}};                                      // Ctrl-U
}
pragma pack(4)
typedef struct tagTEXTMETRIC {;
    LONG tmHeight; 
    LONG tmAscent; 
    LONG tmDescent; 
    LONG tmInternalLeading; 
    LONG tmExternalLeading; 
    LONG tmAveCharWidth; 
    LONG tmMaxCharWidth; 
    LONG tmWeight; 
    LONG tmOverhang; 
    LONG tmDigitizedAspectX; 
    LONG tmDigitizedAspectY; 
    BCHAR tmFirstChar; 
    BCHAR tmLastChar; 
    BCHAR tmDefaultChar; 
    BCHAR tmBreakChar; 
    BYTE tmItalic; 
    BYTE tmUnderlined; 
    BYTE tmStruckOut; 
    BYTE tmPitchAndFamily; 
    BYTE tmCharSet; 
} TEXTMETRIC; 

//---------------------------------------------------------------------------------------------
CLASS ObjEdit FROM TPanel
   DATA oEd
   METHOD New( oParent ) INLINE ::Caption := 'Source Editor',;
                                ::left    := 100,;
                                ::top     := 100,;
                                ::width   := 300,;
                                ::height  := 300,;
                                ::Style   := WS_POPUP+WS_VISIBLE+WS_CAPTION+WS_SYSMENU+DS_MODALFRAME+WS_MAXIMIZEBOX+WS_MINIMIZEBOX+WS_THICKFRAME,;
                                super:new( oParent )
   METHOD OnCreate()
   METHOD OnSize(n,x,y)  INLINE ::SourceEdit:Move(,,x,y,.t.),nil
ENDCLASS

METHOD OnCreate() CLASS ObjEdit
   local aRect := ::ClientRect()
   ::Add( /*'SourceEdit',*/ TEditor():New( self, "", 101,  0,0, aRect[3], aRect[4] ) )

   ::oEd := oEdit():New( ::SourceEdit:handle )
   ResetProcedure(::SourceEdit:handle)
   ::oEd:configure()
   ::oEd:subclass()
   UpdateWindow(::SourceEdit:handle)
return(nil)

CLASS TEditor FROM TForm
   METHOD New( oParent ) INLINE ::Name := "SourceEdit", super:New( oParent )

   METHOD Create() INLINE ::Style  := WS_CHILD+WS_BORDER+WS_TABSTOP+WS_VSCROLL+WS_HSCROLL,;
                          ::ExStyle:= WS_EX_CLIENTEDGE,;
                          super:Create()

   METHOD SetText(c) INLINE ::Parent:oEd:aText    :={''},;
                            ::Parent:oEd:Lines    :=1,;
                            ::Parent:oEd:TopLine  :=1,;
                            ::Parent:oEd:Line     :=1,;
                            ::Parent:oEd:Col      :=1,;
                            ::Parent:oEd:SetText(c),;
                            ::Parent:oEd:Configure()
ENDCLASS

*-----------------------------------------------------------------------------*
Class oedit
  Var Line     // within the text buffer
  Var Col      // within the text buffer
  Var Lines    // number of lines in the buffer
  Var LeftCol  // first visible column
  Var cxChar
  Var cyChar
  Var aFont
  Var xPos       // column within the edit window
  Var yPos       // row within the edit window
  Var MaxRow     // max visible rows
  Var MaxCol     // max visible columns
  Var TopLine    // first visible line
  Var BottomLine // last visible line
  Var ReadOnly
  Var InsertMode
  Var AutoIndent
  Var IndentSize
  Var TabSize
  Var nHoCaret
  Var nWoCaret
  Var nHiCaret
  Var nWiCaret
  Var aText
  Var cFileName
  Var BackupExt
  Var aComments
  Var Stable
  Var FirstInvalid
  Var nColor
  Var hDC
  Var CtrlPressed
  Var ShiftPressed
  Var CapsOn
  Var Changed
  Var bOnColChange             // user assignable display blocks
  Var bOnLineChange
  Var bOnIns
  Var bOnCaps
  Var Selecting
  Var BlockLeft
  Var BlockTop
  Var BlockRight
  Var BlockBottom
  Var BlockStyle     // dos/block style editing or Windows block style
  Var ExtCaret       // caret movement allowed past text
  Var isBlockOn
  Var xBlockStart
  Var yBlockStart
  Var OnBlockOn
  Var OnBlockOff
  Var MaxLineLen  // for dos/block style editing
  Var hWnd
  Var xWnd
  Var yWnd
  Var hMemDC   // bogus DC to prevent flicker
  Var hMemBmp
  Var hFont
  Var hOldBmp
  Var hOldFont
  Var nProc
  Var Configured
  Var Cargo
  Var aColors
  Var aSummary
  Var aSumData
  Var aPrgState
  Var lInSummary
  Var Mark
  Var aUndo
  Var isPrg
  Var doColor
  Var aKeys
  Var cLineBuff

  Method New CONSTRUCTOR
  Method SetCaret
//  Method Read    
//  Method Save    
  Method Backup  
  Method Up      
  Method Down    
  Method Left    
  Method Right   
  Method Home    
  Method PageUp  
  Method PageDown
  Method GoTop   
  Method GoBottom
  Method PageTop 
  Method PageBottom
  Method PanLeft   
  Method PanRight  
  Method ScrollUp  
  Method ScrollDown
  Method GoTop     
  Method GoBottom  
  Method GoToLine  
  Method GoToCol   
  Method FindMatch 
//  Method Find      
//  Method Replace   
  Method OnChar    
  Method OnKey     
  Method OnKeyUp   
  Method OnReturn  
  Method OnDel     
  Method OnBackSpace
  Method OnTab      
  Method OnIns      
  Method OnMouseDown
  Method OnMouseUp  
  Method OnMouseMove
  Method OnVScroll  
  Method OnHScroll  
  Method OnCaps     
  Method OnBlockKey 
  Method GetIndent  
  Method BlockOn    
  Method BlockOff   
  Method isBlockVisible
//  Method BlockGet    
//  Method BlockDel    
  Method BlockGetLen 
  Method ChooseFont  
  Method SetFont     
  Method InsLine     
  Method DelLine     
  Method DupLine     
  Method UnDelLine   
  Method GetWord     
  Method DelRest     
//  Method WordLeft    
//  Method WordRight   
//  Method DelWord     
  Method Cut         
  Method Copy        
  Method Paste       
  Method SelectAll   
  Method GetTextLen  
  Method GetText     
  Method SetText     
  Method DelText     
  Method ToUpper     
  Method ToLower     
  Method Configure     
  Method RefreshAll    
  Method RefreshCurrent
  Method Paint         
  Method DrawLine      
  Method UpdateScroll  
  Method Subclass      
  Method EditorProc    
  Method Kill          
//  Method TestRemStatus 
  Method GetComments   
//  Method UpdateComment 
  Method Collapse      
  Method Expand        
  Method ReadFile
  Method SaveFile
  METHOD FindText
  METHOD ReplaceText
//  Method xsettext
  Method updatecomments
  Method End     
EndClass

*-----------------------------------------------------------------------------*
METHOD new(hWnd)

  Local nStyle
  Local arect
  Local hDC

  ::hWnd:=hWnd

  ::Configured:=.F.
  ::doColor:=.T.
  ::isPrg:=.T.

  ::aText      :={''}
  ::lInSummary :=.F.
  ::aSummary   :={}
  ::aSumData   :={}
  ::aComments  :={}
  ::aColors    :={RGB(0,0,0),RGB(128,0,0),RGB(0,128,0),RGB(128,128,0),RGB(0,0,128),RGB(0,255,255)}
  ::Line       :=1
  ::Col        :=1
  ::Lines      :=0
  ::LeftCol    :=1
  ::Mark       :=0

  ::xPos       :=1
  ::yPos       :=1
  ::TopLine    :=1
  ::ReadOnly   :=.F.
  ::InsertMode :=.T.
  ::TabSize    :=4

  ::Changed    :=.F.
  ::Stable     :=.F.
  ::FirstInvalid:=1
  ::BlockStyle :=.F.
  ::ExtCaret   :=.T.
  ::Selecting  :=.F.
  ::isBlockOn  :=.F.

  ::BlockTop:=0
  ::BlockLeft:=0
  ::BlockBottom:=0
  ::BlockRight:=0

  ::nHoCaret:=0
  ::nWoCaret:=0
  ::nHiCaret:=0
  ::nWiCaret:=0

  ::CtrlPressed:=.F.
  ::ShiftPressed:=.F.

  ::CapsOn:= (And(GetKeyState(VK_CAPITAL),1) > 0)

  ::AutoIndent:=.T.
  ::IndentSize:=2  // test only

  ::BackupExt:='.PRB'

  ::aKeys:=aclone(aUserKeys)

  //aReserved := getReserved()
  //aEval(aReserved,{|x,y| areserved[y]:=upper(x)})

  ::aFont:=array(14)
  ::nColor:=0

  // add scrollbars

  nStyle:=GetWindowLong(::hWnd,GWL_STYLE)
  If And(nStyle,WS_VSCROLL)==0
    nStyle+=WS_VSCROLL
  EndIf

  If And(nStyle,WS_HSCROLL)==0
    nStyle+=WS_HSCROLL
  EndIf

  SetWindowLong(::hWnd,GWL_STYLE,nStyle)

  ::subclass()

  Return(self)

*-----------------------------------------------------------------------------*
METHOD subclass()

  ::nProc:=SetProcedure(::hWnd,;
                          {|hWnd,nMsg,nwParam,nlParam| ::editorproc(nMsg,nwParam,nlParam)},;
                          {WM_SIZE     ,;
                           WM_SETFOCUS ,;
                           WM_KILLFOCUS,;
                           WM_KEYDOWN  ,;
                           WM_KEYUP    ,;
                           WM_CHAR     ,;
                           WM_PAINT    ,;
                           WM_LBUTTONDOWN ,;
                           WM_LBUTTONUP   ,;
                           WM_MOUSEMOVE ,;
                           WM_DESTROY   ,;
                           WM_SYSCOMMAND,;
                           WM_VSCROLL   ,;
                           WM_HSCROLL   ,;
                           WM_GETDLGCODE,;
                           WM_LBUTTONDBLCLK,;
                           WM_ERASEBKGND  ;
                          })


  Return(self)

*-----------------------------------------------------------------------------*
METHOD configure(x,y)

  Local aTM
  Local hDC
  Local Col:=::Col
  Local arect
  local cBin
  local tm
  //view procname(1)+'('+AllTrim(str(procline(1)))+')'

  // window coordinates

  If x # NIL .AND. y # NIL
    ::xWnd:=x
    ::yWnd:=y
  Else
    arect:=GetClientRect(::hWnd)
    ::xWnd:=arect[3]
    ::yWnd:=arect[4]
  EndIf

  If ::MaxLineLen==NIL
     ::MaxLineLen :=256
  EndIf

  // create/update hMemDC

  hDC := GetDC(::hWnd)
  If Empty(::hMemDC)
    ::hMemDC:=CreateCompatibleDC(hDC)
    SetTextAlign(::hMemDC,TA_LEFT+TA_TOP+TA_NOUPDATECP)
  Else
    SelectObject(::hMemDC,::hOldBmp)
    SelectObject(::hMemDC,::hOldFont)
    DeleteObject(::hMemBmp)
  EndIf
  If ::hFont==NIL
    ::hOldFont:=SelectObject(::hMemDC,GetStockObject(SYSTEM_FIXED_FONT))
  Else
    ::hOldFont:=SelectObject(::hMemDC,::hFont)
  EndIf

  tm IS TEXTMETRIC
  tm:Buffer(GetTextMetrics(::hMemDC))
  // update text display parameters

  ::cxChar := tm:tmAveCharWidth
  ::cyChar := tm:tmHeight

  ::MaxCol     :=Int(::xWnd/::cxChar)
  ::MaxRow     :=Int(::yWnd/::cyChar)
  ::Lines:=Len(::aText)
  ::Line:=Max(1,min(::Line,::Lines))

  ::hMemBmp:=CreateCompatibleBitmap(hDC,::xWnd,(::MaxRow+1)*::cyChar)
  ::hOldBmp:=SelectObject(::hMemDC,::hMemBmp)
  ReleaseDC(::hWnd,hDC)

  ::TopLine:=min(::TopLine,Max(1,(::Lines-::MaxRow)))
  ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
  ::gotoline(::Line)
  ::gotocol(Col)

  ::FirstInvalid:=::TopLine
  ::Stable:=.F.

  ::OnCaps()
  If ValType(::bOnIns)=='B'
    eval(::bOnIns,::InsertMode)
  EndIf

  ::Configured:=.T.

  ::refreshall()
  //InvalidateRect(::hWnd,,.F.)

  Return(self)

*-----------------------------------------------------------------------------*
METHOD setcaret()

  DestroyCaret()
  CreateCaret(::hWnd,0,;
              If(::InsertMode,If(::nWiCaret==0,GetSystemMetrics(SM_CXBORDER),::nWiCaret),;
                 If(::nWoCaret==0,::cxChar,::nWoCaret)),;
              If(::InsertMode,If(::nHiCaret==0,::cyChar,::nHiCaret),;
                 If(::nHoCaret==0,::cyChar,::nHoCaret)))

  SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
  ShowCaret(::hWnd)

  Return(self)

*-----------------------------------------------------------------------------*
METHOD setfont()

  Local hFont:=0
  Local hDC

  If ::Configured
    If ::aFont # NIL
      hFont:=CreateFont(::aFont)
    EndIf

    If hFont # 0

      SelectObject(::hMemDC,hFont)

      If ::hFont # NIL
        DeleteObject(::hFont)
      EndIf

      ::hFont:=hFont
    EndIf
    ::configure()
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD ChooseFont()
/*
  Local nColor:=::nColor
  Local aFont

  If !Empty(aFont:=GetObject(::hFont))
    aFont:=Bin2A(aFont,LF_Bin2A)
    // view afont
  Else
    aFont:=aClone(::aFont)
  EndIf
  aFont = ChooseAFont( aFont,,CF_FIXEDPITCHONLY+CF_SCREENFONTS+CF_INITTOLOGFONTSTRUCT+CF_EFFECTS,@nColor,,,,,::hWnd)
  If aFont != NIL
    ::nColor:=nColor
    ::aFont:=aClone(aFont)
    ::setfont()
  EndIf
*/
  Return(self)

*-----------------------------------------------------------------------------*
* reads max of 4096 lines  of a text file
* returns:  file size (bytes read) if successful
*           0 if partially read
*          -1 if file open error occured
*-----------------------------------------------------------------------------*
METHOD readfile()

  Local aText
  Local nRetval:=readafile(::cFileName,@aText,::TabSize)

  ::TopLine:=1
  ::aText:=aText
  ::Lines:=Len(::aText)
  ::getcomments()
  ::Stable:=.F.

  Return(nRetval)

*-----------------------------------------------------------------------------*
METHOD backup()

  Local ctarget:=Left(::cFileName,at('.',::cFileName)-1)+::BackupExt

  Return(nil) //fcopy(::cFileName,ctarget))

*-----------------------------------------------------------------------------*
METHOD savefile()

  Local lSucces
  If ::lInSummary
    Return(saveafile(::cFileName,::aSumData[PRG_ATEXT]))
  EndIf

  if (lSucces:=saveafile(::cFileName,::aText))
     ::Changed:=.F.
  Endif

  Return lSucces

*-----------------------------------------------------------------------------*
METHOD gettextlen()

  Local nlen := 0

  aeval(::aText,{|x| nlen+=Len(x)})

  Return(nlen)

*-----------------------------------------------------------------------------*
METHOD OnKeyUp(nKey)

  Do Case
  Case nKey==VK_CONTROL
    ::CtrlPressed:=.F.

  Case nKey==VK_SHIFT
    ::ShiftPressed:=.F.
    If ::Selecting
      If ::BlockTop # ::BlockBottom .OR. ::BlockLeft # ::BlockRight
        ::isBlockOn:=.T.
      Else
        ::BlockOff()
      EndIf
      ::Selecting:=.F.
    EndIf
  EndCase

  Return(self)

*-----------------------------------------------------------------------------*
METHOD Onkey(nKey)

  Local hWnd


  Do Case
  Case nKey == VK_CONTROL
    ::CtrlPressed:=.T.

  Case nKey == VK_INSERT
    ::Onins()

  Case nKey ==VK_CAPITAL
    ::OnCaps()

  Case nKey==VK_ESCAPE
    // hwnd:=GetNextDlgTabItem(getparent(::hWnd),::hWnd,.T.)
    // setfocus(hWnd)
    Return(self)

  Case nKey == VK_SHIFT
    ::ShiftPressed:=.T.

  Otherwise
    ::OnBlockKey(nKey)

  EndCase

  If !::Stable
    ::refreshall()
  Else
    SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
    ::UpdateScroll(.T.,.T.)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnBlockKey(nKey)

  Local isBlockKey:=.F.
  Local nKeyPos:=0

  Do Case

  Case nKey == VK_HOME
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    ::home(.T.)

  Case nKey == VK_END
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    ::End()

  Case nKey == VK_PRIOR
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::gotop()
    Else
      ::pageup()
    EndIf

  Case nKey == VK_NEXT
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::gobottom()
    Else
      ::pagedown()
    EndIf

  Case nKey == VK_LEFT
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::home()
    Else
      ::Left()
    EndIf

  Case nKey == VK_RIGHT
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::End()
    Else
      ::Right()
    EndIf

  Case nKey == VK_UP
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::pagetop()
    Else
      ::up()
    EndIf

  Case nKey == VK_DOWN
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::pagebottom()
    Else
      ::down()
    EndIf

  Case nKey == VK_DELETE
    If ::ShiftPressed
      If !::Selecting
        ::Selecting:=.T.
        ::isBlockOn:=.F.
        ::blockon(::Col,::Line)
      EndIf
      isBlockKey:=.T.
    EndIf
    If ::CtrlPressed
      ::delrest()
    Else
      ::Ondel()
    EndIf

  EndCase

  //view nkey

  If isBlockKey
    ::blockon(::Col,::Line)
  Else
    if (nKeyPos:=ascan(::aKeys,{|x| x[1]==nKey .and. x[2]==::ShiftPressed .and.;
                                   x[3]==::CtrlPressed})) > 0
      if valtype(::aKeys[nKeyPos,4])=='B'
        ::Stable:=eval(::akeys[nKeyPos,4],self)
      endif
    endif
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD Onchar(nchar)

  Static teststr
  Local hWnd

  if !::CtrlPressed

  Do Case
  Case nchar == VK_ESCAPE
    hWnd:=GetNextDlgTabItem(getparent(::hWnd),::hWnd,.T.)
    SetFocus(hWnd)

  Case nchar == VK_BACK                // backspace
    ::Onbackspace()

  Case nchar == VK_TAB               // tab
    ::Ontab()

  Case nchar == VK_RETURN             // carriage Return
    ::Onreturn()

  Case nchar==6                                   //ctrl+F
    ::ChooseFont()

  Otherwise                         // character codes


    If !::ReadOnly
      If ::ExtCaret .AND. Len(::aText[::Line]) < (::Col-1)
        ::aText[::Line]:=PadR(::aText[::Line],::Col-1)
      EndIf
      ::aText[::Line]:=stuff(::aText[::Line],::Col,If(::InsertMode,0,1),chr(nchar))
      ::FirstInvalid:=::Line
      If ::updatecomments(::Line)
        ::Stable:=.F.
      EndIf
      ::Changed:=.T.
      ::Right()
    Else
      MessageBeep(MB_ICONASTERISK)
    EndIf

  EndCase

  ::Selecting:=.F.

  If ::Stable
    ::refreshcurrent()
  Else
    ::refreshall()
  EndIf

  endif

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnVScroll(nwParam,nlParam)

  Do Case

  Case nwParam==SB_LINEUP
    ::pagetop()
    ::up()

  Case nwParam==SB_LINEDOWN
    ::pagebottom()
    ::down()

  Case nwParam==SB_PAGEDOWN
    ::pagedown()

  Case nwParam==SB_PAGEUP
    ::pageup()

  Case nwParam==SB_TOP
    ::gotop()

  Case nwParam==SB_BOTTOM
    ::gobottom()

  Case nwParam==SB_THUMBPOSITION
    ::gotoline(LoWord(nlParam))

  Case nwParam==SB_THUMBTRACK
    ::gotoline(LoWord(nlParam))

  Case nwParam==SB_ENDSCROLL

  EndCase

  If !::Stable
    ::refreshall()
  Else
    SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
    ::UpdateScroll(.T.,.T.)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnHScroll(nwParam,nlParam)

  Local i

  Do Case
  Case nwParam==SB_LINEUP
    ::gotocol(::LeftCol)
    ::Left()

  Case nwParam==SB_LINEDOWN
    ::gotocol(::LeftCol+::MaxCol-1)
    ::Right()

  Case nwParam==SB_PAGEDOWN
    For i:=1 To ::TabSize
      If !::Right()
        Exit
      EndIf
    Next

  Case nwParam==SB_PAGEUP
    For i:=1 To ::TabSize
      If !::Left()
        Exit
      EndIf
    Next

  Case nwParam==SB_TOP
    ::home(.T.)

  Case nwParam==SB_BOTTOM
    ::End()

  Case nwParam==SB_THUMBPOSITION
    ::gotocol(LoWord(nlParam))

  Case nwParam==SB_THUMBTRACK
    ::gotocol(LoWord(nlParam))


  Case nwParam==SB_ENDSCROLL
  EndCase

  If !::Stable
    ::refreshall()
  Else
    SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
    ::UpdateScroll(.T.,.T.)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnCaps()

  ::CapsOn:= (And(GetKeyState(VK_CAPITAL),1) > 0)
  If ValType(::bOnCaps)=='B'
    eval(::bOnCaps,::CapsOn)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD Onreturn()

  Local colpos
  Local i

  if ::lInSummary

    i:=::aSumData[::Line,2]
    ::expand()
    ::gotoline(i)
    Do While !(::TopLine==i)
      If !::down()
        Exit
      EndIf
    EndDo
    ::gotoline(i)
    ::Stable:=.F.

  else

  If ::InsertMode  .AND. !::ReadOnly
    If ::insline(::Line+1)
      //colpos:=len(::atext[::line])-len(ltrim(::atext[::line]))+1
      ::aText[::Line]:=Trim(::aText[::Line])
      If ::isPrg .AND. ::AutoIndent
        colpos:=::getindent(::Line,colpos)-1
        ::aText[::Line+1]:=Space(colpos)+lTrim(SubStr(::aText[::Line],::Col))
      Else
        ::aText[::Line+1]:=SubStr(::aText[::Line],::Col)
      EndIf
      ::aText[::Line]  :=Left(::aText[::Line],::Col-1)
      ::FirstInvalid:=::Line
      ::getcomments(::Line)  // this takes time
      ::down()
      ::home()
    EndIf
  Else
    ::down()
    ::home()
  EndIf

  endif
  Return(self)

*-----------------------------------------------------------------------------*
METHOD Onbackspace()

  If !::ReadOnly
    If ::Col==1
      If ::Line > 1
        ::FirstInvalid:=::Line-1
        ::up()
        ::End()
        ::aText[::Line]+=::aText[::Line+1]
        ::delline(::Line+1)
        ::getcomments(::Line-1)
      EndIf
    Else
      ::aText[::Line]:=stuff(::aText[::Line],::Col-1,1,'')
      ::Changed:=.T.
      If ::updatecomments(::Line)
        ::FirstInvalid:=::Line
        ::Stable:=.F.
      EndIf
      ::Left()
    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD Ondel()

  If !::ReadOnly
    If Empty(::aText[::Line])    // in some editors  - I like it !
      If ::delline(::Line)
        If ::Lines < ::Line
          ::up()
        EndIf
        ::home()
        ::FirstInvalid:=::Line
        ::getcomments(::Line)
      EndIf
    Else

      If ::Col > Len(::aText[::Line])
        If ::Lines > ::Line
          ::aText[::Line]:=PadR(::aText[::Line],::Col-1)
          ::aText[::Line]+=::aText[::Line+1]
          ::delline(::Line+1)
          ::FirstInvalid:=::Line
          ::getcomments(::Line)
        EndIf
      Else
        ::aText[::Line]:=stuff(::aText[::Line],::Col,1,'')
        If ::updatecomments(::Line)
          ::FirstInvalid:=::Line
          ::refreshall()
        Else
          ::refreshcurrent()    //??????????
        EndIf
        ::Changed:=.T.
      EndIf


    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD Ontab()

  Do While .T.
    If ::ShiftPressed  // backtab
      If ::Col > 1
        ::Left()
      EndIf
    Else
      If ::InsertMode .AND. !::ReadOnly
        ::OnChar(32)
        If ::updatecomments(::Line)
          ::FirstInvalid:=::Line
          ::Stable:=.F.
        EndIf
      Else
        ::Right()
      EndIf
    EndIf

    If ::Col % ::TabSize == 1
      Exit
    EndIf
  EndDo

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnIns()

  ::InsertMode:=!::InsertMode
  ::SetCaret()
  If ValType(::bOnIns)=='B'
    eval(::bOnIns,::InsertMode)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD up()

  If ::yPos > 1
    If ::Line<=::Lines
      ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
    EndIf
    ::Line--
    ::yPos--
    If !::ExtCaret //!::blockstyle
      ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
      ::Col:=::xPos+::LeftCol-1
    EndIf
  Else
    If ::scrolldown(1)==0
      Return(.F.)
    EndIf
  EndIf

  Return(.T.)

*-----------------------------------------------------------------------------*
METHOD down()

  If ::yPos <  ::MaxRow             // not on the bottom of the window
    If ::Line < ::Lines             // not on last text line
      ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
      ::Line++
      ::yPos++
      If !::ExtCaret //!::blockstyle
        ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
        ::Col:=::xPos+::LeftCol-1
      EndIf
    Else
      Return(.F.)
    EndIf
  Else
    If ::scrollup(1)==0
      Return(.F.)
    EndIf
  EndIf

  Return(.T.)

*-----------------------------------------------------------------------------*
METHOD left()

  If ::xPos > 1
    ::Col--
    ::xPos--
  Else
    If ::panright(1)==0
      If  !::BlockStyle .AND. ::up()
        ::End()
      Else
        Return(.F.)
      EndIf
    EndIf
  EndIf

  Return(.T.)

*-----------------------------------------------------------------------------*
METHOD right()

  If ::xPos < ::MaxCol
    If ::Col <= If(::ExtCaret,::MaxLineLen,Len(::aText[::Line])) //if(::blockstyle,::maxlinelen,len(::atext[::line]))
      ::Col++
      ::xPos++
    Else
      If (!::ExtCaret) .AND. ::down() //(!::blockstyle) .and. ::down()
        ::home()    // windows style
      Else
        Return(.F.)
      EndIf
    EndIf
  Else
    If ::Col <= If(::ExtCaret,::MaxLineLen,Len(::aText[::Line])) //if(::blockstyle,::maxlinelen,len(::atext[::line]))
      ::panleft(1)
    Else
      If (!::ExtCaret) .AND. ::down() //(!::blockstyle) .and. ::down()
        ::home()   // windows style
      Else
        Return(.F.)
      EndIf
    EndIf
  EndIf

  Return(.T.)

*-----------------------------------------------------------------------------*
METHOD scrollup(n)

  Local i
  Local y:=0

  If ::Line <= ::Lines
    ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
  EndIf
  For i:=1 To n
    If ::BottomLine < ::Lines
      ::TopLine++
      ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
      ::Line++
      y++
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.
    Else
      Exit
    EndIf
  Next

  If y>0 .AND. !::ExtCaret //!::blockstyle
    ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
    ::Col:=::xPos+::LeftCol-1
  EndIf

  Return(y)

*-----------------------------------------------------------------------------*
METHOD scrolldown(n)

  Local i
  Local y:=0

  If ::Line <= ::Lines
    ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
  EndIf
  For i:=1 To n
    If ::TopLine > 1
      ::TopLine--
      ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
      ::Line--
      y++
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.
    Else
      Exit
    EndIf
  Next

  If y > 0 .AND. ::ExtCaret //!::blockstyle
    ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
    ::Col:=::xPos+::LeftCol-1
  EndIf

  Return(y)

*-----------------------------------------------------------------------------*
METHOD panright(n)

  Local i
  Local y:=0

  For i:=1 To n
    If ::LeftCol > 1
      ::LeftCol--
      ::Col--
      y++
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.
    Else
      Exit
    EndIf
  Next

  Return(y)

*-----------------------------------------------------------------------------*
METHOD panleft(n)

  Local i
  Local y:=0

  For i:=1 To n
    If ::Col <= If(::ExtCaret,::MaxLineLen,Len(::aText[::Line]))  //if(::blockstyle,::maxlinelen,len(::atext[::line]))  // Block/Windows style
      ::LeftCol++
      ::Col++
      y++
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.
    Else
      Exit
    EndIf
  Next

  Return(y)

*-----------------------------------------------------------------------------*
METHOD home(unconditional)

  Local colpos

  If unconditional==NIL
    unconditional:=.F.   // respect autoindent if enabled
  EndIf
  If (::isPrg .AND. ::AutoIndent) .AND. !unconditional
    colpos:=::getindent(::Line)
    ::panright(::LeftCol-colpos)
    ::Col:=colpos
    ::xPos:=::Col-::LeftCol+1
  Else
    ::panright(::LeftCol-1)
    ::Col:=1
    ::xPos:=1
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD end()

  ::aText[::Line]:=Trim(::aText[::Line])

  Do Case
  Case Len(::aText[::Line]) <= ::LeftCol
    ::panright(::LeftCol - Len(::aText[::Line]))
  Case Len(::aText[::Line]) > ::LeftCol+ ::MaxCol
    ::panleft(Len(::aText[::Line]) - ::LeftCol - ::MaxCol+2)
  EndCase

  ::Col:=Len(::aText[::Line])+1
  ::xPos:=::Col-::LeftCol+1

  Return(self)

*-----------------------------------------------------------------------------*
METHOD pagetop()

  ::gotoline(::TopLine)

  Return(self)

*-----------------------------------------------------------------------------*
METHOD pagebottom()

  ::gotoline(::BottomLine)

  Return(self)

*-----------------------------------------------------------------------------*
METHOD gotop

  If ::TopLine > 1
    ::FirstInvalid:=1
    ::Stable:=.F.
  EndIf
  ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
  ::Line:=1
  ::TopLine:=1
  ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
  ::yPos:=1
  ::home()

  Return(self)

*-----------------------------------------------------------------------------*
METHOD gobottom

  If  ::BottomLine <  ::Lines
    ::Stable:=.F.
  EndIf
  ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
  ::Line:=::Lines
  ::TopLine:=If(::Lines >= ::MaxRow,::Lines-::MaxRow+1,1)
  ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
  ::yPos:=min(::BottomLine,::MaxRow)
  ::FirstInvalid:=::TopLine
  ::home()

  Return(self)

*-----------------------------------------------------------------------------*
METHOD pageup

  If ::scrolldown(::MaxRow) # ::MaxRow
    ::gotop()
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD pagedown

  If ::scrollup(::MaxRow) # ::MaxRow
    ::gobottom()
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD gotoline(n)

  If !EMPTY(n) .AND. n>0 .AND. n <= ::Lines

    if ::line > 0 .and. ::line <=::lines
      ::aText[::Line]:=Trim(::aText[::Line]) // keep the size down
    endif

    Do Case
    Case n<::TopLine
      ::TopLine:=n
      ::LeftCol:=1
      ::Col:=1
      ::Line:=n
      ::xPos:=1
      ::yPos:=1
      ::BottomLine:=min(::TopLine+::MaxRow-1,::Lines)
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.

    Case n>::BottomLine
      ::BottomLine:=n
      ::LeftCol:=1
      ::Col:=1
      ::Line:=n
      ::xPos:=1
      ::yPos:=::MaxRow
      ::TopLine:=Max(::BottomLine-::MaxRow+1,1)
      ::FirstInvalid:=::TopLine
      ::Stable:=.F.

    Otherwise
      If ::LeftCol > 1
        ::FirstInvalid:=::TopLine
        ::Stable:=.F.
      EndIf
      ::LeftCol:=1
      ::Col:=1
      ::Line:=n
      ::xPos:=1
      ::yPos:=n-::TopLine+1

    EndCase

  EndIf

  Return(.F.)

*-----------------------------------------------------------------------------*
METHOD gotocol(nCol)

  If nCol < ::LeftCol
    ::panright(::LeftCol-nCol)
  ElseIf  nCol > ::LeftCol+ ::MaxCol
    ::panleft(nCol - ::LeftCol - ::MaxCol+1)
  EndIf

  ::Col:=nCol
  ::xPos:=::Col-::LeftCol+1

  Return(self)

*-----------------------------------------------------------------------------*
METHOD GetWord()

  Local awords:=parsexpr(::aText[::Line],.T.,.F.)
  Local i,nCount:=0
  Local cWord:=''

  For i:=1 To Len(awords)
    If (nCount+=Len(awords[i])) >= ::Col
      cWord:=awords[i]
      Exit
    EndIf
  Next
  Return(cWord)

*-----------------------------------------------------------------------------*
METHOD DelRest()

  If !::ReadOnly
    ::aText[::Line]:=Left(::aText[::Line],::Col-1)
    ::Changed:=.T.
    If ::updatecomments(::Line)
      ::refreshall( ,::Line)
    Else
      ::RefreshCurrent()
    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf
  Return(self)

*-----------------------------------------------------------------------------*
METHOD DupLine

  If !::ReadOnly
    If ::insline(::Line+1)
      ::aText[::Line+1]:=::aText[::Line]
      ::Changed:=.T.
      ::getcomments(::Line)
      ::FirstInvalid:=::Line+1
      ::stable:=.f.
    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD getindent(nLine,oldcol)

  Local colpos:=1
  Local ctemp

  If Empty(::aText[nLine])
    If nLine > 1

      colpos+=Len(::aText[nLine-1])-Len(lTrim(::aText[nLine-1]))
      ctemp:=AllTrim(::aText[nLine-1])+' '

      If (Len(ctemp) > 2 .AND. Upper(Left(ctemp, 3)) $  'IF  DO ' ) .OR. ;
        (Len(ctemp) > 3 .AND. Upper(Left(ctemp, 4)) == 'FOR ' ) .OR. ;
        (Len(ctemp) > 3 .AND. Upper(Left(ctemp, 5)) $  'LOOP CASE ELSE EXIT FUNC PROC' ) .OR. ;
        (Len(ctemp) > 4 .AND. Upper(Left(ctemp, 6)) $  'BEGIN WHILE CLASS BREAK') .OR. ;
        (Len(ctemp) > 5 .AND. Upper(Left(ctemp, 7)) $  'ELSEIF METHOD ' ) .OR. ;
        (Len(ctemp) > 6 .AND. Upper(Left(ctemp, 8)) $  'RECOVER ' ) .OR. ;
        (Len(ctemp) > 7 .AND. Upper(Left(ctemp, 9)) == 'FUNCTION ' ) .OR. ;
        (Len(ctemp) > 8 .AND. Upper(Left(ctemp,10)) $  'OTHERWISE PROCEDURE ') .OR. ;
        (Len(ctemp) >14 .AND. Upper(Left(ctemp,12)) == 'STATIC FUNC ' ) .OR. ;
        (Len(ctemp) >14 .AND. Upper(Left(ctemp,12)) == 'STATIC PROC ' ) .OR. ;
        (Len(ctemp) >14 .AND. Upper(Left(ctemp,16)) == 'STATIC FUNCTION ' ) .OR. ;
        (Len(ctemp) >15 .AND. Upper(Left(ctemp,17)) == 'STATIC PROCEDURE ' )

        colpos+=::IndentSize
      Else
        //colpos:=oldcol
      EndIf
    EndIf
  Else
    colpos+=Len(::aText[nLine])-Len(lTrim(::aText[nLine]))
  EndIf

  Return(colpos)

*-----------------------------------------------------------------------------*
METHOD FindMatch()

  // add clipper constructs, like do..end, if..end, etc.

  Local amatch:={'{}','[]','()'}
  Local curchar:=substr(::aText[::Line],::Col,1)
  Local x:=::Col
  Local n,l,c,y,i
  Local lookfor
  Local stackcount:=1
  Local lFound:=.F.
  Local cTemp

  //VIEW 'IN FIND MATCH',curchar

  If !Empty(curchar)
    If (n:=aScan(amatch,{|x| curchar $ x})) > 0
      l:=::Line
      If curchar==Left(amatch[n],1)
        lookfor:=SubStr(amatch[n],2)
        x++

        Do While l <= ::Lines

          cTemp:=::aText[l]
          y:=Len(cTemp)

          For i:=x To y
            c:=substr(cTemp,i,1)
            If c==curchar
              stackcount++
            ElseIf c==lookfor
              stackcount--
            EndIf
            If stackcount==0
              lFound:=.T.
              Exit
            EndIf
          Next

          If stackcount==0
            Exit
          EndIf

          x:=1
          l++

        EndDo

      Else
        lookfor:=Left(amatch[n],1)
        x--
        cTemp:=::aText[l]

        Do While l >= 1
          For i:=x To 1 Step -1
            c:=substr(cTemp,i,1)
            If c==curchar
              stackcount++
            ElseIf c==lookfor
              stackcount--
            EndIf
            If stackcount==0
              lFound:=.T.
              Exit
            EndIf
          Next

          If stackcount==0
            Exit
          EndIf

          if (--l) > 0
            cTemp:=::aText[l]
            x:=Len(cTemp)
          endif

        EndDo
      EndIf

      If lFound
        ::gotoline(l)
        ::gotocol(i)

        If !::Stable
          ::refreshall()
        Else
          SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
          ::UpdateScroll(.T.,.T.)
        EndIf

      EndIf

    EndIf
  EndIf

  Return(lFound)

*-----------------------------------------------------------------------------*
METHOD FindText(cFind,lMatchCase,lDown,lFromTop)

  Local nStartLine:=::Line
  Local nStartCol:=::Col
  Local nStep
  Local nLimit
  Local i
  Local lFound:=.F.
  Local nFoundCol
  Local cSeek
  Local nAns:=0

  If !Empty(cFind)

    // defaults

    If ValType(lFromTop) # 'L'
      lFromTop:=.T.
    EndIf
    If ValType(lMatchCase) # 'L'
      lMatchCase:=.F.
    EndIf
    If ValType(lDown) # 'L'
      lDown:=.T.
    EndIf

    If lFromTop
      nStartLine:=1
      nStartCol:=1
    EndIf

    If !lMatchCase
      cFind:=Upper(cFind)
    EndIf

    nStep:=If(lDown,1,-1)
    nLimit:=If(lDown,::Lines,1)

    // searching

    For i:=nStartLine To nLimit Step nStep
      If i==nStartLine
        If lDown
          cSeek:=SubStr(::aText[i],nStartCol)
        Else
          cSeek:=Left(::aText[i],nStartCol-1)
        EndIf
      Else
        cSeek:=::aText[i]
      EndIf

      If !lMatchCase
        cSeek:=Upper(cSeek)
      EndIf

      If lDown
        nFoundCol:=at(cFind,cSeek)
      Else
        nFoundCol:=rat(cFind,cSeek)
      EndIf

      If nFoundCol > 0
        lFound:=.T.

        ::isBlockOn:=.T.
        ::BlockLeft:=nFoundCol+If(i==nStartLine,nStartCol-1,0)
        ::BlockTop :=i
        ::BlockRight:=nFoundCol+If(i==nStartLine,nStartCol-1,0)+Len(cFind)
        ::BlockBottom:=i
        ::gotoline(i)
        ::gotocol(nFoundCol+If(i==nStartLine,nStartCol-1,0)+Len(cFind))

        If ValType(::OnBlockOn)=='B'
          eval(::OnBlockOn,self)
        EndIf

        ::Stable:=.F.
        ::refreshall()
        Exit

      EndIf
    Next

  EndIf

  Return(lFound)

*-----------------------------------------------------------------------------*
METHOD ReplaceText(cFind,lMatchCase,lDown,cRepl,lGlobal,lNoPrompt)

  Local nStartLine:=::Line
  Local nStartCol:=::Col
  Local nStep
  Local nLimit
  Local i
  Local nFound:=1
  Local nFoundCol
  Local cSeek
  Local nAns:=0
  Local lFromTop:=.F.

  If !::ReadOnly
    If ValType(cRepl) =='C'

      // defaults

      If ValType(lMatchCase) # 'L'
        lMatchCase:=.F.
      EndIf

      If ValType(lDown) # 'L'
        lDown:=.T.
      EndIf

      If !lMatchCase
        cFind:=Upper(cFind)
      EndIf

      If (ValType(lGlobal) # 'L')
        lGlobal:=.F.
      EndIf

      If (ValType(lNoPrompt) # 'L')
        lNoPrompt:=.F.
      EndIf


      ::delText()
      ::settext(cRepl)

      If lGlobal
        If !lNoPrompt
          If !::find(cFind,lMatchCase,lDown,.F.)
            nFound:=0
          EndIf
        Else
          Do While ::find(cFind,lMatchCase,lDown,.F.)
            nFound++
            ::delText()
            ::settext(cRepl)
          EndDo
        EndIf
      EndIf

      ::getcomments()


    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(nFound)

*-----------------------------------------------------------------------------*
METHOD insline(nLine)

  If !::ReadOnly
    nline:=max(nLine,1)
//    If ::Lines < 4096
      asize(::aText,::Lines+1)
      ::Lines++
      ains(::aText,nLine)
      ::aText[nLine]:=''
      ::Changed:=.T.
      ::Stable:=.F.
      Return(.T.)
//    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(.F.)

*-----------------------------------------------------------------------------*
METHOD delline(nLine)

  If !::ReadOnly
    If ::Lines>0 .AND. nLine > 0 .AND. nLine<=::Lines
      ::cLineBuff:=::aText[nLine]
      adel(::aText,nLine)
      asize(::aText,::Lines-1)
      ::Lines--
      ::Changed:=.T.
      ::firstinvalid:=nline
      ::Stable:=.F.
      Return(.T.)
    EndIf
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(.F.)

*------------------------------------------------------------------------------*
METHOD UndelLine() // to replace by undo ?

   if !::readonly
    if ::cLineBuff # NIL
        if ::insline(::line)
          ::atext[::Line]:=::cLineBuff
          ::firstinvalid:=::line
          return(.T.)
        endif
    endif
  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf


Return(.F.)


*-----------------------------------------------------------------------------*
METHOD settext(cText)


  Local actext    :=str2a(cText,CRLF)
  Local ctextlines:=Len(actext)
  Local i
  Local clinerest
  Local nleft
  Local maxwidth:=0
  Local lSuccess:=.T.

  If !::ReadOnly

    ::isBlockOn:=.F.
    ::BlockLeft:=::Col
    ::BlockTop :=::Line

    If ::BlockStyle

      aeval(actext,{|x| maxwidth:=Max(maxwidth,Len(x))})
      For i:=1 To ctextlines
        ::aText[::Line]:=PadR(::aText[::Line],Max(::Col-1,Len(::aText[::Line])))
        If ::InsertMode
          ::aText[::Line]:=stuff(::aText[::Line],::Col,0,PadR(actext[i],maxwidth))
        Else
          ::aText[::Line]:=Left(::aText[::Line],::Col-1)+PadR(actext[i],maxwidth)+;
          SubStr(::aText[::Line],::Col)
        EndIf
        If i < ctextlines
          ::Line++
          If ::Line > ::Lines
            aAdd(::aText,'')
            ::Lines++
          EndIf
        EndIf
      Next

      ::gotoline(::Line)
      ::gotocol(::BlockLeft+maxwidth)

    Else

      clinerest:=SubStr(::aText[::Line],::Col)
      ::aText[::Line]:=Pad(::aText[::Line],::Col-1)
      For i:=1 To ctextlines
        ::aText[::Line]+=actext[i]
        If i < ctextlines
          ::Line++
          aAdd(::aText,)
          ::Lines++
          ains(::aText,::Line)
          ::aText[::Line]:=''
        EndIf
      Next

      ::gotoline(::Line)
      ::gotocol(Len(::aText[::Line])+1)
      ::aText[::Line]+=clinerest

    EndIf

    ::BlockRight :=::Col
    ::BlockBottom:=::Line

    If ValType(::OnBlockOn)=='B'
      eval(::OnBlockOn,self)
    EndIf

    ::getcomments(::BlockTop)
    ::Stable:=.F.
    ::Changed:=.T.
    //setfocus(::hWnd)
    ::refreshall()

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(lSuccess)

*-----------------------------------------------------------------------------*
METHOD gettext

  Local ctext:=''
  Local actext:={}
  Local i

  If ::isBlockOn
    actext:=array(::BlockBottom-::BlockTop+1)

    If ::BlockStyle
      For i:=::BlockTop To ::BlockBottom
        actext[i-::BlockTop+1]:=SubStr(::aText[i],::BlockLeft,::BlockRight-::BlockLeft)
      Next
    Else
      If Len(actext) == 1
        actext[1]:=SubStr(::aText[::BlockTop],::BlockLeft,::BlockRight-::BlockLeft)
      Else
        actext[1]:=SubStr(::aText[::BlockTop],::BlockLeft)
        For i:=::BlockTop+1 To ::BlockBottom-1
          actext[i-::BlockTop+1]:=::aText[i]
        Next
        actext[Len(actext)]:=Left(::aText[::BlockBottom],::BlockRight-1)
      EndIf
    EndIf
  EndIf

  ctext:=a2str(actext,chr(13)+chr(10))

  Return(ctext)

*-----------------------------------------------------------------------------*
METHOD deltext

  Local i

  //view 'in deltext'

  If !::ReadOnly

    If ::isBlockOn
      If ::BlockStyle
        For i:=::BlockTop To ::BlockBottom
          ::aText[i]:=Left(::aText[i],::BlockLeft-1)+SubStr(::aText[i],::BlockRight)
        Next
      Else
        ::aText[::BlockTop]:=Left(::aText[::BlockTop],::BlockLeft-1)+;
        SubStr(::aText[::BlockBottom],::BlockRight)
        For i:=::BlockTop+1 To ::BlockBottom
          adel(::aText,::BlockTop+1)
        Next
        asize(::aText,Len(::aText)-Max(0,::BlockBottom-::BlockTop))
      EndIf
      ::Lines:=Len(::aText)
      ::getcomments(::BlockTop)
      ::BlockBottom:=::BlockTop
      ::BlockRight :=::BlockLeft
      //view ::BlockTop,::lines
      ::gotoline(::BlockTop)
      ::gotocol(::BlockLeft)
      ::Changed:=.T.
      ::Stable:=.F.

    EndIf

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD selectall()

  ::isBlockOn:=.T.
  ::BlockLeft:=1
  ::BlockTop :=1
  ::BlockRight:=Len(::aText[::Lines])
  ::BlockBottom:=::Lines
  ::Stable:=.F.

  If ValType(::OnBlockOn)=='B'
    eval(::OnBlockOn,self)
  EndIf

  ::refreshall()

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD blockgetlen()

  Local nLen:=0
  Local i

  If ::isBlockOn

    If ::BlockStyle
      nLen:=(::BlockBottom-::BlockTop+1)*(::BlockRight-::BlockLeft+2)-2
    Else
      If ::BlockBottom > ::BlockTop
        nLen:=Len(SubStr(::aText[::BlockTop],::BlockLeft,::BlockRight-::BlockLeft))
      Else
        nLen:=Len(SubStr(::aText[::BlockTop],::BlockLeft))+2
        For i:=::BlockTop+1 To ::BlockBottom-1
          nLen+=(Len(::aText[i])+2)
        Next
        nLen+=(::BlockRight-1)
      EndIf
    EndIf
  EndIf

  Return(nLen)

*-----------------------------------------------------------------------------*
METHOD cut()

  Local ctext
  Local retval:=.F.

  If !::ReadOnly

    If ::isBlockOn
      If ::blockgetlen() < 65534
        If OpenClipboard(::hWnd)
          ctext:=::gettext()
          // view ctext
          if EmptyClipboard()
          if SetClipboardData(CF_TEXT,ctext+chr(0)) # 0
            ::DelText()
            ::RefreshAll()
            retval:=.T.
          endif
          endif
          CloseClipboard()
        EndIf
      EndIf

    EndIf

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  // view retval

  Return(retval)

*-----------------------------------------------------------------------------*
METHOD copy()

  Local ctext
  Local retval:=.F.

  If ::isBlockOn()
    If ::blockgetlen() < 65534
      If OpenClipboard(::hWnd)
        ctext:=::gettext()
     //   view ctext
        if EmptyClipboard()
        if SetClipboardData(CF_TEXT,ctext+chr(0)) # 0
          retval:=.t.
        endif
        endif
        CloseClipboard()
      EndIf
    EndIf

  EndIf

  //view retval

  Return(retval)

*-----------------------------------------------------------------------------*
METHOD paste()

  Local cText
  Local CRLFlen:=2
  Local nlines
  Local retval:=.F.

  If !::ReadOnly

    If OpenClipboard(::hWnd)
      cText:=GetClipboardData(CF_TEXT)
    //  view cText
      CloseClipboard()
      If cText # NIL
        nlines:=(Len(cText)-Len(StrTran(cText,CRLF,'')))/CRLFlen +1
        If ::Lines+nlines-If(::isBlockOn,::BlockBottom-::BlockTop,0) < 4095
          ::deltext()
          ::settext(cText)
          retval:=.T.
        EndIf
      EndIf
    EndIf

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  //view retval

  Return(retval)

*-----------------------------------------------------------------------------*
METHOD toupper()

  Local i

  If !::ReadOnly

    If ::isBlockOn
      If ::BlockStyle
        For i:=::BlockTop To ::BlockBottom
          ::aText[i]:=Left(::aText[i],::BlockLeft-1)+;
          Upper(SubStr(::aText[i],::BlockLeft,::BlockRight-::BlockLeft))+;
          SubStr(::aText[i],::BlockRight)
        Next
      Else
        If ::BlockTop==::BlockBottom
          ::aText[::BlockTop]:=Left(::aText[::BlockTop],::BlockLeft-1)+;
          Upper(SubStr(::aText[::BlockTop],::BlockLeft,;
                       ::BlockRight-::BlockLeft))+;
          SubStr(::aText[::BlockTop],::BlockRight)
        Else
          ::aText[::BlockTop]:=Left(::aText[::BlockTop],::BlockLeft-1)+;
          Upper(SubStr(::aText[::BlockTop],::BlockLeft))
        EndIf

        For i:=::BlockTop+1 To ::BlockBottom-1
          ::aText[i]:=Upper(::aText[i])
        Next
        If ::BlockBottom > ::BlockTop
          ::aText[::BlockBottom]:=Upper(Left(::aText[::BlockBottom],::BlockRight-1))+;
          SubStr(::aText[::BlockBottom],::BlockRight)

        EndIf
      EndIf

      ::Changed:=.T.
      ::Stable:=.F.

    EndIf

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD tolower()

  Local i

  If !::ReadOnly

    If ::isBlockOn
      If ::BlockStyle
        For i:=::BlockTop To ::BlockBottom
          ::aText[i]:=Left(::aText[i],::BlockLeft-1)+;
          Lower(SubStr(::aText[i],::BlockLeft,::BlockRight-::BlockLeft))+;
          SubStr(::aText[i],::BlockRight)
        Next
      Else
        If ::BlockTop==::BlockBottom
          ::aText[::BlockTop]:=Left(::aText[::BlockTop],::BlockLeft-1)+;
          Lower(SubStr(::aText[::BlockTop],::BlockLeft,;
                       ::BlockRight-::BlockLeft))+;
          SubStr(::aText[::BlockTop],::BlockRight)
        Else
          ::aText[::BlockTop]:=Left(::aText[::BlockTop],::BlockLeft-1)+;
          Lower(SubStr(::aText[::BlockTop],::BlockLeft))
        EndIf

        For i:=::BlockTop+1 To ::BlockBottom-1
          ::aText[i]:=Lower(::aText[i])
        Next
        If ::BlockBottom > ::BlockTop
          ::aText[::BlockBottom]:=Lower(Left(::aText[::BlockBottom],::BlockRight-1))+;
          SubStr(::aText[::BlockBottom],::BlockRight)

        EndIf
      EndIf

      ::Changed:=.T.
      ::Stable:=.F.

    EndIf

  Else
    MessageBeep(MB_ICONASTERISK)
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD isBlockVisible()

  Local awindow:={::LeftCol,::TopLine,::LeftCol+::MaxCol-1,::TopLine+::MaxRow-1}

  If ::isBlockOn .AND. ;
    ptinrect(awindow,{::BlockLeft,::BlockTop})    .OR.;
    ptinrect(awindow,{::BlockLeft,::BlockBottom}) .OR.;
    ptinrect(awindow,{::BlockRight,::BlockTop})   .OR.;
    ptinrect(awindow,{::BlockRight,::BlockBottom})
    Return(.T.)
  EndIf

  Return(.F.)

*-----------------------------------------------------------------------------*
METHOD BlockOff()

  If ::isBlockOn
    If ::isblockvisible()
      ::Stable:=.F.
      ::FirstInvalid:=Max(::TopLine,::BlockTop)
    EndIf
    ::isBlockOn:=.F.
  EndIf

  ::BlockLeft  :=0
  ::BlockRight :=0
  ::BlockTop   :=0
  ::BlockBottom:=0

  ::xBlockStart:=::Col
  ::yBlockStart:=::Line

  If ValType(::OnBlockOff)=='B'
    eval(::OnBlockOff,self)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD BlockOn(x,y)

  Local wasblock:=.F.

  If !::isBlockOn
    ::isBlockOn:=.T.
    ::BlockOff()
  Else
    wasblock:=.T.
  EndIf

  If ::BlockStyle
    ::BlockLeft  :=min(::xBlockStart,x)
    ::BlockRight :=Max(::xBlockStart,x)
    ::BlockTop   :=min(::yBlockStart,y)
    ::BlockBottom:=Max(::yBlockStart,y)
  Else

    // validate coordinates as in OnMouseDown

    If y < ::yBlockStart
      ::BlockTop   :=y
      ::BlockBottom:=::yBlockStart
      ::BlockLeft  :=x
      ::BlockRight :=::xBlockStart

    ElseIf y > ::yBlockStart

      ::BlockTop   :=::yBlockStart
      ::BlockBottom:=y
      ::BlockLeft  :=::xBlockStart
      ::BlockRight :=x

    Else
      ::BlockLeft  :=min(::xBlockStart,x)
      ::BlockRight :=Max(::xBlockStart,x)
      ::BlockTop   :=::yBlockStart
      ::BlockBottom:=::yBlockStart

    EndIf

  EndIf

  If !wasblock
    If ValType(::OnBlockOn)=='B'
      eval(::OnBlockOn,self)
    EndIf
  EndIf

  ::isBlockOn:=.T.

  ::Stable:=.F.

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnMouseDown(x,y,fwKeys)

  Local nH,nV

  SetCapture(::hWnd)

  ::yPos:=Int(y/::cyChar)+1
  ::xPos:=Int(x/::cxChar)+1
  ::aText[::Line]:=Trim(::aText[::Line])

  If ::BlockStyle
    ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
    ::Line:=min(::TopLine+::yPos-1,::lines)
    ::Col:=::xPos+::LeftCol-1
  Else
    ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
    ::Line:=min(::TopLine+::yPos-1,::lines)
    ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
    ::Col:=::xPos+::LeftCol-1
  EndIf

  ::isBlockOn:=.T.
  ::Selecting:=.T.
  ::BlockOff()     // clear any block

  If ::ExtCaret  .AND. !::BlockStyle
    ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
    ::xPos:=Int(x/::cxChar)+1
    ::Line:=min(::TopLine+::yPos-1,::lines)
    ::Col:=::xPos+::LeftCol-1
  EndIf

  If !::Stable
    ::refreshall()
  Else
    SetCaretPos((::xPos-1)*::cxChar,(::yPos-1)*::cyChar)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnMouseUp(x,y,fwKeys)

  If ::Selecting

    ::yPos:=Int(y/::cyChar)+1
    ::xPos:=Int(x/::cxChar)+1

    If ::ExtCaret //::blockstyle
      ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
      ::Line:=min(::TopLine+::yPos-1,::lines)
      ::Col:=::xPos+::LeftCol-1
    Else
      ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
      ::Line:=min(::TopLine+::yPos-1,::lines)
      ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
      ::Col:=::xPos+::LeftCol-1
    EndIf

    If ::BlockTop # ::BlockBottom .OR. ::BlockLeft # ::BlockRight
      ::isBlockOn:=.T.
    Else
      ::BlockOff()
    EndIf

    ::Selecting:=.F.

  EndIf

  ReleaseCapture()
  ::RefreshCurrent()

  Return(self)

*-----------------------------------------------------------------------------*
METHOD OnMouseMove(x,y,fwKeys)

  If ::Selecting

    If x > 35534   // arbitrary but less than 65534 (C4W problem)
      ::Left()
    ElseIf x > ::xWnd
      ::Right()
    Else
      ::xPos:=Int(x/::cxChar)+1
    EndIf

    If y > 35534  // arbitrary but less than 65534  (C4W problem)
      ::up()
    ElseIf y > ::yWnd
      ::down()
    Else
      ::yPos:=Int(y/::cyChar)+1
    EndIf


    If ::BlockStyle
      ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
      ::Line:=min(::TopLine+::yPos-1,::lines)
      ::Col:=::xPos+::LeftCol-1
    Else
      ::yPos:=min(::yPos,::BottomLine-::TopLine+1)
      ::Line:=min(::TopLine+::yPos-1,::lines)
      ::xPos:=min(::xPos,Len(::aText[::Line])-::LeftCol+2)
      ::Col:=::xPos+::LeftCol-1
    EndIf

    ::blockOn(::Col,::Line)

    ::refreshall()

  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD refreshall(hDC,nFrom)

  Local i
  Local noDC:=.F.
  Local lInComment:=.F.
  Local n

  // view procname(1)

  If hDC==NIL
    noDC:=.T.
    hDC:=GetDC(::hWnd)
  EndIf

  If nFrom # NIL
    ::FirstInvalid:=nFrom
  EndIf

  If ::lInSummary

    For i := ::FirstInvalid-::TopLine+1 To ::MaxRow+1
      lInComment:=If(::TopLine+i-1 > ::Lines,.F.,::aSumData[::TopLine+i-1,1])
      ::drawline(i,::TopLine+i-1,lInComment)
    Next

  Else

    If (n:=aScan(::aComments,{|x| x[1] < ::FirstInvalid })) > 0
      lInComment:=::aComments[n,3]
    EndIf

    For i := ::FirstInvalid-::TopLine+1 To ::MaxRow+1
      ::drawline(i,::TopLine+i-1,@lInComment)
    Next

  EndIf

  HideCaret(::hWnd)
  ::paint(hDC)

  ::FirstInvalid:=::TopLine
  ::Stable:=.T.
  ::UpdateScroll(.T.,.T.)

  If ::hWnd==GetFocus()
    ::SetCaret()
  EndIf

  If noDC
    ReleaseDC(::hWnd,hDC)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD refreshcurrent(hDC,i)

  Local j
  Local noDC:=.F.
  Local lInComment:=.F.
  Local n

  If hDC==NIL
    noDC:=.T.
    hDC:=GetDC(::hWnd)
  EndIf

  If i==NIL .OR. i==0
    i:=::yPos
  EndIf

  If ::lInSummary
    lInComment:=If(::TopLine+i-1 > ::Lines,.F.,::aSumData[::TopLine+i-1,1])
    ::drawline(i,::TopLine+i-1,lInComment)
  Else
    If (n:=aScan(::aComments,{|x| x[1]< ::TopLine+i-1 })) > 0
      lInComment:=::aComments[n,3]
    EndIf
    ::drawline(i,::TopLine+i-1,lInComment)
  EndIf

  HideCaret(::hWnd)
  ::paint(hDC,i,i)

  ::Stable:=.T.
  ::UpdateScroll(.T.,.T.)

  If ::hWnd==GetFocus()
    ::SetCaret()
  EndIf

  If noDC
    ReleaseDC(::hWnd,hDC)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
* updates comments of the whole file or down from line nline (if supplied)
*-----------------------------------------------------------------------------*
METHOD getcomments(nLine)

  Local nOpen
  Local nClose
  Local nInline:=0
  Local i,j
  Local c
  Local n:=0
  Local nLenLine
  Local isSingle  // quotes
  Local isDouble  // quotes
  Local ch

  If ::doColor

    If Empty(nLine)
      nLine:=1
      ::aComments:=array(1000)
    Else
      i:=Len(::aComments)
      Do While i > 0
        If ::aComments[i,1] >= nLine
          adel(::aComments,1)
          i--
        Else
          Exit
        EndIf
      EndDo
      asize(::aComments,1000)
    EndIf

    For i:=nLine To Len(::aText)

      c     :=::aText[i]
      nOpen :=-1
      nClose:=-1
      If !Left(lTrim(c),1)=="*".AND.!Left(lTrim(c),2)=="*/"
        Do While (nOpen:=atn("/*",c,nOpen+2)) > 0
          nInline:=atn("//",c)
          If nInline == 0 .OR. nInline > nOpen

            // check if not part of the string !!!!!!!!!!

            isDouble:=.F.
            isSingle:=.F.
            nLenLine:=Len(c)

            For j:=nLenLine To nOpen+2 Step -1

              ch:=substr(c,j,1)

              If ch=='"'
                If !isSingle
                  isDouble:=!isDouble
                EndIf
              EndIf
              If ch=="'"
                If !isDouble
                  isSingle:=!isSingle
                EndIf
              EndIf

            Next

            If !( isDouble .OR. isSingle )

              ::aComments[++n]:={i,nOpen,.T.}

            EndIf

          EndIf
        EndDo

        Do While (nClose:=atn("*/",c,nClose+2)) > 0
          nInline:=atn("//",c)
          If nInline == 0 .OR. nInline > nClose
            ::aComments[++n]:={i,nClose,.F.}        // here it doesn't matter if
            // it is within string
          EndIf
        EndDo

      EndIf

    Next

    asize(::aComments,n)
    ::aComments:=aSort(::aComments,,,{|x,y| If(x[1]>y[1],.T.,If(x[1]=y[1],x[2]>y[2],.F.))})

  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
* updates only current line comment status
*-----------------------------------------------------------------------------*
METHOD updatecomments(i)

  Local nOpen:=-1
  Local nClose:=-1
  Local c,n
  Local Changed:=.F.
  Local nInline:=0
  Local isSingle
  Local isDouble
  Local ch
  Local nLenLine
  Local j

  If ::doColor

    Do While (n:=aScan(::aComments,{|x,y| x[1]==i})) > 0
      adel(::aComments,n)
      asize(::aComments,Len(::aComments)-1)
      Changed:=.T.
    EndDo

    c:=::aText[i]

    Do While (nOpen:=atn('/*',c,nOpen+2,1)) > 0
      nInline:=atn('//',c)
      If nInline == 0 .OR. nInline > nOpen

        // check if not part of the string !!!!!!!!!!

        isDouble:=.F.
        isSingle:=.F.
        nLenLine:=Len(c)

        For j:=nLenLine To nOpen+2 Step -1

          ch:=substr(c,j,1)

          If ch=='"'
            If !isSingle
              isDouble:=!isDouble
            EndIf
          EndIf
          If ch=="'"
            If !isDouble
              isSingle:=!isSingle
            EndIf
          EndIf

        Next

        If !( isDouble .OR. isSingle )
          Changed:=.T.
          aAdd(::aComments,{i,nOpen,.T.})
        EndIf

      EndIf
    EndDo

    Do While (nClose:=atn("*/",c,nClose+2,1)) > 0
      nInline:=atn("//",c)
      If nInline == 0 .OR. nInline > nClose
        Changed:=.T.
        aAdd(::aComments,{i,nClose,.F.})
      EndIf
    EndDo

    If Changed
      ::aComments:=aSort(::aComments,,,{|x,y| If(x[1]>y[1],.T.,If(x[1]=y[1],x[2]>y[2],.F.))})
    EndIf

  EndIf

  Return(Changed)

*-----------------------------------------------------------------------------*
METHOD drawline(i,nowline,lInComment)

  Local j
  Local oldtextcolor
  Local oldbkcolor
  Local xPos
  Local startline:=1
  Local oldfont
  Local starthilite
  Local stophilite
  Local t
  Local arect
  Local a
  Local nWritten:=0
  Local strlen
  Local ctext

  SetBkColor(::hMemDC,RGB(255,255,255))
  SetTextColor(::hMemDC,::nColor)

  If nowline<=::Lines

    ctext:=::aText[nowline]
    xPos:=0

    If Empty(ctext)

      TextOut(::hMemDC,0,(i-1)*::cyChar,Space(::MaxCol+1))

    Else

      If nowline==::Mark
        SetBkColor(::hMemDC,::aColors[6])
      EndIf

      If ::isPrg .AND. ::doColor

        ccolorcode(ctext,@lInComment,::hMemDC,::LeftCol,::cxChar,::cyChar,;
                   i,::MaxCol,::aColors)


      Else // no color
        TextOut(::hMemDC,0,(i-1)*::cyChar,;
                PadR(SubStr(ctext,::LeftCol),::MaxCol+::LeftCol))
      EndIf

      If nowline==::Mark
        SetBkColor(::hMemDC,RGB(192,192,192))
      EndIf

    EndIf


    // paint block

    If ::isBlockOn .AND. (nowline >= ::BlockTop .AND. ;
                          nowline <= ::BlockBottom )
      starthilite:=::BlockLeft
      stophilite:=::BlockRight

      If nowline==::BlockTop
        If !::BlockStyle
          If ::BlockBottom > nowline
            stophilite :=::MaxCol+1 //len(::atext[nowline])+1
          EndIf
        EndIf

      ElseIf nowline==::BlockBottom
        If !::BlockStyle
          If ::BlockTop < nowline
            starthilite:=1
          EndIf
        EndIf


      Else
        If !::BlockStyle
          starthilite:=1
          stophilite:=::MaxCol+1 //len(::atext[nowline])+1
        EndIf

      EndIf


      // highlight


      oldtextcolor:=SetTextColor(::hMemDC,GetSysColor(COLOR_HIGHLIGHTTEXT))
      oldbkcolor  :=SetBkColor(::hMemDC,GetSysColor(COLOR_HIGHLIGHT))

      ExtTextOut(::hMemDC,min(::cxChar*Max(0,starthilite-::LeftCol),::xWnd),;
                 (i-1)*::cyChar,ETO_OPAQUE,;
                 {min(::cxChar*Max(0,starthilite-::LeftCol),::xWnd),;
                  (i-1)*::cyChar,;
                  min(::cxChar*Max(0,stophilite-::LeftCol),::xWnd),;
                  i*::cyChar},;
                 SubStr(::aText[nowline],Max(::LeftCol,starthilite),;
                        stophilite-Max(::LeftCol,starthilite)))

      SetBkColor(::hMemDC,oldbkcolor)
      SetTextColor(::hMemDC,::nColor)

    EndIf

  Else

    TextOut(::hMemDC,0,(i-1)*::cyChar,Space(::MaxCol+1))
  EndIf

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD paint(hDC,fromline,toline)

  Local nPaintStart
  Local nPaintStop

  If Empty(fromline)
    fromline:=::FirstInvalid-::TopLine+1
  EndIf
  If Empty(toline)
    toline:=::MaxRow+1
  EndIf
  nPaintStart:=(fromline-1) * ::cyChar
  nPaintStop := toline * ::cyChar

  BitBlt(hDC,0,nPaintStart,::xWnd,nPaintStop,::hMemDC,0,nPaintStart,SRCCOPY)

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD UpdateScroll(lHorz,lVert)

  Local i

  If lHorz

    SetScrollRange(::hWnd,SB_HORZ,1,::MaxLineLen,.F.)
    SetScrollPos(::hWnd,SB_HORZ,::Col,.T.)

    If ::bOnColChange # NIL
      eval(::bOnColChange,::Col)
    EndIf
  EndIf

  If lVert
    SetScrollRange(::hWnd,SB_VERT,1,::Lines,.F.)
    SetScrollPos(::hWnd,SB_VERT,::Line,.T.)

    If ::lInSummary
      i:=::Mark
      ::Mark:=::Line
      If i # ::Mark
        ::refreshcurrent(,i-::TopLine+1)
        ::refreshcurrent(,::Mark-::TopLine+1)
      EndIf
    EndIf

    If ::bOnLineChange # NIL
      eval(::bOnLineChange,::Line,::Lines)
    EndIf

  EndIf

  Return(self)

*-----------------------------------------------------------------------------*
METHOD collapse()

  Local cline
  Local i
  Local n
  Local lInComment:=.F.

  //view procname(1),procline(1)

  ::aPrgState:=array(PRG_STATELEN)
  ::aPrgState[PRG_LEFTCOL    ] := ::LeftCol
  ::aPrgState[PRG_TOPLINE    ] := ::TopLine
  ::aPrgState[PRG_COL        ] := ::Col
  ::aPrgState[PRG_LINE       ] := ::Line
  ::aPrgState[PRG_ISBLOCK    ] := ::isBlockOn
  ::aPrgState[PRG_BLOCKTOP   ] := ::BlockTop
  ::aPrgState[PRG_BLOCKLEFT  ] := ::BlockLeft
  ::aPrgState[PRG_BLOCKRIGHT ] := ::BlockRight
  ::aPrgState[PRG_BLOCKBOTTOM] := ::BlockBottom
  ::aPrgState[PRG_MARK       ] := ::Mark
  ::aPrgState[PRG_READONLY   ] := ::ReadOnly
  ::aPrgState[PRG_ATEXT      ] := ::aText

  // check for comments in ::aComments !

  ::aSummary:={}
  ::aSumData:={}
  For i:=1 To ::Lines
    cline:=Upper(lTrim(::aText[i]))
    If cline = 'FUNCTION ' .OR. ;
      cline = 'PROCEDURE ' .OR. ;
      cline = 'STATIC PROCEDURE ' .OR. ;
      cline = 'STATIC FUNCTION ' .OR. ;
      cline = 'METHOD ' .OR. ;
      cline = 'METHOD FUNC ' .OR. ;
      cline = 'FUNC ' .OR. ;
      cline = 'PROC ' .OR. ;
      cline = 'STATIC PROC ' .OR. ;
      cline = 'STATIC FUNC ' .OR. ;
      cline = 'CLASS ' .OR. ;
      cline = 'INIT CLASS ' .OR. ;
      (cline = 'METHOD ' .AND. ' CLASS ' $ cline)

      If (n:=aScan(::aComments,{|x| x[1]< i })) > 0
        lInComment:=::aComments[n,3]
      EndIf

      aAdd(::aSummary,str(i,4,0)+': '+lTrim(::aText[i]))
      aAdd(::aSumData,{lInComment,i})

    EndIf

  Next

  ::lInSummary:=.T.
  ::aText:=::aSummary
  ::LeftCol:=1
  ::TopLine:=1
  ::Col:=1
  ::Line:=1
  ::isBlockOn:=.F.
  ::BlockTop:=0
  ::BlockLeft:=0
  ::BlockRight:=0
  ::BlockBottom:=0
  ::Mark:=0
  ::ReadOnly:=.T.
  ::FirstInvalid:=::TopLine

  ::configure()

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD expand

  ::aText:=::aSummary

  ::LeftCol     := ::aPrgState[PRG_LEFTCOL    ]
  ::TopLine     := ::aPrgState[PRG_TOPLINE    ]
  ::Col         := ::aPrgState[PRG_COL        ]
  ::Line        := ::aPrgState[PRG_LINE       ]
  ::isBlockOn   := ::aPrgState[PRG_ISBLOCK    ]
  ::BlockTop    := ::aPrgState[PRG_BLOCKTOP   ]
  ::BlockLeft   := ::aPrgState[PRG_BLOCKLEFT  ]
  ::BlockRight  := ::aPrgState[PRG_BLOCKRIGHT ]
  ::BlockBottom := ::aPrgState[PRG_BLOCKBOTTOM]
  ::Mark        := ::aPrgState[PRG_MARK       ]
  ::ReadOnly    := ::aPrgState[PRG_READONLY   ]
  ::aText       := ::aPrgState[PRG_ATEXT      ]

  ::lInSummary:=.F.
  ::ReadOnly:=.F.
  ::FirstInvalid:=::TopLine
  ::configure()

  Return(NIL)

*-----------------------------------------------------------------------------*
METHOD EditorProc( nMsg, nwParam, nlParam)

  Local hDC, cPaint, aTM
  Local  i, x, y,arect

  Do Case
  Case nMsg==WM_GETDLGCODE
    Return(DLGC_HASSETSEL+DLGC_WANTALLKEYS+DLGC_WANTARROWS+;
           DLGC_WANTCHARS+DLGC_WANTMESSAGE+DLGC_WANTTAB)

  Case nMsg==WM_VSCROLL
    If ::hWnd # GetFocus()
      SetFocus(::hWnd)
    EndIf
    ::OnVScroll(nwParam,nlParam)

  Case nMsg==WM_HSCROLL
    If ::hWnd # GetFocus()
      SetFocus(::hWnd)
    EndIf
    ::OnHScroll(nwParam,nlParam)

   Case nMsg==WM_SIZE //.or. nMsg==WM_MOVE
    ::Configure(LoWord(nlParam),HiWord(nlParam))

  Case nMsg==WM_SETFOCUS
    ::OnCaps()     // let's check if caps were toggled somewhere else
    ::SetCaret()
    Return(0)

  Case nMsg==WM_KILLFOCUS
    HideCaret(::hWnd)
    DestroyCaret()
    ::CtrlPressed:=.F.     // important
    ::ShiftPressed:=.F.    // this too.

  Case nMsg == WM_KEYDOWN
    //   view 'down',nwparam,nlparam
    ::Onkey(nwParam)
    Return 0

  Case nMsg == WM_KEYUP
    //view 'up',nwparam,nlparam
    ::OnKeyUp(nwParam)

  Case nMsg == WM_CHAR
    //view 'char',nwparam,nlparam
    For i := 1 To LoWord( nlParam) // That is Repeat Count
      ::Onchar(nwParam)
    Next
    Return 0

  Case nMsg == WM_ERASEBKGND
    Return(1)

  Case  nMsg == WM_LBUTTONDOWN
    If ::hWnd # GetFocus()
      SetFocus(::hWnd)
    EndIf
    ::OnMouseDown(LoWord(nlParam),HiWord(nlParam),nwParam)

  Case nMsg == WM_LBUTTONUP
    ::OnMouseUp(LoWord(nlParam),HiWord(nlParam),nwParam)

  Case nMsg == WM_MOUSEMOVE
    ::OnMouseMove(LoWord(nlParam),HiWord(nlParam),nwParam)

  Case nMsg == WM_LBUTTONDBLCLK
    ::GetWord()

  Case nMsg == WM_PAINT
    hDC := BeginPaint(::hWnd, @cPaint)
    ::FirstInvalid:=::TopLine
    If ::Configured
      ::paint(hDC)
      //::refreshall(hDC)
    EndIf
         * ValidateRect(::hWnd)
    EndPaint(::hWnd, cPaint)
    Return 0

  Case nMsg==WM_SYSCOMMAND
    If nwParam==SC_CLOSE
      DestroyWindow(::hWnd)
    EndIf

  Case nMsg == WM_DESTROY
    ::kill()

  EndCase

  Return CallWindowProc(::nProc, ::hWnd, nMsg, nwParam, nlParam)

*-----------------------------------------------------------------------------*
METHOD kill()

  Local hDC

  HideCaret(::hWnd)
  DestroyCaret()

  If !Empty(::hMemDC)
    SelectObject(::hMemDC,::hOldBmp)
    SelectObject(::hMemDC,::hOldFont)
    DeleteObject(::hMemBmp)
    If ::hFont # NIL
      DeleteObject(::hFont)
    EndIf
    DeleteDC(::hMemDC)
  EndIf

  Return(self)

*-----------------------------------------------------------------------------*


/*
*-----------------------------------------------------------------------------*
Function newedit(hWnd)

  Local o
  Local cfile

if (cfile:=GetOpenFileName(hWnd,'*.prg','Open source file',{},OFN_FILEMUSTEXIST,'c:\desktop','*.prg' ) )==NIL
  return(NIL)
endif

//hstatwin:=createstatuswindow(WS_CHILD+WS_VISIBLE,'Hi',hwnd)
//SendMessage(hstatwin,SB_SETPARTS,4,A2Bin({75,540,-1},'int,int,int'))

o:=oedit():new(hWnd)
o:read(cfile)
o:linechangeblock:={|x,y| Sendmessage(hStatWin,SB_SETTEXT,1,'Line '+alltrim(str(x))+' of '+alltrim(str(y)))}
o:colchangeblock:={|x| Sendmessage(hStatWin,SB_SETTEXT,2,'Col '+alltrim(str(x)))}
o:configure()

  //refreshall()
  InvalidateRect(hWnd,,.F.)

  Return(NIL)

*/




// re-written in C as ccolorcode()

Function colorcode(c,lInComment,hDC,LeftCol,cxChar,cyChar,row,MaxCol,aColors)

  Local  assembled:={}
  Static cGood := "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890."
  Local  lasttype:=0
  Local  nlen:=0
  Local  ch
  Local  i,n
  Local cTemp:=''
  Local clT:=lTrim(c)
  Local nWritten:=0
  Local strlen:=0
  Local added:=.F.
  Local cLeadSpace:=''

  // calling C function
  colorcode(c,@lInComment,hDC,LeftCol,cxChar,cyChar,row,MaxCol,aColors)
  Return(NIL)

  If (clT = '*') .AND. !(clT == '*/')        //  NOTE: "*" overrides "*/"
    aAdd(assembled,{c,2})  // remark
    nlen++
    added:=.T.
    c:=''

  ElseIf lInComment
    If (i:=at('*/',c)) > 0
      lInComment:=.F.
      aAdd(assembled,{Left(c,i+1),2})  // remark
      nlen++
      added:=.T.
      lasttype:=2
      c:=SubStr(c,i+2)

    Else
      aAdd(assembled,{c,2})  // remark
      nlen++
      added:=.T.
      c:=''
    EndIf
  EndIf

  Do While !(c=='')

    Do Case
    Case c = '//'
      aAdd(assembled,{cLeadSpace+c,2})  // remark   NOTE: "//" overrides /* and */
      cLeadSpace:=''
      nlen++
      added:=.T.
      Exit

    Case c = '/*'
      If (i:=at('*/',c)) > 0
        lInComment:=.F.
        aAdd(assembled,{cLeadSpace+Left(c,i+1),2})  // remark
        cLeadSpace:=''
        nlen++
        added:=.T.
        lasttype:=2
        c:=SubStr(c,i+2)
      Else
        lInComment:=.T.
        aAdd(assembled,{cLeadSpace+c,2})  // remark
        cLeadSpace:=''
        nlen++
        added:=.T.
        Exit
      EndIf

    Case c = '"' .OR. c="'"     // string constant
      ch:=Left(c,1)
      lasttype:=3
      If (i:=atn(ch,c,2)) > 0
        aAdd(assembled,{cLeadSpace+Left(c,i),3})
        cLeadSpace:=''
        nlen++
        added:=.T.
        c:=SubStr(c,i+1)
      Else
        aAdd(assembled,{cLeadSpace+c,3})
        cLeadSpace:=''
        nlen++
        added:=.T.
        Exit
      EndIf

    Case Left(c,1) $ cGood
      n:=Len(c)
      i:=1
      Do While i<=n .AND. (SubStr(c,i,1) $ cGood)
        i++
      EndDo
      cTemp:=Left(c,i-1)
      If aScan(aReserved,{|x| x==Upper(cTemp)})>0 // reserved
        If lasttype==5
          assembled[nlen,1]+=cTemp
        Else
          aAdd(assembled,{cLeadSpace+cTemp,5})
          cLeadSpace:=''
          nlen++
          added:=.T.
          lasttype:=5
        EndIf
      Else
        If lasttype==1              // standard
          assembled[nlen,1]+=cTemp
        Else
          aAdd(assembled,{cLeadSpace+cTemp,1})
          cLeadSpace:=''
          nlen++
          added:=.T.
          lasttype:=1
        EndIf
      EndIf
      c:=SubStr(c,i)

    Otherwise
      If lasttype==1              // standard
        assembled[nlen,1]+=Left(c,1)
      Else
        aAdd(assembled,{cLeadSpace+Left(c,1),1})
        cLeadSpace:=''
        nlen++
        added:=.T.
        lasttype:=1
      EndIf
      c:=SubStr(c,2)

    EndCase

    Do While c=' '
      If nlen > 0
        assembled[nlen,1]+=' ' // trailing space
      Else
        cLeadSpace+=' '  // leading white space
      EndIf
      c:=SubStr(c,2)
    EndDo

    If added .AND. nlen > 1
      strlen:=Len(assembled[nlen-1,1])
      If LeftCol < nWritten+strlen+1
        SetTextColor(hDC,aColors[assembled[nlen-1,2]])
        TextOut(hDC,(nWritten-LeftCol+1)*cxChar,(row-1)*cyChar,;
                assembled[nlen-1,1])
      EndIf
      nWritten+=strlen
      added:=.F.
    EndIf

  EndDo

  //if added
  If nlen>0
    strlen:=Len(assembled[nlen,1])
    If LeftCol < nWritten+strlen+1
      SetTextColor(hDC,aColors[assembled[nlen,2]])
      TextOut(hDC,(nWritten-LeftCol+1)*cxChar,(row-1)*cyChar,;
              PadR(assembled[nlen,1],MaxCol-(nWritten-LeftCol)))
    EndIf
  EndIf
  //endif

  Return(NIL) //assembled)


Function readafile(cFileName,aText,nTabSpace) // atext must be by reference

  Local h
  Local cBuff:=Space(32768)
  Local n
  Local nRetVal:=0
  Local LastPartial:=.F.

  Local CRLFpos:=0
  Local nElems:=0
  Local nStart:=1

  Local CRLFlen:=2
  Local cReminder:=''
  Local nlines:=0

  // reset atext buffer
  aText:={''}
  nTabSpace:=If(ValType(nTabSpace)=='N',nTabSpace,4)
  h:=fOpen(cFileName)
  // verify fopen error
  If h == -1
    nRetVal:=-1
  Else

    // read file in chunks till all done

    Do While (n:=fRead(h,@cBuff,32768)) > 0

      nRetVal+=n
      cBuff:=cReminder+Left(cBuff,n)
      cReminder:=''

      // count lines in buffer

      nElems:=nlines+(Len(cBuff)-Len(StrTran(cBuff,CRLF,'')))/CRLFlen +1

      // check for array overflow limit

//      If nElems > 4096
//        nElems:=4096
//        nRetVal:=0
//      EndIf

      // adjust array size

      asize(aText,nElems)
      nStart:=1
        
      // put current buffer into array
      Do While .T.

        If (CRLFpos:=atn(CRLF,cBuff,nStart)) > 0
          //    view CRLFpos
          nlines++
          aText[nlines]:=StrTran(SubStr(cBuff,nStart,CRLFpos-nStart),chr(9),nTabSpace)
          nStart:=CRLFpos+CRLFlen
          LastPartial:=.F.

        Else   // elements first and size

          cReminder:=SubStr(cBuff,nStart)
          LastPartial:=.T.
          Exit

        EndIf

        If nlines==nElems-If(LastPartial,1,0)
          Exit
        EndIf
      EndDo
      cBuff:=Space(32768)
    EndDo

    // check and add buffer reminder

    If LastPartial
      nlines++
      aText[nlines]:=cReminder
    EndIf

    fClose(h)
  EndIf

  Return(nRetVal)

Function saveafile(cFileName,aText)

  Local nRetval:=0
  Local h:=fCreate(cFileName)
  Local i

  If h == -1
    nRetval:=-1
  Else
    aeval(aText,{|x| nRetval+=fWrite(h,Trim(x)+CRLF)})
    fClose(h)
  EndIf

  Return(nRetval)


*-----------------------------------------------------------------------------*
* splits Clipper source code line into array of words
*-----------------------------------------------------------------------------*
Function parsexpr(c,lHonorSpacing,lInRemark,lUpperKeyWord)

  Local aWords:={}
  Local i
  Local NextWord
  Local temptxt:=AllTrim(c)
  Local npos:=0
  Local good:='_ABCDEFGHIJKLMNOPQRSTUVWXYZ.'

  lHonorSpacing:=If(ValType(lHonorSpacing)=='L',lHonorSpacing,.F.)
  lInRemark:=If(ValType(lInRemark)=='L',lInRemark,.F.)
  lUpperKeyWord:=if(valtype(lUpperKeyWord)=='L',lUpperKeyWord,.f.)


  NextWord:=GetWord(@c,lHonorSpacing)

  //view c,GetWord(@c,lHonorSpacing),c


  Do While Len(NextWord) > 0
    Do Case

    Case NextWord=='/*' // remark start
      lInRemark:=(npos:=at('*/',c))==0
      If lInRemark
        aAdd(aWords,NextWord+c)
        Exit
      Else
        aAdd(aWords,NextWord+Left(c,npos+1))
        c:=SubStr(c,npos+2)
      EndIf


    Case NextWord=='*/' // remark end
      aAdd(aWords,NextWord)
      lInRemark:=.F.

    Case NextWord=='//' .OR. NextWord=='&&' // inline remark
      aAdd(aWords,NextWord+c)
      Exit

    Otherwise // normal word - asert the case

      If !lInRemark
        If Upper(Left(NextWord,1)) $ good
          If (npos:=aScan(aReserved,{|x| Upper(NextWord)==Upper(x) })) > 0
            if lUpperKeyWord
              NextWord:=Upper(aReserved[npos])
            else
              NextWord:=aReserved[npos]
            endif

          ElseIf (npos:=aScan(aStandard,{|x| Upper(NextWord)==Upper(x) })) > 0
            NextWord:=aStandard[npos]

          ElseIf (npos:=aScan(adictionary,{|x| Upper(NextWord)==Upper(x) })) > 0
            NextWord:=adictionary[npos]

          ElseIf (npos:=aScan(aLocals,{|x| Upper(NextWord)==Upper(x) })) > 0
            NextWord:=aLocals[npos]

          ElseIf (npos:=aScan(aGlobals,{|x| Upper(NextWord)==Upper(x) })) > 0
            NextWord:=aGlobals[npos]

          EndIf
        EndIf
      EndIf

      aAdd(aWords,NextWord)

    EndCase

    If aWords[1]=='*' // remark line
      aWords[1]+=c
      Exit
    EndIf

    NextWord:=GetWord(@c,lHonorSpacing)
  EndDo

  Return(aWords)

*-----------------------------------------------------------------------------*
* returns next word from the Clipper source line
*-----------------------------------------------------------------------------*
Static Function getword(cText,lHonorSpacing)

  Local ch:=Left(cText,1)
  Local nPos:=1
  Local cWord:=""
  Local temp
  Local maxlen:=Len(cText)
  Local isgood:=.T.
  Local odbctest
  Local good:='"_ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890.'+"'"
  Local adouble:={'->','::','||','++','--','**',':=','<=','>=',;
                  '<>','!=','==','+=','-=','*=','/=','%=','^=',;
                  '//','/*','*/','&&'}
                  // space added for SQL field names containing spaces

                  If ch==',' // lists
                  cWord:=ch
                  nPos++
                Else // literals
                  If ch =='"' .OR. ch=="'"

                  temp:=ch
                  cWord:=ch
                  ch:=' '
                  Do While (nPos<=maxlen) .AND. (ch # temp)
                  nPos++
                  ch:=SubStr(cText,nPos,1)
                  cWord+=ch
                EndDo
                  nPos++

                Else

                  If Upper(ch) $ good // variables, commands, function names
                  Do While (nPos<=maxlen) .AND. (Upper(ch) $ good)
                  cWord+=ch
                  nPos++
                  ch:=SubStr(cText,nPos,1)
                EndDo
                ElseIf ch==' '
                  Do While (nPos<=maxlen) .AND. ch==' '
                  cWord+=ch
                  nPos++
                  ch:=SubStr(cText,nPos,1)
                EndDo
                  If !lHonorSpacing
                  cWord:=' ' //reduce spaces to 1
                EndIf

                Else //operators, punctuation
                  cWord+=ch
                  nPos++
                  ch:=SubStr(cText,nPos,1)
                  If Len(cText) >= nPos
                  If aScan(adouble,cWord+ch) > 0
                  cWord+=ch
                  nPos++
                  ch:=SubStr(cText,nPos,1)
                EndIf
                EndIf
                EndIf

                EndIf
                EndIf
                  cText:=SubStr(cText,nPos)
                  Return(cWord)

function atn( cStr1, cStr2, nVal ) 
return(at(cStr1,cStr2,nVal))

