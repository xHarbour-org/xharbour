/*
 * $Id: TCButton.prg,v 1.17 2002/10/17 17:16:20 what32 Exp $
 */
/*
 * xHarbour Project source code:
 *
 * Whoo.lib CLASS TWBrowse
 *
 * Copyright 2002 Andrew J. Wos [andrwos@aust1.net]
 *
 * Whoo Version Augusto Infante [augusto@2vias.com.ar]
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

#INCLUDE 'windows.ch'
#INCLUDE 'commdlg.ch'
#INCLUDE 'header.ch'
#INCLUDE "inkey.ch"
#INCLUDE "accel.ch"
#INCLUDE "cstruct.ch"
#INCLUDE "wintypes.ch"
#INCLUDE "hbclass.ch"
#INCLUDE 'What32.ch'
#INCLUDE "imglist.ch"
#Include "wingdi.ch"
#include "debug.ch"


pragma pack(4)

IMPORT C STRUCTURE TEXTMETRIC
IMPORT C STRUCTURE NMHDR

typedef struct tagSCROLLINFO {;  // si
    UINT cbSize; 
    UINT fMask; 
    int  nMin; 
    int  nMax; 
    UINT nPage; 
    int  nPos; 
    int  nTrackPos; 
} SCROLLINFO

typedef struct _HDITEM {;
    UINT    mask; 
    int     cxy; 
    LPTSTR  pszText; 
    HBITMAP hbm; 
    int     cchTextMax; 
    int     fmt; 
    LPARAM  lParam; 
    int     iImage;
    int     iOrder;
} HDITEM, FAR * LPHDITEM

typedef struct tagNMHEADER{;
    NMHDR    hdr;
    int      iItem;
    int      iButton;
    HDITEM   *pitem;
} NMHEADER

*-----------------------------------------------------------------------------*

CLASS TWBrowse FROM TWinControl

   DATA Style     INIT WS_CHILD+WS_BORDER+WS_VISIBLE+WS_VSCROLL+WS_HSCROLL
   DATA ExStyle   INIT WS_EX_CLIENTEDGE
   DATA lRegister PROTECTED INIT .T.
   DATA FrameWnd  PROTECTED INIT .F.

   DATA nDataTop           PROTECTED // data display area top coordinate (usually == headHeight)
   DATA nDataLeft          PROTECTED // data display area left coordinate ( usually 0 )
   DATA nDataWidth         PROTECTED // data display area width
   DATA nDataHeight        PROTECTED // data display area height
   DATA hWndHeader         PROTECTED // handle to the header window
   DATA aRect              PROTECTED // browse window client rectangle
   DATA Configured         PROTECTED // internal subclassing flag
   DATA ImageList          PROTECTED 

   // tbd

   DATA hWndNub            PROTECTED // numbs window handle ( future expansion )
   DATA NubWidth           PROTECTED // width of nums window

   // user options (assignable)

   DATA HeadHeight         PROTECTED // header height
   DATA ItemHeight         PROTECTED // height of a single data item
   DATA wantHeading        PROTECTED //
   DATA wantResize         PROTECTED 
   DATA wantHScroll        PROTECTED 
   DATA wantVScroll        PROTECTED 
   DATA wantHiliteAll      PROTECTED 
   DATA wantRowSep         PROTECTED 
   DATA wantColSep         PROTECTED 
   DATA wantNotify         PROTECTED 
   DATA wantNubs           PROTECTED 
   DATA wantDrawFocus      PROTECTED 
   DATA wantUseSysColors   PROTECTED 
   DATA wantEnterKey       PROTECTED 
   DATA wantAutoHilite     PROTECTED 
   //

   DATA si                 PROTECTED // scrollinfo

   DATA ColWidths          PROTECTED // array of cumulative column widths (starting from 0)
   DATA Columns            PROTECTED // array of column objects
   DATA aData              PROTECTED // data buffer - containg all columns text data for all displayed rows
   DATA aFgColors          PROTECTED 
   DATA aBgColors          PROTECTED 

   DATA RowPos             PROTECTED // row number in the data display window
   DATA ColPos             PROTECTED // current column position
   DATA Hilited            PROTECTED // hilite flag
   DATA Frozen             PROTECTED 

   DATA RowCountVisible    PROTECTED // all rows incl. the partially visible row
   DATA RowCountUsable     PROTECTED // fully visible rows only
   DATA OldRecNo           PROTECTED // previous record
   DATA OldColPos          PROTECTED // provious column position (temp)
   DATA ColCount           PROTECTED // number of all comumns in the browse
   DATA LeftVisible        PROTECTED // leftmost visible column
   DATA RightVisible       PROTECTED // rightmost visible column
   DATA lResizing          PROTECTED // busy resizing a column logical flag
   DATA lMoving            PROTECTED // busy moving a column logical flag

   DATA xvScroll           PROTECTED 
   DATA xhScroll           PROTECTED 
   DATA SepColor           PROTECTED 
   DATA HeadFgColor        PROTECTED 
   DATA HeadBgColor        PROTECTED 
   DATA HeadFont           PROTECTED 
   DATA FgColor            PROTECTED 
   DATA BgColor            PROTECTED 
   DATA Font               PROTECTED 
   DATA HiliteColor        PROTECTED 
   DATA HiliteBgColor      PROTECTED 
   DATA HiliteNoFocus      PROTECTED 
   DATA LinePen            PROTECTED 
   DATA HeadText           PROTECTED 
   DATA HeadAlign          PROTECTED 
   DATA HeadBmps           PROTECTED 
   DATA HeadBmpAlign       PROTECTED 

   DATA ColBgColors        PROTECTED 
   DATA ColFgColors        PROTECTED 
   DATA ColAlign           PROTECTED 
   DATA ColVAlign          PROTECTED 
   DATA ColFonts           PROTECTED 
   DATA ColBmps            PROTECTED 
   DATA ColBmpAlign        PROTECTED 
   DATA ColOffset          PROTECTED 
   DATA ColStyle           PROTECTED 

   DATA objects            PROTECTED 

   DATA vScrollEx          PROTECTED 

   DATA Source             PROTECTED 
   DATA ArrayMode          PROTECTED 
   DATA Element            PROTECTED 
   DATA RecPos             PROTECTED 
   DATA RecCount           PROTECTED 
   DATA HitTop             PROTECTED 
   DATA HitBottom          PROTECTED 

   DATA bSeekChar          PROTECTED 
   DATA bGoToPos           PROTECTED 
   DATA bGoTop             PROTECTED 
   DATA bGoBottom          PROTECTED 
   DATA bSkip              PROTECTED 
   DATA bRecNo             PROTECTED 
   DATA bSetLogPos         PROTECTED 
   DATA bOnChangeBlock     PROTECTED 
   DATA bOnClick           PROTECTED 
   DATA bOnDblClick        PROTECTED 

   DATA bOnKey             PROTECTED 
   DATA bOnChar            PROTECTED 
   DATA bKillBlock         PROTECTED 

   DATA xTrack             PROTECTED 
   DATA xTrackOffset       PROTECTED 
   DATA xTrackColumn       PROTECTED 
   DATA xDragColumn        PROTECTED 

   METHOD New() CONSTRUCTOR
   METHOD AddColumn()
   METHOD InsColumn()
   METHOD DelColumn()
   METHOD SetColumn()
   METHOD GetColumn()
   METHOD Configure()
   METHOD SetColPos()

   METHOD SetColWidth()
   METHOD GetColPos()
   METHOD GetColIndex()
   METHOD GetColBlock()
   METHOD GetColFont()
   METHOD RestoreColumnWidth()
   METHOD RestoreColumnOrder()
   METHOD Freeze()

   METHOD Create()      INLINE super:Create(),::Configure()
   METHOD RefreshAll()
   METHOD RefreshCurrent()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD drawdata()
   METHOD GoToCol()
   METHOD drawheader()
   METHOD OnChange()
   METHOD UpdateVScrollBar()
   METHOD UpdateHScrollBar()
   METHOD GoToPos()
   METHOD Kill()

   METHOD OnPaint(hDC) INLINE ::drawheader(),;
                              ::drawdata(hDC),;
                              self

   METHOD OnGetDlgCode() INLINE DLGC_WANTALLKEYS+DLGC_WANTARROWS+DLGC_WANTCHARS
   METHOD OnDestroy()    INLINE ::Kill(),NIL
   METHOD OnSetFocus()   INLINE ::drawdata(,::RowPos,IF(::wantHiliteAll,NIL,::ColPos)),NIL
   METHOD OnKillFocus()  INLINE ::drawdata(,::RowPos,IF(::wantHiliteAll,NIL,::ColPos)),NIL
   METHOD OnEraseBkGnd() INLINE 0
   METHOD OnSize()       INLINE ::RefreshAll(),0
   METHOD OnNotify()
   METHOD OnSysColorChange()

   METHOD OnBeginTrack()
   METHOD OnEndTrack()
   METHOD OnBeginDrag()
   METHOD OnEndDrag()
   METHOD OnTracking()
   METHOD OnItemChanged()
   METHOD OnChange()
   METHOD OnHeadClick()
   METHOD OnHeadDblClick()
   METHOD OnKeyDown()
   METHOD OnLButtonDown()
   METHOD OnLButtonDblClk()
   METHOD OnChar()
   METHOD OnVScroll()
   METHOD OnHScroll()

   METHOD Up()
   METHOD Down()
   METHOD Pgup()
   METHOD Pgdown()
   METHOD Home()
   METHOD EndKey()
   METHOD PanLeft()
   METHOD PanRight()
   METHOD PanHome()
   METHOD PanEnd()
   METHOD KeyLeft()
   METHOD KeyRight()
   METHOD GotoLine()

   METHOD Display()
   METHOD ScrollUp()
   METHOD ScrollDown()

   METHOD EditCell()
   METHOD EditGetCell()
   METHOD EditCellProc()

   METHOD GetItemRect()
   METHOD GetItemText()

ENDCLASS

*-----------------------------------------------------------------------------*

METHOD New( oParent, nId, nLeft, nTop, nWidth, nHeight, Source, aHeader, bNotify ) CLASS TWBrowse
   local i, oCol
   ::id        := nId
   ::Left      := IFNIL( nLeft,    ::Left,    nLeft    )
   ::Top       := IFNIL( nTop,     ::Top,     nTop     )
   ::Width     := IFNIL( nWidth ,  ::Width,   nWidth   )
   ::Height    := IFNIL( nHeight,  ::height,  nHeight  )
   ::Msgs      := -1
   ::WndProc   :="FormProc"

   ::wantHiliteAll   := .F.
   ::wantRowSep      := .T.
   ::wantColSep      := .T.
   ::wantResize      := .T.
   ::wantNubs        := .F.
   ::wantHScroll     := .T.
   ::wantVScroll     := .T.
   ::wantHeading     := .T.
   ::wantDrawFocus   := .T.
   ::wantUseSysColors:= .T.
   ::wantEnterKey    := .F.
   ::wantAutoHilite  := .T.
   ::bOnChangeBlock  := bNotify
   ::wantNotify      := (ValType(bNotify)=="B") // tbd

   // initialize

   ::ColWidths       := {0}
   ::Columns         := {}
   ::aData           := {}
   ::aFgColors       := {}
   ::aBgColors       := {}

   ::RowPos          := 1
   ::OldRecNo        := 0
   ::ColPos          := 1
   ::OldColPos       := 0
   ::ColCount        := 0
   ::LeftVisible     := 1
   ::RightVisible    := 0
   ::HeadHeight      := 0
   ::NubWidth        := 0
   ::HeadHeight      := 0
   ::lResizing       := .F.
   ::lMoving         := .F.
   ::nDataWidth      := 0
   ::nDataHeight     := 0
   ::RowCountVisible := 0
   ::RowCountUsable  := 0
   ::xTrack          := 0
   ::xTrackOffset    := 0
   ::xTrackColumn    := 0
   ::Frozen          := 0

   ::Configured      :=.F.
   ::Font            := GetMessageFont()
   ::HeadFont        := GetMessageFont()

   // colours

   ::xvScroll        := GetSystemMetrics( SM_CXVSCROLL )+1
   ::xhScroll        := GetSystemMetrics( SM_CYHSCROLL )+1

   ::SepColor        := GetSysColor( COLOR_BTNSHADOW )
   ::HeadFgColor     := GetSysColor( COLOR_WINDOWTEXT )
   ::HeadBgColor     := GetSysColor( COLOR_BTNFACE )

   ::FgColor         := GetSysColor( COLOR_WINDOWTEXT )
   ::BgColor         := GetSysColor( COLOR_WINDOW )

   ::HiliteColor     := GetSysColor( COLOR_HIGHLIGHTTEXT )
   ::HiliteBgColor   := GetSysColor( COLOR_HIGHLIGHT )
   ::HiliteNoFocus   := GetSysColor( COLOR_INACTIVECAPTION )
   ::Hilited         := .F.

   ::ItemHeight      := 18
   ::HeadHeight      := 18

   ::ArrayMode       :=.F.
   ::Source          :=Source
   DO CASE
   CASE ValType(Source)=="A"
      ::ArrayMode:=.T.
   CASE ValType(Source)=="C"
      ::ArrayMode:=.F.
   CASE Source==NIL .AND. ! Empty(ALIAS())
      ::Source:=ALIAS()
   OTHERWISE
      // error will come on configure or refresh !
   ENDCASE
   IF ::ArrayMode
      ::Element   :=1
      ::RecPos    :=::Element
      ::RecCount  :=Len(::Source)
      ::bSeekChar :={|ob,chr| .F.}

      ::bGoToPos  :={|ob,nPos| ob:Element:=nPos,;
                               ob:HitTop:=(ob:Element < 1),;
                               ob:HitBottom:=(ob:Element > Len(ob:Source)),;
                               ob:Element:=Min(ob:Element,Len(ob:Source)+1),;
                               ob:Element:=Max(ob:Element,1),;
                               ob:RecCount:=LEN(ob:Source),;
                               ob:RecPos:=ob:Element}

      ::bGoTop    := {||::Element:=1,;
                        ::HitTop:=(Len(::Source)==0),;
                        ::HitBottom:=(::Element > Len(::Source)),;
                        ::RecCount := LEN(::Source),;
                        ::RecPos:=::Element}

      ::bGoBottom := {||::Element:=Len(::Source),;
                        ::HitTop:=(Len(::Source)==0),;
                        ::HitBottom:=(Len(::Source)==0),;
                        ::RecCount := LEN(::Source),;
                        ::RecPos:=::Element}

      ::bSkip     :={|n|::Element+=n,;
                        ::HitTop:=(::Element < 1),;
                        ::HitBottom:=(::Element > Len(::Source)),;
                        ::RecCount:=Len(::Source),;
                        ::Element:=Min(::Element,Len(::Source)+1),;
                        ::Element:=Max(::Element,1),;
                        ::RecPos:=::Element}

      ::bRecNo    :={||::Element}

   ELSE

      ::Element:=1
      ::RecPos:=(::Source)->(RECNO())
      ::RecCount:=(::Source)->(RecCount())
      ::bSeekChar :=NIL
      ::bGoToPos :={|ob,nPos|  gotorec(ob,nPos),;
                               ::RecPos:=nPos,;
                               ::HitTop:=(::Source)->(bof()),;
                               ::HitBottom:=(::Source)->(eof()),;
                               ::RecCount:=(::Source)->(RecCount()),;
                               IF(::HitTop,::RecPos:=1,),;
                               IF(::HitBottom,::RecPos:=::RecCount,)}

      ::bGoTop :={| | (::Source)->(dbgotop()),;
                  ::HitTop:=(::source)->(BOF()),;
                  ::HitBottom:=(::Source)->(eof()),;
                  ::RecCount:=(::Source)->(RecCount()),;
                  ::RecPos:=1,;
                  IF(::HitTop,::RecPos:=1,),;
                  IF(::HitBottom,::RecPos:=::RecCount,)}

      ::bGoBottom :={|n| (::Source)->(dbgobottom()),;
                     ::HitTop:=(::Source)->(bof()),;
                     ::HitBottom:=(::source)->(EOF()),;
                     ::RecCount:=(::Source)->(RecCount()),;
                     ::RecPos:=::RecCount,;
                     IF(::HitTop,::RecPos:=1,),;
                     IF(::HitBottom,::RecPos:=::RecCount,)}

      ::bSkip :={|n| (::Source)->(dbSkip(n)),;
                 ::RecPos+=n,;
                 ::HitTop:=(::Source)->(bof()),;
                 ::HitBottom:=(::Source)->(eof()),;
                 ::RecCount:=(::Source)->(RecCount()),;
                 IF(::HitTop,::RecPos:=1,),;
                 IF(::HitBottom,::RecPos:=::RecCount,)}
      ::bRecNo :={| | (::Source)->(RecNo())}
   ENDIF
   ::HeadText     :={}
   ::HeadAlign    :={}
   ::HeadBmps     :={}
   ::HeadBmpAlign :={}
   ::ColBgColors  :={}
   ::ColFgColors  :={}
   ::ColAlign     :={}
   ::ColVAlign    :={}
   ::ColFonts     :={}
   ::ColBmps      :={}
   ::ColBmpAlign  :={}
   ::ColStyle     :={}
   ::ColOffset    :={}
   ::objects      :={}


   if aHeader!=NIL
      for i:=1 to len( aHeader )
          oCol:=GetAColumn( aHeader, i)
          oCol:VertAlign := TA_CENTER
          oCol:bSaveBlock:= aHeader[i][3]
          ::AddColumn(oCol)
      next
   END

return(super:new( oParent ))

//---------------------------------------------------------------------------------------------

METHOD AddColumn(colob,lConfigure) CLASS TWBrowse
   LOCAL nIndex:=::ColCount+1
   DO WHILE aScan(::Columns,{|o| o:OriginalIndex==nIndex})>0
      nIndex++
   ENDDO
   colob:OriginalIndex:=nIndex
   aAdd(::Columns,colob)
   ::ColCount:=Len(::Columns)
   DEFAULT lConfigure TO .F.
   IF lConfigure
      ::Configure()
   ENDIF
RETURN(nIndex)

//---------------------------------------------------------------------------------------------

METHOD InsColumn(nCol,colob,lConfigure) CLASS TWBrowse
   LOCAL nIndex:=::ColCount+1
   DEFAULT nCol TO ::ColPos
   IF nCol>0 .AND. nCol<=::ColCount
      DO WHILE aScan(::Columns,{|o| o:OriginalIndex==nIndex})>0
         nIndex++
      ENDDO
      colob:OriginalIndex:=nIndex
      ASIZE(::Columns,Len(::Columns)+1)
      aIns(::Columns,nCol,)
      ::Columns[nCol]:=colob
      ::ColCount:=Len(::Columns)
      DEFAULT lConfigure TO .T.
      IF lConfigure
         ::Configure()
      ENDIF
      RETURN(nIndex)
     ELSEIF nCol > ::ColCount
      RETURN(::AddColumn(colob,lConfigure))
   ENDIF
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD Configure(hNewImageList) CLASS TWBrowse

   LOCAL i
   LOCAL wantHeading:=.F.
   LOCAL pos
   LOCAL nstyle
   LOCAL nProc
   IF ::Source==NIL
      MessageBox(,'No data source.','Error',MB_OK)
   ENDIF
   IF hNewImageList<>NIL
      ::ImageList:=hNewImageList
   ENDIF
   aEval(::objects,{|x| IF(!Empty(x[3]),DeleteObject(x[3]),NIL)})
   ::objects :={}
   ::LinePen:=CreatePen(PS_SOLID,0,::SepColor)
   ::ColWidths:={0}

   ::HeadText :={}
   ::HeadAlign :={}
   ::HeadBmps :={}
   ::HeadBmpAlign :={}

   ::ColBgColors :={}
   ::ColFgColors :={}
   ::ColAlign :={}
   ::ColVAlign:={}
   ::ColFonts :={}
   ::ColBmps  :={}
   ::ColBmpAlign:={}
   ::ColStyle:={}
   ::ColOffset:={}
   ::aFgColors:={}
   ::aBgColors:={}
   ::objects :={}

   ::ColCount:=Len(::Columns)
   ::ColPos:=Max(1,MIN(::ColCount,::ColPos))
   ::LeftVisible:=Max(1,MIN(::ColCount,::LeftVisible))
   ::RightVisible:=Max(::LeftVisible,MIN(::RightVisible,::ColCount))

   FOR i:=1 TO ::ColCount
      aAdd(::ColWidths ,::ColWidths[i]+::Columns[i]:width)
      aAdd(::ColBgColors ,::Columns[i]:BgColor)
      aAdd(::ColFgColors ,::Columns[i]:FgColor)
      aAdd(::ColAlign ,::Columns[i]:align)
      aAdd(::ColVAlign ,::Columns[i]:vertalign)
      aAdd(::ColFonts ,::Columns[i]:FONT)
      aAdd(::ColBmpAlign ,::Columns[i]:bmpAlign)
      aAdd(::ColOffset,::Columns[i]:Offset)
      aAdd(::ColStyle,::Columns[i]:style)
      aAdd(::HeadText ,::Columns[i]:heading)
      aAdd(::HeadAlign ,::Columns[i]:HeadAlign)
      aAdd(::HeadBmps ,::Columns[i]:headbmpindex)
      aAdd(::HeadBmpAlign,::Columns[i]:HeadBmpAlign)
   NEXT

   FOR i:=1 TO ::ColCount
      IF ::ColBgColors[i]==NIL
         ::ColBgColors[i]:=::BgColor // default background
      ENDIF
      IF ::ColFgColors[i]==NIL
         ::ColFgColors[i]:=::FgColor // default foreground colors
      ENDIF
      IF ::ColAlign[i]==NIL
         ::ColAlign[i]:=TA_LEFT // default alignment
      ENDIF
      IF ::ColVAlign[i]==NIL
         ::ColVAlign[i]:=TA_TOP // default alignment
      ENDIF
      IF ::ColFonts[i]==NIL
         ::ColFonts[i]:=::Font // default font
      ENDIF
      IF ::HeadAlign[i]==NIL
         ::HeadAlign[i]:=::ColAlign[i]
      ENDIF
      IF ::HeadBmpAlign[i] == NIL
         ::HeadBmpAlign[i]:=::ColAlign[i]
      ENDIF
   NEXT
   ::Hilited:=(!::Configured .AND. ::wantAutoHilite)
   ::RefreshAll()
   ::Configured:=.T.
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD RefreshAll(lNoDraw) CLASS TWBrowse

   LOCAL i
   LOCAL j
   LOCAL temprec
   LOCAL skipcount:=0
   LOCAL nrec:=0
   LOCAL hi
   LOCAL nNewRow
   LOCAL nOldRow:=::RowPos
   LOCAL nOldRec
   LOCAL mask
   LOCAL fmt
   LOCAL wasHilite:=::Hilited

   IF Empty(::handle) .OR. !IsWindow(::handle)
      RETURN(self)
   ENDIF
   hi IS HDITEM
   ZeroInit(hi)

   BEGIN SEQUENCE
      ShowScrollBar(::handle,SB_HORZ, ::ColCount > 1 .AND. ::wantHScroll )
      ShowScrollBar(::handle,SB_VERT,::wantVScroll)
      UpdateWindow(::handle)
      ::aRect           := GetClientRect(::handle)
      ::nDataLeft       := ::aRect[1]
      ::nDataTop        := ::aRect[2]+::HeadHeight
      ::nDataHeight     := ::aRect[4]-::HeadHeight
      ::nDataWidth      := ::aRect[3]+1
      ::RowCountVisible := Ceiling(::nDataHeight/::ItemHeight)
      ::RowCountUsable  := Int(::nDataHeight/::ItemHeight)

      nrec:=Eval(::bRecNo)
      IF ::RowPos == 0
         Eval(::bSkip,1)
         Eval(::bSkip,-1)
      ENDIF

      Eval(::bSkip,-Min(::RowPos,::RowCountUsable)+1 )
      IF ::HitTop
         Eval(::bGoTop)
        ELSE
         skipcount+=(-Min(::RowPos,::RowCountUsable)+1)
      ENDIF
      ::aData     :={}
      ::ColBmps   :={}
      ::aFgColors :={}
      ::aBgColors :={}

      IF IsWindow(::hWndHeader)
         IF !::wantHeading
            DestroyWindow(::hWndHeader)
            ::HeadHeight:=0
           ELSE
            FOR i:=1 TO ::ColCount
               Header_DeleteItem(::hWndHeader,0)
            NEXT
         ENDIF
        ELSEIF ::wantHeading
         ::hWndHeader:=Header_Create(WS_CHILD+WS_VISIBLE+HDS_BUTTONS+HDS_DRAGDROP, 0, 0,  ::Width, ::HeadHeight,::handle,100)
         SendMessage(::hWndHeader,WM_SETFONT,IF(::HeadFont==NIL,::Font,::HeadFont),0)
         IF ::ImageList<>NIL
            Header_SetImageList(::hWndHeader,::ImageList)
         ENDIF
      ENDIF

      IF ::wantHeading .AND. IsWindow(::hWndHeader)
         FOR i:=1 TO ::ColCount
            mask:=HDI_WIDTH+HDI_FORMAT+HDI_ORDER
            fmt :=0
            IF !Empty(::HeadText[i])
               mask+=HDI_TEXT
               fmt+=HDF_STRING
               DO CASE
               CASE ::HeadAlign[i]==TA_LEFT
                  fmt+=HDF_LEFT
               CASE ::HeadAlign[i]==TA_RIGHT
                  fmt+=HDF_RIGHT
               OTHERWISE
                  fmt+=HDF_CENTER
               ENDCASE
               hi:pszText   := ::HeadText[i]
               hi:cchTextMax:= Len(::HeadText[i])+1
            ENDIF

            IF ::ImageList <> NIL
               IF ::HeadBmps[i]<>NIL
                  mask+=HDI_IMAGE
                  fmt +=HDF_IMAGE
                  hi:iImage:=::HeadBmps[i]
               ENDIF
            ENDIF

            hi:mask      := mask
            hi:fmt       := fmt
            hi:cxy       := ::Columns[i]:width
            hi:iorder    := i-1

            Header_InsertItem(::hWndHeader,i-1,hi:value)
         NEXT
      ENDIF

      IF !::HitBottom
         FOR i:=1 TO ::RowCountVisible
            aAdd(::aData,{Eval(::bRecNo),array(::ColCount)})
            aAdd(::ColBmps,array(::ColCount))
            aAdd(::aFgColors,ARRAY(::ColCount))
            aAdd(::aBgColors,ARRAY(::ColCount))
            FOR j:=1 TO ::ColCount
               ::aData[i,2,j]:=Eval(::Columns[j]:block,::Columns[j],self,::aData[i,1],i)
               IF ValType(::Columns[j]:bmpindex)=="B"
                  ::ColBmps[i,j]:=EVAL(::Columns[j]:bmpindex,::Columns[j],self,::aData[i,1],i)
                 ELSE
                  ::ColBmps[i,j]:=::Columns[j]:bmpindex
               ENDIF
               IF ValType(::ColFgColors[j])=="B"
                  ::aFgColors[i,j]:=EVAL(::ColFgColors[j],::Columns[j],self,::aData[i,1],i)
                 ELSE
                  ::aFgColors[i,j]:=::ColFgColors[j]
               ENDIF
               IF ValType(::ColBgColors[j])=="B"
                  ::aBgColors[i,j]:=EVAL(::ColBgColors[j],::Columns[j],self,::aData[i,1],i)
                 ELSE
                  ::aBgColors[i,j]:=::ColBgColors[j]
               ENDIF
            NEXT
            Eval(::bSkip,1)
            IF ::HitBottom
               Eval(::bGoBottom)
               EXIT
            ENDIF
            skipcount++
         NEXT
      ENDIF

      IF i <= ::RowCountUsable
         Eval(::bSkip,-i)
         skipcount-=(i-1)
         DO WHILE !::HitTop
            skipcount-=1
            IF Len(::aData)>=::RowCountUsable
               EXIT
            ENDIF
            aSize(::aData,Len(::aData)+1)
            aIns(::aData,1)
            aSize(::ColBmps,Len(::aData))
            aIns(::ColBmps,1)
            ASIZE(::aFgColors,Len(::aData))
            aIns(::aFgColors,1)
            ASIZE(::aBgColors,Len(::aData))
            aIns(::aBgColors,1)
            ::aData[1]     :={Eval(::bRecNo),array(::ColCount)}
            ::ColBmps[1]   :=array(::ColCount)
            ::aFgColors[1] :=ARRAY(::ColCount)
            ::aBgColors[1] :=ARRAY(::ColCount)
            FOR j:=1 TO ::ColCount
               ::aData[1,2,j]:=Eval(::Columns[j]:block,::Columns[j],self,::aData[1,1],1)
               IF ValType(::Columns[j]:bmpindex)=="B"
                  ::ColBmps[1,j]:=EVAL(::Columns[j]:bmpindex,::Columns[j],self,::aData[1,1],1)
               ELSE
                  ::ColBmps[1,j]:=::Columns[j]:bmpindex
               ENDIF
               IF ValType(::ColFgColors[j])=="B"
                  ::aFgColors[1,j]:=EVAL(::ColFgColors[j],::Columns[j],self,::aData[1,1],1)
               ELSE
                  ::aFgColors[1,j]:=::ColFgColors[j]
               ENDIF
               IF ValType(::ColBgColors[j])=="B"
                  ::aBgColors[1,j]:=EVAL(::ColBgColors[j],::Columns[j],self,::aData[1,1],1)
               ELSE
                  ::aBgColors[1,j]:=::ColBgColors[j]
               ENDIF
            NEXT
            Eval(::bSkip,-1)
         ENDDO
      ENDIF
      EVAL(::bSkip,-skipcount)

      i:=Len(::aData)
      DO WHILE i < ::RowCountVisible
         aAdd(::aData,{0,array(::ColCount)})
         aAdd(::ColBmps,array(::ColCount))
         aAdd(::aFgColors,ARRAY(::ColCount))
         aAdd(::aBgColors,ARRAY(::ColCount))
         i++
         FOR j:=1 TO ::ColCount
            ::aData[i,2,j]:=''
         NEXT
      ENDDO

      IF (::RowPos:=aScan(::aData,{|x| x[1]==nrec}))==0
         IF nOldRow <= ::RowCountUsable
            IF (nrec:=::aData[nOldRow,1])==0
               IF (nrec:=::aData[::RowCountUsable,1])== 0
                  ::RowPos:=1
                  nrec:=::aData[1,1]
               ELSE
                  ::RowPos:=::RowCountUsable
               ENDIF
            ELSE
               ::RowPos:=nOldRow
            ENDIF
         ELSE
            ::RowPos:=nOldRow
         ENDIF

      ENDIF

      DEFAULT lNoDraw TO .F.

      IF !lNoDraw
         ::DeHilite(.T.)
         ::GoToCol()
         IF wasHilite
            ::Hilite(.T.)
         ENDIF
         ::UpdateVScrollBar(.T.)
         ::UpdateHScrollBar(.T.)
      ENDIF
      ::OnChange()

   END
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD GoToCol(newcol) CLASS TWBrowse
   LOCAL i
   LOCAL temppos:=::ColPos

   IF newcol==NIL
      newcol:=::ColPos
      FOR i:=::LeftVisible TO ::ColCount
         ::RightVisible:=i
         IF ::ColWidths[::LeftVisible] + ::nDataWidth < ::ColWidths[i+1]
            EXIT
         ENDIF
      NEXT
      ::RowCountVisible:=Ceiling(::nDataHeight/::ItemHeight)
      ::drawheader()
      ::drawdata()
    ELSE
      ::ColPos:=Max(1,newcol)
      ::ColPos:=Min(::ColPos,::ColCount)
      IF ::ColPos>=::RightVisible .OR. ::ColWidths[::ColPos+1] - ::ColWidths[::LeftVisible] > ::nDataWidth
         ::RightVisible:=::ColPos
         ::LeftVisible:=::ColPos
         FOR i:=::RightVisible TO 1 STEP -1
            IF ::ColWidths[::RightVisible+1] - ::ColWidths[i] > ::nDataWidth
               EXIT
            ELSE
               ::LeftVisible:=i
            ENDIF
         NEXT
         FOR i:=::LeftVisible TO ::ColCount
            ::RightVisible:=i
            IF ::ColWidths[::LeftVisible] + ::nDataWidth < ::ColWidths[i+1]
               EXIT
            ENDIF
         NEXT
         ::drawheader()
         ::drawdata()
        ELSEIF ::ColPos < ::LeftVisible
         ::LeftVisible:=::ColPos
         FOR i:=::LeftVisible TO ::ColCount
            ::RightVisible:=i
            IF ::ColWidths[::LeftVisible] + ::nDataWidth < ::ColWidths[i+1]
               EXIT
            ENDIF
         NEXT
         ::drawheader()
         ::drawdata()
      ENDIF
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD DeHilite(lInternal) CLASS TWBrowse
   LOCAL lRet:=::Hilited
   DEFAULT lInternal TO .F.
   IF ::Hilited
      IF (::wantAutoHilite .OR. !lInternal)
         ::Hilited:=.F.
         ::drawdata(,::RowPos,IF(::wantHiliteAll,NIL,::ColPos))
      ENDIF
   ENDIF
RETURN lRet

//---------------------------------------------------------------------------------------------

METHOD Hilite(lInternal) CLASS TWBrowse
   LOCAL lRet:=::Hilited
   DEFAULT lInternal TO .F.
   IF !::Hilited
      IF  (::wantAutoHilite .OR. !lInternal)
         ::Hilited:=.T.
         ::drawdata(,::RowPos,IF(::wantHiliteAll,NIL,::ColPos))
      ENDIF
   ENDIF
RETURN lRet

//---------------------------------------------------------------------------------------------

METHOD drawdata(hDC,nRow,nCol,nRowStop) CLASS TWBrowse

   LOCAL nColStart
   LOCAL nColEnd
   LOCAL nRowStart
   LOCAL nRowEnd
   LOCAL lNoDC:=Empty(hDC)
   LOCAL i,j
   LOCAL l
   LOCAL t
   LOCAL r
   LOCAL b
   LOCAL hOldFont
   LOCAL isHilite
   LOCAL nDataEnd
   LOCAL hOldPen
   LOCAL n,nY
   LOCAL nTextHeight
   LOCAL x,xi,yi,cx,cy,nB
   LOCAL lHasFocus:=(GetFocus()==::handle)

   nColStart:=Max(1,IF(nCol==NIL,::LeftVisible,nCol))
   nColEnd:=Max(1,IF(nCol==NIL,::RightVisible,nCol))
   nRowStart:=IF(nRow==NIL,1,nRow)
   nRowEnd:=IF(nRowStop==NIL,IF(nRow==NIL,::RowCountVisible,nRow),nRowStop)

   IF lNoDC
      hDC:=GetDC(::handle)
   ENDIF

   hOldFont:=SelectObject(hDC,::Font)
   hOldPen:=SelectObject(hDC,::LinePen)

   nDataEnd:=Len(::aData)

   BEGIN SEQUENCE
      FOR i:=nRowStart TO nRowEnd
         t:=::nDataTop+(i-1)*::ItemHeight
         b:=MIN(::Height,t+::ItemHeight-1)
         FOR j:=nColStart TO nColEnd
            l:=::ColWidths[j]-::ColWidths[::LeftVisible]
            r:=MIN(::Width,::ColWidths[j+1]-::ColWidths[::LeftVisible]-1)
            isHilite:= ::Hilited .AND. (i==::RowPos) .AND. (::wantHiliteAll .OR. j==::ColPos)
            SetBkColor(hDC,IF(isHilite,IF(lHasFocus,::HiliteBgColor,::HiliteNoFocus),IF(::aBgColors[i,j]==NIL,::BgColor,::aBgColors[i,j])))
            SetTextColor(hDC,IF(isHilite,::HiliteColor,IF(::aFgColors[i,j]==NIL,::FgColor,::aFgColors[i,j])))
            IF i<=nDataEnd
               SelectObject(hDC,::ColFonts[j] )
               SetTextAlign(hDC, ::ColAlign[j] )
               DO CASE
               CASE ::ColAlign[j]==TA_CENTER
                  x:=(l+r)/2
               CASE ::ColAlign[j]==TA_RIGHT
                  x:=r-::ColOffset[j]
               OTHERWISE
                  x:=l+::ColOffset[j]
               ENDCASE
               nTextHeight:=GetTextExtentPoint32(hDC," ")[2]
               DO CASE
               CASE ::ColVAlign[j]==TA_CENTER
                  IF ValType(::aData[i,2,j])=="C"
                     nY = (( (t+::ItemHeight)-nTextHeight)/2)+(t/2)
                  ELSE
                     nY = (( (t+::ItemHeight)-(nTextHeight*Len(::aData[i,2,j])) )/2)+(t/2)
                  END
               CASE ::ColVAlign[j]==TA_TOP
                  nY := IIF( ValType(::aData[i,2,j])=="C", t+1, t+2)
               CASE ::ColVAlign[j]==TA_BOTTOM
                  IF ValType(::aData[i,2,j])=="C"
                     nY = t+::ItemHeight-nTextHeight
                  ELSE
                     nY =  (t+::ItemHeight)-(nTextHeight*Len(::aData[i,2,j]))
                  END
               END
               IF ValType(::aData[i,2,j])=="C"
                  ExtTextOut(hDC,x,nY,ETO_CLIPPED+ETO_OPAQUE,{l,t,r+1,b+1},::aData[i,2,j])
               ELSEIF ValType(::aData[i,2,j])=="A"
                  ExtTextOut(hDC,x,nY,ETO_CLIPPED+ETO_OPAQUE,{l,t,r+1,b+1},::aData[i,2,j,1])
                  FOR n:=2 TO Len(::aData[i,2,j])
                     ExtTextOut(hDC,x,nY+(n-1)*nTextHeight,ETO_CLIPPED,{l,t,r+1,b+1},::aData[i,2,j,n])
                  NEXT
               ENDIF
               IF ::ColBmps[i,j]<> NIL .AND. ::ColBmps[i,j] >= 0 .AND. !Empty(::Columns[j]:ImageList)
                  ImageList_GetIconSize(::Columns[j]:ImageList,@cx,@cy)
                  DO CASE
                  CASE ::ColBmpAlign[j]==TA_CENTER
                     xi:=(l+r-cx)/2
                  CASE ::ColBmpAlign[j]==TA_RIGHT
                     xi:=r-cx-1-2
                  OTHERWISE
                     xi:=l+2
                  ENDCASE
                  ImageList_DrawEx(::Columns[j]:ImageList,::ColBmps[i,j],hDC,;
                                   xi, Max(t,t+(b-t-cy)/2),min(cx,r-l),min(cy,b-t),;
                                   CLR_NONE, CLR_NONE,ILD_TRANSPARENT)

               ENDIF
            ENDIF
            IF ::wantColSep
               PolyLine(hDC,{{l,b},{r+1,b}})
            ENDIF
            IF ::wantRowSep
               PolyLine(hDC,{{r,b},{r,t-1}})
            ENDIF
            IF isHilite .AND. lHasFocus .AND. ::wantDrawFocus
               DrawFocusRect(hDC,{l,t,r,b})
            ENDIF

         NEXT

         // paint over empty space
         IF j > ::ColCount .AND. r< ::nDataWidth
            SetBkColor(hDC,::BgColor)
            SetTextColor(hDC,::FgColor)
            nY = t+2
            ExtTextOut(hDC,r+2,nY,ETO_CLIPPED+ETO_OPAQUE,{r+1,t,::nDataWidth+0,b+1},"")
            IF ::wantRowSep
               PolyLine(hDC,{{r,b},{::nDataWidth,b}})
            ENDIF
         ENDIF
      NEXT
      SelectObject(hDC,::Font)
      SetBkColor(hDC,::BgColor)
      SetTextColor(hDC,::FgColor)
   END
   SelectObject(hDC,hOldPen)
   SelectObject(hDC,hOldFont)
   IF lNoDC
      hDC:=ReleaseDC(::handle,hDC)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD drawheader() CLASS TWBrowse
   IF IsWindow(::hWndHeader)
      MoveWindow(::hWndHeader,-::ColWidths[::LeftVisible],0,;
                 ::Width+::ColWidths[::LeftVisible],::HeadHeight,.T.)
      RedrawWindow(::hWndHeader,,,RDW_NOERASE+RDW_UPDATENOW)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnChange() CLASS TWBrowse
   IF ::OldRecNo <> ::RecPos .OR. ::OldColPos <> ::ColPos
      IF ::OldRecNo <> ::RecPos
         ::UpdateVScrollBar()
      ENDIF
      IF ::OldColPos <> ::ColPos
         ::UpdateHScrollBar()
      ENDIF
      IF ::wantNotify
         IF ValType(::bOnChangeBlock)=="B"
            EVAL(::bOnChangeBlock, self, ::RecPos, ::RowPos, ::ColPos )
         ENDIF
      ENDIF
   ENDIF
   ::OldRecNo:=::RecPos
   ::OldColPos:=::ColPos
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD UpdateHScrollBar(lRedraw) CLASS TWBrowse
   STATIC si
   IF ISNIL(si)
      si IS SCROLLINFO
      si:cbSize := si:sizeof()
      si:fMask  := SIF_ALL
      si:nMin   := 0
   ENDIF
   si:nMax   := ::ColWidths[::ColCount+1]
   si:nPage  := IF(::wantHiliteAll,::nDataWidth,(::ColWidths[::ColPos+1]-::ColWidths[::ColPos])) //::nDataWidth
   si:nPos   := IF(::wantHiliteAll,::ColWidths[::LeftVisible],::ColWidths[::ColPos]) //::ColWidths[::leftVisible]
   si:nTrackPos := 0
   SetScrollInfo( ::handle, SB_HORZ, si:value, TRUE )
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD UpdateVScrollBar(lRedraw) CLASS TWBrowse
   STATIC si
   IF ISNIL(si)
      si IS SCROLLINFO
      si:cbSize := si:sizeof()
      si:nMin   := 1
      si:fMask  := SIF_ALL
   ENDIF
   si:nMax   := ::RecCount
   si:nPage  := ::RowCountUsable
   si:nPos   := ::RecPos - ::RowPos + 1
   si:nTrackPos := 0
   SetScrollInfo( ::handle, SB_VERT, si:value,TRUE )
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnNotify( hdr, nlParam ) CLASS TWBrowse
   local c, a, hi
   LOCAL nmHdr IS NMHEADER
   
   DO CASE
      CASE hdr:code == HDN_BEGINTRACK
         nmHdr:Pointer(nlParam)
         IF !::OnBeginTrack(nmHdr)
            RETURN(1)
         ENDIF
      CASE hdr:code == HDN_TRACK
         nmHdr:Pointer(nlParam)
         ::OnTracking(nmHdr)

      CASE hdr:code == HDN_ENDTRACK
         nmHdr:Pointer(nlParam)
         IF ::OnEndTrack(nmHdr)
            RETURN(0)
         ENDIF

      CASE hdr:code==HDN_BEGINDRAG
         nmHdr:Pointer(nlParam)
         IF !::OnBeginDrag(nmHdr)
            RETURN(1)
         ENDIF

      CASE hdr:code == HDN_ENDDRAG
         c:=peek(nlParam,nmHdr:sizeof())
         a:=Bin2A(c,{{LONG,LONG,LONG},LONG,LONG,LONG})
         IF ::OnEndDrag(a)
            RETURN(1)
         ENDIF
         
      CASE hdr:code==HDN_GETDISPINFO
      CASE hdr:code==HDN_ITEMCHANGING
      CASE hdr:code==HDN_ITEMCHANGED
         nmHdr:Pointer(nlParam)
         ::OnItemChanged(nmHdr)
         RETURN(1)

      CASE hdr:code==HDN_ITEMCLICK
         nmHdr:Pointer(nlParam)
         ::GoToPos(,nmHdr:iItem+1,.T.)
         ::OnChange()
         ::OnHeadClick(nmHdr:iItem+1)

      CASE hdr:code==HDN_ITEMDBLCLICK
         nmHdr:Pointer(nlParam)
         ::GoToPos(,nmHdr:iItem+1,.T.)
         ::OnChange()
         ::OnHeadDblClick(nmHdr:iItem+1)

      CASE hdr:code==HDN_FILTERCHANGE

   ENDCASE
return(nil)

//---------------------------------------------------------------------------------------------

METHOD GoToPos(nRow,nCol,lInternal) CLASS TWBrowse
   ::DeHilite(lInternal)
   IF !Empty(nRow)
      ::GotoLine(nRow,lInternal)
   ENDIF
   IF nCol<> NIL
      ::GoToCol(nCol,IF(nCol==0,NIL,nCol))
   ENDIF
   ::Hilite(lInternal)
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnHeadClick(nPos) CLASS TWBrowse
   IF (::ColPos>0 .AND. ::ColPos<=::ColCount) .AND. ;
      (::ColPos>=::LeftVisible .AND. ::ColPos<=::RightVisible)
      IF ValType(::Columns[::ColPos]:bHeadOnClick)=='B'
         Eval(::Columns[::ColPos]:bHeadOnClick,self,nPos,1)
      ENDIF
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnBeginTrack(hdr) CLASS TWBrowse
   LOCAL aMouse:=GetCursorPos()
   LOCAL hDC
   LOCAL aRect
   LOCAL xColPos
   IF hdr:iItem >= 0 .AND. AND(::ColStyle[hdr:iItem+1],TBC_SIZE)>0
      hDC:=GetDC(::handle)
      ::xTrackColumn:=hdr:iItem+1
      xColPos:=::ColWidths[::xTrackColumn+1]-::ColWidths[::LeftVisible]
      ::lResizing:=.T.
      ScreenToClient(::handle,aMouse)
      ::xTrack:=aMouse[1]
      ::xTrackOffset:=::xTrack-xColPos
      aRect:={::xTrack-1-::xTrackOffset,::HeadHeight,::xTrack+1-::xTrackOffset,::Height}
      InvertRect(hDC,aRect)
      ReleaseDC(::handle,hDC)
      RETURN(.T.)
   ENDIF
RETURN( .F. )
 
//---------------------------------------------------------------------------------------------

METHOD OnBeginDrag( hdr) CLASS TWBrowse
   IF hdr:iItem >=0 .AND. AND(::ColStyle[hdr:iItem+1],TBC_MOVE)>0
      ::lMoving:=.T.
      ::xDragColumn:=hdr:iItem+1
      RETURN(.T.)
   ENDIF
RETURN(.F.)

//---------------------------------------------------------------------------------------------
/*
METHOD OnEndDrag(hdr) CLASS TWBrowse
   LOCAL lRet:=.F.
   IF ::lMoving
      ::lMoving:=.F.
      IF ::xDragColumn <> hdr:pItem:iOrder+1 .AND. hdr:pItem:iOrder>=0
         ::SetColPos(::xDragColumn,hdr:pItem:iOrder+1)
         lRet:=.T.
      ENDIF
      ::xDragColumn:=0
   ENDIF
RETURN(lRet)
*/

METHOD OnEndDrag(a)
   STATIC xTypes:={"-4","-4","-4","-4","-4","-4","-4","-4","-4"}
   LOCAL aItem
   LOCAL cItem
   LOCAL lRet:=.F.
   IF ::lMoving
      ::lMoving:=.F.
      cItem:=peek(a[4],9*4)
      IF cItem <> NIL
         aItem:=Bin2Array(cItem,@xTypes)
         IF ::xDragColumn <> aItem[9]+1 .AND. aItem[9]>=0
            ::SetColPos(::xDragColumn,aItem[9]+1)
            lRet:=.T.
         ENDIF
      ENDIF
      ::xDragColumn:=0
   ENDIF
RETURN(lRet)

//---------------------------------------------------------------------------------------------

METHOD SetColPos(nCol,nPos,lInternal) CLASS TWBrowse
   LOCAL oCol:=::Columns[nCol]
   ::DeHilite(lInternal)
   ::DelColumn(nCol,,.F.)
   ::InsColumn(nPos,oCol,.T.)
   ::Hilite(lInternal)
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD DelColumn(nCol,nPosType,lConfigure) CLASS TWBrowse
   LOCAL OldCol
   LOCAL nPos
   IF nPosType==NIL .OR. nPosType==TBC_POSITION
      DEFAULT nCol TO ::ColPos
   ENDIF
   DEFAULT nPosType TO TBC_POSITION
   IF nPosType==TBC_POSITION
      nPos:=nCol
      IF nPos<=0 .OR. nPos>::ColCount
         RETURN(.F.)
      ENDIF
     ELSE
      IF (nPos:=aScan(::Columns,{|o| o:OriginalIndex==nCol})) == 0
         RETURN(.F.)
      ENDIF
   ENDIF
   OldCol:=::Columns[nPos]
   aDel(::Columns,nPos)
   ASIZE(::Columns,Len(::Columns)-1)
   ::ColCount:=Len(::Columns)
   OldCol:Kill()
   DEFAULT lConfigure TO .T.
   IF lConfigure
      ::Configure()
   ENDIF
RETURN(.T.)

//---------------------------------------------------------------------------------------------

METHOD RefreshCurrent(lredraw,COL,row) CLASS TWBrowse
   LOCAL j
   LOCAL arec:={0,0,0,0}
   LOCAL startcol:=IF(COL==NIL,1,COL)
   LOCAL endcol:=IF(COL==NIL,::ColCount,COL)
   IF row==NIL
      row:=::RowPos
   ENDIF
   BEGIN SEQUENCE
      FOR j:=startcol TO endcol
         ::aData[row,2,j]:=Eval(::Columns[j]:block,::Columns[j],self,::aData[row,1],row)
         IF ValType(::Columns[j]:bmpindex)=="B"
            ::ColBmps[row,j]:=EVAL(::Columns[j]:bmpindex,::Columns[j],self,::aData[row,1],row)
         ELSE
            ::ColBmps[row,j]:=::Columns[j]:bmpindex
         ENDIF
         IF ValType(::ColFgColors[j])=="B"
            ::aFgColors[row,j]:=EVAL(::ColFgColors[j],::Columns[j],self,::aData[row,1],row)
         ELSE
            ::aFgColors[row,j]:=::ColFgColors[j]
         ENDIF
         IF ValType(::ColBgColors[j])=="B"
            ::aBgColors[row,j]:=EVAL(::ColBgColors[j],::Columns[j],self,::aData[row,1],row)
         ELSE
            ::aBgColors[row,j]:=::ColBgColors[j]
         ENDIF
      NEXT
      IF IF(ValType(lredraw)=='L',lredraw,.F.)
         ::drawdata(,row,COL)
      ENDIF

   END
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Kill() CLASS TWBrowse
   IF ::LinePen # NIL
      DeleteObject(::LinePen)
      ::LinePen:=NIL
   ENDIF
   aEval(::Columns,{|x| x:Kill()})
   ::Columns:={}
   IF ValType(::bKillBlock)=="B"
      EVAL(::bKillBlock)
   ENDIF
   DeleteObject( ::Font )
   DeleteObject( ::HeadFont )
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD SetColumn(nCol, colob, nPosType, lConfigure ) CLASS TWBrowse
   LOCAL nIndex
   LOCAL OldCol
   LOCAL nPos
   DEFAULT nPosType TO TBC_POSITION
   IF nPosType==TBC_POSITION
      nPos:=nCol
      IF nPos<=0 .OR. nPos>::ColCount
         RETURN(0)
      ENDIF
     ELSE
      IF (nPos:=aScan(::Columns,{|o| o:OriginalIndex==nCol})) == 0
         RETURN(0)
      ENDIF
   ENDIF
   nIndex:=::Columns[nPos]:OriginalIndex
   colob:OriginalIndex:=nIndex
   ::DelColumn(nPos,,.F.)
   ::InsColumn(nPos,colob,.F.)
   colob:OriginalIndex:=nIndex
   DEFAULT lConfigure TO .T.
   IF lConfigure
      ::Configure()
   ENDIF
RETURN(IF(nPosType==TBC_POSITION,nIndex,nPos))

//---------------------------------------------------------------------------------------------

METHOD GetColumn(nCol,nPosType) CLASS TWBrowse
   LOCAL nPos
   IF nPosType==NIL .OR. nPosType==TBC_POSITION
      DEFAULT nCol TO ::ColPos
   ENDIF
   DEFAULT nPosType TO TBC_POSITION
   IF nPosType==TBC_POSITION
      IF nCol>0 .AND. nCol<=::ColCount
         RETURN(::Columns[nCol])
      ENDIF
     ELSE
      IF (nPos:=aScan(::Columns,{|o| o:OriginalIndex==nCol})) > 0
         RETURN(::Columns[nPos])
      ENDIF
   ENDIF
RETURN(nil)

//---------------------------------------------------------------------------------------------

METHOD SetColWidth(nCol,nWidth) CLASS TWBrowse
   LOCAL i
   LOCAL nDiff:=nWidth-(::ColWidths[nCol+1]-::ColWidths[nCol])
   LOCAL nMaxCol:=Len(::ColWidths)
   FOR i:=nCol+1 TO ::ColCount+1
      ::ColWidths[i]+=nDiff
      ::Columns[nCol]:Width:=::ColWidths[nCol+1]-::ColWidths[nCol]
   NEXT
   ::GoToCol()
   ::UpdateHScrollBar(.T.)
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD GetColPos(nIndex) CLASS TWBrowse
   RETURN aScan(::Columns,{|o| o:OriginalIndex==nIndex})

//---------------------------------------------------------------------------------------------

METHOD GetColIndex(nCol) CLASS TWBrowse
   DEFAULT nCol TO ::ColPos
   IF nCol>0 .AND. nCol<=::ColCount
      RETURN ::Columns[nCol]:OriginalIndex
   ENDIF
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD GetColBlock(nCol,nPosType) CLASS TWBrowse
   LOCAL nPos
   IF nPosType==NIL .OR. nPosType==TBC_POSITION
      DEFAULT nCol TO ::ColPos
   ENDIF
   DEFAULT nPosType TO TBC_POSITION
   IF nPosType==TBC_POSITION
      IF nCol>0 .AND. nCol<=::ColCount
         RETURN(::Columns[nCol]:Block)
      ENDIF
     ELSE
      IF (nPos:=aScan(::Columns,{|o| o:OriginalIndex==nCol})) > 0
         RETURN(::Columns[nPos]:Block)
      ENDIF
   ENDIF
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD GetColFont(nCol,nPosType) CLASS TWBrowse
   LOCAL nPos
   IF nPosType==NIL .OR. nPosType==TBC_POSITION
      DEFAULT nCol TO ::ColPos
   ENDIF
   DEFAULT nPosType TO TBC_POSITION
   IF nPosType==TBC_POSITION
      IF nCol>0 .AND. nCol<=::ColCount
         RETURN(::ColFonts[nCol])
      ENDIF
     ELSE
      IF (nPos:=aScan(::Columns,{|o| o:OriginalIndex==nCol})) > 0
         RETURN(::ColFonts[nPos])
      ENDIF
   ENDIF
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD RestoreColumnWidth(nCol, lConfigure) CLASS TWBrowse
   LOCAL i
   LOCAL n
   LOCAL nDiff
   LOCAL nMaxCol
   LOCAL nStartCol:=IF(nCol==NIL,1,Max(MIN(::ColCount,nCol),1))
   LOCAL nEndCol  :=IF(nCol==NIL,::ColCount,Max(MIN(::ColCount,nCol),1))
   LOCAL nWidth
   FOR n:=nStartCol TO nEndCol
      nCol:=n
      nWidth:=::Columns[nCol]:OriginalWidth
      nDiff:=nWidth-(::ColWidths[nCol+1]-::ColWidths[nCol])
      nMaxCol:=Len(::ColWidths)
      FOR i:=nCol+1 TO ::ColCount+1
         ::ColWidths[i]+=nDiff
         ::Columns[nCol]:Width:=::ColWidths[nCol+1]-::ColWidths[nCol]
      NEXT
   NEXT
   ::RefreshAll()
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD RestoreColumnOrder(nCol) CLASS TWBrowse
   RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Freeze() CLASS TWBrowse
   RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnTracking() CLASS TWBrowse
   LOCAL aMouse:=GetCursorPos()
   LOCAL hDC
   LOCAL aRect
   IF ::lResizing
      hDC:=GetDC(::handle)
      IF ::xTrack-::xTrackOffset >=0
         aRect:={::xTrack-1-::xTrackOffset,::HeadHeight,::xTrack+1-::xTrackOffset,::Height}
         InvertRect(hDC,aRect)
      ENDIF
      ScreenToClient(::handle,aMouse)
      ::xTrack:=aMouse[1]
      IF ::xTrack-::xTrackOffset >= 0
         aRect:={::xTrack-1-::xTrackOffset,::HeadHeight,::xTrack+1-::xTrackOffset,::Height}
         InvertRect(hDC,aRect)
      ENDIF
      ReleaseDC(::handle,hDC)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnItemChanged(hdr) CLASS TWBrowse
DO CASE
   CASE ::xTrackColumn<>0
        ::SetColWidth( ::xTrackColumn, hdr:pItem:cxy)
   ENDCASE
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnHeadDblClick(nPos) CLASS TWBrowse
   IF (::ColPos>0 .AND. ::ColPos<=::ColCount) .AND. ;
      (::ColPos>=::LeftVisible .AND. ::ColPos<=::RightVisible)
      IF ValType(::Columns[::ColPos]:bHeadOnClick)=='B'
         Eval(::Columns[::ColPos]:bHeadOnClick,self,nPos,2)
      ENDIF
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnEndTrack() CLASS TWBrowse
   LOCAL hDC
   LOCAL aRect
   IF ::lResizing
      ::lResizing:=.F.
      IF ::xTrack-::xTrackOffset >=0
         hDC:=GetDC(::handle)
         aRect:={::xTrack-1-::xTrackOffset,::HeadHeight,::xTrack+1-::xTrackOffset,::Height}
         InvertRect(hDC,aRect)
         ReleaseDC(::handle,hDC)
         RETURN(.T.)
      ENDIF
   ENDIF
RETURN(.F.)

//---------------------------------------------------------------------------------------------

METHOD Up(lInternal) CLASS TWBrowse
   LOCAL lTop
   IF ::RowPos> 1
      ::DeHilite(lInternal)
      ::GotoLine(::RowPos-1,lInternal)
      ::Hilite(lInternal)
     ELSE
      EVAL(::bSkip,-1)
      lTop:=::HitTop
      IF lTop
         EVAL(::bGoTop)
        ELSE
         EVAL(::bSkip,1)
         ::DeHilite(lInternal)
         ::ScrollUp(1,lInternal)
         ::Hilite(lInternal)
      ENDIF
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Down(lInternal) CLASS TWBrowse
   LOCAL lBottom
   IF ::RowPos < ::RowCountUsable
      ::DeHilite(lInternal)
      ::GotoLine(::RowPos+1,lInternal)
      ::Hilite(lInternal)
     ELSE
      EVAL(::bSkip,1)
      lBottom:=::HitBottom
      IF lBottom
         EVAL(::bGoBottom)
        ELSE
         EVAL(::bSkip,-1)
         ::DeHilite(lInternal)
         ::ScrollDown(1,lInternal)
         ::Hilite(lInternal)
      ENDIF
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Pgup(lInternal) CLASS TWBrowse
   Local lTop:=.F.
   IF ::RowPos == 1
      EVAL(::bSkip,-1)
      lTop:=::HitTop
      IF !lTop
        EVAL(::bSkip,1)
      Else
        EVAL(::bGoTop)
      Endif
   Endif
   IF !lTop
     ::DeHilite(lInternal)
     ::ScrollUp(::RowCountUsable,lInternal)
     ::Hilite(lInternal)
   Endif
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Pgdown(lInternal) CLASS TWBrowse
   LOCAL lBottom:=.F.
   IF ::RowPos >= ::RowCountUsable .OR. ::RowPos >= LEN(::aData)
      EVAL(::bSkip,1)
      lBottom:=::HitBottom
      IF !lBottom
        EVAL(::bSkip,-1)
      Else
        EVAL(::bGoBottom)
      Endif
   Endif
   IF !lBottom
      ::DeHilite(lInternal)
      ::ScrollDown(::RowCountUsable,lInternal)
      ::Hilite(lInternal)
   Endif
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD Home(lInternal) CLASS TWBrowse
   LOCAL lTop
   IF ::RowPos>1
      ::DeHilite(lInternal)
      ::GotoLine(1,lInternal)
      ::Hilite(lInternal)
   ENDIF
   EVAL(::bSkip,-1)
   lTop:=::HitTop
   Eval(::bGoTop)
   IF !lTop
      ::DeHilite(lInternal)
      ::RefreshAll()
      ::Hilite(lInternal)
   ELSE
      MessageBeep(MB_ICONSTOP)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD EndKey(lInternal) CLASS TWBrowse
   LOCAL lBottom
   LOCAL nLastLine := MIN(::RowCountUsable,::RecCount)
   IF ::RowPos < nLastLine
      ::DeHilite(lInternal)
      ::GotoLine(nLastLine,lInternal)
      ::Hilite(lInternal)
   ENDIF
   EVAL(::bSkip,1)
   lBottom:=::HitBottom
   Eval(::bGoBottom)
   IF !lBottom
      ::DeHilite(lInternal)
      ::RefreshAll()
      ::Hilite(lInternal)
     ELSE
      MessageBeep(MB_ICONSTOP)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD KeyLeft(lInternal) CLASS TWBrowse
   IF ::wantHiliteAll
      RETURN ::PanLeft(lInternal)
   ENDIF
   IF ::ColPos> 1
      ::DeHilite(lInternal)
      ::GoToCol(Max(1,::ColPos-1))
      ::Hilite(lInternal)
   ENDIF
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD KeyRight(lInternal) CLASS TWBrowse
   IF ::wantHiliteAll
      RETURN ::PanRight(lInternal)
   ENDIF
   IF ::ColPos <::ColCount
      ::DeHilite(lInternal)
      ::GoToCol(min(::ColPos+1,::ColCount))
      ::Hilite(lInternal)
   ENDIF
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD PanLeft(lInternal) CLASS TWBrowse
   IF ::LeftVisible > 1
      ::DeHilite(lInternal)
      ::GoToCol(Max(1,::LeftVisible-1))
      ::Hilite(lInternal)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD PanRight(lInternal) CLASS TWBrowse
   IF ::RightVisible < ::ColCount .OR. ;
      ( ::LeftVisible<::ColCount  .AND. ::ColWidths[::ColCount+1] - ::ColWidths[::LeftVisible] > ::nDataWidth )
      ::LeftVisible++
      ::ColPos:=min(::ColPos+1,::ColCount)
      ::DeHilite(lInternal)
      ::GoToCol()
      ::Hilite(lInternal)
   ELSEIF ::ColPos < ::RightVisible .AND. !::wantHiliteAll
      ::DeHilite(lInternal)
      ::GoToCol(min(::ColPos+1,::ColCount))
      ::Hilite(lInternal)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD PanHome(lInternal) CLASS TWBrowse
   ::DeHilite(lInternal)
   ::GoToCol(1)
   ::Hilite(lInternal)
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD PanEnd(lInternal) CLASS TWBrowse
   ::DeHilite(lInternal)
   ::GoToCol(::ColCount)
   ::DeHilite(lInternal)
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD ScrollUp(n,lInternal) CLASS TWBrowse
   LOCAL scrolled:=0
   LOCAL i
   LOCAL j
   EVAL(::bSkip,-(::RowPos-1))
   FOR i:=1 TO n
      Eval(::bSkip,-1)
      IF ::HitTop
         EXIT
        ELSE
         scrolled++
         aIns(::aData,1)
         aIns(::ColBmps,1)
         aIns(::aFgColors,1)
         aIns(::aBgColors,1)
         ::aData[1]:={Eval(::bRecNo),array(::ColCount)}
         ::ColBmps[1]:=array(::ColCount)
         ::aFgColors[1]:=ARRAY(::ColCount)
         ::aBgColors[1]:=ARRAY(::ColCount)
         FOR j:=1 TO ::ColCount
            ::aData[1,2,j]:=Eval(::Columns[j]:block,::Columns[j], self,::aData[1,1],1)
            IF ValType(::Columns[j]:bmpindex)=="B"
               ::ColBmps[1,j]:=EVAL(::Columns[j]:bmpindex,::Columns[j],self,::aData[1,1],1)
            ELSE
               ::ColBmps[1,j]:=::Columns[j]:bmpindex
            ENDIF
            IF ValType(::ColFgColors[j])=="B"
               ::aFgColors[1,j]:=EVAL(::ColFgColors[j],::Columns[j], self,::aData[1,1],1)
            ELSE
               ::aFgColors[1,j]:=::ColFgColors[j]
            ENDIF
            IF ValType(::ColBgColors[j])=="B"
               ::aBgColors[1,j]:=EVAL(::ColBgColors[j],::Columns[j], self,::aData[1,1],1)
            ELSE
               ::aBgColors[1,j]:=::ColBgColors[j]
            ENDIF
         NEXT
      ENDIF
   NEXT

   IF scrolled==n
      Eval(::bSkip,::RowPos-1)
      ::Display(IF(scrolled==n,scrolled,))
   ELSE
      ::RowPos:=1
      ::Home(lInternal)
      ::drawdata()
   ENDIF
RETURN(scrolled)

//---------------------------------------------------------------------------------------------

METHOD ScrollDown(n,lInternal) CLASS TWBrowse
   LOCAL scrolled:=0
   LOCAL i
   LOCAL j
   IF Len(::aData) > 0 .AND. ::aData[::RowCountVisible,1] > 0
      EVAL(::bSkip,::RowCountVisible-::RowPos)
      FOR i:=1 TO n
         Eval(::bSkip,1)
         IF ::HitBottom .AND. ::RowCountVisible==::RowCountUsable
            EXIT
         ELSE
            scrolled++
            aDel(::aData,1)
            aDel(::ColBmps,1)
            ADEL(::aFgColors,1)
            ADEL(::aBgColors,1)
            ::aData[::RowCountVisible]:={0,array(::ColCount)}
            ::ColBmps[::RowCountVisible]:=array(::ColCount)
            ::aFgColors[::RowCountVisible]:=ARRAY(::ColCount)
            ::aBgColors[::RowCountVisible]:=ARRAY(::ColCount)
            IF ::HitBottom
               aFill(::aData[::RowCountVisible,2],"")
               EXIT
            ELSE
               ::aData[::RowCountVisible,1]:=Eval(::bRecNo)
               FOR j:=1 TO ::ColCount
                  ::aData[::RowCountVisible,2,j]:=Eval(::Columns[j]:block,::Columns[j],self,::aData[::RowCountVisible,1],::RowCountVisible)
                  IF ValType(::Columns[j]:bmpindex)=="B"
                     ::ColBmps[::RowCountVisible,j]:=EVAL(::Columns[j]:bmpindex,::Columns[j],self,::aData[::RowCountVisible,1],::RowCountVisible)
                  ELSE
                     ::ColBmps[::RowCountVisible,j]:=::Columns[j]:bmpindex
                  ENDIF
                  IF ValType(::ColFgColors[j])=="B"
                     ::aFgColors[::RowCountVisible,j]:=EVAL(::ColFgColors[j],::Columns[j],self,::aData[::RowCountVisible,1],::RowCountVisible)
                  ELSE
                     ::aFgColors[::RowCountVisible,j]:=::ColFgColors[j]
                  ENDIF
                  IF ValType(::ColBgColors[j])=="B"
                     ::aBgColors[::RowCountVisible,j]:=EVAL(::ColBgColors[j],::Columns[j],self,::aData[::RowCountVisible,1],::RowCountVisible)
                  ELSE
                     ::aBgColors[::RowCountVisible,j]:=::ColBgColors[j]
                  ENDIF
               NEXT
            ENDIF
         ENDIF
      NEXT
      IF scrolled==n
         EVAL(::bSkip,-(::RowCountVisible-::RowPos))
         ::Display(IF(scrolled==n,-scrolled,))
      ELSE //scrolled <> 0
         ::EndKey(lInternal)
         ::drawdata()
      ENDIF
   ELSE
      ::EndKey(lInternal)
      ::drawdata()
   ENDIF
RETURN(scrolled)

//---------------------------------------------------------------------------------------------

METHOD Display(n) CLASS TWBrowse

   LOCAL aScroll
   LOCAL aClip
   LOCAL temprow:=::RowPos
   IF n==NIL .OR. abs(n) >= ::RowCountVisible
      ::drawheader()
      ::drawdata()
   ELSE
      IF n>0
         aScroll:={0,::HeadHeight,::nDataWidth,::nDataHeight+::HeadHeight}
         aClip:={0,::HeadHeight,::nDataWidth,::nDataHeight+::HeadHeight}
         ScrollWindow(::handle,0,n*::ItemHeight,aScroll,aClip)
         ::drawdata(,1,,n)
      ELSEIF n<0
         aScroll:={0,::HeadHeight+n*::ItemHeight,::nDataWidth,::nDataHeight+::HeadHeight}
         aClip:={0,::HeadHeight,::nDataWidth,::nDataHeight+::HeadHeight}
         ScrollWindow(::handle,0,n*::ItemHeight,aScroll,aClip)
         ::drawdata(,::RowCountUsable+n+1,,::RowCountVisible)
      ENDIF
      ValidateRect(::handle)
   ENDIF
RETURN(self)

//---------------------------------------------------------------------------------------------

METHOD OnKeyDown( nwParam, nlParam)  CLASS TWBrowse
   LOCAL lShift,h
   SetFocus(::handle)
   IF ValType(::bOnKey) # 'B' .OR. !eval(::bOnKey,self,nwParam,nlParam)
      DO CASE
      CASE nwParam==9 // TAB
         lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
         IF ( h := GetNextDlgTabItem( ::Parent:handle , ::handle, lShift ) ) # 0
            SetFocus( h )
            RETURN (0)
         ENDIF
      CASE nwParam==VK_UP
         ::Up(.T.)

      CASE nwParam==VK_DOWN
         ::Down(.T.)

      CASE nwParam==VK_NEXT
         ::Pgdown(.T.)

      CASE nwParam==VK_PRIOR
         ::Pgup(.T.)

      CASE nwParam==VK_END
         ::EndKey(.T.)

      CASE nwParam==VK_HOME
         ::Home(.T.)

      CASE nwParam == VK_LEFT
         ::KeyLeft(.T.)

      CASE nwParam == VK_RIGHT
         ::KeyRight(.T.)

      ENDCASE
   ENDIF
   ::OnChange()
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD OnLButtonDown(nwParam,xPos,yPos) CLASS TWBrowse

   LOCAL nClickLine:=Ceiling((yPos-::HeadHeight) /::ItemHeight)
   LOCAL nClickCol:=aScan(::ColWidths,{|x| x-::ColWidths[::LeftVisible]>xPos})-1
   LOCAL lLineChange:=.F.

   SetFocus(::handle)
   IF nClickCol > ::ColCount
      nClickCol=::ColCount
   ELSEIF nClickCol ==-1
      nClickCol:=::RightVisible
   ELSEIF nClickCol < 1
      nClickCol:=::LeftVisible
   ENDIF
   ::DeHilite(.T.)
   ::GoToCol(nClickCol,.F.)
   IF nClickLine # ::RowPos
      lLineChange:=::GotoLine(nClickLine,.T.)
   ENDIF
   ::Hilite(.T.)
   IF !lLineChange
      RETURN(1)
   ENDIF
   ::OnChange()
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD GotoLine(Line,lInternal) CLASS TWBrowse
   IF Line > 0 .AND. Line<= ::RowCountVisible .AND.;
      Line <> ::RowPos .AND.  ::aData[Line,1] # 0
      IF Line <=::RowCountUsable
         EVAL(::bSkip,Line-::RowPos)
         ::RowPos:=Line
         RETURN(.T.)
      ELSE
         Line:=::RowCountUsable
         EVAL(::bSkip,Line-::RowPos)
         ::RowPos:=Line
         RETURN ::ScrollDown(1,lInternal) > 0
      ENDIF
   ENDIF
RETURN(.F.)

//---------------------------------------------------------------------------------------------

METHOD OnLButtonDblClk() CLASS TWBrowse
   IF (::ColPos>0 .AND. ::ColPos<=::ColCount) .AND. ;
      (::ColPos>=::LeftVisible .AND. ::ColPos<=::RightVisible)
      IF ValType(::bOnDblClick)=='B'
         IF ::RowPos > 0
            Eval(::bOnDblClick,self,::RowPos,::ColPos)
         ENDIF
      ELSEIF AND(::Columns[::ColPos]:Style,TBC_READWRITE)==TBC_READWRITE
         ::EditCell(,::Columns[::ColPos]:bSaveBlock,,,,)
      ENDIF
   ENDIF
   ::OnChange()
RETURN(nil)

//---------------------------------------------------------------------------------------------

METHOD OnChar(nwParam) CLASS TWBrowse
   local nAlign, key := nwParam
   IF ( nwParam < 32 .OR. nwParam > 128 )
      RETURN( NIL )
   ENDIF
   IF (::ColPos>0 .AND. ::ColPos<=::ColCount) .AND. ;
      (::ColPos>=::LeftVisible .AND. ::ColPos<=::RightVisible)
      IF ValType(::Columns[::ColPos]:bOnChar)=='B'
         IF ::RowPos > 0
            Eval(::Columns[::ColPos]:bOnChar,self,key)
         ENDIF
      ELSEIF AND(::Columns[::ColPos]:Style,TBC_READWRITE)==TBC_READWRITE
         DO CASE
            CASE ::Colalign[::ColPos]==TA_RIGHT
                 nAlign:=ES_RIGHT
            CASE ::Colalign[::ColPos]==TA_LEFT
                 nAlign:=ES_LEFT
            CASE ::Colalign[::ColPos]==TA_CENTER
                 nAlign:=ES_CENTER
         ENDCASE
         ::EditCell(,::Columns[::ColPos]:bSaveBlock,nAlign,,,key)
      ENDIF
   ENDIF
   ::OnChange()
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD EditCell(limit,bEndBlock,xStyle,lAsNumber,aColor,FirstKey) CLASS TWBrowse
   LOCAL hWin
   LOCAL nProc
   LOCAL aRect
   LOCAL cRect:=Space(8)
   LOCAL cText
   LOCAL hDC
   local nL,nR
   IF !(::wantHiliteAll .AND. ::ColCount > 0 )
      IF !::HitBottom .AND. !::HitTop
         SetFocus(::handle)
         IF (aRect:=::GetItemRect())==NIL
            RETURN(NIL)
         ENDIF
         cText:=::GetItemText()
         IF cText==NIL
            RETURN(NIL)
         ELSEIF ValType(cText)=="A"
            cText:=a2str(cText,CHR(13)+CHR(10))
         ENDIF
         IF xStyle==NIL
            xStyle:=0
         ENDIF
         hWin:=CreateWindow("edit",cText,;
                            WS_CHILD+WS_BORDER+WS_VISIBLE+ES_AUTOHSCROLL+ES_MULTILINE+4096+xStyle,;
                            aRect[1]-1,aRect[2]-1,aRect[3]-aRect[1]+1,aRect[4]-aRect[2]+1,;
                            ::handle,)
         nProc:=SetProcedure(hWin,{|hWin,nMsg,nwParam,nlParam| ;
                                   ::EditCellProc(nProc,hWin,nMsg,nwParam,nlParam,bEndBlock,,aColor)},;
                             {WM_KILLFOCUS,WM_KEYUP,WM_CTLCOLOREDIT,WM_KEYDOWN,WM_CHAR,WM_USER+322,WM_USER+321})

         IF ValType(limit)=='N'
            SendMessage(hWin,EM_LIMITTEXT,limit,0)
         ENDIF

         IF ValType(lAsNumber)=="L" .AND. lAsNumber
            EditAsNumber(hWin)
         ENDIF
         SendMessage(hWin,WM_SETFONT,::GetColFont(),MAKELPARAM(1,0))
         SendMessage(hWin,WM_USER+322,0,0)
         SetFocus(hWin)
         IF FirstKey # NIL
            PostMessage(hWin,WM_CHAR,FirstKey,0)
         ENDIF
        ELSE
         MessageBeep(-1)
      ENDIF
   ENDIF
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD EditCellProc(nProc,hWin,nMsg,nwParam,nlParam,bEndBlock,oGet,aColor) CLASS TWBrowse
   LOCAL cText
   LOCAL TempFocus
   STATIC isDone:=.F.
   DO CASE
   CASE nMsg==WM_USER+322
      isDone:=.F.
   CASE nMsg==WM_KILLFOCUS
      IF IsWindow(hWin)
         IF !isDone
            cText:=GetWindowText(hWin)
            PostMessage(hWin,WM_USER+321,0,0)
            IF ValType(bEndBlock)=='B'
               isDone:=.T.
               Eval(bEndBlock,cText,oGet,0)
            ENDIF
         ENDIF
      ENDIF

   CASE nMsg==WM_USER+321
      IF IsWindow(hWin)
         DestroyWindow(hWin)
      ENDIF
      RETURN(0)
   CASE nMsg==WM_KEYDOWN
      IF nwParam==VK_ESCAPE .OR. nwParam==VK_RETURN .OR. nwParam==VK_UP .OR. nwParam==VK_DOWN
         RETURN(0)
      ENDIF
   CASE nMsg==WM_KEYUP
      IF nwParam==VK_ESCAPE .OR. nwParam==VK_RETURN .OR. nwParam==VK_UP .OR. nwParam==VK_DOWN
         IF nwParam==VK_UP .OR. nwParam==VK_DOWN
            IF IsWindow(hWin)
               isDone:=.T.
               cText:=GetWindowText(hWin)
               PostMessage(hWin,WM_USER+321,nwParam,0)
               IF ValType(bEndBlock)=='B'
                  Eval(bEndBlock,cText,oGet,nwParam)
               ENDIF
            ENDIF
         ENDIF
         RETURN(0)
      ENDIF
   CASE nMsg==WM_CHAR
      IF nwParam==VK_RETURN
         IF IsWindow(hWin)
            isDone:=.T.
            cText:=GetWindowText(hWin)
            PostMessage(hWin,WM_USER+321,nwParam,0)
            IF ValType(bEndBlock)=='B'
               Eval(bEndBlock,cText,oGet,nwParam)
            ENDIF
         ENDIF
         RETURN(0)
      ELSEIF nwParam==VK_ESCAPE
         isDone:=.T.
         PostMessage(hWin,WM_USER+321,nwParam,0)
         RETURN(0)
      ENDIF
   ENDCASE
   IF IsWindow(hWin)
      RETURN(CallWindowProc(nProc,hWin,nMsg,nwParam,nlParam))
   ENDIF
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD EditGetCell(bsetblock,bEndBlock,xstyle,cpicture,ccolor,firstkey) CLASS TWBrowse
   LOCAL hwin
   LOCAL nproc
   LOCAL aRect
   LOCAL crect:=Space(8)
   LOCAL cvar
   LOCAL oget
   IF !(::wantHiliteAll .AND. ::ColCount > 0 )
      IF !::HitBottom .AND. !::HitTop
         SetFocus(::handle)
         IF (aRect:=::GetItemRect())==NIL
            RETURN(NIL)
         ENDIF
         IF (cvar:=Eval(bsetblock,::aData[::nline+1,2,::ColPos]))==NIL
            RETURN(NIL)
         ENDIF
         IF xstyle==NIL
            xstyle:=0
         ENDIF
         hwin:=CreateWindow("edit","",;
                            WS_CHILD+WS_BORDER+WS_VISIBLE+ES_AUTOHSCROLL+ES_MULTILINE+4096+xstyle,;
                            aRect[1]-1,aRect[2]-1,aRect[3]-aRect[1]+1,aRect[4]-aRect[2]+1,;
                            ::handle,)

//         oget:=WinGet(hwin,cvar,cpicture,,,ccolor)

         nproc:=SetProcedure(hwin,{|hwin,nMsg,nwParam,nlParam| ;
                                   ::EditCellProc(nproc,hwin,nMsg,nwParam,nlParam,bEndBlock,oget)},;
                             {WM_KILLFOCUS,WM_KEYUP,WM_KEYDOWN,WM_CHAR,WM_USER+322,WM_USER+321})

         SendMessage(hwin,WM_SETFONT,::GetColFont(),MAKELPARAM(1,0))
         SendMessage(hwin,WM_USER+322,0,0)
         SetFocus(hwin)
         IF firstkey # NIL
            PostMessage(hwin,WM_CHAR,firstkey,0)
         ENDIF
      ELSE
         MessageBeep(-1)
      ENDIF
   ENDIF
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD GetItemRect() CLASS TWBrowse
   LOCAL aRect
   IF ::RowPos> 0 .AND. ::RowPos<=::RowCountUsable .AND. ;
      ::ColPos >= ::LeftVisible .AND. ::ColPos <=::ColCount
      aRect:={::nDataLeft,::HeadHeight+(::RowPos-1)*::ItemHeight,;
              ::nDataWidth,::HeadHeight+::RowPos*::ItemHeight}
      IF !::wantHiliteAll
         aRect[1]+=::ColWidths[::ColPos]-::ColWidths[::LeftVisible]
         aRect[3]:=MIN(aRect[1]+(::ColWidths[::ColPos+1]-::ColWidths[::ColPos]),::nDataWidth)
      ENDIF
   ENDIF
RETURN(aRect)

//---------------------------------------------------------------------------------------------

METHOD GetItemText() CLASS TWBrowse
   LOCAL nPos
   IF ::RowPos> 0 .AND. ::RowPos<=::RowCountUsable .AND. ;
      ::ColPos >= ::LeftVisible .AND. ::ColPos <=::ColCount
      IF Len(::aData) >= ::RowPos .AND. Len(::aData[::RowPos,2]) >= ::ColPos
         IF !::wantHiliteAll
            RETURN(::aData[::RowPos,2,::ColPos])
         ENDIF
      ENDIF
   ENDIF
RETURN(NIL)

//---------------------------------------------------------------------------------------------

METHOD OnSysColorChange() CLASS TWBrowse
   IF ::wantSysColor
      ::FgColor       := GetSysColor( COLOR_WINDOWTEXT )
      ::BgColor       := GetSysColor( COLOR_WINDOW )
      ::HiliteColor   := GetSysColor( COLOR_HIGHLIGHTTEXT )
      ::HiliteBgColor := GetSysColor( COLOR_HIGHLIGHT )
      ::HiliteNoFocus := GetSysColor( COLOR_INACTIVECAPTION )
      ::drawdata()
   ENDIF
RETURN(nil)

//---------------------------------------------------------------------------------------------

METHOD OnVScroll(nwParam, nlParam) CLASS TWBrowse
   LOCAL oldrow
   local nCode,nPos,hSBar
   nCode := loword(nwParam)
   nPos  := hiWord(nwParam)
   hSBar := nlParam
   SetFocus(::handle)
   DO CASE
   CASE nCode==SB_LINEUP
      ::Up(.T.)

   CASE nCode==SB_LINEDOWN
      ::Down(.T.)

   CASE nCode==SB_PAGEDOWN
      ::Pgdown(.T.)

   CASE nCode==SB_PAGEUP
      ::Pgup(.T.)

   CASE nCode==SB_TOP
      ::Home(.T.)

   CASE nCode==SB_BOTTOM
      ::EndKey(.T.)

   CASE nCode==SB_THUMBPOSITION
      Eval(::bGoToPos,self,nPos)
      oldrow:=::RowPos
      ::RowPos:=1
      ::RefreshAll()
      ::GoToPos(oldrow,,.T.)

   CASE nCode==SB_THUMBTRACK

   CASE nCode==SB_ENDSCROLL

   ENDCASE
   ::OnChange()
RETURN(0)

//---------------------------------------------------------------------------------------------

METHOD OnHScroll(nwParam, nlParam) CLASS TWBrowse
   LOCAL nCol
   local nCode,nPos,hSBar
   nCode := loword(nwParam)
   nPos  := hiWord(nwParam)
   hSBar := nlParam

   SetFocus(::handle)
   DO CASE
   CASE nCode==SB_LINEUP
      ::KeyLeft(.T.)

   CASE nCode==SB_PAGEUP
      ::PanLeft(.T.)

   CASE nCode==SB_LINEDOWN
      ::KeyRight(.T.)

   CASE nCode==SB_PAGEDOWN
      ::PanRight(.T.)

   CASE nCode==SB_TOP
      ::DeHilite(.T.)
      ::GoToCol(1)
      ::Hilite(.T.)

   CASE nCode==SB_BOTTOM
      ::DeHilite(.T.)
      ::GoToCol(::ColCount)
      ::Hilite(.T.)

   CASE nCode==SB_THUMBPOSITION
      nCol:=aScan(::ColWidths,{|n| n > nPos })-1
      IF nCol # ::ColPos
         ::DeHilite(.T.)
         ::GoToCol(nCol)
         ::Hilite(.T.)
      ENDIF

   CASE nCode==SB_THUMBTRACK
      nCol:=aScan(::ColWidths,{|n| n > nPos })-1
      IF nCol # ::ColPos
         ::DeHilite(.T.)
         ::GoToCol(nCol)
         ::Hilite(.T.)
      ENDIF
   CASE nCode==SB_ENDSCROLL
   ENDCASE
   ::OnChange()
RETURN(0)

//---------------------------------------------------------------------------------------------

STATIC FUNCTION gotorec(ob,newrec)
   LOCAL hcursor :=LoadCursor(_getinstance(),IDC_WAIT)
   LOCAL holdcursor:=SetCursor(hcursor)
   LOCAL recdiff:=0
   LOCAL absdiff:=0

   IF ob:RecCount > 1
      recdiff:=newrec-ob:RecPos
      absdiff:=abs(recdiff)
      IF absdiff > newrec
         IF ob:RecCount-newrec < newrec
            (ob:Source)->(dbgobottom())
            (ob:Source)->(DBSKIP(newrec-ob:RecCount)) // (cmkeyskip(newrec-ob:RecCount))
           ELSE
            (ob:Source)->(DBGOTOP())
            (ob:Source)->(DBSKIP(newrec-1)) //(cmkeygoto(newrec))
         ENDIF
        ELSE
         IF ob:RecCount-newrec < absdiff
            (ob:Source)->(dbgobottom())
            (ob:Source)->(DBSKIP(newrec-ob:RecCount)) // (cmkeyskip(newrec-ob:RecCount))
           ELSE
            (ob:Source)->(DBSKIP(recdiff)) // (cmkeyskip(recdiff))
         ENDIF
      ENDIF
   ENDIF
   Eval(ob:bSkip,1)
   Eval(ob:bSkip,-1)
   SetCursor(holdcursor)
RETURN(ob)

STATIC FUNCTION EditAsNumber(hWnd,cpicture)
   LOCAL nproc:=SetProcedure(hWnd,;
                             {|hWnd,nMsg,nwParam,nlParam| ;
                              _EditAsNumber(nproc,hWnd,nMsg,nwParam,nlParam,cpicture)},;
                             {WM_CHAR,WM_KILLFOCUS})
   IF ValType(cpicture) == 'C'
      SendMessage(hWnd,EM_LIMITTEXT,Len(cpicture),0)
   ENDIF
RETURN(NIL)

STATIC FUNCTION _EditAsNumber(nproc,hWnd,nMsg,nwParam,nlParam,cpicture)
   LOCAL ntext
   IF nMsg==WM_CHAR
      IF (nwParam > 33 .AND. nwParam < 256 )
         IF !(Chr(nwParam) $ ' -.0123456789')
            MessageBeep(-1)
            RETURN(0)
         ENDIF
      ENDIF
     ELSEIF nMsg==WM_KILLFOCUS
      ntext:=Val(GetWindowText(hWnd))
      IF ValType(cpicture) =='C'
         SetWindowText(hWnd,lTrim(transform(ntext,cpicture)))
      ELSE
         SetWindowText(hWnd,lTrim(Str(ntext)))
      ENDIF
   ENDIF
RETURN(CallWindowProc(nproc,hWnd,nMsg,nwParam,nlParam))

STATIC FUNCTION GetAColumn(a,i)
 RETURN whColumn():INIT(a[i][1],{|oCol,oB,n| asString(oB:source[n,i]) } ,DT_LEFT, a[i][2] )
