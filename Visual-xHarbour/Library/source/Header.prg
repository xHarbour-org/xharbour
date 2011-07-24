/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Header.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include "vxh.ch"

#define HDF_SORTUP 0x400
#define HDF_SORTDOWN 0x200

#define HP_HEADERITEM         1
#define HP_HEADERITEMLEFT     2
#define HP_HEADERITEMRIGHT    3
#define HP_HEADERSORTARROW    4
#define HIS_NORMAL            1
#define HIS_HOT               2
#define HIS_PRESSED           3
#define HILS_NORMAL           1
#define HILS_HOT              2
#define HILS_PRESSED          3
#define HIRS_NORMAL           1
#define HIRS_HOT              2
#define HIRS_PRESSED          3
#define HSAS_SORTEDUP         1
#define HSAS_SORTEDDOWN       2

#pragma BEGINDUMP
   #include <windows.h>
   #include <commdlg.h>
   #include <shlobj.h>

   #include "item.api"
   #include "hbdefs.h"
   #include "hbvmpub.h"
   #include "hbinit.h"
   #include "hbapi.h"
   #include "hbfast.h"
   #include "hbvm.h"
   #include "hbapierr.h"
   #include "hbpcode.h"
   #include "winreg.h"
#pragma ENDDUMP
//-----------------------------------------------------------------------------------------------

CLASS HeaderStrip INHERIT Control
   PROPERTY ImageList  GET __ChkComponent( Self, ::xImageList ) SET SetImageList
   PROPERTY ImageMargin READ xImageMargin WRITE SetImageMargin DEFAULT 2 PROTECTED

   DATA AllowUnDock          EXPORTED INIT FALSE
   DATA AllowClose           EXPORTED INIT FALSE

   DATA OnBeginTrack       EXPORTED
   DATA OnTrack            EXPORTED
   DATA OnBeginDrag        EXPORTED
   DATA OnEndDrag          EXPORTED
   DATA OnGetDispInfo      EXPORTED
   DATA OnItemChanging     EXPORTED
   DATA OnItemChanged      EXPORTED
   DATA OnCustomDraw       EXPORTED
   DATA OnItemClick        EXPORTED
   DATA OnItemDblClick     EXPORTED
   DATA OnFilterChange     EXPORTED
   
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD InsertItem()
   METHOD DeleteItem( nPos ) INLINE ::Children[nPos]:Destroy()
   METHOD SetItemWidth()
   METHOD SetItemCaption()
   METHOD OnParentNotify()
   METHOD SetArrow()
   METHOD DrawFrame()
   METHOD GetItemRect()
   METHOD OnSize()
   METHOD OnHScroll() INLINE 0
   METHOD SetImageList()
   METHOD SetImageMargin()
ENDCLASS

//---------------------------------------------------------------------------------------------------

METHOD Init( oParent, cName ) CLASS HeaderStrip
   ::ClsName   := WC_HEADER
   DEFAULT cName TO "HeaderStrip"
   ::__xCtrlName := cName
   ::Super:Init( oParent )
   ::__lMoveable := .T.
   ::Height := 17
   ::Width  := 0
   ::Style  := WS_CHILD | WS_VISIBLE | HDS_BUTTONS | HDS_DRAGDROP | HDS_FULLDRAG | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD Create() CLASS HeaderStrip
//   ::Left   := 0
//   ::Top    := 0
   IF ::Width  == 0
      ::Width := ::Parent:ClientWidth - ::Left
   ENDIF
   ::Super:Create()
   IF ::ImageList != NIL
      ::SetImageList( ::ImageList )
   ENDIF
   ::SetImageMargin( ::ImageMargin )
   IF ::__ClassInst != NIL // it's been created in the IDE
      ::__IdeContextMenuItems := { { "&Add New HeaderItem", {|o| o := HeaderItem( Self ):Create(),;
                                                             ::Application:Project:Modified := .T.,;
                                                             ::Application:Project:CurrentForm:SelectControl(o) } } }
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------
METHOD OnSize( x, y ) CLASS HeaderStrip
   Super:OnSize( x, y )
   IF LEN( ::Children ) > 0 .AND. ::Children[1]:Height != y
      AEVAL( ::Children, {|o| o:Height := y} )
   ENDIF
RETURN NIL

METHOD SetImageList( oList ) CLASS HeaderStrip
   IF ::hWnd != NIL
      oList := __ChkComponent( Self, oList )
      ::SendMessage( HDM_SETIMAGELIST, 0, IIF( oList != NIL, oList:Handle, NIL ) )
      AEVAL( ::Children, {|o| o:ImageIndex := o:ImageIndex  } )
   ENDIF
RETURN Self

METHOD SetImageMargin( n ) CLASS HeaderStrip
   IF ::hWnd != NIL
      ::SendMessage( HDM_SETBITMAPMARGIN, n, 0 )
   ENDIF
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD GetItemRect( nItem ) CLASS HeaderStrip
   LOCAL aRect, rc := (struct RECT)
   SendMessage( ::hWnd, HDM_GETITEMRECT, nItem, @rc )
RETURN rc

//---------------------------------------------------------------------------------------------------

METHOD InsertItem( nPos, cText, nWidth, nAlign, nImageIndex ) CLASS HeaderStrip
   LOCAL o       := HeaderItem( Self )
   o:Caption     := cText
   o:Width       := nWidth
   o:Alignment   := nAlign
   o:xImageIndex := nImageIndex
   o:Position    := nPos
   o:Create()
RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD OnParentNotify( nwParam, nlParam, hdr ) CLASS HeaderStrip
   LOCAL nRet, hTheme, pt
   LOCAL cd, nmh, hcd, hdti
   LOCAL aMouse, aRect, nColPos, nTrack, n, hdht

   DO CASE
      CASE hdr:code == HDN_BEGINTRACK
           nRet := __Evaluate( ::OnBeginTrack, __GetHeaderItem( nlParam ) )

      CASE hdr:code == HDN_BEGINDRAG
           nRet := __Evaluate( ::OnBeginDrag, __GetHeaderItem( nlParam ) )

      CASE hdr:code == HDN_ENDDRAG
           n := HB_INLINE( nlParam ){
              NMHEADER *hdr = (NMHEADER*) hb_parnl(1);
              hb_retni( hdr->pitem->iOrder );
           }
           nRet := __Evaluate( ::OnEndDrag, n )

      CASE hdr:code == HDN_ITEMCHANGED
           n := HB_INLINE( nlParam ){
              NMHEADER *hdr = (NMHEADER*) hb_parnl(1);
              hb_retnl( hdr->pitem->cxy );
           }
           nRet := __Evaluate( ::OnItemChanged, n )

      CASE hdr:code == HDN_ITEMCLICK
           IF ( n := __GetHeaderItem( nlParam ) ) > 0
              nRet := __Evaluate( ::OnItemClick, n )
              ExecuteEvent( "OnClick", ::Children[ n + 1 ] )
           ENDIF
           
      CASE hdr:code == HDN_GETDISPINFO
      CASE hdr:code == HDN_ITEMDBLCLICK
      CASE hdr:code == HDN_FILTERCHANGE
      CASE hdr:code == HDN_TRACK
      CASE hdr:code == HDN_ITEMCHANGING

      CASE hdr:code == NM_CUSTOMDRAW
           nRet := __Evaluate( ::OnCustomDraw, nlParam )
           
   ENDCASE
   //DEFAULT nRet TO 0

RETURN nRet

METHOD SetArrow( nColumn, nImage ) CLASS HeaderStrip
   LOCAL ii, hdi, hdi2, nCount, nAlignment

   DEFAULT nImage TO 0

   nAlignment := ::Children[nColumn+1]:Alignment-1

   hdi2 := (struct HDITEM)
   hdi := (struct HDITEM)

   IF ::Application:OsVersion:dwMajorVersion > 4
      // use built in sort arrows
      hdi:mask  := HDI_FORMAT
      hdi:fmt   := HDF_STRING | IIF( nImage == 0, HDF_SORTDOWN, HDF_SORTUP ) | nAlignment

    ELSE
      // put our arrow icon next to the text
      hdi:mask   := HDI_FORMAT | HDI_IMAGE
      hdi:fmt    := HDF_STRING | HDF_IMAGE | HDF_BITMAP_ON_RIGHT
      hdi:iImage := nImage
   ENDIF
   ::SendMessage( HDM_SETITEM, nColumn, hdi )

   IF ( nCount := LEN( ::Children )-1 ) > 0
      FOR ii := 0 TO nCount
          IF nColumn != ii
             hdi2 := (struct HDITEM)
             hdi2:mask  := HDI_FORMAT
             hdi2:fmt   := HDF_STRING | ::Children[ii+1]:Alignment-1
             ::SendMessage( HDM_SETITEM, ii, hdi2 )
          ENDIF
      NEXT
   ENDIF

RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD DrawFrame( hDC, aRect, nStatus, lDraw ) CLASS HeaderStrip
   LOCAL hTheme, n, nFlags := DFCS_BUTTONPUSH
   DEFAULT lDraw TO .T.

   IF lDraw
      IF ::OsVer:dwMajorVersion > 4 .AND. ::Application:ThemeActive

         IF nStatus == DFCS_PUSHED
            nStatus := HILS_PRESSED
         ENDIF

         hTheme := OpenThemeData(,"header")
         ::Drawing:DrawThemeBackground( hTheme, HP_HEADERITEM, nStatus, aRect, aRect )
         CloseThemeData( hTheme )
       ELSE
         IF nStatus != NIL
            nFlags := nFlags | nStatus
         ENDIF
         DrawFrameControl( hDC, aRect, DFC_BUTTON, nFlags )
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------

METHOD SetItemWidth( nPos, nWidth ) CLASS HeaderStrip
   LOCAL hi := (struct HDITEM)
   IF ::hWnd != NIL
      hi:mask  := HDI_WIDTH
      hi:cxy   := nWidth
      ::Children[nPos]:xWidth := nWidth
      ::SendMessage( HDM_SETITEM, nPos-1, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetItemCaption( nPos, cCaption ) CLASS HeaderStrip
   LOCAL hi   := (struct HDITEM)
   IF ::hWnd != NIL
      hi:mask    := HDI_TEXT
      hi:pszText := cCaption
      ::SendMessage( HDM_SETITEM, nPos-1, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS HeaderItem INHERIT Object
   DATA AllowResize            PUBLISHED INIT .T.
   DATA AllowDrag              PUBLISHED INIT .T.

   DATA Parent                 EXPORTED
   DATA Position               EXPORTED
   DATA __Alignments           EXPORTED  INIT { "Left", "Right", "Center", "JustifyMask", "RTLReading" }

   PROPERTY Caption            READ xCaption    WRITE SetCaption                 PROTECTED
   PROPERTY ImageIndex         READ xImageIndex WRITE SetImageIndex DEFAULT 0    PROTECTED
   PROPERTY Width              READ xWidth      WRITE SetWidth      DEFAULT 100
   PROPERTY Alignment          READ xAlignment  WRITE SetAlignment  DEFAULT 1    PROTECTED

   DATA Anchor                 EXPORTED
   DATA Dock                   EXPORTED
   DATA Events                 EXPORTED
   DATA __lResizeable          EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.T.,.F.,.F.}
   DATA __lMoveable            EXPORTED INIT .F.
   DATA __lCreateAfterChildren EXPORTED INIT .F.
   DATA __IdeImageIndex        EXPORTED INIT 3
   DATA __PropFilter           EXPORTED INIT {}
   DATA __TempRect             EXPORTED
   DATA IsContainer            EXPORTED INIT .F.

   DATA xLeft                  EXPORTED
   ACCESS Left                 INLINE ::GetSize(1)
   ASSIGN Left(n)              INLINE ::xLeft := n

   DATA xTop                   EXPORTED
   ACCESS Top                  INLINE ::GetSize(2)
   ASSIGN Top(n)               INLINE ::xTop := n

   DATA xHeight                EXPORTED
   ACCESS Height               INLINE ::GetSize(4)
   ASSIGN Height(n)            INLINE ::xHeight := n, ::SetHeight()

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Destroy()
   METHOD SetCaption()
   METHOD SetImageIndex()
   METHOD SetWidth()
   METHOD SetHeight()
   METHOD SetAlignment()
   METHOD __OnParentSize()    INLINE Self
   METHOD OnParentMove()      INLINE NIL
   METHOD GetRect()
   METHOD GetSize()
   METHOD MoveWindow()        INLINE Self
   METHOD UpdateWindow()      INLINE Self
   METHOD IsWindowVisible()   INLINE .T.
   METHOD GetRectangle()      INLINE { ::Left, ::Top, ::Left + ::Width, ::Top + ::Height }
   METHOD GetChildFromPoint() INLINE Self
   METHOD SetWindowPos(h, x, y, cx, cy ) INLINE ::Width := cx
   METHOD Refresh()           INLINE ::Parent:InvalidateRect()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS HeaderItem
   LOCAL n, aProp, nPos
   IF oParent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
   ENDIF
   ::__lCopyCut   := .F.
   ::EventHandler := Hash()
   HSetCaseMatch( ::EventHandler, .F. )
   ::ClsName      := "HeaderItem"
   ::Parent       := oParent
   ::Position     := LEN( ::Parent:Children )
   ::Events       := { ;
                        {"Mouse",     {;
                                      { "OnClick"     , "", "" } } } }
   ::__CreateProperty( "HeaderItem" )
   ::Caption := ::Name
RETURN Self


//-----------------------------------------------------------------------------------------------
METHOD Create() CLASS HeaderItem
   LOCAL hi := (struct HDITEM)
   hi:mask  := HDI_WIDTH | HDI_FORMAT | HDI_ORDER
   hi:fmt   := 0

   IF !EMPTY( ::Caption )
      hi:mask      := hi:mask | HDI_TEXT
      hi:fmt       := hi:fmt | HDF_STRING | ::Alignment-1
      hi:pszText   := ::Caption
      hi:cchTextMax:= LEN( ::Caption ) + 1
   ENDIF

   DEFAULT ::xImageIndex TO 0
   IF ::Parent:ImageList != NIL .AND. ::xImageIndex > 0
      hi:mask   := hi:mask | HDI_IMAGE
      hi:fmt    := hi:fmt | HDF_IMAGE
      hi:iImage := ::ImageIndex-1
   ENDIF

   hi:cxy        := ::Width
   hi:iorder     := ::Position

   ::hWnd := HB_RandomInt( 500000, 999999 )

   AINS( ::Parent:Children, ::Position+1, Self, .T. )
   ::Parent:SendMessage( HDM_INSERTITEM, ::Position, hi )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD Destroy() CLASS HeaderItem
   LOCAL n := 0
   ::Parent:SendMessage(  HDM_DELETEITEM, ::Position, 0 )
   ADEL( ::Parent:Children, ::Position, .T. )
   AEVAL( ::Parent:Children, {|o|o:Position := n++} )
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetCaption() CLASS HeaderItem
   LOCAL hi
   IF LEN( ::Parent:Children ) >= ::Position + 1
      hi := (struct HDITEM)
      hi:mask       := HDI_FORMAT | HDI_TEXT
      hi:fmt        := HDF_STRING | ::Alignment-1
      hi:pszText    := ::Caption
      hi:cchTextMax := LEN( ::Caption ) + 1
      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetImageIndex() CLASS HeaderItem
   LOCAL hi
   IF LEN( ::Parent:Children ) >= ::Position + 1
      hi := (struct HDITEM)
      hi:mask       := HDI_FORMAT | HDI_TEXT
      hi:fmt        := HDF_STRING | ::Alignment-1
      hi:pszText    := ::Caption
      hi:cchTextMax := LEN( ::Caption ) + 1
      IF ::xImageIndex > 0 .AND. ::Parent:ImageList != NIL
         hi:mask   := hi:mask | HDI_IMAGE
         hi:fmt    := hi:fmt | HDF_IMAGE
         hi:iImage := IIF( ::Parent:ImageList != NIL, ::xImageIndex-1, -2 )
      ENDIF
      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetWidth() CLASS HeaderItem
   LOCAL hi
   IF LEN( ::Parent:Children ) >= ::Position + 1
      hi := (struct HDITEM)
      hi:mask := HDI_WIDTH
      hi:cxy  := ::xWidth
      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetHeight() CLASS HeaderItem
   LOCAL hi
   IF LEN( ::Parent:Children ) >= ::Position + 1
      hi := (struct HDITEM)
      hi:mask := HDI_HEIGHT
      hi:cxy  := ::xHeight
      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
      hi:mask := HDI_WIDTH
      hi:cxy  := ::xWidth
      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD SetAlignment() CLASS HeaderItem
   LOCAL hi
   IF LEN( ::Parent:Children ) >= ::Position + 1
      hi := (struct HDITEM)
      hi:mask       := HDI_FORMAT | HDI_TEXT
      hi:fmt        := HDF_STRING | ::Alignment-1
      hi:pszText    := ::Caption
      hi:cchTextMax := LEN( ::Caption ) + 1

      DEFAULT ::xImageIndex TO 0
      IF ::Parent:ImageList != NIL .AND. ::xImageIndex > 0
         hi:mask   := hi:mask | HDI_IMAGE
         hi:fmt    := hi:fmt | HDF_IMAGE
         hi:iImage := ::xImageIndex-1
      ENDIF

      ::Parent:SendMessage( HDM_SETITEM, ::Position, hi )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD GetRect() CLASS HeaderItem
   LOCAL n, pt, rc := (struct RECT)
   SendMessage( ::Parent:hWnd, HDM_GETITEMRECT, ::Position, @rc )
   n    := rc:Top
   pt   := (struct POINT)
   pt:x := rc:left
   pt:y := rc:top

   ClientToScreen( ::Parent:hWnd, @pt )
   rc:left := pt:x - ::Parent:HorzScrollPos
   rc:top  := pt:y
   pt:x := rc:right
   pt:y := rc:bottom

   ClientToScreen( ::Parent:hWnd, @pt )
   rc:right  := pt:x - ::Parent:HorzScrollPos
   rc:bottom := pt:y
RETURN rc

//-----------------------------------------------------------------------------------------------
METHOD GetSize( nPos ) CLASS HeaderItem
   LOCAL rc := (struct RECT)
   SendMessage( ::Parent:hWnd, HDM_GETITEMRECT, ::Position, @rc )
   SWITCH nPos
      CASE 1
         RETURN rc:Left
      CASE 2
         RETURN rc:top
      CASE 3
         RETURN rc:Right - rc:Left
      CASE 4
         RETURN rc:bottom - rc:top
   END
RETURN 0
