/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Rebar.prg                                                                                            *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "vxh.ch"
#include "debug.ch"

#define ATL_IDM_FIRST_MDICHILD 50000
#define IDM_MDI_BASE      (ATL_IDM_FIRST_MDICHILD - 5)
#define IDM_MDI_ICON      (IDM_MDI_BASE + 0)
#define IDM_MDI_GAP       (IDM_MDI_BASE + 1)
#define IDM_MDI_MINIMIZE  (IDM_MDI_BASE + 2)
#define IDM_MDI_RESTORE   (IDM_MDI_BASE + 3)
#define IDM_MDI_CLOSE     (IDM_MDI_BASE + 4)
#define IDM_MDI_FIRST     IDM_MDI_ICON
#define IDM_MDI_LAST      IDM_MDI_CLOSE

//------------------------------------------------------------------------------*

CLASS CoolBar FROM Control

   DATA Repaint              EXPORTED  INIT .T.
   DATA Bands                EXPORTED  INIT {}
   DATA lDestroyed           PROTECTED INIT .F.
//   ACCESS Height             INLINE    ::GetHeight()
   DATA hWndBand             PROTECTED
   DATA rbi                  PROTECTED
   DATA nmr                  PROTECTED
   
   DATA xImageList           PROTECTED
   ACCESS ImageList          INLINE    __ChkComponent( Self, ::xImageList )
   ASSIGN ImageList( oImg )  INLINE    ::SetImageList( oImg )

   DATA xBackColor           EXPORTED INIT GetSysColor( COLOR_BTNFACE )
   ACCESS BackColor          INLINE    ::xBackColor PERSISTENT
   ASSIGN BackColor( n )     INLINE    ::SetBkColor(n)

   PROPERTY Vertical        INDEX CCS_VERT            READ xVertical        WRITE SetStyle DEFAULT .F.
//   PROPERTY VerticalGripper INDEX RBS_VERTICALGRIPPER READ xVerticalGripper WRITE SetStyle DEFAULT .F.

//   DATA Dock                 EXPORTED
   DATA AllowUnDock          EXPORTED INIT FALSE
   DATA AllowClose           EXPORTED INIT FALSE
   METHOD Init() CONSTRUCTOR
   METHOD GetBandInfo()
   METHOD GetBandRect()
   METHOD SetImageList()
   METHOD GetBkColor()       INLINE    IIF( ::IsWindow(), ::SendMessage( RB_GETBKCOLOR, 0, 0 ), GetSysColor( COLOR_BTNFACE ) )
   METHOD SetTextColor(n)    INLINE    SendMessage( ::hWnd, RB_SETTEXTCOLOR, 0, n )
   METHOD SetBkColor(n)      INLINE    ::xBackColor := n, SendMessage( ::hWnd, RB_SETBKCOLOR, 0, n ), AEVAL( ::Bands, {|o|o:BackColor := o:Parent:xBackColor } )
   METHOD GetHeight()        INLINE    SendMessage( ::hWnd, RB_GETBARHEIGHT, 0, 0 )//+4
   METHOD __OnParentSize( x, y, hDef )   INLINE ::Super:__OnParentSize( x, y, @hDef ), ::InvalidateRect(,.F.)
   METHOD OnParentNotify()
   METHOD Create()
   METHOD OnMouseMove()
   METHOD BandFromhWnd()
   METHOD OnNCDestroy()
   METHOD UpdateSize()
   METHOD __SetTheming()
   METHOD FindRemoveBand()
   //METHOD OnParentSize()
ENDCLASS

//----------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS CoolBar

   LOCAL aRect, aCnt, hClass, o
   LOCAL rbi := (struct REBARINFO)

   InitCommonControlsEx(ICC_COOL_CLASSES)
   ::ClsName           := REBARCLASSNAME
   DEFAULT ::__xCtrlName TO "CoolBar"
   ::Style             := WS_CHILD | WS_VISIBLE | WS_BORDER | RBS_BANDBORDERS | RBS_VARHEIGHT | CCS_NODIVIDER | CCS_NOPARENTALIGN | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::Super:Init( oParent )
   ::Caption           := ""
   ::Width             := oParent:ClientWidth
   ::Height            := 30
   ::xBorder           := .T.
   ::DeferRedraw       := .F.
   ::ExStyle           := WS_EX_TOOLWINDOW
   ::ThemeName         := "rebar"
   ::IsContainer       := .F.
   ::Dock:Margin       := 0
   ::Dock:Left         := oParent
   ::Dock:Top          := oParent
   ::Dock:Right        := oParent
RETURN SELF

//----------------------------------------------------------------------------------------------------

METHOD Create() CLASS CoolBar
   ::Super:Create()
   //SetClassLong( ::hWnd, GCL_STYLE, GetClassLong( ::hWnd, GCL_STYLE ) & NOT( CS_VREDRAW + CS_HREDRAW ) )
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "&Add CoolBar Band", {|o| o := CoolBarBand( Self ):Create(), o:Break := .T.,;
                                                             ::Application:Project:Modified := .T.,;
                                                             ::Application:Project:CurrentForm:SelectControl(o) } },;
                                   { "&Remove Band", {|o| Self:FindRemoveBand( o:Cargo ), ::Application:Project:Modified := .T. } } }
   ENDIF
   ::UpdateSize()
   ::MoveWindow()
RETURN Self

//----------------------------------------------------------------------------------------------------

METHOD FindRemoveBand( aPt ) CLASS CoolBar
   LOCAL pt, rbh
   pt := (struct POINT)
   pt:x := aPt[1]
   pt:y := aPt[2]
   rbh := (struct RBHITTESTINFO)
   rbh:pt := pt
   SendMessage( ::hWnd, RB_HITTEST, 0, @rbh )
   IF rbh:iBand >= 0
      ::Bands[ rbh:iBand+1 ]:Destroy()
      ::Application:Project:Modified := .T.
//      ::Application:ComboSelect:Reset()
//      ::Application:ComboSelect:FillData( ::Application:Project:CurrentForm )
   ENDIF
RETURN Self


METHOD OnNCDestroy() CLASS CoolBar
   ::Super:OnNCDestroy()
   AEVAL( ::Bands, {|o|IIF( o:Image != NIL, o:Image:Delete(), NIL )} )
   ::Parent:TopMargin := 0
RETURN SELF


//----------------------------------------------------------------------------------------------------

METHOD OnMouseMove( n, x, y ) CLASS CoolBar

   LOCAL aMouse, rc, oTool, nBand, pt, oBar, oBand
   IF ::hWnd != GetCapture() .OR. ::hwndBand == NIL
      RETURN NIL
   ENDIF

   GetWindowRect( ::hWnd, @rc )
   GetCursorPos( @pt )

   IF pt:y > rc:bottom + 10 //.OR. aMouse[2] < aRect[2] .OR. aMouse[1] > aRect[3] .OR. aMouse[1] < aRect[1]

      nBand := ASCAN( ::Bands, {|o|o:oStruct:hwndChild == ::hwndBand } )
      ReleaseCapture( ::hWnd )

      IF nBand > 0

         IF !::Bands[ nBand ]:AllowUndock
            RETURN NIL
         ENDIF

         oTool := FloatingToolBar( ::Parent, ::Bands[ nBand ] )
         oTool:Left   := pt:x-10
         oTool:Top    := pt:y-2
         oTool:Width  := ::Bands[ nBand ]:BandChild:Width
         oTool:Height := ::Bands[ nBand ]:MinHeight + GetSystemMetrics( SM_CYCAPTION )
         oTool:Create()
         oTool:Show()

         ::SendMessage( RB_SHOWBAND, IIF( pt:y < rc:top, 0, ::SendMessage( RB_GETBANDCOUNT )-1 ), .F.)
         ::SendMessage( RB_DELETEBAND, IIF( pt:y < rc:top, 0, ::SendMessage( RB_GETBANDCOUNT )-1 ) )

         oTool:Band:BandChild:Show( SW_SHOWNOACTIVATE )
         oTool:Band:BandChild:UpdateWindow()

         ADEL( ::Bands, nBand, .T. )

         ::MapWindowPoints(, @pt, 1 )
         WinSetCursor( LoadCursor(,IDC_ARROW))

         IF ::SendMessage( RB_GETBANDCOUNT ) == 0
            ::Destroy()
         ENDIF
         oTool:SendMessage( WM_NCLBUTTONDOWN, HTCAPTION, MAKELPARAM( pt:x, pt:y ) )

         ::hwndBand := NIL
      ENDIF
   ENDIF

RETURN NIL

METHOD __SetTheming( lSet ) CLASS CoolBar
   LOCAL Band
   ::Super:__SetTheming( lSet )
   FOR EACH Band IN ::Bands
       IF Band:BandChild != NIL
          Band:BandChild:Theming := lSet
       ENDIF
   NEXT
   ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
RETURN Self

//----------------------------------------------------------------------------------------------------

METHOD SetImageList( oImage ) CLASS CoolBar

   LOCAL rbi

   IF oImage != NIL
      ::xImageList := oImage
   ENDIF
   IF ::hWnd != NIL
      rbi := (struct REBARINFO)
      rbi:cbSize := rbi:sizeof()
      rbi:fMask  := RBIM_IMAGELIST
      rbi:hIml   := oImage:handle

      SendMessage( ::hWnd, RB_SETBARINFO, 0, rbi )
   ENDIF

RETURN SELF


//----------------------------------------------------------------------------------------------------

METHOD BandFromhWnd( hWnd ) CLASS CoolBar

   LOCAL n, iMax
   LOCAL rbi  := (struct REBARBANDINFO)

   rbi:cbSize := rbi:sizeof()
   rbi:fMask  := RBBIM_CHILD
   iMax       := SendMessage( ::hWnd, RB_GETBANDCOUNT )

   FOR n := 0 TO iMax
      SendMessage( ::hWnd, RB_GETBANDINFO, n, @rbi )

      IF rbi:hwndChild == hWnd
         RETURN n
      ENDIF
   NEXT

RETURN iMax


//----------------------------------------------------------------------------------------------------
/*
METHOD OnParentSize( x, y, hDef ) CLASS CoolBar

   IF ::Vertical
      ::Top    := 0//::Parent:TopMargin
      ::Height := ::Parent:ClientHeight //- ::Parent:TopMargin
      ::Parent:LeftMargin := ::Height
     ELSE
      ::Top   := 0
      ::Width := ::Parent:ClientWidth
      ::Height:= ::GetHeight()
      ::Parent:TopMargin := ::Height-IIF( ::Application:IsThemedXP, 4, 0)
   ENDIF
   SetWindowPos( ::hWnd, , ::Left, ::Top, ::Width, ::Height, SWP_NOACTIVATE+SWP_NOOWNERZORDER+SWP_NOZORDER+SWP_DEFERERASE )
RETURN Self
*/
//----------------------------------------------------------------------------------------------------

METHOD OnParentNotify( nwParam, nlParam ) CLASS CoolBar

   LOCAL nBand, chev, aRect, hDef, oChild, nmr, rbi, aMouse, nW, nH
   LOCAL cBuffer, n, rc

   DO CASE
      CASE ::Parent:hdr:code==RBN_BEGINDRAG
           ::nmr := (struct NMREBAR*) nlParam

           ::rbi := (struct REBARBANDINFO)
           ::rbi:cbSize := ::rbi:sizeof()
           ::rbi:fMask  := RBBIM_CHILD

           SendMessage( ::hWnd, RB_GETBANDINFO, ::nmr:uBand, @::rbi )

           ::hWndBand := ::rbi:hwndChild
           RETURN 0

      CASE ::Parent:hdr:code==RBN_ENDDRAG
           ::hWndBand := NIL
           RETURN 1

      CASE ::Parent:hdr:code==RBN_HEIGHTCHANGE
           ::UpdateSize()
           RETURN 0

      CASE ::Parent:hdr:code==RBN_CHEVRONPUSHED
           chev := (struct NMREBARCHEVRON*) nlParam
           //chev:Buffer( Peek( nlParam, chev:Sizeof ) )

           rbi := (struct REBARBANDINFO)
           rbi:cbSize := rbi:SizeOf()
           rbi:fMask  := RBBIM_CHILD

           SendMessage( ::hWnd, RB_GETBANDINFO, chev:uBand, @rbi )

           nBand := ASCAN( ::Bands, {|o|o:oStruct:hwndChild == rbi:hwndChild } )

           // Notify the children ( REBARBAND ) responsible of what to do when the chevron is pushed
           RETURN ::Bands[ nBand ]:OnChevronPushed( chev )

   ENDCASE
RETURN NIL

METHOD UpdateSize() CLASS CoolBar
   LOCAL oChild, rc, hDef, nW, nH, oTop
   IF ::Repaint
      ::xHeight:= ::GetHeight()

      hDef := BeginDeferWindowPos( LEN( ::Parent:Children )-1 )
      FOR EACH oChild IN ::Parent:Children
         IF oChild:hWnd != ::hWnd
            oChild:__OnParentSize( ::Parent:ClientWidth, ::Parent:ClientHeight, @hDef )
            oChild:InvalidateRect()
         ENDIF
      NEXT
      EndDeferWindowPos( hDef )
      
      IF ::Parent:ClsName == "Vxh_Form" .AND. ::Parent:MDIClient != NIL
         PostMessage( ::Parent:hWnd, WM_SIZE, 0, MAKELPARAM( ::Parent:ClientWidth, ::Parent:ClientHeight ) )
         RETURN 0
      END
   ENDIF
RETURN NIL


//----------------------------------------------------------------------------------------------------

METHOD GetBandInfo( nBand, nMask ) CLASS CoolBar

   LOCAL rbb := (struct REBARBANDINFO)

   DEFAULT nMask TO RBBIM_SIZE + RBBIM_CHILDSIZE

   rbb:cbSize := rbb:SizeOf()
   rbb:fMask  := nMask

   SendMessage( ::hWnd, RB_GETBANDINFO, nBand, @rbb )

RETURN rbb


//----------------------------------------------------------------------------------------------------

METHOD GetBandRect( nBand ) CLASS CoolBar

   LOCAL rc := (struct RECT)

   SendMessage( ::hWnd, RB_GETRECT, nBand, @rc )

RETURN rc


//----------------------------------------------------------------------------------------------------
//-------------------------------------  REBAR BANDS CLASS  ------------------------------------------
//----------------------------------------------------------------------------------------------------

CLASS CoolBarBand INHERIT Control

   DATA oStruct        EXPORTED
   DATA Index          EXPORTED
   DATA Parent         EXPORTED
   DATA Image          EXPORTED
   DATA AllowUndock    EXPORTED INIT .F.
   DATA Backup         PROTECTED
   DATA ToolTip        PROTECTED
   DATA Dock           EXPORTED
   DATA Anchor         EXPORTED
   
   PROPERTY BandChild  GET __ChkComponent( Self, ::xBandChild ) SET SetChild
   
   PROPERTY Caption                          READ xCaption   WRITE SetCaption
   PROPERTY MinWidth                         READ xMinWidth  WRITE SetMinWidth    DEFAULT 200
   PROPERTY MinHeight                        READ xMinHeight WRITE SetMinHeight   DEFAULT 22
   PROPERTY Width                            READ xWidth     WRITE SetWidth       DEFAULT 100
   PROPERTY Chevron                          READ xChevron   WRITE SetChevron     DEFAULT .F.
   PROPERTY Grippers                         READ xGrippers  WRITE SetGrippers    DEFAULT .T.
   PROPERTY FixedSize  INDEX RBBS_FIXEDSIZE  READ xFixedSize WRITE SetStyle       DEFAULT .F.
   PROPERTY Break      INDEX RBBS_BREAK      READ xBreak     WRITE SetStyle       DEFAULT .F.
   PROPERTY BackColor                        READ xBackColor WRITE ReflectColor   

   ACCESS Height INLINE ::xMinHeight   
   DATA Left           EXPORTED INIT 0
   DATA Top            EXPORTED INIT 0
   
   DATA __IsInstance     EXPORTED INIT .F.
   DATA lCreated       PROTECTED INIT .F.
   
   DATA __lResizeable          EXPORTED INIT {.F.,.F.,.F.,.F.,.F.,.t.,.F.,.F.}
   DATA __lMoveable            EXPORTED INIT .F.
   DATA __lCopyCut             EXPORTED INIT .F.

   METHOD Init()       CONSTRUCTOR
   METHOD SetChild()
   METHOD SetImage()
   METHOD SetWidth()
   METHOD SetMinWidth()
   METHOD SetMinHeight()
   METHOD SetHeight()
   METHOD SetStyle()
   METHOD SetCaption()
   METHOD SetGrippers()
   METHOD Show()           INLINE ::Parent:SendMessage( RB_SHOWBAND, ::Index,.T. )
   METHOD Hide()           INLINE ::Parent:SendMessage( RB_SHOWBAND, ::Index,.F. )
   METHOD SetChevron()
   METHOD GetBandInfo()
   METHOD OnChevronPushed()
   METHOD RemoveChild()
   METHOD SaveChild()
   METHOD RestoreChild()
   METHOD Delete()         INLINE ::Parent:SendMessage( RB_DELETEBAND, ::Index )
   METHOD Destroy()
   METHOD ReflectColor()
   METHOD OnParentMove()   INLINE NIL
   METHOD __OnParentSize()   INLINE NIL
   METHOD Create()
   METHOD InvalidateRect() INLINE Self
   METHOD GetRectangle()
   METHOD GetRect()
   METHOD IsWindowVisible() INLINE .T.
ENDCLASS

//----------------------------------------------------------------------------------------------------

METHOD SetStyle( nStyle, Value ) CLASS CoolBarBand
   DEFAULT Value TO .T.
   IF Value
      ::oStruct:fStyle := ::oStruct:fStyle | nStyle
   ELSE
      ::oStruct:fStyle := ::oStruct:fStyle & NOT( nStyle )
      ::oStruct:fStyle := ::oStruct:fStyle | nStyle
   ENDIF
   ::oStruct:fMask := RBBIM_STYLE
   IF ::lCreated
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN Self

METHOD Init( oParent, nPos ) CLASS CoolBarBand
   ::Parent             := oParent
   ::oStruct            := (struct REBARBANDINFO)
   ::Index              := LEN( oParent:Bands )
   ::__lAllowCopy       := .F.
   ::__lCopyCut         := .F.
   ::oStruct:cbSize     := ::oStruct:sizeof()
   ::oStruct:hwndChild  := 0
   ::oStruct:wID        := LEN( ::Parent:Bands )
   ::oStruct:fMask      := RBBIM_STYLE | RBBIM_ID
   ::oStruct:fStyle     := RBBS_NOVERT | RBBS_GRIPPERALWAYS
   ::oStruct:wID        := LEN( oParent:Bands )
   
   ::ClsName            := "CoolBarBand"

   ::Font := Font()
   ::Font:Parent := Self
   IF ::__ClassInst != NIL 
      ::Font:__ClassInst := __ClsInst( ::Font:ClassH )
      ::Font:__ClassInst:__IsInstance := .T.
   ENDIF
   
   ::Font:Create()

   ::__CreateProperty( "Band" )
   AADD( ::Parent:Bands, Self )
   IF /*::Application:IdeActive .AND. */::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance := .T.
   ENDIF
RETURN Self

METHOD Create() CLASS CoolBarBand
   SendMessage( ::Parent:hWnd, RB_INSERTBAND, -1, ::oStruct )
   AADD( ::Parent:Children, Self )
   ::lCreated := .T.
   ::SetMinWidth( ::xMinWidth )
   ::SetMinHeight( ::xMinHeight )
   IF ::BandChild != NIL
      ::SetChild( ::BandChild, .T. )
   ENDIF
   IF ::BackColor != NIL
      ::ReflectColor()
   ENDIF
   IF ::xChevron
      ::SetChevron()
   ENDIF
   SendMessage( ::Parent:hWnd, RB_MAXIMIZEBAND, ::Index, 0 )
RETURN SELF

//-----------------------------------------------------------------------------------------------

METHOD ReflectColor()
   IF ::oStruct:hbmBack == NIL
      ::oStruct:fMask      := ::oStruct:fMask | RBBIM_COLORS
      ::oStruct:clrBack    := IIF( ::BackColor != NIL, ::BackColor, ::Parent:BackColor )
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------

METHOD SetImage( cImage, hInst, nType, nLoad ) CLASS CoolBarBand

   DEFAULT nType TO IMAGE_BITMAP
   DEFAULT hInst TO ::Parent:Instance

   ::Image := Bitmap( cImage,,, .T. )

   ::oStruct:fMask   := RBBIM_BACKGROUND
   ::oStruct:hbmBack := ::Image:Handle //LoadImage( hInst, cImage, nType,,, nLoad )

   SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )

RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetGrippers( Value ) CLASS CoolBarBand

   IF Value
      ::oStruct:fStyle := ::oStruct:fStyle | RBBS_GRIPPERALWAYS
   ELSE
      ::oStruct:fStyle := ::oStruct:fStyle & NOT( RBBS_GRIPPERALWAYS )
      ::oStruct:fStyle := ::oStruct:fStyle | RBBS_NOGRIPPER
   ENDIF

   ::oStruct:fMask := RBBIM_STYLE
   IF ::hWnd != NIL
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetChild( oChild, lInit ) CLASS CoolBarBand
   LOCAL aExt, oOwner
   DEFAULT lInit TO .F.
   
   IF oChild != NIL
      oChild := __ChkComponent( Self, oChild )
      
      IF !lInit .AND. oChild:Owner != NIL
         oOwner := oChild:Owner
         oOwner:BandChild := NIL
         oOwner:Owner := NIL
      ENDIF
      oChild:Owner := Self
   ENDIF
   IF ::lCreated
      ::oStruct:fMask      := RBBIM_CHILD
      ::oStruct:hwndChild  := IIF( Valtype( oChild )=="N" .OR. oChild == NIL, oChild, oChild:hWnd )
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )

      IF oChild != NIL .AND. ( oChild:__xCtrlName == "CoolMenu" .OR. ASCAN( oChild:Children, {|o|o:ClsName == "ComboBox"} ) > 0 )
         aExt := ::Parent:Drawing:GetTextExtentPoint32( "A" )
         IF aExt[2]+6 > ::MinHeight 
            ::MinHeight := aExt[2]+6
         ENDIF
         IF oChild:Height > ::MinHeight
            ::MinHeight := oChild:Height
         ENDIF
      ENDIF
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD RemoveChild() CLASS CoolBarBand

   ::oStruct:fMask      := RBBIM_CHILD
   ::oStruct:hwndChild  := NIL

   SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )

RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetMinWidth( nMinWidth ) CLASS CoolBarBand

   ::xMinWidth          := nMinWidth

   IF ::lCreated
      ::oStruct:fMask      := RBBIM_CHILDSIZE
      ::oStruct:cxMinChild := nMinWidth
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF

RETURN Self


//--------------------------------------------------------------------------------------------------------

METHOD SetMinHeight( nMinHeight ) CLASS CoolBarBand
   IF ::lCreated //!::__IsInstance
      ::oStruct:fMask      := RBBIM_CHILDSIZE + RBBIM_SIZE
      ::oStruct:cyChild    := nMinHeight
      ::oStruct:cyMinChild := nMinHeight

      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetWidth( nWidth ) CLASS CoolBarBand
   IF ::lCreated
      ::oStruct:fMask := RBBIM_SIZE
      ::oStruct:cx    := nWidth

      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetHeight( nHeight ) CLASS CoolBarBand

   ::oStruct:fMask   := RBBIM_SIZE
   ::oStruct:cyChild := nHeight

   SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )

RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetCaption( cText ) CLASS CoolBarBand

   ::xCaption       := cText

   IF !::__IsInstance
      ::oStruct:fMask  := RBBIM_TEXT
      ::oStruct:lpText := cText

      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SetChevron( lSet ) CLASS CoolBarBand
   LOCAL n
   DEFAULT lSet TO ::xChevron
   IF lSet
      ::oStruct:fMask := RBBIM_IDEALSIZE + RBBIM_STYLE
      IF ::BandChild != NIL .AND. ::BandChild:ClsName == "ToolBarWindow32"
         ::oStruct:fStyle  := ::oStruct:fStyle | RBBS_USECHEVRON
         ::oStruct:cxIdeal := ::BandChild:GetButtonsWidth()
      ENDIF
    ELSE
      ::oStruct:fStyle  := ::oStruct:fStyle & NOT( RBBS_USECHEVRON )
      ::oStruct:cxIdeal := 0
   ENDIF
   IF ::lCreated
      SendMessage( ::Parent:hWnd, RB_SETBANDINFO, ::Index, ::oStruct )
   ENDIF
RETURN ::oStruct:cxIdeal


//--------------------------------------------------------------------------------------------------------
METHOD GetRect() CLASS CoolBarBand
   LOCAL rbb, rc, pt
   rc := ::Parent:GetBandRect( ::Index )
   pt := (struct POINT)
   pt:x := rc:left
   pt:y := rc:top 
   ClientToScreen( ::Parent:hWnd, @pt )
   rc:left := pt:x
   rc:top  := pt:y
   pt:x := rc:right
   pt:y := rc:bottom
   ClientToScreen( ::Parent:hWnd, @pt )
   rc:right  := pt:x
   rc:bottom := pt:y
RETURN rc

//--------------------------------------------------------------------------------------------------------
METHOD GetRectangle() CLASS CoolBarBand
   LOCAL rbb, rc, pt
   rc := ::Parent:GetBandRect( ::Index )
RETURN rc:Array

//--------------------------------------------------------------------------------------------------------
METHOD GetBandInfo() CLASS CoolBarBand
   LOCAL rbb := (struct REBARBANDINFO)
   rbb:cbSize     := rbb:sizeof()
   rbb:fMask      := RBBIM_STYLE | RBBIM_ID | RBBIM_STYLE | RBBIM_CHILD | RBBIM_SIZE | RBBIM_CHILDSIZE | RBBIM_IDEALSIZE | RBBIM_IMAGE | RBBIM_LPARAM | RBBIM_HEADERSIZE
   SendMessage( ::Parent:hWnd, RB_GETBANDINFO, ::Index, @rbb )
RETURN rbb

//--------------------------------------------------------------------------------------------------------

METHOD OnChevronPushed( chev ) CLASS CoolBarBand

   LOCAL pt, oBtn, rcBtn, oItem, oSub, n, rc, nSize

   // Will create the vertical toolbar ( custom menu )
   IF ::BandChild:ClsName == "ToolBarWindow32"
      pt := (struct POINT)
      rc := (struct RECT)

      pt:x := chev:rc:Left
      pt:y := chev:rc:Bottom

      ::BandChild:ChevronRect := chev:rc
      ::BandChild:MenuWindow  := 1

      ClientToScreen( ::BandChild:Form:hWnd, @pt )

      GetWindowRect( ::Parent:hWnd, @rc )
      pt:x := rc:left + chev:rc:Left
      pt:y := rc:top + chev:rc:Bottom

      // Create the vertical bar
      ::BandChild:Chevron              := MenuPopup( Self )
      ::BandChild:Chevron:Style        := TPM_LEFTALIGN+TPM_TOPALIGN
      ::BandChild:Chevron:Left         := pt:x + 2
      ::BandChild:Chevron:Top          := pt:y + 2
      ::BandChild:Chevron:ImageList    := ::BandChild:ImageList
      ::BandChild:Chevron:HotImageList := ::BandChild:HotImageList
      ::BandChild:Chevron:CoolBar      := ::BandChild:__MenuBar
      ::BandChild:Chevron:Create()
      ::BandChild:lToolMenu := .F.

      IF ::BandChild:Chevron:ImageList == NIL
         nSize := ::BandChild:GetButtonSize()
         ::BandChild:Chevron:ImageList := ImageList( ::BandChild )
         ::BandChild:Chevron:ImageList:Destroy()
         ::BandChild:Chevron:ImageList:Handle := ::BandChild:GetImageList()
      ENDIF

      n := 1
      FOR EACH oBtn IN ::BandChild:aItems

          rcBtn := oBtn:GetRect()

          pt:x := ::BandChild:GetButtonsWidth( n )
          pt:y := rcBtn:Bottom
          n++

          ClientToScreen( oBtn:Parent:hWnd, @pt )

          IF oBtn:id <= IDM_MDI_ICON  .AND. pt:x + 2 >= ::BandChild:Chevron:Left
             IF oBtn:Style & BTNS_SEP == 0
                oItem := CMenuItem( ::BandChild:Chevron )
                oItem:Id      := oBtn:Id
                DEFAULT oBtn:Caption TO ""
                oItem:Caption := oBtn:Caption
                IF oBtn:Menu != NIL
                   oItem:ImageBkColor := oBtn:Menu:ImageBkColor
                ENDIF
                oItem:Style := 0
                IF !::BandChild:__MenuBar
                   oItem:ImageIndex := oBtn:ImageIndex
                   oItem:Style      := oBtn:Style
                   oItem:Action     := oBtn:Action
                   oItem:Message    := oBtn:Message
                 ELSE
                   oItem:ImageList := oBtn:Menu:ImageList
                   oItem:aItems := ACLONE( oBtn:Menu:aItems )
                   FOR EACH oSub IN oItem:aItems
                       oSub:Menu := oItem
                   NEXT
                ENDIF

                IF oItem:Style != NIL
                   ::BandChild:lToolMenu := .T.
                ENDIF
                oItem:Enabled := oBtn:Enabled
                oItem:Create()
             ENDIF

          ENDIF

      NEXT
      ::BandChild:hWndHook := ::BandChild:hWnd
      ::Application:__CurCoolMenu := ::BandChild
      ::BandChild:Chevron:Context( ::BandChild:hWnd )

      IF !::BandChild:Chevron:CoolBar
         ::Application:__CurCoolMenu := NIL
//         UnhookWindowsHookEx( ::Application:hMenuHook )
      ENDIF

      ::BandChild:Chevron:Destroy()
      ::BandChild:hWndHook := NIL
      ::BandChild:Chevron  := NIL
      ::BandChild:hHook    := NIL
   ENDIF

RETURN 1


//--------------------------------------------------------------------------------------------------------

METHOD Destroy( lDestroyOwner ) CLASS CoolBarBand

   LOCAL n, oChild

   DEFAULT lDestroyOwner TO .F.

   oChild := ::BandChild
   ::Hide()
   ::Delete()
   ADEL( ::Parent:Bands, ::Index+1, .T. )
   ADEL( ::Parent:Children, ::Index+1, .T. )

   //__clsDelMsg( ::Parent:ClassH, ::Name )
   //__clsDelMsg( ::Parent:ClassH, "_" + ::Name )
   //__cls_DecData( ::Parent:ClassH )         // Decrease wData

   IF ::Image != NIL
      ::Image:Delete()
   ENDIF

   FOR n := 1 TO LEN( ::Parent:Bands )
       ::Parent:Bands[n]:Index := n - 1
       ::Parent:Bands[n]:oStruct:wID := n-1
   NEXT
   IF lDestroyOwner .AND. LEN( ::Parent:Bands ) == 0
      ::Parent:Destroy()
   ENDIF
   IF oChild != NIL
      SetParent( oChild:hWnd, oChild:Parent:hWnd )
      oChild:Show()
      oChild:Parent:CtrlMask:BringWindowToTop()
      oChild:InvalidateRect()
      ::Parent:InvalidateRect()
      oChild:Parent:CtrlMask:InvalidateRect()
   ENDIF
RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD SaveChild() CLASS CoolBarBand
   ::Backup := ::BandChild
RETURN SELF

//--------------------------------------------------------------------------------------------------------

METHOD RestoreChild() CLASS CoolBarBand
   ::SetChild( ::Backup )
RETURN SELF


//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------------

CLASS FloatingToolBar INHERIT WinForm

   DATA BandBuffer
   DATA Band

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD OnMove()

ENDCLASS

//--------------------------------------------------------------------------------------------------------

METHOD Init( oParent, oBand ) CLASS FloatingToolBar

   ::Super:Init( oParent )
   ::ClsName    := "FloatBar"
   ::ExStyle := WS_EX_TOOLWINDOW
   ::BandBuffer := oBand
   ::KeepParentActive := .T.
   ::KeepActive       := .T.

RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD Create() CLASS FloatingToolBar

   LOCAL oBar, oTool, oBtn, n, aBtns, cCaption

   //::SetStyle( WS_THICKFRAME, .F. )
   ::Width   := ::BandBuffer:MinWidth + 20
   ::Super:Create()

   oBar := CoolBar( Self )
   oBar:SetStyle( WS_BORDER, .F. )
   oBar:SetStyle( RBS_BANDBORDERS, .F. )
   oBar:Create()

   ::Band := CoolBarBand( oBar )

   ::Band:MinWidth  := 0//::Width + ::BandBuffer:BandChild:Banner:Width
   ::Band:MinHeight := ::Height - GetSystemMetrics( SM_CYCAPTION )
   ::Band:Grippers  := .F.
   //::Band:Caption   := ::BandBuffer:Caption

   oTool := ::BandBuffer:BandChild

   ::Caption := ::BandBuffer:Caption

   oTool:SetStyle( TBSTYLE_TRANSPARENT )

   ::Band:BandChild := oTool 
   ::Band:Create()
   oBar:Show()


   IF ::BandBuffer:oStruct:hbmBack != NIL
      ::Band:oStruct:fMask   := RBBIM_BACKGROUND
      ::Band:oStruct:hbmBack := ::BandBuffer:oStruct:hbmBack

      SendMessage( oBar:hWnd, RB_SETBANDINFO, 0, ::Band:oStruct )
   ENDIF
   ::Band:Chevron := .T.

RETURN SELF


//--------------------------------------------------------------------------------------------------------

METHOD OnMove( x, y ) CLASS FloatingToolBar
   LOCAL hWnd, aRect, n, oBand, aMouse, oChild, pt, oControl
   
   pt := (struct POINT)
   pt:x  := x
   pt:y  := y-18
   ScreenToClient( ::Parent:hWnd, @pt )

   oControl := ::GetChildFromPoint( pt )

   IF oControl != NIL .AND. oControl:__xCtrlName == "CoolBar"
      oBand := CoolBarBand( oControl )

      oBand:oStruct      := ::BandBuffer:oStruct
      oBand:MinWidth     := ::BandBuffer:MinWidth
      oBand:MinHeight    := ::BandBuffer:MinHeight
      oBand:AllowUndock  := ::BandBuffer:AllowUndock
      oBand:Caption      := ::Caption
      oBand:BandChild    := ::BandBuffer:BandChild
      oBand:Break        := .T.
      oBand:Chevron      := .T.
      oBand:Create()
      ::Destroy()
      IF ::BandBuffer:Image != NIL
         oBand:Image := ::BandBuffer:Image
         oBand:Image:Create()
      ENDIF
      oBand:BandChild:Show()
   ENDIF

RETURN NIL


//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------------

CLASS Bitmap

   DATA Handle    EXPORTED
   DATA Name      EXPORTED
   DATA Path      EXPORTED
   DATA MaskColor EXPORTED
   DATA Width     EXPORTED
   DATA Height    EXPORTED
   DATA nLoad     PROTECTED
   DATA Caption   EXPORTED

   METHOD Init() CONSTRUCTOR
   METHOD Create()
   METHOD Delete() INLINE DeleteObject( ::Handle )

ENDCLASS

//---------------------------------------------------------------------------------------------------

METHOD Init( cName, cPath, nMask, lCreate) CLASS BITMAP

   DEFAULT lCreate TO .F.
   ::Name      := cName
   ::Path      := cPath
   ::MaskColor := nMask
   IF lCreate
      ::Create()
   ENDIF

RETURN Self

//---------------------------------------------------------------------------------------------------

METHOD Create() CLASS Bitmap

   LOCAL aSize, cName := ::Name

   IF ::Path != NIL
      ::nLoad := ::nLoad | LR_LOADFROMFILE
      cName   := ::Path+"\"+::Name
   ENDIF
   ::Handle := LoadImage( ::Instance, cName, IMAGE_BITMAP,,, ::nLoad )
   aSize    := GetBmpSize( ::Handle )
   ::Width  := aSize[1]
   ::Height := aSize[2]

RETURN SELF


//---------------------------------------------------------------------------------------------------

