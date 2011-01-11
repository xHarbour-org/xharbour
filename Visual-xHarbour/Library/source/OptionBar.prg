/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// OptionBar.prg                                                                                      *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

#ifdef VXH_PROFESSIONAL

#include "vxh.ch"
#include "debug.ch"

#include "colors.ch"

//-----------------------------------------------------------------------------------------------------
#define DG_ADDCONTROL             1

CLASS OptionBar INHERIT Control

   DATA PagerSize    EXPORTED INIT 12
   DATA __oPage      EXPORTED
   DATA __ToolBar    EXPORTED
   DATA lCreated     PROTECTED INIT .F.
   DATA lDown        PROTECTED INIT .F.
   DATA IsContainer  EXPORTED INIT .F.
   DATA BackSysColor EXPORTED INIT GetSysColor( COLOR_BTNSHADOW )
   DATA ForeSysColor EXPORTED INIT GetSysColor( COLOR_BTNTEXT )

   DATA  xButtonCheckColor PROTECTED INIT GetSysColor( COLOR_BTNSHADOW )
   ACCESS ButtonCheckColor    INLINE ::xButtonCheckColor 
   ASSIGN ButtonCheckColor(n) INLINE ::xButtonCheckColor := n, IIF( ::__ToolBar != NIL, ::__ToolBar:ButtonCheckColor := n, )

   DATA  xButtonCheckSolid PROTECTED INIT .T.
   ACCESS ButtonCheckSolid    INLINE ::xButtonCheckSolid
   ASSIGN ButtonCheckSolid(l) INLINE ::xButtonCheckSolid := l, IIF( ::__ToolBar != NIL, ::__ToolBar:ButtonCheckSolid := l, )

   DATA xCheckGroup  PROTECTED INIT .T.
   ACCESS CheckGroup    INLINE ::xCheckGroup PERSISTENT
   ASSIGN CheckGroup(l) INLINE ::xCheckGroup := l

   DATA xImageList  PROTECTED
   ACCESS ImageList    INLINE __ChkComponent( Self, ::xImageList ) PERSISTENT
   ASSIGN ImageList(o) INLINE ::xImageList := o, IIF( ::__ToolBar != NIL, ::__ToolBar:ImageList := o,)

   DATA xHotImageList      EXPORTED
   ACCESS HotImageList     INLINE __ChkComponent( Self, ::xHotImageList ) PERSISTENT
   ASSIGN HotImageList(o)  INLINE ::xHotImageList := o


   DATA xList  PROTECTED INIT .F.
   ACCESS List    INLINE ::xList PERSISTENT
   ASSIGN List(l) INLINE ::xList := l, IIF( ::__ToolBar != NIL, ::__ToolBar:List := l,)

   METHOD IsButtonChecked( nId ) INLINE ::__ToolBar:IsButtonChecked( nId )

   METHOD Init() CONSTRUCTOR
   METHOD __AddButton()
   METHOD Create()

   METHOD CheckButton( nId, lCheck ) INLINE ::__ToolBar:CheckButton( nId, lCheck )
   METHOD OnSize()
   //METHOD OnMove()
   METHOD Undock()
   METHOD GetChildFromPoint()
ENDCLASS

METHOD GetChildFromPoint( pt, bAction, aSel ) CLASS OptionBar
   LOCAL o, rc, oCtrl, Control, Band
   ScreenToClient( ::__ToolBar:hWnd, @pt )
   FOR EACH Control IN ::Children
       rc := Control:GetRect()
       IF ptInRect( rc, pt )
          oCtrl := Control
          EXIT
       ENDIF
   NEXT
   DEFAULT oCtrl TO Self
RETURN oCtrl

//-----------------------------------------------------------------------------------------------------
METHOD __AddButton() CLASS OptionBar
   ::Application:Project:SetAction( { { DG_ADDCONTROL, 0, 0, 0, .T., Self, "OptionBarButton",,,1, {}, } }, ::Application:Project:aUndo )
RETURN( Self )

//-----------------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS OptionBar

   ::ClsName      := "OptionBar"
   DEFAULT ::__xCtrlName TO "OptionBar"
   ::Style        := WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::xWidth       := 150
   ::xHeight      := 400
   ::Super:Init( oParent )
   ::__IsStandard := .F.
   ::SmallCaption := .T.
RETURN Self


//-----------------------------------------------------------------------------------------------------

METHOD Create() CLASS OptionBar

   LOCAL n, oButton
   ::ClassBrush := ::BkBrush
   DEFAULT ::ClassBrush TO GetSysColorBrush( COLOR_BTNSHADOW )
   ::Super:Create()
   IF ::__ClassInst != NIL 
      ::__IdeContextMenuItems := { { "&Add Button", {|| ::__AddButton()} } }
   ENDIF

   ::__ToolBar := ToolBar( Self )
   WITH OBJECT ::__ToolBar
      :SetChildren      := .F.
      :ClipSiblings     := .T.
      :Width            := ::Width
      :Height           := 0
      :ButtonCheckSolid := ::ButtonCheckSolid
      :ButtonCheckColor := ::ButtonCheckColor
      :Transparent      := .T.
      :ImageList        := ::ImageList
      :List             := ::xList
      :Theming          := ::Theming
      :ForeColor        := ::ForeColor
      :SetStyle( CCS_ADJUSTABLE, .F. )
      :Create()
                      // ----- button highlight ---- | --- shadow --- | ------------ text color ---------
      :SetColorScheme( GetSysColor( COLOR_3DLIGHT ),  RGB( 0, 0, 0) )
   END
   IF ::ImageList != NIL
      ::__ToolBar:SetImageList(::ImageList)
   ENDIF
   IF ::HotImageList != NIL
      ::__ToolBar:SetHotImageList(::HotImageList)
   ENDIF

   //---------------------------------------------------------------------------------------------
   ::__oPage := PageScroller( Self )
   WITH OBJECT ::__oPage
      :SetChildren      := .F.
      :Width  := ::ClientWidth
      :Height := ::ClientHeight
      :PageChild := ::__ToolBar
      :Create()
   END
   ::lCreated := .T.

   ::SetWindowPos(,0,0,0,0,SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER)
   ::RedrawWindow( , , RDW_FRAME | RDW_NOERASE | RDW_NOINTERNALPAINT | RDW_INVALIDATE | RDW_UPDATENOW | RDW_NOCHILDREN )

   ::SetWindowTheme(, "BUTTON" )

RETURN Self

//-----------------------------------------------------------------------------------------------------

METHOD OnSize( n, x, y ) CLASS OptionBar
   ::Super:OnSize( n, x, y )

   IF ::__ToolBar != NIL
      ::__ToolBar:SendMessage( TB_SETBUTTONWIDTH, 0, MAKELONG( ::ClientWidth, ::ClientWidth ) )
      TRY
      AEVAL( ::Children, {|o| o:SetWidth( ::ClientWidth ) } )
      CATCH
      END
   ENDIF
   IF ::__oPage != NIL
      ::__oPage:__OnParentSize( ::Width, ::Height )
   ENDIF

RETURN( NIL )

/*
METHOD OnMove( x, y ) CLASS OptionBar
   ::Super:OnMove( x, y )
//   IF ::__ToolBar != NIL
//      ::__ToolBar:RedrawWindow( , , RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
//   ENDIF
RETURN( NIL )
*/

METHOD Undock() CLASS OptionBar
   ::Super:Undock()
   ::__oPage:RecalSize()
RETURN Self

//-----------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------------

CLASS OptionBarButton INHERIT ToolButton
   DATA __xCtrlName  INIT "OptionBarButton"
   DATA IsContainer   EXPORTED INIT .F.
   DATA Wrap          EXPORTED INIT .F.
   DATA ShowText      EXPORTED INIT .F.
   DATA AutoSize      EXPORTED INIT .F.
   DATA DropDown      EXPORTED INIT .F.
   DATA WholeDropDown EXPORTED INIT .F.
   DATA ContextMenu   EXPORTED INIT .F.
   DATA Check         EXPORTED INIT .F.
   DATA Separator     EXPORTED INIT .F.
   DATA CheckGroup    EXPORTED INIT .F.
   DATA Position      EXPORTED INIT 1
   ACCESS Width INLINE ::Parent:Width
   ACCESS xWidth INLINE ::Parent:xWidth
   METHOD GetRect()
ENDCLASS

METHOD GetRect() CLASS OptionBarButton
   LOCAL tbbi, rc := (struct RECT)
   SendMessage( ::Parent:hWnd, TB_GETITEMRECT, ::xPosition, @rc )
   rc:right := ::Parent:Width
RETURN rc

#endif