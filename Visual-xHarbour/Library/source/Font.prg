/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Font.prg                                                                                             *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

#include "debug.ch"
#Include "vxh.ch"
#include "colors.ch"
#include "commdlg.ch"

//--------------------------------------------------------------------------------------------------------

CLASS Font
   DATA Handle         EXPORTED
   
   DATA AllowHandle    EXPORTED INIT .T.
   PROPERTY FaceName     READ xFaceName    WRITE Modify
   PROPERTY Escapement   READ xEscapement  WRITE Modify
   PROPERTY Orientation  READ xOrientation WRITE Modify
   PROPERTY Width        READ xWidth       WRITE Modify
   PROPERTY FileName     READ xFileName    WRITE AddResource

   PROPERTY CharSet      READ xCharSet     WRITE Modify HIDDEN

   PROPERTY Height       READ xHeight      WRITE Modify HIDDEN
   PROPERTY Weight       READ xWeight      WRITE Modify HIDDEN
   PROPERTY nItalic      READ xnItalic     WRITE Modify HIDDEN
   PROPERTY nUnderline   READ xnUnderline  WRITE Modify HIDDEN
   PROPERTY nStrikeOut   READ xnStrikeOut  WRITE Modify HIDDEN

   DATA Parent         EXPORTED
   DATA ClsName        EXPORTED INIT "Font"
   DATA OutPrecision   EXPORTED
   DATA ClipPrecision  EXPORTED
   DATA Quality        EXPORTED
   DATA PitchAndFamily EXPORTED
   DATA Shared         EXPORTED INIT .F.
   DATA ncm            EXPORTED
   DATA __IsInstance     EXPORTED INIT .F.
   DATA __ClassInst      EXPORTED
   DATA __ExplorerFilter EXPORTED INIT {;
                                       { "Font Files", "*.ttf;*.fnt;*.fon;*.ttc;*.fot;*.otf" },;
                                       { "Raw TrueType (*.ttf)", "*.ttf" },;
                                       { "Raw bitmap font (*.fnt)", "*.fnt*" },;
                                       { "Font resource (*.fon)", "*.fon" },;
                                       { "East Asian Windows (*.ttc)", "*.ttc" },;
                                       { "TrueType resource (*.fot)", "*.fot" },;
                                       { "PostScript OpenType (*.otf)", "*.otf" };
                                       }

   ACCESS Bold      INLINE ::Weight == 700  PERSISTENT
   ASSIGN Bold(l)   INLINE ::Weight := IIF( l, 700, 400 )

   ACCESS Italic    INLINE ::nItalic == 1  PERSISTENT
   ASSIGN Italic(l) INLINE ::nItalic := IIF( l, 1, 0 )

   ACCESS Underline    INLINE ::nUnderline == 1  PERSISTENT
   ASSIGN Underline(l) INLINE ::nUnderline := IIF( l, 1, 0 )

   ACCESS StrikeOut    INLINE ::nStrikeOut == 1  PERSISTENT
   ASSIGN StrikeOut(l) INLINE ::nStrikeOut := IIF( l, 1, 0 )

   DATA xPointSize     PROTECTED INIT 0
   ACCESS PointSize    INLINE ::xPointSize PERSISTENT
   ASSIGN PointSize(n) INLINE ::SetPointSize( n )

   METHOD Create()
   METHOD Select( hDC ) INLINE SelectObject( hDC, ::Handle )
   METHOD Delete()
   METHOD Choose()
   METHOD Modify()
   METHOD Set()
   METHOD SetPointSize()
   METHOD GetPointSize()
   METHOD AddResource(c) INLINE AddFontResource( c )//, FR_PRIVATE | FR_NOT_ENUM )
   METHOD EnumFamilies()
   METHOD EnumFamiliesProc()
ENDCLASS

//--------------------------------------------------------------------------------------------------------

METHOD Create() CLASS Font
   LOCAL cBuffer, hDC, nFactor, cFont, n, cName, cChar, aFont

   ::ncm := (struct NONCLIENTMETRICS)
   ::ncm:cbSize := ::ncm:Sizeof()
   
   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ::ncm:Sizeof(), @::ncm, 0 )

   IF ::FaceName != NIL
      ::ncm:lfMessageFont:lfFaceName:Buffer( ::FaceName )
   ENDIF

   cFont := ::ncm:lfMessageFont:lfFaceName:AsString()

   IF ::Escapement != 0 .AND. cFont == "MS Sans Serif"
      ::ncm:lfMessageFont:lfFaceName:Buffer( "Tahoma" )
      cFont := "Tahoma"
   ENDIF
   
   IF ::Parent != NIL
      IF ::nUnderline == NIL .AND. ::Parent:__xCtrlName == "LinkLabel"
         ::ncm:lfMessageFont:lfUnderline := 1
         ::nUnderline := 1
      ENDIF
   ENDIF
   
   ::ncm:lfMessageFont:lfHeight         := IFNIL( ::Height        , ::ncm:lfMessageFont:lfHeight        , -::Height         )
   ::ncm:lfMessageFont:lfWidth          := IFNIL( ::Width         , ::ncm:lfMessageFont:lfWidth         , ::Width          )
   ::ncm:lfMessageFont:lfEscapement     := IFNIL( ::Escapement    , ::ncm:lfMessageFont:lfEscapement    , ::Escapement     )
   ::ncm:lfMessageFont:lfOrientation    := IFNIL( ::Orientation   , ::ncm:lfMessageFont:lfOrientation   , ::Orientation    )
   ::ncm:lfMessageFont:lfWeight         := IFNIL( ::Weight        , ::ncm:lfMessageFont:lfWeight        , ::Weight         )
   ::ncm:lfMessageFont:lfItalic         := IFNIL( ::nItalic       , ::ncm:lfMessageFont:lfItalic        , ::nItalic         )
   ::ncm:lfMessageFont:lfUnderline      := IFNIL( ::nUnderline    , ::ncm:lfMessageFont:lfUnderline     , ::nUnderline      )
   ::ncm:lfMessageFont:lfStrikeOut      := IFNIL( ::nStrikeOut    , ::ncm:lfMessageFont:lfStrikeOut     , ::nStrikeOut      )
   ::ncm:lfMessageFont:lfCharSet        := IFNIL( ::CharSet       , ::ncm:lfMessageFont:lfCharSet       , ::CharSet        )

   ::ncm:lfMessageFont:lfOutPrecision   := IFNIL( ::OutPrecision  , OUT_DEFAULT_PRECIS                  , ::OutPrecision   )
   ::ncm:lfMessageFont:lfClipPrecision  := IFNIL( ::ClipPrecision , CLIP_DEFAULT_PRECIS                 , ::ClipPrecision  )
   ::ncm:lfMessageFont:lfQuality        := IFNIL( ::Quality       , DEFAULT_QUALITY                     , ::Quality        )
   ::ncm:lfMessageFont:lfPitchAndFamily := IFNIL( ::PitchAndFamily, DEFAULT_PITCH + FF_DONTCARE         , ::PitchAndFamily )
   
   ::Delete()
   IF ::AllowHandle
      ::Handle := CreateFontIndirect( ::ncm:lfMessageFont )
   ENDIF
   
   ::xFaceName     := cFont

   ::xHeight       := ::ncm:lfMessageFont:lfHeight
   ::xWidth        := ::ncm:lfMessageFont:lfWidth
   ::xEscapement   := ::ncm:lfMessageFont:lfEscapement
   ::xOrientation  := ::ncm:lfMessageFont:lfOrientation
   ::xCharSet      := ::ncm:lfMessageFont:lfCharSet

   ::xWeight       := ::ncm:lfMessageFont:lfWeight
   ::xnItalic      := ::ncm:lfMessageFont:lfItalic
   ::xnUnderline   := ::ncm:lfMessageFont:lfUnderline
   ::xnStrikeOut   := ::ncm:lfMessageFont:lfStrikeOut
   ::GetPointSize()

   IF ::Parent != NIL .AND. ::Parent:__ClassInst != NIL
      ::__ClassInst := __ClsInst( ::ClassH )
      ::__ClassInst:__IsInstance  := .T.
      ::__ClassInst:xWeight      := ::xWeight
      ::__ClassInst:xFaceName    := ::xFaceName
      ::__ClassInst:xWidth       := ::xWidth
      ::__ClassInst:xEscapement  := ::xEscapement
      ::__ClassInst:xOrientation := ::xOrientation
      ::__ClassInst:xCharSet     := ::xCharSet
      ::__ClassInst:xHeight      := ::xHeight
      ::__ClassInst:xnItalic     := ::xnItalic
      ::__ClassInst:xnUnderline  := ::xnUnderline
      ::__ClassInst:xnStrikeOut  := ::xnStrikeOut
      ::__ClassInst:xPointSize   := ::xPointSize
   ENDIF

RETURN Self

METHOD Delete() CLASS Font
   IF ::Handle != NIL
      DeleteObject( ::Handle )
   ENDIF
RETURN Self


//--------------------------------------------------------------------------------------------------------

METHOD Set( o ) CLASS Font
   ::Parent := o
   IF o:HasMessage( "SendMessage" )// o:ClsName != "CMenuItem" .AND. o:ClassName != "COOLMENUITEM"
      o:SendMessage( WM_SETFONT, ::Handle, MAKELPARAM( 1, 0 ) )
    ELSEIF o:HasMessage( "InvalidateRect" )
      o:InvalidateRect()
   ENDIF
   ::GetPointSize()
RETURN Self

METHOD Choose( oOwner, lSet ) CLASS Font
   LOCAL n, cf := (struct CHOOSEFONT)

   DEFAULT lSet TO .T.
   cf:lStructSize := cf:sizeof()
   cf:hwndOwner   := IIF( oOwner != NIL, oOwner:hWnd, GetActiveWindow() )

   ::ncm:lfMessageFont:lfFaceName:Buffer( ::FaceName )
   ::ncm:lfMessageFont:lfHeight         := IFNIL( ::Height        , ::ncm:lfMessageFont:lfHeight        , -::Height         )
   ::ncm:lfMessageFont:lfWidth          := IFNIL( ::Width         , ::ncm:lfMessageFont:lfWidth         , ::Width          )
   ::ncm:lfMessageFont:lfEscapement     := IFNIL( ::Escapement    , ::ncm:lfMessageFont:lfEscapement    , ::Escapement     )
   ::ncm:lfMessageFont:lfOrientation    := IFNIL( ::Orientation   , ::ncm:lfMessageFont:lfOrientation   , ::Orientation    )
   ::ncm:lfMessageFont:lfCharSet        := IFNIL( ::CharSet       , ::ncm:lfMessageFont:lfCharSet       , ::CharSet        )

   ::ncm:lfMessageFont:lfWeight         := IFNIL( ::Weight        , ::ncm:lfMessageFont:lfWeight        , ::Weight         )
   ::ncm:lfMessageFont:lfItalic         := IFNIL( ::nItalic        , ::ncm:lfMessageFont:lfItalic        , ::nItalic         )
   ::ncm:lfMessageFont:lfUnderline      := IFNIL( ::nUnderline     , ::ncm:lfMessageFont:lfUnderline     , ::nUnderline      )
   ::ncm:lfMessageFont:lfStrikeOut      := IFNIL( ::nStrikeOut     , ::ncm:lfMessageFont:lfStrikeOut     , ::nStrikeOut      )

   ::ncm:lfMessageFont:lfOutPrecision   := IFNIL( ::OutPrecision  , OUT_DEFAULT_PRECIS                  , ::OutPrecision   )
   ::ncm:lfMessageFont:lfClipPrecision  := IFNIL( ::ClipPrecision , CLIP_DEFAULT_PRECIS                 , ::ClipPrecision  )
   ::ncm:lfMessageFont:lfQuality        := IFNIL( ::Quality       , DEFAULT_QUALITY                     , ::Quality        )
   ::ncm:lfMessageFont:lfPitchAndFamily := IFNIL( ::PitchAndFamily, DEFAULT_PITCH + FF_DONTCARE         , ::PitchAndFamily )
   cf:lpLogFont   := ::ncm:lfMessageFont
   IF ::Parent != NIL
      cf:rgbColors := ::Parent:ForeColor
   ENDIF
   cf:Flags       := CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT | CF_EFFECTS

   IF ChooseFont( @cf )
      IF cf:lpLogFont:lfItalic <> 0
         cf:lpLogFont:lfItalic := 1
      ENDIF
      IF lSet
         ::xFaceName      := cf:lpLogFont:lfFaceName:AsString()

         IF ::Parent != NIL
            ::Parent:ForeColor := cf:rgbColors
         ENDIF

         ::xHeight        := cf:lpLogFont:lfHeight
         ::xWidth         := cf:lpLogFont:lfWidth
         ::xEscapement    := cf:lpLogFont:lfEscapement
         ::xOrientation   := cf:lpLogFont:lfOrientation
         ::xCharSet       := cf:lpLogFont:lfCharSet
         ::xWeight        := cf:lpLogFont:lfWeight

         ::xnItalic       := cf:lpLogFont:lfItalic
         ::xnUnderline    := cf:lpLogFont:lfUnderline
         ::xnStrikeOut    := cf:lpLogFont:lfStrikeOut

         ::OutPrecision   := cf:lpLogFont:lfOutPrecision
         ::ClipPrecision  := cf:lpLogFont:lfClipPrecision
         ::Quality        := cf:lpLogFont:lfQuality
         ::PitchAndFamily := cf:lpLogFont:lfPitchAndFamily

         ::ncm:lfMessageFont:lfFaceName:Buffer( ::FaceName )
         ::ncm:lfMessageFont:lfHeight         := ::Height
         ::ncm:lfMessageFont:lfWidth          := ::Width
         ::ncm:lfMessageFont:lfEscapement     := ::Escapement
         ::ncm:lfMessageFont:lfOrientation    := ::Orientation
         ::ncm:lfMessageFont:lfWeight         := ::Weight
         ::ncm:lfMessageFont:lfItalic         := ::nItalic
         ::ncm:lfMessageFont:lfUnderline      := ::nUnderline
         ::ncm:lfMessageFont:lfStrikeOut      := ::nStrikeOut
         ::ncm:lfMessageFont:lfCharSet        := ::CharSet
         ::ncm:lfMessageFont:lfOutPrecision   := ::OutPrecision
         ::ncm:lfMessageFont:lfClipPrecision  := ::ClipPrecision
         ::ncm:lfMessageFont:lfQuality        := ::Quality
         ::ncm:lfMessageFont:lfPitchAndFamily := ::PitchAndFamily
         ::GetPointSize()
      ENDIF
    ELSE
      cf := NIL
   ENDIF
RETURN cf

//--------------------------------------------------------------------------------------------------------

METHOD Modify() CLASS Font
   LOCAL lf
   IF !EMPTY( ::FaceName ) .AND. !::__IsInstance
      lf := (struct LOGFONT)
      lf:lfFaceName:Buffer( ::xFaceName )
      lf:lfHeight         := ::xHeight
      lf:lfWidth          := ::xWidth
      lf:lfEscapement     := ::xEscapement
      lf:lfOrientation    := ::xOrientation
      lf:lfWeight         := ::xWeight
      lf:lfItalic         := ::xnItalic
      lf:lfUnderline      := ::xnUnderline
      lf:lfStrikeOut      := ::xnStrikeOut
      lf:lfCharSet        := ::xCharSet
      lf:lfOutPrecision   := ::OutPrecision
      lf:lfClipPrecision  := ::ClipPrecision
      lf:lfQuality        := ::Quality
      lf:lfPitchAndFamily := ::PitchAndFamily

      ::Delete()
      IF ::AllowHandle
         ::Handle := CreateFontIndirect( lf )
      ENDIF
      IF ::Parent != NIL .AND. IsWindow( ::Parent:hWnd )
         ::Set( ::Parent )
      ENDIF
   ENDIF
RETURN Self

METHOD EnumFamilies( cFamily ) CLASS Font
   EnumFontFamilies( ::Parent:Drawing:hDC, cFamily, WinCallBackPointer( HB_ObjMsgPtr( Self, "EnumFamiliesProc" ), Self ), NIL )
RETURN Self

METHOD EnumFamiliesProc( lpelf, lpntm, FontType, lParam ) CLASS Font
   LOCAL lf := (struct ENUMLOGFONT*) lpelf
RETURN 1

//-----------------------------------------------------------------------------

METHOD SetPointSize( n ) CLASS Font
   LOCAL hDC := CreateCompatibleDC() //GetDC(0)
   ::Height := -MulDiv( n, GetDeviceCaps( hDC, LOGPIXELSY ), 72 )
   DeleteDC( hDC )
   ::xPointSize := n
   IF ::Parent != NIL .AND. IsWindow( ::Parent:hWnd )
      ::Parent:SetWindowPos(, 0,0,0,0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
   ENDIF
RETURN Self


METHOD GetPointSize() CLASS Font
   LOCAL tm, n, hDC
   hDC := GetDC(0)
   GetTextMetrics( hDC, @tm )
   IF tm != NIL
      ::xPointSize := MulDiv( ABS( ::Height ) - tm:tmInternalLeading, 72, GetDeviceCaps( hDC, LOGPIXELSY ) ) + 2
   ENDIF
   ReleaseDC( 0, hDC )
RETURN n
