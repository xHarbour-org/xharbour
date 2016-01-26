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

#define CLEARTYPE_QUALITY 0x05

//--------------------------------------------------------------------------------------------------------

CLASS Font
   DATA EnumQuality EXPORTED INIT { { "Default", "Draft", "Proof", "Non Antialiased", "Antialiased" },;
                                    { DEFAULT_QUALITY, DRAFT_QUALITY, PROOF_QUALITY, NONANTIALIASED_QUALITY, ANTIALIASED_QUALITY, CLEARTYPE_QUALITY } }

   PROPERTY FaceName     SET (::xFaceName    := v, ::Modify(v))
   PROPERTY Escapement   SET (::xEscapement  := v, ::Modify(v))
   PROPERTY Orientation  SET (::xOrientation := v, ::Modify(v))
   PROPERTY Width        SET (::xWidth       := v, ::Modify(v))
   PROPERTY Quality      SET (::xQuality     := v, ::Modify(v)) DEFAULT DEFAULT_QUALITY

   PROPERTY FileName     SET ::AddResource(v)

   PROPERTY Bold         GET (::Weight == 700)   SET (::Weight := IIF( v, 700, 400 ))
   PROPERTY Italic       GET (::nItalic == 1)    SET (::nItalic := IIF( v, 1, 0 ))
   PROPERTY Underline    GET (::nUnderline == 1) SET (::nUnderline := IIF( v, 1, 0 ))
   PROPERTY StrikeOut    GET (::nStrikeOut == 1) SET (::nStrikeOut := IIF( v, 1, 0 ))
   PROPERTY PointSize    SET ::SetPointSize( v )

   PROPERTY CharSet      SET (::xCharSet    := v, ::Modify(v)) NOTPUBLIC
   PROPERTY Height       SET (::xHeight     := v, ::Modify(v)) NOTPUBLIC
   PROPERTY Weight       SET (::xWeight     := v, ::Modify(v)) NOTPUBLIC
   PROPERTY nItalic      SET (::xnItalic    := v, ::Modify(v)) NOTPUBLIC
   PROPERTY nUnderline   SET (::xnUnderline := v, ::Modify(v)) NOTPUBLIC
   PROPERTY nStrikeOut   SET (::xnStrikeOut := v, ::Modify(v)) NOTPUBLIC

   DATA Handle           EXPORTED
   DATA Owner            EXPORTED
   DATA ClsName          EXPORTED INIT "Font"
   DATA __xCtrlName      EXPORTED INIT "Font"
   DATA OutPrecision     EXPORTED
   DATA ClipPrecision    EXPORTED
   DATA PitchAndFamily   EXPORTED
   DATA Shared           EXPORTED INIT .F.
   DATA ncm              EXPORTED
   DATA lCreateHandle    EXPORTED INIT .T.
   DATA __IsInstance     EXPORTED INIT .F.
   DATA __ExplorerFilter EXPORTED INIT {;
                                       { "Font Files", "*.ttf;*.fnt;*.fon;*.ttc;*.fot;*.otf" },;
                                       { "Raw TrueType (*.ttf)", "*.ttf" },;
                                       { "Raw bitmap font (*.fnt)", "*.fnt*" },;
                                       { "Font resource (*.fon)", "*.fon" },;
                                       { "East Asian Windows (*.ttc)", "*.ttc" },;
                                       { "TrueType resource (*.fot)", "*.fot" },;
                                       { "PostScript OpenType (*.otf)", "*.otf" };
                                       }
   ACCESS DesignMode      INLINE IIF( ::Owner != NIL .AND. __ObjHasMsg( ::Owner, "DesignMode" ), ::Owner:DesignMode, .F. )

   METHOD Init() CONSTRUCTOR
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
   METHOD Destroy()      INLINE ::Delete()
ENDCLASS

//--------------------------------------------------------------------------------------------------------
METHOD Init( oOwner ) CLASS Font
   ::Owner := oOwner
RETURN Self

//--------------------------------------------------------------------------------------------------------
METHOD Create() CLASS Font
   LOCAL cFont

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

   IF ::Owner != NIL
      IF ::nUnderline == NIL .AND. ::Owner:__xCtrlName == "LinkLabel"
         ::ncm:lfMessageFont:lfUnderline := 1
         ::nUnderline := 1
      ENDIF
   ENDIF

   ::ncm:lfMessageFont:lfHeight         := IFNIL( ::Height        , ::ncm:lfMessageFont:lfHeight        , -::Height        )
   ::ncm:lfMessageFont:lfWidth          := IFNIL( ::Width         , ::ncm:lfMessageFont:lfWidth         , ::Width          )
   ::ncm:lfMessageFont:lfEscapement     := IFNIL( ::Escapement    , ::ncm:lfMessageFont:lfEscapement    , ::Escapement     )
   ::ncm:lfMessageFont:lfOrientation    := IFNIL( ::Orientation   , ::ncm:lfMessageFont:lfOrientation   , ::Orientation    )
   ::ncm:lfMessageFont:lfWeight         := IFNIL( ::Weight        , ::ncm:lfMessageFont:lfWeight        , ::Weight         )
   ::ncm:lfMessageFont:lfItalic         := IFNIL( ::nItalic       , ::ncm:lfMessageFont:lfItalic        , ::nItalic        )
   ::ncm:lfMessageFont:lfUnderline      := IFNIL( ::nUnderline    , ::ncm:lfMessageFont:lfUnderline     , ::nUnderline     )
   ::ncm:lfMessageFont:lfStrikeOut      := IFNIL( ::nStrikeOut    , ::ncm:lfMessageFont:lfStrikeOut     , ::nStrikeOut     )
   ::ncm:lfMessageFont:lfCharSet        := IFNIL( ::CharSet       , ::ncm:lfMessageFont:lfCharSet       , ::CharSet        )

   ::ncm:lfMessageFont:lfOutPrecision   := IFNIL( ::OutPrecision  , OUT_DEFAULT_PRECIS                  , ::OutPrecision   )
   ::ncm:lfMessageFont:lfClipPrecision  := IFNIL( ::ClipPrecision , CLIP_DEFAULT_PRECIS                 , ::ClipPrecision  )
   ::ncm:lfMessageFont:lfQuality        := IFNIL( ::Quality       , DEFAULT_QUALITY                     , ::Quality        )
   ::ncm:lfMessageFont:lfPitchAndFamily := IFNIL( ::PitchAndFamily, DEFAULT_PITCH + FF_DONTCARE         , ::PitchAndFamily )

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

   ::Delete()
   IF ::lCreateHandle
      ::Handle := CreateFontIndirect( ::ncm:lfMessageFont )
   ENDIF

   __SetInitialValues( Self )

RETURN Self

METHOD Delete() CLASS Font
   IF ::Handle != NIL
      IF ::Owner != NIL .AND. __objHasMsg( ::Owner, "hWnd" )
         SendMessage( ::Owner:hWnd, WM_SETFONT, NIL, MAKELPARAM( 1, 0 ) )
      ENDIF
      DeleteObject( ::Handle )
      ::Handle := NIL
   ENDIF
RETURN .T.

//--------------------------------------------------------------------------------------------------------

METHOD Set( oOwner ) CLASS Font
   DEFAULT oOwner TO ::Owner
   ::Owner := oOwner
   IF oOwner:ClsName != "FontDialog"
      IF oOwner:HasMessage( "SendMessage" )
         oOwner:SendMessage( WM_SETFONT, ::Handle, MAKELPARAM( 1, 0 ) )
       ELSEIF oOwner:HasMessage( "InvalidateRect" )
         oOwner:InvalidateRect()
      ENDIF
      ::GetPointSize()
   ENDIF
RETURN Self

METHOD Choose( oOwner, lSet, nStyle ) CLASS Font
   LOCAL cf := (struct CHOOSEFONT)

   DEFAULT lSet TO .T.
   cf:lStructSize := cf:sizeof()
   cf:hwndOwner   := IIF( oOwner != NIL, IIF( VALTYPE(oOwner)=="O", oOwner:hWnd, oOwner ), GetActiveWindow() )

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
   IF ::Owner != NIL .AND. ::Owner:HasMessage( "ForeColor" )
      cf:rgbColors := ::Owner:ForeColor
   ENDIF
   cf:Flags := IIF( nStyle != NIL, nStyle, CF_SCREENFONTS | CF_INITTOLOGFONTSTRUCT | CF_EFFECTS )

   IF ChooseFont( @cf )
      IF cf:lpLogFont:lfItalic <> 0
         cf:lpLogFont:lfItalic := 1
      ENDIF
      IF lSet
         ::xFaceName      := cf:lpLogFont:lfFaceName:AsString()

         IF ::Owner != NIL .AND. ::Owner:HasMessage( "ForeColor" )
            ::Owner:ForeColor := cf:rgbColors
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

      IF ::Delete()
         IF ::lCreateHandle
            ::Handle := CreateFontIndirect( lf )
         ENDIF
         IF ::Owner != NIL .AND. __objHasMsg( ::Owner, "hWnd" ) .AND. IsWindow( ::Owner:hWnd )
            ::Set( ::Owner )
         ENDIF
      ENDIF
   ENDIF
RETURN Self

METHOD EnumFamilies( cFamily ) CLASS Font
   EnumFontFamilies( ::Owner:Drawing:hDC, cFamily, WinCallBackPointer( HB_ObjMsgPtr( Self, "EnumFamiliesProc" ), Self ), NIL )
RETURN Self

METHOD EnumFamiliesProc( lpelf ) CLASS Font
   LOCAL lf := (struct ENUMLOGFONT*) lpelf
RETURN 1

//-----------------------------------------------------------------------------

METHOD SetPointSize( n ) CLASS Font
   LOCAL hDC := CreateCompatibleDC() //GetDC(0)
   ::Height := -MulDiv( n, GetDeviceCaps( hDC, LOGPIXELSY ), 72 )
   DeleteDC( hDC )
   ::xPointSize := n

   IF ::DesignMode .AND. ::Owner != NIL  .AND. __objHasMsg( ::Owner, "hWnd" ) .AND. IsWindow( ::Owner:hWnd )
      ::Owner:SetWindowPos(, 0,0,0,0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER )
   ENDIF
RETURN Self


METHOD GetPointSize() CLASS Font
   LOCAL tm, n, hDC
   hDC := CreateCompatibleDC()
   GetTextMetrics( hDC, @tm )
   IF tm != NIL
      ::xPointSize := MulDiv( ABS( ::Height ) - tm:tmInternalLeading, 72, GetDeviceCaps( hDC, LOGPIXELSY ) ) + 2
   ENDIF
   DeleteDC( hDC )
RETURN n

