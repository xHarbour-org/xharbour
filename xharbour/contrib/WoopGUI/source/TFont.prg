/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"
//#include "wingdi.ch"

CLASS TFont FROM TGDIObject

    CLASSDATA aoResources AS ARRAY   INIT {} HIDDEN
    //DATA nResources  AS NUMERIC INIT 0

    //DATA nHandle            AS NUMERIC    // Handle
    DATA nHeight            AS NUMERIC    // height of font
    DATA nWidth             AS NUMERIC    // average character width
    DATA nEscapement        AS NUMERIC    // angle of escapement
    DATA nOrientation       AS NUMERIC    // base-line orientation angle
    DATA nWeight            AS NUMERIC    // font weight
    DATA lItalic            AS LOGICAL    // italic attribute option
    DATA lUnderline         AS LOGICAL    // underline attribute option
    DATA lStrikeOut         AS LOGICAL    // strikeout attribute option
    DATA nCharSet           AS NUMERIC    // character set identifier
    DATA nOutputPrecision   AS NUMERIC    // output precision
    DATA nClipPrecision     AS NUMERIC    // clipping precision
    DATA nQuality           AS NUMERIC    // output quality
    DATA nPitchAndFamily    AS NUMERIC    // pitch and family
    DATA cFace              AS STRING     // typeface name

    METHOD New() CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD Choose()                      INLINE LOCAL h,;
                                                h := WG_ChooseFont( Self ),;
                                                IIF( h <> 0, ( ::AddResource( Self ), Self ), NIL )
    METHOD GetValue()
    METHOD Destroy()                     INLINE ::DelResource()
    METHOD SetBold()
    METHOD SetUnderlined()

    METHOD CreateResource()

ENDCLASS

METHOD New( cFontFace AS STRING, nFontSize AS NUMERIC, lItalic, lUnderline, lStrikeOut, lBold ) CLASS TFont

    LOCAL nHeight
    LOCAL nWidth
    LOCAL nEscapement
    LOCAL nOrientation
    LOCAL nWeight
    LOCAL nCharSet
    LOCAL nOutputPrecision
    LOCAL nClipPrecision
    LOCAL nQuality
    LOCAL nPitchAndFamily
    LOCAL cFace

    DEFAULT lBold      TO FALSE
    DEFAULT cFontFace  TO "MS Sans Serif"
    DEFAULT nFontSize  TO 8

    nHeight            := ConvertPointsToLU( nFontSize )
    nWidth             := 0
    nEscapement        := 0
    nOrientation       := 0
    nWeight            := IIF( lBold, FW_NORMAL, FW_BOLD )
    DEFAULT lItalic    TO FALSE
    DEFAULT lUnderline TO FALSE
    DEFAULT lStrikeOut TO FALSE
    nCharSet           := DEFAULT_CHARSET
    nOutputPrecision   := OUT_DEFAULT_PRECIS
    nClipPrecision     := CLIP_DEFAULT_PRECIS
    nQuality           := DEFAULT_QUALITY
    nPitchAndFamily    := DEFAULT_PITCH
    cFace              := cFontFace

    ::NewExtended( nHeight, nWidth, nEscapement, nOrientation, nWeight, lItalic, lUnderline,;
                   lStrikeOut, nCharSet, nOutputPrecision, nClipPrecision, nQuality, nPitchAndFamily,;
                   cFace )

RETURN Self


METHOD NewExtended( nHeight, nWidth, nEscapement, nOrientation, nWeight, lItalic, lUnderline,;
                    lStrikeOut, nCharSet, nOutputPrecision, nClipPrecision, nQuality, nPitchAndFamily,;
                    cFace ) CLASS TFont
    LOCAL oFont

    ASSIGN ::nHeight          WITH nHeight          DEFAULT 0
    ASSIGN ::nWidth           WITH nWidth           DEFAULT 0
    ASSIGN ::nEscapement      WITH nEscapement      DEFAULT 0
    ASSIGN ::nOrientation     WITH nOrientation     DEFAULT 0
    ASSIGN ::nWeight          WITH nWeight          DEFAULT FW_DONTCARE
    ASSIGN ::lItalic          WITH lItalic          DEFAULT FALSE
    ASSIGN ::lUnderline       WITH lUnderline       DEFAULT FALSE
    ASSIGN ::lStrikeOut       WITH lStrikeOut       DEFAULT FALSE
    ASSIGN ::nCharSet         WITH nCharSet         DEFAULT DEFAULT_CHARSET
    ASSIGN ::nOutputPrecision WITH nOutputPrecision DEFAULT OUT_DEFAULT_PRECIS
    ASSIGN ::nClipPrecision   WITH nClipPrecision   DEFAULT CLIP_DEFAULT_PRECIS
    ASSIGN ::nQuality         WITH nQuality         DEFAULT DEFAULT_QUALITY
    ASSIGN ::nPitchAndFamily  WITH nPitchAndFamily  DEFAULT DEFAULT_PITCH
    ASSIGN ::cFace            WITH cFace            DEFAULT "MS Sans Serif"

    ::SetEvalResource( {|o| o:ClassName        == "TFONT"          .AND. ;
                            o:nHeight          == ::nHeight           .AND. ;
                            o:nWidth           == ::nWidth            .AND. ;
                            o:nEscapement      == ::nEscapement       .AND. ;
                            o:nOrientation     == ::nOrientation      .AND. ;
                            o:nWeight          == ::nWeight           .AND. ;
                            o:lItalic          == ::lItalic           .AND. ;
                            o:lUnderline       == ::lUnderline        .AND. ;
                            o:lStrikeOut       == ::lStrikeOut        .AND. ;
                            o:nCharSet         == ::nCharSet          .AND. ;
                            o:nOutputPrecision == ::nOutputPrecision  .AND. ;
                            o:nClipPrecision   == ::nClipPrecision    .AND. ;
                            o:nQuality         == ::nQuality          .AND. ;
                            o:nPitchAndFamily  == ::nPitchAndFamily   .AND. ;
                            o:cFace            == ::cFace ;
                        } )

    ::AddResource()
RETURN Self

METHOD CreateResource() CLASS TFont
    LOCAL oFont

    //IF ValType( ::nHandle ) == "N"
    //   ::Destroy()
    //ENDIF

    WG_RegisterFont( ::nHeight, ::nWidth, ::nEscapement, ::nOrientation, ::nWeight, ;
                               ::lItalic, ::lUnderline, ::lStrikeOut, ::nCharSet, ::nOutputPrecision,;
                               ::nClipPrecision, ::nQuality, ::nPitchAndFamily, ::cFace )

    ::nHandle := WG_CreateFontIndirect( Self )

    // Now font is created and data values updated with real values
    // i check if there is a font resource of my application that match with this values


    //IF ( oFont := ::FindFontResourceByAttribute( ::nHeight, ::nWidth, ::nEscapement, ::nOrientation, ::nWeight, ;
    //                              ::lItalic, ::lUnderline, ::lStrikeOut, ::nCharSet, ::nOutputPrecision,;
    //                              ::nClipPrecision, ::nQuality, ::nPitchAndFamily, ::cFace ) ) <> NIL
    //   // Ok, found. I have already this font. I can release this new and use the old
    //   ::DelResource()
    //   // and make this font like the other
    //   ::nHandle   := oFont:nHandle
    //   // And add the font resource
    //   oFont:AddResource( oFont )
    //ELSE
    //   ::AddResource( Self )
    //ENDIF
RETURN Self

METHOD GetValue() CLASS TFont
  //IF ::nHandle == NIL
  //   ::Create()
  //ENDIF
RETURN Self

METHOD SetBold( lBold ) CLASS TFont
  DEFAULT lBold TO TRUE
  ::nWeight          := IIF( lBold, FW_NORMAL, FW_BOLD )
  ::AddResource()
RETURN Self

METHOD SetUnderlined( lUnderlined ) CLASS TFont
  DEFAULT lUnderlined TO TRUE
  ::lUnderline := lUnderlined
  ::AddResource()
RETURN Self

EXIT PROCEDURE __WG_TFont_Destroy()
   //MessageBox( , "Nø font = " + cStr( TFont():nResources ) )
   WG_DebugTrace( "TFont_Exit_Proc", "TFont():aoResources", TFont():aoResources )
   // Run directly releaseresource() without use delresource() because this make a scan for each
   aEval( TFont():aoResources, {|o| o:ReleaseResource() } )
RETURN
