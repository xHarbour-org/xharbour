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

// Windows definitions
CLASS TRadioGroup FROM TObject

    DATA   aoItems   AS ARRAY     INIT {}
    DATA   lVertical AS LOGICAL   INIT TRUE
    DATA   bVarBlock AS CODEBLOCK     // User variable code block - set/get
    DATA   nValue    AS NUMERIC   INIT 0

    METHOD New()                 CONSTRUCTOR
    METHOD NewExtended()         CONSTRUCTOR
    METHOD AddItem( x )          INLINE aAdd( ::aoItems, x )
    METHOD GetValue()            INLINE ::nValue
    METHOD SetValue()
    METHOD UpdateVar( xVal )   INLINE LOCAL xRet, IIF( ValType( ::bVarBlock ) == "B", ;
                                                  xRet := Eval( ::bVarBlock, IIF( xVal <> NIL, xVal, ::GetValue() ) ) ,;
                                                  NIL ), xRet

ENDCLASS

METHOD New( aValues AS ARRAY OF STRING, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel,;
            lVertical, nSelected ) CLASS TRadioGroup
    LOCAL n, nHPos, nVPos

    DEFAULT lPixel TO TRUE

    ::aoItems := {}

    ASSIGN ::lVertical WITH lVertical DEFAULT TRUE
    DEFAULT nSelected TO 1

    IF ::lVertical
       DEFAULT nWidth    TO IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
       DEFAULT nHeight   TO IIF( lPixel, 28, WG_Pixel2DialogY( 28 ) )
    ELSE
       DEFAULT nWidth    TO IIF( lPixel, 80, WG_Pixel2DialogX( 80 ) )
       DEFAULT nHeight   TO IIF( lPixel, 28, WG_Pixel2DialogY( 28 ) )
    ENDIF

    FOR n := 1 TO LEN( aValues )
        IF ::lVertical
           nVPos := nRow+((n-1)*(nHeight+2))
           nHPos := nCol
        ELSE
           nVPos := nRow
           nHPos := nCol+((n-1)*(nWidth+2))
        ENDIF

        ::AddItem( TRadioButton():New( aValues[n], nVPos, nHPos, nWidth, nHeight, oParent, bAction, ;
                                          cToolTip, cStatusBar, lPixel, IIF( n == 1, TRUE, FALSE ), ;
                                          IIF( n == nSelected, TRUE, FALSE ) ) )
    NEXT n
    ::SetValue( nSelected )

RETURN Self

METHOD NewExtended( aValues AS ARRAY OF STRING, nRow, nCol, nWidth, nHeight, oParent, bAction, cToolTip, cStatusBar, lPixel, ;
                    lVertical, nSelected, bVarBlock, oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) CLASS TRadioGroup
    LOCAL n, nHPos, nVPos

    DEFAULT lPixel TO TRUE

    ::aoItems := {}

    IF ValType( bVarBlock ) == "B"
       ::bVarBlock := bVarBlock
    ENDIF

    ASSIGN ::lVertical WITH lVertical DEFAULT TRUE
    DEFAULT nSelected TO 1

    IF ::lVertical
       DEFAULT nWidth    TO IIF( lPixel, 100, WG_Pixel2DialogX( 100 ) )
       DEFAULT nHeight   TO IIF( lPixel, 28, WG_Pixel2DialogY( 28 ) )
    ELSE
       DEFAULT nWidth    TO IIF( lPixel, 80, WG_Pixel2DialogX( 80 ) )
       DEFAULT nHeight   TO IIF( lPixel, 28, WG_Pixel2DialogY( 28 ) )
    ENDIF

    FOR n := 1 TO LEN( aValues )
        IF ::lVertical
           nVPos := nRow+((n-1)*(nHeight+2))
           nHPos := nCol
        ELSE
           nVPos := nRow
           nHPos := nCol+((n-1)*(nWidth+2))
        ENDIF

        ::AddItem( TRadioButton():NewExtended( aValues[n], nVPos, nHPos, nWidth, nHeight, oParent, bAction, ;
                                       cToolTip, cStatusBar, lPixel, IIF( n == 1, TRUE, FALSE ), ;
                                       IIF( n == nSelected, TRUE, FALSE ),;
                                       WG_RadioGroupValue(Self, n), oFont, cFontName, nFontSize, bWhen, bValid, ncFgColor, ncBgColor ) )
    NEXT n
    ::SetValue( nSelected )

RETURN Self

STATIC FUNCTION WG_RadioGroupValue( o, n )
RETURN {|| o:SetValue( n ) }

METHOD SetValue( nValue AS NUMERIC ) CLASS TRadioGroup
   ::nValue := nValue
   ::UpdateVar( nValue )
RETURN Self

