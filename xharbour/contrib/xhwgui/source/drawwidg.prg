/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Pens, brushes, fonts, bitmaps, icons handling
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "HBClass.ch"
#include "windows.ch"
#include "guilib.ch"

#define HS_HORIZONTAL       0       /* ----- */
#define HS_VERTICAL         1       /* ||||| */
#define HS_FDIAGONAL        2       /* \\\\\ */
#define HS_BDIAGONAL        3       /* ///// */
#define HS_CROSS            4       /* +++++ */
#define HS_DIAGCROSS        5       /* xxxxx */

//- HFont

CLASS HFont INHERIT HObject

   CLASS VAR aFonts   INIT {}
   DATA handle
   DATA name, width, height ,weight
   DATA charset, italic, Underline, StrikeOut
   DATA nCounter   INIT 1

   METHOD Add( fontName, nWidth, nHeight ,fnWeight, fdwCharSet, fdwItalic, fdwUnderline, fdwStrikeOut, nHandle )
   METHOD Select( oFont )
   METHOD Release()

ENDCLASS

METHOD Add( fontName, nWidth, nHeight ,fnWeight, ;
         fdwCharSet, fdwItalic, fdwUnderline, fdwStrikeOut, nHandle ) CLASS HFont

Local i, nlen := Len( ::aFonts )

   nHeight  := Iif( nHeight==Nil,-13,nHeight )
   fnWeight := Iif( fnWeight==Nil,0,fnWeight )
   fdwCharSet := Iif( fdwCharSet==Nil,0,fdwCharSet )
   fdwItalic := Iif( fdwItalic==Nil,0,fdwItalic )
   fdwUnderline := Iif( fdwUnderline==Nil,0,fdwUnderline )
   fdwStrikeOut := Iif( fdwStrikeOut==Nil,0,fdwStrikeOut )

   For i := 1 TO nlen
      IF ::aFonts[i]:name == fontName .AND.          ;
         ::aFonts[i]:width == nWidth .AND.           ;
         ::aFonts[i]:height == nHeight .AND.         ;
         ::aFonts[i]:weight == fnWeight .AND.        ;
         ::aFonts[i]:CharSet == fdwCharSet .AND.     ;
         ::aFonts[i]:Italic == fdwItalic .AND.       ;
         ::aFonts[i]:Underline == fdwUnderline .AND. ;
         ::aFonts[i]:StrikeOut == fdwStrikeOut

         ::aFonts[i]:nCounter ++
         IF nHandle != Nil
            DeleteObject( nHandle )
         ENDIF
         Return ::aFonts[i]
      ENDIF
   NEXT

   IF nHandle == Nil
      ::handle := CreateFont( fontName, nWidth, nHeight ,fnWeight, fdwCharSet, fdwItalic, fdwUnderline, fdwStrikeOut )
   ELSE
      ::handle := nHandle
   ENDIF

   ::name      := fontName
   ::width     := nWidth
   ::height    := nHeight
   ::weight    := fnWeight
   ::CharSet   := fdwCharSet
   ::Italic    := fdwItalic
   ::Underline := fdwUnderline
   ::StrikeOut := fdwStrikeOut

   Aadd( ::aFonts,Self )

Return Self

METHOD Select( oFont ) CLASS HFont
Local af := SelectFont( oFont )

   IF af == Nil
      Return Nil
   ENDIF

Return ::Add( af[2],af[3],af[4],af[5],af[6],af[7],af[8],af[9],af[1] )

METHOD Release() CLASS HFont
Local i, nlen := Len( ::aFonts )

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i in ::aFonts
         IF i:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aFonts,hb_enumindex() )
            Asize( ::aFonts,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aFonts[i]:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aFonts,i )
            Asize( ::aFonts,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil

//- HPen

CLASS HPen INHERIT HObject

   CLASS VAR aPens   INIT {}
   DATA handle
   DATA style, width, color
   DATA nCounter   INIT 1

   METHOD Add( nStyle,nWidth,nColor )
   METHOD Release()

ENDCLASS

METHOD Add( nStyle,nWidth,nColor ) CLASS HPen
Local i

   nStyle := Iif( nStyle == Nil,BS_SOLID,nStyle )
   nWidth := Iif( nWidth == Nil,1,nWidth )
   nColor := Iif( nColor == Nil,Vcolor("000000"),nColor )

   #ifdef __XHARBOUR__
   For EACH i in ::aPens 
      IF i:style == nStyle .AND. ;
         i:width == nWidth .AND. ;
         i:color == nColor

         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aPens )
      IF ::aPens[i]:style == nStyle .AND. ;
         ::aPens[i]:width == nWidth .AND. ;
         ::aPens[i]:color == nColor

         ::aPens[i]:nCounter ++
         Return ::aPens[i]
      ENDIF
   NEXT
   #endif

   ::handle := CreatePen( nStyle,nWidth,nColor )
   ::style  := nStyle
   ::width  := nWidth
   ::color  := nColor
   Aadd( ::aPens, Self )

Return Self

METHOD Release() CLASS HPen
Local i, nlen := Len( ::aPens ), p

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i  in ::aPens 
         p := hb_EnumIndex()
         IF i:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aPens,p )
            Asize( ::aPens,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aPens[i]:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aPens,i )
            Asize( ::aPens,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil

//- HBrush

CLASS HBrush INHERIT HObject

   CLASS VAR aBrushes   INIT {}
   DATA handle
   DATA color
   DATA nHatch   INIT 99
   DATA nCounter INIT 1

   METHOD Add( nColor )
   METHOD Release()

ENDCLASS

METHOD Add( nColor,nHatch ) CLASS HBrush
Local i

   IF nHatch == Nil
      nHatch := 99
   ENDIF
   #ifdef __XHARBOUR__
   For EACH i IN ::aBrushes 
      IF i:color == nColor .AND. i:nHatch == nHatch
         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aBrushes )
      IF ::aBrushes[i]:color == nColor .AND. ::aBrushes[i]:nHatch == nHatch
         ::aBrushes[i]:nCounter ++
         Return ::aBrushes[i]
      ENDIF
   NEXT
   #endif
   IF nHatch != 99
      ::handle := CreateHatchBrush( nHatch,nColor )
   ELSE
      ::handle := CreateSolidBrush( nColor )
   ENDIF
   ::color  := nColor
   Aadd( ::aBrushes,Self )

Return Self

METHOD Release() CLASS HBrush
Local i, nlen := Len( ::aBrushes ), p

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i IN ::aBrushes 
         p := hb_enumindex()
         IF i:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aBrushes,p )
            Asize( ::aBrushes,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aBrushes[i]:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aBrushes,i )
            Asize( ::aBrushes,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil


//- HBitmap

CLASS HBitmap INHERIT HObject

   CLASS VAR aBitmaps   INIT {}
   DATA handle
   DATA name
   DATA nWidth, nHeight
   DATA nCounter   INIT 1

   METHOD AddResource( name )
   METHOD AddFile( name,hDC )
   METHOD AddWindow( oWnd,lFull )
   METHOD Release()

ENDCLASS

METHOD AddResource( name ) CLASS HBitmap
Local lPreDefined := .F., i, aBmpSize

   IF Valtype( name ) == "N"
      name := Ltrim( Str( name ) )
      lPreDefined := .T.
   ENDIF
   #ifdef __XHARBOUR__
   For EACH i  IN  ::aBitmaps 
      IF i:name == name
         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aBitmaps )
      IF ::aBitmaps[i]:name == name
         ::aBitmaps[i]:nCounter ++
         Return ::aBitmaps[i]
      ENDIF
   NEXT
   #endif
   // ::classname:= "HBITMAP"
   ::handle :=   LoadBitmap( Iif( lPreDefined, Val(name),name ) )
   ::name   := name
   aBmpSize  := GetBitmapSize( ::handle )
   ::nWidth  := aBmpSize[1]
   ::nHeight := aBmpSize[2]
   Aadd( ::aBitmaps,Self )

Return Self

METHOD AddFile( name, hDC ) CLASS HBitmap
Local i, aBmpSize

   #ifdef __XHARBOUR__
   For EACH i IN ::aBitmaps 
      IF i:name == name
         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aBitmaps )
      IF ::aBitmaps[i]:name == name
         ::aBitmaps[i]:nCounter ++
         Return ::aBitmaps[i]
      ENDIF
   NEXT
   #endif
   // ::classname:= "HBITMAP"
   ::handle :=   OpenBitmap( name, hDC )
   ::name := name
   aBmpSize  := GetBitmapSize( ::handle )
   ::nWidth  := aBmpSize[1]
   ::nHeight := aBmpSize[2]
   Aadd( ::aBitmaps,Self )

Return Self

METHOD AddWindow( oWnd,lFull ) CLASS HBitmap
Local i, aBmpSize

   // ::classname:= "HBITMAP"
   ::handle := Window2Bitmap( oWnd:handle,lFull )
   ::name := Ltrim( Str( oWnd:handle ) )
   aBmpSize  := GetBitmapSize( ::handle )
   ::nWidth  := aBmpSize[1]
   ::nHeight := aBmpSize[2]
   Aadd( ::aBitmaps,Self )

Return Self

METHOD Release() CLASS HBitmap
Local i, nlen := Len( ::aBitmaps ), p

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i IN ::aBitmaps
      p := hB_enumIndex()
         IF i:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aBitmaps,p )
            Asize( ::aBitmaps,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aBitmaps[i]:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aBitmaps,i )
            Asize( ::aBitmaps,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil


//- HIcon

CLASS HIcon INHERIT HObject

   CLASS VAR aIcons   INIT {}
   DATA handle
   DATA name
   DATA nCounter   INIT 1

   METHOD AddResource( name )
   METHOD AddFile( name,hDC )
   METHOD Release()

ENDCLASS

METHOD AddResource( name ) CLASS HIcon
Local lPreDefined := .F., i

   IF Valtype( name ) == "N"
      name := Ltrim( Str( name ) )
      lPreDefined := .T.
   ENDIF
   #ifdef __XHARBOUR__
   For EACH i IN ::aIcons 
      IF i:name == name
         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aIcons )
      IF ::aIcons[i]:name == name
         ::aIcons[i]:nCounter ++
         Return ::aIcons[i]
      ENDIF
   NEXT
   #endif
   // ::classname:= "HICON"
   ::handle :=   LoadIcon( Iif( lPreDefined, Val(name),name ) )
   ::name   := name
   Aadd( ::aIcons,Self )

Return Self

METHOD AddFile( name ) CLASS HIcon
Local i

#ifdef __XHARBOUR__
   For EACH i IN  ::aIcons 
      IF i:name == name
         i:nCounter ++
         Return i
      ENDIF
   NEXT
#else
   For i := 1 TO Len( ::aIcons )
      IF ::aIcons[i]:name == name
         ::aIcons[i]:nCounter ++
         Return ::aIcons[i]
      ENDIF
   NEXT
#endif
   // ::classname:= "HICON"
   ::handle := LoadImage( 0, name, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE+LR_LOADFROMFILE )
   ::name := name
   Aadd( ::aIcons,Self )

Return Self

METHOD Release() CLASS HIcon
Local i, nlen := Len( ::aIcons ), p

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i IN ::aIcons
      p:= hb_enumindex()
         IF i:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aIcons,p )
            Asize( ::aIcons,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aIcons[i]:handle == ::handle
            DeleteObject( ::handle )
            Adel( ::aIcons,i )
            Asize( ::aIcons,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil


EXIT PROCEDURE CleanDrawWidg
Local i

   For i := 1 TO Len( HPen():aPens )
      DeleteObject( HPen():aPens[i]:handle )
   NEXT
   For i := 1 TO Len( HBrush():aBrushes )
      DeleteObject( HBrush():aBrushes[i]:handle )
   NEXT
   For i := 1 TO Len( HFont():aFonts )
      DeleteObject( HFont():aFonts[i]:handle )
   NEXT
   For i := 1 TO Len( HBitmap():aBitmaps )
      DeleteObject( HBitmap():aBitmaps[i]:handle )
   NEXT
   For i := 1 TO Len( HIcon():aIcons )
      DeleteObject( HIcon():aIcons[i]:handle )
   NEXT

Return

