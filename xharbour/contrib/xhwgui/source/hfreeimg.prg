/*
 * HWGUI - Harbour Win32 GUI library source code:
 * HFreeImage - Image handling class
 *
 * To use this class you need to have the FreeImage library
 * http://freeimage.sourceforge.net/
 * Authors: Floris van den Berg (flvdberg@wxs.nl) and
 *          Hervé Drolon (drolon@infonie.fr)
 *
 * Copyright 2003 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "HBClass.ch"
#include "windows.ch"
#include "guilib.ch"

CLASS HFreeImage INHERIT HObject

   CLASS VAR aImages   INIT {}
   DATA handle
   DATA hBitmap
   DATA name
   DATA nWidth, nHeight
   DATA nCounter   INIT 1

   METHOD AddFile( name )
   METHOD FromBitmap( oBitmap )
   METHOD Draw( hDC,nLeft,nTop,nWidth,nHeight )
   METHOD Release()

ENDCLASS

METHOD AddFile( name ) CLASS HFreeImage
Local i, aBmpSize

   #ifdef __XHARBOUR__
   For EACH i IN ::aImages
      IF i:name == name
         i:nCounter ++
         Return i
      ENDIF
   NEXT
   #else
   For i := 1 TO Len( ::aImages )
      IF ::aImages[i]:name == name
         ::aImages[i]:nCounter ++
         Return ::aImages[i]
      ENDIF
   NEXT
   #endif
   ::handle := FI_Load( name )
   // ::hBitmap := FI_2Bitmap( ::handle )
   ::name := name
   ::nWidth  := FI_GetWidth( ::handle )
   ::nHeight := FI_GetHeight( ::handle )
   Aadd( ::aImages,Self )

Return Self

METHOD FromBitmap( oBitmap )

   ::handle := FI_Bmp2FI( oBitmap:handle )
   ::name := Ltrim( Str( oBitmap:handle ) )
   ::nWidth  := FI_GetWidth( ::handle )
   ::nHeight := FI_GetHeight( ::handle )
   Aadd( ::aImages,Self )

Return Self

METHOD Draw( hDC,nLeft,nTop,nWidth,nHeight ) CLASS HFreeImage

   FI_Draw( ::handle, hDC, ::nWidth, ::nHeight, nLeft, nTop, nWidth, nHeight )
   // DrawBitmap( hDC, ::hBitmap,, nLeft, nTop, ::nWidth, ::nHeight )
Return Nil

METHOD Release() CLASS HFreeImage
Local i, nlen := Len( ::aImages ), p

   ::nCounter --
   IF ::nCounter == 0
   #ifdef __XHARBOUR__
      For EACH i IN ::aImages
      p := hB_enumIndex()
         IF i:handle == ::handle
            FI_Unload( ::handle )
            IF ::hBitmap != Nil
               DeleteObject( ::hBitmap )
            ENDIF
            Adel( ::aImages,p )
            Asize( ::aImages,nlen-1 )
            Exit
         ENDIF
      NEXT
   #else
      For i := 1 TO nlen
         IF ::aImages[i]:handle == ::handle
            FI_Unload( ::handle )
            IF ::hBitmap != Nil
               DeleteObject( ::hBitmap )
            ENDIF
            Adel( ::aImages,i )
            Asize( ::aImages,nlen-1 )
            Exit
         ENDIF
      NEXT
   #endif
   ENDIF
Return Nil

//- HSayFImage

CLASS HSayFImage INHERIT HSayImage

   METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight,Image,bInit, ;
                  bSize,ctoolt )
   METHOD Redefine( oWndParent,nId,Image,bInit,bSize,ctoolt )
   METHOD Paint( lpdis )

ENDCLASS

METHOD New( oWndParent,nId,nLeft,nTop,nWidth,nHeight,Image,bInit, ;
                  bSize,ctoolt ) CLASS HSayFImage

   ::oImage := Iif( Valtype(Image) == "C", HFreeImage():AddFile( Image ), Image )
   IF nWidth == Nil
      nWidth  := ::oImage:nWidth
      nHeight := ::oImage:nHeight
   ENDIF
   Super:New( oWndParent,nId,nLeft,nTop,nWidth,nHeight,bInit,bSize,ctoolt )
   // ::classname:= "HSAYFIMAGE"

   ::style   += SS_OWNERDRAW
   ::bPaint  := {|o,lpdis|o:Paint(lpdis)}

   ::Activate()

Return Self

METHOD Redefine( oWndParent,nId,Image,bInit,bSize,ctoolt ) CLASS HSayFImage

   ::oImage := Iif( Valtype(Image) == "C", HFreeImage():AddFile( Image ), Image )

   Super:Redefine( oWndParent,nId,bInit,bSize,ctoolt )
   // ::classname:= "HSAYFIMAGE"

   ::bPaint  := {|o,lpdis|o:Paint(lpdis)}

Return Self

METHOD Paint( lpdis ) CLASS HSayFImage
Local drawInfo := GetDrawItemInfo( lpdis )
Local hDC := drawInfo[3], x1 := drawInfo[4], y1 := drawInfo[5], x2 := drawInfo[6], y2 := drawInfo[7]

   ::oImage:Draw( hDC, 0, 0, ::nWidth, ::nHeight )

Return Self


EXIT PROCEDURE CleanImages
Local i

   For i := 1 TO Len( HFreeImage():aImages )
      FI_Unload( HFreeImage():aImages[i]:handle )
      IF HFreeImage():aImages[i]:hBitmap != Nil
         DeleteObject( HFreeImage():aImages[i]:hBitmap )
      ENDIF
   NEXT
   FI_End()

Return
