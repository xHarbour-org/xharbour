
//#DEFINE SHOWDEBUG
//#Include "debug.ch"

//#define   MAKELONG(nLow, nHigh)   ((nLow) + (nHigh) * 65536)
//#define   MAKEWORD(nLow, nHigh)   ((nLow) + (nHigh) * 256)

#INCLUDE "hbclass.ch"
#INCLUDE 'windows.ch'
//#Include 'header.ch'
#INCLUDE "what32.ch"
#INCLUDE "WinGdi.ch"

#XCOMMAND ? ? < x > = > OutputDebugString( asString( < x > ) )
#XCOMMAND ? < x > = > OutputDebugString( asString( < x > ) + chr( 13 ) )


*-----------------------------------------------------------------------------*

CLASS whColumn

   DATA Width
   DATA Block
   DATA fgColor
   DATA BgColor
   DATA Align
   DATA VertAlign
   DATA FONT
   DATA BmpIndex
   DATA BmpAlign
   DATA Offset
   DATA bOnClick

   DATA Heading
   DATA HeadAlign
   DATA HeadBmpIndex
   DATA HeadBmpAlign
   DATA HeadOffset
   DATA bHeadOnClick

   DATA bKillBlock
   DATA bOnChar
   DATA bSaveBlock

   DATA OriginalWidth
   DATA OriginalIndex

   DATA ImageList
   DATA cargo
   DATA style

   METHOD Init( )
   METHOD Kill( )

ENDCLASS


*-----------------------------------------------------------------------------*

METHOD Init( ;
             cHeading  , ; // title
             bBlock    , ; // data retrieval block
             nAlign    , ; // data align
             nWidth    , ; // data width
             fColor    , ; // data color
             bColor    , ; // data background color
             hFont     , ; // data font
             bhBlock   , ; // header click block
             hHeadFont , ; // header font
             hFgColor  , ; // header text color
             hAlign    , ; // header align
             hBmpIndex , ; // header bitmap
             hBmpAlign , ; // header bmp align
             dBmpIndex , ; // data bitmap
             dBmpAlign , ; // data bitmap align
             ImageList , ; // image list for data pictures
             textoffset, ; // amount by which to move the text to accommodate the image, or just move the text for the sake of moving it
             nStyle ;
           )
   DEFAULT textoffset TO 2
   ::Width      := IF( nWidth == NIL, Max( 10, IF( ValType( cHeading ) == "C", 10 * Len( cHeading ) , 0 ) ) , nWidth )
   ::Block      := IF( ValType( bBlock ) <> "B", { || "" } , bBlock )
   ::fgColor    := IF( ValType( fColor ) $ "BN", fColor, )
   ::BgColor    := IF( ValType( bColor ) $ "BN", bColor, )
   ::Align      := IF( ValType( nAlign ) <> "N", TA_LEFT, nAlign )
   ::VertAlign  := TA_TOP
   ::FONT       := hFont
   ::BmpIndex   := dBmpIndex
   ::BmpAlign   := IF( ValType( dBmpAlign ) <> "N", TA_LEFT, dBmpAlign )
   ::Offset     := textoffset

   ::Heading       := IF( ValType( cHeading ) <> "C", "", cHeading )
   ::HeadAlign     := IF( ValType( hAlign ) <> "N", TA_LEFT, hAlign )
   ::HeadBmpIndex  := hBmpIndex
   ::HeadBmpAlign  := TA_LEFT
   ::HeadOffset    := 2

   ::bHeadOnClick  := bhBlock
   ::style         := IF( ValType( nStyle ) <> "N", TBC_MOVE + TBC_SIZE + TBC_READWRITE, nStyle )

   ::OriginalWidth := ::Width
   ::OriginalIndex := 0

   ::ImageList := ImageList

   RETURN( self )


*-----------------------------------------------------------------------------*

METHOD Kill( )

   IF ValType( ::bKillBlock ) == "B"
      eval( ::bKillBlock )
   ENDIF

   RETURN( NIL )


