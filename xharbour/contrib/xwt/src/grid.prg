/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: grid.prg,v 1.3 2003/04/08 18:21:46 jonnymind Exp $

   Grid Pane class.
   The child objects are arranged in a fixed size grid,
   each cell being similar to a pane.
   Size of each row/col is determined by the largest widget
   in it.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTGrid FROM XWTLayContainer
   METHOD New( iRows, iCols )
   METHOD Attach( oChild, nFromRow, nFromCol, nRowWidth, nColWidth )
   METHOD Add( oChild )
   METHOD SetPadding( nRowPad, nColPad )
   METHOD SetDimension( nRow, nCol )
   METHOD GetDimension( nRow, nCol )
ENDCLASS

METHOD New(iRows, iCols ) CLASS XWTGrid
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_GRID )

   IF iRows < 1
      iRows := 1
   ENDIF

   IF iCols < 1
      iCols := 1
   ENDIF

  XWT_SetProperty( ::oRawWidget, XWT_PROP_COLROWS, iCols, iRows )
RETURN Self

METHOD Attach( oChild, nFromRow, nFromCol, nHeight, nWidth ) CLASS XWTGrid
   LOCAL bRes

   IF ValType( oChild ) != "O"
      ? "ERROR"
   ENDIF

   bRes := .not. XWT_FastRiseEvent( XWT_E_ADDCHILD, Self, oChild )

   IF bRes
      IF ValType( nWidth ) != 'N'
         nWidth = 1
      ENDIF

      IF ValType( nHeight ) != 'N'
         nHeight = 1
      ENDIF

      bRes := XWT_SetProperty( ::oRawWidget, XWT_PROP_ATTACH, ;
               { oChild:oRawWidget, nFromRow, nFromCol, nHeight, nWidth} )
      IF bRes
         AAdd( ::aChildren, oChild )
         oChild:oOwner := Self
      ENDIF
   ENDIF

RETURN bRes


METHOD Add( oChild ) CLASS XWTGrid
RETURN ::Attach( oChild, 1, 1, ::nRows, ::nCols )

METHOD SetPadding( nRowPad, nColPad ) CLASS XWTGrid
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_PADDING, nRowPad, nColPad )


METHOD SetDimension( nRows, nCols ) CLASS XWTGrid
RETURN XWT_SetProperty( ::oRawWidget, XWT_PROP_COLROWS, nCols, nRows )

METHOD GetDimension( nRows, nCols ) CLASS XWTGrid
RETURN XWT_GetProperty( ::oRawWidget, XWT_PROP_COLROWS, nCols, nRows )
