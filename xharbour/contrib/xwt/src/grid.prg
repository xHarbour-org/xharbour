/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: grid.prg,v 1.1 2003/04/07 18:20:28 jonnymind Exp $

   Grid Pane class.
   The child objects are arranged in a fixed size grid,
   each cell being similar to a pane.
   Size of each row/col is determined by the largest widget
   in it.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTGrid FROM XWTLayContainer
   DATA nRows
   DATA nCols

   METHOD New( iRows, iCols )
   METHOD Attach( oChild, nFromRow, nFromCol, nRowWidth, nColWidth )
   METHOD Add( oChild )
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

   ::nRows := iRows
   ::nCols := iCols
   XWT_SetProperty( ::oRawWidget, XWT_PROP_COLROWS, iCols, iRows )
RETURN Self

METHOD Attach( oChild, nFromRow, nFromCol, nWidth, nHeight ) CLASS XWTGrid
   LOCAL bRes

   bRes := XWT_FastRiseEvent( XWT_E_ADDCHILD, Self, oChild )

   IF bRes
      IF ValType( nWidth ) != 'N'
         nWidth = 1
      ENDIF

      IF ValType( nHeight ) != 'N'
         nHeight = 1
      ENDIF

      bRes := XWT_SetProperty( ::oRawWidget, XWT_PROP_ATTACH, ;
         { oChild, nFromRow, nFromCol, nWidth, nHeight } )
      IF bRes
         AAdd( oChildren, oChild )
         oChild:oOwner := Self
      ENDIF
   ENDIF

RETURN bRes


METHOD Add( oChild ) CLASS XWTGrid
RETURN ::Attach( oChild, 1, 1, ::nRows, ::nCols )
