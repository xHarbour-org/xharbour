/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: gridpane.prg,v 1.1 2003/04/02 00:56:38 jonnymind Exp $

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
ENDCLASS

METHOD New(iRows, iCols, iPadding ) CLASS XWTGrid
   ::Super:New()
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_GRID )
RETURN Self

METHOD Attach( oChild, nFromRow, nFromCol, nWidth, nHeight ) CLASS XWTGrid
   bRes := ::Super:Add( oChild )

   IF bRes
      IF ValType( nWidth ) != 'N'
         nWidth = 1
      ENDIF

      IF ValType( nHeight ) != 'N'
         nHeight = 1;
      ENDIF

   ENDIF
   
RETURN bRes
