/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: grid.prg,v 1.5 2003/05/11 15:14:43 jonnymind Exp $

   Grid Pane class.
   The child objects are arranged in a fixed size grid,
   each cell being similar to a pane.
   Size of each row/col is determined by the largest widget
   in it.
*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTGrid FROM XWTContainer
   METHOD New( xProps, oParent ) CONSTRUCTOR
   METHOD Attach( oChild, nFromRow, nFromCol, nRowWidth, nColWidth )
   METHOD Add( oChild )
ENDCLASS

/* properties:
   column
   rows
   colpadding
   rowpadding
*/

METHOD New( xProps, oParent ) CLASS XWTGrid
   ::nWidgetType := XWT_TYPE_GRID
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_GRID )
   ::Super:New( xProps, oParent )

   IF oParent != NIL
      oParent:Add( Self )
   ENDIF
RETURN Self

METHOD Attach( oChild, nFromRow, nFromCol, nHeight, nWidth ) CLASS XWTGrid
   LOCAL bRes

   IF ValType( oChild ) != "O"
      Alert( "XWTGrid_Attach: only object are allowed" )
      RETURN
   ENDIF

   bRes := .not. XWT_FastRiseEvent( "addchild", Self, oChild )

   IF bRes
      IF ValType( nWidth ) != 'N'
         nWidth = 1
      ENDIF

      IF ValType( nHeight ) != 'N'
         nHeight = 1
      ENDIF

      bRes := XWT_SetProperty( ::oRawWidget, "attach", ;
               { oChild:oRawWidget, nFromRow, nFromCol, nHeight, nWidth} )
      IF bRes
         AAdd( ::aChildren, oChild )
         oChild:oOwner := Self
      ENDIF
   ENDIF

RETURN bRes


METHOD Add( oChild ) CLASS XWTGrid
   LOCAL nRows
   LOCAL nCols
   
   nRows := ::GetProperty( "rows" )
   nCols := ::GetProperty( "columns" )
RETURN ::Attach( oChild, 1, 1, nRows, nCols )
