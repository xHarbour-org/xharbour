/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: treelist.prg,v 1.2 2003/06/08 14:05:33 jonnymind Exp $

   XWT Browse - A tbrowse like class for XWT.

*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTBrowse FROM XWTWidget
   DATA aColumns  INIT {}
   DATA nRows     INIT 0

   METHOD New()
   METHOD CountColumns()   INLINE   Len( ::aColumns )

   METHOD AddColumn( oCol ) INLINE AAdd( ::aColumns, oCol )
   METHOD RemoveColumn( iPos ) INLINE ADel( ::aColumns, iPos ), aSize( ::aColumns, Len(::aColumns) -1 )
   METHOD InsertColumn( iPos, oCol ) INLINE ASize( ::aColumns, Len( ::aColumns ) +1) , AIns( ::aColumns, iPos, oCol )
   METHOD GetColumn( iPos ) INLINE ::aColumns[ iPos ]

   METHOD CountColumns()   INLINE Len( ::aColumns )

   METHOD Stabilize()   INLINE XWT_SetProperty( ::oRawWidget, XWT_PROP_UPDATE, .T. )
ENDCLASS


METHOD New( nLenght ) CLASS XWTBrowse
   ::Super:New()
   ::nWidgetType := XWT_TYPE_BROWSE
   ::oRawWidget := XWT_Create( Self, XWT_TYPE_BROWSE )
   IF HB_IsNum( nLenght )
      ::nRows := nLenght
   ENDIF
RETURN Self



