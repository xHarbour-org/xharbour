/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: treelist.prg,v 1.2 2003/06/08 14:05:33 jonnymind Exp $

   XWT bColumn - A tbrowse like class for XWT (single column control).

*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTBColumn
   DATA cHeading
   DATA bGetItem             // Code block to retrieve data for the column
   DATA bSetItem             // Code block to set the data
   DATA xCargo               // User-definable variable
   DATA nWidth
   DATA bVisible  INIT .T.
   DATA bEditable INIT .T.
   DATA cColor    INIT "#000000"
   DATA cBackground INIT "#FFFFFF"
   DATA cHighlight INIT "darkblue"

   METHOD New( cHead, xCargo, bGetItem ) CONSTRUCTOR

ENDCLASS


METHOD New( cHead, xCargo, bGetItem )
   ::cHeading := cHead
   ::xCargo := xCargo
   IF bGetItem == NIL
      IF xCargo != NIL
         ::bGetItem := { | nId | nId := ::xCargo[ nId ] }
      ENDIF
   ELSE
      ::bGetItem := bGetItem
   ENDIF

RETURN Self

