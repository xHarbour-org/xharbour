/*
   XWT - xHarbour Windowing Toolkit

   (C) 2003 Giancarlo Niccolai

   $Id: inputmask.prg,v 1.1 2003/04/12 23:47:15 jonnymind Exp $

   This is an "input" mask. It has a grid, an Ok and a Cancel button.
   When the OK button is pressed, the variables are automagically updated,
   and if the cancel is pressed they are restored to their original value.

   The buttons can be also be displayed elsewhere.

   This widget does not require low-level driver implementation.

*/

#include "hbclass.ch"
#include "xwt.ch"

CLASS XWTInputMask FROM XWTLayout
   DATA oPanel
   DATA oGrid
   DATA oOkButton
   DATA oCancelButton
   DATA oStatus
   DATA aInputFields
   DATA aBoxes

   METHOD New( aInputFields, bDisplayBtn )
   METHOD Commit()
   METHOD Cancel()
   METHOD Destroy()
ENDCLASS


METHOD New( aInputFields, bDisplayBtn ) CLASS XWTInputMask
   LOCAL hLay  // where to put the ok/cancel buttons
   LOCAL oBox, nCount
   LOCAL nFields := Len( aInputFields )
   LOCAL nRows
   
   ::Super:New( XWT_LM_VERT )

   IF Empty( bDisplayBtn )
      bDisplayBtn := .T.
   ENDIF

   nRows := IIF ( bDisplayBtn, nFields + 1, nFields )

   ::oGrid := XWTGrid():New( nRows, 2 )
   ::oOkButton := XWTButton():New( "Ok" )
   ::oOkButton:AddEventListener( XWT_E_CLICKED, Self, "Commit" )
   ::oCancelButton := XWTButton():New( "Cancel" )
   ::oCancelButton:AddEventListener( XWT_E_CLICKED, Self, "Cancel" )

   // building the grid
   ::aBoxes := Array( Len( aInputFields ) )
   FOR nCount := 1 TO nFields
      // Saving the original variable values
      // variable data is stored in the second field
      ::oGrid:Attach( XWTLabel():New( aInputFields[nCount][1] ), nCount, 1 )
      // TODO: Using a picture text box
      oBox := XWTTextbox():New( aInputFields[nCount][2] )
      oBox:bEventsToParent := .F.
      ::oGrid:Attach( oBox, nCount, 2 )
      ::aBoxes[ nCount ] := oBox
   NEXT

   ::aInputFields := aInputFields

   ::Add( ::oGrid )

   // Setting buttons if needed
   IF bDisplayBtn
      hLay := XWTLayout( XWT_LM_HORIZ )
      hLay:Add( ::oOkButton )
      hLay:Add( ::oCancelButton )
      ::Add( hLay )
      hLay:Show()
   ENDIF

RETURN Self

METHOD Destroy() CLASS XWTInputMask
   IF ::oOkButton:oOwner == NIL
      ::oOkButton:Destroy()
   ENDIF

   IF ::oCancelButton:oOwner == NIL
      ::oCancelButton:Destroy()
   ENDIF

RETURN ::Super:Destroy()


//THIS METHODS are mainly meant to be overloaded
METHOD Commit() CLASS XWTInputMask
   LOCAL nCount

   FOR nCount := 1 TO Len( ::aInputFields )
      ::aInputFields[ nCount ][2] := ::aBoxes[ nCount ]:GetText()
   NEXT

   XWT_FastRiseEvent( XWT_E_CHANGED, Self )
RETURN .T.


METHOD Cancel() CLASS XWTInputMask
   XWT_FastRiseEvent( XWT_E_CANCELED, Self )
RETURN .T.
