/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"

// Windows definitions
CLASS TPartHandler

    DATA cargo    EXPORTED     // Any type, for ad-hoc use

    DATA nNameID  AS NUMERIC   // Numeric ID of object
    DATA nHandle  AS NUMERIC   // Handle of the object

    DATA aoChilds AS ARRAY  INIT {} HIDDEN   // Array of Child Objects
    DATA oParent  AS OBJECT         HIDDEN   // Parent object - Phisical parent object
    DATA oOwner   AS OBJECT         HIDDEN   // Owner object  - Logical parent object
    DATA nStatus  AS NUMERIC        HIDDEN   // Object status

    METHOD New() CONSTRUCTOR           // initializes an instance of class
    METHOD Create()                    // Requests system resources
    METHOD Configure()                 // Reconfigures the object after system resources have been allocated
    METHOD Destroy()                   // Releases system resources
    METHOD HandleEvent()  VIRTUAL      // Processes events
    METHOD Status()                    // Determines the status of an object.

    METHOD AddChild()                  // Defines a part as a child.
    METHOD ChildFromName()             // Returns a part from the child list based on a numeric ID
    METHOD ChildList()    INLINE ::aoChilds  // Returns the child list of the Part.
    METHOD DelChild()                  // Deletes a Part from the child list
    METHOD SetName()                   // Defines or retrieves a numeric ID associated with self
    METHOD SetOwner()                  // Sets or returns the owner of the Part
    METHOD SetParent()                 // Sets or returns the parent of the Part

ENDCLASS

METHOD New( oParent AS OBJECT, oOwner AS OBJECT ) CLASS TPartHandler

    ASSIGN ::oParent WITH oParent
    ASSIGN ::oOwner  WITH oOwner

RETURN Self

METHOD Create( oParent AS OBJECT, oOwner AS OBJECT ) CLASS TPartHandler

    ASSIGN ::oParent WITH oParent
    ASSIGN ::oOwner  WITH oOwner

    // nothing to do, it's virtual

RETURN Self

METHOD Configure( oParent AS OBJECT, oOwner AS OBJECT ) CLASS TPartHandler

    IF oParent <> NIL THEN ::SetParent( oParent )
    IF oOwner <> NIL  THEN ::SetOwner( oOwner )

    // nothing to do, it's virtual

RETURN Self

METHOD Destroy() CLASS TPartHandler

    aEval( ::aoChilds, {|o| o:Destroy() } )

    ::cargo     := NIL
    ::nHandle   := NIL
    ::aoChilds  := {}
    ::oParent   := NIL
    ::oOwner    := NIL
    ::nStatus   := NIL

RETURN Self

// Constant          Description
//
//  WOG_STAT_INIT     Part object is initialized
//  WOG_STAT_CREATE   The :create() method was called successfully
//  WOG_STAT_FAILURE  The :create() method failed to obtain system resources
//
METHOD Status() CLASS TPartHandler
RETURN ::nStatus

METHOD AddChild( oChild AS OBJECT ) CLASS TPartHandler
   aAdd( ::aoChilds, oChild )
RETURN Self

METHOD ChildFromName( nID AS NUMERIC ) CLASS TPartHandler
   LOCAL oChild
   LOCAL nPos, n
   LOCAL aoChilds := ::aoChilds
   // Search child in direct child array
   nPos := aScan( aoChilds, {|o| o:nNameID == nID } )
   IF nPos == 0
      // Search if is a child of child objects
      FOR n := 1 TO Len( aoChilds )
          nPos := aScan( aoChilds[n]:ChildList(), {|o| o:nNameID == nID } )
          IF nPos > 0
             oChild := aoChilds[n]:ChildList()[nPos]
             EXIT
          ENDIF
      NEXT n
   ELSE
      oChild := aoChilds[nPos]
   ENDIF
RETURN oChild

METHOD DelChild( oChild AS OBJECT ) CLASS TPartHandler
   LOCAL nPos
   nPos := aScan( ::aoChilds, {|o| o:nNameID == oChild:nNameID } )
   IF nPos > 0
      ::aoChilds[nPos]:Destroy()
      ::aoChilds := aShrink( ::aoChilds, nPos )
      // ::aoChilds := aDel( ::aoChilds, nPos )
      // ::aoChilds := aSize( ::aoChilds, Len( ::aoChilds ) - 1 )
   ENDIF
RETURN Self

METHOD SetName( nNewNameID AS NUMERIC ) CLASS TPartHandler
  LOCAL nOldNameID := ::nNameID
  ASSIGN ::nNameID WITH nNewNameID
RETURN nOldNameID

METHOD SetOwner( oNewOwner AS OBJECT ) CLASS TPartHandler
  LOCAL oOldOwner := ::oOwner
  ASSIGN ::oOwner WITH oNewOwner
RETURN oOldOwner

METHOD SetParent( oNewParent AS OBJECT ) CLASS TPartHandler
  LOCAL oOldParent := ::oParent
  IF oNewParent <> NIL
     ::oParent := oNewParent
     // code to change ownership relation with API SetParent( oChild, oParent )
     SetParent( ::nHandle, ::oParent:nHandle )
  ENDIF
RETURN oOldParent

