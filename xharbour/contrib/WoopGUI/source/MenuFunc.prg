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
#include "windows.ch"

// -----------------------------------------------------

//STATIC soWnd            // Parent Window Object of Menu
STATIC soStack
STATIC snID

FUNCTION WG_BeginContextMenu()
   LOCAL oPopUp

   WG_BeginMenuBar()
   oPopUp := WG_BeginMenuPopup( "__Dummymenu" )

RETURN oPopUp

FUNCTION WG_BeginMenuBar()
   LOCAL oMenuBar

   // Reset static values for new menu
   soStack   := TStack():New()
   //soWnd     := oWnd
   IF snID == NIL THEN snID := 5000  // ID automatically generated
                                     // user can define own id under 5000

   oMenuBar := TMenuBar():New()
   soStack:Push( oMenuBar )    //oMenuBar

RETURN oMenuBar

FUNCTION WG_BeginMenuPopup( cCaption )
   LOCAL oMenuBarItem, oMenuPopup

   oMenuBarItem := TMenuItem():New(MF_POPUP, snID++, cCaption )
   soStack:Push( oMenuBarItem )  // oMenuBarItem
   oMenuPopup := TMenuPopup():New()
   soStack:Push( oMenuPopup ) // oMenuPopup

RETURN oMenuPopup

FUNCTION WG_DefineMenuItem( cCaption, bAction, lChecked, nShortcut, cMsg, nId,;
	                         lDisabled, lGrayed, bBfAction, bAfAction, lOnBuildMenu  )
   LOCAL oMenuItem
   DEFAULT lOnBuildMenu TO TRUE

   DEFAULT nId TO snID++

   oMenuItem := TMenuItem():New(, nID, cCaption, bAction, lChecked, lDisabled, lGrayed, cMsg )
   IF lOnBuildMenu THEN soStack:Tail():Append( oMenuItem ) // oMenuPopup

RETURN oMenuItem

FUNCTION WG_DefineMenuItemSeparator( lOnBuildMenu )
   LOCAL oMenuItem
   DEFAULT lOnBuildMenu TO TRUE

   oMenuItem := TMenuItem():Separator() // New( MF_SEPARATOR )
   IF lOnBuildMenu THEN soStack:Tail():Append( oMenuItem ) // oMenuPopup

RETURN oMenuItem

PROCEDURE WG_EndMenuPopup()
   LOCAL oMenuBarItem, oMenuPopup

   oMenuPopup   := soStack:Pop() // oMenuPopup
   oMenuBarItem := soStack:Pop() // oMenuBarItem
   oMenuBarItem:SetID( oMenuPopup )
   soStack:Tail():Append( oMenuBarItem ) // oMenuBar
   oMenuPopup:ReparentChilds()

RETURN

PROCEDURE WG_EndMenuBar()
   LOCAL oMenuBar
   oMenuBar := soStack:Pop() // oMenuBar

   oMenuBar:ReparentChilds()

   //soWnd:SetMenu( a )

RETURN

PROCEDURE WG_EndContextMenu()
   WG_EndMenuPopup()
   WG_EndMenuBar()

RETURN

