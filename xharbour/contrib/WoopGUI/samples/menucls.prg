/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
*
*/

// Sample of use menu directly with classes

#include "woopgui.ch"
#include "common.ch"
#include "windows.ch"

function main

   local oApp, oWnd1
   local oMenuBar

   oApp := tApplication():New()
   oApp:Create()

   oWnd1 := TFrame():New("Sample Menu with Class")
   oWnd1:SetStatusBar("Sample Form window - Status bar")

   oMenuBar := DefineMenu()

   oWnd1:SetMenu( oMenuBar )

   oWnd1:Show()

   oApp:Activate()
   oApp:Quit()

return (0)


STATIC FUNCTION DefineMenu()
  LOCAL oMenuBar, oMenuPopup
  LOCAL oMBi, oMPi

   // New Menu Bar
   oMenuBar := TMenuBar():New()
     oMBi  := TMenuItem():New(MF_POPUP, 100,"&File")

       // New popup menu
       oMenuPopup := TMenuPopup():New()
       // Add Items to Popup
       oMPi  := TMenuItem():New(,101,"&New ...", {|| NothingToDo() } )
       oMenuPopup:Append( oMPi )
       oMPi  := TMenuItem():New(,102,"&Open" )
       oMenuPopup:Append( oMPi )
       oMPi  := TMenuItem():New(,103,"&Save" )
       oMenuPopup:Append( oMPi )
       oMPi  := TMenuItem():New(,104,"&Save as ..." )
       oMenuPopup:Append( oMPi )
       oMPi  := TMenuItem():New(MF_SEPARATOR )
       oMenuPopup:Append( oMPi )
       oMPi  := TMenuItem():New(,105,"&Exit", {|oW| oW:Destroy() } )
       oMenuPopup:Append( oMPi )

     oMBi:Set( oMenuPopup )
   oMenuBar:Append( oMBi )

     oMBi  := TMenuItem():New(MF_POPUP,200,"&Modify")
        // New popup menu
        oMenuPopup := TMenuPopup():New()
        // Add Items to Popup
        oMPi  := TMenuItem():New(,201,"&Cut" )
        oMenuPopup:Append( oMPi )
        oMPi  := TMenuItem():New(,201,"&Copy" )
        oMenuPopup:Append( oMPi )
        oMPi  := TMenuItem():New(,201,"&Paste" )
        oMenuPopup:Append( oMPi )

     oMBi:Set( oMenuPopup )
   oMenuBar:Append( oMBi )

     oMBi  := TMenuItem():New(MF_POPUP,300,"&?")
        // New popup menu
        oMenuPopup := TMenuPopup():New()
        // Add Items to Popup
        oMPi  := TMenuItem():New(,301,"&About", ;
                 {||MessageBox(0, "WoopGUI - A Win32 OOP GUI for Harbour"+CRLF+;
                                  "(C) Francesco Saverio Giudice - 2002");
                 } )
        oMenuPopup:Append( oMPi )

     oMBi:Set( oMenuPopup )
   oMenuBar:Append( oMBi )

RETURN oMenuBar

PROCEDURE NothingToDo()
   MessageBox(0, "This Item do nothing !" )
RETURN
