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
#include "windows.ch"

CLASS WG_TTimer FROM WG_TObject

   CLASSDATA aoTimers AS ARRAY INIT {}    HIDDEN
   DATA    oOwner   AS OBJECT             HIDDEN
   DATA    nCounter AS NUMERIC INIT 0     HIDDEN
   DATA    lOneShot AS LOGICAL INIT FALSE HIDDEN


   METHOD New( oOwner )       INLINE ( ::aData := {}, Self )
   METHOD Create()

   METHOD GetInterval()
   METHOD IsOneShot()
   METHOD IsRunning()
   METHOD Notify()
   METHOD SetOwner()
   METHOD Start()
   METHOD Stop()

   HIDDEN:
   METHOD AppendTimer()       INLINE aAdd( ::aoTimers, oTimer )
   METHOD DeleteTimer()
   METHOD FindTimer()

ENDCLASS

//----------------------------------------------------------------------------//

