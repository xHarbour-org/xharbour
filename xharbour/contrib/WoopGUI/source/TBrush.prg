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
* Base Class - All windows and controls derive from this class
* NOT USE THIS CLASS DIRECTLY !!!
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

CLASS TBrush FROM TGDIObject

    CLASSDATA aoResources AS ARRAY   INIT {} HIDDEN

    DATA    oColor      AS OBJECT  HIDDEN
    //DATA    nStyle      AS NUMERIC HIDDEN
    //DATA    oBitmap     AS OBJECT  HIDDEN

    METHOD New()          CONSTRUCTOR
    METHOD GetColor()     INLINE  ::oColor:GetColor()
    //METHOD  GetStipple()   VIRTUAL
    //METHOD  GetStyle()     VIRTUAL
    //METHOD  Ok()           VIRTUAL
    //METHOD  SetColor()     VIRTUAL
    //METHOD  SetStipple()   VIRTUAL
    //METHOD  SetStyle()     VIRTUAL


    METHOD CreateResource()

ENDCLASS

METHOD New( nRGBColor ) CLASS TBrush

  ::oColor  := TColor():New( nRGBColor )
  ::SetEvalResource( {|o| o:oColor:nRGBColor == ::oColor:nRGBColor } )
  ::AddResource()
RETURN Self

METHOD CreateResource() CLASS TBrush
  ::nHandle := CreateSolidBrush( ::oColor:nRGBColor )
RETURN Self

EXIT PROCEDURE __WG_TBrush_Destroy()
   // Run directly releaseresource() without use delresource() because this make a scan for each
   WG_DebugTrace( "TBrush_Exit_Proc", "TBrush():aoResources", TBrush():aoResources )
   aEval( TBrush():aoResources, {|o| o:ReleaseResource() } )
RETURN
