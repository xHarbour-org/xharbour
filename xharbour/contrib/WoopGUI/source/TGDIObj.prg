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

// This is a virtual class - don't use directly

CLASS WG_TGDIObject FROM WG_TObject

    //CLASSDATA aoReGDI       AS ARRAY INIT {} // Array of gdi object
    DATA nHandle          AS NUMERIC       // Handle
    DATA nResources       AS NUMERIC  INIT 0     // Number of gdi resource
    DATA bEvalResource    AS CODEBLOCK
    //DATA cType            AS STRING        // Type of gdi


    //DATA lUpdated         AS LOGICAL FALSE

    METHOD  New() CONSTRUCTOR
    //METHOD  ReleaseResource()          VIRTUAL

    METHOD  AddResource()              HIDDEN
    METHOD  CreateResource()           VIRTUAL
    METHOD  DelResource()              HIDDEN
    METHOD  FindResource()             HIDDEN
    METHOD  ReleaseResource()          HIDDEN
    METHOD  SetEvalResource()          HIDDEN
    METHOD  FindResourceByEval()       HIDDEN


    //ON ERROR ErrorHandler( oError )

ENDCLASS

METHOD New() CLASS WG_TGDIObject
    // Nothing to do
RETURN Self

/*
METHOD AddResource( oGDI, bEval ) CLASS WG_TGDIObject
   LOCAL nPos := ::FindResource( oGDI:nResource )

   WG_ParamDisplay( Self, hb_aparams(), "TGDIObject" )

   mESSAGEbOX( , "N. risorse = " + cStr( ::nResources ) + " Pos = " + cstr( nPos ) )

   IF nPos == 0
      aAdd( ::aoResources, oGDI )
      ::nHandle := oGDI:nHandle
   ENDIF
   ::nResources++

RETURN oGDI:nHandle
*/

METHOD SetEvalResource( bEval ) CLASS WG_TGDIObject
  ::bEvalResource := bEval
RETURN Self

METHOD AddResource() CLASS WG_TGDIObject
  LOCAL nPos := ::FindResourceByEval()

  IF nPos > 0
     Self := ::aoResources[ nPos ]
  ELSE
     ::CreateResource()
  ENDIF
  ::nResources++

RETURN Self

METHOD FindResourceByEval() CLASS WG_TGDIObject
RETURN aScan( ::aoResources, ::bEvalResource )

METHOD DelResource() CLASS WG_TGDIObject
  LOCAL nPos
  ::nResources--
  IF ::nResources == 0
     nPos := ::FindResource()
     IF nPos > 0
        ::ReleaseResource()
        WG_aShrink( ::aoResources, nPos )
     //ELSE
     //   MessageBox( , "Brush not found!", "Error" )
     ENDIF
  ENDIF
RETURN NIL

METHOD FindResource() CLASS WG_TGDIObject
RETURN aScan( ::aoResources, {|o| o:nHandle == ::nHandle } )

METHOD ReleaseResource() CLASS WG_TGDIObject
RETURN DeleteObject( ::nHandle )
