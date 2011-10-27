/*
 * $Id$
 */

//-----------------------------------------------------------------------------------------------
// Copyright   WinFakt! / SOCS BVBA  http://www.WinFakt.com
//
// This source file is an intellectual property of SOCS bvba.
// You may NOT forward or share this file under any conditions!
//-----------------------------------------------------------------------------------------------

#include "debug.ch"
#include "vxh.ch"
#include "hbxml.ch"

#define  acObjectTypePicture        7

//-----------------------------------------------------------------------------------------------

CLASS VrImage INHERIT VrObject
   DATA OnePerPage       EXPORTED INIT .F.
   DATA ClsName          EXPORTED INIT "Image"
   DATA FileName         EXPORTED INIT ""

   DATA Opacity          EXPORTED INIT 100
   DATA Alignment        EXPORTED INIT 1
   DATA KeepAspectRatio  EXPORTED INIT .F.
   
   DATA __ExplorerFilter EXPORTED
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD Draw()
   METHOD WriteProps()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrImage
   IF oParent != NIL
      Super:Init( oParent )
      AADD( ::aProperties, { "Name",     "Object"  } )
      AADD( ::aProperties, { "FileName", "General" } )
      AADD( ::aProperties, { "Width",  "Size"  } )
      AADD( ::aProperties, { "Height", "Size" } )
   ENDIF
RETURN Self

METHOD Create() CLASS VrImage

   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
   WITH OBJECT ::EditCtrl := __VrImage( ::Parent )
      :Cargo     := Self
      :ImageName := ::FileName
      :Left      := ::Left
      :Top       := ::Top
      :BackColor := RGB(255,255,255)
      :Create()
   END
   ::__ExplorerFilter := ::EditCtrl:__ExplorerFilter

   Super:Create()
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrImage
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "FileName", NIL, ::FileName )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Left", NIL, XSTR( ::Left ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Width", NIL, XSTR( ::Width ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Height", NIL, XSTR( ::Height ) )
   oXmlControl:addBelow( oXmlValue )
RETURN Self

METHOD Draw( hDC ) CLASS VrImage
   local nX, nY, x, y, cx, cy, cName := "Image" + AllTrim( Str( ::Parent:nImage++ ) )
   nX := GetDeviceCaps( hDC, LOGPIXELSX )
   nY := GetDeviceCaps( hDC, LOGPIXELSY )

   x  := ( ::nPixPerInch / nX ) * ::Left
   y  := ::Parent:nRow + ( ( ::nPixPerInch / nY ) * ::Top )
   cx := ( ::nPixPerInch / nX ) * ::Width
   cy := ( ::nPixPerInch / nY ) * ::Height
   WITH OBJECT ::Parent:oPDF
      :CreateObject( acObjectTypePicture,  cName )
      ::PDFCtrl := :GetObjectByName( cName )
      WITH OBJECT ::PDFCtrl
         :Attribute( "FileName", ::FileName )
         :Attribute( "Left",     x )
         :Attribute( "Top",      y )
         :Attribute( "Right",    x + cx )
         :Attribute( "Bottom",   y + cy )
      END
   END
RETURN NIL


//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS __VrImage INHERIT FreeImage
   DATA aSize EXPORTED INIT {.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.}
   METHOD OnLButtonDown()
   METHOD OnMouseMove(n,x,y) INLINE MouseMove( Self, n, x, y )
   METHOD OnMouseLeave()     INLINE ::Parent:Cursor := NIL, NIL
   #ifndef VRDLL
      METHOD OnKeyDown(n)       INLINE KeyDown( Self, n )
   #endif
ENDCLASS

METHOD OnLButtonDown(n,x,y) CLASS __VrImage 
   LOCAL aRect, oCtrl
   ::Parent:SetCapture()
   IF ::Application:Props:PropEditor:ActiveObject != NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      TRY
         IF oCtrl != NIL
            aRect := oCtrl:GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            oCtrl:Parent:InvalidateRect( aRect, .F. )
            aRect := ::GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            ::Parent:InvalidateRect( aRect, .F. )
            ::Parent:nDownPos := {x,y}
             AINS( ::Application:Report:aUndo, 1, { { oCtrl:Left, oCtrl:Top, oCtrl:Width, oCtrl:Height }, "__MOVESIZE", oCtrl }, .T. )
            ::Application:Props:EditUndoBttn:Enabled := .T.
         ENDIF
      CATCH
      END
   ENDIF
   Super:OnLButtonDown()
RETURN NIL
