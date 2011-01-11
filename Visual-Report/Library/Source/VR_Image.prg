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

#define  acObjectTypePicture        7

//-----------------------------------------------------------------------------------------------

CLASS VrImage INHERIT VrObject
   DATA ClsName          EXPORTED INIT "Image"
   DATA FileName         EXPORTED INIT ""

   DATA Opacity          EXPORTED INIT 100
   DATA Alignment        EXPORTED INIT 1
   DATA KeepAspectRatio  EXPORTED INIT .F.
   
   DATA __ExplorerFilter EXPORTED
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD Draw()
ENDCLASS

//-----------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS VrImage
   Super:Init( oParent )
   AADD( ::aProperties, { "Name",     "Object"  } )
   AADD( ::aProperties, { "FileName", "General" } )

   AADD( ::aProperties, { "Opacity", "General" } )
   AADD( ::aProperties, { "Alignment", "General" } )
   AADD( ::aProperties, { "KeepAspectRatio", "General" } )

   AADD( ::aProperties, { "Width",  "Size"  } )
   AADD( ::aProperties, { "Height", "Size" } )
RETURN Self

METHOD Create() CLASS VrImage

   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
   WITH OBJECT ::EditCtrl := __VrImage( ::Parent )
      :ImageName := ::FileName
      :Left      := ::Left
      :Top       := ::Top
      :BackColor := RGB(255,255,255)
      :Create()
   END
   ::__ExplorerFilter := ::EditCtrl:__ExplorerFilter

   Super:Create()
RETURN Self

METHOD Draw( x, y, cx, cy ) CLASS VrImage
   local cName := "Image" + AllTrim( Str( ::nImage++ ) )
   WITH OBJECT ::Report:oPDF
      :CreateObject( acObjectTypePicture,  cName )
      :ObjectAttribute( cName, "FileName", ::FileName )
      :ObjectAttribute( cName, "Left",     x )
      :ObjectAttribute( cName, "Top",      y )
      :ObjectAttribute( cName, "Right",    x + cx )
      :ObjectAttribute( cName, "Bottom",   y + cy )
   END
RETURN NIL


//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------

CLASS __VrImage INHERIT FreeImage
   METHOD OnLButtonDown()
ENDCLASS

METHOD OnLButtonDown(n,x,y) CLASS __VrImage
   LOCAL aRect, oCtrl
   ::Parent:SetCapture()
   IF ::Application:Props:PropEditor:ActiveObject != NIL
      oCtrl := ::Application:Props:PropEditor:ActiveObject:EditCtrl
      aRect := oCtrl:GetRectangle()
      aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
      oCtrl:Parent:InvalidateRect( aRect, .F. )
      aRect := ::GetRectangle()
      aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
      ::Parent:InvalidateRect( aRect, .F. )
      ::Parent:nDownPos := {x,y}
   ENDIF
   Super:OnLButtonDown()
RETURN NIL
