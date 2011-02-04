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

#define  acObjectTypeText           5

CLASS VrLabel INHERIT VrObject
   PROPERTY Text     READ xText WRITE SetText
   DATA ClsName      EXPORTED  INIT "Label"
   DATA SysBackColor EXPORTED  INIT GetSysColor( COLOR_WINDOW )
   DATA SysForeColor EXPORTED  INIT GetSysColor( COLOR_BTNTEXT )
   DATA Data         EXPORTED  INIT ""
   DATA BackColor    PUBLISHED INIT GetSysColor( COLOR_WINDOW )
   DATA ForeColor    PUBLISHED INIT GetSysColor( COLOR_BTNTEXT )
   METHOD Init()  CONSTRUCTOR
   METHOD Create()
   METHOD SetText()
   METHOD Draw()
   METHOD WriteProps()
   METHOD Configure()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent ) CLASS VrLabel
   IF oParent != NIL
      Super:Init( oParent )
      AADD( ::aProperties, { "Name",      "Object"  } )
      AADD( ::aProperties, { "Font",      "General" } )
      AADD( ::aProperties, { "Text",      "General" } )
      AADD( ::aProperties, { "BackColor", "Color"   } )
      AADD( ::aProperties, { "ForeColor", "Color"   } )
   ENDIF
   DEFAULT ::Font TO Font()
   ::Font:AllowHandle := oParent != NIL
RETURN Self

METHOD Create() CLASS VrLabel
   DEFAULT ::Font TO Font()
   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
   ::Font:Create()

   WITH OBJECT ::EditCtrl := __VrLabel( ::Parent )
      :Caption := ::Text
      :Left    := ::Left
      :Top     := ::Top
      :Create()
   END
   ::Font:Set( ::EditCtrl )

   Super:Create()
   ::SetText( ::xText )
RETURN Self

METHOD Configure() CLASS VrLabel
   WITH OBJECT ::EditCtrl
      :Caption        := ::Text
      :ForeColor      := ::ForeColor     
      :BackColor      := ::BackColor     
      :Font:FaceName  := ::Font:FaceName 
      :Font:PointSize := ::Font:PointSize
      :Font:Italic    := ::Font:Italic   
      :Font:Underline := ::Font:Underline
      :Font:Weight    := ::Font:Weight   
   END
   ::SetText( ::xText )
RETURN Self

METHOD SetText( cText ) CLASS VrLabel
   LOCAL aSize, aRect
   IF ::EditCtrl != NIL
      WITH OBJECT ::EditCtrl
         IF :hWnd != NIL 
            aRect := :GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            :Parent:InvalidateRect( aRect, .F. )

            IF VALTYPE( cText ) == "C"
               SetWindowText( :hWnd, cText )
            ENDIF
            
            aSize := :Drawing:GetTextExtentPoint32( cText )
            :xWidth := aSize[1]+4
            :xHeight := aSize[2]+2
            :MoveWindow()

            aRect := :GetRectangle()
            aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
            :Parent:InvalidateRect( aRect, .F. )
         ENDIF
      END
   ENDIF
RETURN Self

METHOD WriteProps( oXmlControl ) CLASS VrLabel
   LOCAL oXmlValue, oXmlFont
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Text", NIL, ::Text )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "ForeColor", NIL, XSTR( ::ForeColor ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "BackColor", NIL, XSTR( ::BackColor ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Left", NIL, XSTR( ::Left ) )
   oXmlControl:addBelow( oXmlValue )
   oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Top", NIL, XSTR( ::Top ) )
   oXmlControl:addBelow( oXmlValue )

   oXmlFont := TXmlNode():new( , "Font" )
      oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "FaceName", NIL, XSTR( ::Font:FaceName ) )
      oXmlFont:addBelow( oXmlValue )
      oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "PointSize", NIL, XSTR( ::Font:PointSize ) )
      oXmlFont:addBelow( oXmlValue )
      oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Italic", NIL, IIF( ::Font:Italic, "True", "False" ) )
      oXmlFont:addBelow( oXmlValue )
      oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Underline", NIL, IIF( ::Font:Underline, "True", "False" ) )
      oXmlFont:addBelow( oXmlValue )
      oXmlValue := TXmlNode():new( HBXML_TYPE_TAG, "Weight", NIL, XSTR( ::Font:Weight ) )
      oXmlFont:addBelow( oXmlValue )
   oXmlControl:addBelow( oXmlFont )
RETURN Self

METHOD Draw() CLASS VrLabel
   LOCAL x, y, cUnderline, cText, cItalic, cName := "Text" + AllTrim( Str( ::Parent:nText++ ) )

   x  := ( ::nLogPixelX() / 72 ) * ::Left
   y  := ::Parent:nRow + ( ( ::nLogPixelY() / 72 ) * ::Top )
 
   IF ::Text != NIL
      cItalic    := IIF( ::Font:Italic, "1", "0" )
      cUnderline := IIF( ::Font:Underline, "1", "0" )

      ::Parent:oPDF:CreateObject( acObjectTypeText, cName )
      ::PDFCtrl := ::Parent:oPDF:GetObjectByName( cName )
      WITH OBJECT ::PDFCtrl
         :Attribute( "AutoResize", 1 )
         :Attribute( "Single Line", 1 )
         :Attribute( "Left",   x )
         :Attribute( "Top",    y )
         :Attribute( "TextFont", Alltrim( ::Font:FaceName ) + "," +Alltrim( Str( ::Font:PointSize ) ) + "," + Alltrim( Str( ::Font:Weight ) ) +","+cItalic+","+cUnderline )
         TRY
            cText := &(::Text)
         catch
            cText := ::Text
         END
         :Attribute( "Text", ALLTRIM( cText ) )
         IF ::ForeColor != ::SysForeColor
            :Attribute( "TextColor", PADL( DecToHexa( ::ForeColor ), 6, "0" ) )
         ENDIF
         IF ::BackColor != ::SysBackColor
            :Attribute( "BackColor", PADL( DecToHexa( ::BackColor ), 6, "0" ) )
         ENDIF
      END
   ENDIF
RETURN Self

CLASS __VrLabel INHERIT Label
   METHOD OnLButtonDown()
ENDCLASS

METHOD OnLButtonDown(n,x,y) CLASS __VrLabel 
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
         ENDIF
      CATCH
      END
   ENDIF
   Super:OnLButtonDown()
RETURN NIL

FUNCTION DecToHexa(nNumber)
   local cNewString := ""
   local nTemp := 0
   WHILE nNumber > 0
      nTemp      := nNumber % 16
      cNewString := SubStr( "0123456789ABCDEF", (nTemp+1), 1 ) + cNewString
      nNumber    := Int( (nNumber-nTemp)/16 )
   ENDDO
RETURN cNewString
