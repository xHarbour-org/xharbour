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
RETURN Self

METHOD Create() CLASS VrLabel
   DEFAULT ::Font TO Font()
   ::Font:Create()

   IF ::__ClsInst == NIL // Runtime
      RETURN ::Draw()
   ENDIF
   
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
            aSize := :Drawing:GetTextExtentPoint32( ::Text )
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

METHOD WriteProps( cBuffer ) CLASS VrLabel
   cBuffer += "   oCtrl:Text           := " + ValToPrgExp( ::Text )           + CRLF
   cBuffer += "   oCtrl:ForeColor      := " + ValToPrgExp( ::ForeColor )      + CRLF
   cBuffer += "   oCtrl:BackColor      := " + ValToPrgExp( ::BackColor )      + CRLF
   cBuffer += "   oCtrl:Font:FaceName  := " + ValToPrgExp( ::Font:FaceName )  + CRLF
   cBuffer += "   oCtrl:Font:PointSize := " + ValToPrgExp( ::Font:PointSize ) + CRLF
   cBuffer += "   oCtrl:Font:Italic    := " + ValToPrgExp( ::Font:Italic )    + CRLF
   cBuffer += "   oCtrl:Font:Underline := " + ValToPrgExp( ::Font:Underline ) + CRLF
   cBuffer += "   oCtrl:Font:Weight    := " + ValToPrgExp( ::Font:Weight )    + CRLF
   cBuffer += "   oCtrl:Left           := " + XSTR( ::Left ) + CRLF
   cBuffer += "   oCtrl:Top            := " + XSTR( ::Top ) + CRLF
RETURN Self

METHOD Draw() CLASS VrLabel
   LOCAL x, y, cUnderline, cItalic, cName := "Text" + AllTrim( Str( ::nText++ ) )

   x  := ( ::nLogPixelX() / 72 ) * ::Left
   y  := ::Parent:nRow + ( ( ::nLogPixelY() / 72 ) * ::Top )
 
   IF ::Text != NIL
      cItalic    := IIF( ::Font:Italic, "1", "0" )
      cUnderline := IIF( ::Font:Underline, "1", "0" )

      WITH OBJECT ::Parent:oPDF
         :CreateObject( acObjectTypeText, cName )
         ::PDFCtrl := :GetObjectByName( cName )
         WITH OBJECT ::PDFCtrl
            :Attribute( "AutoResize", 1 )
            :Attribute( "Single Line", 1 )
            :Attribute( "Left",   x )
            :Attribute( "Top",    y )
            :Attribute( "TextFont", Alltrim( ::Font:FaceName ) + "," +Alltrim( Str( ::Font:PointSize ) ) + "," + Alltrim( Str( ::Font:Weight ) ) +","+cItalic+","+cUnderline )
            :Attribute( "Text",   ::Text )
            IF ::ForeColor != ::SysForeColor
               :Attribute( "TextColor", PADL( DecToHexa( ::ForeColor ), 6, "0" ) )
            ENDIF
            IF ::BackColor != ::SysBackColor
               :Attribute( "BackColor", PADL( DecToHexa( ::BackColor ), 6, "0" ) )
            ENDIF
            :Refresh()
         END
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
