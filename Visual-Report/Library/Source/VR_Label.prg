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

#define PIX_PER_INCH   1440

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
   METHOD nLogPixelX()    INLINE PIX_PER_INCH
   METHOD nLogPixelY()    INLINE PIX_PER_INCH
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

METHOD Draw( x, y, cx, cy ) CLASS VrLabel
   LOCAL cUnderline, cItalic, cName := "Text" + AllTrim( Str( ::nText++ ) )
   DEFAULT x TO ::Left
   DEFAULT y TO ::Top
   DEFAULT cx TO ::Width
   DEFAULT cy TO ::Height

   x  := ( ::nLogPixelX() / 72 ) * x
   y  := ::Parent:nRow + ( ( ::nLogPixelY() / 72 ) * y )
   cx := 0//( ::nLogPixelX() / 72 ) * cx
   cy := 0//( ::nLogPixelY() / 72 ) * cy
 

   IF ::Text != NIL
      //IF ::BackColor != ::SysBackColor
      //   ::FillRect( { x, y-30, x + cx, y + cy + 30}, PADL( DecToHexa( ::BackColor ), 6, "0" ) ) //"FFFFFF" )
      //ENDIF

      cItalic    := IIF( ::Font:Italic, "1", "0" )
      cUnderline := IIF( ::Font:Underline, "1", "0" )

      WITH OBJECT ::Parent:oPDF
         :CreateObject( acObjectTypeText, cName )
         ::PDFCtrl := :GetObjectByName( cName )
         WITH OBJECT ::PDFCtrl
            :Attribute( "AutoResize", 1 )  // see above slowness note
            :Attribute( "Single Line", 1 )
            :Attribute( "Left",   x )
            :Attribute( "Top",    y )
            :Attribute( "Right",  x+cx )
            :Attribute( "Bottom", y+cy )
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
