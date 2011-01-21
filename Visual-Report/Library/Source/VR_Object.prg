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
#define PIX_PER_INCH   1440


#define  acObjectTypeText           5

CLASS VrObject
   DATA Font        PUBLISHED

   DATA nImage      PROTECTED INIT 1
   DATA nText       PROTECTED INIT 1
   DATA nLine       PROTECTED INIT 1
   DATA nBox        PROTECTED INIT 1
   
   DATA lUI         EXPORTED INIT .T.
   DATA Name        EXPORTED
   DATA Text        EXPORTED

   DATA Left        EXPORTED INIT 0
   DATA Top         EXPORTED INIT 0
   DATA Width       EXPORTED INIT 150
   DATA Height      EXPORTED INIT 150

   DATA Alignment   EXPORTED INIT 0
   DATA EnumAlignment EXPORTED INIT { { "No Alignment", "Left", "Center", "Right" }, {0,1,2,3} }

   DATA Application EXPORTED
   DATA System      EXPORTED
   DATA Parent      EXPORTED
   DATA ClsName     EXPORTED
   DATA __ClsInst   EXPORTED
   DATA aProperties INIT { { "Left", "Position" },;
                           { "Top",  "Position" },;
                           { "Alignment",  "Position" } }
   ACCESS EditMode  INLINE ::__ClsInst != NIL
   ACCESS __xCtrlName INLINE ::ClsName
   
   DATA EditCtrl    EXPORTED
   DATA PDFCtrl     EXPORTED
   
   DATA Report      EXPORTED
   METHOD Init() CONSTRUCTOR
   METHOD GetValue( cVal ) INLINE ::&cVal
   METHOD SetControlName()
   METHOD Create()
   METHOD SetSize()
   METHOD Draw()           VIRTUAL
   METHOD FillRect()
   METHOD MoveWindow()     INLINE ::EditCtrl:MoveWindow( ::Left, ::Top )
   METHOD nLogPixelX()     INLINE PIX_PER_INCH
   METHOD nLogPixelY()     INLINE PIX_PER_INCH
   METHOD WriteProps()     VIRTUAL
ENDCLASS

METHOD Init( oParent ) CLASS VrObject
   ::Application := __GetApplication()
   ::System      := __GetSystem()
   ::Parent      := oParent
   IF ::Parent != NIL
      ::SetControlName()
   ENDIF
   ::Font := Font()
RETURN Self

METHOD Create() CLASS VrObject
   IF ::Parent != NIL
      AADD( ::Parent:Objects, Self )
      IF ::lUI
         ::EditCtrl:OnWMLButtonDown := {|| ::Application:Props:PropEditor:ResetProperties( {{ Self }} ) }
      ENDIF
   ENDIF
RETURN Self

METHOD SetControlName() CLASS VrObject
   LOCAL cProp, n := 1
   IF ::__ClsInst != NIL
      WHILE .T.
         cProp := ::ClsName + XSTR( n )
         IF ASCAN( ::Parent:Objects, {|o| UPPER(o:Name) == UPPER(cProp) } ) == 0
            EXIT
         ENDIF
         n ++
      ENDDO
      ::Name := cProp
      ::Text := cProp
   ENDIF
RETURN n

METHOD SetSize( cx, cy ) CLASS VrObject
   LOCAL aRect
   DEFAULT cx TO ::Width
   DEFAULT cy TO ::Height
   
   WITH OBJECT ::EditCtrl
      :Parent:nDownPos := {0,0}
      aRect := :GetRectangle()
      aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
      :Parent:InvalidateRect( aRect, .F. )

      :xWidth  := cx
      :xHeight := cy
      :MoveWindow()

      :Parent:nDownPos := NIL
      aRect := :GetRectangle()
      aRect := {aRect[1]-1, aRect[2]-1, aRect[3]+1, aRect[4]+1}
      :Parent:InvalidateRect( aRect, .F. )
   END
RETURN Self

METHOD FillRect( aRect, cColor ) CLASS VrObject
   LOCAL cName := "Box" + AllTrim( Str( ::nBox++ ) )

   WITH OBJECT ::Parent:oPDF
      :CreateObject( acObjectTypeText, cName )
      :ObjectAttribute( cName, "BackColor", IIF( cColor != NIL, cColor, PADL( DecToHexa( ::BackColor ), 6, "0" ) ) )

      :ObjectAttribute( cName, "Left",   aRect[1] )
      :ObjectAttribute( cName, "Top",    aRect[2] )
      :ObjectAttribute( cName, "Right",  aRect[3] )
      :ObjectAttribute( cName, "Bottom", aRect[4] )
   END
RETURN Self

