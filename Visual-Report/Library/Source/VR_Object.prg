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
   PROPERTY Name SET ::SetControlName(v)
   DATA Font        PUBLISHED

   DATA lUI         EXPORTED INIT .T.
   DATA Text        EXPORTED
   DATA Objects     EXPORTED INIT {}

   DATA Left        EXPORTED INIT 0
   DATA Top         EXPORTED INIT 0
   DATA Width       EXPORTED INIT 150
   DATA Height      EXPORTED INIT 150
   DATA ParName     EXPORTED INIT ""
   DATA ParCls      EXPORTED INIT ""
   DATA Alignment   EXPORTED INIT 0
   DATA EnumAlignment EXPORTED INIT { { "No Alignment", "Left", "Center", "Right" }, {0,1,2,3} }

   ACCESS Application INLINE __GetApplication()
   ACCESS System      INLINE __GetSystem()

   DATA System      EXPORTED


   DATA Parent      EXPORTED
   DATA ClsName     EXPORTED
   DATA __ClassInst EXPORTED
   DATA aProperties INIT { { "Left", "Position" },;
                           { "Top",  "Position" },;
                           { "Alignment",  "Position" } }
   ACCESS EditMode  INLINE ::__ClassInst != NIL
   ACCESS __xCtrlName INLINE ::ClsName

   DATA EditCtrl    EXPORTED
   DATA PDFCtrl     EXPORTED

   DATA Report      EXPORTED
   DATA nPixPerInch EXPORTED INIT PIX_PER_INCH
   DATA bCreate

   METHOD Init() CONSTRUCTOR
   METHOD GetValue( cVal ) INLINE ::&cVal
   METHOD SetControlName()
   METHOD Create()
   METHOD SetSize()
   METHOD Draw()           VIRTUAL
   METHOD FillRect()
   METHOD MoveWindow()     INLINE ::EditCtrl:MoveWindow( ::Left, ::Top )
   METHOD WriteProps()     VIRTUAL
   METHOD Configure()      VIRTUAL
   METHOD Delete()
   METHOD __GetDataSource()
   METHOD GetProps()
ENDCLASS

METHOD Init( oParent ) CLASS VrObject
   ::Parent      := oParent
   IF ::Parent != NIL
      ::SetControlName()
   ENDIF
   ::Font := Font( Self )
RETURN Self

METHOD __GetDataSource( cDataSource ) CLASS VrObject
   LOCAL n, oSource
   IF ( n := ASCAN( ::Objects, {|o| o:Name == cDataSource} ) ) > 0
      oSource := ::Objects[n]
   ENDIF
RETURN oSource

METHOD Create() CLASS VrObject
   IF ::Parent != NIL
      AADD( IIF( ::lUI, ::Parent:Objects, ::Application:Props:CompObjects ), Self )
      IF ::lUI
         ::EditCtrl:OnWMLButtonDown := {|| ::Application:Props:PropEditor:ResetProperties( {{ Self }} ) }
      ENDIF
   ENDIF
RETURN Self

METHOD Delete() CLASS VrObject
   LOCAL n := ASCAN( ::Parent:Objects, Self,,, .T. )
   IF n > 0
      ADEL( ::Parent:Objects, n, .T. )
      ::EditCtrl:Destroy()
      ::Application:Props:Components:Children[1]:Select():SelectComponent()
      ::Parent:InvalidateRect()
   ELSE
      n := ASCAN( ::Application:Props:CompObjects, Self,,, .T. )
      IF n > 0
         ADEL( ::Application:Props:CompObjects, n, .T. )
         ::EditCtrl:Destroy()
         ::Application:Props:Components:Children[1]:Select():SelectComponent()
         ::Parent:InvalidateRect()
      ENDIF
   ENDIF

   TRY
      ::Button:Delete()
   CATCH
   END
   n := ASCAN( ::Application:aNames, ::Name,,, .T. )
   IF n > 0
      ADEL( ::Application:aNames, n, .T. )
   ENDIF
   ::Application:Props:EditCopyBttn:Enabled := .F.
   ::Application:Props:EditCutBttn:Enabled  := .F.
   ::Application:Props:EditDelBttn:Enabled  := .F.

RETURN Self

METHOD GetProps() CLASS VrObject
   LOCAL xVal, n, cProp, aProps := {}
   FOR n := 1 TO LEN( ::aProperties )
       cProp := ::aProperties[n][1]
       xVal  := ::&cProp
       AADD( aProps, { cProp, xVal } )
   NEXT
   AINS( aProps, 1, { "Parent", ::Parent }, .T. )
   AINS( aProps, 1, { "ClsName", ::ClassName }, .T. )
RETURN aProps

METHOD SetControlName( cProp ) CLASS VrObject
   LOCAL n := 1
   IF UPPER( ::Parent:ClassName ) != "VRREPORT" .AND. __objHasMsg( ::Application, "aNames" )
      IF !EMPTY(cProp) .AND. !EMPTY(::xName)
         IF UPPER(cProp) == UPPER(::xName)
            RETURN Self
         ENDIF
         IF ( n := ASCAN( ::Application:aNames, ::xName,,, .T. ) ) > 0
            ADEL( ::Application:aNames, n, .T. )
         ENDIF
      ENDIF
      IF cProp == NIL
         n := 1
         WHILE .T.
            cProp := ::ClsName + XSTR( n )
            IF ASCAN( ::Application:aNames, cProp,,, .T. ) == 0
               EXIT
            ENDIF
            n ++
         ENDDO
      ENDIF
      AADD( ::Application:aNames, cProp )
      ::xName := cProp
      IF Empty( ::Text )
         ::Text := cProp
      ENDIF
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

FUNCTION KeyDown( oCtrl, nKey )
   LOCAL lShift, aRect, nMove := 1, lMod := .T.
   IF oCtrl != NIL .AND. oCtrl:Cargo != NIL
      IF oCtrl:Cargo:lUI
         aRect := oCtrl:GetRectangle()
         lShift := CheckBit( GetKeyState( VK_SHIFT ) , 32768 )
         IF CheckBit( GetKeyState( VK_CONTROL ) , 32768 )
            nMove := 8
         ENDIF
         aRect[1]-=(nMove+4)
         aRect[2]-=(nMove+4)
         aRect[3]+=(nMove+4)
         aRect[4]+=(nMove+4)
      ENDIF
      IF nKey == VK_DELETE
         oCtrl:Cargo:Delete()
       ELSEIF nKey == VK_LEFT
         IF lShift .AND. oCtrl:aSize[2]
            oCtrl:Cargo:Width -= nMove
            oCtrl:Width -= nMove
          ELSE
            oCtrl:Cargo:Left -= nMove
            oCtrl:Left -= nMove
         ENDIF
       ELSEIF nKey == VK_UP
         IF lShift .AND. oCtrl:aSize[8]
            oCtrl:Cargo:Height -= nMove
            oCtrl:Height -= nMove
          ELSE
            oCtrl:Cargo:Top -= nMove
            oCtrl:Top -= nMove
         ENDIF
       ELSEIF nKey == VK_RIGHT
         IF lShift .AND. oCtrl:aSize[6]
            oCtrl:Cargo:Width += nMove
            oCtrl:Width += nMove
          ELSE
            oCtrl:Cargo:Left += nMove
            oCtrl:Left += nMove
         ENDIF
       ELSEIF nKey == VK_DOWN
         IF lShift .AND. oCtrl:aSize[4]
            oCtrl:Cargo:Height += nMove
            oCtrl:Height += nMove
          ELSE
            oCtrl:Cargo:Top += nMove
            oCtrl:Top += nMove
         ENDIF
       ELSE
         lMod := .F.
      ENDIF
      IF lMod
         IF !oCtrl:Application:Report:Modified
            oCtrl:Application:Report:Modified := .T.
         ENDIF
         IF oCtrl:Cargo:lUI
            oCtrl:Parent:InvalidateRect( aRect, .T.)
          ELSEIF oCtrl:Cargo:Button != NIL
            oCtrl:Cargo:Button:Delete()
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

