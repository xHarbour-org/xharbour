/*
 * $Id$
 */

#include "vxh.ch"
#include "debug.ch"
#include "colors.ch"
#include "fileio.ch"

#define XFM_EOL Chr(13) + Chr(10)

#define PBS_NORMAL       1
#define PBS_HOT          2
#define PBS_PRESSED      3
#define PBS_DISABLED     4
#define PBS_DEFAULTED    5
#define BP_PUSHBUTTON    1

#define TMT_BTNTEXT   1619

#ifdef VXH_ENTERPRISE
   #define VXH_PROFESSIONAL
#endif

//-----------------------------------------------------------------------------------------------

CLASS CustomControl INHERIT Control
   DATA Reference PUBLISHED
   DATA __ChgRef  PRIVATE

   PROPERTY ImageList GET __ChkComponent( Self, ::xImageList )

   METHOD Init()    CONSTRUCTOR
   METHOD Create()
ENDCLASS

//-----------------------------------------------------------------------------------------------

METHOD Init( oParent, cReference ) CLASS CustomControl
   DEFAULT ::__xCtrlName TO "CustomControl"
   ::ClsName := "CCTL"
   ::Super:Init( oParent )
   ::__IsStandard := .F.
   ::Style   := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ExStyle := WS_EX_NOACTIVATE | WS_EX_CONTROLPARENT
   ::Width   := 200
   ::Height  := 200
   ::__ChgRef := cReference
RETURN Self

METHOD Create() CLASS CustomControl
   #ifdef VXH_PROFESSIONAL
    LOCAL hFile, nLine, aChildren, cLine, cFile, aErrors, aEditors, nLeft, nTop, cName, cReference, nWidth, nHeight
   #endif
   ::Super:Create()

   #ifdef VXH_PROFESSIONAL
   IF ::__ClassInst != NIL
      IF ::__ChgRef != NIL
         ::Reference := ::__ChgRef
         ::__ChgRef := NIL
         ::Application:Project:Modified := .T.
         ::Form:__lModified := .T.
      ENDIF
      IF ASCAN( ::Application:Project:CustomControls, ::Reference,,, .T. ) == 0
         AADD( ::Application:Project:CustomControls, ::Reference )
      ENDIF
      IF !EMPTY( ::Reference )
         nLeft   := ::Left
         nTop    := ::Top
         nWidth  := ::Width
         nHeight := ::Height
         cName   := ::Name
         cReference := ::Reference

         hFile       := FOpen( ::Reference, FO_READ )
         nLine       := 1
         aChildren   := {}
         aErrors     := {}

         ::SetRedraw( .F. )
         WHILE HB_FReadLine( hFile, @cLine, XFM_EOL ) == 0
            ::Application:Project:ParseXFM( cLine, hFile, @aChildren, cFile, @nLine, @aErrors, @aEditors, Self, .T. )
            nLine++
         END

         FClose( hFile )
         ::__DockChildren := ::Children

         __ResetClassInst( Self )

         ::Reference := cReference
         ::__ClassInst:Reference := ""

         ::xLeft   := nLeft
         ::xTop    := nTop
         ::xWidth  := nWidth
         ::xHeight := nHeight

         ::SetRedraw( .T. )
         MoveWindow( ::hWnd, ::xLeft, ::xTop, ::xWidth, ::xHeight, .F. )
         ::Application:DoEvents()

         ::Name   := cName
      ENDIF

   ENDIF
   #endif
RETURN Self

