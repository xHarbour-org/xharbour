/*
 * $Id: FormEdit.Prg,v 1.97 2002/11/19 20:23:20 what32 Exp $
 */
/*
 * xHarbour Project source code:
 * xIDE FormEditor
 *
 * Copyright 2002 Andy Wos [andrwos@aust1.net] Augusto Infante [systems@quesoro.com] Ron Pinkas [ron@ronpinkas.com]
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 */

#include "windows.ch"
#include "wingdi.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#Include "cstruct.ch"
#include "toolbar.ch"
#include "debug.ch"

#define XFM_Header   1
#define XFM_Class    2
#define XFM_Datas    3
#define XFM_Events   4
#define XFM_Controls 5
#define XFM_EndClass 6
#define XFM_Methods  7
#define XFM_Footer   8
#define XFM_Size     XFM_Footer

GLOBAL EXTERNAL hThisInstance
GLOBAL EXTERNAL Application
GLOBAL EXTERNAL MainForm
GLOBAL EXTERNAL FormEdit

GLOBAL EXTERNAL ObjTree
GLOBAL EXTERNAL ObjInspect
GLOBAL EXTERNAL ObjEdit

GLOBAL EXTERNAL InspCombo
GLOBAL EXTERNAL InspTree

/*
   For performance NOT using OS indpendant R/T function,
   this file only used in win32.
 */
#undef CRLF
#define CRLF Chr(13) + Chr(10)

//----------------------------------------------------------------------------//

CLASS TFormEdit FROM TForm

   CLASSDATA nForms  PROTECTED INIT 0
   CLASSDATA aBases  PROTECTED INIT {0}

   DATA oMask        PROTECTED
   DATA Editing      PROTECTED
   DATA aCtrl        PROTECTED
   DATA nCurrent     PROTECTED
   DATA GridColor    PROTECTED
   DATA xGrid        PROTECTED
   DATA yGrid        PROTECTED
   DATA hBmpGrid     PROTECTED
   DATA xBmpSize     PROTECTED
   DATA yBmpSize     PROTECTED
   DATA CtrlName     PROTECTED
   DATA aXFM         PROTECTED INIT Array( XFM_Size )

   METHOD Create()

   METHOD WMCreate()
   METHOD WMSize()
   METHOD WMMove()           INLINE ObjInspect:SetBrowserData( HB_QSelf(), .T. ), NIL
   METHOD WMPaint()
   METHOD WMDestroy()

   METHOD OnMenuCommand(oItem)

   METHOD CreateControl(aRect)
   METHOD GetUniquePropName()
   METHOD SetControl( oObj )
   METHOD SetGridSize(x,y)

   METHOD XFMTop()
   METHOD XFMRoot()
   METHOD XFMControl( cName, oControl, lDelete )
   METHOD XFMReflect()
   METHOD XFMTrace()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD Create( oParent ) CLASS TFormEdit 
   LOCAL oBtn
   LOCAL xRet
   Local oTree

   ::Super:Create( oParent )
   
   ::FCaption := 'TForm' + LTrim( Str( ++::nForms ) )
   ::Editing  := .F.
   ::aCtrl    := {}
   ::FLeft    := 200
   ::FTop     := 125
   ::FWidth   := 600
   ::FHeight  := 400
   ::xGrid    := 8
   ::yGrid    := 8
   ::GridColor:=RGB(0,0,0)
   ::OnCloseQuery := {|| MainForm:StdBar:Disable(), InspCombo:DelObject( HB_QSELF() ), NIL }

   FormEdit := Self
   
   ::XFMTop()
   ::SetParent( MainForm )

RETURN Self

//----------------------------------------------------------------------------//
METHOD WMCreate() CLASS TFormEdit 

   LOCAL oBtn
   LOCAL xRet
   Local oTree

   ::SetGridSize()

   MainForm:StdBar:Enable()
   MainForm:Win32Bar:Enable()

   ::oMask:=oCtrlMask():Create( self )
   ::oMask:SetParent( Self )

   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

   InspCombo:AddString( ::Name + chr(9) + "TFORM", self)
  
   oTree := InspTree:Add( ::Name,, ::Fhandle )
   oTree:Select()


   ::oMask:Hint:= ""
   ::oMask:HintWindow:SetMaxTipWidth(10)

RETURN NIL

//--------------------------------------------------------------------------------------------

METHOD WMSize( nFlag, Width, Height) CLASS TFormEdit 

   LOCAL aRect

   IF ::oMask != NIL
      aRect := GetClientRect(::FHandle)
      
      ::oMask:FLeft   := aRect[1]
      ::oMask:FTop    := aRect[2]
      ::oMask:FWidth  := aRect[3]
      ::oMask:FHeight := aRect[4]
      ::oMask:Move( , , , , .T. )
      
      ::oMask:Refresh()
      ObjInspect:SetBrowserData( Self, .T. )
   ENDIF

RETURN 0

//----------------------------------------------------------------------------//

METHOD WMDestroy() CLASS TFormEdit 

   IF !Empty(::hBmpGrid)
      DeleteObject(::hBmpGrid)
   ENDIF

   RETURN(Super:WMDestroy())

//----------------------------------------------------------------------------//

METHOD OnMenuCommand(oItem) CLASS TFormEdit 

   // check for the range of commands which create controls
   // otherwise process the command as usual

   IF .T. //command if range of control creation commands

      // store the control type in memory
      ::CtrlName := oItem:Hint // control type to be created
      // this cursor shape must be correct for the type of the control
      ::oMask:Cursor :=LoadCursor(hThisInstance,"FRAMES") //<= correct cursor for the control

   ELSE

      ::CtrlName := NIL
      ::oMaskCursor:=NIL

      // process command
      DO CASE


      ENDCASE


   ENDIF



RETURN(NIL)

//----------------------------------------------------------------------------//

// note !
// all controls must have default sizes !!!!!
// please check Tab control

METHOD CreateControl( aRect ) CLASS TFormEdit

   EXTERN TButton, TEdit, TLabel, TCheckBox, TRadioButton,;
          TComboBox, TListBox, TStatusBar, TTabControl,;
          TGroupBox, TTreeView, TCoolBar, TToolBar, TProgressBar

   LOCAL oObj, oBand
   LOCAL nInd:=0
   LOCAL cName, cClass, n

   IF ! Empty( ::CtrlName )

      IF Empty(aRect)
         aRect:={10,10,NIL,NIL}
      ENDIF

      cName:=::GetUniquePropName(::CtrlName)

      cClass := "T" + AllTrim( ::CtrlName )
      n := HB_FuncPtr( cClass )
      oObj := HB_Exec( n )
      
      DO CASE
         CASE ::CtrlName == "StatusBar"
            IF ("2" $ cName) // only one status bar
               RETURN NIL
            ENDIF
         CASE ::CtrlName == "CoolBar"
            IF ("2" $ cName) // only one CoolBar
               RETURN NIL
            ENDIF
      ENDCASE

      IF oObj != NIL

         IF ::CtrlName == "CoolBar"
            aRect[1] := 0
            aRect[2] := 0
         ENDIF

         oObj:Create( Self )
         oObj:FCaption := oObj:Name
         oObj:FLeft    := aRect[1]
         oObj:FTop     := aRect[2]
         oObj:SetParent( Self )

         IF ::CtrlName == "CoolBar"
            oBand := TCoolBand():Create( oObj )
            oBand:MinHeight := 28
         ENDIF
         
         ::SetControl( oObj )
         
      ENDIF
   ENDIF

   MainForm:StdBar:ToolButton1:Click()
   MainForm:Win32Bar:ToolButton1:Click()

RETURN oObj

METHOD SetControl( oObj ) CLASS TFormEdit

   LOCAL oTree, o, n

   aAdd( ::aCtrl, oObj )
   ::oMask:GetSiblings()
   ::oMask:SetFocus()
   ::oMask:Cursor := NIL
   ::CtrlName := NIL

   ::XFMControl( oObj:name, oObj, .F. )

   oObj:SetMethod( "Delete", {||InspCombo:DelObject( oObj ), ;
                              HB_QSELF():DelControl(), oObj:Parent:XFMControl( NIL, oObj, .T. ) } )

   InspCombo:AddString( oObj:name + chr(9) + oObj:ClassName(), oObj )
   oTree := InspTree:Add( oObj:Name, 1, oObj:Fhandle )
   oTree:Select()

   IF !EMPTY( oObj:Controls )
      FOR n := 1 TO LEN( oObj:Controls )
          o := oObj:Controls[ n ]
          oTree:Add( o:name,1, o:Fhandle )
          oTree:Expand()
      NEXT
   ENDIF

RETURN oObj
//----------------------------------------------------------------------------//
METHOD GetUniquePropName(cName) CLASS TFormEdit 

   LOCAL nInd:=1

   DO WHILE aScan( ::Controls, {|o| o:Name == cName+AllTrim(Str(nInd))}) > 0
      nInd++
   ENDDO

RETURN(cName+AllTrim(Str(nInd)))


//----------------------------------------------------------------------------//

METHOD SetGridSize(x,y) CLASS TFormEdit 

   LOCAL xSize
   LOCAL ySize
   LOCAL cBits

   ::xGrid:=IFNIL( x, ::xGrid, x )
   ::yGrid:=IFNIL( y, ::yGrid, y )

   cBits:= MakeGridTile(::xGrid,::yGrid,@xSize,@ySize)

   IF !Empty(::hBmpGrid)
      DeleteObject(::hBmpGrid)
   ENDIF
   ::hBmpGrid  := CreateBitmap( @xSize, ySize, 1, 1, cBits )
   ::xBmpSize:=xSize
   ::yBmpSize:=ySize

RETURN(NIL)


*------------------------------------------------------------------------------

METHOD WMPaint( hDC ) CLASS TFormEdit 

   LOCAL hMemDC  := CreateCompatibleDC(hDC)
   LOCAL i       := 0
   LOCAL j       := 0
   LOCAL hOldBmp := SelectObject( hMemDC, ::hBmpGrid )

   SetTextColor( hDC, ::GridColor )
   DO WHILE j < ::FHeight
      i:=0
      DO WHILE i < ::FWidth
         BitBlt( hDC, i, j, ::xBmpSize,::yBmpSize, hMemDC, 0, 0, SRCAND )
         i+=::xBmpSize
      ENDDO
      j+=::yBmpSize
   ENDDO

   SelectObject(hMemDC,hOldBmp)
   DeleteDC(hMemDC)

RETURN(0)


//----------------------------------------------------------------------------//
METHOD XFMTop() CLASS TFormEdit

   LOCAL XFM

   // Warning can NOT use aFill() because all elements will share SAME array!!!
   FOR EACH XFM IN ::aXFM
      XFM := {}
   NEXT

   aAdd( ::aXFM[ XFM_Header ], "GLOBAL " + SubStr( ::Caption, 2 ) + CRLF )

   aAdd( ::aXFM[ XFM_Header ], '#include "hbclass.ch"' )
   aAdd( ::aXFM[ XFM_Header ], '#include "xide.ch"' )

   aAdd( ::aXFM[ XFM_Class ], "// ! AUTO_GENERATED !" )
   aAdd( ::aXFM[ XFM_Class ], "CLASS " + ::Caption + " FROM TForm" )

   aAdd( ::aXFM[ XFM_EndClass ], "END CLASS //" + ::Caption )

   ::aBases[1] := __ClsInst( ::ClassH )

   ::XFMReflect()

RETURN Self

//----------------------------------------------------------------------------//
METHOD XFMRoot() CLASS TFormEdit

   LOCAL aVar, aDiff
   STATIC lOn := .F.

   IF lOn
      Return Self
   ENDIF
   lOn := .T.

   aSize( ::aXFM[ XFM_Datas ], 0 )

   aDiff := __ObjGetValueDiff( Self, ::aBases[1] ) //, HB_OO_CLSTP_EXPORTED is default.
   ::nForms--

   FOR EACH aVar IN aDiff
      IF ValType( aVar[2] ) != 'O'
         aAdd( ::aXFM[ XFM_Datas ], aVar )
      ENDIF
   NEXT

   lOn := .F.

   ::XFMReflect()

RETURN Self

//----------------------------------------------------------------------------//
METHOD XFMControl( cName, oControl, lDelete ) CLASS TFormEdit

   LOCAL nControl, nBase, aVar, XFMControl

   IF cName == NIL
      nControl := aScan( ::aXFM[ XFM_Controls ], { |XFM| XFM[2] == oControl } )

      IF lDelete
         aDel( ::aXFM[ XFM_Controls ], nControl, .T. )
         ::XFMReflect()
         Return Self
      ENDIF

      nBase := aScan( ::aBases, { | oBase | oBase:ClassH == oControl:ClassH } )

      XFMControl := ::aXFM[ XFM_Controls ][ nControl ]

      XFMControl[ 3 ] := __ObjGetValueDiff( oControl, ::aBases[ nBase ] ) //, HB_OO_CLSTP_EXPORTED is default.

      aSize( XFMControl[ 4 ], 0 )
      aAdd( XFMControl[ 4 ], "   CONTROL " + XFMControl[ 1 ] + " FROM " + oControl:ClassName )

      FOR EACH aVar IN XFMControl[ 3 ]
         IF ValType( aVar[2] ) == 'O'
            aAdd( XFMControl[ 4 ], "" )
            aAdd( XFMControl[ 4 ], ValToPrg( aVar[2], aVar[1], 6 ) )
            aAdd( XFMControl[ 4 ], "" )
         ELSE
            aAdd( XFMControl[ 4 ], "      :" + aVar[1] + " := " + ValToPrg( aVar[2] ) )
         ENDIF
      NEXT

      aAdd( XFMControl[ 4 ], "   END CONTROL" )
   ELSE
      aAdd( ::aXFM[ XFM_Controls ], { cName, oControl, , {} } )

      XFMControl := ::aXFM[ XFM_Controls ][ -1 ]

      aAdd( XFMControl[4], "   CONTROL " + cName + " FROM " + oControl:ClassName )

      IF ( nBase := aScan( ::aBases, { | oBase | oBase:ClassH == oControl:ClassH } ) ) == 0
         aAdd( ::aBases, __ClsInst( oControl:ClassH ) )
         nBase := -1
      ENDIF

      XFMControl[ 3 ] := __ObjGetValueDiff( oControl, ::aBases[ nBase ] ) //, HB_OO_CLSTP_EXPORTED is default.

      FOR EACH aVar IN XFMControl[ 3 ]
         IF ValType( aVar[2] ) == 'O'
            aAdd( XFMControl[ 4 ], ValToPrg( aVar[2], aVar[1], 6 ) )
         ELSE
            aAdd( XFMControl[ 4 ], "      :" + aVar[1] + " := " + ValToPrg( aVar[2] ) )
         ENDIF
      NEXT

      aAdd( XFMControl[ 4 ], "   END CONTROL" )
   ENDIF

   ::XFMReflect()

RETURN Self

//----------------------------------------------------------------------------//
METHOD XFMReflect() CLASS TFormEdit

   LOCAL cText := "", cLine, aVar, XFMControl

   FOR EACH cLine IN ::aXFM[ XFM_Header ]
      cText += ( cLine + CRLF )
   NEXT

   cText += CRLF

   FOR EACH cLine IN ::aXFM[ XFM_Class ]
      cText += ( cLine + CRLF )
   NEXT
   cText += CRLF

   FOR EACH aVar IN ::aXFM[ XFM_Datas ]
      IF ValType( aVar[2] ) == 'O'
         ctext += ValToPrg( aVar[2], aVar[1], 6 ) + CRLF
      ELSE
         cText += "      DATA " + aVar[1] + " INIT " + ValToPrg( aVar[2] ) + CRLF
      ENDIF
   NEXT

   cText += CRLF

   FOR EACH XFMControl IN ::aXFM[ XFM_Controls ]
      FOR EACH cLine IN XFMControl[ 4 ]
         cText += ( cLine + CRLF )
      NEXT

      cText += CRLF
   NEXT

   FOR EACH cLine IN ::aXFM[ XFM_EndClass ]
      cText += ( cLine + CRLF )
   NEXT

   ObjEdit:SourceEdit:aText := {''}
   ObjEdit:SourceEdit:Line  := 1
   ObjEdit:SourceEdit:Col   := 1
   ObjEdit:SourceEdit:Lines := 1
   
   ObjEdit:SourceEdit:SetText( cText )

RETURN Self

//----------------------------------------------------------------------------//
METHOD XFMTrace() CLASS TFormEdit

   SayMembers( ::aXFM )

RETURN Self

//----------------------------------------------------------------------------//
Static Procedure SayMembers( aArray, cPad )

   LOCAL xProperty

   IF cPad == NIL
      cPad := ""
   ENDIF

   OutputDebugString( cPad + SubStr( aArray:ClassName, 13 ) + CRLF )
   OutputDebugString( cPad + Replicate( "-", Len( SubStr( aArray:ClassName, 13 ) ) ) + CRLF )

   FOR EACH xProperty IN aArray
      IF ValType( xProperty ) == 'A'
         SayMembers( xProperty, cPad + cPad )
      ELSE
         OutputDebugString( cPad + ":" + CStr( xProperty ) + CRLF )
      END
   NEXT

Return

//----------------------------------------------------------------------------//

// make the tile as close to 256x256 as possible for speed
// Maximum grid size 256x256 !
// Mininimu 2x2

FUNCTION MakeGridTile(nxGrid,nyGrid,width,height)

   LOCAL nWidth   := 256 - 256 % nxGrid
   LOCAL nHeight  := 256 - 256 % nyGrid
   LOCAL cBits    := ""
   LOCAL nByte    := 1
   LOCAL nVal     := 0
   LOCAL cDotted
   LOCAL cEmpty
   LOCAL nBits
   LOCAL nPos
   LOCAL i

   width   := nWidth                              // actual bitmap x size
   height  := nHeight                             // actual bitmap y size
   nWidth  := Ceiling(nWidth/8)                     // convert to bytes
   nWidth  := If( nWidth%2==0, nWidth, nWidth+1 ) // align on word
   cDotted := Replicate(chr(255),nWidth)
   cEmpty  := cDotted
   nBits   := (8* nWidth) -1                    // -1 for the counter

   FOR i:=0 TO nBits STEP nxGrid
      IF i >= nByte*8                            // next byte
         cDotted[nByte]:=255-nVal
         nVal:=2^(7-(i%8))
         nByte:=Int(i/8)+1
      ELSE
         nVal+=2^(7-(i%8))
      ENDIF
   NEXT
   cDotted[nByte] := 255-nVal

   nHeight--
   FOR i:= 0 TO nHeight
      IF i % nyGrid == 0
         cBits += cDotted
      ELSE
         cBits += cEmpty
      ENDIF
   NEXT

   RETURN(cBits)

