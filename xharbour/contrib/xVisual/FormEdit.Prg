/*
 * $Id: FormEdit.Prg,v 1.77 2002/10/30 08:14:01 ronpinkas Exp $
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
GLOBAL EXTERNAL oApp
GLOBAL EXTERNAL MainFrame
GLOBAL EXTERNAL FormEdit

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

   METHOD New( oParent )       INLINE ::Caption := 'TForm' + LTrim( Str( ++::nForms ) ), ;
                                      ::Editing := .F.        ,;
                                      ::aCtrl   := {}         ,;
                                      ::left    := 200        ,;
                                      ::top     := 125        ,;
                                      ::width   := 600        ,;
                                      ::height  := 400        ,;
                                      ::xGrid   := 8          ,;
                                      ::yGrid   := 8          ,;
                                      ::GridColor:=RGB(0,0,0) ,;
                                      super:New( oParent )    ,;
                                      ::XFMTop()

   METHOD AddControl()
   METHOD OnCreate()         INLINE ::CreateSub()
   METHOD OnSize()
   METHOD OnMove()           INLINE ::Parent:ObjInspect:SetBrowserData( HB_QSelf(), .F. ),NIL

   METHOD OnPaint( hDC )     INLINE ::TileGrid(hDC)
   METHOD TileGrid(hDC)
   METHOD OnMenuCommand(oItem)
   METHOD CreateControl(aRect)
   METHOD CreateSub()
   METHOD GetUniquePropName()
   METHOD OnDestroy()
   METHOD SetControl( oObj )
   METHOD SetGridSize(x,y)
   METHOD OnCloseQuery()     INLINE ::Parent:WindowMenu:Enable( 101 ),;
                                    ::Parent:StdBar:DisableAll(),;
                                    ::Parent:ObjInspect:ComboBox1:DelObject( self ),;
                                    super:OnCloseQuery()

   METHOD XFMTop()
   METHOD XFMRoot()
   METHOD XFMControl( cName, oControl, lDelete )
   METHOD XFMReflect()
   METHOD XFMTrace()

 END CLASS

//----------------------------------------------------------------------------//

METHOD AddControl( cName, oObj )

   LOCAL oCtrl

   oObj:Name := cName
   oCtrl := Super:Add( oObj )

   aAdd( ::aCtrl, oCtrl)
   ::oMask:GetSiblings()
   ::oMask:SetFocus()
   ::oMask:Cursor := NIL
   ::CtrlName := NIL

   ::XFMControl( cName, oObj, .F. )

   RETURN(oCtrl)

//----------------------------------------------------------------------------//

METHOD OnSize( nFlag, Width, Height)

   LOCAL aRect

   IF ::oMask <> NIL
      aRect := GetClientRect(::Handle)
      ::oMask:Move( aRect[1], aRect[2], aRect[3], aRect[4], .T. )
      ::oMask:Refresh()
   ENDIF

   ::Parent:ObjInspect:SetBrowserData( Self, .F. )

   RETURN NIL

//----------------------------------------------------------------------------//

METHOD OnDestroy()

   IF !Empty(::hBmpGrid)
      DeleteObject(::hBmpGrid)
   ENDIF

   RETURN(Super:OnDestroy())

//----------------------------------------------------------------------------//

METHOD OnMenuCommand(oItem)

   // check for the range of commands which create controls
   // otherwise process the command as usual

   IF .T. //command if range of control creation commands

      // store the control type in memory
      ::CtrlName := oItem:Name // control type to be created

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

   LOCAL oObj
   LOCAL nInd:=0
   LOCAL cName

   IF ! Empty( ::CtrlName )

      IF Empty(aRect)
         aRect:={10,10,NIL,NIL}
      ENDIF

      cName:=::GetUniquePropName(::CtrlName)

      DO CASE
      CASE ::CtrlName=="Button"
         oObj:=::Add( TButton():New(   self, , 500,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="Edit"
         oObj:=::Add( TEdit():New(     self, , 501,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="Label"
         oObj:=::Add( TStatic():New(   self, , 502,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="CheckBox"
         oObj:=::Add( TCheck():New(    self, , 503,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="RadioButton"
         oObj:=::Add( TRadio():New(    self, , 504,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="ComboBox"
         oObj:=::Add( TComboBox():New( self,   505,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="ListBox"
         oObj:=::Add( TListBox():New(  self,   506,  aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="StatusBar"
         IF !("2" $ cName) // only one status bar
           oObj:=::Add( TStatusBar():New( self, , 507 ) )
         ENDIF

      CASE ::CtrlName=="TabControl"
         oObj:=::Add( TTabControl():New( self, 508, aRect[1], aRect[2], aRect[3], aRect[4] ) )
         oObj:AddTab( "Tab1" )
         oObj:AddTab( "Tab2" )
         oObj:configure()

      CASE ::CtrlName=="TabPage"
         // What ???

      CASE ::CtrlName=="GroupBox"
         oObj:=::Add( TGroupBox():New(   self, , 509, aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="TreeView"
         oObj:=::Add( TTreeView():New(   self,   510, aRect[1], aRect[2], aRect[3], aRect[4] ) )

      CASE ::CtrlName=="Rebar"
         IF !("2" $ cName) // only one Rebar
            oObj:=::Add( TRebar():New( self ) )
         ENDIF

      CASE ::CtrlName=="ToolBar"
         oObj:=::Add( TToolBar():New( self, 511, 2, HINST_COMMCTRL, IDB_STD_SMALL_COLOR, 16, 16, 16, 16, 14 ) )
         oObj:AddButton("New", ToolButton():New( 0 , ,"New",  100 ) )
         oObj:AddButton("Open",ToolButton():New( 1 , ,"Open", 100 ) )
         UpdateWindow( ::handle )

      CASE ::CtrlName=="ProgressBar"
         oObj:=::Add( TProgressBar():New( self, aRect[1], aRect[2], aRect[3], aRect[4] ) )

      OTHERWISE // unknown name
         ::CtrlName    :=NIL
         ::oMask:Cursor:=NIL
      ENDCASE

      IF oObj != NIL
         ::SetControl( oObj )
      ENDIF
   ENDIF

   ::Parent:StdBar:CheckButton( 150, .T. )
   ::Parent:Win32Bar:CheckButton( 250, .T. )

RETURN oObj

METHOD SetControl( oObj ) CLASS TFormEdit

   LOCAL oTree, o, n

   aAdd( ::aCtrl, oObj )
   ::oMask:GetSiblings()
   ::oMask:SetFocus()
   ::oMask:Cursor := NIL
   ::CtrlName := NIL

   ::XFMControl( oObj:name, oObj, .F. )

   oObj:SetMethod( "Delete", {||MainFrame:ObjInspect:ComboBox1:DelObject( oObj ), ;
                              HB_QSELF():DelControl(), oObj:Parent:XFMControl( NIL, oObj, .T. ) } )

   MainFrame:ObjInspect:ComboBox1:AddString( oObj:name + chr(9) + oObj:ClassName(), oObj )
   oTree := MainFrame:ObjTree:TreeView1:Add( oObj:Name, 1, oObj:handle )
   oTree:Select()

   IF !EMPTY( oObj:Controls )
      FOR n := 1 TO LEN( oObj:Controls )
          o := oObj:Controls[ n ]
          oTree:Add( o:name,1, o:handle )
          oTree:Expand()
      NEXT
   ENDIF

RETURN oObj
//----------------------------------------------------------------------------//
METHOD GetUniquePropName(cName)

   LOCAL nInd:=1

   DO WHILE aScan( ::Controls, {|o| o:Name == cName+AllTrim(Str(nInd))}) > 0
      nInd++
   ENDDO

   RETURN(cName+AllTrim(Str(nInd)))


//----------------------------------------------------------------------------//

METHOD SetGridSize(x,y)

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

//----------------------------------------------------------------------------//

METHOD CreateSub()

   LOCAL oBtn
   LOCAL xRet
   Local oTree

   ::SetGridSize()

   ::Parent:StdBar:EnableAll()
   ::Parent:Win32Bar:EnableAll()

   ::Parent:ObjInspect:ComboBox1:AddString( ::Name + chr(9) + "TFORM", self)
//   ::Parent:ObjInspect:ComboBox1:SetCurSel(::Parent:ObjInspect:ComboBox1:GetCount()-1)
   oTree := ::Parent:ObjTree:TreeView1:Add( ::Name,, ::handle )
   oTree:Select()


   xRet := super:OnCreate()

   ::oMask:=oCtrlMask():New( self )
   ::oMask:Create()
   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

   ::oMask:Hint:=""
   ::oMask:HintWindow:SetMaxTipWidth(10)
   RETURN( xRet )

*------------------------------------------------------------------------------

METHOD TileGrid(hDC)

   LOCAL hMemDC := CreateCompatibleDC(hDC)
   LOCAL i:=0
   LOCAL j:=0
   LOCAL hOldBmp := SelectObject(hMemDC,::hBmpGrid)

   SetTextColor(hDC,::GridColor)
   DO WHILE j < ::Height
      i:=0
      DO WHILE i < ::Width
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

   LOCAL cTExt := "", cLine, aVar, XFMControl

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

   //TraceLog( cText )

   MainFrame:ObjEdit:SourceEdit:SetText( cText )

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

