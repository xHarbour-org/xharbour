
// Form class in edit mode


#include "windows.ch"
#include "wingdi.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#Include "cstruct.ch"
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
//----------------------------------------------------------------------------//

 CLASS TFormEdit FROM TForm

   CLASSDATA nForms INIT 0

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

   METHOD Add()
   METHOD OnCreate()         INLINE ::CreateSub()
   METHOD OnSize()
   METHOD OnMove()           INLINE ::Parent:ObjInsp:SetBrowserData( self ),NIL

   METHOD OnPaint( hDC )     INLINE ::TileGrid(hDC)
   METHOD TileGrid(hDC)
   METHOD OnMenuCommand(oItem)
   METHOD CreateControl(aRect)
   METHOD CreateSub()
   METHOD GetUniquePropName()
   METHOD OnDestroy()
   METHOD SetGridSize(x,y)
   METHOD OnCloseQuery()     INLINE ::Parent:WindowMenu:Enable( 101 ),;
                                    ::Parent:StdBar:DisableAll(),;
                                    ::XFMTrace(),;
                                    super:OnCloseQuery()

   METHOD XFMTop()
   METHOD XFMRoot()
   METHOD XFMTrace()

 END CLASS

//----------------------------------------------------------------------------//

METHOD Add( cName, oObj )

   LOCAL oCtrl:=Super:Add( cName, oObj )

   aAdd( ::aCtrl, oCtrl)
   ::oMask:GetSiblings()
   ::oMask:SetFocus()
   ::oMask:Cursor := NIL
   ::CtrlName := NIL

   RETURN(oCtrl)

//----------------------------------------------------------------------------//

METHOD OnSize(nFlag,Width,Height)

   LOCAL aRect

   IF ::oMask <> NIL
      aRect:=GetClientRect(::Handle)
      ::oMask:Move(aRect[1],aRect[2],aRect[3],aRect[4],.T.)
      ::oMask:Refresh()
   ENDIF
   ::Parent:ObjInsp:SetBrowserData( self )

   RETURN(NIL)

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


METHOD CreateControl(aRect)

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
         oObj:=::Add(cName, TButton():New(   self, cName, 500,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="Edit"
         oObj:=::Add(cName, TEdit():New(     self, cName, 501,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="Label"
         oObj:=::Add(cName, TStatic():New(   self, cName, 502,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="CheckBox"
         oObj:=::Add(cName, TCheck():New(    self, cName, 503,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="RadioButton"
         oObj:=::Add(cName, TRadio():New(    self, cName, 504,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="ComboBox"
         oObj:=::Add(cName, TComboBox():New( self,        505,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="ListBox"
         oObj:=::Add(cName, TListBox():New(  self,        506,  aRect[1], aRect[2], aRect[3], aRect[4] ) )
      CASE ::CtrlName=="StatusBar"
         IF !("2" $ cName) // only one status bar
           oObj:=::Add(cName, TStatusBar():New( self, cName, 507 ) )
         ENDIF
      CASE ::CtrlName=="TabControl"
         oObj:=::Add(cName, TTabControl():New( self,        508, aRect[1], aRect[2], aRect[3], aRect[4] ) )
         oObj:AddTab( "Tab1",  'TEST' )
         oObj:configure()
      CASE ::CtrlName=="GroupBox"
         oObj:=::Add(cName, TGroupBox():New(   self, cName, 509, aRect[1], aRect[2], aRect[3], aRect[4] ) )

      OTHERWISE // unknown name

         ::CtrlName:=NIL
         ::oMask:Cursor:=NIL

      ENDCASE
      if oObj!=NIL
         oObj:SetMethod("OnMove",{||oObj:Parent:Parent:ObjInsp:SetBrowserData( oObj )})
         oObj:SetMethod("OnSize",{||oObj:Parent:Parent:ObjInsp:SetBrowserData( oObj )})

         oApp:MainFrame:ObjInsp:InspCombo:AddString(oObj:propname+chr(9)+oObj:ClassName(),oObj)
         oApp:MainFrame:ObjInsp:InspCombo:SetCurSel(oApp:MainFrame:ObjInsp:InspCombo:GetCount()-1)
         ::Parent:ObjTree:Tree:Add( oObj:PropName,1 )
         ::Parent:ObjTree:Tree:Expand()
      endif
   ENDIF
   ::Parent:StdBar:CheckButton( 150, .T. )

   RETURN(oObj)

//----------------------------------------------------------------------------//


METHOD GetUniquePropName(cName)

   LOCAL nInd:=1

   DO WHILE aScan( ::Controls, {|o| o: PropName==cName+AllTrim(Str(nInd))}) > 0
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

   ::SetGridSize()

   ::Parent:StdBar:EnableAll()

   ::Parent:ObjInsp:InspCombo:AddString(::PropName+chr(9)+"TFORM",self)
   ::Parent:ObjInsp:InspCombo:SetCurSel(::Parent:ObjInsp:InspCombo:GetCount()-1)

   ::Parent:ObjTree:Tree:Add( ::PropName )

   xRet := super:OnCreate()

   ::oMask:=oCtrlMask():New( self )
   ::oMask:Create()
   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

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

   aAdd( ::aXFM[ XFM_Header ], "#include [hbclass.ch]" )
   aAdd( ::aXFM[ XFM_Header ], "#include [xide.ch]" )

   aAdd( ::aXFM[ XFM_Class ], "CLASS " + ::Caption + " FROM TForm" )

   aAdd( ::aXFM[ XFM_EndClass ], "END CLASS " + ::Caption )

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
   aSize( ::aXFM[ XFM_Controls ], 0 )

   aDiff := __ObjGetValueDiff( TFormEdit():New( oApp:MainFrame, .F. ), Self )

   FOR EACH aVar IN aDiff
      IF ValType( aVar[2] ) == 'O'
         aAdd( ::aXFM[ XFM_Controls ], aVar )
      ELSE
         aAdd( ::aXFM[ XFM_Datas ], aVar )
      ENDIF
   NEXT

   lOn := .F.

RETURN Self

//----------------------------------------------------------------------------//
METHOD XFMTrace() CLASS TFormEdit
   /*
   OutputDebugString( "Header: " + Str( Len( ::aXFM[ XFM_Header ] ) ) + CRLF )
   OutputDebugString( "Class: " + Str( Len( ::aXFM[ XFM_Class ] ) ) + CRLF )
   OutputDebugString( "Datas: " + Str( Len( ::aXFM[ XFM_Datas ] ) ) + CRLF )
   OutputDebugString( "Events: " + Str( Len( ::aXFM[ XFM_Events ] ) ) + CRLF )
   OutputDebugString( "Controls: " + Str( Len( ::aXFM[ XFM_Controls ] ) ) + CRLF )
   OutputDebugString( "EndClass: " + Str( Len( ::aXFM[ XFM_EndClass ] ) ) + CRLF )
   OutputDebugString( "Methdos: " + Str( Len( ::aXFM[ XFM_Methods ] ) ) + CRLF )
   OutputDebugString( "Footer: " + Str( Len( ::aXFM[ XFM_Footer ] ) ) + CRLF )
   */

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

