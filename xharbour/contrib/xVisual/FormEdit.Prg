
// Form class in edit mode

#include "windows.ch"
#include "wingdi.ch"
#include "HbClass.ch"
#include "what32.ch"
#include "wintypes.ch"
#Include "cstruct.ch"
#include "debug.ch"

GLOBAL EXTERNAL hThisInstance

//----------------------------------------------------------------------------//

 CLASS TFormEdit FROM TForm

   DATA oMask
   DATA Editing
   DATA aCtrl
   DATA nCurrent

   METHOD New( oParent )     INLINE ::Caption := 'SubForm1 from TForm', ;
                                    ::Editing:=.F.       ,;
                                    ::aCtrl:={}          ,;
                                    ::left:=100          ,;
                                    ::top:=100           ,;
                                    ::width:=400         ,;
                                    ::height:=300        ,;
                                     super:New( oParent )

   METHOD Add()
   METHOD OnPaint( hDC )     INLINE ::DrawGrid( hDC, 3 ),0
   METHOD OnCreate()         INLINE ::CreateSub()
  // METHOD OnEraseBkgnd()
   METHOD OnSize()

   METHOD OnMenuCommand(oItem) //INLINE ::SubCommands( nwParam )
   METHOD CreateSub()
   METHOD DrawGrid()
   //METHOD SubCommands()


 END CLASS

//----------------------------------------------------------------------------//

METHOD Add( cName, oObj )

   LOCAL oCtrl:=Super:Add( cName, oObj )

   aAdd( ::aCtrl, oCtrl)
   ::oMask:GetSiblings()
   ::oMask:SetFocus()

   RETURN(oCtrl)

//----------------------------------------------------------------------------//

// move mask to cover the entire window

METHOD OnSize(nFlag,Width,Height)

   LOCAL aRect

   IF ::oMask <> NIL
      aRect:=GetClientRect(::Handle)
      ::oMask:Move(aRect[1],aRect[2],aRect[3],aRect[4],.T.)
      ::oMask:BringToTop()
      ::oMask:SetFocus()
   ENDIF

   RETURN(NIL)

//----------------------------------------------------------------------------//

METHOD OnMenuCommand(oItem)

   //view oItem:Id

   DO CASE
   CASE oItem:Text=="Button"
     ::Add('Button1',        TButton():New(   self, 'Button1',      500,   0,  0, 200, 100 ) )
   CASE oItem:Text=="Edit"
      ::Add('TestEdit',      TEdit():New(     self, 'Edit1',        501, 210,  0, 200,  20 ) )
   CASE oItem:Text=="ComboBox"
       ::Add('ComboBox1',    TComboBox():New( self, 'ComboBox1',    502, 210, 30, 200, 100 ) )
   CASE oItem:Text=="CheckBox"
       ::Add('CheckBox1',    TCheck():New(    self, 'CheckBox1',    505, 210,105, 200,  20 ) )
   CASE oItem:Text=="RadioButton"
       ::Add('RadioButton1', TRadio():New(    self, 'RadioButton1', 504, 210, 80, 200,  20 ) )
   CASE oItem:Text=="ListBox"
       ::Add('ListBox1',     TListBox():New(  self, 'ListBox1',     506, 210,  20,200,200 ) )
   CASE oItem:Text=="Label"
       ::Add('Label1',       TStatic():New(   self, 'Label1',       503, 210, 55, 200,  20 ) )

   ENDCASE

   RETURN(NIL)

//----------------------------------------------------------------------------//

METHOD CreateSub()

   LOCAL oBtn
   LOCAL xRet


   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM EDITOR')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()


   xRet := super:OnCreate()

   ::oMask:=oCtrlMask():New( self )
   ::oMask:Create()
   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

   RETURN( xRet )

//----------------------------------------------------------------------------//


// __objGetValueList( oObject ) <- lists properties ?

METHOD DrawGrid(hDC,nGran)

/*

   LOCAL i,j
   LOCAL nColor:=rgb(0,0,0)

   FOR i:=0 to ::width step 8 // <= example
      FOR j:=0 to ::height step 8 // <= example
         SetPixel(hDC,i,j,nColor)
      NEXT
   NEXT

   RETURN(0)

*/


   LOCAL aRect,hBrush,hOldPen,hOldBrush,hPen,hBmp

   DEFAULT nGran TO 3

   DO CASE
      CASE nGran == 1
          hBmp := CreateBitmap( 8, 8, 1, 1,;
                          Chr(255)+chr(0) +;
                          Chr(170)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(170)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(170)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(170)+chr(0) )
      CASE nGran == 2
           hBmp := CreateBitmap( 8, 8, 1, 1,;
                          Chr(255)+chr(0) +;
                          Chr(187)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(187)+chr(0) +;
                          Chr(255)+chr(0) +;
                          Chr(255)+chr(0) )
      CASE nGran == 3
           hBmp := CreateBitmap( 8, 8, 1, 1, ;
                          CHR(255)+CHR(0) + ;
                          CHR(255)+CHR(0) + ;
                          CHR(255)+CHR(0) + ;
                          CHR(255)+CHR(0) + ;
                          CHR(255)+CHR(0) + ;
                          CHR(251)+CHR(0) + ;
                          CHR(255)+CHR(0) + ;
                          CHR(255)+CHR(0))
   ENDCASE

   hBrush    := CreatePatternBrush( hBmp )
   hPen      := CreatePen( PS_NULL, 0, 0 )
   hOldBrush := SelectObject( hDC, hBrush )
   hOldPen   := SelectObject( hDC, hPen )
   aRect     := GetClientRect( ::handle )

   SetTextColor( hDC, rgb( 0, 0, 0 ) )
   SetBkColor( hDC, GetSysColor( COLOR_BTNFACE ) )
   Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
   SelectObject( hDC, hOldBrush )
   SelectObject( hDC, hOldPen )
   DeleteObject( hBrush )
   DeleteObject( hBmp )
   DeleteObject( hPen )

RETURN(0)

//----------------------------------------------------------------------------//




