
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
   METHOD OnPaint( hDC )     INLINE ::DrawGrid( hDC, 3 ),1
   METHOD OnCreate()         INLINE ::CreateSub()

   METHOD OnSize()

   METHOD OnCommand(nwParam) INLINE ::SubCommands( nwParam )
   METHOD CreateSub()
   METHOD DrawGrid()
   METHOD SubCommands()


 END CLASS

//----------------------------------------------------------------------------//

METHOD Add( cName, oObj )

   LOCAL oCtrl:=Super:Add( cName, oObj )

   aAdd( ::aCtrl, oCtrl)

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

METHOD SubCommands( nwParam )

   IF nwParam == 500 
      ::MsgBox( 'HI FROM TBUTTON')
   ENDIF

   RETURN(0)

//----------------------------------------------------------------------------//

METHOD CreateSub()

   LOCAL oBtn
   LOCAL xRet


   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM SUBFORM1')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()

   ::Add('TestButton',  TButton():New( self, 'OOPS',                       500,   0,  0, 200, 100 ) ) 
   ::Add('TestEdit',      TEdit():New( self, 'This is an edit control',    501, 210,  0, 200,  20 ) )
   ::Add('TestCombo', TComboBox():New( self,                               502, 210, 30, 200, 100 ) )
   ::Add('TestText',    TStatic():New( self, 'This is a Static control',   503, 210, 55, 200,  20 ) )
   ::Add('TestRadio',    TRadio():New( self, 'This is a &Radio Button',    504, 210, 80, 200,  20 ) )
   ::Add('TestCheck',    TCheck():New( self, 'This is a &Check Button',    505, 210,105, 200,  20 ) )

   ::TestButton:SetFocus()


   xRet := super:OnCreate()

   ::oMask:=oCtrlMask():New( self )
   ::oMask:Create()
   ::oMask:GetSiblings()
   ::oMask:BringToTop()
   ::oMask:SetFocus()

   RETURN( NIL )

//----------------------------------------------------------------------------//

METHOD DrawGrid(hDC,nGran)

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




