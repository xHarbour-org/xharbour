#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "debug.ch"

static oApp


//-------------------------------------------------------------------------------------------

FUNCTION Main

   oApp := Application():Initialize()

   WITH OBJECT oApp

      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )

         :SetBkBrush( COLOR_APPWORKSPACE+1 )
         :WindowMenu := TMenu():New()

         WITH OBJECT :WindowMenu
            :AddPopup('popup 1')

            WITH OBJECT :Popup
               :AddItem( 'item 100', 100, {||MessageBox( GetActiveWindow(),'HI FROM THE MAIN CLASS')})
               :AddItem( 'item 101', 101)
               :AddItem( 'item 102', 102)
               :AddItem( 'item 103', 103)
            END

            :AddPopup('popup 2')

            WITH OBJECT :Popup
               :AddItem( 'item 200', 200, {||oApp:CreateForm( 'SubForm1', SubForm1(),oApp:MainFrame ) } )
               :AddItem( 'item 201', 201)
               :AddItem( 'item 202', 202)
               :AddItem( 'item 203', 203, {||oApp:SubForm1:SetProcedure()} )
            END
         END

         :SetWindowMenu()
      END

      :Run()
  END

RETURN( nil)

//-------------------------------------------------------------------------------------------

CLASS MainFrame FROM TFrame
   
   METHOD New( oParent ) INLINE ::Caption := 'Main Form from TFrame',;
                                ::left    := 0,;
                                ::top     := 0,;
                                ::width   := 200,;
                                ::height  := 100,;
                                super:new( oParent )

   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Quitting Whoo', 'OnCloseQuery', MB_YESNO ) == IDYES,;
                                    PostQuitMessage(0), 0 )

   METHOD OnCommand( nwParam, nlParam ) INLINE ::MainCommands( nwParam, nlParam )
   METHOD MainCommands()
ENDCLASS

//----------------------------------

METHOD MainCommands( nwParam, nlParam ) CLASS MainFrame
   local oForm
   do case
      case nwParam == 101
           oForm := SubForm1():New( self )
           oForm:Create()
   endcase
return( super:OnCommand() )


//-------------------------------------------------------------------------------------------

CLASS SubForm1 FROM TForm
   
   METHOD New( oParent )     INLINE ::Caption := 'SubForm1 from TForm', ;
                                    ::left    := 0,;
                                    ::top     := 150,;
                                    ::width   := 500,;
                                    ::height  := 200,;
                                    super:New( oParent )

   METHOD OnPaint( hDC )     INLINE ::DrawGrid( hDC, 3 ),0
   METHOD OnCreate()         INLINE ::CreateSub()

   METHOD OnCommand(nwParam) INLINE ::SubCommands( nwParam )

   METHOD CreateSub()
   METHOD DrawGrid()
   METHOD SubCommands()
ENDCLASS

//----------------------------------

METHOD SubCommands( nwParam ) CLASS SubForm1
   if nwParam == 500 
      ::MsgBox( 'HI FROM TBUTTON')
   endif
return(nil)

METHOD CreateSub() CLASS SubForm1

   local oBtn
   local oMask
   local xRet

   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM SUBFORM1')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()


   ::Add('TestButton',  TButton():New( self, 'OOPS',                       500,   0,  0, 200, 100 ) ) 
   ::Add('TestEdit',      TEdit():New( self, 'This is an edit control',    501, 210,  0, 200,  20 ) )
   ::Add('TestCombo', TComboBox():New( self, 'This is a ComboBox control', 502, 210, 30, 200, 100 ) )
   ::Add('TestText',    TStatic():New( self, 'This is a Static control',   503, 210, 55, 200,  20 ) )
   ::Add('TestRadio',    TRadio():New( self, 'This is a Radio Button',     504, 210, 80, 200,  20 ) )
   ::Add('TestCheck',    TCheck():New( self, 'This is a Check Button',     505, 210,105, 200,  20 ) )

   ::TestButton:SetFocus()

   oMask:=oCtrlMask():New( ::TestButton )
   ::Add('mask', oMask)

return( super:OnCreate() )

//----------------------------------

METHOD DrawGrid(hDC,nGran) CLASS SubForm1

   local aRect,hBrush,hOldPen,hOldBrush,hPen,hBmp

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
return(0)

