#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"
#include "wintypes.ch"
#include "cstruct.ch"
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
return( 0 )


//-------------------------------------------------------------------------------------------

CLASS SubForm1 FROM TForm
   
   METHOD New( oParent ) INLINE ::Caption := 'SubForm1 from TForm', ;
                                super:New( oParent )
                                
   METHOD OnPaint( hDC ) INLINE ::DrawGrid( hDC, 3 ),0
   METHOD OnCreate()     INLINE ::CreateSub()  // careful handles OnCreate not OnCreation
//   METHOD OnCloseQuery() INLINE if( ::MsgBox( 'Closing SubForm1', 'OnCloseQuery', MB_YESNO ) == IDYES,;
//                                    nil, 0 )

   METHOD CreateSub()
   METHOD DrawGrid()

ENDCLASS

//----------------------------------

METHOD CreateSub() CLASS SubForm1

   local oCtrl

   ::WindowMenu := TMenu():New()

   ::WindowMenu:AddPopup( 'popup 1' )
   
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {|| ::MsgBox( 'HI FROM SUBFORM1')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      
   ::SetWindowMenu()

   oCtrl := TControl():New( self, 500 )
   
   oCtrl:Caption := 'Testing a Button'
   oCtrl:Width   := 200
   oCtrl:Height  := 100
   oCtrl:Create()

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
