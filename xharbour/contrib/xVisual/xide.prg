#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"

static oApp

//-------------------------------------------------------------------------------------------

FUNCTION Main

   oApp := Application():New()

   WITH OBJECT oApp

      WITH OBJECT :CreateFrame( 'MainFrame', MainFrame() )

         :SetBkBrush( COLOR_APPWORKSPACE+1 )

         :WindowMenu := TMenu():New()

         WITH OBJECT :WindowMenu
            :AddPopup('popup 1')

            WITH OBJECT :Popup
               :AddItem( 'item 100', 100, {||MessageBox(,'HI FROM THE MAIN CLASS')})
               :AddItem( 'item 101', 101)
               :AddItem( 'item 102', 102)
               :AddItem( 'item 103', 103)
            END

            :AddPopup('popup 2')

            WITH OBJECT :Popup
               :AddItem( 'item 200', 200, {||oApp:MainFrame:Maximize(), oApp:CreateForm( 'SubForm1', SubForm1(),oApp:MainFrame ) } )
               :AddItem( 'item 201', 201)
               :AddItem( 'item 202', 202)
               :AddItem( 'item 203', 203, {||oApp:SubForm1:SetProcedure()} )
            END
         END

         :SetWindowMenu()
      END

      :MainFrame:PostMessage( WM_COMMAND, 200 )
      :Run()
  END

 #ifdef ZZ
   oApp:CreateForm( 'MainFrame', MainFrame() )

   oApp:MainFrame:SetBkBrush( COLOR_APPWORKSPACE+1 )

   oApp:MainFrame:WindowMenu := TMenu():New()

      oApp:MainFrame:WindowMenu:AddPopup( 'popup 1' )

         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 100', 100, {||MessageBox(,'HI FROM THE MAIN CLASS')})
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 101', 101)
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 102', 102)
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 103', 103)

      oApp:MainFrame:WindowMenu:AddPopup( 'popup 2' )

         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 200', 200, {||oApp:MainFrame:Maximize(),;
                                                                     oApp:CreateForm( 'SubForm1',SubForm1(),oApp:MainFrame ) } )
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 201', 201)
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 202', 202)
         oApp:MainFrame:WindowMenu:Popup:AddItem( 'item 203', 203)

      oApp:MainFrame:SetWindowMenu()

   oApp:MainFrame:PostMessage( WM_COMMAND, 200 )
   oApp:Run()
 #endif


RETURN( nil)

//-------------------------------------------------------------------------------------------

CLASS MainFrame FROM TFrame

   METHOD New( oParent )       INLINE ::Caption := 'Main Form from TForm', super:new( oParent )

   METHOD OnClose()            INLINE MessageBox( ::handle, 'OnClose','Whoo'),;
                                      PostQuitMessage(0)

   METHOD OnCommand( nwParam ) INLINE IF( nwParam == 103,;
                                         MessageBox(, 'THIS IS FROM THE OnCommand MESSAGE' ),),0

ENDCLASS

//-------------------------------------------------------------------------------------------

CLASS SubForm1 FROM TForm

   METHOD New( oParent )       INLINE ::Caption := 'SubForm1 from TForm', super:new( oParent )

   METHOD OnPaint( hDC )       INLINE DrawGrid( ::handle, hDC, 3 ),0

ENDCLASS













//-------------------------------------------------------------------------------------------


FUNCTION DrawGrid(hWnd,hDC,nGran)

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
   aRect     := GetClientRect( hWnd )

   SetTextColor( hDC, rgb( 0, 0, 0 ) )
   SetBkColor( hDC, GetSysColor( COLOR_BTNFACE ) )
   Rectangle( hDC, aRect[1], aRect[2], aRect[3], aRect[4] )
   SelectObject( hDC, hOldBrush )
   SelectObject( hDC, hOldPen )
   DeleteObject( hBrush )
   DeleteObject( hBmp )
   DeleteObject( hPen )

return(0)
