#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"

static oApp


//-------------------------------------------------------------------------------------------

FUNCTION Main

   oApp := Application():New()
   oApp:CreateForm( 'MainForm', mainForm() )
   oApp:Run()

RETURN( nil)

//-------------------------------------------------------------------------------------------

CLASS MainForm FROM TForm

   METHOD New( oParent ) INLINE  ::Caption := 'Main Form from TForm', super:new( oParent )
   
   METHOD OnPaint( hDC ) INLINE DrawGrid( ::handle, hDC, 3 ),0
   METHOD OnClose()      INLINE MessageBox( ::handle, 'OnClose','Whoo'),;
                                PostQuitMessage(0)
   MESSAGE OnActivate()  METHOD CreateMainMenu()
   
ENDCLASS

METHOD CreateMainMenu() CLASS MainForm

   ::WindowMenu := TMenu():New( self )
   
   ::WindowMenu:AddPopup( 'popup 1' )
      
      ::WindowMenu:Popup:AddItem( 'item 100', 100, {||MessageBox(,'HI FROM THE MAIN CLASS')})
      ::WindowMenu:Popup:AddItem( 'item 101', 101)
      ::WindowMenu:Popup:AddItem( 'item 102', 102)
      ::WindowMenu:Popup:AddItem( 'item 103', 103)
         
   ::WindowMenu:AddPopup( 'popup 2' )
      
      ::WindowMenu:Popup:AddItem( 'item 200', 200)
      ::WindowMenu:Popup:AddItem( 'item 201', 201)
      ::WindowMenu:Popup:AddItem( 'item 202', 202)
      ::WindowMenu:Popup:AddItem( 'item 203', 203)
         
   ::SetWindowMenu()   

return( super:OnActivate() )














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

/*
   WITH OBJECT oApp
      
      oMain := :CreateForm( 'MainForm', mainForm() )

      WITH OBJECT oMain
      
         :WindowMenu := TMenu():New()
         
         oMenu := oMain:WindowMenu 
         
         WITH OBJECT oMenu
            oSub1 := :AddPopup('popup 1')

            WITH OBJECT oSub1
               :AddItem('item 100',100)
               :AddItem('item 101',101)
               :AddItem('item 102',102)
               :AddItem('item 103',103)
            END
            
            oSub2 := :AddPopup('popup 2')

            WITH OBJECT oSub2
               :AddItem('item 200',200)
               :AddItem('item 201',201)
               :AddItem('item 202',202)
               :AddItem('item 203',203)
            END
         END
         
         :SetWindowMenu()
         
      END
      :Run()
  END
*/