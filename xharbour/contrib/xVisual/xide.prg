#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"

FUNCTION Main
   LOCAL oApp, oMain, oPopup, oMenu, oItem
   oApp  := Application():New()
   oMain := Form1():New()
   oMain:Create()
   
   oMenu := TMenu():New( oMain )
      oMenu:AddPopup('popup 1')
         oMenu:Popup:AddItem('item 100',100)
         oMenu:Popup:AddItem('item 101',101)
         oMenu:Popup:AddItem('item 102',102)
         oMenu:Popup:AddItem('item 103',103)
      oMenu:AddPopup('popup 2')
         oMenu:Popup:AddItem('item 200',200)
         oMenu:Popup:AddItem('item 201',201)
         oMenu:Popup:AddItem('item 202',202)
         oMenu:Popup:AddItem('item 203',203)
      oMenu:AddPopup('popup 3')
         oMenu:Popup:AddItem('item 300',300)
         oMenu:Popup:AddItem('item 301',301)
         oMenu:Popup:AddItem('item 302',302)
         oMenu:Popup:AddItem('item 303',303)
   oMenu:Set()

   oApp:Run()
RETURN( nil)


CLASS Form1 FROM TForm
   METHOD OnPaint( hDC ) INLINE DrawGrid( ::handle, hDC, 3 ),0
   METHOD OnClose()      INLINE MessageBox( ::handle, 'OnClose','Whoo'),;
                                PostQuitMessage(0)

ENDCLASS
