#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"

FUNCTION Main
   LOCAL oApp, oMain, oPopup, oMenu, oItem
   oApp  := Application():New()
   
   oApp:CreateForm( oMain )
//   oMain := Form1():New()
//   oMain:Create()
   
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


FUNCTION DrawGrid(hWnd,hDC,nGran)
local aRect,hBrush,hOldPen,hOldBrush,hPen,hBmp
DEFAULT nGran TO 3
DO CASE
   CASE nGran == 1
       hBmp:=CreateBitmap(8,8,1,1,;
                       Chr(255)+chr(0) +;
                       Chr(170)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(170)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(170)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(170)+chr(0) )
   CASE nGran == 2
        hBmp:=CreateBitmap(8,8,1,1,;
                       Chr(255)+chr(0) +;
                       Chr(187)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(187)+chr(0) +;
                       Chr(255)+chr(0) +;
                       Chr(255)+chr(0) )
   CASE nGran == 3
        hBmp:=CreateBitmap(8, 8, 1, 1, ;
                       CHR(255)+CHR(0) + ;
                       CHR(255)+CHR(0) + ;
                       CHR(255)+CHR(0) + ;
                       CHR(255)+CHR(0) + ;
                       CHR(255)+CHR(0) + ;
                       CHR(251)+CHR(0) + ;
                       CHR(255)+CHR(0) + ;
                       CHR(255)+CHR(0))
ENDCASE
hBrush := CreatePatternBrush(hBmp)
hPen   := CreatePen(PS_NULL, 0, 0)
hOldBrush := SelectObject(hDC, hBrush)
hOldPen   := SelectObject(hDC, hPen)
aRect:=GetClientRect(hWnd)
SetTextColor(hDC,rgb(0,0,0))
SetBkColor(hDC,GetSysColor(COLOR_BTNFACE))
Rectangle(hDC, aRect[1], aRect[2], aRect[3], aRect[4])
SelectObject(hDC, hOldBrush)
SelectObject(hDC, hOldPen)
DeleteObject(hBrush)
DeleteObject(hBmp)
DeleteObject(hPen)
return(0)
