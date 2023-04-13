/*
   Pegged v1.0 Beta 1
   Copyright (C) 2004 Marcos Antonio Gambeta

   Contact: marcosgambeta@yahoo.com.br

   Website: http://geocities.yahoo.com.br/marcosgambeta/

   This file is part of the program "Pegged".

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

#Include "winuser.ch"
#Include "wingdi.ch"
#Include "what32.ch"
#include "wintypes.ch"
#Include "cstruct.ch"

#define MENU_NEW     101
#define MENU_BACK    102
#define MENU_EXIT    103
#define MENU_GAME1   104
#define MENU_GAME2   105
#define MENU_GAME3   106
#define MENU_GAME4   107
#define MENU_GAME5   108
#define MENU_GAME6   109
#define MENU_GAME7   110
#define MENU_GAME8   111
#define MENU_HELP    112
#define MENU_ABOUT   113

#define BUTTON_NEW   151
#define BUTTON_BACK  152
#define BUTTON_GAME1 153
#define BUTTON_GAME2 154
#define BUTTON_GAME3 155
#define BUTTON_GAME4 156
#define BUTTON_GAME5 157
#define BUTTON_GAME6 158
#define BUTTON_GAME7 159
#define BUTTON_GAME8 160
#define BUTTON_HELP  161
#define BUTTON_ABOUT 162
#define BUTTON_EXIT  163

#define IMAGE_EMPTY   01
#define IMAGE_DEFAULT 02
#define IMAGE_MARKED  03
#define IMAGE_NEW     04
#define IMAGE_BACK    05
#define IMAGE_GAME1   06
#define IMAGE_GAME2   07
#define IMAGE_GAME3   08
#define IMAGE_GAME4   09
#define IMAGE_GAME5   10
#define IMAGE_GAME6   11
#define IMAGE_GAME7   12
#define IMAGE_GAME8   13
#define IMAGE_HELP    14
#define IMAGE_ABOUT   15
#define IMAGE_EXIT    16

#define SND_SYNC            0x0000
#define SND_ASYNC           0x0001
#define SND_NODEFAULT       0x0002
#define SND_MEMORY          0x0004
#define SND_LOOP            0x0008
#define SND_NOSTOP          0x0010
#define SND_NOWAIT	       0x00002000L
#define SND_ALIAS           0x00010000L
#define SND_ALIAS_ID	       0x00110000L
#define SND_FILENAME        0x00020000L
#define SND_RESOURCE        0x00040004L

//---------------------------------------------------------------------------//

Static aBoard
Static aMoves
Static nMarked
Static nTotal
Static nRemoved
Static nGame
Static hMenu
Static aImages
Static aLabel
Static hStatusBar

//---------------------------------------------------------------------------//

pragma pack(4)
typedef struct { ;
    UINT    style;
    WNDPROC lpfnWndProc;
    int     cbClsExtra;
    int     cbWndExtra;
    HANDLE  hInstance;
    HICON   hIcon;
    HCURSOR hCursor;
    HBRUSH  hbrBackground;
    LPCTSTR lpszMenuName;
    LPCTSTR lpszClassName;
} WNDCLASS

//---------------------------------------------------------------------------//

Function WinMain ()

   Local wcMain IS WNDCLASS
   Local hWnd
   Local hIcon
   Local hBmp
   Local hBrush
   Local cMsg

   aBoard   := array(49,2)
   aMoves   := array(32,3)
   nMarked  := 0
   nTotal   := 0
   nRemoved := 0
   nGame    := 1

   hIcon  := LoadIcon(_GetInstance(),"MAINICON")
   hBmp   := LoadBitmap(,"WALLPAPER")
   hBrush := CreatePatternBrush(hBmp)
   DeleteObject(hBmp)

   wcMain:hIcon         := hIcon
   wcMain:hbrBackground := hBrush
   wcMain:lpszClassName := "pegged"

   RegisterClass( wcMain,,{ | hWnd, nMsg, nwParam, nlParam | MainWndProc( hWnd, nMsg, nwParam, nlParam ) } , - 1 )

   hWnd := CreateWindow("pegged","Pegged"+chr(0),WS_OVERLAPPEDWINDOW+WS_CLIPCHILDREN,100,100,530,480,0 )

   SetStyle( hWnd, WS_THICKFRAME, FALSE )
   SetStyle( hWnd, WS_MAXIMIZEBOX, FALSE )

   InitCommonControls()

   hStatusBar := CreateStatusWindow( WS_CHILD+WS_VISIBLE, "Made with xHarbour and WHAT32 - http://www.xharbour.org", hWnd, 100 )

   hMenu := PrepareMenu()
   SetMenu( hWnd, hMenu )

   CenterWindow(hWnd)
   ShowWindow(hWnd,SW_NORMAL)
   UpdateWindow(hWnd)

   CheckMenuItem( hMenu, MENU_GAME1, MF_CHECKED )

   NewGame()

   Do While GetMessage( @cMsg, 0, 0, 0 )
      If !IsDialogMessage( , cMsg )
         TranslateMessage( cMsg )
         DispatchMessage( cMsg )
      EndIf
   EndDo
   Return Nil

//---------------------------------------------------------------------------//

Static Function MainWndProc( hWnd, nMsg, nwParam, nlParam )

   Local i := 0
   Local l := 0
   Local c := 0
   Local aToolBar := array(13)
   Local hFont1 := 0
   Local hFont2 := 0

   Do Case

   Case nMsg == WM_CREATE

      // load images
      aImages := array(16)
      aImages[01] := LoadImage( GetModuleHandle(), "IMAGE_EMPTY"  , IMAGE_BITMAP, 0, 0, LR_SHARED )
      aImages[02] := LoadImage( GetModuleHandle(), "IMAGE_DEFAULT", IMAGE_BITMAP, 0, 0, LR_SHARED )
      aImages[03] := LoadImage( GetModuleHandle(), "IMAGE_MARKED" , IMAGE_BITMAP, 0, 0, LR_SHARED )
      aImages[04] := LoadImage( GetModuleHandle(), "IMAGE_NEW"    , IMAGE_BITMAP, 0, 0            )
      aImages[05] := LoadImage( GetModuleHandle(), "IMAGE_BACK"   , IMAGE_BITMAP, 0, 0            )
      aImages[06] := LoadImage( GetModuleHandle(), "IMAGE_GAME1"  , IMAGE_BITMAP, 0, 0            )
      aImages[07] := LoadImage( GetModuleHandle(), "IMAGE_GAME2"  , IMAGE_BITMAP, 0, 0            )
      aImages[08] := LoadImage( GetModuleHandle(), "IMAGE_GAME3"  , IMAGE_BITMAP, 0, 0            )
      aImages[09] := LoadImage( GetModuleHandle(), "IMAGE_GAME4"  , IMAGE_BITMAP, 0, 0            )
      aImages[10] := LoadImage( GetModuleHandle(), "IMAGE_GAME5"  , IMAGE_BITMAP, 0, 0            )
      aImages[11] := LoadImage( GetModuleHandle(), "IMAGE_GAME6"  , IMAGE_BITMAP, 0, 0            )
      aImages[12] := LoadImage( GetModuleHandle(), "IMAGE_GAME7"  , IMAGE_BITMAP, 0, 0            )
      aImages[13] := LoadImage( GetModuleHandle(), "IMAGE_GAME8"  , IMAGE_BITMAP, 0, 0            )
      aImages[14] := LoadImage( GetModuleHandle(), "IMAGE_HELP"   , IMAGE_BITMAP, 0, 0            )
      aImages[15] := LoadImage( GetModuleHandle(), "IMAGE_ABOUT"  , IMAGE_BITMAP, 0, 0            )
      aImages[16] := LoadImage( GetModuleHandle(), "IMAGE_EXIT"   , IMAGE_BITMAP, 0, 0            )

      // create a toolbar with buttons
      aToolBar := array(13)
      For i := 1 To 13
         aToolBar[i] := CreateWindow("button","button"+chr(0),WS_CHILD+WS_VISIBLE+BS_BITMAP+BS_PUSHBUTTON+BS_NOTIFY+BS_FLAT,(i-1)*40,0,40,40,hWnd,150+i)
         SendMessage( aToolBar[i], BM_SETIMAGE, IMAGE_BITMAP, aImages[i+3] )
      Next i

      // create a board with buttons
      i := 1
      For l := 0 To 6
         For c := 0 To 6
            aBoard[i,2] := CreateWindow("button",,WS_CHILD+WS_VISIBLE+BS_BITMAP+BS_PUSHBUTTON+BS_NOTIFY+BS_FLAT,c*38+133,l*38+32+32,38,38,hWnd,200+i)
            SendMessage( aBoard[i,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_EMPTY] )
            ++i
         Next c
      Next l

      // hide corners of the board
      SetStyle( aBoard[01,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[02,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[06,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[07,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[08,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[09,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[13,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[14,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[36,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[37,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[41,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[42,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[43,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[44,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[48,2], WS_VISIBLE, FALSE )
      SetStyle( aBoard[49,2], WS_VISIBLE, FALSE )

      // create labels
      aLabel := array(6)
      aLabel[1] := CreateWindow("static","TOTAL",WS_CHILD+WS_VISIBLE+ES_CENTER,133-10,350,80,15,hWnd)
      aLabel[2] := CreateWindow("static","0",WS_CHILD+WS_VISIBLE+ES_CENTER,133-10,370,80,15,hWnd)
      aLabel[3] := CreateWindow("static","REMOVED",WS_CHILD+WS_VISIBLE+ES_CENTER,233-10,350,80,15,hWnd)
      aLabel[4] := CreateWindow("static","0",WS_CHILD+WS_VISIBLE+ES_CENTER,233-10,370,80,15,hWnd)
      aLabel[5] := CreateWindow("static","REMAINING",WS_CHILD+WS_VISIBLE+ES_CENTER,333-10,350,80,15,hWnd)
      aLabel[6] := CreateWindow("static","0",WS_CHILD+WS_VISIBLE+ES_CENTER,333-10,370,80,15,hWnd)

      // create fonts
      hFont1 := CreateFont(-10,0,0,0,100,0,0,0,DEFAULT_CHARSET,OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FF_DONTCARE,"Arial")
      hFont2 := CreateFont(-12,0,0,0,600,0,0,0,DEFAULT_CHARSET,OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FF_DONTCARE,"Arial")

      // change fonts of the labels
      SendMessage( aLabel[1], WM_SETFONT, hFont1, 1 )
      SendMessage( aLabel[2], WM_SETFONT, hFont2, 1 )
      SendMessage( aLabel[3], WM_SETFONT, hFont1, 1 )
      SendMessage( aLabel[4], WM_SETFONT, hFont2, 1 )
      SendMessage( aLabel[5], WM_SETFONT, hFont1, 1 )
      SendMessage( aLabel[6], WM_SETFONT, hFont2, 1 )

   Case nMsg == WM_SIZE

      // adjust the size of the statusbar
      MoveWindow( hStatusBar, 0, 0, 0, 0, 1 )

   Case nMsg == WM_COMMAND

      Do Case
      // menu
      Case nwParam == MENU_NEW     ; NewGame()
      Case nwParam == MENU_BACK    ; MoveBack()
      Case nwParam == MENU_EXIT    ; DestroyWindow(hWnd)
      Case nwParam == MENU_GAME1   ; SetGame(1)
      Case nwParam == MENU_GAME2   ; SetGame(2)
      Case nwParam == MENU_GAME3   ; SetGame(3)
      Case nwParam == MENU_GAME4   ; SetGame(4)
      Case nwParam == MENU_GAME5   ; SetGame(5)
      Case nwParam == MENU_GAME6   ; SetGame(6)
      Case nwParam == MENU_GAME7   ; SetGame(7)
      Case nwParam == MENU_GAME8   ; SetGame(8)
      Case nwParam == MENU_HELP    ; HowToPlay()
      Case nwParam == MENU_ABOUT   ; About(hWnd)
      // toolbar
      Case nwParam == BUTTON_NEW   ; NewGame()
      Case nwParam == BUTTON_BACK  ; MoveBack()
      Case nwParam == BUTTON_GAME1 ; SetGame(1)
      Case nwParam == BUTTON_GAME2 ; SetGame(2)
      Case nwParam == BUTTON_GAME3 ; SetGame(3)
      Case nwParam == BUTTON_GAME4 ; SetGame(4)
      Case nwParam == BUTTON_GAME5 ; SetGame(5)
      Case nwParam == BUTTON_GAME6 ; SetGame(6)
      Case nwParam == BUTTON_GAME7 ; SetGame(7)
      Case nwParam == BUTTON_GAME8 ; SetGame(8)
      Case nwParam == BUTTON_HELP  ; HowToPlay()
      Case nwParam == BUTTON_ABOUT ; About(hWnd)
      Case nwParam == BUTTON_EXIT  ; DestroyWindow(hWnd)
      // board
      Case nwParam >= 201 .And. nwParam <= 249 ; BoardClick(nwParam)
      EndCase

   Case nMsg == WM_DESTROY

      For i := 1 To 16
         DeleteObject( aImages[i] )
      Next i
      DeleteObject(hFont1)
      DeleteObject(hFont2)
      PostQuitMessage(0)

   EndCase

   Return DefWindowProc( hWnd, nMsg, nwParam, nlParam )

//---------------------------------------------------------------------------//

Static Function PrepareMenu ()

   Local hMenu
   Local hPopupMenu

   hMenu := CreateMenu()
   // Game
   hPopupMenu := CreatePopupMenu()
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_NEW  , "&New"               )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_BACK , "&Back"              )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_EXIT , "&Exit"              )
   AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu, "&Game"              )
   // Options
   hPopupMenu := CreatePopupMenu()
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME1, "&Cross"             )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME2, "&Plus"              )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME3, "&Fireplace"         )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME4, "&Plane"             )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME5, "&Up Arrow"          )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME6, "&Pyramid"           )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME7, "&Diamond"           )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_GAME8, "&Solitaire"         )
   AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu, "&Options"           )
   // Help
   hPopupMenu := CreatePopupMenu()
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_HELP , "&Howto play"        )
   AppendMenu( hPopupMenu, MF_ENABLED + MF_STRING, MENU_ABOUT, "&About the program" )
   AppendMenu( hMenu     , MF_ENABLED + MF_POPUP , hPopupMenu, "&Help"              )

   Return hMenu

//---------------------------------------------------------------------------//

Static Function SetStyle ( nHandle, nStyle, lAdd ) // code adapted from WHOO

   lAdd:=If(lAdd==Nil,TRUE,lAdd)

   SetWindowLong(nHandle,GWL_STYLE,;
      IIF(lAdd,OR(GetWindowLong(nHandle,GWL_STYLE),nStyle ),AND(GetWindowLong(nHandle,GWL_STYLE),NOT(nStyle))))

   Return Nil

//---------------------------------------------------------------------------//

Static Function SetExStyle ( nHandle, nStyle, lAdd ) // code adapted from WHOO

   lAdd:=if(lAdd==Nil,TRUE,lAdd)

   SetWindowLong(nHandle,GWL_EXSTYLE,;
      IIF(lAdd,OR(GetWindowLong(nHandle,GWL_EXSTYLE),nStyle ),AND(GetWindowLong(nHandle,GWL_EXSTYLE),NOT(nStyle))))

   Return Nil

//---------------------------------------------------------------------------//

Static Function SetGame( n )

    // uncheck menu item
    Do Case
    Case nGame == 1 ; CheckMenuItem( hMenu, MENU_GAME1, MF_UNCHECKED )
    Case nGame == 2 ; CheckMenuItem( hMenu, MENU_GAME2, MF_UNCHECKED )
    Case nGame == 3 ; CheckMenuItem( hMenu, MENU_GAME3, MF_UNCHECKED )
    Case nGame == 4 ; CheckMenuItem( hMenu, MENU_GAME4, MF_UNCHECKED )
    Case nGame == 5 ; CheckMenuItem( hMenu, MENU_GAME5, MF_UNCHECKED )
    Case nGame == 6 ; CheckMenuItem( hMenu, MENU_GAME6, MF_UNCHECKED )
    Case nGame == 7 ; CheckMenuItem( hMenu, MENU_GAME7, MF_UNCHECKED )
    Case nGame == 8 ; CheckMenuItem( hMenu, MENU_GAME8, MF_UNCHECKED )
    EndCase

    nGame := n

    // check menu item
    Do Case
    Case nGame == 1 ; CheckMenuItem( hMenu, MENU_GAME1, MF_CHECKED )
    Case nGame == 2 ; CheckMenuItem( hMenu, MENU_GAME2, MF_CHECKED )
    Case nGame == 3 ; CheckMenuItem( hMenu, MENU_GAME3, MF_CHECKED )
    Case nGame == 4 ; CheckMenuItem( hMenu, MENU_GAME4, MF_CHECKED )
    Case nGame == 5 ; CheckMenuItem( hMenu, MENU_GAME5, MF_CHECKED )
    Case nGame == 6 ; CheckMenuItem( hMenu, MENU_GAME6, MF_CHECKED )
    Case nGame == 7 ; CheckMenuItem( hMenu, MENU_GAME7, MF_CHECKED )
    Case nGame == 8 ; CheckMenuItem( hMenu, MENU_GAME8, MF_CHECKED )
    EndCase

    NewGame()

    Return Nil

//---------------------------------------------------------------------------//

Static Function GetRow ( nIndex ) // convert index (1-49) to row (1-7)

   Local nRow := 0

   Do Case
   Case nIndex >= 01 .And. nIndex <= 07 ; nRow := 1
   Case nIndex >= 08 .And. nIndex <= 14 ; nRow := 2
   Case nIndex >= 15 .And. nIndex <= 21 ; nRow := 3
   Case nIndex >= 22 .And. nIndex <= 28 ; nRow := 4
   Case nIndex >= 29 .And. nIndex <= 35 ; nRow := 5
   Case nIndex >= 36 .And. nIndex <= 42 ; nRow := 6
   Case nIndex >= 43 .And. nIndex <= 49 ; nRow := 7
   EndCase

   Return nRow

//---------------------------------------------------------------------------//

Static Function GetCol ( nIndex ) // convert index (1-49) to column (1-7)

   Local nCol := 0

   Do Case
   Case StrZero( nIndex, 2 ) $ "01-08-15-22-29-36-43"; nCol := 1
   Case StrZero( nIndex, 2 ) $ "02-09-16-23-30-37-44"; nCol := 2
   Case StrZero( nIndex, 2 ) $ "03-10-17-24-31-38-45"; nCol := 3
   Case StrZero( nIndex, 2 ) $ "04-11-18-25-32-39-46"; nCol := 4
   Case StrZero( nIndex, 2 ) $ "05-12-19-26-33-40-47"; nCol := 5
   Case StrZero( nIndex, 2 ) $ "06-13-20-27-34-41-48"; nCol := 6
   Case StrZero( nIndex, 2 ) $ "07-14-21-28-35-42-49"; nCol := 7
   EndCase

   Return nCol

//---------------------------------------------------------------------------//

Static Function NewGame ()

   Local i := 0

   // aBoard [n,1] :=  0   [empty]
   // aBoard [n,1] := -1   [corner]
   // aBoard [n,1] :=  1   [occupied]

   For i := 1 To 49
      aBoard[i,1] := 0
   Next i

   aBoard[01,1] := -1
   aBoard[02,1] := -1
   aBoard[08,1] := -1
   aBoard[09,1] := -1
   aBoard[06,1] := -1
   aBoard[07,1] := -1
   aBoard[13,1] := -1
   aBoard[14,1] := -1
   aBoard[36,1] := -1
   aBoard[37,1] := -1
   aBoard[43,1] := -1
   aBoard[44,1] := -1
   aBoard[41,1] := -1
   aBoard[42,1] := -1
   aBoard[48,1] := -1
   aBoard[49,1] := -1

   Do Case

   Case nGame == 1 // cross

      aBoard[11,1] := 1
      aBoard[17,1] := 1
      aBoard[18,1] := 1
      aBoard[19,1] := 1
      aBoard[25,1] := 1
      aBoard[32,1] := 1

      nTotal := 6

   Case nGame == 2 // plus

      aBoard[11,1] := 1
      aBoard[18,1] := 1
      aBoard[23,1] := 1
      aBoard[24,1] := 1
      aBoard[25,1] := 1
      aBoard[26,1] := 1
      aBoard[27,1] := 1
      aBoard[32,1] := 1
      aBoard[39,1] := 1

      nTotal := 9

   Case nGame == 3 // fireplace

      aBoard[03,1] := 1
      aBoard[04,1] := 1
      aBoard[05,1] := 1
      aBoard[10,1] := 1
      aBoard[11,1] := 1
      aBoard[12,1] := 1
      aBoard[17,1] := 1
      aBoard[18,1] := 1
      aBoard[19,1] := 1
      aBoard[24,1] := 1
      aBoard[26,1] := 1

      nTotal := 11

   Case nGame == 4 // plane

      aBoard[11,1] := 1
      aBoard[18,1] := 1
      aBoard[22,1] := 1
      aBoard[23,1] := 1
      aBoard[24,1] := 1
      aBoard[25,1] := 1
      aBoard[26,1] := 1
      aBoard[27,1] := 1
      aBoard[28,1] := 1
      aBoard[29,1] := 1
      aBoard[30,1] := 1
      aBoard[31,1] := 1
      aBoard[32,1] := 1
      aBoard[33,1] := 1
      aBoard[34,1] := 1
      aBoard[35,1] := 1
      aBoard[39,1] := 1
      aBoard[45,1] := 1
      aBoard[46,1] := 1
      aBoard[47,1] := 1

      nTotal := 20

   Case nGame == 5 // up arrow

      aBoard[04,1] := 1
      aBoard[10,1] := 1
      aBoard[11,1] := 1
      aBoard[12,1] := 1
      aBoard[16,1] := 1
      aBoard[17,1] := 1
      aBoard[18,1] := 1
      aBoard[19,1] := 1
      aBoard[20,1] := 1
      aBoard[25,1] := 1
      aBoard[32,1] := 1
      aBoard[38,1] := 1
      aBoard[39,1] := 1
      aBoard[40,1] := 1
      aBoard[45,1] := 1
      aBoard[46,1] := 1
      aBoard[47,1] := 1

      nTotal := 17

   Case nGame == 6 // pyramid

      aBoard[11,1] := 1
      aBoard[17,1] := 1
      aBoard[18,1] := 1
      aBoard[19,1] := 1
      aBoard[23,1] := 1
      aBoard[24,1] := 1
      aBoard[25,1] := 1
      aBoard[26,1] := 1
      aBoard[27,1] := 1
      aBoard[29,1] := 1
      aBoard[30,1] := 1
      aBoard[31,1] := 1
      aBoard[32,1] := 1
      aBoard[33,1] := 1
      aBoard[34,1] := 1
      aBoard[35,1] := 1

      nTotal := 16

   Case nGame == 7 // diamond

      aBoard[04,1] := 1
      aBoard[10,1] := 1
      aBoard[11,1] := 1
      aBoard[12,1] := 1
      aBoard[16,1] := 1
      aBoard[17,1] := 1
      aBoard[18,1] := 1
      aBoard[19,1] := 1
      aBoard[20,1] := 1
      aBoard[22,1] := 1
      aBoard[23,1] := 1
      aBoard[24,1] := 1
      aBoard[26,1] := 1
      aBoard[27,1] := 1
      aBoard[28,1] := 1
      aBoard[30,1] := 1
      aBoard[31,1] := 1
      aBoard[32,1] := 1
      aBoard[33,1] := 1
      aBoard[34,1] := 1
      aBoard[38,1] := 1
      aBoard[39,1] := 1
      aBoard[40,1] := 1
      aBoard[46,1] := 1

      nTotal := 24

   Case nGame == 8 // solitaire

      For i := 1 To 49
         aBoard[i,1] := 1
      Next i

      aBoard[01,1] := -1
      aBoard[02,1] := -1
      aBoard[08,1] := -1
      aBoard[09,1] := -1
      aBoard[06,1] := -1
      aBoard[07,1] := -1
      aBoard[13,1] := -1
      aBoard[14,1] := -1
      aBoard[36,1] := -1
      aBoard[37,1] := -1
      aBoard[43,1] := -1
      aBoard[44,1] := -1
      aBoard[41,1] := -1
      aBoard[42,1] := -1
      aBoard[48,1] := -1
      aBoard[49,1] := -1

      aBoard[25,1] := 0

      nTotal := 32

   EndCase

   // draw the board
   For i := 1 To 49
      // empty
      If aBoard[i,1] == 0
         SendMessage( aBoard[i,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_EMPTY] )
      EndIf
      // occupied
      If aBoard[i,1] == 1
         SendMessage( aBoard[i,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_DEFAULT] )
      EndIf
      // occupied/marked to move
      If aBoard[i,1] == 2
         SendMessage( aBoard[i,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_MARKED] )
      EndIf
   Next i

   nMarked := -1

   nRemoved := 0

   // update labels
   SetWindowText( aLabel[2], AllTrim(Str(nTotal)) )
   SetWindowText( aLabel[4], AllTrim(Str(0))     )
   SetWindowText( aLabel[6], AllTrim(Str(nTotal)) )

   Return Nil

//---------------------------------------------------------------------------//

Static Function BoardClick ( nIndex )

   Local nDirection := 0   // -2:left +2:right -14:up +14:down
   Local lInvalid := FALSE // TRUE:invalid move FALSE:valid move
   Local nEmpty := 0

   nIndex := nIndex - 200 // convert nIndex to range 1-49

   If aBoard[nIndex,1] == 0 .And. nMarked <> -1
      nDirection := nIndex - nMarked
      // check for a valid move
      If Abs(nDirection) <> 14 .And. Abs(nDirection) <> 2
         lInvalid := TRUE
      EndIf
      If Abs(nDirection) == 14 .And. GetCol(nMarked) <> GetCol(nIndex)
         lInvalid := TRUE
      EndIf
      If Abs(nDirection) == 2 .And. GetRow(nMarked) <> GetRow(nIndex)
         lInvalid := TRUE
      EndIf
      If     nDirection == 14  // down
         If aBoard[nMarked+7,1] == 0
            lInvalid := TRUE
         EndIf
      ElseIf nDirection == -14 // up
         If aBoard[nMarked-7,1] == 0
            lInvalid := TRUE
         EndIf
      ElseIf nDirection == 2   // right
         If aBoard[nMarked+1,1] == 0
            lInvalid := TRUE
         EndIf
      ElseIf nDirection == -2  // left
         If aBoard[nMarked-1,1] = 0
            lInvalid := TRUE
         EndIf
      EndIf
      // move piece if the move is valid
      If lInvalid == FALSE
         // move from position [nMarked] to position [nIndex]
         aBoard[nMarked,1] := 0
         aBoard[nIndex,1] := 1
         // update the board
         SendMessage( aBoard[nMarked,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_EMPTY] )
         SendMessage( aBoard[nIndex,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_DEFAULT] )
         // get the piece that was jumped
         Do Case
         Case nDirection == 14  ; nEmpty := nMarked + 7
         Case nDirection == -14 ; nEmpty := nMarked - 7
         Case nDirection == 2   ; nEmpty := nMarked + 1
         Case nDirection == -2  ; nEmpty := nMarked - 1
         EndCase
         // remove the piece
         aBoard[nEmpty,1] := 0
         // update the board
         SendMessage( aBoard[nEmpty,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_EMPTY] )
         //
         nRemoved := nRemoved + 1
         // add move to array of moves
         aMoves[nRemoved,1] := nMarked
         aMoves[nRemoved,2] := nEmpty
         aMoves[nRemoved,3] := nIndex
         // update labels
         SetWindowText( aLabel[4], AllTrim(Str(nRemoved)) )
         SetWindowText( aLabel[6], AllTrim(Str(nTotal-nRemoved)) )
         PlaySound( "WAV_JUMP", GetModuleHandle(), SND_ASYNC+SND_RESOURCE )
         // check if the user win
         If (nTotal - nRemoved) == 1
            PlaySound( "WAV_WIN", GetModuleHandle(), SND_ASYNC+SND_RESOURCE )
         EndIf
         nMarked := -1
      EndIf
   ElseIf aBoard[nIndex,1] == 1 .And. nMarked == -1
      // user selected a piece
      PlaySound( "WAV_MARK", GetModuleHandle(), SND_ASYNC+SND_RESOURCE )
      nMarked := nIndex
      aBoard[nIndex,1] := 2
      SendMessage( aBoard[nIndex,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_MARKED] )
   ElseIf aBoard[nIndex,1] == 2
      // user deselected a piece
      nMarked := -1
      aBoard[nIndex,1] := 1
      SendMessage( aBoard[nIndex,2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_DEFAULT] )
   EndIf

   Return Nil

//---------------------------------------------------------------------------//

Static Function MoveBack ()

   If nRemoved > 0 .And. nMarked == -1
      SendMessage( aBoard[aMoves[nRemoved,1],2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_DEFAULT] )
      aBoard[aMoves[nRemoved,1],1] := 1
      SendMessage( aBoard[aMoves[nRemoved,2],2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_DEFAULT] )
      aBoard[aMoves[nRemoved,2],1] := 1
      SendMessage( aBoard[aMoves[nRemoved,3],2], BM_SETIMAGE, IMAGE_BITMAP, aImages[IMAGE_EMPTY] )
      aBoard[aMoves[nRemoved,3],1] := 0
      nRemoved := nRemoved - 1
      SetWindowText( aLabel[4], AllTrim(Str(nRemoved)) )
      SetWindowText( aLabel[6], AllTrim(Str(nTotal-nRemoved)) )
      nMarked := -1
      PlaySound( "WAV_BACK", GetModuleHandle(), SND_ASYNC + SND_RESOURCE )
   EndIf

   Return Nil

//---------------------------------------------------------------------------//

Static Function HowToPlay ()

   ShellExecute(GetActiveWindow(),"open","pegged.wri",,,5)

   Return Nil

//---------------------------------------------------------------------------//

Static Function About ( hParent )

   DialogBox(,"about",hParent,{|hDlg,nMsg,nwParam,nlParam|AboutProc(hDlg,nMsg,nwParam,nlParam)})

   Return Nil

//---------------------------------------------------------------------------//

Static Function AboutProc ( hDlg, nMsg, nwParam, nlparam )

   Local hFontBI

   Do Case
   Case nMsg == WM_COMMAND
      If nwParam == IDOK
         EndDialog( hDlg, 0 )
      EndIf
   Case nMsg == WM_INITDIALOG
      hFontBI := CreateFont(-10,0,0,0,600,1,0,0,DEFAULT_CHARSET,OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,DEFAULT_QUALITY,FF_DONTCARE,"Arial")
      SendDlgItemMessage( hDlg, 101, WM_SETFONT, hFontBI, 1 )
      SendDlgItemMessage( hDlg, 103, WM_SETFONT, hFontBI, 1 )
      SendDlgItemMessage( hDlg, 107, WM_SETFONT, hFontBI, 1 )
      SendDlgItemMessage( hDlg, 110, WM_SETFONT, hFontBI, 1 )
      SendDlgItemMessage( hDlg, 114, WM_SETFONT, hFontBI, 1 )
   Case nMsg == WM_SYSCOMMAND
      If nwParam == SC_CLOSE
         EndDialog( hDlg, 0 )
      EndIf
   Case nMsg == WM_DESTROY
      DeleteObject( hFontBI )
   EndCase

   Return 0

//---------------------------------------------------------------------------//

#pragma BEGINDUMP

#include <windows.h>
#include <mmsystem.h>
#include <hbapi.h>

HB_FUNC( PLAYSOUND )
{
 hb_retl( PlaySound( hb_parc(1), GetModuleHandle(NULL), SND_ASYNC | SND_RESOURCE ) );
}

HB_FUNC( CENTERWINDOW )
{
 RECT rect;
 GetWindowRect( (HWND) hb_parnl(1), &rect );
 SetWindowPos( (HWND) hb_parnl(1), HWND_TOP,
               (GetSystemMetrics(SM_CXSCREEN) - (rect.right - rect.left)) / 2,
               (GetSystemMetrics(SM_CYSCREEN) - (rect.bottom - rect.top)) / 2,
               0, 0, SWP_NOSIZE );
}

#pragma ENDDUMP

