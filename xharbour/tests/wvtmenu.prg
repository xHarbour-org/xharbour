/*
 * $Id: wvtmenu.prg,v 1.0 2004/06/11 21:54:04 peterrees Exp $
 */

/*
 * Harbour Project source code:
 * WVT Functions at Prg Level
 *
 * Copyright 2004 Peter Rees, Rees Software & Systems Ltd <peter@rees.co.nz>
 * www - http://www.xharbour.org http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
#include "hbclass.ch"

// Defines from winuser.h
#define MF_BYPOSITION       1024
#define MF_SEPARATOR        2048
#define MF_ENABLED          0
#define MF_GRAYED           1
#define MF_STRING           0
#define MF_POPUP            16

#define FORM_A4 9

// wvtmenu defines
#define WVT_MENU_TYPE       1
#define WVT_MENU_IDENTIFIER 2
#define WVT_MENU_CAPTION    3
#define WVT_MENU_ACTION     4
#define WVT_MENU_MENUOBJ    4

GLOBAL g_oMenuBar:= NIL  // The menu is created global so it can be disabled etc....


FUNCTION Main()
  LOCAL GetList:= {}, c:= SPACE( 10 )
  CLS
  @ 10,0 SAY 'Select an item from the menu above ' GET c PICT '@!'
  READ
  RETURN(NIL)

INIT FUNCTION AppSetup()
  LOCAL nWidth
  SetShutDownEvent(2048)
  SetKey(2048, { || ReShutDown()} )
  SetCloseEvent(2049)
  SetKey(2049, { || ReAppQuit() } )
  WVT_SETALTF4CLOSE( .T. )  // Allow Alt+F4 key to close app
  setMouse(.T.)
  SetCursor(0)
  SET COLOR TO W+/B, W+/R,,,N/GR
  SetMode(32,98)
  nWidth := Wvt_GetScreenWidth()
  Wvt_SetCodePage(255)  // #define OEM_CHARSET 255 - from wingdi.h
  DO CASE
  CASE nWidth >= 1024
    Wvt_SetFont('Terminal',18,10)
  CASE nWidth >= 800
    Wvt_SetFont('System',16,-8)
  OTHERWISE
    Wvt_SetFont('Terminal',12,6)
  ENDCASE
  Wvt_SetMenu( MenuCreate():hMenu )
  SetKey(Wvt_SetMenuKeyEvent(), { || ActivateMenu() })
  CLS
  RETURN( .T. )

FUNCTION ReAppQuit()
  LOCAL nButton:= WVT_MESSAGEBOX( "Do you really want to quit ?", "Quit ", 4 )// MB_YESNO defined in winuser.h
  IF nButton == 6   // IDYES defined in winuser.h
    QUIT
  ENDIF
  RETURN( 0 )

//---- ReShutDown ------------------------

FUNCTION ReShutDown()
  WVT_MESSAGEBOX( "You must shutdown ["+HB_ARGV( 0 )+"] first", HB_ARGV( 0 )+" - Quit " )
  RETURN( 0 )

FUNCTION MB( cDisplay )
  enableMenu( .F. )
  wvt_messageBox( cDisplay, "Message" )
  enableMenu( .T. )
  RETURN( .T. )

FUNCTION menuCreate()
  LOCAL oMenu := wvtMenu():new():create()
  g_oMenuBar:= wvtMenu():new():create()  // oMenuBar is GLOBAL
  oMenu:Caption:= "File"
  oMenu:AddItem( "Print Screen", {|| Mb( "ScreenPrint" ) } )
  oMenu:AddItem( "Screen To ClipBoard", {|| Mb( "ScreenToClip" ) } )
  oMenu:AddItem( "-" )
  oMenu:AddItem( "Exit", {|| ReAppQuit() })
  g_oMenuBar:addItem("",oMenu)

  oMenu := wvtMenu():new():create()
  oMenu:Caption := "Start"
  oMenu:AddItem( "RunAgain", {|| RunAgain() } )
  g_oMenuBar:addItem("",oMenu)

  oMenu := wvtMenu():new():create()
  oMenu:Caption:= "Help"
  oMenu:AddItem( "Content",{|| Mb( "Help" ) } )
  oMenu:AddItem( "-")
  oMenu:AddItem( "About",{|| Mb( "About" ) } )
  g_oMenuBar:addItem( "",oMenu)
  RETURN( g_oMenuBar )

PROCEDURE enableMenu( lOn )
  LOCAL nItem
  IF !EMPTY(g_oMenuBar)
    nItem:= g_oMenuBar:numItems()
    DO WHILE !EMPTY(nItem)
      IF lOn
        g_oMenuBar:enableItem(nItem)
      ELSE
        g_omenuBar:disableItem(nItem)
      ENDIF
      nItem--
    ENDDO
    g_omenuBar:DrawMenuBar()
  ENDIF
  RETURN

FUNCTION ActivateMenu()
  LOCAL nMenu:= Wvt_GetLastMenuEvent()
  LOCAL oMenu, aMenuItem
  IF !EMPTY(nMenu)
    oMenu:= g_oMenuBar
    IF HB_ISOBJECT(oMenu)
      IF !EMPTY(aMenuItem:=oMenu:FindMenuItemById(nMenu))
        IF HB_ISBLOCK(aMenuItem[WVT_MENU_ACTION])
          EVAL(aMenuItem[WVT_MENU_ACTION])
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  RETURN(NIL)

PROCEDURE RunAgain()
  IF WVT_MESSAGEBOX( "Do you really want to Run this program again ?", HB_ARGV( 0 ), 4 ) == 6
     // RunShell( HB_ARGV( 0 ) )
  ENDIF
  RETURN

CLASS wvtMenu

  METHOD Create(cCaption)
  METHOD AddItem(cCaption, bAction)
  METHOD DelAllItems()
  METHOD DelItem(nItem)
  METHOD EnableItem(nItemNum)
  METHOD DisableItem(nItemNum)
  METHOD NumItems()
  METHOD Destroy()
  METHOD GetItem(nItemNum)
  METHOD FindMenuItemById(nId)
  METHOD DrawMenuBar()

  CLASSVAR MenuItemId INIT 1

  VAR aItems
  VAR hMenu
  VAR Caption
  VAR IdNumber

ENDCLASS


METHOD Create(cCaption) CLASS wvtMenu
  ::aItems := {}
  IF EMPTY(::hMenu:= Wvt_CreateMenu())
    Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Init()", "Create Menu Error", { cCaption, cCaption },"WVT.PRG" ) )
  ENDIF
  ::Caption:= IIF(cCaption==NIL,"",cCaption)
  RETURN(Self)

METHOD Destroy() CLASS wvtMenu
  IF !EMPTY(::hMenu)
    ::DelAllItems()
    IF !Wvt_DestroyMenu(::hMenu)
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {},"WVT.PRG" ) )
    ENDIF
    ::hMenu:= 0
  ENDIF
  RETURN(.T.)

METHOD AddItem(cCaption, bAction) CLASS wvtMenu
  LOCAL lResult:= .F., aItem
  IF !EMPTY(::hMenu) .AND. (!EMPTY(cCaption) .OR. !EMPTY(bAction))
    IF HB_ISOBJECT(bAction)
      cCaption:= IIF(!EMPTY(cCaption),cCaption,bAction:Caption)
      aItem:= {MF_POPUP,bAction:hMenu,cCaption,bAction} // bAction is a wvtMenu object reference
    ELSEIF HB_ISBLOCK(bAction)
      aItem:= {MF_STRING,::MenuItemId++,cCaption,bAction} // bAction is a code block to execute
    ELSEIF cCaption[1]=="-"
      aItem:= {MF_SEPARATOR,0,0,NIL}
    ELSE
      Throw( ErrorNew( "wvtMenu", 3101, "wvtMenu:AddItem()", "Argument Error", { cCaption, bAction },"WVT.PRG" ) )
    ENDIF
    IF !Wvt_AppendMenu(::hMenu, aItem[WVT_MENU_TYPE],aItem[WVT_MENU_IDENTIFIER],aItem[WVT_MENU_CAPTION])
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:AddItem()", "Add menu item", { cCaption, bAction },"WVT.PRG" ) )
    ENDIF
    AADD(::aItems, aItem)
    lResult:= .T.
  ENDIF
  RETURN(lResult)

METHOD DelAllItems() CLASS wvtMenu
  LOCAL lResult:= .T.,  nItems
  nItems:= ::NumItems()
  DO WHILE nItems>0 .AND. lResult
    lResult:= ::DelItem( nItems)
    nItems--
  ENDDO
  RETURN(lResult)

METHOD DelItem(nItemNum) CLASS wvtMenu
  LOCAL lResult:= .F.
  IF nItemNum>0 .AND. nItemNum<= ::NumItems()
    IF ::aItems[nItemNum,WVT_MENU_TYPE]== MF_POPUP
      ::aItems[nItemNum,WVT_MENU_MENUOBJ]:Destroy()
    ENDIF
    IF (lResult:= Wvt_DeleteMenu(::hMenu, nItemNum-1,MF_BYPOSITION)) // Remember ZERO base
      ADEL(::aItems,nItemNum)
      ASIZE(::aItems,LEN(::aItems)-1)
    ELSE
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum },"WVT.PRG" ) )
    ENDIF
  ENDIF
  RETURN(lResult)

METHOD EnableItem(nItemNum) CLASS wvtMenu
  LOCAL nPrevious:= -1
  IF !EMPTY(::hMenu) && !EMPTY(nItemNum)
    nPrevious:= Wvt_EnableMenuItem(::hMenu, nItemNum-1, MF_BYPOSITION + MF_ENABLED)
  ENDIF
  RETURN(nPrevious)

METHOD DisableItem(nItemNum) CLASS wvtMenu
  LOCAL nPrevious:= -1
  IF !EMPTY(::hMenu) && !EMPTY(nItemNum)
    nPrevious:= Wvt_EnableMenuItem(::hMenu, nItemNum-1, MF_BYPOSITION + MF_GRAYED)
  ENDIF
  RETURN(nPrevious)

METHOD NumItems() CLASS wvtMenu
  RETURN(LEN(::aItems))

METHOD GetItem(nItemNum) CLASS wvtMenu
  LOCAL nItems:= ::NumItems(), aResult:= NIL
  IF nItemNum>0 .AND. nItemNum<= nItems
    aResult:= ::aItems[nItemNum]
  ENDIF
  RETURN(aResult)

METHOD FindMenuItemById(nId) CLASS wvtMenu
  LOCAL x, aResult:= {}
  IF !EMPTY(nId)
    x:= ::NumItems()
    DO WHILE x> 0 .AND. EMPTY(aResult)
      IF ::aItems[x,WVT_MENU_TYPE] == MF_POPUP
         aResult:= ::aItems[x,WVT_MENU_MENUOBJ]:FindMenuItemById(nId)
      ELSEIF ::aItems[x,WVT_MENU_IDENTIFIER] == nId
        aResult:= ::aItems[x]
      ENDIF
      x--
    ENDDO
  ENDIF
  RETURN(aResult)

METHOD DrawMenuBar() CLASS wvtMenu
  Wvt_DrawMenuBar()
  RETURN( NIL )
