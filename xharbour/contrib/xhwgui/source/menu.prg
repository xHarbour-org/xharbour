/*
 * HWGUI - Harbour Win32 GUI library source code:
 * Prg level menu functions
 *
 * Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.geocities.com/alkresin/
*/

#include "windows.ch"
#include "HBClass.ch"
#include "guilib.ch"
#define  MENU_FIRST_ID   32000
#define  CONTEXTMENU_FIRST_ID   32900

STATIC _aMenuDef, _oWnd, _aAccel, _nLevel, _Id, _oMenu

CLASS HMenu INHERIT HObject
   DATA handle
   DATA aMenu 
   METHOD New()  INLINE Self
   METHOD End()  INLINE Hwg_DestroyMenu(::handle)
   METHOD Show( oWnd,xPos,yPos,lWnd )
ENDCLASS

METHOD Show( oWnd,xPos,yPos,lWnd ) CLASS HMenu
Local aCoor

   oWnd:oPopup := Self
   IF Pcount() == 1 .OR. lWnd == Nil .OR. !lWnd
      IF Pcount() == 1
         aCoor := hwg_GetCursorPos()
         xPos  := aCoor[1]
         yPos  := aCoor[2]
      ENDIF
      Hwg_trackmenu( ::handle,xPos,yPos,oWnd:handle )
   ELSE
      aCoor := ClientToScreen( oWnd:handle,xPos,yPos )
      Hwg_trackmenu( ::handle,aCoor[1],aCoor[2],oWnd:handle )
   ENDIF

Return Nil

Function Hwg_CreateMenu
Local hMenu

   IF ( hMenu := hwg__CreateMenu() ) == 0
      Return Nil
   ENDIF

Return { {},,, hMenu }

Function Hwg_SetMenu( oWnd, aMenu )
   IF hwg__SetMenu( oWnd:handle, aMenu[5] )
      oWnd:menu := aMenu
   ELSE
      Return .F.
   ENDIF
Return .T.

/*
 *  AddMenuItem( aMenu,cItem,nMenuId,lSubMenu,[bItem] [,nPos [,lPos]] ) --> aMenuItem
 *
 *  If nPos is omitted, the function adds menu item to the end of menu,
 *  else if lPos is omitted or TRUE, it inserts menu item in nPos position,
 *  else if lPos is FALSE - before item with ID == nPos
 */
Function Hwg_AddMenuItem( aMenu,cItem,nMenuId,lSubMenu,bItem,nPos,lPos )
Local hSubMenu

   IF nPos == Nil
      nPos := Len( aMenu[1] ) + 1
      lPos := .T.
   ELSEIF lPos == Nil
      lPos := .F.
   ENDIF
   IF !lPos
      IF ( aMenu := Hwg_FindMenuItem( aMenu, nMenuId, @nPos ) ) == Nil
         Return Nil
      ENDIF
   ENDIF
   hSubMenu := aMenu[5]
   hSubMenu := hwg__AddMenuItem( hSubMenu, cItem, nPos, .T., nMenuId,,lSubMenu )
   /*
   IF !hwg__AddMenuItem( hSubMenu, cItem, nPos, .T., nMenuId )
      Return Nil
   ENDIF
   IF lSubmenu
      IF ( hSubMenu := hwg__CreateSubMenu( hSubMenu,nMenuId ) ) == 0
         Return Nil
      ENDIF
   ENDIF
   */
   IF nPos > Len( aMenu[1] )
      IF lSubmenu
         Aadd( aMenu[1],{ {},cItem,nMenuId,hSubMenu } )
      ELSE
         Aadd( aMenu[1],{ bItem,cItem,nMenuId } )
      ENDIF
      Return ATail( aMenu[1] )
   ELSE
      Aadd( aMenu[1],Nil )
      Ains( aMenu[1],nPos )
      IF lSubmenu
         aMenu[ 1,nPos ] := { {},cItem,nMenuId,hSubMenu }
      ELSE
         aMenu[ 1,nPos ] := { bItem,cItem,nMenuId }
      ENDIF
      Return aMenu[ 1,nPos ]
   ENDIF
Return Nil

Function Hwg_FindMenuItem( aMenu, nId, nPos )
Local nPos1, aSubMenu
   nPos := 1
   DO WHILE nPos <= Len( aMenu[1] )
      IF aMenu[ 1,npos,3 ] == nId
         Return aMenu
      ELSEIF Len( aMenu[ 1,npos ] ) > 4
         IF ( aSubMenu := Hwg_FindMenuItem( aMenu[ 1,nPos ] , nId, @nPos1 ) ) != Nil
            nPos := nPos1
            Return aSubMenu
         ENDIF
      ENDIF
      nPos ++
   ENDDO
Return Nil

Function Hwg_GetSubMenuHandle( aMenu,nId )
Local nPos
   IF ( aMenu := Hwg_FindMenuItem( aMenu, nId, nPos ) ) != Nil
      Return aMenu[1,nPos,5]
   ENDIF
Return 0

Function BuildMenu( aMenuInit, hWnd, oWnd, nPosParent,lPopup )
Local hMenu, nPos, aMenu

   IF nPosParent == Nil   
      IF lPopup == Nil .OR. !lPopup
         hMenu := hwg__CreateMenu()
      ELSE
         hMenu := hwg__CreatePopupMenu()
      ENDIF
      aMenu := { aMenuInit,,,,hMenu }
   ELSE
      hMenu := aMenuInit[5]
      nPos := Len( aMenuInit[1] )
      aMenu := aMenuInit[ 1,nPosParent ]
      hMenu := hwg__AddMenuItem( hMenu, aMenu[2], nPos+1, .T., aMenu[3],aMenu[4],.T. )
      /*
      hwg__AddMenuItem( hMenu, aMenu[2], nPos+1, .T., aMenu[3] )
      hMenu := hwg__CreateSubMenu( hMenu,aMenu[3] )
      */
      IF Len( aMenu ) < 5
         Aadd( aMenu,hMenu )
      ELSE
         aMenu[5] := hMenu
      ENDIF
   ENDIF

   nPos := 1
   DO WHILE nPos <= Len( aMenu[1] )
      IF Valtype( aMenu[ 1,nPos,1 ] ) == "A"
         BuildMenu( aMenu,,,nPos )
      ELSE
         hwg__AddMenuItem( hMenu, aMenu[1,npos,2], nPos, .T., aMenu[1,nPos,3], ;
                   aMenu[1,npos,4],.F. )
      ENDIF
      nPos ++
   ENDDO
   IF hWnd != Nil .AND. oWnd != Nil
      Hwg_SetMenu( oWnd, aMenu )
   ELSEIF _oMenu != Nil
      _oMenu:handle := aMenu[5]
      _oMenu:aMenu := aMenu
   ENDIF
Return Nil

Function Hwg_BeginMenu( oWnd,nId,cTitle )
Local aMenu, i
   IF oWnd != Nil
      _aMenuDef := {}
      _aAccel   := {}
      _oWnd     := oWnd
      _oMenu    := Nil
      _nLevel   := 0
      _Id       := Iif( nId == Nil, MENU_FIRST_ID, nId )
   ELSE
      nId   := Iif( nId == Nil, ++ _Id, nId )
      aMenu := _aMenuDef
      FOR i := 1 TO _nLevel
         aMenu := Atail(aMenu)[1]
      NEXT
      _nLevel++
      Aadd( aMenu, { {},cTitle,nId,.T. } )
   ENDIF
Return .T.

Function Hwg_ContextMenu()
   _aMenuDef := {}
   _oWnd := Nil
   _nLevel := 0
   _Id := CONTEXTMENU_FIRST_ID
   _oMenu := HMenu():New()
Return _oMenu

Function Hwg_EndMenu()
   IF _nLevel > 0
      _nLevel --
   ELSE
      BuildMenu( Aclone(_aMenuDef), Iif( _oWnd!=Nil,_oWnd:handle,Nil ), ;
                   _oWnd,,Iif( _oWnd!=Nil,.F.,.T. ) )
      IF _oWnd != Nil .AND. _aAccel != Nil .AND. !Empty( _aAccel )
         _oWnd:hAccel := CreateAcceleratorTable( _aAccel )
      ENDIF
      _aMenuDef := Nil
      _aAccel   := Nil
      _oWnd     := Nil
      _oMenu    := Nil
   ENDIF
Return .T.

Function Hwg_DefineMenuItem( cItem, nId, bItem, lDisabled, accFlag, accKey )
Local aMenu, i
   aMenu := _aMenuDef
   FOR i := 1 TO _nLevel
      aMenu := Atail(aMenu)[1]
   NEXT
   nId := Iif( nId == Nil .AND. cItem != Nil, ++ _Id, nId )
   Aadd( aMenu, { bItem,cItem,nId,Iif(lDisabled==Nil,.T.,!lDisabled) } )
   IF accFlag != Nil .AND. accKey != Nil
      Aadd( _aAccel, { accFlag,accKey,nId } )
   ENDIF
Return .T.
