/*
*-----------------------------------------------------------------------------
* WoopGUI for Harbour - Win32 OOP GUI library source code
* Copyright 2002 Francesco Saverio Giudice <info@fsgiudice.com>
*
*-----------------------------------------------------------------------------
* Parts of this project come from:
* "Harbour MiniGUI"
*                   Copyright 2002 Roberto Lopez <roblez@ciudad.com.ar>
*                   http://www.geocities.com/harbour_minigui/
* "Harbour GUI framework for Win32"
*                   Copyright 2001 Alexander S.Kresin <alex@belacy.belgorod.su>
*                   Copyright 2001 Antonio Linares <alinares@fivetech.com>
*                   http://www.harbour-project.org
*-----------------------------------------------------------------------------
*
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "windows.ch"

// Menu definitions
CLASS TMenu FROM TObject
    // Base
    CLASSDATA aoMenus  AS ARRAY INIT {}

    DATA nHandle  //PROTECTED             // Handle

    DATA bWindowProc   AS CODEBLOCK     // Code block to evaluate events

    // Voci di menu controlli associati esclusivamente alla classe
    DATA aoItems INIT {}   HIDDEN
    DATA oParent AS OBJECT HIDDEN

    // METODI
    METHOD New() CONSTRUCTOR
    METHOD AddMenu( oMenu )        INLINE aAdd( ::aoMenus, oMenu )
    METHOD Append()
    METHOD Delete()
    METHOD DelMenu()
    METHOD Destroy()               INLINE ::DelMenu()
    METHOD Check( nID, lCheck )    INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:Check( lCheck ), NIL )
    METHOD Disable( nID )          INLINE ::Enable( nID, FALSE )
    METHOD Enable( nID, lEnable )  INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:Enable( lEnable ), NIL )
    METHOD FindItem()
    METHOD FindItemInAllMenus()
    METHOD FindMenu()
    METHOD GetLabel( nID )         INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:GetLabel(), NIL )
    METHOD GetItems()              INLINE ::aoItems
    METHOD GetItemCount()          INLINE Len( ::aoItems )
    METHOD GetItemFromMenu()
    METHOD GetMenu( nHandle )      INLINE LOCAL nPos,;
                                          nPos := ::FindMenu( nHandle ),;
                                          IIF( nPos > 0, ::aoMenus[ nPos ], NIL )
    METHOD GetState( nID )         INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:GetState(), NIL )
    METHOD GetPosition()
    METHOD GetItemID( nPos )       INLINE GetMenuItemID( ::nHandle, nPos-1 )
    METHOD Gray( nID, lGrayed )    INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:Gray( lGrayed ), NIL )
    METHOD HasParent()             INLINE ( ::oParent <> NIL )
    METHOD Hilite( nID, lHilite )  INLINE LOCAL oItem, ;
                                          oItem := ::FindItem( nID ), ;
                                          IIF( oItem <> NIL, oItem:Hilite( lHilite ), NIL )
    METHOD Insert()
    METHOD IsChecked( nID )        INLINE AND( ::GetState( nID ), MF_CHECKED ) != 0
    METHOD IsEnabled( nID )        INLINE AND( ::GetState( nID ), MF_DISABLED ) == 0
    METHOD IsGrayed( nID )         INLINE AND( ::GetState( nID ), MF_GRAYED ) != 0
    METHOD IsHilite( nID )         INLINE AND( ::GetState( nID ), MF_HILITE ) != 0
    METHOD IsPopup( nID )          INLINE AND( ::GetState( nID ), MF_POPUP ) != 0

    METHOD Modify()
    METHOD Redraw()                INLINE DrawMenuBar( ::nHandle )
    METHOD ReparentChilds()
    METHOD SetParent()
    //METHOD SetTitle( cTitle )      INLINE
    METHOD UnCheck( nID )          INLINE ::Check( nID, FALSE )
    METHOD UnGray( nID )           INLINE ::Gray( nID, FALSE )
    METHOD UnHilite( nID )         INLINE ::Hilite( nID, FALSE )

    METHOD WindowProc()

ENDCLASS

METHOD New() CLASS TMenu

    // Azzero l'array delle voci
    ::aoItems      := {}

RETURN Self

METHOD Append( oItem ) CLASS TMenu
    LOCAL nIDItem
    LOCAL lOk

    aAdd( ::aoItems, oItem )
    nIDItem := iif( ValType( oItem:nIDItem) == "O", oItem:nIDItem:nHandle, oItem:nIDItem )
    lOk := AppendMenu( ::nHandle, oItem:nFlags, nIDItem, oItem:cItem )
    IF lOk
       oItem:SetParent( Self )
    ENDIF

RETURN lOk

METHOD Delete( nPos )  CLASS TMenu          // nPos = Specifies the menu item before
                                               //        which the new menu item is to be inserted
    LOCAL lOk
    LOCAL nIDItem
    LOCAL uFlags := MF_BYPOSITION

    // Add to array
    WG_aShrink( ::aoItems, nPos )
    lOk := DeleteMenu( ::nHandle, nPos-1, uFlags )

RETURN lOk

METHOD DelMenu() CLASS TMenu
   LOCAL nPos := ::FindMenu( ::nHandle )
   WG_DebugTrace( "TMenu:Destroy()", "Self", Self, "::nHandle", ::nHandle )
   IF nPos > 0
      IF !::HasParent()
         WG_DebugTrace( "TMenu:Destroy() - Not Has Parent, delete" )
         DestroyMenu( ::nHandle )
      ELSE
         WG_DebugTrace( "TMenu:Destroy() - Has Parent. Parent will delete this menu" )
      ENDIF
      WG_aShrink( ::aoMenus, nPos )
   ELSE
      // MessageBox(, "Error: Menu not found!" )
      WG_DebugTrace( "TMenu:Destroy() - It's Not a main menu" )
      // It's not a main menu
   ENDIF
RETURN Self

METHOD FindItem( nID ) CLASS TMenu
   LOCAL oItem

   oItem := WG_FindMenuItem( Self, nID )

RETURN oItem

STATIC FUNCTION WG_FindMenuItem( oMenu, nID )
   LOCAL oItem, aoItems
   LOCAL n

   aoItems := oMenu:aoItems

   FOR n := 1 TO Len( aoItems )
       IF ValType( aoItems[n]:nIDItem ) == "O" // There is a submenu
          // Make a recursive call
          IF ( oItem := WG_FindMenuItem( aoItems[n]:nIDItem, nID ) ) <> NIL
             EXIT
          ENDIF
       ELSE
          IF aoItems[n]:nIDItem == nID
             oItem := aoItems[n]
             EXIT
          ENDIF
       ENDIF

   NEXT n

RETURN oItem

METHOD FindItemInAllMenus( nID ) CLASS TMenu
   LOCAL oItem
   LOCAL nPos  := aScan( ::aoMenus, {|m| m:FindItem( nID ) <> NIL } )
   IF nPos > 0
      oItem := ::GetItemFromMenu( ::aoMenus[ nPos ], nID )
   ENDIF
RETURN oItem

METHOD GetItemFromMenu( oMenu, nID ) CLASS TMenu
RETURN oMenu:FindItem( nID )

METHOD FindMenu( nHandle ) CLASS TMenu
   DEFAULT nHandle TO ::nHandle
RETURN aScan( ::aoMenus, {|m| m:nHandle == nHandle } )

METHOD GetPosition( nID ) CLASS TMenu
RETURN aScan( ::aoItems, {|m| IIF( ValType( m:nIDItem ) == "O", FALSE, m:nIDItem == nID ) } )

METHOD Insert( oItem, nPos ) CLASS TMenu  // nPos = Specifies the menu item before
                                             //        which the new menu item is to be inserted
    LOCAL lOk
    LOCAL nIDItem
    LOCAL uFlags

    IF oItem != NIL
       uFlags := oItem:nFlags + MF_BYPOSITION

       // Add to array
       WG_aGrow( ::aoItems, oItem, nPos )
       IF ValType( oItem:nIDItem ) == "O"
          nIDItem := oItem:nIDItem:nHandle
          uFlags += MF_POPUP
       ELSE
          nIDItem := oItem:nIDItem
       ENDIF
       lOk := InsertMenu( ::nHandle, nPos-1, uFlags, nIDItem, oItem:cItem )
       IF lOk
          oItem:SetParent( Self )
       ENDIF
    ELSE
       // Error
       lOk := FALSE
    ENDIF
RETURN lOk

METHOD Modify( oItem, nPos ) CLASS TMenu   // nPos = Specifies the menu item before
                                              //        which the new menu item is to be inserted
    LOCAL lOk
    LOCAL nIDItem
    LOCAL uFlags

    IF oItem != NIL
       uFlags := oItem:nFlags + MF_BYPOSITION

       // Add to array
       ::aoItems[ nPos ] := oItem
       IF ValType( oItem:nIDItem ) == "O"
          nIDItem := oItem:nIDItem:nHandle
          uFlags += MF_POPUP
       ELSE
          nIDItem := oItem:nIDItem
       ENDIF
       lOk := ModifyMenu( ::nHandle, nPos-1, uFlags, nIDItem, oItem:cItem )
       IF lOk
          oItem:SetParent( Self )
       ENDIF
    ELSE
       // Error
       lOk := FALSE
    ENDIF
RETURN lOk

METHOD ReparentChilds() CLASS TMenu
   WG_ReparentMenuItem( Self, Self )
RETURN Self

STATIC FUNCTION WG_ReparentMenuItem( oMenu, oParentMenu )
   LOCAL oItem, aoItems
   LOCAL n

   aoItems := oMenu:aoItems

   FOR n := 1 TO Len( aoItems )
       IF ValType( aoItems[n]:nIDItem ) == "O" // There is a submenu
          //// Make a recursive call
          //WG_ReparentMenuItem( aoItems[n]:nIDItem, oParentMenu )
          //EXIT
       ELSE
          aoItems[n]:SetParent( oParentMenu )
       ENDIF

   NEXT n

RETURN NIL

METHOD SetParent( oWnd AS OBJECT ) CLASS TMenu
   LOCAL oOldParent := ::oParent
   IF oWnd <> NIL
      ::oParent := oWnd
   ENDIF
RETURN oOldParent

METHOD WindowProc( nMsg, wParam, lParam ) CLASS TMenu
   LOCAL nRet := -1
   LOCAL wmId, wmEvent, wmHandle, wmFlag, wmPos
   LOCAL oMenu, oItem
   //MessageBox( , "Passato da menu" )
   //ParamDisplay( , hb_aparams() )
   IF ValType( ::bWindowProc ) == "B"
      //MessageBox( , "Passato da bWindowProc" )
      // User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
   ENDIF
   IF nRet == -1
      // Class event handler
      DO CASE
         CASE nMsg == WM_COMMAND
              WG_DebugTrace( "TMenu:WindowProc() - WM_COMMAND" )
              wmId    = LOWORD(wParam) // menu identifier
              wmEvent = HIWORD(wParam) // 0 = menu, 1 = accelerator
              wmHandle = lParam        // control handle

              IF wmEvent == 0    // Command from menu or window
                 IF !IsWindow( wmHandle ) // Is from a menu
                    oItem := ::FindItemInAllMenus( wmId )
                    IF oItem <> NIL
                       // Write event to events.log
                       WG_ApplObj():MenuEventsWrite( oItem )
                       IF oItem:HasAction()
                          //MessageBox( , "Passato" )
                          oItem:ExecAction( ::oParent )
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
              nRet := 0

         CASE nMsg == WM_CONTEXTMENU
              WG_DebugTrace( "TMenu:WindowProc() - WM_CONTEXTMENU" )

         CASE nMsg == WM_ENTERMENULOOP
              WG_DebugTrace( "TMenu:WindowProc() - WM_ENTERMENULOOP" )

         CASE nMsg == WM_EXITMENULOOP
              WG_DebugTrace( "TMenu:WindowProc() - WM_EXITMENULOOP" )

         CASE nMsg == WM_MENUCOMMAND  // Only for Win98 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_MENUCOMMAND" )
         //     //MessageBox( , "Passato da menu" )
         //
         //     wmPos    = wParam // menu position
         //     wmHandle = lParam        // control handle
         //
         //     //MessageBox( , "Passato da menu" )
         //     MessageBox( , "wmHandle = " + cStr( wmHandle ) )
         //     oMenu := ::GetMenu( wmHandle )
         //     oItem := oMenu:GetItems()[wmPos+1]
         //     IF oItem <> NIL
         //        // Write event to events.log
         //        WG_ApplObj():MenuEventsWrite( oItem )
         //        IF oItem:HasAction()
         //           //MessageBox( , "Passato" )
         //           oItem:ExecAction( ::oParent )
         //        ENDIF
         //     ENDIF
         //     nRet := 0

         CASE nMsg == WM_MENUDRAG          // Only for Win98 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_MENUDRAG" )

         CASE nMsg == WM_MENUGETOBJECT     // Only for Win98 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_MENUGETOBJECT" )

         CASE nMsg == WM_MENURBUTTONUP     // Only for Win98 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_MENURBUTTONUP" )

         CASE nMsg == WM_MENUSELECT
              WG_DebugTrace( "TMenu:WindowProc() - WM_MENUSELECT" )
              wmId     = LOWORD(wParam) // menu identifier
              wmFlag   = HIWORD(wParam) // FLAGS
              wmHandle = lParam         // menu handle

              oItem := ::FindItemInAllMenus( wmId )

              // Possible flags
              // MF_BITMAP       // Item displays a bitmap.
              // MF_CHECKED      // Item is checked.
              // MF_DISABLED     // Item is disabled.
              // MF_GRAYED       // Item is grayed.
              // MF_HILITE       // Item is highlighted.
              // MF_MOUSESELECT  // Item is selected with the mouse.
              // MF_OWNERDRAW    // Item is an owner-drawn item.
              // MF_POPUP        // Item opens a drop-down menu or submenu.
              // MF_SYSMENU      // Item is contained in the window menu. The lParam parameter contains a handle to the menu associated with the message.

              DO CASE
                 CASE AND( wmFlag, MF_HILITE ) != 0
                      WG_DebugTrace( "TMenu:WindowProc() - WM_MENUSELECT - MF_HILITE" )

                      IF oItem <> NIL
                         oItem:OnHilite( ::oParent )
                      ENDIF

              ENDCASE

         CASE nMsg == WM_NEXTMENU          // Only for Win95 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_NEXTMENU" )

         CASE nMsg == WM_UNINITMENUPOPUP   // Only for Win98 and newer
              WG_DebugTrace( "TMenu:WindowProc() - WM_UNINITMENUPOPUP" )
      ENDCASE
   ENDIF
RETURN nRet

EXIT PROCEDURE __WG_TMenu_Destroy()
   //MessageBox( , "Nø font = " + cStr( TFont():nResources ) )
   WG_DebugTrace( "TMenu_Exit_Proc - Destroy menu without parent window", "TMenu():aoMenus", TMenu():aoMenus )
   // Run directly releaseresource() without use delresource() because this make a scan for each
   aEval( TMenu():aoMenus, {|o| IIF( !o:HasParent(), DestroyMenu( o:nHandle ), NIL ) } )
RETURN
