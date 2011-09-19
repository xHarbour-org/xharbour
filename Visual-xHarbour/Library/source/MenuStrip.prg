/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// MenuStrip.prg                                                                                        *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

GLOBAL EXTERNAL s_lExecuting
GLOBAL EXTERNAL s_CurrFocus
GLOBAL EXTERNAL s_CurrentObject
GLOBAL EXTERNAL s_lKey
GLOBAL EXTERNAL s_lOpenMenu
GLOBAL EXTERNAL s_hKeyMenuHook

#include "debug.ch"
#include "vxh.ch"

#define DG_ADDCONTROL             1

#define FROMARGB(r,g,b,a)  ((((b)<<16)|(((g)<<16)|(((r)<<16)|(((a)<<16)))

//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------

CLASS MenuStrip INHERIT ToolStrip
   DATA __lIsMenu  EXPORTED  INIT .T.
   
   PROPERTY Height      READ xHeight      WRITE __SetHeight DEFAULT 22 MIN 22 HIDDEN
   PROPERTY ShowChevron READ xShowChevron WRITE __ShowChevron  PROTECTED DEFAULT .F. HIDDEN
   
   METHOD Init() CONSTRUCTOR
   METHOD OnPaint()
   METHOD OnSize()
   METHOD OnMove()
   METHOD __OnParentSize()
   METHOD __SetHeight( x )    INLINE ::__SetSizePos( 4, x ), IIF( ::hWnd != NIL, (::Parent:__RefreshLayout( .T. ), /*AEVAL( ::Children, {|o| o:Height := o:Parent:Height - 1, o:MoveWindow() } )*/ ),)
   METHOD __UpdateWidth()
   METHOD OnSysKeyDown()
   METHOD OnParentSysCommand()
   METHOD OnParentSysKeyDown()
ENDCLASS

//-------------------------------------------------------------------------------------------------------
METHOD Init( oParent ) CLASS MenuStrip
   ::__xCtrlName   := "MenuStrip"
   ::Style         := WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS
   ::ClsName       := "MenuStrip"
   ::Super:Init( oParent )
   IF ::__ClassInst != NIL
      ::__IdeContextMenuItems := { { "Add MenuStripItem",   {|| ::__AddToolStripItem( "MenuStripItem" ) } },;
                                   { "Add ToolStrip&Label", {|| ::__AddToolStripItem( "ToolStripLabel" ) } },;
                                   { "Add ToolStrip&ComboBox", {|| ::__AddToolStripItem( "ToolStripComboBox" ) } } }
   ENDIF
RETURN Self

//-----------------------------------------------------------------------------------------------
METHOD OnParentSysKeyDown( nwParam ) CLASS MenuStrip
   LOCAL nItem, oItem

   IF nwParam != VK_MENU
      IF ( nItem := __GetHotItem( Self, 1, nwParam ) ) > 0
         oItem := ::Children[ nItem ]

         IF !EMPTY( oItem:Children ) .AND. ( oItem:DropDown > 1 .OR. oItem:Parent:__lIsMenu )
            IF s_CurrFocus != NIL
               s_CurrFocus:__lSelected := .F.
               s_CurrFocus:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
               s_CurrFocus := NIL
            ENDIF
            oItem:PostMessage( WM_USER + 1028 )
            RETURN 0
         ENDIF
      ENDIF
   ENDIF
RETURN NIL

//-----------------------------------------------------------------------------------------------
METHOD OnParentSysCommand( nwParam ) CLASS MenuStrip

   IF nwParam == SC_KEYMENU .AND. !CheckBit( GetKeyState( VK_SPACE ) , 32768 ) .AND. !s_lKey
      IF s_CurrentObject != NIL
         s_CurrentObject := NIL
         RETURN 0
      ENDIF
      ::Form:SendMessage( WM_CANCELMODE, 0, 0 )

      s_lKey := .T.

      s_CurrentObject := ::Children[1]
      DEFAULT s_hKeyMenuHook TO SetWindowsHookEx( WH_MSGFILTER, ( @__KeyMenuHook() ), NIL, GetCurrentThreadId() )

      s_CurrFocus  := ::Children[1]
      s_lOpenMenu  := .F.
      ::Children[1]:PostMessage( WM_USER + 1029 )

      RETURN 0
   ENDIF
RETURN NIL


//-------------------------------------------------------------------------------------------------------
METHOD OnSysKeyDown( nwParam ) CLASS MenuStrip
   LOCAL n

   // close the menu on ALT KEY y it is selected
   IF nwParam == VK_MENU .AND. s_CurrentObject != NIL
      s_CurrentObject:__lSelected := .F.
      s_CurrentObject:RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      s_CurrFocus := NIL
      s_CurrentObject := NIL
      UnhookWindowsHookEx( s_hKeyMenuHook )

      s_hKeyMenuHook := NIL

      s_lExecuting := .F.
      s_lKey       := .F.
      s_lOpenMenu  := .T.
      RETURN 1
      
    ELSEIF nwParam != VK_MENU

      FOR n := 1 TO LEN( ::Children )
         IF AT( "&"+UPPER( CHR( nwParam ) ), UPPER( ::Children[n]:Caption ) ) > 0

            SendMessage( ::Form:hWnd, WM_CANCELMODE, 0, 0 )
            s_lKey := .F.
            ::Children[n]:PostMessage( WM_USER + 1028 )
            RETURN 0
         ENDIF
      NEXT

   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __UpdateWidth() CLASS MenuStrip
   ::Width := ::Parent:Width - IIF( ::xShowGrip, (::__GripperPos + 1), 0 )
   ::__nWidth := ::Width
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnSize( n, x, y ) CLASS MenuStrip
   (n,y)
   ::__PrevSize := x
   IF ::Row > 0 //.AND. ::__PrevRow == 0 
      ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      AEVAL( ::Children, {|o| o:SetWindowPos( , 0, 0, 0, 0, SWP_FRAMECHANGED | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER ) } )
   ENDIF
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD __OnParentSize( x, y, hDef ) CLASS MenuStrip
   LOCAL nLeft, nTop, i, n, aLines, aLine
   (x,y)
   IF ::IsWindowVisible() .AND. ::Parent:ClientWidth > 0
      ::Width := ::Parent:ClientWidth - IIF( ::xShowGrip, (::__GripperPos + 1), 0 )
      IF ::Row > 0 .AND. ::__PrevRow == 0 
         ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
      ENDIF
      IF LEN( ::Children ) > 0
         aLines := {}
         aLine  := {}
         i := ::Children[1]:Left + ::Children[1]:Width + 2

         FOR n := 1 TO LEN( ::Children )
             ::Children[n]:InvalidateRect()
             IF i > ::Parent:ClientWidth
                AADD( aLines, aLine )
                aLine  := {}
                i := ::Children[1]:Left + ::Children[n]:Width + 2
             ENDIF
             AADD( aLine, n )
             i += ::Children[n]:Width + 2
         NEXT
         AADD( aLines, aLine )

         nTop  := 1
         nLeft := ::Children[1]:Left
         FOR i  := 1 TO LEN( aLines )
             FOR n := 1 TO LEN( aLines[i] )
                 ::Children[ aLines[i][n] ]:Left := nLeft
                 ::Children[ aLines[i][n] ]:Top  := nTop
                 nLeft += ::Children[ aLines[i][n] ]:Width + 2
             NEXT
             nTop += 22
             nLeft := ::Children[1]:Left
         NEXT
         ::Height := 22 * LEN( aLines )
      ENDIF
   ENDIF
RETURN hDef

//-------------------------------------------------------------------------------------------------------
METHOD OnMove( x, y ) CLASS MenuStrip
   Super:OnMove( x, y )
   ::RedrawWindow( , , RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT )
   AEVAL( ::Children, {|o| o:InvalidateRect() } )
RETURN NIL

//-------------------------------------------------------------------------------------------------------
METHOD OnPaint( hDC, hMemDC ) CLASS MenuStrip
   LOCAL aRect := Array(4)
   LOCAL y, n, nDots := ( ::Height - 6 ) / 4
   LOCAL hMemBitmap, hOldBitmap, oChild, hOldBitmap1, hMemDC1

   IF hDC != NIL
      hMemDC     := CreateCompatibleDC( hDC )
      hMemBitmap := CreateCompatibleBitmap( hDC, ::ClientWidth, ::ClientHeight )
      hOldBitmap := SelectObject( hMemDC, hMemBitmap)
   ENDIF
   
   _FillRect( hMemDC, {0,0,::ClientWidth,::ClientHeight}, ::__hBrush )
   
   IF ::Row > 0 .AND. ::ShowGrip
      y := 4
      FOR n := 1 TO nDots  
          SetPixel( hMemDC, ::__GripperPos + 1, y + 1, ::System:CurrentScheme:GripLight )
          SetPixel( hMemDC, ::__GripperPos + 1, y + 2, ::System:CurrentScheme:GripLight )
          SetPixel( hMemDC, ::__GripperPos + 2, y + 1, ::System:CurrentScheme:GripLight )
          SetPixel( hMemDC, ::__GripperPos + 2, y + 2, ::System:CurrentScheme:GripLight )

          SetPixel( hMemDC, ::__GripperPos,     y + 0, ::System:CurrentScheme:GripDark )
          SetPixel( hMemDC, ::__GripperPos,     y + 1, ::System:CurrentScheme:GripDark )
          SetPixel( hMemDC, ::__GripperPos + 1, y + 0, ::System:CurrentScheme:GripDark )
          SetPixel( hMemDC, ::__GripperPos + 1, y + 1, ::System:CurrentScheme:GripDark )
          y += 4
      NEXT
   ENDIF
   
   IF hMemBitmap != NIL
      FOR EACH oChild IN ::Children
          IF oChild:__hBrush != NIL
             DeleteObject( oChild:__hBrush )
          ENDIF

          DEFAULT oChild:__hMemBitmap TO CreateCompatibleBitmap( hDC, oChild:Width+oChild:__BackMargin, oChild:Height+oChild:__BackMargin )

          hMemDC1      := CreateCompatibleDC( hDC )
          hOldBitmap1  := SelectObject( hMemDC1, oChild:__hMemBitmap )

          BitBlt( hMemDC1, 0, 0, oChild:Width, oChild:Height, hMemDC, oChild:Left+oChild:__BackMargin, oChild:Top+oChild:__BackMargin, SRCCOPY )

          oChild:__hBrush := CreatePatternBrush( oChild:__hMemBitmap )

          SelectObject( hMemDC1,  hOldBitmap1 )
          DeleteDC( hMemDC1 )

      NEXT
   ENDIF

   IF hDC != NIL
      BitBlt( hDC, 0, 0, ::ClientWidth, ::ClientHeight, hMemDC, 0, 0, SRCCOPY )
      SelectObject( hMemDC,  hOldBitmap )
      DeleteObject( hMemBitmap )
      DeleteDC( hMemDC )
   ENDIF
RETURN 0

