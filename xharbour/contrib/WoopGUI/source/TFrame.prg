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
* From this class descends application window
*/

#include "woopgui.ch"
#include "common.ch"
#include "hbclass.ch"
#include "winuser.ch"

CLASS TFrame FROM TWindow

    // Data of this class only
    DATA oMenu        AS OBJECT HIDDEN
    DATA oStatusBar   AS OBJECT HIDDEN
    DATA lStatusBar   AS LOGICAL INIT FALSE

    // Public Methods
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR

    //METHOD oStatusBar() SETGET

    //METHOD GetStatusBar()
    METHOD CreateStatusBar()
    METHOD GetStatusBar()
    METHOD SetStatusBar()
    METHOD SetTitle( cTitle ) INLINE ::SetValue( cTitle )

    METHOD OnCreate()
    METHOD OnDestroy()
    METHOD WindowProc()

    // Hidden methods
    HIDDEN:
    METHOD AddControl( oCtrl ) INLINE ::AddChild( oCtrl )
    METHOD DelControl( oCtrl ) INLINE LOCAL nPos, iif( nPos := AScan( ::aoChilds, oCtrl ), aDel( ::aoChilds, nPos ), NIL )
    METHOD DestroyControls()   INLINE aEval( ::aoChilds, {|x| x:Destroy() } )
    METHOD FindMenuItem()

ENDCLASS

METHOD New( cName, nStyle, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar ) CLASS TFrame

    ASSIGN ::cClassName   WITH "FrmClass"
    ASSIGN ::cName        WITH cName                   DEFAULT "Frame_1"
    ASSIGN ::nStyle       WITH nStyle                  DEFAULT WS_OVERLAPPEDWINDOW
    ASSIGN ::nTop         WITH nTop                    DEFAULT CW_USEDEFAULT
    ASSIGN ::nLeft        WITH nLeft                   DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth                  DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight                 DEFAULT CW_USEDEFAULT
    //ASSIGN ::nChild       WITH NULL
    //ASSIGN ::nApplication WITH NULL // ApplObj()
    //ASSIGN ::pStruct      WITH NULL
    ASSIGN ::lStatusBar   WITH lStatusBar           DEFAULT FALSE

    ::Super:New( ::nExStyle, ::cClassName, ::cName, ::nStyle, ;
                 ::nTop, ::nLeft, ::nWidth, ::nHeight, ;
                 oParent /*, ::nChild, ::nApplication, ::pStruct*/ )

RETURN Self

METHOD NewExtended( cTitle, nStyle, nTop, nLeft, nWidth, nHeight,;
                    oMenu, oBrush, oIcon, oParent, lStatusBar, ;
                    lvScroll, lhScroll, nClrFore, nClrBack, oCursor,;
                    cBorder, lNoSysMenu, lNoCaption,;
                    lNoIconize, lNoMaximize, lPixel ) CLASS TFrame

   ::New( cTitle, nStyle, nTop, nLeft, nWidth, nHeight, oParent, lStatusBar )

   ::oMenu := oMenu
   // IF ValType( oBrush ) == "O" THEN ::SetBrush( oBrush )
   // IF ValType( oIcon ) == "O" THEN ::SetIcon( oIcon )

RETURN Self

//METHOD oStatusBar( oStatusBar AS OBJECT ) CLASS TFrame
//   LOCAL oOldSB := ::myoStatusBar
//   IF oStatusBar <> NIL
//      ::myoStatusBar := oStatusBar
//   ENDIF
//RETURN oOldSB

METHOD CreateStatusBar( naParts, nStyle ) CLASS TFrame
  ::oStatusBar := tStatusBar():New( Self, naParts, nStyle )
  ::oStatusBar:Create()
  ::lStatusBar := TRUE
RETURN IIF( ::oStatusBar <> NIL, TRUE, FALSE )

METHOD GetStatusBar() CLASS TFrame
RETURN ::oStatusBar

METHOD SetStatusBar( cString, nPart, uFlags ) CLASS TFrame
   IF ValType( ::oStatusBar ) == "O"
      ::oStatusBar:SetValue( cString, nPart, uFlags )
   ENDIF
RETURN Self

METHOD OnCreate() CLASS TFrame
   LOCAL nRet := ::Super:OnCreate()
   WG_DebugTrace( "TFrame:OnCreate()", "Self", Self )
   ::SetMenu( ::oMenu )
   ::SetCurrentWindow( Self )

   IF ValType( ::oMenu ) == "O" THEN ::SetMenu( ::oMenu )
   // IF ValType( oBrush ) == "O" THEN ::SetBrush( oBrush )
   // IF ValType( oIcon ) == "O" THEN ::SetIcon( oIcon )

   IF ::lStatusBar
      // Create SIMPLE status bar
      // FSG - Define application variable to make a standard statusbar
      ::CreateStatusBar()
   ENDIF
RETURN nRet


METHOD OnDestroy() CLASS TFrame
   LOCAL nRet := -1
   WG_DebugTrace( "TFrame:OnDestroy()", "Self", Self )
   PostQuitMessage( 0 )
   nRet := 0
RETURN nRet

METHOD WindowProc( nMsg, wParam, lParam ) CLASS TFrame
   LOCAL nRet := -1
   WG_DebugTrace( "TFrame:WindowProc()", "Self", Self, "nHandle", ::nHandle )
   IF ValType( ::bWindowProc ) == "B"
      // User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
   ENDIF
   IF nRet == -1
      // Class event handler
      // A frame can have a status bar, a menu, , so we must check its event handler

      IF nRet == -1 .AND. ValType( ::oStatusBar ) == "O"   THEN nRet := ::oStatusBar:WindowProc( nMsg, wParam, lParam )
      IF nRet == -1 .AND. ValType( ::oMenu ) == "O"        THEN nRet := ::oMenu:WindowProc( nMsg, wParam, lParam )
      IF nRet == -1 .AND. ValType( ::oContextMenu ) == "O" THEN nRet := ::oContextMenu:WindowProc( nMsg, wParam, lParam )

      // Events will be evaluated in ::super:windowproc()
      //DO CASE
      //   CASE nMsg == WM_DESTROY
      //        PostQuitMessage( 0 )
      //        nRet := 0
      //ENDCASE

   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF
RETURN nRet

// reserved methods  ------------------------------------------

METHOD FindMenuItem( oMenu, nID ) CLASS TFrame
   LOCAL oItem
   DEFAULT oMenu TO ::oMenu
   IF ValType( oMenu ) == "O"
      oItem := WG_FindMenuItem( oMenu, nID )
   ENDIF

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

