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
#include "windows.ch"
#include "getexit.ch"


CLASS TPanel FROM TWindow

    CLASSDATA anDialogs AS ARRAY INIT {} HIDDEN    // Array of handles of dialogs

    DATA oTemplate   AS OBJECT

    DATA nDlgVer     AS NUMERIC INIT 1
    DATA nSignature  AS NUMERIC INIT 0xFFFF        // Extended mode
    DATA nHelpID     AS NUMERIC INIT 0
    //DATA nExStyle    AS NUMERIC INIT 0
    //DATA nStyle      AS NUMERIC
    ACCESS nDlgItems INLINE Len( ::aoChilds )
    //DATA nRow
    //DATA nCol
    //DATA nWidth
    //DATA nHeight
    DATA nMenu       AS NUMERIC INIT 0
    DATA nClassName  AS NUMERIC INIT 0
    //DATA cTitle      AS STRING  INIT ""

    //DATA nPointSize  AS NUMERIC INIT 0
    //DATA nWeight     AS NUMERIC INIT 0
    //DATA lItalic     AS LOGICAL INIT FALSE
    //DATA nCharSet    AS NUMERIC INIT 0
    //DATA cFace       AS STRING  INIT ""

    DATA lInitialize AS LOGICAL INIT FALSE

    // Base
    //DATA aoItems AS ARRAY INIT {} HIDDEN
    DATA lModal       AS LOGICAL
    DATA nReturnValue AS NUMERIC

    // Data of this class only
    //DATA oMenu        HIDDEN
    DATA oDialogTemplate AS OBJECT HIDDEN

    DATA oGetList        AS OBJECT //HIDDEN

    // Public Methods
    METHOD New()         CONSTRUCTOR
    METHOD NewExtended() CONSTRUCTOR
    METHOD Create()
    METHOD EndDialog()  //INLINE IIF( ::lModal, EndDialog( ::nHandle ), ::Destroy() )

    METHOD AddDialogWindowHandle( hDlg )  INLINE aAdd( ::anDialogs, hDlg )
    METHOD DelDialogWindowHandle( hDlg )  INLINE LOCAL nPos,;
                                                 nPos := ::FindDialogWindowHandle( hDlg ),;
                                                 IIF( nPos > 0, WG_aShrink( ::anDialogs, nPos ), NIL )
    METHOD FindDialogWindowHandle( hDlg ) INLINE aScan( ::anDialogs, hDlg )
    METHOD GetDialogHandleArray()

    METHOD NextControl()
    METHOD PreviousControl()

    METHOD GoToControl( nID )       INLINE ::oGetList:Settle( ::GetControlPosByID( nID ) )
    METHOD GetControlPosByID( nID ) INLINE ::FindChildByID( nID )

    METHOD WindowProc()

    METHOD InitGetList()

    // Events
    METHOD OnCommand()
    METHOD OnContextMenu()
    METHOD OnCtlColor()
    METHOD OnDrawItem()
    METHOD OnInitDialog()
    METHOD OnNCDestroy()

ENDCLASS

METHOD New( nExStyle, cName, nStyle, nRow, nCol, nWidth, nHeight, oParent, lPixel, lModal ) CLASS TPanel

    ASSIGN ::lModal       WITH lModal               DEFAULT FALSE
    ASSIGN ::nExStyle     WITH nExStyle             DEFAULT 0 //WS_EX_CLIENTEDGE
    ::cClassName  := "WoopGUIDlgClass"
    ASSIGN ::nClassName   WITH 0
    ASSIGN ::cName        WITH cName                DEFAULT "Panel_1"
    ASSIGN ::nStyle       WITH nStyle               DEFAULT WS_CHILD + WS_BORDER
                               //IIF( ::lModal, ;
                               //     WS_POPUP + WS_SYSMENU + WS_CAPTION + DS_MODALFRAME + DS_3DLOOK,;
                               //     WS_TABSTOP + WS_POPUP + WS_DLGFRAME + WS_BORDER + DS_3DLOOK + WS_SYSMENU + WS_MINIMIZEBOX )
    ASSIGN ::nRow         WITH nRow                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nCol         WITH nCol                 DEFAULT CW_USEDEFAULT
    ASSIGN ::nWidth       WITH nWidth               DEFAULT CW_USEDEFAULT
    ASSIGN ::nHeight      WITH nHeight              DEFAULT CW_USEDEFAULT
    ASSIGN ::oParent      WITH oParent
    ASSIGN ::nMenu        WITH 0
    ASSIGN ::lPixel       WITH lPixel               DEFAULT FALSE

    //::oFont := TFont():New( "MS Sans Serif", 8 )
    ::oFont := TFont():NewExtended( -11, 0, 0, 0, 400, .F., .F.,;
                       .F., 0, 1, 2, 1, 34, "MS Sans Serif" )

    ::Super:New( ::nExStyle, ::cClassName, ::cName, ::nStyle, ::nRow, ::nCol, ;
                 ::nWidth, ::nHeight, ::oParent /*, ::nChild, ::nApplication, pStruct */ )

    // Set as current dialog window
    ::SetCurrentWindow( Self )

RETURN Self

METHOD NewExtended( nExStyle, cTitle, nStyle, nRow, nCol, nWidth, nHeight,;
                    oMenu, oBrush, oIcon, oParent AS OBJECT, lPixel, lModal, ;
                    lvScroll, lhScroll, nClrFore, nClrBack, oCursor,;
                    cBorder ) CLASS TPanel


   ::New( nExStyle, cTitle, nStyle, nRow, nCol, nWidth, nHeight, oParent, lPixel, lModal )

RETURN Self


METHOD Create( GetList ) CLASS TPanel
    //WG_ObjDisplayData( Self )

    // Add dialog window before create because i need to find
    ::AddWindow( Self )

    IF GetList <> NIL
       ::oGetList := ::InitGetList( GetList )
    ENDIF

    //::nHandle := WG_CreateDialogIndirect( Self )
    IF ::lModal
       ::nReturnValue := WG_DialogBoxIndirect( Self )
    ELSE
       ::nHandle := WG_CreateDialogIndirect( Self )
       // Add this window in dialog array
       ::AddDialogWindowHandle( ::nHandle )
    ENDIF

RETURN Self

METHOD EndDialog() CLASS TPanel

  IF ::lModal
      WG_DebugTrace( "TPanel:EndDialog()", "Self", Self )
     //IF __objHasMethod( Self, "DestroyControls" ) THEN ::DestroyControls()
     EndDialog( ::nHandle )

     //IF ValType( ::oToolTip ) == "O"     THEN ::oToolTip:Destroy()
     //IF ValType( ::oMenu ) == "O"        THEN ::oMenu:Destroy()
     //IF ValType( ::oContextMenu ) == "O" THEN ::oContextMenu:Destroy()
     //
     //IF ::HasChildren()
     //   // Shrink array, when this window will destroy windows destroy childs too
     //   WG_DebugTrace( "TPanel:EndDialog() - Has Children, shrink array" )
     //   ::DestroyChildren()
     //ENDIF
     //IF ::HasParent()
     //   // Parent will destroy handle, here only shrink array
     //   WG_DebugTrace( "TPanel:EndDialog() - Has Parent, shrink array" )
     //   ::DelWindow()
     //ELSE
     //   WG_DebugTrace( "TPanel:EndDialog() - call EndDialog()" )
     //   ::DelWindow()
     //   Self := NIL
     //ENDIF

  ELSE
     //IF ValType( ::oToolTip ) == "O"     THEN ::oToolTip:Destroy()
     //IF ValType( ::oMenu ) == "O"        THEN ::oMenu:Destroy()
     //IF ValType( ::oContextMenu ) == "O" THEN ::oContextMenu:Destroy()
     //
     //IF ::HasChildren()
     //   // Shrink array, when this window will destroy windows destroy childs too
     //   WG_DebugTrace( "TPanel:Destroy() - Has Children, shrink array" )
     //   ::DestroyChildren()
     //ENDIF
     WG_DebugTrace( "TPanel:Destroy() - call DestroyWindow()" )
     //::Destroy()
     DestroyWindow( ::nHandle )
  ENDIF

RETURN Self

METHOD GetDialogHandleArray() CLASS TPanel
RETURN ::anDialogs

//------------------------------------------------------------------------------

METHOD InitGetList( GetList, nPos ) CLASS TPanel

   LOCAL oGetList, oSaveGetList

   oGetList := HBGetList():New( GetList )
   oGetList:cReadProcName := ProcName( 1 )
   oGetList:nReadProcLine := ProcLine( 1 )

   oSaveGetList := __GetListActive( )
   __GetListSetActive( oGetList )
   __GetListLast( oGetList )

   IF ! ( ISNUMBER( nPos ) .AND. nPos > 0 )
      oGetList:nPos := oGetList:Settle( 0 )
   ENDIF

RETURN oGetList
/*
   DO WHILE oGetList:nPos != 0

      oGetList:oGet := oGetList:aGetList[ oGetList:nPos ]
      oGetList:PostActiveGet()

      IF ISBLOCK( oGetList:oGet:Reader )
         Eval( oGetList:oGet:Reader, oGetList:oGet )
      ELSE
         oGetList:Reader()
      ENDIF

      oGetList:nPos := oGetList:Settle()

   ENDDO

   __GetListSetActive( oSaveGetList )

RETURN oGetList:lUpdated
*/


METHOD NextControl()
   LOCAL oGet

   //::oGetList:GetActive():ExitState := GE_DOWN
   WG_DebugTrace( "TPanel:NextControl()", "::oGetList:nPos", ::oGetList:nPos, "::oGetList:nLastExitState", ::oGetList:nLastExitState )
   ::oGetList:nPos := ::oGetList:Settle()
   oGet := ::oGetList:aGetList[ ::oGetList:nPos ]
   WG_DebugTrace( "TPanel:NextControl()", "::oGetList:nPos", ::oGetList:nPos, "oGet:Name", oGet:Name, "::oGetList:nLastExitState", ::oGetList:nLastExitState )
   oGet:SetWindowFocus()
RETURN Self

METHOD PreviousControl()
   LOCAL oGet
   //::oGetList:GetActive():ExitState := GE_UP
   ::oGetList:nPos := ::oGetList:Settle()
   oGet := ::oGetList:aGetList[ ::oGetList:nPos ]
   WG_DebugTrace( "TPanel:PreviousControl()", "::oGetList:nPos", ::oGetList:nPos, "oGet", oGet )
   oGet:SetWindowFocus()
RETURN Self


//------------------------------------------------------------------------------
// Events

METHOD OnCtlColor( wParam, lParam ) CLASS TPanel
   LOCAL nRet := -1
   LOCAL nHDC := wParam
   LOCAL nCtl := lParam
   LOCAL oWin := ::FindChildByHandle( nCtl )

   IF oWin <> NIL
      nRet := oWin:OnCtlColor( nHDC )
   ENDIF

RETURN nRet

METHOD OnDrawItem( nID, lParam ) CLASS TPanel
   LOCAL nRet := -1
   LOCAL oWin := ::FindChildByID( nID )
   MessageBox( , "Passato da TDialog_OnDrawItem" )
   IF oWin <> NIL
      nRet := oWin:OnDrawItem( lParam )
   ENDIF

RETURN nRet

METHOD OnInitDialog( wParam, lParam ) CLASS TPanel
   LOCAL nRet := -1

   //MessageBox(, "TDIALOG Window Proc INIT DIALOG" )
   WG_DebugTrace( "TPanel:OnInitDialog()", "Self", Self )
   IF ::lInitialize
      aEval( ::GetChildren(), ;
             {|oItem| ;
              oItem:nHandle := GetDlgItem( ::nHandle, oItem:nID ), ; // Set nHandle
              oItem:Init(), ; // Init controls
              IIF( oItem:lDefault, oItem:SetFocus(), NIL ) ;
             } )
      // MessageBox(,"Impostati Handle ai controlli" )

      IF ValType( ::oGetList ) == "O"
         ::oGetList:nPos := ::oGetList:Settle( 0 )
      ENDIF

      ::lInitialize := FALSE
   ENDIF

RETURN nRet

METHOD OnCommand( wParam, lParam ) CLASS TPanel
   LOCAL nRet := -1
   LOCAL nEvent := HiWord( wParam )
   LOCAL nID    := LoWord( wParam )
   //LOCAL oWin := ::FindChildByID( nID )
   LOCAL oWin := ::FindChildByHandle( lParam )

   WG_DebugTrace( "TPanel:OnCommand()", "Self", Self )

   IF oWin <> NIL
      nRet := oWin:OnCommand( wParam, lParam )
      DO CASE
         CASE nEvent == BN_CLICKED//IF oWin <> NIL .AND. oWin:HasAction()
              // Default actions
              DO CASE
                 CASE nID == IDOK
                      ::EndDialog( IDOK )
                 CASE nID == IDCANCEL
                      ::EndDialog( IDCANCEL )
              ENDCASE
      ENDCASE
   ENDIF

RETURN nRet

METHOD OnContextMenu( wParam, lParam ) CLASS TPanel
   LOCAL nRet := -1
   LOCAL x := LoWord( lParam )
   LOCAL y := HiWord( lParam )
   LOCAL oWin := ::FindChildByHandle( wParam )

   IF oWin <> NIL
      nRet := oWin:OnContextMenu( x, y )
   ELSE
      nRet := ::Super:OnContextMenu( wParam, lParam )
   ENDIF

RETURN nRet

METHOD OnNCDestroy() CLASS TPanel
   WG_DebugTrace( "TPanel:OnNCDestroy() - shrink array of dialogs", "Self", Self )
   ::Super:OnNCDestroy()
   ::DelDialogWindowHandle( ::nHandle )
RETURN -1

//------------------------------------------------------------------------------

METHOD WindowProc( nMsg, wParam, lParam ) CLASS TPanel
   LOCAL nRet := -1  // = TRUE
   LOCAL wmId, wmEvent, wmHandle
   LOCAL oWin

   // Check if there is a user event handler
   IF ::HasEventHandler() // ValType( ::bWindowProc ) == "B"
      // Evaluate User event handler
      nRet := Eval( ::bWindowProc, ::nHandle, nMsg, wParam, lParam )
      //MessageBox(, "TDIALOG Window Proc Event handler return " + cStr( nRet ) )
   ENDIF
   IF nRet == -1
      // Class event handler

      DO CASE

         CASE nMsg == WM_GETDLGCODE
              WG_DebugTrace( "TPanel:WindowProc() - WM_GETDLGCODE" )
              nRet := DLGC_HASSETSEL + DLGC_WANTALLKEYS + DLGC_WANTARROWS + DLGC_WANTCHARS

         CASE nMsg == WM_CHAR
          	  nRet = ::OnChar(wParam, lParam)

      ENDCASE

   ENDIF
   IF nRet == -1
      // Standard Event Handler
      nRet := ::Super:WindowProc( nMsg, wParam, lParam )
   ENDIF

RETURN nRet


FUNCTION WG_GetDialogArray()
RETURN TPanel():GetDialogHandleArray()

// ----------------------------------------------------
//
// W_DefDlgEvents( hWnd, nMsg, wParam, lParam ) --> Bool
//
// interface between Windows API event handling and Class handling
// ----------------------------------------------------
FUNCTION WG_DefDlgEvents( hWnd, nMsg, wParam, lParam )
  LOCAL oWin, oWinClass
  LOCAL nRet := -1  // Assume that default is -1 and not 0 which i the C default for dialogs,
                    // so i can use same window procedure of windows -- i have a doubt of it correctness

  // Send messages to function that write events.log for debug - See TApplication
  WG_ApplObj():EventsWrite( "DLG", hWnd, nMsg, wParam, lParam )

  DO CASE
     // If wm_initdialog i assign only the handle, then other in windowproc()
     CASE nMsg == WM_INITDIALOG
          oWin         := aTail( TWindow():GetWindows() )
          oWin:nHandle := hWnd
          WG_DebugTrace( "WG_DefDlgEvents() - WM_INITDIALOG", "oWin:ClassName", oWin:ClassName )
  ENDCASE

  // Search window
  oWin      := TWindow():FindWindowByHandle( hwnd )
  IF oWin <> NIL
     // Call his window procedure
     nRet := oWin:WindowProc( nMsg, wParam, lParam )
  ENDIF

  // I must return to c call function the default that is 0
  //WG_DebugTrace( "WG_DefDlgEvents() - return value", "nRet", nRet )
  IF nRet == NIL .OR. nRet == -1
     nRet := 0  // == FALSE
  ELSEIF nRet == 0
     nRet := -1  // == TRUE
  ENDIF

RETURN nRet
