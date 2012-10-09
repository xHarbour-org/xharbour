/*
 * $Id$
 */
//------------------------------------------------------------------------------------------------------*
//                                                                                                      *
// Splash.prg                                                                                           *
//                                                                                                      *
// Copyright (C) xHarbour.com Inc. http://www.xHarbour.com                                              *
//                                                                                                      *
//  This source file is an intellectual property of xHarbour.com Inc.                                   *
//  You may NOT forward or share this file under any conditions!                                        *
//------------------------------------------------------------------------------------------------------*

static __pCallBackPtr
static __pPicture
static __hText

static aSize
static nSecs, __aCenter, s_lProgress, s_cCancel, s_hFont, s_cText, s_hProgress

#define SHOWDEBUG

#include "vxh.ch"
#include "debug.ch"
#include "uxTheme.ch"

FUNCTION Splash( hInst, cImage, cType, nTimeout, aCenter )
   LOCAL nTop, nWidth, nHeight, nStyle, dt, nLeft
   nSecs := nTimeout
   
   DEFAULT hInst TO GetModuleHandle()
   __aCenter   := aCenter
   
   HB_CStructureCSyntax("_DIALOGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
   __ClsSetModule(__ActiveStructure() )

   dt := (struct _DIALOGTEMPLATE)
      
   __pCallBackPtr := WinCallBackPointer( @__SplashDlgProc() )
   IF cType != NIL
      IF cType == "BMP"
         __pPicture := PictureLoadImageFromResource( hInst, UPPER( cImage ), 1 )
       ELSE
         __pPicture := PictureLoadFromResource( hInst, UPPER( cImage ), cType )
      ENDIF
    ELSE
      __pPicture     := PictureLoadFromFile( cImage )
   ENDIF
   aSize   := PictureGetSize( __pPicture )
   nStyle  := WS_POPUP | DS_SYSMODAL | WS_VISIBLE
   nLeft   := 0
   nTop    := 0
   nWidth  := Int( ( aSize[1] * 4 )/LOWORD(GetDialogBaseUnits()) )
   nHeight := Int( ( aSize[2] * 4 )/LOWORD(GetDialogBaseUnits()) )
   
   dt:style           := nStyle
   dt:dwExtendedStyle := WS_EX_TOOLWINDOW 
   dt:x               := Int( (nLeft * 4)/LOWORD(GetDialogBaseUnits()) )
   dt:y               := nTop
   dt:cx              := nWidth
   dt:cy              := nHeight
   CreateDialogIndirect( hInst, dt, GetActiveWindow(), __pCallBackPtr )
RETURN NIL

FUNCTION __SplashDlgProc( hWnd, nMsg, nwParam )
   LOCAL nLeft, nTop, aRect, aPar
   SWITCH nMsg
      CASE WM_INITDIALOG
           SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE )
           aPar  := _GetWindowRect( GetDeskTopWindow() )
           aRect := _GetWindowRect( hWnd )
           
           DEFAULT __aCenter TO aPar
           DEFAULT __aCenter[1] TO 0
           DEFAULT __aCenter[2] TO 0
           DEFAULT __aCenter[3] TO aPar[3]
           DEFAULT __aCenter[4] TO aPar[4]
           
           nLeft := __aCenter[1] + ( ( __aCenter[3] ) / 2 ) - ( (aRect[3]-aRect[1]) / 2 )
           nTop  := __aCenter[2] + ( ( __aCenter[4] ) / 2 ) - ( (aRect[4]-aRect[2]) / 2 )
            
           MoveWindow( hWnd, nLeft, nTop, aRect[3]-aRect[1], aRect[4]-aRect[2] )
           IF nSecs == NIL
              SetTimer( hWnd, 2, 100 )
            ELSE
              SetTimer( hWnd, 1, nSecs * 1000 )
           ENDIF
           EXIT

      CASE WM_TIMER
           IF nwParam == 2
              IF __GetApplication() != NIL .AND. __GetApplication():MainForm != NIL .AND. __GetApplication():MainForm:IsWindowVisible()
                 DestroyWindow( hWnd )
              ENDIF
            ELSE
              DestroyWindow( hWnd )
           ENDIF
           EXIT

      CASE WM_ERASEBKGND  
           PicturePaint( __pPicture, nwParam, 0, 0, aSize[1], aSize[2], .F., .T. )
           RETURN 1
           
      CASE WM_NCDESTROY
           FreeCallBackPointer( __pCallBackPtr )

   END
RETURN 0

//-------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------
//-------------------------------------------------------------------------------------------------------------------------------------

CLASS MessageWait
   DATA hWnd            EXPORTED
   DATA xPosition       PROTECTED INIT 0
   DATA pCallBackPtr    PROTECTED
   DATA __nListProc     PROTECTED

   ACCESS Position      INLINE ::xPosition PERSISTENT
   ASSIGN Position( n ) INLINE ::xPosition := n, ::SetPosition()

   METHOD SetText()
   METHOD Init() CONSTRUCTOR
   METHOD SetPosition()
   METHOD Destroy()  INLINE DestroyWindow( ::hWnd )
ENDCLASS

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( cText, cTitle, lProgress, cCancel ) CLASS MessageWait
   ::hWnd := __MsgWait( cText, cTitle, lProgress, cCancel )
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD SetText( cText ) CLASS MessageWait
   SetWindowText( __hText, cText )
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD SetPosition() CLASS MessageWait
   SendMessage( s_hProgress, PBM_SETPOS, ::xPosition, 0 )
   __GetApplication():DoEvents()
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWait( cText, cTitle, lProgress, cCancel )
   LOCAL nWidth, nHeight, nStyle, dt, hDC, hWnd, hFont

   DEFAULT cText  TO ""
   DEFAULT lProgress TO .F.

   s_hFont   := __GetMessageFont()

   hDC       := GetDC(0)
   hFont     := SelectObject( hDC, s_hFont )
   nHeight   := 55
   nWidth    := Max( _GetTextExtentPoint32( hDC, cText )[1] + 40, 270)
   SelectObject( hDC, hFont )
   ReleaseDC(0,hDC)

   s_cCancel   := cCancel
   s_lProgress := lProgress
   s_cText     := cText
   
   nStyle := WS_POPUP | WS_DLGFRAME

   IF cTitle != NIL
      nStyle := nStyle | WS_CAPTION | WS_THICKFRAME
   ENDIF

   HB_CStructureCSyntax("_DIALOGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
   __ClsSetModule(__ActiveStructure() )

   dt := (struct _DIALOGTEMPLATE)
      
   __pCallBackPtr := WinCallBackPointer( @__MsgWaitDlgProc() )

   dt:style           := nStyle
   dt:dwExtendedStyle := WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE
   dt:x               := 0
   dt:y               := 0
   dt:cx              := Int( ( nWidth  * 4 )/LOWORD(GetDialogBaseUnits()) )
   dt:cy              := Int( ( nHeight * 4 )/LOWORD(GetDialogBaseUnits()) )

   hWnd := CreateDialogIndirect( GetModuleHandle(), dt, GetActiveWindow(), __pCallBackPtr )
   SetWindowText( hWnd, cTitle )
   ShowWindow( hWnd, SW_SHOW )
   UpdateWindow( hWnd )
RETURN hWnd

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWaitDlgProc( hWnd, nMsg, nwParam )
   LOCAL aClient, nLeft, nTop, aRect, aPar, hDC, aSize, hBtn, hFont, rc := (struct RECT)
   LOCAL nBorder, aCenter

   SWITCH nMsg
      CASE WM_INITDIALOG
           SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE )
           aPar    := _GetWindowRect( GetDeskTopWindow() )
           aRect   := _GetWindowRect( hWnd )
           aClient := _GetClientRect( hWnd )
           nBorder := aRect[4]-aRect[2]-aClient[4]

           hDC   := GetDC( hWnd )
           hFont := SelectObject( hDC, s_hFont )
           DrawText( hDC, s_cText, @rc, DT_CALCRECT )

           aRect[4] := aRect[2] + rc:bottom + nBorder + 20
           
           aCenter := aPar
           DEFAULT aCenter[1] TO 0
           DEFAULT aCenter[2] TO 0
           DEFAULT aCenter[3] TO aPar[3]
           DEFAULT aCenter[4] TO aPar[4]
           
           nLeft := aCenter[1] + ( ( aCenter[3] ) / 2 ) - ( (aRect[3]-aRect[1]) / 2 )
           nTop  := aCenter[2] + ( ( aCenter[4] ) / 2 ) - ( (aRect[4]-aRect[2]) / 2 )

           MoveWindow( hWnd, nLeft, nTop, aRect[3]-aRect[1], ( aRect[4]-aRect[2] ) + IIF( s_cCancel != NIL .OR. s_lProgress, 25, 0 ) )

           aRect := _GetClientRect( hWnd )
           aRect[1]+=5
           aRect[3]-=5
           aRect[4]-= ( IIF( s_cCancel != NIL .OR. s_lProgress, 25, 0 ) )


           aRect[2] := (aRect[4]-rc:bottom)/2
           aRect[4] := rc:bottom

           __hText := CreateWindowEx( 0, "static", s_cText,;
                              WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | SS_CENTER,;
                              aRect[1], aRect[2], aRect[3], aRect[4],;
                              hWnd, 4002, __GetApplication():Instance, NIL )

           SendMessage( __hText, WM_SETFONT, s_hFont )

           aSize := {0,0}

           IF s_cCancel != NIL
              aRect := _GetClientRect( hWnd )
              aSize := _GetTextExtentPoint32( hDC, s_cCancel )
              aSize[1] += 18

              nLeft := ( aRect[3] - aSize[1] ) / 2
              
              IF s_lProgress
                 nLeft := ( aRect[3] - aSize[1] ) - 4
              ENDIF
              hBtn  := CreateWindowEx( 0, "Button", s_cCancel,;
                              WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS,;
                              nLeft, aRect[4]-26, aSize[1], 22,;
                              hWnd, 4000, __GetApplication():Instance, NIL )
              SendMessage( hBtn, WM_SETFONT, s_hFont )
           ENDIF
           IF s_lProgress
              aRect := _GetClientRect( hWnd )
              s_hProgress := CreateWindowEx( 0, PROGRESS_CLASS, ,;
                              WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS,;
                              6, aRect[4]-23, aRect[3]-aSize[1]-14, 16,;
                              hWnd, 4001, __GetApplication():Instance, NIL )
           ENDIF
           SelectObject( hDC, hFont )
           ReleaseDC( hWnd, hDC )
           RETURN 1

      CASE WM_COMMAND
           IF nwParam == 4000
              DestroyWindow( hWnd )
           ENDIF

      CASE WM_NCDESTROY
           DeleteObject( s_hFont )
           FreeCallBackPointer( __pCallBackPtr )
   END
RETURN 0
