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
static aSize
static nSecs, __aCenter, s_lProgress, s_hFont, s_cText

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

   METHOD Init() CONSTRUCTOR
   METHOD SetPosition()
   METHOD Destroy()  INLINE DestroyWindow( ::hWnd )
ENDCLASS

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( cText, cTitle, lProgress ) CLASS MessageWait
   ::hWnd := __MsgWait( cText, cTitle, lProgress )
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD SetPosition() CLASS MessageWait
   LOCAL hTheme, aBar, aRect := _GetClientRect( ::hWnd )
   LOCAL hDC := GetDC( ::hWnd )
   aBar := {4,aRect[4]-24,aRect[3]-4,aRect[4]-4}
   hTheme := OpenThemeData(,"PROGRESS")
   DrawThemeBackground( hTheme, hDC, PP_BAR, 0, aBar )
   aBar[1]+=1
   aBar[2]+=1
   aBar[3]-=1
   aBar[4]-=1

   aBar[3] := aBar[1] + (((aBar[3] - aBar[1]) * ::xPosition )/100)
   DrawThemeBackground( hTheme, hDC, PP_CHUNK, 0, aBar )

   CloseThemeData( hTheme )
   ReleaseDC( ::hWnd, hDC )
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWait( cText, cTitle, lProgress )
   LOCAL nWidth, nHeight, nStyle, dt, hDC, hWnd

   DEFAULT cText  TO ""
   DEFAULT cTitle TO ""
   DEFAULT lProgress TO .F.

   hDC       := GetDC(0)
   nHeight   := 55
   nWidth    := Max( _GetTextExtentPoint32( hDC, cText )[1] + 40, 270)
   ReleaseDC(0,hDC)

   IF lProgress
      nHeight += 24
   ENDIF
   
   s_lProgress := lProgress
   s_cText     := cText
   
   nStyle := WS_POPUP | WS_DLGFRAME | WS_THICKFRAME
   IF !EMPTY( cTitle )
      nStyle := nStyle | WS_CAPTION
   ENDIF

   __aCenter   := NIL
   
   HB_CStructureCSyntax("_DIALOGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
   __ClsSetModule(__ActiveStructure() )

   dt := (struct _DIALOGTEMPLATE)
      
   __pCallBackPtr := WinCallBackPointer( @__MsgWaitDlgProc() )

   s_hFont := __GetMessageFont()

   dt:style           := nStyle
   dt:dwExtendedStyle := WS_EX_TOOLWINDOW 
   dt:x               := 0
   dt:y               := 0
   dt:cx              := Int( ( nWidth  * 4 )/LOWORD(GetDialogBaseUnits()) )
   dt:cy              := Int( ( nHeight * 4 )/LOWORD(GetDialogBaseUnits()) )

   hWnd := CreateDialogIndirect( GetModuleHandle(), dt, GetActiveWindow(), __pCallBackPtr )
   SetWindowText( hWnd, cTitle )
   ShowWindow( hWnd, SW_SHOW )
RETURN hWnd

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWaitDlgProc( hWnd, nMsg, nwParam )
   LOCAL nLeft, nTop, aRect, aPar, hDC
   LOCAL hTheme, aBar
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
           RETURN 1

      CASE WM_ERASEBKGND  
           hDC := nwParam
           aRect := _GetClientRect( hWnd )
           _FillRect( hDC, aRect, GetSysColorBrush( COLOR_BTNFACE ) )
           aBar := {4,aRect[4]-24,aRect[3]-4,aRect[4]-4}
           aRect[1]+=5
           aRect[2]+=5
           aRect[3]-=5
           aRect[4]-=5
           IF s_lProgress
              aRect[4]-= 26
              hTheme := OpenThemeData(,"PROGRESS")
              IF hTheme != NIL
                 DrawThemeBackground( hTheme, hDC, 1, 0, aBar )
                 CloseThemeData( hTheme )
              ENDIF
           ENDIF
           SetBkMode( hDC, TRANSPARENT )
           SelectObject( hDC, s_hFont )
           _DrawText( hDC, s_cText, @aRect, DT_LEFT+DT_CENTER+DT_VCENTER+DT_SINGLELINE )
           RETURN 1
           
      CASE WM_NCDESTROY
           DeleteObject( s_hFont )
           FreeCallBackPointer( __pCallBackPtr )
   END
RETURN 0
