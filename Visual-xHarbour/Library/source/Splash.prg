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
static nSecs, __aCenter

#define SHOWDEBUG

#include "vxh.ch"
#include "debug.ch"

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

FUNCTION __SplashDlgProc( hWnd, nMsg, nwParam, nlParam )
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
