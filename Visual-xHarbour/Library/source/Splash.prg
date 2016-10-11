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
static __pPicture, hRegion, nWidth, nHeight, hBkBrush
//static __hText

static aSize
static nSecs, __aCenter, s_lProgress, s_lMarquee, s_cCancel, s_hFont, s_cText, s_hProgress, s_aRect, s_hParent, s_nMaxRange
static s_lAutoClose

#define SHOWDEBUG

#include "vxh.ch"
#include "debug.ch"
#include "uxTheme.ch"

#define PBS_MARQUEE        0x08
#define PBM_SETMARQUEE WM_USER + 10

FUNCTION Splash( hInst, cImage, cType, nTimeout, aCenter, nBitmapMaskColor, nTolerance )
   LOCAL nTop, nStyle, dt, nLeft
   nSecs := nTimeout

   DEFAULT hInst TO GetModuleHandle()
   __aCenter   := aCenter

   HB_CStructureCSyntax("_DIALOGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
   __ClsSetModule(__ActiveStructure() )

   dt := (struct _DIALOGTEMPLATE)

   __pCallBackPtr := WinCallBackPointer( @__SplashDlgProc() )

   nStyle  := (WS_POPUP | DS_SYSMODAL | WS_VISIBLE)
   nLeft   := 0
   nTop    := 0

   IF nBitmapMaskColor != NIL
      __pPicture := LoadBitmap( hInst, cImage )
      hBkBrush   := CreatePatternBrush( __pPicture )
      hRegion    := BitmapToRegion( __pPicture, nBitmapMaskColor, nTolerance )
      aSize      := GetBmpSize( __pPicture )
   ELSE
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
   ENDIF

   nWidth  := Int( ( aSize[1] * 4 )/LOWORD(GetDialogBaseUnits()) )
   nHeight := Int( ( aSize[2] * 4 )/LOWORD(GetDialogBaseUnits()) )

   dt:style           := nStyle
   dt:dwExtendedStyle := WS_EX_TOOLWINDOW
   dt:x               := Int( (nLeft * 4)/LOWORD(GetDialogBaseUnits()) )
   dt:y               := nTop
   dt:cx              := nWidth
   dt:cy              := nHeight
RETURN CreateDialogIndirect( hInst, dt, GetActiveWindow(), __pCallBackPtr )

FUNCTION __SplashDlgProc( hWnd, nMsg, nwParam )
   LOCAL nLeft, nTop, aRect, aPar, hDC, hMemDC, hMemBitmap, hOldBitmap
   SWITCH nMsg
      CASE WM_INITDIALOG
           IF hRegion != NIL
              SetWindowRgn( hWnd, hRegion, .T. )
              InvalidateRgn( hWnd, hRegion, .T. )
              SetWindowPos( hWnd,,0,0,0,0,SWP_FRAMECHANGED+SWP_NOMOVE+SWP_NOSIZE+SWP_NOZORDER)
              RedrawWindow( hWnd,,, RDW_FRAME + RDW_INVALIDATE + RDW_UPDATENOW )
              InvalidateRgn( hWnd, hRegion, .T. )

              hDC        := GetDC( hWnd )
              hMemDC     := CreateCompatibleDC( nwParam )
              hMemBitmap := CreateCompatibleBitmap( hDC, nWidth, nHeight )
              hOldBitmap := SelectObject( hMemDC, hMemBitmap)

              _FillRect( hMemDC, { 0, 0, nWidth, nHeight }, hBkBrush )

              BitBlt( hDC, 0, 0, nWidth, nHeight, hMemDC, 0, 0, SRCCOPY )

              SelectObject( hMemDC,  hOldBitmap )
              DeleteObject( hMemBitmap )
              ReleaseDC( hWnd, hMemDC )
           ENDIF

           SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE) )
           aPar  := _GetWindowRect( GetDeskTopWindow() )
           aRect := _GetWindowRect( hWnd )

           DEFAULT __aCenter TO aPar
           DEFAULT __aCenter[1] TO 0
           DEFAULT __aCenter[2] TO 0
           DEFAULT __aCenter[3] TO aPar[3]
           DEFAULT __aCenter[4] TO aPar[4]

           nLeft := __aCenter[1] + ( ( __aCenter[3] ) / 2 ) - ( (aRect[3]-aRect[1]) / 2 )
           nTop  := __aCenter[2] + ( ( __aCenter[4] ) / 2 ) - ( (aRect[4]-aRect[2]) / 2 )

           MoveWindow( hWnd, nLeft, nTop, aRect[3]-aRect[1]+1, aRect[4]-aRect[2]+1 )
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
           IF hRegion == NIL
              PicturePaint( __pPicture, nwParam, 0, 0, aSize[1], aSize[2], .F., .T. )
           ELSE
              _FillRect( nwParam, { 0, 0, aSize[1], aSize[2] }, hBkBrush )
           ENDIF
           RETURN 1

      CASE WM_NCDESTROY
           __GetApplication():MainForm:PostMessage( WM_VXH_FREECALLBACK, __pCallBackPtr )
           IF hRegion != NIL
              DeleteObject( __pPicture )
           ENDIF
           //VXH_FreeCallBackPointer( __pCallBackPtr )

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

   ACCESS IsWindow      INLINE IsWindow( ::hWnd )
   ACCESS Text          INLINE s_cText
   ASSIGN Text(cText)   INLINE ::SetText( cText ), UpdateWindow( ::hWnd )

   ACCESS Position      INLINE ::xPosition
   ASSIGN Position( n ) INLINE ::xPosition := n, ::SetPosition(n)

   ACCESS AutoClose     INLINE s_lAutoClose
   ASSIGN AutoClose(l)  INLINE s_lAutoClose := l

   ACCESS Marquee       INLINE s_lMarquee
   ASSIGN Marquee(l)    INLINE ::__SetMarquee(l)

   METHOD SetText()
   METHOD Init() CONSTRUCTOR
   METHOD SetPosition()
   METHOD Destroy()  INLINE DestroyWindow( ::hWnd )
   METHOD Close()    INLINE ::Destroy()
   METHOD __SetMarquee()
   METHOD AdjustSize()
ENDCLASS

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD Init( cText, cTitle, lProgress, cCancel, lMarquee, hParent, nMaxRange ) CLASS MessageWait
   DEFAULT lMarquee TO .F.
   s_lAutoClose := .F.
   ::hWnd := __MsgWait( cText, cTitle, lProgress, cCancel, lMarquee, hParent, nMaxRange )
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD AdjustSize() CLASS MessageWait
   LOCAL nBorder, aClient, hFont, aRect, hDC, rc := (struct RECT)

   aRect   := _GetWindowRect( ::hWnd )
   aClient := _GetClientRect( ::hWnd )
   nBorder := aRect[4]-aRect[2]-aClient[4]
   hDC     := GetDC( ::hWnd )
   hFont   := SelectObject( hDC, s_hFont )

   s_aRect := aClone(aRect)

   DrawText( hDC, s_cText, @rc, DT_CALCRECT )

   SelectObject( hDC, hFont )
   ReleaseDC( ::hWnd, hDC )

   IF s_lProgress
      rc:bottom += 25
   ENDIF
   IF s_cCancel != NIL
      rc:bottom += 20
   ENDIF
   MoveWindow( ::hWnd, aRect[1], aRect[2], rc:right, rc:bottom+nBorder )

RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD __SetMarquee( lSet ) CLASS MessageWait
   LOCAL nStyle
   IF s_lMarquee != lSet
      nStyle := GetWindowLong( s_hProgress, GWL_STYLE )
      IF lSet
         nStyle := (nStyle | PBS_MARQUEE)
       ELSE
         nStyle := (nStyle & NOT( PBS_MARQUEE ))
      ENDIF
      SetWindowLong( s_hProgress, GWL_STYLE, nStyle )
      SetWindowPos( s_hProgress, ,0,0,0,0,(SWP_FRAMECHANGED|SWP_NOMOVE|SWP_NOSIZE|SWP_NOZORDER))
      RedrawWindow( s_hProgress, , , (RDW_FRAME | RDW_INVALIDATE | RDW_UPDATENOW ))
      SendMessage( s_hProgress, PBM_SETMARQUEE, lSet, 30 )
      s_lMarquee := lSet
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD SetText( cText ) CLASS MessageWait
   s_cText := cText
   //SetWindowText( __hText, cText )
   RedrawWindow( ::hWnd, , , (RDW_INVALIDATE | RDW_UPDATENOW | RDW_INTERNALPAINT) )
   __GetApplication():DoEvents()
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
METHOD SetPosition(n) CLASS MessageWait
   IF ! s_lMarquee
      SendMessage( s_hProgress, PBM_SETPOS, n, 0 )
      __GetApplication():DoEvents()
   ENDIF
RETURN Self

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWait( cText, cTitle, lProgress, cCancel, lMarquee, hParent, nMaxRange )
   LOCAL nWidth, nHeight, nStyle, dt, hDC, hWnd, hFont

   DEFAULT cText  TO ""
   DEFAULT lProgress TO .F.
   DEFAULT lMarquee TO .F.

   s_hParent := hParent
   s_hFont   := __GetMessageFont()

   hDC       := GetDC(0)
   hFont     := SelectObject( hDC, s_hFont )
   nHeight   := 55
   nWidth    := Max( _GetTextExtentPoint32( hDC, cText )[1] + 40, 270)
   SelectObject( hDC, hFont )
   ReleaseDC(0,hDC)

   s_cCancel   := cCancel
   s_lProgress := lProgress
   s_lMarquee  := lMarquee
   s_cText     := cText
   s_nMaxRange := nMaxRange

   nStyle := (WS_POPUP | WS_DLGFRAME | WS_CLIPCHILDREN)

   IF cTitle != NIL
      nStyle := (nStyle | WS_CAPTION | WS_THICKFRAME)
   ENDIF

   HB_CStructureCSyntax("_DIALOGTEMPLATE",{"-4","style -4","dwExtendedStyle -2","cdit","2","x","2","y","2","cx","2","cy -2","menu -2","windowclass -2","title",},,,4 )
   __ClsSetModule(__ActiveStructure() )

   dt := (struct _DIALOGTEMPLATE)

   __pCallBackPtr := WinCallBackPointer( @__MsgWaitDlgProc() )

   dt:style           := nStyle
   dt:dwExtendedStyle := (WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE)
   dt:x               := 0
   dt:y               := 0
   dt:cx              := Int( ( nWidth  * 4 )/LOWORD(GetDialogBaseUnits()) )
   dt:cy              := Int( ( nHeight * 4 )/LOWORD(GetDialogBaseUnits()) )

   hWnd := CreateDialogIndirect( GetModuleHandle(), dt, GetActiveWindow(), __pCallBackPtr )
   SetWindowText( hWnd, cTitle )
   ShowWindow( hWnd, SW_SHOWNOACTIVATE )
   UpdateWindow( hWnd )
   IF hParent != NIL
      SetForegroundWindow( hParent )
   ENDIF
RETURN hWnd

//-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION __MsgWaitDlgProc( hWnd, nMsg, nwParam )
   LOCAL aClient, nLeft, nTop, aRect, aPar, hDC, aSize, hBtn, hFont, rc := (struct RECT)
   LOCAL nBorder, hOldFont, cPaint, nStyle

   SWITCH nMsg
      CASE WM_INITDIALOG
           SetWindowPos( hWnd, HWND_TOPMOST, 0, 0, 0, 0, (SWP_NOSIZE | SWP_NOMOVE | SWP_NOACTIVATE) )

           IF s_hParent == NIL
              aPar := _GetWindowRect( GetDeskTopWindow() )
           ELSE
              aPar := _GetWindowRect( s_hParent )
           ENDIF
           aRect   := _GetWindowRect( hWnd )
           aClient := _GetClientRect( hWnd )

           nBorder := aRect[4]-aRect[2]-aClient[4]

           nLeft := ( ( aPar[1] + aPar[3] ) / 2 ) - ( aClient[3] / 2 )
           nTop  := ( ( aPar[2] + aPar[4] ) / 2 ) - ( aClient[4] / 2 )

           hDC   := GetDC( hWnd )
           hFont := SelectObject( hDC, s_hFont )
           DrawText( hDC, s_cText, @rc, DT_CALCRECT )

           aRect[4] := aRect[2] + rc:bottom + nBorder + 20

           MoveWindow( hWnd, nLeft, nTop, aRect[3]-aRect[1], ( aRect[4]-aRect[2] ) + IIF( s_cCancel != NIL .OR. s_lProgress, 25, 0 ) )

           aRect := _GetClientRect( hWnd )
           aRect[1]+=5
           aRect[3]-=5
           aRect[4]-= ( IIF( s_cCancel != NIL .OR. s_lProgress, 25, 0 ) )


           aRect[2] := (aRect[4]-rc:bottom)/2

           s_aRect := aClone( aRect )

           aRect[4] := rc:bottom

           aSize := {0,0}

           IF s_cCancel != NIL
              aRect := _GetClientRect( hWnd )
              aSize := _GetTextExtentPoint32( hDC, s_cCancel )
              aSize[1] += 18

              nLeft := ( aRect[3] - aSize[1] ) / 2

              IF s_lProgress
                 nLeft := ( aRect[3] - aSize[1] ) - 4
              ENDIF
              hBtn  := CreateWindowEx( 0, "Button", s_cCancel, (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS),;
                                       nLeft, aRect[4]-26, aSize[1], 22, hWnd, 4000, GetModuleHandle(), NIL )
              SendMessage( hBtn, WM_SETFONT, s_hFont )
           ENDIF
           IF s_lProgress
              aRect := _GetClientRect( hWnd )
              nStyle := (WS_CHILD | WS_VISIBLE | WS_TABSTOP | WS_CLIPCHILDREN | WS_CLIPSIBLINGS)
              IF s_lMarquee
                 nStyle := (nStyle | PBS_MARQUEE)
              ENDIF
              s_hProgress := CreateWindowEx( 0, PROGRESS_CLASS, , nStyle, 6, aRect[4]-23, aRect[3]-aSize[1]-14, 16, hWnd, 4001, GetModuleHandle(), NIL )
              IF s_lMarquee
                 SendMessage( s_hProgress, PBM_SETMARQUEE, .T., 30 )
              ENDIF
              IF s_nMaxRange != NIL
                 SendMessage( s_hProgress, PBM_SETRANGE, 0, MAKELONG( 0, s_nMaxRange ) )
              ENDIF

           ENDIF
           SelectObject( hDC, hFont )
           ReleaseDC( hWnd, hDC )
           RETURN 1

      CASE WM_PAINT
           hDC := _BeginPaint( hWnd, @cPaint )

           SetBkColor( hDC, GetSysColor( COLOR_BTNFACE ) )
           _FillRect( hDC, s_aRect, GetSysColorBrush( COLOR_BTNFACE ) )

           hOldFont := SelectObject( hDC, s_hFont )

           _DrawText( hDC, s_cText, s_aRect, (DT_CENTER|DT_VCENTER|DT_NOPREFIX) )

           SelectObject( hDC, hOldFont )
           _EndPaint( hWnd, cPaint)

           IF s_lAutoClose .AND. IsKeyDown( VK_ESCAPE )
              DestroyWindow( hWnd )
           ENDIF
           EXIT

      CASE WM_KEYDOWN
           IF s_lAutoClose .AND. nwParam == VK_ESCAPE
              DestroyWindow( hWnd )
           ENDIF
           EXIT
      CASE WM_COMMAND
           IF nwParam == 4000
              DestroyWindow( hWnd )
           ENDIF
           EXIT

      CASE WM_NCDESTROY
           DeleteObject( s_hFont )
           //VXH_FreeCallBackPointer( __pCallBackPtr )
   END
RETURN 0
