// WHAT32

// common controls and common dialogs

#Include "windows.ch"
#include 'statbar.ch'


*------------------------------------------------------------------------------

FUNCTION CreateStatusBar(nStyle, cText, hParent, nId  )

LOCAL hSBWnd
LOCAL nProc

   IF ( hSBWnd := CreateStatusWindow(nStyle, cText,hParent, nId )) <> 0
      nProc:=SetProcedure(hParent, {|hWnd, nMsg, nwParam, nlParam| ;
             _SBMove( nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd ) }, WM_SIZE )
   ENDIF

   RETURN(hSBWnd)

*------------------------------------------------------------------------------

// internal use

Static FUNCTION  _SBMove(  nProc, hWnd, nMsg, nwParam, nlParam, hSBWnd )

   LOCAL aRect

   IF nMsg == WM_SIZE
      If IsWindow( hSBWnd )
         aRect := GetWindowRect( hSBWnd )
         MoveWindow( hSBWnd, 0, HiWord( nlParam ) - ( aRect[ 4 ] - aRect[ 2 ] ) , ;
                     LoWord( nlParam ) , aRect[ 4 ] - aRect[ 2 ] , .T. )

      Endif
   EndIf


   Return CallWindowProc( nProc, hWnd, nMsg, nwParam, nlParam )


*------------------------------------------------------------------------------

FUNCTION SetStatusBarParts( hSBWnd, aParts )

   LOCAL bSizes := ""

   AEVAL(aParts,{|x| bSizes+=L2BIN(x)})

   return SendMessage( hSBWnd, SB_SETPARTS, LEN( aParts ), bSizes )


*------------------------------------------------------------------------------

FUNCTION SetStatusBarText( hSBWnd, nPart, cText )

   return SendMessage( hSBWnd, SB_SETTEXT, nPart, cText )















