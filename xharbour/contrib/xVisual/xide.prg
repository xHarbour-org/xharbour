#include "windows.ch"
#include "wingdi.ch"
#include "common.ch"
#include "hbclass.ch"

FUNCTION Main
   LOCAL oApp, oMain

   oApp  := Application():New()

   oMain := Form1():New()
   
   oMain:Create()
   
   oApp:Run()
RETURN( nil)


CLASS Form1 FROM TForm
   METHOD OnPaint( hDC ) INLINE DrawGrid( ::handle, hDC, 3 ),0
   METHOD OnClose()      INLINE MessageBox( ::handle, 'OnClose','Whoo'),;
                                PostQuitMessage(0)

ENDCLASS
