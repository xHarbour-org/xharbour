// Augusto Infante
// Whoo.lib

#include "hbclass.ch"
#include "windows.ch"
#include "debug.ch"
#include "what32.ch"

CLASS TSplash FROM TForm
   DATA bitmap
   METHOD New()           CONSTRUCTOR
   METHOD OnPaint( hDC )  INLINE DrawBitmap(hDC,::bitmap),0
   METHOD OnDestroy()     INLINE DeleteObject(::bitmap),NIL
   METHOD OnTimer(n)      INLINE if( n==1,::Destroy(),)
   METHOD OnLButtonDown() INLINE ::Destroy()
   METHOD OnRButtonDown() INLINE ::Destroy()
ENDCLASS

METHOD New( oParent, cFile, nTimeOut ) CLASS TSplash
   local aRect,abRect
   super:new( oParent )
   DEFAULT nTimeOut TO 2000
   aRect := GetWindowRect(GetDesktopWindow())
   
   ::bitmap:= LoadImage( NIL, cFile, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE )
   abRect  := GetBitmapSize( ::bitmap )
   
   ::width := abRect[1]
   ::height:= abRect[2]
   ::left  := (aRect[3]/2)-(::width/2)
   ::top   := (aRect[4]/2)-(::height/2)
   ::style := WS_POPUP + WS_BORDER
   ::ExStyle:= WS_EX_TOPMOST
   ::Create()
   UpdateWindow( ::handle)
   SetTimer( ::handle, 1, nTimeOut )
return( self )
