#include "vxh.ch"
#include "Form1.xfm"
//---------------------------------------- End of system code ----------------------------------------//

// move myapp.JPG to a desired folder
// set myapp.JPG as BackgroundImage for FormSplash

//----------------------------------------------------------------------------------------------------//
METHOD Button1_OnClick( Sender ) CLASS Form1
   ::Close()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Form1_OnCreate( Sender ) CLASS Form1
   ::oSplash:=FormSplash( NIL )
   ::nTimer:=0
   ::Timer1:STart()
RETURN Self
//----------------------------------------------------------------------------------------------------//
METHOD Timer1_OnTimeOut( Sender ) CLASS Form1
   if ::oSplash <> NIL
      ::nTimer++
// set up the Timer Delay and repetition factor ( here 8 )
// to keep the Splash visible as much as you need
      if ::nTimer>8 .and. ::IsWindowVisible()
         ::oSplash:Close()
         ::oSplash:=NIL
         Sender:Stop()
      endif
   endif
RETURN Self