**********************************************
* Service/daemon and error handling demo.
*
* This program demonstrates how to use signal (error)
* handlers and what to do to start services.
*
* Best viewed with gtcgi. On unix, you can change
*      HB_StartService( .F. )
* into
*      HB_StartService( .T. )
* only if you are using gtcgi
*
* (C) 2003 Giancarlo Niccolai
*
* $Id: openportd.prg,v 1.2 2003/07/25 23:49:33 gian Exp $
*

#include "hbservice.ch"
#include "hblog.ch"
GLOBAL bWait


PROCEDURE Main()
   LOCAL nTime := 10, nStart

   // put it on a file: windows console could be detached!
   INIT LOG ON CONSOLE()

   bWait := .T.
   HB_PushSignalHandler( HB_SIGNAL_ALL, "Handle" )
   // a newer push will override previous ones
   HB_PushSignalHandler( HB_SIGNAL_FAULT + HB_SIGNAL_MATHERR, @SignalFault())

   //Service can be started before or after pushing handler.
   // Don't detach under windws (just pass .F.); it works, but must be
   // finetuned.
   HB_StartService( .F. )

   StartThread( @waiter() )
   SecondsSleep( 0.5 )
   ? "Main thread is waiting"
   DO WHILE bWait
      // does service checks/process WM_ messages
      // does also GC
      HB_ServiceLoop()
      SecondsSleep( 0.5 )
   ENDDO

   ? "Main thread terminated"
   ? "Press Any key"
   Inkey(0)
   ? ""
RETURN


Function Handle( nSignal, aParams )
   // THIS SHOULD NOT BE DONE, but I do it to test that it works :-)!
   // Don't do it in real life. Is not good to start threads from
   // within signal handlers.
   //StartThread(@Teller(), nSignal, aParams)

   Teller( nSignal, aParams)
   IF nSignal == HB_SIGNAL_QUIT
      bWait = .F.
   ENDIF
RETURN HB_SERVICE_HANDLED

PROCEDURE Waiter()
   LOCAL nCount := 30

   WHILE bWait
      ThreadSleep( 1000 )
      OutStd( "." )
      // create a segfault after a while..
      IF nCount == 0
         #ifdef DEBUG
         HB_ServiceGenerateFault()
         // notice, under linux the segfault address is not the program
         // address that caused the fault, but is the address of the
         // offended memory ( in our example 0x0).
         #endif
      ENDIF
      nCount--
   ENDDO

RETURN

PROCEDURE Teller(nSignal, aParams)
   ? "Thread: ", ThreadGetCurrent(), ThreadGetCurrentInternal()
   ? "Handled: ", nSignal
   ? "Syserror: ", aParams[1], aParams[2], HB_SignalDesc( aParams[1], aParams[2] )
RETURN

// A generic and useful fault detector using LOG system
// you can set it as a handler.

FUNCTION SignalFault(nSignal, aParams )
   LOCAL n := 1, cRet := ""

   WHILE ( ! Empty(ProcName( n ) ) )
      cRet += "Called from: "+ ProcName( n ) +;
              "(" + LTrim( Str( ProcLine( n ) ) ) +;
              ")  in Module: "+ ProcFile( n ) + HB_OsNewLine()
      n++
   END
   cRet += "Internal address: 0x"+AllTrim( HB_NumToHex( aParams[4] ) )+ HB_OSNewLine()
   cRet += "Low-level code: " + AllTrim( str( aParams[1] ) ) +"/"+;
      AllTrim( HB_NumToHex(aParams[2])) + ": " + ;
      HB_SignalDesc( aParams[1], aParams[2] ) + HB_OSNewLine()

   LOG "The program has caused an internal error. Framestack: " + HB_OSNewLine() + cRet;
      PRIO HB_LOG_CRITICAL

      // THIS KILLS THE PROGRAM
RETURN HB_SERVICE_QUIT
